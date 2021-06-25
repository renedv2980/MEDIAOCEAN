*          DATA SET SPSFM59    AT LEVEL 045 AS OF 03/06/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE T21759A                                                                  
T21759   TITLE 'SPSFM59 - COST2 MAINTENANCE OVERLAY'                            
T21759   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKL,*T21759*,R7,R5,RR=RE                                     
*                                                                               
         LR    RF,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         ST    RF,AMYWORK          SET A(MY D-CHAIN WORK AREA)                  
         ST    RE,RELO                                                          
         ST    RB,BASE1                                                         
         ST    R7,BASE2                                                         
         ST    R5,BASE3                                                         
         BAS   RE,MYINIT           INITIALIZE VALUES FIRST                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECPUT         PUT RECORD                                   
         BE    PR                                                               
         CLI   MODE,XRECPUT        AFTER PUT RECORD                             
         BE    XP                                                               
*                                                                               
NEXTMODE DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*========================== INITIALIZE VALUES ========================*         
MYINIT   NTR1                                                                   
                                                                                
         GOTO1 DATCON,DMCB,(X'03',BTODAY),(X'02',CTODAY),0                      
*                                                                               
         DS    0H                  SET ADDRESSES NEEDED W/IN CSECT              
         LA    R0,DSPCSCTQ                                                      
         LH    R1,=Y(DSPCSECT-T21759)                                           
         A     R1,BASE1                                                         
         LA    RF,AINCSECT                                                      
MINIT03  LH    RE,0(R1)                                                         
         A     RE,BASE1                                                         
         ST    RE,0(RF)                                                         
         LA    R1,L'DSPCSECT(R1)                                                
         LA    RF,L'AINCSECT(RF)                                                
         BCT   R0,MINIT03                                                       
*                                                                               
         DS    0H                  SET UP LABELS IN SPOT STORAGE AREA           
         L     RE,ATIADSPT                                                      
         L     RF,ALBLTAB                                                       
         LA    R0,TIADSPQ                                                       
MINIT05  LH    R1,0(RE)                                                         
         A     R1,ATIA                                                          
         MVC   0(L'LBLTAB,R1),0(RF)                                             
         LA    RE,L'TIADSPTB(RE)                                                
         LA    RF,L'LBLTAB(RF)                                                  
         BCT   R0,MINIT05                                                       
*                                                                               
         DS    0H                  INITIALIZE D-CHAIN WORKING STORAGE           
         LA    R0,INIWRKTQ          R0 = LOOP COUNTER                           
         LH    R1,=Y(INIWRKTB-T21759)                                           
         A     R1,BASE1                                                         
MINIT07  ZICM  RE,0(R1),(3)                                                     
         A     RE,AMYWORK          RE-->WHERE LABEL GOES                        
         MVC   0(8,RE),4(R1)                                                    
         LA    RE,8(RE)                                                         
         ZICM  RF,2(R1),(3)                                                     
         A     RF,ASYSD                                                         
         ST    RE,0(RF)                                                         
         LA    R1,L'INIWRKTB(R1)                                                
         BCT   R0,MINIT07                                                       
*                                                                               
         L     RF,ATIA             SET UP SPOT STORAGE AREA ADDRESSES           
         LH    R1,=Y(ACCUTAB-SPOTAREA)                                          
         AR    R1,RF                                                            
         ST    R1,AACCUTAB                                                      
*                                                                               
         LA    R1,C2MCCOSH         MY FIRST DATA FIELD                          
         ST    R1,AM1STREC                                                      
*                                                                               
         MVC   AADDAY,ADDAY        SET UP MOBILE ADCON LIST                     
         MVC   ADATCON,DATCON                                                   
         MVC   AGETDAY,GETDAY                                                   
         LA    R2,COREQLSQ         # OF CORE-RES PHASES TO GET                  
         L     R3,ACORQLST         LIST OF CORE-RES PHASES TO GET               
         LA    R4,ACORQADD         R4-->START OF PLACE TO PUT ADDRESSES         
         ICM   R0,14,=X'D9000A'    GET SET FOR CALLOV                           
                                                                                
MINIT10  IC    R0,0(R3)                                                         
         GOTO1 CALLOV,DMCB,0,(R0),0                                             
         CLI   DMCB+4,XFF                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB                                                     
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R2,MINIT10                                                       
*                                                                               
         GOTO1 DICTATE,DMCB,C'LL  ',DCLIST,DSLIST                               
*                                                                               
         DS    0H                  CHECK FOR FIRST TIME                         
         NI    MISCFLG3,XFF-MF31STIM                                            
         CLC   C2M1TIM,=C'HI'                                                   
         BE    *+14                                                             
         MVC   C2M1TIM,=C'HI'                                                   
         OI    MISCFLG3,MF31STIM                                                
*                                                                               
         DS    0H                  STUFF TO DO FOR FIRST TIME                   
         TM    MISCFLG3,MF31STIM                                                
         BZ    MINIT12X                                                         
         XC    TSARBLK,TSARBLK      CLEAR TSAR BLOCK                            
MINIT12X EQU   *                                                                
*                                                                               
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          DO MY OWN IO                                 
*                                                                               
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'OFF'                                 
*&&                                                                             
*                                                                               
         MVC   REQID,SPACES                                                     
*                                                                               
         MVC   CBLSCMSK,=X'FFFF80' CABLE STATION SYSCODE MASK                   
*                                                                               
         MVC   RELOCKSW,DCRELOCK   DATE-FLAG TO FLAG NEED TO RE-LOCK            
*                                                                               
         DS    0H                  SOME INITIALIZATION FOR TSAR BLOCK           
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVC   TSACOM,ACOMFACS                                                  
         LA    R0,STACREC           TSAR USED FOR STTN ACCUM TABLE              
         ST    R0,TSAREC                                                        
         DROP  R1                                                               
*                                                                               
** THE FUCKING SELECT CODES **                                                  
*                                                                               
         DS    0H                  PF KEYS STUFF                                
         OI    CONSERVH+6,X'81'    FOR PFKEY TO WORK                            
         OI    C2MPFLNH+6,X80                                                   
         MVC   C2MPFLN+67(9),SPACES                                             
****     MVC   MYCALLSP(1),CALLSTCK                                             
****     MVC   MYCALLSP+1(1),CALLSP                                             
         NI    MISCFLG2,XFF-MF2SLAVE   ASSUME WE'RE NOT SLAVING                 
                                                                                
         DS    0H                  CHECK IF PFKEY WAS PASSED IN                 
         CLI   CALLSP,1                                                         
         BNE   MINIT15                                                          
         CLI   CALLSTCK,LSTSCRNQ   IT HAD BETTER BE FROM LIST SCREEN!           
         BNE   MINIT15              IF NOT, JUST IGNORE & KEEP GOING            
         CLI   CALLPFK,0           SEE IF WE HAD BEEN HERE                      
         BNE   MINIT15              YEP, DON'T REPEAT                           
         MVC   CALLPFK,PFKEY                                                    
         MVI   PFKEY,0                                                          
                                                                                
*&&DO                                                                           
         MVI   GOSUBN,SRE#         SLAVE RESTORE ELEMENTS                       
         CLI   CALLPFK,3                                                        
         BE    MINITPF                                                          
*&&                                                                             
         MVI   GOSUBN,SSL#         SLAVE STATION LOCK                           
         CLI   CALLPFK,5                                                        
         BE    MINITPF                                                          
         MVI   GOSUBN,SBL#         SLAVE BUY     LOCK                           
         CLI   CALLPFK,6                                                        
         BE    MINITPF                                                          
         MVI   GOSUBN,SBU#         SLAVE BUY     UNLOCK                         
         CLI   CALLPFK,7                                                        
         BE    MINITPF                                                          
         MVI   GOSUBN,SPL#         SLAVE PW      LOCK                           
         CLI   CALLPFK,8                                                        
         BE    MINITPF                                                          
         MVI   GOSUBN,0            DON'T NEED TO SLAVE                          
         B     MINIT15                                                          
                                                                                
MINITPF  DS    0H                                                               
         OI    MISCFLG2,MF2SLAVE   WE'RE SLAVING!                               
         GOTO1 AGOSUB                                                           
         NI    MISCFLG2,XFF-MF2SLAVE   TURN IT OFF AFTERWARDS                   
         MVI   PFKEY,12                                                         
         B     MINIT15                                                          
                                                                                
MINIT15  DS    0H                                                               
         L     R2,APFMAINT         R0-->PF TABLES FROM MAINT ONLY               
         CLI   CALLSP,1            DID WE GET HERE FROM SOMEWHERE?              
         BNE   MINIT20              NOPE                                        
         CLI   CALLSTCK,LSTSCRNQ   IT HAD BETTER BE FROM LIST SCREEN!           
*&&DO                                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         BNE   MINIT20              IF NOT, JUST IGNORE & KEEP GOING            
         MVC   C2MPFLN+68(7),PF12DC                                             
         L     R2,APFLIST          R0-->PF TABLES FROM LIST SCREEN              
MINIT20  DS    0H                                                               
         LA    R1,C2MMEDH          IN CASE OF ERROR,                            
         ST    R1,ACURFORC          WE KNOW WHERE TO PUT CURSOR                 
         GOTO1 INITPFKY,DMCB,(R2)                                               
         XC    ACURFORC,ACURFORC   CLEAR THIS IF WE GOT BACK OKAY               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------- MYINIT ERRORS ---------------------------*         
PWNAVE   DS    0H                                                               
         LA    R2,CONRECH                                                       
         MVI   MYERRCD,PWNAVQ                                                   
         B     MYERROR                                                          
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY (VALKEY)'           
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VK       DS    0H                                                               
         CLI   XFRCALL,C'Y'        TEST XFR CONTROL                             
         BNE   VK2                                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',C2MMEDH,,GLVSPMD                              
         GOTO1 (RF),(R1),,C2MCLTH,,GLVSPCLT                                     
         GOTO1 (RF),(R1),,C2MPRDH,,GLVSPPRD                                     
         GOTO1 (RF),(R1),,C2MESTH,,GLVSPEST                                     
         GOTO1 (RF),(R1),,C2MMKTH,,GLVSPMKT                                     
VK2      DS    0H                                                               
         MVI   MYIOFLAG,0                                                       
         NI    MISCFLG1,XFF-MF1KYCHG                                            
         NI    MISCFLG2,XFF-MF2RQADD                                            
         NI    MISCFLG3,XFF-MF3GDPLY  DON'T FORCE TO DISPLAY, YET               
         XC    MYTEXT(MYTEXTL),MYTEXT                                           
*                                                                               
*--------------------------- VALIDATE MEDIA --------------------------*         
*                                                                               
         LA    R2,C2MMEDH          MEDIA                                        
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         GOTO1 VALIMED                                                          
         OI    4(R2),X20                                                        
                                                                                
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVC   MYAGYID,AGYID       HOLD ONTO AGENCY ID                          
         MVC   MYCANADA,AGYPCNDA   SAVE OFF FOR NEW CABLE TEST                  
         DROP  R6                                                               
*                                                                               
*-------------------------- VALIDATE CLIENT --------------------------*         
*                                                                               
         LA    R2,C2MCLTH          CLIENT                                       
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         GOTO1 VALICLT                                                          
         OI    4(R2),X20                                                        
                                                                                
         DS    0H                  SET SPOT PROFILE                             
         MVC   WORK(12),=CL12'S000'                                             
         MVC   WORK+4(3),MYAGYID                                                
         MVC   WORK+7(3),QCLT                                                   
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         NI    CLTFLAG,XFF-CFWSTRAD                                             
         TM    COPT2,COP2TRAD                                                   
         BZ    *+8                                                              
         OI    CLTFLAG,CFWSTRAD       TRADE CLIENT                              
                                                                                
         CLI   COFFICE,X'41'                                                    
         BL    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
                                                                                
         DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,SPOTPROF,DATAMGR                               
*                                                                               
*-------------------------- VALIDATE PRODUCT -------------------------*         
*                                                                               
         LA    R2,C2MPRDH          PRODUCT                                      
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
                                                                                
         CLI   5(R2),0                                                          
         BE    MFLDE               MISSING FIELD ERROR                          
                                                                                
         MVI   MYERRCD,NPOLQ       MAKE SURE POL WAS NOT REQUESTED              
         CLC   8(3,R2),=C'POL'                                                  
         BE    MYERROR                                                          
                                                                                
         DS    0H                  DISALLOW TRADE PRODUCTS                      
         ZIC   R1,5(R2)                                                         
         LA    R1,(8-1)(R1,R2)      R1-->LAST CHAR OF INPUT                     
         CLI   0(R1),C'#'                                                       
         BNE   VKPRD049                                                         
         MVI   MYERRCD,IPRD2Q       SET ERROR CODE                              
         MVC   MYTEXT+1(6),=C'for PW'  AND EXTRA TEXT                           
         MVI   MYTEXT+0,6                                                       
         B     MYERROR                                                          
VKPRD049 EQU   *                                                                
                                                                                
         GOTO1 VALIPRD                                                          
         OI    4(R2),X20                                                        
*                                                                               
*------------------------- VALIDATE ESTIMATE -------------------------*         
*                                                                               
         LA    R2,C2MESTH          ESTIMATE                                     
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         GOTO1 VALIEST                                                          
         OI    4(R2),X20                                                        
                                                                                
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         SR    R1,R1                                                            
         ICM   R1,15,ECOST2        CHECK IF COST2 EXISTS                        
         BZ    MPCTE               IF NONE, THEN ERROR                          
         ST    R1,OC2PCT           SAVE CS2                                     
         EDIT  (R1),(8,C2MPRCT),6,ZERO=NOBLANK                                  
         OI    C2MPRCTH+6,X80       AND DISPLAY IT                              
                                                                                
         DS    0H                      PULL OTHER INFO FROM EST HDR             
         NI    ESTFLAG,XFF-EFBILEST              ESTIMATE PERIOD FLG            
         TM    ECONTROL,EBILESTQ                                                
         BZ    *+8                                                              
         OI    ESTFLAG,EFBILEST     TREAT AS ONE WHOLE PERIOD                   
*                                                                               
         NI    ESTFLAG,XFF-EFOWPW                OOW PW BILLING FLAG            
         TM    EFLAG1,EF1OOWPW                                                  
         BZ    *+8                                                              
         OI    ESTFLAG,EFOWPW                                                   
                                                                                
         NI    ESTFLAG,XFF-EFWSTRAD              TRADE ESTIMATE FLAG            
                                                                                
         MVC   ESDATE(L'ESTART+L'EEND),ESTART    EST START/END DATES            
         GOTO1 DATCON,DMCB,(X'10',ESDATE),(5,C2MESDT),0                         
         OI    C2MESDTH+6,X80                                                   
         CLI   EOWSDAY,0                         OUT-OF-WEEK START DAY          
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         DROP  R6                                                               
*                                                                               
*-------------------------- VALIDATE MARKET --------------------------*         
*                                                                               
         XC    BSTA,BSTA           DON'T WANT ANY LEFTOVERS IN HERE             
         XC    QSTA,QSTA            NOR IN HERE                                 
         LA    R2,C2MMKTH          MARKET                                       
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
                                                                                
         CLI   5(R2),0                                                          
         BE    MFLDE               MISSING FIELD ERROR                          
                                                                                
         TM    4(R2),X08           TEST NUMERIC                                 
         BZ    VKSTA                                                            
         GOTO1 VALIMKT                                                          
         OI    4(R2),X20                                                        
         MVC   C2MMKNM,MKTNM                                                    
         OI    C2MMKNMH+6,X80                                                   
         B     VKOPT                                                            
*                                                                               
*---------------- FOR TESTING ONLY, VALIDATE STATION -----------------*         
*                                                                               
VKSTA    DS    0H                                                               
         CLI   TWAOFFC,C'*'        TESTING W/ STATION ONLY FOR                  
         BNE   IMKTE                DDS TERMINAL                                
                                                                                
         GOTO1 VALISTA                                                          
         OI    4(R2),X20                                                        
         MVC   C2MMKNM,MKTNM                                                    
         OI    C2MMKNMH+6,X80                                                   
         B     VKOPT                                                            
*                                                                               
*---------------------- VALIDATE OPTIONS FIELD -----------------------*         
*                                                                               
VKOPT    LA    R2,C2MOPTNH                                                      
         BAS   RE,VALOPTS                                                       
*                                                                               
*--------------------- BUILD THE KEY FOR GENCON ----------------------*         
*                                                                               
VKPW     DS    0H                                                               
         L     R0,AMYC2REC                                                      
         LA    R1,L'MYPWREC                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR STORAGE FOR PW RECORD                  
*                                                                               
VKPW03   DS    0H                                                               
         NI    MISCFLG2,XFF-MF2RRDCH                                            
         MVI   GOSUBN,TSR_INI#                                                  
         GOTO1 AGOSUB                                                           
*                                                                               
VKPW05   DS    0H                                                               
         BAS   RE,PWSETUP          SET UP PW TABLES, ETC.                       
*                                                                               
** ADD NEW PW RECORD **                                                         
*                                                                               
         DS    0H                  ADD CS2 RECORD W/ C2STEL ELEMS               
         NI    MISCFLG1,XFF-MF1ADNOW                                            
         CLI   MYIOFLAG,IOFADD     THE FIRST TIME CS2/MAINT IS USED             
         BNE   VKPW20                                                           
*&&DO                                                                           
         BNE   VKPWX                                                            
*&&                                                                             
         TM    MISCFLG2,MF2BUY0Q    W/ AT LEAST 1 BUY                           
         BZ    VKPW10                                                           
         TM    MISCFLG1,MF1GOL0Q    OR GOAL IN MARKET.                          
         BO    VKPWX                                                            
                                                                                
VKPW10   OI    MISCFLG1,MF1ADNOW   GO ADD IT NOW                                
         B     VR410                                                            
*                                                                               
VKPW10A  NI    MISCFLG1,XFF-MF1ADNOW                                            
         MVI   MYIOFLAG,0                                                       
         B     VKPW03              REDO PW SETUP                                
*                                                                               
VKPW20   DS    0H                  IF NOT 1ST TIME, CHECK RE-FINAL BILL         
*                                                                               
VKPWX    B     XIT                                                              
         EJECT                                                                  
*------------------------- TEST CHANGE OF KEY ------------------------*         
*                                                                               
KYCHNGED DS    0H                                                               
         TM    4(R2),X20                                                        
         BZ    KYCH10                                                           
         TM    4(R2),X80                                                        
         BZR   RE                                                               
KYCH10   OI    MISCFLG1,MF1KYCHG                                                
         BR    RE                                                               
         SPACE 2                                                                
*---------------------------- VALKEY ERRORS -------------------------*          
                                                                                
MFLDE    DS    0H                                                               
         MVI   MYERRCD,MFLDQ       MISSING INPUT FIELD ERROR                    
         B     MYERROR                                                          
                                                                                
IMKTE    DS    0H                                                               
         MVI   MYERRCD,EMKTQ       INVALID MARKET ERROR                         
         B     MYERROR                                                          
*------------------------------ PW SETUP -----------------------------*         
*                                                                               
PWSETUP  NTR1                                                                   
         MVI   GOSUBN,IAC#         INITIALIZE ACCUM TABLE                       
         GOTO1 AGOSUB                                                           
*                                                                               
         MVI   GOSUBN,GPR#         GET PW RECORD (KEY IS BUILT TOO)             
         GOTO1 (RF)                                                             
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         NI    LOCKFLAG,XFF-LKFFBLKQ-LKFFPLKQ-LKFUBLKQ                          
         OI    LOCKFLAG,LKFUPLKQ              1ST TIME, DEFAULT TO LOCK         
*                                                                               
         CLI   MYIOFLAG,IOFADD                IF PW RECORD EXISTS,              
         BE    PWS03                                                            
         NI    LOCKFLAG,XFF-LKFBUYQ-LKFPWQ                                      
         TM    PWGNFLG,PWGNBILQ                SET BUY LOCK FLAGS               
         BZ    *+8                                                              
         OI    LOCKFLAG,LKFBUYQ                                                 
         TM    PWGNFLG,PWGNPLKQ                SET PW  LOCK FLAGS               
         BZ    PWS03                                                            
         OI    LOCKFLAG,LKFPWQ                                                  
*                                                                               
PWS03    DS    0H                  GET PWDTIEL ELEMENT ALSO                     
         XC    DTIELEM,DTIELEM     ASSUME THERE ARE NONE IN RECORD              
         MVI   DTIELEM,PWDTICDQ                                                 
         MVI   DTIELEM+1,PWDTILNQ                                               
         MVC   DTIELEM+(PWDTIPLD-PWDTIEL)(L'PWDTIPLD),CTODAY                    
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWDTICDQ',(R6)),0,0                   
         CLI   12(R1),0                                                         
         BNE   PWS05                                                            
         L     RF,12(R1)                                                        
         MVC   DTIELEM,0(RF)                                                    
*                                                                               
PWS05    DS    0H                                                               
         L     RF,AGOSUB           RF=A(INTERFACE ROUTINE)                      
         TM    MISCFLG1,MF1KYOPT   IF KEY OR OPTIONS DID NOT CHANGE,            
         BNZ   PWS05A                                                           
****                                                                            
         CLI   C2MRQIDH+5,0        REQUESTING STATION LOCKIN?                   
         BNE   PWS05A              YES, DON'T CARE IF NO BUYS                   
****                                                                            
         TM    MISCFLG2,MF2RRDCH    SEE IF RE-READING, OR                       
         BO    PWS05B                                                           
         CLI   PFKEY,9              SEE IF PF9 HIT FOR RE-DISPLAY               
         BE    PWS05B                                                           
         B     PWS30                IF NOT, JUST RESTORE TIA TABLES             
*                                                                               
PWS05A   TM    MISCFLG1,MF1KYCHG   IF KEY DID NOT CHANGE,                       
         BZ    PWS10                SKIP PROCEDURES FOR NEW KEY                 
                                                                                
         NI    MISCFLG1,XFF-MF1NKFS                                             
         NI    MISCFLG2,XFF-MF2NKFS                                             
         NI    MISCFLG3,XFF-MF3NKFS                                             
         NI    LOCKFLAG,XFF-LKFNKFS                                             
PWS05B   XC    GNELVALS(GNELVALQ),GNELVALS  CLR ACT GOALS, GRP, & TAX           
         XC    SKEDCGTX,SKEDCGTX                                                
         XC    SKEDCGL,SKEDCGL                                                  
                                                                                
         CLI   MYIOFLAG,IOFADD                                                  
         BE    PWS10                                                            
         MVC   SKEDACG2,PWGNGOAL                                                
         MVC   PWGRP,PWGNGRP                                                    
         MVC   GLTXRATE,PWGNTAX                                                 
         MVC   SKEDCGTX,PWGNGOAL   SKED'S ACTUAL GOAL + TAX                     
         MVI   GOSUBN,CGLNTX#      GET SKEDCGL (SKEDCGTX - TAX AMT)             
         GOTO1 AGOSUB                                                           
         TM    PWGNFLG,PWGNUGOL    IF IT'S A USER-INPUTTED GOAL,                
         BZ    *+8                                                              
         OI    MISCFLG3,MF3UGOAL    REMEMBER IT                                 
         TM    PWGNFLG,PWGNUGRP    IF IT'S A USER-INPUTTED GRP,                 
         BZ    *+8                                                              
         OI    MISCFLG3,MF3UGRP     REMEMBER IT                                 
         DROP  R6                                                               
*                                                                               
PWS10    MVI   GOSUBN,GBT#         BUILD BROADCAST MONTH TABLE                  
         GOTO1 (RF)                                                             
                                                                                
         TM    MISCFLG1,MF1KYOPT                                                
         BNZ   PWS10A                                                           
         TM    MISCFLG2,MF2RRDCH                                                
         BO    PWS10A                                                           
         CLI   PFKEY,9                                                          
         BNE   PWS10X                                                           
PWS10A   MVI   GOSUBN,BPT#         BUILD PW TABLE                               
         GOTO1 (RF)                                                             
PWS10X   DS    0H                                                               
                                                                                
         MVI   MYERRCD,0                                                        
         MVI   MYWRNCD,0                                                        
         MVI   GOSUBN,GBG#         GET BUYS AND GOAL FOR KEY                    
         GOTO1 (RF)                                                             
         CLI   MYERRCD,0                                                        
         BNE   MYERROR                                                          
         CLI   MYWRNCD,0                                                        
         BNE   MYWARN                                                           
                                                                                
         MVI   GOSUBN,FCO#         FILL THE CLCOST OVRRDES INTO ACCUTAB         
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,FLK#         FILL THE LOCKINS INTO ACCUTAB                
         GOTO1 (RF)                                                             
*****    MVI   GOSUBN,FDC#         FILL THE ADJ DR/CR INTO ACCUTAB              
*****    GOTO1 (RF)                                                             
         MVI   GOSUBN,FOWP#        FILL THE OOW PW INFO INTO ACCUTAB            
         GOTO1 (RF)                                                             
         MVI   GOSUBN,SUM#         SUM UP ACCUTAB                               
         GOTO1 (RF)                                                             
         MVC   SVUSETAX,USETAX     SAVE OPTIONS VALUES                          
         MVC   SVSHOWBA,SHOWBA                                                  
         B     PWSX                                                             
*                                                                               
PWS30    MVI   GOSUBN,RTI#         RESTORE TIA                                  
         GOTO1 (RF)                                                             
         MVI   GOSUBN,RFG#         RESET FLAGS IN TABLES                        
         GOTO1 (RF)                                                             
*                                                                               
PWSX     B     XIT                                                              
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE SORTS THE ENTRIES IN ACCUTAB                                     
***********************************************************************         
SORTACTB NTR1                                                                   
         L     R4,AACCUTAB         COUNT THE NUMBER OF ENTRIES TO SORT          
         XR    RF,RF               RF = NUMBER OF ENTRIES                       
         USING ACCUTABD,R4                                                      
SACTB10  TM    ATFLAG,ATFSTTLQ     SCHEDULE ENTRY?                              
         BNZ   SACTB20             YES, DON'T NEED TO SORT THOSE                
         LA    RF,1(RF)                                                         
         LA    R4,ACCUTABQ(R4)                                                  
         B     SACTB10                                                          
*                                                                               
SACTB20  CHI   RF,1                MORE THAN 1 ENTRY?                           
         BNH   SACTBX              NO, NOTHING TO SORT                          
         GOTO1 XSORT,DMCB,(0,AACCUTAB),(RF),ACCUTABQ,3,0                        
*                                                                               
SACTBX   B     XIT                                                              
         EJECT                                                                  
*------------------------- VALIDATE OPTIONS --------------------------*         
VALOPTS  NTR1                                                                   
         LA    R2,C2MOPTNH                                                      
         TM    MISCFLG1,MF1ERRQ    WAS THERE A PREVIOUS ERROR?                  
         BZ    VLOP03               NO                                          
         LH    R0,PRVFDSP                                                       
         A     R0,ATWA                                                          
         CR    R0,R2                YES, WAS IT IN OPTIONS FIELD?               
         BNE   VLOP03                                                           
         BAS   RE,CLRERRS            YES, CLEAR ERROR CODES                     
         NI    MISCFLG1,XFF-MF1ERRQ   AND FLAG                                  
*                                                                               
VLOP03   DS    0H                  SET DEFAULT OPTION VALUES                    
         MVI   USETAX,C'Y'                                                      
         MVI   SHOWBA,C'Y'                                                      
         MVI   OPTUSED,0           CLEAR OPTIONS USED FLAG                      
         NI    MISCFLG1,XFF-MF1OPTN                                             
                                                                                
         TM    4(R2),X80           INPUT THIS TIME?                             
         BZ    VLOP05                                                           
         OI    MISCFLG1,MF1OPTN     YEP, TURN ON FLAG                           
*                                                                               
VLOP05   CLI   5(R2),0             ANY OPTIONS INPUTTED?                        
         BE    VALOPTX              NO                                          
         GOTO1 SCANNER,DMCB,(R2),(X'81',AIO3),0                                 
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    INVLFLD              YEP, UNFORTUNATELY                          
         MVC   LNCNTDWN,4(R1)      USE LNCNTDWN FOR LOOP COUNTER                
         L     R3,AIO3                                                          
*                                                                               
VLOP10   L     R4,AOPTABLE                                                      
         USING OPTDSECT,R4                                                      
VLOP10A  CLI   0(R4),EOT           END OF TABLE?                                
         BE    INVOPERR             YES, CAN'T MATCH INPUTTED OPTION            
                                                                                
         CLC   OPTNAME(1),0(R3)    MATCH L(KEYWORD)                             
         BNE   VLOPBUMP                                                         
                                                                                
         ZIC   R1,OPTNAME          MATCH KEYWORD                                
         BCTR  R1,0                                                             
         EXCLC R1,OPTNAME+1,12(R3)                                              
         BNE   VLOPBUMP                                                         
                                                                                
         MVC   BYTE,OPTUSED        KEYWORD FOUND                                
         NC    BYTE,OPTBIT          WAS IT SPECIFIED ALREADY?                   
         BNZ   DUPOPERR             YES, ERROR                                  
         LA    RF,OPTNAME+2(R1)    RF-->START OF VALUES                         
         TM    OPTFLAG,OPFOTHAB    FOUND KEYWORD, CHECK VALUES                  
         BZ    VLOP15               VALUES IN ANOTHER TABLE                     
         ZICM  R0,0(RF),(3)                                                     
         A     R0,BASE1                                                         
         LR    RF,R0               RF-->VALUES IN SEPARATE TABLE                
*                                                                               
VLOP15   DS    0H                                                               
         TM    OPTFLAG,OPFKYWRD    OPTION SPECIFIABLE BY KEYWORD ONLY           
         BO    VLOP20                                                           
                                                                                
VLOP15A  CLI   0(RF),EOT           AT END OF VALUES TABLE?                      
         BE    OPDTAERR             YEP, VALUE INPUTTED IS INVALID              
         CLC   1(1,R3),0(RF)       MATCH L(VALUE) INPUTTED                      
         BH    VLOP15B                                                          
         ZIC   R1,1(R3)            R1=L(INPUTTED VALUE)                         
         BCTR  R1,0                                                             
         EXCLC R1,1(RF),22(R3)                                                  
         BE    VLOP20                                                           
VLOP15B  ZIC   R1,0(RF)                                                         
         LA    RF,1(RF,R1)         BUMP TO NEXT OPTION VALUE                    
         B     VLOP15A                                                          
*                                                                               
VLOP20   ZIC   R1,OPTOLEN          USER'S INPUTTED VALUE FOUND                  
         BCTR  R1,0                 HOLD ONTO TO IT FOR LATER USE               
         ZICM  RE,OPTOADDR,(3)                                                  
         A     RE,ASYSD            RE-->OUTPUT FIELD                            
         EXMVC R1,0(RE),1(RF)                                                   
         OC    OPTUSED,OPTBIT                                                   
                                                                                
         ZIC   R1,LNCNTDWN         ANY MORE SCANNER ENTRIES?                    
         SH    R1,=H'1'                                                         
         BZ    VALOPTX              NOPE, FINISHED W/ OPTIONS                   
         STC   R1,LNCNTDWN                                                      
         LA    R3,32(R3)            YES, BUMP TO NEXT SCANNER ENTRY             
         B     VLOP10                                                           
*                                                                               
VLOPBUMP ZIC   R0,OPTLEN                                                        
         AR    R4,R0                                                            
         B     VLOP10A                                                          
*                                                                               
VALOPTX  MVC   SVOPTFLD,C2MOPTN    SAVE OPTIONS INPUTTED                        
         B     XIT                                                              
         DROP  R4                                                               
                                                                                
*                                                                               
** VALOPT ERRORS **                                                             
*                                                                               
INVOPERR DS    0H                                                               
         MVI   MYERRCD,INVOQ                                                    
         B     MYERROR                                                          
                                                                                
DUPOPERR DS    0H                                                               
         MVI   MYERRCD,DUPOQ                                                    
         B     MYERROR                                                          
                                                                                
OPDTAERR DS    0H                                                               
         MVI   MYERRCD,IOPDQ                                                    
         B     MYERROR                                                          
                                                                                
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY (VALREC)'           
***********************************************************************         
*========================== VALIDATE RECORD ==========================*         
VR       DS    0H                                                               
         XC    ACURFORC,ACURFORC                                                
         NI    MISCFLG2,XFF-MF2RRDCH                                            
*****    BAS   RE,SAVEACTB                                                      
         MVI   GOSUBN,STI#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
VR02     TM    MISCFLG1,MF1KYOPT   DID KEY OR OPTION CHANGE?                    
         BNZ   *+12                YEP, GO DISPLAY RECORD                       
         TM    MISCFLG3,MF3GDPLY   GO STRAIGHT TO DISPLAY LOGIC?                
         BZ    VR03                NO                                           
         MVI   PFKEY,0                                                          
         B     DR                  DISPLAY RECD & IGNORE PFKEY                  
*                                                                               
VR03     CLI   PFKEY,0             PFKEY HIT?                                   
         BE    VR05                 NO, CHECK FOR ANYTHING MODIFIED             
         CLI   PFKEY,2             WAS PF2 HIT?                                 
         BE    VR05                                                             
         CLI   PFKEY,3             WAS PF3 HIT?                                 
         BE    VR05                                                             
         TM    MISCFLG2,MF2GOGRC   GOAL OR GRP CHANGED IN GOAL RECD?            
         BNZ   VR400                                                            
         B     DR                   NO, REFORMAT DISPLAY                        
*                                                                               
** Station Lock (Request ID) **                                                 
*                                                                               
VR05     LA    R2,C2MRQIDH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR05X                NOPE, SKIP THIS PART                        
         TM    LOCKFLAG,LKFBYPWQ   BOTH BUY & PW LOCKED?                        
         BNO   NOTLOCKE             NOPE                                        
         CLI   USETAX,C'N'         CAN'T LOCK WHEN TAX=NO                       
         BE    NLKNTXE                                                          
         OC    BSTA,BSTA            OR STATION GIVEN FROM DDS TERMINAL          
         BNZ   NLKSTAE                                                          
                                                                                
         DS    0H                  CHECK FOR INVALID FIELD ERROR                
         CLI   5(R2),2              NEED AT LEAST 2-CHAR ID                     
         BL    INVLFLD                                                          
         MVI   GOSUBN,CAN#          CHECK IF INPUT IS ALPHANUMERIC              
         GOTO1 AGOSUB                                                           
         BNE   INVLFLD                                                          
         CLI   8(R2),C'0'           1ST CHAR MUST BE ALPHABETIC                 
         BNL   INVLFLD                                                          
                                                                                
         DS    0H                  CHECK IF OKAY TO UPDATE FILE                 
         MVI   GOSUBN,OKUPD#                                                    
         GOTO1 AGOSUB               OKAY TO UPDATE FILE?                        
         BE    VR05UOK                                                          
         LA    R0,*                                                             
         ST    R0,ALOCERR            NO, SET ADDR WHERE ERROR IS CAUGHT         
         B     XCUERE                                                           
VR05UOK  EQU   *                                                                
                                                                                
         ZIC   R1,5(R2)            GET REQUEST ID                               
         BCTR  R1,0                                                             
         EXMVC R1,REQID,8(R2)                                                   
         MVI   GOSUBN,GRQ#          AND GENERATE REQUEST                        
         GOTO1 AGOSUB                                                           
                                                                                
         OI    MISCFLG2,MF2RQADD   STATION LOCKIN REQUEST ADDED                 
         MVC   DTIELEM+(PWDTISLD-PWDTIEL)(L'PWDTISLD),CTODAY                    
         MVC   DTIELEM+(PWDTISLI-PWDTIEL)(L'PWDTISLI),REQID                     
*                                                                               
VR05X    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*  BECAUSE IT IS EASIER TO WORK FROM THE ACCUMULATOR TABLE, THE                 
*   MODIFY BITS ARE TRANSFERRED FROM THE TWA TO ACCUTAB.  FROM                  
*   THERE, THE PRIORITY RULES FOR VALIDATING SCHEDULE, MONTHLY, AND             
*   WEEKLY VALUES INPUTTED ARE FOLLOWED.                                        
***********************************************************************         
VR20     DS    0H                                                               
         NI    MISCFLG1,XFF-MF1MODFY   ASSUME NOTHING MODIFIED                  
         USING MSLDSECT,R2                                                      
*                                                                               
** CHECK SCHEDULE TOTALS LINE MODIFIED FIRST **                                 
*                                                                               
VR100    LA    R2,C2MSTTLH         CHECK IF SCHED LINE MODIFIED                 
         BAS   RE,LOCSKED          GET R4 TO SCHED ENTRY                        
         USING ACCUTABD,R4                                                      
         LA    R4,ACCUTABQ(R4)      BUMP PAST SKED'S BILL ADJ ENTRY             
*                                                                               
         TM    MSLCCOSH+4,X80      WAS ADJBUY INPUTTED THIS TIME?               
         BZ    *+8                  NOPE                                        
         OI    ATFLAG,ATFAJBQ       YEP, TURN FLAG ON                           
*                                                                               
         TM    MSLCOS2H+4,X80       WAS  CS2  INPUTTED THIS TIME?               
         BZ    *+8                  NOPE                                        
         OI    ATFLAG,ATFPWQ        YEP, TURN FLAG ON                           
*                                                                               
         TM    ATFLAG,ATFAJBQ+ATFPWQ                                            
         BO    EAJBPWE             CAN'T MODIFY ADJBUY & CS2                    
         BZ    VR400               CHECK MONTH IF NOTHING MODIFIED              
                                                                                
         OI    MISCFLG1,MF1MDFY    FLAG SOMETHING WAS MODIFIED                  
         TM    ATFLAG,ATFAJBQ                                                   
         BO    VR110                                                            
                                                                                
         BAS   RE,MODSPW           PROCESS SCHED CS2 MODIFIED                   
         BE    VR400                NO ERROR OCCURRED                           
         B     BADPWE               ERROR OCCURRED                              
*                                                                               
VR110    BAS   RE,MODSAJB          PROCESS SCHED ADJBUY MODIFIED                
         BE    VR400                NO ERROR OCCURRED                           
         B     BADAJBE              ERROR OCCURRED                              
         EJECT                                                                  
*&&DO                                                                           
VR200    B     VR300                                                            
*                                                                               
** CHECK MONTHLY LINE MODIFIED SECOND **                                        
*                                                                               
         LA    R2,C2MSTATH                                                      
         MVC   DATE4,MNTHMRKS                                                   
         BAS   RE,LOC1WMTH         GET R4 TO 1ST WK OF MTH W/IN ACCUTAB         
*                                                                               
VR210    DS    0H                                                               
         TM    ATFLAG2,ATF2DPLY    IS THIS LINE DISPLAYED?                      
         BZ    VR240A               NO, BUMP TO KEEP IN SYNC W/ DISPLAY         
                                                                                
         TM    ATFLAG,ATFSTTLQ     IF SCHED TOTAL LINE REACHED,                 
         BO    VR250                FINISHED CHECKING MONTH LINES               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BO    VR240                                                            
         TM    ATFLAG,ATFMTTLQ     GET A MONTHLY TOTALS LINE                    
         BZ    VR240                                                            
                                                                                
         TM    ATFLAG,ATFAJBQ+ATFPWQ                                            
         BO    EAJBPWE             CAN'T MODIFY ADJBUY & CS2                    
         BZ    VR240               GOTO NEXT LINE IF NOTHING MODIFIED           
                                                                                
         OI    MISCFLG1,MF1MDFY    FLAG SOMETHING WAS MODIFIED                  
         TM    ATFLAG,ATFAJBQ                                                   
         BO    VR220                                                            
                                                                                
         BAS   RE,MODMPW           PROCESS MONTHLY CS2 MODIFIED                 
         BE    VR240                NO ERROR OCCURRED                           
         BNE   BADPWE               ERROR OCCURRED                              
*                                                                               
VR220    BAS   RE,MODMAJB          PROCESS MONTHLY ADJBUY MODIFIED              
         BE    VR240                NO ERROR OCCURRED                           
         BNE   BADAJBE                                                          
*                                                                               
VR240    DS    0H                  BUMP SCREEN POINTER                          
         LA    R2,MSLLENQ(R2)                                                   
         CLC   MSLSTAT+1(9),=C'(MORE...)'                                       
         BE    VR250                                                            
         OC    MSLSTAT,MSLSTAT                                                  
         BZ    VR250                                                            
                                                                                
VR240A   DS    0H                  BUMP ACCUTAB POINTER                         
         LA    R4,ACCUTABQ(R4)                                                  
         B     VR210                                                            
*                                                                               
VR250    TM    MISCFLG1,MF1MDFY                                                 
         BO    VR400                                                            
         B     VR270                                                            
         EJECT                                                                  
*                                                                               
** CHECK MONTHLY DR/CR MODIFIED NEXT **                                         
*                                                                               
VR270    DS    0H                                                               
         TM    MISCFLG2,MF2BILAJ   ANY BILL ADJ LINES SHOWN?                    
         BZ    VR300                NOPE, NO DR/CR TO CHECK                     
                                                                                
         LA    R2,C2MSTATH                                                      
         MVC   DATE4,MNTHMRKS                                                   
         BAS   RE,LOC1WMTH         GET R4 TO MONTH W/IN ACCUTAB                 
                                                                                
VR272    DS    0H                  BUMP POINTERS TO DR/CR LINES                 
         TM    ATFLAG2,ATF2DPLY                                                 
         BZ    VR273A                                                           
                                                                                
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    VR278                                                            
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   VR273                                                            
                                                                                
         DS    0H                  R2-->SCREEN, R4-->ACCUTAB  BILL ADJ          
         TM    ATFLAG,ATFAJBQ      WAS IT MODIFIED                              
         BZ    VR273                NOPE, KEEP BUMPING                          
                                                                                
         OI    MISCFLG1,MF1MDFY    YES, FLAG SOMETHING WAS MODIFIED             
         BAS   RE,MODDRCR                                                       
         BNE   BADDRCR                                                          
*                                                                               
VR273    DS    0H                  BUMP SCREEN POINTER                          
         LA    R2,MSLLENQ(R2)                                                   
         CLC   MSLSTAT+1(9),=C'(more...)'                                       
         BE    VR278                                                            
         OC    MSLSTAT,MSLSTAT                                                  
         BZ    VR278                                                            
                                                                                
VR273A   DS    0H                  BUMP ACCUTAB POINTER                         
         LA    R4,ACCUTABQ(R4)                                                  
         B     VR272                                                            
*                                                                               
VR278    TM    MISCFLG1,MF1MDFY                                                 
         BO    VR400                                                            
         B     VR300                                                            
         EJECT                                                                  
*                                                                               
** CHECK WEEKLY LINE MODIFIED LAST **                                           
VR300    B     VR400                                                            
*                                                                               
         LA    R2,C2MSTATH         R2-->1ST WK OF 1ST MTH ON SCREEN             
         USING MSLDSECT,R2                                                      
         MVC   DATE4,MNTHMRKS                                                   
         BAS   RE,LOC1WMTH         GET R4 TO SAME 1ST WK IN ACCUTAB             
         MVC   LNCNTDWN,NUMLNDSP   GET # OF LINES DISPLAYED                     
         CLI   LNCNTDWN,0                                                       
         BE    VR400                                                            
*                                                                               
VR310    DS    0H                  START OF LOOP                                
         TM    ATFLAG,ATFSTTLQ+ATFMTTLQ                                         
         BNZ   VR340                                                            
         TM    ATFLAG,ATFAJBQ+ATFPWQ                                            
         BO    EAJBPWE             CAN'T MODIFY ADJBUY & CS2                    
         BZ    VR340               GOTO NEXT LINE IF NOTHING MODIFIED           
                                                                                
         OI    MISCFLG1,MF1MDFY    FLAG SOMETHING WAS MODIFIED                  
         LA    RF,MODWAJB           MODIFIED ADJBUY ROUTINE                     
         LA    R0,BADAJBE           ADJBUY INPUT ERROR ROUTINE                  
         TM    ATFLAG,ATFAJBQ      WAS ADJBUY MODIFIED?                         
         BO    VR320                YEP                                         
         TM    ESTFLAG,EFOWPW       OUT-OF-WEEK PW BILLING ESTIMATE:            
         BO    NCOWPE                CAN'T CHANGE CS2                           
         LA    RF,MODWPW            MODIFIED CS2 ROUTINE                        
         LA    R0,BADPWE            CS2 INPUT ERROR ROUTINE                     
                                                                                
VR320    DS    0H                  GOTO ROUTINE TO PROCESS                      
         BASR  RE,RF                MODIFIED VALUE                              
         BE    VR340               NO ERROR OCCURRED                            
         LR    RF,R0                                                            
         BR    RF                  ERROR OCCURRED                               
*                                                                               
VR340    DS    0H                  BUMP POINTERS                                
         ZIC   R1,LNCNTDWN          AND DECREMENT COUNTERS                      
         SH    R1,=H'1'                                                         
         BZ    VR400                                                            
         BP    *+6                                                              
         DC    H'0'                R1 SHOULD NOT BE NEGATIVE                    
                                                                                
         STC   R1,LNCNTDWN                                                      
         LA    R2,MSLLENQ(R2)                                                   
VR342    LA    R4,ACCUTABQ(R4)                                                  
         CLC   =X'FFFFFF',0(RF)    CAN'T JUST USE X'FF' ANYMORE                 
         BNE   *+6                                                              
         DC    H'0'                CABLE STATIONS CAN BEGIN WITH X'FF'          
         TM    ATFLAG2,ATF2DPLY                                                 
         BZ    VR342                                                            
         B     VR310                                                            
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------- UPDATE FILE ----------------------------*         
*&&                                                                             
VR400    DS    0H                                                               
         MVI   GOSUBN,OKUPD#                                                    
         GOTO1 AGOSUB              OKAY TO UPDATE FILE?                         
         BE    VR400OK                                                          
         LA    R0,*                                                             
         ST    R0,ALOCERR           NO, SET ADDR WHERE ERROR IS CAUGHT          
         B     XCUERE                                                           
VR400OK  EQU   *                                                                
*                                                                               
         TM    MISCFLG1,MF1MODFY   IF SOMETHING MODIFIED,                       
         BZ    VR402                                                            
         CLI   PFKEY,2              AND DON'T ALLOW USER TO PF2,                
         BE    NMODUPD0                                                         
         CLI   PFKEY,3              NOR PF3 AT THE SAME TIME,                   
         BE    NMODUPD0                                                         
                                                                                
         MVI   GOSUBN,UPT#         ELSE, UPDATE PWTAB FOR NEW CS2%s             
         GOTO1 AGOSUB                                                           
         B     VR410               GO UPDATE FILE NOW                           
*                                                                               
VR402    CLI   PFKEY,2             NOTHING MODIFIED BUT PF2  HIT,               
         BNE   VR404                                                            
         XI    LOCKFLAG,LKFUBLKQ    SO LOCK/UNLOCK BUY                          
         B     VR410               GO UPDATE FILES                              
*                                                                               
VR404    CLI   PFKEY,3             NOTHING MODIFIED BUT PF3  HIT,               
         BNE   VR406                                                            
         XI    LOCKFLAG,LKFUPLKQ    SO LOCK/UNLOCK PW                           
         B     VR410               GO UPDATE FILES                              
*                                                                               
VR406    DS    0H                                                               
         TM    LOCKFLAG,LKFCOLKQ   NOTHING MODIFIED, BUT IF 'LOCK'              
         BO    VR410                ENTERRED, UPDATE LOCKIN ELEMENTS            
         TM    MISCFLG2,MF2RQADD   STATION LOCKIN REQUESTED?                    
         BO    VR410                YES, NEED TO UPDATE PWDTIEL ELEM            
         TM    MISCFLG2,MF2GOGRC   GOAL OR GRP CHANGED IN GOAL RECD?            
         BNZ   VR410                YEP, GO UPDATE PW RECORD                    
*                                                                               
VR408    DS    0H                                                               
         B     VRX                 NONE OF THE ABOVE                            
         EJECT                                                                  
*                                                                               
** MARKET-LEVEL PW RECORD **                                                    
*                                                                               
VR410    MVI   RDUPDATE,C'Y'       WANT TO UPDATE FILE NOW                      
         MVI   GOSUBN,GPR#         GET PW RECORD (KEY IS BUILT TOO)             
         GOTO1 AGOSUB                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
*                                                                               
         MVC   PWGNGOAL,SKEDCGTX   ALWAYS WRITE SKED GOAL (W/ TAX) &            
         MVC   PWGNGRP,PWGRP        GRP TO FILE                                 
         NI    PWGNFLG,XFF-PWGNUGOL-PWGNUGRP                                    
         TM    MISCFLG3,MF3UGOAL                                                
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNUGOL     USER INPUTTED GOAL AMOUNT                   
         TM    MISCFLG3,MF3UGRP                                                 
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNUGRP     USER INPUTTED GRP  AMOUNT                   
                                                                                
         CLI   PFKEY,2             CHANGE BUY LOCK STATUS?                      
         BE    VR410A10                                                         
         CLI   PFKEY,3             CHANGE PW  LOCK STATUS?                      
         BE    VR410B                                                           
         TM    MISCFLG1,MF1ADNOW   SPECIAL OVERRIDE TO ADD PW RECD?             
         BO    VR410C                                                           
         TM    MISCFLG1,MF1MODFY   UPDATE PW RECORDS?                           
         BNZ   VR410D                                                           
         TM    LOCKFLAG,LKFCOLKQ   COPY COSTS TO LOCK COLUMN?                   
         BO    VR415                                                            
*                                                                               
         TM    MISCFLG2,MF2RQADD   STATION LOCKIN REQUEST?                      
         BNO   VR410A00                                                         
         MVI   GOSUBN,LKC#         COPY COSTS OVER TO LOCKINS COLUMN            
         GOTO1 AGOSUB                                                           
         BAS   RE,UPDATEPW                                                      
         B     VR440B                                                           
*                                                                               
VR410A00 TM    MISCFLG2,MF2GOGRC   GOAL/GRP IN GOAL RECD CHANGED?               
         BNZ   VR440B                                                           
         DC    H'0'                                                             
*                                                                               
VR410A10 NI    PWGNFLG,XFF-PWGNBILQ       UPDATE X'01' ELEMENT                  
         TM    LOCKFLAG,LKFUBLKQ                                                
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNBILQ+PWGNBPLK   FOR BUY LOCK/UNLOCK                  
                                                                                
         LA    RF,DTIELEM                                                       
         USING PWDTIEL,RF                                                       
         TM    LOCKFLAG,LKFUBLKQ          BUY BEING LOCKED?                     
         BZ    VR410BF                     NOPE                                 
         CLC   PWDTISLD,CTODAY             YEP, SAME DAY AS STTN-LOCK?          
         BNE   VR410BF                      NO                                  
         MVC   PWDTISLD,RELOCKSW            YES, NEED STTN RELOCK               
VR410BF  EQU   *                                                                
         DROP  RF                                                               
                                                                                
         MVC   DTIELEM+(PWDTIBLD-PWDTIEL)(L'PWDTIBLD),CTODAY                    
         B     VR430                                                            
*                                                                               
VR410B   NI    PWGNFLG,XFF-PWGNPLKQ       UPDATE X'01' ELEMENT                  
         TM    LOCKFLAG,LKFUPLKQ                                                
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNPLKQ+PWGNBPLK   FOR PW  LOCK/UNLOCK                  
         MVC   DTIELEM+(PWDTIPLD-PWDTIEL)(L'PWDTIPLD),CTODAY                    
         B     VR430                                                            
*                                                                               
VR410C   DS    0H                                                               
         TM    LOCKFLAG,LKFBYPWQ                                                
         BNO   *+8                                                              
         OI    PWGNFLG,PWGNPLKQ+PWGNBPLK                                        
         B     VR410D                                                           
*                                                                               
VR410D   DS    0H                                                               
         MVC   PWGNTAX,GLTXRATE     WRITE TAX TO RECORD                         
*                                                                               
VR415    BAS   RE,UPDATEPW                                                      
*                                                                               
VR427    TM    MISCFLG1,MF1ADNOW   IF WE WANT TO ADD A PW RECD NOW              
         BZ    VR430                                                            
         GOTO1 ADDREC               GO ADD IT                                   
*                                                                               
         MVI   GOSUBN,PPR#         COPY NEW RECORD INTO STORAGE                 
         GOTO1 AGOSUB                                                           
         B     VKPW10A                                                          
         DROP  R4                                                               
*                                                                               
VR430    DS    0H                  UPDATE DOLLAR (LOCKIN) ELEMENTS              
         OI    MISCFLG2,MF2RRDCH                                                
         TM    LOCKFLAG,LKFCOLKQ   IF 'LOCK' ENTERRED,                          
         BO    VR430A               GO STRAIGHT TO UPDATE LOGIC                 
         TM    LOCKFLAG,LKFBPLK2   IF BUY OR PW HAS BEEN PREVSLY LCKED          
         BO    VR440A               DON'T UPDATE DOLLAR ELEMENTS                
         TM    LOCKFLAG,LKFBYPWQ   POTENTIAL 1ST TIME LOCK                      
         BZ    VR440A               ...NOT!  DON'T UPDATE ELEMENTS              
*                                                                               
VR430A   DS    0H                  DELETE BACKUP DOL ELEMS FROM RECD            
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,STACREC                                                       
         USING STACRECD,R4                                                      
                                                                                
VR435A   DS    0H                                                               
         XC    STACRECD(STACRECL),STACRECD                                      
         MVI   GOSUBN,TSR_RDH#                                                  
VR435B   GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     EOF IN STTN ACCUM TABLE?                     
         BNZ   VR438                YES, CHK FOR CLCOST OVERRIDES NEXT          
         TM    STACFLG,STAFMKTQ    ENTRY PROCESSED ALREADY?                     
         BZ    VR435C               NOPE, GO ADD ELEM FOR ENTRY                 
         MVI   GOSUBN,TSR_NXT#                                                  
         B     VR435B               YEP, TRY NEXT ENTRY                         
*                                                                               
VR435C   DS    0H                                                               
         LA    R3,ELEMENT                                                       
         USING PWDOLEL,R3                                                       
                                                                                
         MVI   PWDOLCD,PWDOLCDQ    ELEM CODE                                    
         MVI   PWDOLLEN,PWDOLLNQ   ELEM LENGTH                                  
*****    MVC   PWDOLWK,STACSTRT    WEEK DATE                                    
                                                                                
VR435E   DS    0H                                                               
         ICM   R0,15,PWDOLSPT      UPDATE # OF SPOTS                            
         ICM   R1,15,STACSPT                                                    
         AR    R0,R1                                                            
         STCM  R0,15,PWDOLSPT                                                   
         ST    R6,FULL             A(MKT-LEVEL PW RECD) FOR UPNSPT              
         BAS   RE,UPNSPT           SHOULD WE UPDATE # OF SPOTS                  
         BE    *+10                 YEP                                         
         MVC   PWDOLSPT,TEMPNSPT    NOPE, RESTORE IT                            
                                                                                
         ICM   R0,15,PWDOLWG       UPDATE WIM GROSS $                           
         ICM   R1,15,STACGRS                                                    
         AR    R0,R1                                                            
         STCM  R0,15,PWDOLWG                                                    
                                                                                
         ICM   R0,15,PWDOLWN       UPDATE WIM NET   $                           
         ICM   R1,15,STACNET                                                    
         AR    R0,R1                                                            
         STCM  R0,15,PWDOLWN                                                    
                                                                                
         ICM   R0,15,PWDOLTAX      UPDATE WIM TAX   $                           
         ICM   R1,15,STACTAX                                                    
         AR    R0,R1                                                            
         STCM  R0,15,PWDOLTAX                                                   
                                                                                
         ICM   R0,15,PWDOLCTX      UPDATE CLT TAX   $                           
         ICM   R1,15,STACCTX                                                    
         AR    R0,R1                                                            
         STCM  R0,15,PWDOLCTX                                                   
                                                                                
         ICM   R0,15,PWDOLCG       UPDATE CLT GROSS $                           
         ICM   R1,15,STACCGRS                                                   
         AR    R0,R1                                                            
         STCM  R0,15,PWDOLCG                                                    
                                                                                
         ICM   R0,15,PWDOLCN       UPDATE CLT NET $                             
         ICM   R1,15,STACCNET                                                   
         AR    R0,R1                                                            
         STCM  R0,15,PWDOLCN                                                    
                                                                                
         OI    STACFLG,STAFMKTQ    MARK FOR PROCESSED ALREADY                   
         MVI   GOSUBN,TSR_PUT#      AND PUT STTN ACCUM RECD BACK                
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VR435G                                                           
                                                                                
VR435F   DS    0H                                                               
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     NO MORE ENTRIES W/ SAME WEEK                 
         BNZ   VR436                GO ADD ELEMENT                              
*****    CLC   STACSTRT,PWDOLWK    SAME WEEK DATE AS IN ELEMENT?                
*****    BNE   VR435G               NO, TRY NEXT ENTRY                          
         TM    STACFLG,STAFMKTQ     YES, WAS IT PROCESSED?                      
         BZ    VR435E                NO, UPDATE ELEMENT W/ ENTRY                
         DC    H'0'                  YEAH (SIGH!)                               
VR435G   DS    0H                                                               
         MVI   GOSUBN,TSR_NXT#                                                  
         B     VR435F                                                           
*                                                                               
VR436    DS    0H                                                               
         B     VR435A                                                           
*                                                                               
VR438    DS    0H                  CHECK FOR CLCOST OVERRIDES                   
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
VR440    MVI   GOSUBN,LKC#         COPY COSTS OVER TO LOCKINS COLUMN            
         GOTO1 AGOSUB                                                           
VR440A   DS    0H                                                               
VR440B   DS    0H                  PWDTIEL ELEMENT                              
         MVI   ELCODE,PWDTICDQ                                                  
         MVI   ELEMENT,0            NO SEARCH ARGUMENTS                         
         MVI   GOSUBN,DLM#                                                      
         GOTO1 AGOSUB              DELETE PWDTIEL ELEMS FROM PW RECD            
         DS    0H                   AND PUT NEW ONE IN                          
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),DTIELEM,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&DO                                                                           
         MVI   GOSUBN,DCD#         DO CURRENT$ ELEMENTS                         
         GOTO1 AGOSUB                                                           
*&&                                                                             
         TM    MISCFLG1,MF1ADNOW                                                
         BO    *+12                                                             
         CLI   MYIOFLAG,IOFADD     UPDATE FILE                                  
         BNE   VR445                                                            
         GOTO1 ADDREC                                                           
         MVI   MYIOFLAG,IOFWRITE                                                
         B     VR448                                                            
VR445    CLI   MYIOFLAG,IOFWRITE                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,UPDATEPW                                                      
****                                                                            
         TM    MISCFLG2,MF2BUY0Q   NO BUYS?                                     
         BZ    VR447                                                            
         TM    MISCFLG2,MF2RQADD     AND DOING A STATION LOCKIN?                
         BZ    VR447                                                            
         GOTO1 HIGH                YES, DELETE THE MKT-LEVEL RECORD             
         OI    KEY+L'PWFKEY,X80                                                 
         GOTO1 WRITE                                                            
         L     RE,AIO                                                           
         OI    PWCNTL-PWFKEY(RE),X80   DELETE THIS RECORD                       
****                                                                            
VR447    GOTO1 PUTREC                                                           
*                                                                               
VR448    DS    0H                  COPY MKT-LEVEL PW RECD INTO IO3              
         L     R0,AIO3                                                          
         LA    R1,L'IO                                                          
         L     RE,AIO                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AMYC2REC                                                      
         LA    R1,L'IO                                                          
         L     RE,AIO                                                           
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   GOSUBN,PPR#         COPY MKT-LEVEL PW RECD TO STORAGE            
         GOTO1 AGOSUB                                                           
*                                                                               
         TM    MISCFLG1,MF1MDFY    CLCOST OR COST2 CHANGED?                     
         BZ    *+8                                                              
         BAS   RE,CS2BUYS              PUT COST2 FACTOR TO THE BUYLINES         
         EJECT                                                                  
*                                                                               
** STATION-LEVEL PW RECORD **                                                   
VR450    TM    MISCFLG2,MF2RQADD   STATION LOCKIN?                              
         BZ    VR500               NO, THEN NO NEED TO LOOK AT STATIONS         
*                                                                               
         LA    R6,KEY                                                           
         USING PWRECD,R6                                                        
         MVI   GOSUBN,BPK#         BUILD PWKEY                                  
         GOTO1 AGOSUB                                                           
         MVI   PWKSTA+2,1           TO READ STATION-LEVEL PW RECD               
         OC    BSTA,BSTA           IF DDS TESTING                               
         BZ    VR450A                                                           
         MVC   PWKSTA,BSTA          READ FOR SPECIFIC STATION                   
*        TM    PWKSTA,X'F0'        IF CABLE STATION,                            
*        BNO   *+10                                                             
         CLI   MYCANADA,C'C'        CANADIAN?                                   
         BE    VR450A               YES                                         
         CLI   PWKSTA,X'E8'        IF CABLE STATION,                            
         BL    *+10                                                             
         NC    PWKSTA,CBLSCMSK       KEEP SYSCODE & IGNORE NETWORK              
*                                                                               
VR450A   GOTO1 HIGH                                                             
         B     VR451B                                                           
VR451    GOTO1 SEQ                                                              
*                                                                               
VR451B   CLC   KEY(PKYMKTL),KEYSAVE   IF KEY HAS DIFF MKT,                      
         BNE   VR480                   NO MORE STATION LEVEL RECORDS            
         LA    R6,KEY                 RESTORE R6 TO POINT TO KEY                
         OC    BSTA,BSTA              IF DDS TESTING,                           
         BZ    VR453                                                            
         MVC   TEMPBSTA,BSTA                                                    
*        TM    TEMPBSTA,X'F0'                                                   
*        BNO   *+10                                                             
         CLI   MYCANADA,C'C'        CANADIAN?                                   
         BE    VR451C               YES                                         
         CLI   TEMPBSTA,X'E8'                                                   
         BL    *+10                                                             
         NC    TEMPBSTA,CBLSCMSK                                                
*                                                                               
VR451C   CLC   PWKSTA,TEMPBSTA         AND STATION NOT FOUND,                   
         BNE   VR480                   GO ADD IT                                
*                                                                               
VR453    DS    0H                  SEE IF STATION IS IN TABLE                   
         L     R4,TSARBLK+(TSAREC-TSARD)                                        
         LA    R4,0(R4)                                                         
         USING STACRECD,R4                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVC   STACSTA,PWKSTA                                                   
         MVI   GOSUBN,TSR_RDH#                                                  
                                                                                
VR453L   GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF      EOF OF STTN ACCUM TABLE?                    
         BNZ   VR470                 YES, "DELETE" RECORD                       
         CLC   STACSTA,PWKSTA                                                   
         BE    VR454                YES IT IS, BUILD ELEMENTS                   
         MVI   GOSUBN,TSR_NXT#                                                  
         B     VR453L               ELSE, KEEP LOOKING IN TABLE                 
*                                                                               
VR454    GOTO1 GETREC                                                           
         L     R6,AIO                R6 USED BY PWRECD                          
         NI    MISCFLG2,XFF-MF2DELEL NO ELEMS DELETED YET                       
*                                                                               
*****    MVC   PWGNGOAL,SKEDCGTX   ALWAYS WRITE SKED GOAL (W/ TAX) &            
*****    MVC   PWGNGRP,PWGRP         AND  GRP TO FILE                           
         NI    PWGNFLG,XFF-PWGNUGOL-PWGNUGRP                                    
         TM    MISCFLG3,MF3UGOAL                                                
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNUGOL      USER INPUTTED GOAL AMOUNT                  
         TM    MISCFLG3,MF3UGRP                                                 
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNUGRP      USER INPUTTED GRP  AMOUNT                  
                                                                                
         CLI   PFKEY,2             CHANGE BUY LOCK STATUS?                      
         BE    VR455                                                            
         CLI   PFKEY,3             CHANGE PW  LOCK STATUS?                      
         BE    VR456                                                            
         TM    MISCFLG1,MF1MODFY   UPDATE PW RECORDS?                           
         BNZ   VR457                                                            
         TM    LOCKFLAG,LKFCOLKQ   COPY COSTS TO LOCK COLUMN?                   
         BO    VR458                                                            
*                                                                               
         TM    MISCFLG2,MF2RQADD   STATION LOCKIN REQUEST?                      
         BNO   VR454A                                                           
         MVI   GOSUBN,LKC#         COPY COSTS OVER TO LOCKINS COLUMN            
         GOTO1 AGOSUB                                                           
         BAS   RE,UPDATEPW                                                      
         B     VR465A                                                           
*                                                                               
VR454A   TM    MISCFLG2,MF2GOGRC   GOAL/GRP IN GOAL RECD CHANGED?               
         BNZ   VR458                                                            
         DC    H'0'                                                             
*                                                                               
VR455    NI    PWGNFLG,XFF-PWGNBILQ  UPDATE X'01' ELEMENT                       
         TM    LOCKFLAG,LKFUBLKQ      IF USER WANTS BUY LOCK,                   
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNBILQ+PWGNBPLK   THEN BUY SHALL BE LOCKED             
         B     VR458                                                            
VR456    NI    PWGNFLG,XFF-PWGNPLKQ  UPDATE X'01' ELEMENT                       
         TM    LOCKFLAG,LKFUPLKQ      IF USER WANTS PW  LOCK,                   
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNPLKQ+PWGNBPLK   THEN PW  SHALL BE LOCKED             
         B     VR458                                                            
*                                                                               
VR457    DS    0H                   SOMETHING MODIFIED, WRITE                   
         MVC   PWGNTAX,GLTXRATE      TAX TO RECORD                              
*                                                                               
VR458    DS    0H                  UPDATE DOLLAR (LOCKIN) ELEMENTS              
         TM    LOCKFLAG,LKFCOLKQ   IF 'LOCK' ENTERRED,                          
         BO    VR460                GO STRAIGHT TO UPDATE LOGIC                 
         TM    MISCFLG2,MF2GOGRC   IF GOALS/GRP (ONLY) HAVE CHANGED,            
         BNZ   VR460C               DON'T TOUCH DOLLAR ELEMENTS                 
         TM    LOCKFLAG,LKFBPLK2   IF BUY OR PW HAS BEEN PREVSLY LCKED,         
         BO    VR459                DON'T UPDATE DOLLAR ELEMENTS                
         TM    LOCKFLAG,LKFBYPWQ   POTENTIAL 1ST TIME LOCK                      
         BNZ   VR460                YES!  GO UPDATE ELEMENTS                    
*                                                                               
VR459    DS    0H                  DOLLAR ELEMS NOT TO BE UPDATED               
         OC    STACSTA,STACSTA      ANY REASON TO MOVE ON?                      
         BZ    VR465                 NO, PUT RECORD BACK NOW                    
         BNZ   VR460C                YES, KEEPING BUMPING THRU STACTAB          
*                                                                               
VR460    DS    0H                                                               
         TM    MISCFLG2,MF2DELEL    ELEMS DELETED YET?                          
         BO    VR460B               YEP                                         
*                                                                               
         MVI   ELCODE,PWDOLCDQ                                                  
         OI    ELCODE,X80                                                       
         MVI   ELEMENT,0            NO SEARCH ARGUMENTS                         
         MVI   GOSUBN,DLM#                                                      
         GOTO1 AGOSUB              DELETE BACKUP DOL ELEMS FROM RECD            
*                                                                               
         DS    0H                  BACKUP CRRNT DOL ELEMS FOR NEW ONES          
         MVI   ELCODE,PWDOLCDQ                                                  
         MVI   GOSUBN,BLM#                                                      
         GOTO1 (RF)                                                             
         OI    MISCFLG2,MF2DELEL   ELEMS HAVE BEEN DELETED                      
*                                                                               
         OC    STACSTA,STACSTA     ANY ELEMENT TO BUILD?                        
         BZ    VR465                NO, RSTR DRCR & PUT RECD BACK NOW           
*                                                                               
VR460B   DS    0H                                                               
*                                                                               
VR460C   OI    STACFLG,STAFPRCQ    ENTRY-PUT-INTO-FILE FLAG                     
         MVI   GOSUBN,TSR_PUT#      AND PUT STTN ACCUM RECD BACK                
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR460D   DS    0H                                                               
         MVI   GOSUBN,TSR_NXT#     GET NEXT STTN ACCUM RECD                     
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     EOF IN STTN ACCUM TABLE?                     
         BNZ   VR465                                                            
                                                                                
         CLC   STACSTA,PWKSTA       NO, GET STATION'S NEXT ENTRY                
         BNE   VR460D                                                           
         TM    STACFLG,STAFPRCQ     AND IT BETTER NOT BE PROCESSED              
         BZ    VR458                                                            
         DC    H'0'                                                             
*                                                                               
VR465    DS    0H                  GOT ALL ENTRIES FOR STATION,                 
         BAS   RE,UPDATEPW                                                      
*                                                                               
VR465A   GOTO1 PUTREC               AND PUT RECORD BACK INTO FILE               
         B     VR451               GET NEXT STATION-LEVEL RECORD                
*                                                                               
VR470    DS    0H                  HAVE PW-STA RECD, NO STACTABD ENTRY          
         GOTO1 GETREC              GO GET THE RECORD FIRST                      
         L     R6,AIO                                                           
*                                                                               
****     TM    LOCKFLAG,LKFCOLKQ    IF NOT LOCKING DOLLARS,                     
****     BZ    VR465A                DO NOT DELETE STTN-LEVEL PW RECD           
         OI    PWCNTL,X80           MARK RECORD                                 
         OI    KEY+L'PWFKEY,X80      AND KEY FOR DELETE,                        
         GOTO1 WRITE                WRITE KEY BACK TO FILE,                     
         B     VR465A               AND PUT "DELETED" RECD BACK                 
         DROP  R4                                                               
*                                                                               
VR480    DS    0H                                                               
         XC    TEMPDRCR,TEMPDRCR                                                
         XC    TEMPBLD,TEMPBLD                                                  
                                                                                
         L     R4,TSARBLK+(TSAREC-TSARD)                                        
         LA    R4,0(R4)                                                         
         USING STACRECD,R4                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVI   GOSUBN,TSR_RDH#                                                  
                                                                                
VR480A   DS    0H                                                               
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF                                                  
         BNZ   VR500                DONE W/ ALL STATIONS, EXIT                  
                                                                                
         TM    STACFLG,STAFPRCQ    STATION ENTRY PROCESSED YET?                 
         BZ    VR485                NOPE                                        
VR480B   DS    0H                                                               
         MVI   GOSUBN,TSR_NXT#      YEP, TRY NEXT ENTRY                         
         B     VR480A                                                           
         SPACE 2                                                                
VR485    DS    0H                  BUILD & ADD NEW STATION-LEVEL RECD           
         OC    STACSTA,STACSTA     IF THIS IS NULLS,                            
         BNZ   *+6                                                              
         DC    H'0'                 MKT-LEVEL RECD WILL GET CLOBBERED!          
         L     R6,AIO                                                           
                                                                                
         LR    R0,R6                                                            
         LA    R1,L'IO                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA BEFORE BUILDING RECORD            
                                                                                
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB              BUILD PWKEY                                  
         MVC   KEY+(PWKSTA-PWFKEY)(L'PWKSTA),STACSTA                            
                                                                                
         MVC   PWFKEY,KEY                                                       
         MVC   PWAGYA,TWAAGY        AGENCY TAG                                  
         XC    PWEL(PWGNLENQ),PWEL                                              
         MVI   PWGNEL,PWGNELQ      X'01' ELEMENT                                
         MVI   PWGNLEN,PWGNLENQ                                                 
         OI    PWGNFLG,PWGNBPLK     FOR CONSISTENCY W/ MKT-LEVEL RECD           
         MVI   PWEL+PWGNLENQ,0     EORECORD MARKER                              
         LA    R0,(PWEL-PWFKEY+PWGNLENQ+1)                                      
         STCM  R0,3,PWLEN           L(RECORD) SO FAR                            
                                                                                
         TM    LOCKFLAG,LKFUBLKQ   IF USER WANTS BUY LOCK,                      
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNBILQ+PWGNBPLK   THEN BUY SHALL BE LOCKED             
         TM    LOCKFLAG,LKFUPLKQ   IF USER WANTS PW  LOCK,                      
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNPLKQ+PWGNBPLK   THEN PW  SHALL BE LOCKED             
*****    MVC   PWGNGOAL,SKEDCGTX   WRITE SKED ACT GOAL (W/ TAX),                
*****    MVC   PWGNGRP,PWGRP        GRP,                                        
         MVC   PWGNTAX,GLTXRATE     AND TAX TO RECORD                           
         NI    PWGNFLG,XFF-PWGNUGOL-PWGNUGRP                                    
         TM    MISCFLG3,MF3UGOAL                                                
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNUGOL     USER INPUTTED GOAL AMOUNT                   
         TM    MISCFLG3,MF3UGRP                                                 
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNUGRP     USER INPUTTED GRP  AMOUNT                   
*                                                                               
VR485A   DS    0H                                                               
         TM    LOCKFLAG,LKFCOLKQ   IF 'LOCK' ENTERRED,                          
         BO    VR487                UPDATE DOLLAR (LOCKIN) ELEMS                
         TM    LOCKFLAG,LKFBPLK2   IF BUY OR PW HAS BEEN PREVSLY LCKED,         
         BO    VR487A               DON'T UPDATE THE ELEMENTS                   
         TM    LOCKFLAG,LKFBYPWQ   POTENTIAL 1ST TIME LOCK                      
         BZ    VR487A               ...NOT!  DATE DOLLAR ELEMENTS               
*                                                                               
VR487    BAS   RE,BPWDOLEL         BUILD ELEM AND APPEND TO RECORD              
VR487A   OI    STACFLG,STAFPRCQ    ENTRY-PUT-INTO-FILE FLAG                     
         MVI   GOSUBN,TSR_PUT#      AND PUT STTN ACCUM RECD BACK                
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR487B   DS    0H                                                               
         MVI   GOSUBN,TSR_NXT#                                                  
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     RETRIEVED ALL ENTRY OF CURR STTN?            
         BNZ   VR490                YEP, GO ADD THE *&@#$ RECORD                
         CLC   STACSTA,PWKSTA      SAME STATION?                                
         BNE   VR487B                                                           
         TM    STACFLG,STAFPRCQ     YEP, AND IT                                 
         BZ    VR485A                                                           
         DC    H'0'                  BETTER BE NOT PROCESSED                    
*                                                                               
VR490    DS    0H                                                               
         BAS   RE,UPDATEPW                                                      
*&&DO                                                                           
         MVI   GOSUBN,DDC#         RESTORE DRCR INFO BACK INTO RECORD,          
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,DCD#          DO CURRENT$ ELEMENTS,                       
         GOTO1 (RF)                                                             
*&&                                                                             
         DS    0H                  DETERMINE WHETHER TO ADD OR PUT RECD         
         OI    DMINBTS,X08          READ FOR DELETES                            
         GOTO1 HIGH                                                             
         NI    DMINBTS,XFF-X08                                                  
                                                                                
         L     RF,ADDREC            ASSUME WE'LL NEED TO ADDREC                 
         CLC   KEY(L'PWFKEY),KEYSAVE                                            
         BNE   VR495                                                            
                                                                                
         L     R0,AIO               REMEMBER WHAT'S IN AIO                      
         MVC   AIO,AIO2                                                         
         OI    DMINBTS,X08                                                      
         GOTO1 GETREC                                                           
         NI    DMINBTS,XFF-X08                                                  
         ST    R0,AIO               RESTORE AIO                                 
                                                                                
         NI    KEY+L'PWFKEY,XFF-X80 TURN OFF DELETE IN KEY                      
         GOTO1 WRITE                 AND WRITE IT BACK TO FILE                  
         L     RF,PUTREC                                                        
         B     VR495                                                            
                                                                                
VR495    DS    0H                                                               
         GOTO1 (RF)                RF = A(ADDREC) OR A(PUTREC)                  
*                                                                               
         B     VR480               CHECK FOR MORE UNPROCESSED STTNS             
         DROP  R4,R6                                                            
*                                                                               
** EXIT TASKS **                                                                
*                                                                               
VR500    DS    0H                                                               
         TM    MISCFLG2,MF2SLAVE   IF VALREC CALLED WHILE SLAVE MODE,           
         BO    XIT                  EXIT NOW                                    
*                                                                               
         TM    MISCFLG2,MF2RRDCH   NEED TO RE-READ FILE?                        
         BZ    VR600                NOPE                                        
         BAS   RE,PWSETUP                                                       
         NI    MISCFLG2,XFF-MF2RRDCH                                            
         L     R6,AMYC2REC         WE MIGHT BE MISSING STATIONS THAT            
         MVI   ELCODE,C2STCODQ       HAVE BEEN LOCKED BUT DO NOT HAVE           
         BAS   RE,GETEL              BUYS                                       
         BNE   VR600                                                            
         USING C2STEL,R6                                                        
VR510    L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
VR520    TM    ATFLAG,ATFSTTLQ                                                  
         BNZ   VR540                                                            
         CLC   C2STSTAP,ATSTAPKD                                                
         BE    VR550                                                            
         AHI   R4,ACCUTABQ                                                      
         B     VR520                                                            
*                                                                               
VR540    LA    R3,ELEMENT+64       TEMP STORAGE                                 
*        MVC   0(ACCUTABQ*2+1,R3),0(R4)   BACKUP 2 TOTAL ENTRIES                
         MVC   0(ACCUTABQ*3,R3),0(R4)   BACKUP 2 TOTAL ENTRIES & FFFFFF         
         XC    0(ACCUTABQ,R4),0(R4)                                             
         MVC   ATSTAPKD,C2STSTAP                                                
         GOTO1 DATCON,DMCB,(0,ESDATE),(2,ATMSTART),0                            
         GOTO1 DATCON,DMCB,(0,EEDATE),(2,ATMEND),0                              
****     MVC   ATWSTART(L'ATWEEK),ATMSTART                                      
         OI    ATFLAG,ATFNOSPT      INITIALIZE TO NO SPOTS                      
         MVC   ATLKCS2,C2STPCT      MOVE IN LOCKED CS2                          
         MVC   ATCS2,C2STFCTR       MOVE IN CURRENT CS2                         
         MVC   ATCLCK(8),C2STLKC2   MOVE IN LOCKED DOLLARS                      
*        MVC   ACCUTABQ(ACCUTABQ*2+1,R4),0(R3)                                  
         MVC   ACCUTABQ(ACCUTABQ*3,R4),0(R3)                                    
         DROP  R4                                                               
*                                                                               
VR550    BAS   RE,NEXTEL                                                        
         BE    VR510                                                            
*                                                                               
VR600    B     DR                       GO AND RE-DISPLAY VALUES                
*                                                                               
VRX      MVI   GOSUBN,STI#             SAVE OFF TABLES IN TIA                   
         GOTO1 AGOSUB                                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SEEDS THE COST2 FACTOR TO ALL BUYLINES AFFECTED                  
***********************************************************************         
CS2BUYS  NTR1                                                                   
         MVI   MYERRCD,0                                                        
         MVI   MYWRNCD,0                                                        
         L     R2,ASBLOCKD                                                      
         USING SBLOCKD,R2                                                       
         MVI   SPTIOMDE,SIOMBUY    READ BUYS ONLY                               
*                                                                               
C2B002   DS    0H                  START OF A NEW SPOTIO MODE                   
         LA    RE,SBLOCK                                                        
         LA    RF,SPBKLEN                                                       
         XCEF                                                                   
*                                                                               
         MVC   SBCOMFAC,ACOMFACS                                                
         MVC   SBAIO1(12),AIO1                                                  
                                                                                
         LA    R1,BCS2HOOK             SET ADDRESS OF IOHOOK,                   
         ST    R1,SBIOHOOK                                                      
         L     R0,ATIA                                                          
         LH    R1,=Y(ESTAB-SPOTAREA)                                            
         AR    R1,R0                                                            
         ST    R1,SBASVETB              ESTIMATE TABLE,                         
*&&DO                                                                           
         MVC   SBACHUNK,ASPCHUNK        CHUNKS TABLE,                           
*&&                                                                             
         LH    R1,=Y(SVSPARE-T217FFD)                                           
         LA    R1,T217FFD(R1)                                                   
         ST    R1,SBACHUNK              CHUNKS TABLE,                           
         LH    R1,=Y(SPTTAB-SPOTAREA)                                           
         AR    R1,R0                                                            
         ST    R1,SBASPTTB              AND SPOT TABLE                          
         LH    R1,=Y(SPTTABX-SPTTAB)                                            
         ST    R1,SBLSPTTB             L(SPOT TABLE)                            
                                                                                
         DS    0H                  1ST PASS-READ BUY & STTN BILL                
         CLI   USETAX,C'Y'                                                      
         BE    *+8                                                              
         OI    SBEFLAG,SBEPWNTX                                                 
         TM    ESTFLAG,EFBILEST    IF THIS IS AN E ESTIMATE,                    
         BZ    *+8                                                              
         OI    SBEFLAG,SBEEEST      TURN ON THIS FLAG FOR SPOTBUY               
         MVI   SBEPAID,C'Y'        EXTRACT PAID DATA                            
         MVI   SBEBYDT,C'Y'        GET AFFID DATE                               
                                                                                
         MVI   SBQSKIP,SBQSKGL+SBQSKBIL+SBQSKMED+SBQSKMKT                       
         MVI   SBQREAD,0                                                        
                                                                                
         MVC   SBQAGY,AGENCY                                                    
         MVC   SBQMED,QMED                                                      
         MVC   SBQCLT,QCLT                                                      
                                                                                
         MVC   SBQPRD,QPRD                                                      
         MVC   SBQBPRD,BPRD                                                     
         MVC   SBEPRD,BPRD                                                      
         TM    CLTFLAG,CFWSTRAD    IS IT A TRADE CLIENT?                        
         BO    C2BPRDG              YEP, TREAT PRODUCT DIFFERENTLY              
         TM    ESTFLAG,EFWSTRAD    IS IT A TRADE ESTIMATE?                      
         BO    C2BPRDG              YEP, TREAT PRODUCT DIFFERENTLY              
         B     C2BPRDX             ELSE, LEAVE PRODUCT ALONE                    
C2BPRDG  EQU   *                                                                
         MVC   SBQPRD,=C'POL'      SET TO PRODUCT TO POL                        
         MVI   SBQBPRD,XFF          TO READ ALL BRANDS                          
         MVI   SBEPRD,XFF                                                       
C2BPRDX  EQU   *                                                                
                                                                                
         MVC   SBQMKT,QMKT                                                      
         MVC   SBQSTA,QSTA                                                      
         MVC   SBBAGYMD,BAGYMD                                                  
         ZIC   R0,BEST                                                          
         STC   R0,SBQEST                                                        
         STC   R0,SBQESTND                                                      
         L     R1,SBASVETB                                                      
         XC    0(256,R1),0(R1)     CLEAR ESTIM TABLE                            
         AR    R1,R0                                                            
         MVI   0(R1),1                                                          
                                                                                
         MVC   SBAMKEST,AMKESTAB   A(MKT/EST TABLE)                             
                                                                                
         L     RE,SBACHUNK         CLEAR CHUNK TABLE                            
         LH    RF,=Y(CHUNKX-CHUNK)                                              
         XCEF                                                                   
         L     RE,SBASPTTB         CLEAR SPOTTAB TABLE                          
         L     RF,SBLSPTTB                                                      
         XCEF                                                                   
                                                                                
         LA    R1,BRDWKTB2                                                      
         ST    R1,SBADATE          SET A(DATES),                                
         ZIC   R1,NUMWEEKS                                                      
         ST    R1,SBNDATES          AND THE NUMBER OF THEM                      
                                                                                
         XC    ELEM,ELEM           CLEAR ELEM FIELD,                            
         LA    R1,ELEM              AND USE IT AS AN 110-BYTE WRK AREA          
         STCM  R1,7,SBAWIPW         FOR SPOTIO TO DO ITS PW STUFF               
                                                                                
         MVI   GOSUBN,STW#         SAVE TWA DATA BEFORE SPOTIO CALL             
         GOTO1 AGOSUB                                                           
*                                                                               
         GOTO1 ASPOTIO,DMCB,SBLOCK                                              
*                                                                               
         MVI   GOSUBN,RTW#         RESTORE TWA DATA AFTER SPOTIO CALL           
         GOTO1 AGOSUB                                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATE SPECIFIC ELEMENTS IN THE PW RECORD                        
*                                                                               
* ON ENTRY:    (R6)                A(PW RECORD)                                 
***********************************************************************         
UPDATEPW NTR1                      CS2 AND CLCOST OVERRIDE ELEMS                
         MVI   ELEMENT,0            NO SEARCH ARGUMENTS                         
         MVI   ELCODE,C2STCODQ                                                  
         MVI   GOSUBN,DLM#                                                      
         GOTO1 AGOSUB              DELETE C2STEL ELEMENTS FROM PW RECD          
         MVI   ELCODE,PWDOLCDQ                                                  
         GOTO1 AGOSUB              DELETE WEEKLY LOCKED DOLLAR ELEMS            
         MVI   ELCODE,PWCURCDQ                                                  
         GOTO1 (RF)                DELETE WEEKLY CURRENT DOLLAR ELEMS           
*                                                                               
         OI    MISCFLG2,MF2RRDCH                                                
*                                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*****                                                                           
         LR    RF,R4                                                            
         TM    ATFLAG-ACCUTABD(RF),ATFSTTLQ  NO STATIONS WHATSOEVER?            
         BZ    UPDPW05                                                          
         MVC   ATCS2-ACCUTABD(4,RF),OC2PCT   NONE, USE EST C2 FACTOR            
         AHI   RF,ACCUTABQ                                                      
         MVC   ATCS2-ACCUTABD(4,RF),OC2PCT                                      
*****                                                                           
UPDPW05  OC    PWKSTA-PWFKEY(L'PWKSTA,R6),PWKSTA-PWFKEY(R6)                     
         BNZ   UPDPW20                                                          
***************                                                                 
* MARKET LEVEL PW RECORD                                                        
***************                                                                 
UPDPW10  TM    ATFLAG,ATFMTTLQ                                                  
         BO    UPDPW15                                                          
         TM    ATFLAG,ATFSTTLQ     LOCATE THE SCHEDULE ENTRY                    
         BZ    UPDPW13                                                          
         TM    ATFLAG,ATFBILAJ                                                  
         BZ    UPDPW30                                                          
         B     UPDPW15                                                          
*                                                                               
UPDPW13  MVC   FULL,ATLKCS2        COPY LOCKED C2 FROM THE STATIONS             
UPDPW15  AHI   R4,ACCUTABQ                                                      
         B     UPDPW10                                                          
***************                                                                 
* STATION LEVEL PW RECORD                                                       
***************                                                                 
UPDPW20  TM    ATFLAG,ATFSTTLQ                                                  
         BO    UPDPW50                                                          
         CLC   ATSTAPKD,PWKSTA-PWFKEY(R6)                                       
         BE    UPDPW30                                                          
UPDPW25  AHI   R4,ACCUTABQ                                                      
         B     UPDPW20                                                          
***************                                                                 
* BUILD C2STEL                                                                  
***************                                                                 
UPDPWD   USING C2STEL,ELEMENT                                                   
UPDPW30  XC    ELEMENT,ELEMENT                                                  
         MVI   UPDPWD.C2STCD,C2STCODQ     ELEMENT CODE                          
         MVI   UPDPWD.C2STLEN,C2STLENQ    ELEMENT LENGTH                        
*                                                                               
         OC    PWKSTA-PWFKEY(L'PWKSTA,R6),PWKSTA-PWFKEY(R6)                     
         BNZ   UPDPW35                                                          
         MVC   UPDPWD.C2STFCTR,ATCS2      CS2 CURRENT FACTOR                    
         CLC   =X'40404040',UPDPWD.C2STFCTR                                     
         BE    *+14                                                             
         OC    UPDPWD.C2STFCTR,UPDPWD.C2STFCTR                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   UPDPWD.C2STPCT,FULL        CS2 LOCKED FACTOR                     
         TM    MISCFLG2,MF2RQADD                                                
         BZ    UPDPW40                                                          
         MVC   UPDPWD.C2STPCT,ATCS2       CURRENT CS2 <- LOCKED CS2             
         B     UPDPW40                                                          
*                                                                               
UPDPW35  MVC   UPDPWD.C2STLKC2(8),ATCLCK  C2  LOCKED DOLLARS                    
*                                                                               
UPDPW40  GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),ELEMENT,0                          
         CLI   12(R1),0            CHECK FOR ERRORS                             
         BE    *+6                  IF NONE, BUILD NEXT ELEMENT                 
         DC    H'0'                 ELSE, DIE                                   
         OC    PWKSTA-PWFKEY(L'PWKSTA,R6),PWKSTA-PWFKEY(R6)                     
         BNZ   UPDPW50                                                          
         EDIT  (B4,UPDPWD.C2STPCT),(8,C2MLOCK),6,ZERO=NOBLANK                   
         OI    C2MLOCKH+6,X'80'                                                 
***************                                                                 
* NOW UPDATE THE PW RECORD IN STORAGE                                           
***************                                                                 
UPDPW50  L     RE,AMYC2REC                                                      
         MVC   PWGNEL-PWFKEY(PWGNLENQ,RE),PWGNEL-PWFKEY(R6)                     
*                                                                               
         OC    PWKSTA-PWFKEY(L'PWKSTA,R6),PWKSTA-PWFKEY(R6)                     
         BZ    UPDPW70                                                          
         MVC   UPDPWD.C2STSTAP,PWKSTA-PWFKEY(R6)                                
         MVC   UPDPWD.C2STFCTR,ATCS2                                            
         MVC   UPDPWD.C2STPCT,ATLKCS2                                           
*********                                                                       
* UPDATING STATION LEVEL                                                        
*********                                                                       
         L     R6,AMYC2REC                                                      
         LA    R6,24(R6)                                                        
         XR    R0,R0                                                            
UPDPW65  CLI   0(R6),0                                                          
         BNE   UPDPW65A                                                         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AMYC2REC,ELEMENT,0                      
         CLI   12(R1),0            CHECK FOR ERRORS                             
         BE    UPDPW80              IF NONE, BUILD NEXT ELEMENT                 
         DC    H'0'                 ELSE, DIE                                   
UPDPW65A CLI   0(R6),C2STCODQ                                                   
         BE    UPDPW67                                                          
UPDPW66  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     UPDPW65                                                          
*                                  UPDATE COSTS IN THE STATION REC              
         USING C2STEL,R6                                                        
UPDPW67  CLC   C2STSTAP,UPDPWD.C2STSTAP                                         
         BNE   UPDPW66                                                          
         MVC   C2STPCT(16),UPDPWD.C2STPCT                                       
         B     UPDPW80                                                          
*********                                                                       
* UPDATING MARKET LEVEL                                                         
*********                                                                       
UPDPW70  L     R6,AMYC2REC                                                      
         LA    R6,24(R6)                                                        
         XR    R0,R0                                                            
UPDPW75  CLI   0(R6),0                                                          
         BE    UPDPW80                                                          
         CLI   0(R6),C2STCODQ                                                   
         BE    UPDPW77                                                          
UPDPW76  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     UPDPW75                                                          
*                                  UPDATE COSTS IN THE STATION REC              
         USING C2STEL,R6                                                        
UPDPW77  MVC   C2STPCT,UPDPWD.C2STPCT                                           
         MVC   C2STFCTR,UPDPWD.C2STFCTR                                         
         CLC   =X'40404040',C2STFCTR                                            
         BE    *+14                                                             
         OC    C2STFCTR,C2STFCTR                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     UPDPW76                                                          
         DROP  UPDPWD                                                           
*                                                                               
UPDPW80  L     R6,AMYC2REC                                                      
         MVI   ELCODE,PWDTICDQ                                                  
         MVI   ELEMENT,0            NO SEARCH ARGUMENTS                         
         MVI   GOSUBN,DLM#                                                      
         GOTO1 AGOSUB              DELETE PWDTIEL ELEMS FROM PW RECD            
         DS    0H                   AND PUT NEW ONE IN                          
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),DTIELEM,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDPWX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SEEDS THE COST2 FACTOR TO THE BUYLINE IN AIO                     
***********************************************************************         
BCS2HOOK NTR1                                                                   
         L     R6,SBAIO1                                                        
         TM    (BUYRCNTL-BUYRECD)(R6),X80                                       
         BNZ   BCSHKX               IGNORE IF RECORD IS DELETED                 
*                                                                               
         CLI   SBMODE,SBPROCSP                                                  
         BNE   BCSHKX                                                           
*                                                                               
         USING BUYKEY,R6                                                        
         CLI   0(R6),X'10'         BUY RECORD?                                  
         BNH   BCSHKX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+14(L'SBRECDA),SBRECDA                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
BCSHK02  LA    R1,BDELEM           ANY COST2 FACTOR?                            
         SR    R0,R0                                                            
BCSHK02A CLI   0(R1),0             NONE, ADD ONE TO BUY                         
         BE    BCSHK04                                                          
         CLI   0(R1),X'73'                                                      
         BE    BCSHK03                                                          
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BCSHK02A                                                         
BCSHK03  MVC   2(L'TEMPPW,R1),TEMPPW   CHANGE IT TO WHAT WE HAVE                
         B     BCSHK05                                                          
*                                                                               
BCSHK04  XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         MVI   0(R1),X'73'                                                      
         MVI   1(R1),6                                                          
         MVC   2(L'TEMPPW,R1),TEMPPW                                            
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),WORK,0                             
         CLI   12(R1),0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BCSHK05  MVI   ACTELOPT,C'N'                                                    
         XC    DMCB(24),DMCB                                                    
         GOTO1 PUTREC                                                           
         MVI   ACTELOPT,C'Y'                                                    
*                                                                               
BCSHKX   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*------------------------ BUILD PWDOLEL ELEM -------------------------*         
* AT ENTRY,                                                                     
*    R4 --> ENTRY IN STACTAB                                                    
*    R6 --> RECORD TO ADD ELEMENT TO                                            
*    AIO3 = A(MKT-LEVEL PW RECORD)                                              
                                                                                
BPWDOLEL NTR1                                                                   
         USING STACRECD,R4                                                      
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING PWDOLEL,R3                                                       
         MVI   PWDOLCD,PWDOLCDQ    ELEM CODE                                    
         MVI   PWDOLLEN,PWDOLLNQ   ELEM LENGTH                                  
*****    MVC   PWDOLWK,STACSTRT    BRDCST WEEK DATE                             
*                                                                               
         MVC   PWDOLSPT,STACSPT    SPOTS                                        
         MVC   FULL,AIO3           A(MKT-LVL PW RECD) FOR UPNSPT                
         BAS   RE,UPNSPT           SHOULD WE UPDATE # OF SPOTS                  
         BE    *+10                 YEP                                         
         MVC   PWDOLSPT,TEMPNSPT    NOPE, RESTORE IT                            
                                                                                
         MVC   PWDOLWG,STACGRS     WIM GROSS                                    
         MVC   PWDOLWN,STACNET     WIM NET                                      
         MVC   PWDOLCG,STACCGRS    CLT GROSS                                    
         MVC   PWDOLCN,STACCNET    CLT NET                                      
         MVC   PWDOLTAX,STACTAX    TAX                                          
         MVC   PWDOLCTX,STACCTX    CLT TAX                                      
         DROP  R3                                                               
                                                                                
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),ELEMENT,0                          
         CLI   12(R1),0            DON'T WANT ANY ERRORS                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*--------------------- TEST # OF SPOTS UPDATEABLE --------------------*         
                                                                                
* At entry,                                                                     
*   ELEMENT contains a PWDOLEL being built,                                     
*   R6-->the PW record where ELEMENT will belong,                               
*   FULL = A(mkt-level PW record).                                              
* At exit,                                                                      
*   TEMPNSPT = # of spots to use if CC not-equal.                               
                                                                                
         DS    0H                                                               
UPNSPT   NTR1                                                                   
         LA    R3,ELEMENT                                                       
         USING PWDOLEL,R3                                                       
                                                                                
         L     R0,FULL                                                          
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWCURCDQ',(R0)),0,0                   
         CLI   12(R1),0            IF FOUND,                                    
         BE    UPNSPT10             DON'T UPDATE # OF SPOTS                     
         CLI   12(R1),6            IF NOT FOUND,                                
         BE    YES                  WE CAN UPDATE # OF SPOTS                    
         DC    H'0'                OTHERWISE, DON'T RETURN AT ALL               
*                                                                               
UPNSPT10 DS    0H                  FIND # OF SPOTS TO USE                       
         XC    TEMPNSPT,TEMPNSPT                                                
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWBKUP06',(R6)),0,0                   
         CLI   12(R1),0                                                         
         BNE   UPNSPT15                                                         
         L     RF,12(R1)                                                        
         MVC   TEMPNSPT,PWDOLSPT-PWDOLEL(RF)                                    
         B     UPNSPT15                                                         
                                                                                
UPNSPT15 DS    0H                                                               
         B     NO                  CC NEQUAL ==> DON'T UPDATE # SPOTS           
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
*--------------------- MY VALREC ERRORS & WARNINGS -------------------*         
                                                                                
INVLFLD  DS    0H                                                               
         MVI   MYERRCD,IFLDQ       INVALID FIELD ERROR                          
         B     MYERROR                                                          
                                                                                
MPCTE    DS    0H                                                               
         MVI   MYERRCD,MPCTQ                                                    
         B     MYERROR                                                          
                                                                                
EAJBPWE  DS    0H                                                               
         LA    R2,(MSLCCOSH-MSLDSECT)(R2)                                       
         MVI   MYERRCD,EAJBPWQ                                                  
         B     MYERROR                                                          
                                                                                
BADAJBE  LA    R2,(MSLCCOSH-MSLDSECT)(R2)                                       
         CLI   MYERRCD,0           IF MYERRCD IS 0,                             
         BE    INVLFLD              THEN DISPLAY INVALID FIELD ERROR            
         B     MYERROR                                                          
                                                                                
BADPWE   LA    R2,(MSLCOS2H-MSLDSECT)(R2)                                       
         CLI   MYERRCD,0           IF MYERRCD IS 0,                             
         BE    INVLFLD              THEN DISPLAY INVALID FIELD ERROR            
         B     MYERROR                                                          
                                                                                
BADDRCR  DS    0H                                                               
         LA    R2,(MSLCCOSH-MSLDSECT)(R2)                                       
         CLI   MYERRCD,0                                                        
         BE    INVLFLD                                                          
         B     MYERROR                                                          
                                                                                
NMODUPD0 DS    0H                  LOCATE FIRST FIELD MODIFIED                  
         MVC   DATE4,MNTHMRKS                                                   
         BAS   RE,LOC1WMTH         SET UP R4 POINTING IN ACCUTAB                
         ST    R4,FULL              AND PASS AS PARAM                           
         MVI   GOSUBN,LMD#                                                      
         GOTO1 AGOSUB                                                           
         L     R2,FULL             R2-->1ST MODIFIED FIELD                      
         MVI   MYERRCD,NMODLCKQ    CAN'T MODIFY AND LOCK                        
         CLI   PFKEY,2                                                          
         BE    MYERROR                                                          
         MVI   MYERRCD,NMODPLKQ    CAN'T MODIFY AND PW LOCK                     
         CLI   PFKEY,3                                                          
         BE    MYERROR                                                          
         DC    H'0'                                                             
                                                                                
NLKNTXE  MVI   MYERRCD,NLKNTXQ                                                  
         B     MYERROR                                                          
                                                                                
NLKSTAE  MVI   MYERRCD,NLKSTAQ                                                  
         LA    R1,C2MMKTH          FORCE CURSOR TO BE IN                        
         ST    R1,ACURFORC          MARKET KEY FIELD                            
         B     MYERROR                                                          
                                                                                
NOTLOCKE MVI   MYERRCD,BPNLQ                                                    
         B     MYERROR                                                          
                                                                                
NCOWPE   DS    0H                                                               
         LA    R2,(MSLCOS2H-MSLDSECT)(R2)                                       
         MVI   MYERRCD,NCOWPQ                                                   
         B     MYERROR                                                          
                                                                                
XCUERE   DS    0H                                                               
         LA    R2,CONSERVH                                                      
         MVI   MYERRCD,XCUERQ                                                   
         B     MYERROR                                                          
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY (PUTREC && +        
               XPUTREC)'                                                        
***********************************************************************         
*=============================== PUTREC ==============================*         
PR       DS    0H                                                               
         MVI   IOOPT,C'Y'                                                       
         B     XIT                                                              
         SPACE 2                                                                
*============================== XPUTREC ==============================*         
XP       DS    0H                                                               
         MVI   IOOPT,C'N'                                                       
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY (DISPREC)'          
***********************************************************************         
*=========================== DISPLAY RECORD ==========================*         
DR       DS    0H                                                               
         NI    MISCFLG1,XFF-MF1ERRQ  FORGET PREV ERROR WHEN DSPLYING            
         BAS   RE,SORTACTB         SORT ACCUTAB ENTRIES                         
*                                                                               
DR03     CLI   PFKEY,0             IF PFKEY HIT,                                
         BE    DR05                                                             
         BAS   RE,DOPFKEY           PROCESS PF FUNCTIONS                        
         BE    DR05                                                             
         LA    R2,C2MMEDH          IF CC NEQ,                                   
         B     MYWARN               DISPLAY "WARNING" MESSAGE                   
*                                                                               
DR05     LA    R0,NMSLINEQ         RESET SCREEN                                 
         LA    R2,C2MSTATH                                                      
         MVI   BYTE,C'N'                                                        
DR05A    DS    0H                                                               
*****    MVI   GOSUBN,CIS#          BY CHANGING TO NORMAL INTENSITY,            
*****    GOTO1 AGOSUB                                                           
         MVI   GOSUBN,CLF#          CLEARING FIELDS,                            
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,TRF#          AND TRANSMITTING THEM                       
         GOTO1 (RF)                                                             
         LA    R2,MSLLENQ(R2)                                                   
         BCT   R0,DR05A                                                         
                                                                                
         MVI   GOSUBN,CLF#         CLEAR AND TRANSMIT                           
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,TRF#          FIELDS OF SCHED TOTALS LINE                 
         GOTO1 (RF)                                                             
                                                                                
         DS    0H                  RESET FLAGS ON DISPLAY                       
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         LA    RE,ACCUTABX-ACCUTAB-1(R4)                                        
DR07     NI    ATFLAG,XFF-ATFAJBQ-ATFPWQ     MODIFIED FLAGS                     
         NI    ATFLAG2,XFF-ATF2DPLY-ATF2CHDC DISPLAYED & DR/CR CHNG FLG         
*                                                                               
         TM    MISCFLG2,MF2RQADD                                                
         BZ    DR08                                                             
         MVC   ATCLCK(8),ATAJBUY      LOCK IN ADJUSTED BUYS FOR DISPLAY         
*                                                                               
DR08     AHI   R4,ACCUTABQ                                                      
         CLC   =X'FFFFFF',0(R4)    HIT LOGICAL END OF TABLE?                    
         BE    DR08A                                                            
         CR    R4,RE               OR PHYSICAL END OF TABLE?                    
         BL    DR07                                                             
*                                                                               
DR08A    LA    R2,C2MSTATH                                                      
         USING MSLDSECT,R2                                                      
         NI    MISCFLG2,XFF-MF2BILAJ  ASSUME NO BILL ADJ LINE TO SHOW           
         NI    MISCFLG3,XFF-MF3NDSCR                                            
         MVI   LNCNTDWN,NMSLINEQ   INITIALIZE LINE-COUNTDOWN                    
         MVI   NUMLNDSP,0          RESET # OF LINES DISPLAYED                   
*                                                                               
         TM    MISCFLG1,MF1KYOPT   MODIFIED KEY OR OPTION?                      
         BZ    DR09                                                             
         MVI   DISPACTB,0                                                       
*                                                                               
DR09     L     R4,AACCUTAB                                                      
         ZIC   R1,DISPACTB                                                      
         MHI   R1,ACCUTABQ                                                      
         AR    R4,R1               CONTINUE DISPLAY FROM HERE                   
*                                                                               
*****    BAS   RE,LOCMMARK         GET R3-->1ST MONTH ON DISPLAY                
*****    USING BCSTTABD,R3                                                      
         EJECT                                                                  
* Note: The Billing Adjustment feature, which is analagous to the               
*  Adjusted DR/CR on the PW/BILL screen, is to be activated once all            
*  spots are paid in a month.  It is suppose to remain in effect                
*  thereinafter regardless of future pay status, i.e. if more buys              
*  are added after a billing adjustment was entered.  Hence, I expect           
*  that the billing adjustment will not be erased or left blank once            
*  it has been inputted.  If a billing adj amount is to be zero, then           
*  a '0' should be entered into the field.  Otherwise, I have no way            
*  of knowing that this feature was activated previously.                       
                                                                                
DR10     CLC   =X'FFFFFF',0(R4)    END OF STATION TABLE?                        
         BE    DR67                                                             
*                                                                               
*                                                                               
         TM    ATFLAG,ATFSTTLQ+ATFBILAJ    IF ENTRY IS A                        
         BNO   DR20                                                             
         MVI   DISPACTB,0          NEXT TIME WE START ON THE 1ST ENTRY          
         B     DR54                                                             
*                                                                               
DR20     TM    ATFLAG,ATFSTTLQ                 SKED LINE                        
         BO    DR45A                                                            
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),ATSTAPKD                                                
         GOTO1 MSUNPK,DMCB,DUB,WORK,MSLSTAT,0                                   
         B     DR50                                                             
*&&DO                                                                           
DR40     DS    0H                  MONTHLY BILLING ADJ LINE                     
         CLI   SHOWBA,C'Y'          DISPLAY IT IF NOT TURNED OFF,               
         BNE   DR54                                                             
         TM    ATFLAG,ATFNOSPT      AND AT LEAST 1 SPOT IN MONTH,               
         BO    DR54                                                             
         OC    ATDRCR,ATDRCR        AND EITHER BILL ADJ NOT NULLS               
         BNZ   DR40A                                                            
         TM    ATFLAG2,ATF2UNPD      OR ALL SPOTS ARE PAID FOR                  
         BO    DR54                                                             
                                                                                
DR40A    DS    0H                  A BILLING ADJ LINE TO DISPLAY                
         OI    MISCFLG2,MF2BILAJ                                                
         OI    ATFLAG2,ATF2DPLY                                                 
                                                                                
         LA    RF,MSLSTAT                                                       
         MVC   0(L'SP@BLNG,RF),SP@BLNG      BILLING                             
         LA    RF,L'SP@BLNG+1(RF)                                               
         MVC   0(L'SP@ADJ,RF),SP@ADJ        ADJ                                 
                                                                                
         OI    MSLCOS2H+1,X20      PROTECT CS2 FIELD FOR BILL ADJ LINE          
         OI    MSLCOS2H+6,X80                                                   
                                                                                
         ICM   R0,15,ATDRCR        EDIT DR/CR AMOUNT                            
         BZ    DR40X               IF ALL NULLS, LEAVE BLANK                    
         MVI   MSLCCOS,C'0'                                                     
         CLM   R0,15,=X'80000000'  IF H.O.BIT ON,                               
         BE    DR40X                DISPLAY A ZERO                              
         MVI   GOSUBN,RUP#         OTHERWISE, ROUND AND DISPLAY VALUE           
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         EDIT  (R1),(7,MSLCCOS),ALIGN=LEFT,FLOAT=-                              
                                                                                
DR40X    DS    0H                                                               
         B     DR54                                                             
*                                                                               
DR42     DS    0H                  MONTHLY TOTALS LINE                          
         GOTO1 DATCON,DMCB,(2,ATMSTART),(0,MYDATE6),0                           
         GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(4,WORK),0                            
                                                                                
         MVC   MSLSTAT(3),WORK                                                  
         MVC   MSLSTAT+4(L'SP@TOTAL),SP@TOTAL                                   
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+10                                                             
         MVC   MSLSTAT(3),PERIOD                                                
                                                                                
         TM    ATFLAG,ATFESTBQ     TEST FOR ESTIM BILL FIRST                    
         BZ    DR45C                                                            
         MVI   MSLSTAT+L'MSLSTAT-2,C'*'                                         
         TM    ATFLAG,ATFFNLBQ     TEST FOR FINAL BILL                          
         BZ    DR45C                                                            
         MVI   MSLSTAT+L'MSLSTAT-1,C'*'                                         
         B     DR45C                                                            
*                                                                               
DR45     DS    0H                  DISPLAY SCHED TOTALS LINE                    
         LA    R3,BCSTTABQ(R3)                                                  
*&&                                                                             
DR45A    LA    R2,C2MSTTLH          POINT TO SCHED TOTALS LINE                  
DR45B    TM    ATFLAG,ATFSTTLQ                                                  
         BO    *+12                                                             
         LA    R4,ACCUTABQ(R4)                                                  
         B     DR45B                                                            
*                                                                               
         TM    ATFLAG,ATFSTTLQ+ATFBILAJ                                         
         BNO   *+12                                                             
         LA    R4,ACCUTABQ(R4)                                                  
         B     DR45B                                                            
*                                                                               
         L     R1,AACCUTAB                                                      
         MVC   ATCS2,ATCS2-ACCUTABD(R1)                                         
*                                                                               
         DS    0H                  R4-->SCHED TOTALS ENTRY                      
         LA    RE,MSLSTAT                                                       
         CLC   =C'(MORE...)',1(RE)                                              
         BE    DR45C                                                            
         MVC   0(L'SP@TOTAL,RE),SP@TOTAL                                        
DR45C    MVI   BYTE,C'H'           MAKE TOTALS LINE HIGH INTENSITY              
         MVI   GOSUBN,CIS#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
DR50     DS    0H                  DISPLAY WEEKLY, MONTHLY, & SKED #S           
         OI    ATFLAG2,ATF2DPLY     FLAG THAT ENTRY IS TO BE DISPLAYED          
         MVI   GOSUBN,RUP#          SET ROUND-UP ROUTINE                        
         MVI   BYTE,0               USE BYTE AS A LOOP COUNTER                  
                                                                                
DR50A    DS    0H                  START OF LOOP                                
         ZIC   RE,BYTE                                                          
*********                                                                       
         CLI   BYTE,2              DISPLAYING ATACGOAL/ATAJGOAL ?               
         BNL   DR50A00             NO                                           
         TM    ATFLAG,ATFSTTLQ     ONLY FOR THE SCHEDULE TOTAL LINE             
         BZ    DR50F                                                            
*********                                                                       
DR50A00  MH    RE,=Y(L'ATACVALS)                                                
         LA    RE,ATACVALS(RE)                                                  
         ICM   R0,15,0(RE)                                                      
         GOTO1 AGOSUB                                                           
         L     R1,FULL             R1 = VALUE TO BE DISPLAYED                   
                                                                                
         ZIC   RE,BYTE                                                          
         MH    RE,=Y(L'EDITFLD)                                                 
         A     RE,AEDITFLD                                                      
         LH    R6,0(RE)                                                         
         LA    R6,MSLDSECT(R6)     R6=A(TO DISPLAY VALUE)                       
                                                                                
         LA    RF,7                CHECK FOR CLCOST OVERRIDE                    
         LA    R0,MSLCLCK-MSLDSECT   SPECIAL FORMATTING                         
         CLM   R0,3,0(RE)          ARE WE UP TO CLLOCK COLUMN                   
         BNE   DR50B_01             NOPE                                        
         TM    ATFLAG2,ATF2CLOD     YEP, AN OVERRIDE?                           
         BZ    DR50D                 NOPE                                       
         B     DR50C                 YEP, GO HANDLE IT                          
DR50B_01 LA    R0,MSLCCOS-MSLDSECT  NOPE                                        
         CLM   R0,3,0(RE)          ARE WE UP TO CLCOST COLUMN                   
         BNE   DR50B_02             NOPE                                        
         TM    ATFLAG2,ATF2CCOD     YEP, AN OVERRIDE?                           
         BZ    DR50D                 NOPE                                       
         B     DR50C                 YEP, GO HANDLE IT                          
DR50B_02 DS    0H                                                               
         B     DR50D                                                            
DR50C    BCTR  RF,0                OVERRIDE ENCOUNTERED, NEED TO CHANGE         
         MVI   0(R6),C'='           L(OUTPUT) AND A(OUTPUT) TO                  
         LA    R6,1(R6)             ACCOMMODATE THE C'='                        
DR50D    STH   RF,HALF             HALF = OUTPUT LENGTH                         
                                                                                
* Doing my own editting of numbers into the field because the output            
*  length needs to be coded soft.                                               
*--->    EDIT  (R1),(HALF,(R6)),ZERO=NOBLANK,ALIGN=LEFT                         
                                                                                
         CVD   R1,DUB              CHANGE VALUE TO PACKED DECIMAL               
         MVC   WORK(17),=X'4040404040402020202020202020202020'                  
         MVI   WORK+15,X'21'       SET PATTERN FOR ED                           
         ED    WORK(17),DUB+2                                                   
         LH    RE,HALF             MOVE DISPLAYABLE VALUE                       
         BCTR  RE,0                                                             
         LA    RF,WORK+17                                                       
         SH    RF,HALF                                                          
         EXMVC RE,0(R6),0(RF)       INTO OUTPUT FIELD                           
         LH    R0,HALF             LEFT-ALIGN OUTPUT                            
         LA    RF,0(R6,RE)         RF-->LAST BYTE OF OUTPUT FIELD               
DR50E_01 CLI   0(R6),C' '           IF LEFT-MOST POSITION IS A BLANK            
         BNE   DR50E_02                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),1(R6)         SHIFT REST OF OUTPUT TO THE LEFT           
         MVI   0(RF),C' '            AND RIGHT-PAD W/ SPACES                    
         BCT   R0,DR50E_01                                                      
DR50E_02 DS    0H                  END OF EDITTING                              
*                                                                               
DR50F    ZIC   RE,BYTE             INCREMENT LOOP COUNTER                       
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLI   BYTE,EDITFLDQ       MORE VALUES TO EDIT?                         
         BL    DR50A                YEP                                         
*                                                                               
         MVC   MSLCOS2,=C'**OVRD*' ASSUME CLCOST OVERRIDE                       
         TM    ATFLAG2,ATF2CCOD                                                 
         BO    DR54                                                             
         MVC   MSLCOS2,=C'**N/A**' ASSUME NO SPOTS                              
         TM    ATFLAG,ATFNOSPT      ANY SPOTS HERE?                             
         BO    DR54                  NOPE                                       
         OC    ATACBUY,ATACBUY      ARE THEY NON-ZERO?                          
         BZ    DR54                  NOPE                                       
         TM    ATFLAG,ATFSTTLQ+ATFMTTLQ                                         
         BZ    DR51A                                                            
         MVC   MSLCOS2,=C'*RANGE*' ASSUME OUT-OF-RANGE                          
         OC    ATAJBUY,ATAJBUY      WHEN CLCOST=0                               
         BZ    DR54                                                             
         ICM   R1,15,ATCS2                                                      
         C     R1,=A(MINPWQ)                                                    
         BL    DR54                                                             
         C     R1,=A(MAXPWQ)                                                    
         BH    DR54                                                             
                                                                                
DR51A    DS    0H                                                               
         XC    MSLCOS2,MSLCOS2     DISPLAY CS2                                  
         MVI   MSLCOS2,C'0'        ASSUME CS=0                                  
         ICM   R1,15,ATCS2                                                      
         BZ    DR54                                                             
         EDIT  (R1),(8,MSLCOS2),6,,ALIGN=LEFT,FLOAT=-                           
*                                                                               
DR54     LA    RF,ACCUTABQ(R4)     JUST GET NEXT ACCUTAB ENTRY                  
         TM    ATFLAG,ATFSTTLQ+ATFBILAJ                                         
         BZ    DR55                                                             
         LR    R4,RF                                                            
         BO    DR45A                                                            
         B     DR67                JUST THE SCHEDULE TOTAL                      
*                                                                               
DR55     LR    R4,RF                                                            
         ZIC   R0,NUMLNDSP          UPDATE NUMLNDSP                             
         AHI   R0,1                                                             
         STC   R0,NUMLNDSP                                                      
         AHI   R2,MSLLENQ          BUMP SCREEN HEADER                           
*                                                                               
         CLI   NUMLNDSP,NMSLINEQ                                                
         BL    DR10                                                             
         CLC   =X'FFFFFF',0(R4)                                                 
         BNE   DR62                                                             
         MVI   DISPACTB,0          RESTART FROM BEG NEXT TIME                   
         B     DR45A               NEED TO SHOW THE C2 FACTOR                   
*                                                                               
DR62     MVC   MSLSTAT+1(9),=C'(MORE...)'                                       
         ZIC   R1,DISPACTB                                                      
         ZIC   R0,NUMLNDSP                                                      
         AR    R1,R0                                                            
         STC   R1,DISPACTB                                                      
         B     DR45A               NEED TO SHOW THE C2 FACTOR                   
*                                                                               
DR67     OI    C2MOPTNH+6,X80                                                   
         MVC   C2MOPTN,SVOPTFLD                                                 
*                                                                               
** TAX FIELD **                                                                 
*                                                                               
         OI    C2MGLTXH+6,X80                                                   
         LH    R0,GLTXRATE                                                      
         CURED (R0),(6,C2MGLTX),3,ZERO=NOBLANK,ALIGN=LEFT                       
*                                                                               
** Station Lock **                                                              
*                                                                               
         DS    0H                                                               
         MVC   REQID,SPACES                                                     
         CLI   CALLPFK,5           IF ENSLAVED FOR STATION LOCKIN,              
         BNE   *+10                                                             
         MVC   REQID,BUYRID         TRANSFER BUYER'S INITIALS OVER              
                                                                                
         MVC   C2MRQID,REQID                                                    
         LA    R0,L'REQID          DETERMINE LENGTH OF ID (W/O BLANKS)          
         LA    RE,REQID                                                         
         SR    RF,RF                                                            
         CLI   0(RE),C' '                                                       
         BNH   *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         STC   RF,C2MRQIDH+5                                                    
         OI    C2MRQIDH+6,X80                                                   
*                                                                               
** TOTAL GRP FIELD **                                                           
*                                                                               
         OI    C2MTGRPH+6,X80                                                   
         L     R0,PWGRP                                                         
         CURED (R0),(7,C2MTGRP),1,ZERO=NOBLANK,ALIGN=LEFT                       
*                                                                               
** BUY & PW STATUS **                                                           
*                                                                               
         OI    C2MBYSTH+6,X80          BUY STATUS                               
         XC    C2MBYST,C2MBYST                                                  
         MVC   C2MBYST(6),=CL6'LOCKED'                                          
         TM    LOCKFLAG,LKFUBLKQ                                                
         BO    *+10                                                             
         MVC   C2MBYST,=CL8'UNLOCKED'                                           
                                                                                
         OI    C2MPWSTH+6,X80          PW  STATUS                               
         XC    C2MPWST,C2MPWST                                                  
         MVC   C2MPWST(6),=CL6'LOCKED'                                          
         TM    LOCKFLAG,LKFUPLKQ                                                
         BO    DR68                                                             
         MVC   C2MPWST,=CL8'UNLOCKED'                                           
*                                                                               
DR68     DS    0H                                                               
         MVI   GOSUBN,CPRTF#       GO SET PROTECTION OF FIELDS                  
         GOTO1 AGOSUB                                                           
*                                                                               
DR70     DS    0H                  SET MSG #S IF NECESSARY                      
         MVI   MYINFCD,0           NO INFO MSG TO BE DISPLAYED, YET             
         MVI   MYWRNCD,0           NO WARN MSG TO BE DISPLAYED, YET             
                                                                                
         TM    MISCFLG2,MF2BUY0Q                                                
         BZ    DR75                                                             
         CLI   C2MRQIDH+5,0        REQUESTING STATION LOCKIN?                   
         BNE   DR75                YES, DON'T CARE IF NO BUYS                   
****                                                                            
         L     RF,AACCUTAB                                                      
         TM    ATFLAG-ACCUTABD(RF),ATFSTTLQ  NO STATIONS WHATSOEVER?            
         BZ    DR73                                                             
         MVC   ATCS2-ACCUTABD(4,RF),OC2PCT   NONE, USE EST C2 FACTOR            
         AHI   RF,ACCUTABQ                                                      
         MVC   ATCS2-ACCUTABD(4,RF),OC2PCT                                      
****                                                                            
DR73     MVI   GOSUBN,STI#                                                      
         GOTO1 AGOSUB                                                           
         MVI   MYINFCD,NBYMKTQ                                                  
         B     DR99X                                                            
                                                                                
DR75     TM    MISCFLG2,MF2RQADD                                                
         BZ    DR80                                                             
         MVI   MYINFCD,RQADDQ                                                   
         NI    MISCFLG2,XFF-MF2RQADD                                            
         B     DR99X                                                            
                                                                                
DR80     TM    LOCKFLAG,LKFUBLKQ                                                
         BZ    DR99X                                                            
         CLC   DTIELEM+(PWDTISLD-PWDTIEL)(L'PWDTISLD),RELOCKSW                  
         BNE   *+12                                                             
         MVI   MYWRNCD,RSLMKTQ                                                  
         B     DR99X                                                            
                                                                                
DR99X    DS    0H                                                               
                                                                                
*                                                                               
** PREPARE TO EXIT **                                                           
*                                                                               
DRX      DS    0H                                                               
         TM    MISCFLG2,MF2SLAVE   IF DISPREC CALLED WHILE IN SLAVE             
         BO    DRXX                 MODE, EXIT NOW                              
*                                                                               
         MVI   GOSUBN,STI#         SAVE OFF TABLES IN TIA                       
         GOTO1 AGOSUB                                                           
*                                                                               
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
         TM    LOCKFLAG,LKFUPLKQ                                                
         BO    DRXX                                                             
         TM    MISCFLG2,MF2BUY0Q                                                
         BO    DRXX                                                             
         MVC   ACURFORC,AM1STREC    UNLESS 1ST DATA FLD IS PREFERRABLE          
                                                                                
DRXX     OI    C2MLOCKH+6,X'80'                                                 
         LA    R1,C2MMEDH                                                       
         ST    R1,ACURFORC                                                      
*                                                                               
         L     R4,AACCUTAB         FIGURE LKD C2 FROM THE LOCKED $$             
         TM    ATFLAG,ATFSTTLQ                                                  
         BNZ   *+12                                                             
         AHI   R4,ACCUTABQ                                                      
         B     *-12                                                             
         AHI   R4,ACCUTABQ         SKIP THE BILL ADJ                            
*                                                                               
         OC    ATWLCK,ATWLCK       GOING TO DIVIDE BY ZERO?                     
         BNZ   *+14                                                             
         MVC   C2MLOCK(8),=C'***N/A**'                                          
         B     DRXX02                                                           
*                                                                               
         ICM   R1,15,ATCLCK        DIVIDE  CLT LOCKED  BY  WIM LOCKED           
         CVD   R1,DUB                  TO GET THE LOCKED C2 FACTOR              
         ZAP   WORK(16),DUB                                                     
         SRP   WORK(16),6,0                                                     
         ICM   R1,15,ATWLCK                                                     
         CVD   R1,DUB                                                           
*                                                                               
         LA    RE,8                NUMBER OF BYTES OF DIVISOR                   
         LA    RF,DUB              1ST BYTE OF DIVISOR                          
DRXX00   CLI   0(RF),0             FIRST BYTE OF DIVISOR                        
         BNE   DRXX01                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,DRXX00           LAST BYTE SHOULD NEVER BE 0                  
*                                                                               
DRXX01   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         DP    WORK(16),0(0,RF)                                                 
*                                                                               
         AHI   RE,2                HIGH ORDER PORTION IS THE QUOTIENT           
         LA    RF,16                                                            
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,WORK(0)                                                      
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
*                                                                               
         EDIT  (B4,FULL),(8,C2MLOCK),6,ZERO=NOBLANK                             
*                                                                               
DRXX02   TM    MISCFLG2,MF2RQADD   STATION LOCKIN REQUEST?                      
         BZ    DRXXX                                                            
         TM    MISCFLG2,MF2BUY0Q   NO BUYS?                                     
         BZ    VR447                                                            
*                                                                               
         L     RE,AMYC2REC         FORCE READ OF MKT-LEVEL AGAIN                
         XC    0(13,RE),0(RE)      WAS OVERWRITING LAST STA IN SET              
*                                                                               
DRXX06   MVI   RDUPDATE,C'Y'       WANT TO UPDATE FILE NOW                      
         MVI   GOSUBN,GPR#         GET PW RECORD (KEY IS BUILT TOO)             
         GOTO1 AGOSUB                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         MVI   ELCODE,C2STCODQ     C2 ELEMENT (X'04')                           
         BAS   RE,GETEL                                                         
         BNE   DRXXX                                                            
         USING C2STEL,R6                                                        
DRXX10   L     R4,AACCUTAB                                                      
DRXX15   CLC   =X'FFFFFF',0(R4)                                                 
         BE    DRXX20                                                           
         TM    ATFLAG,ATFSTTLQ                                                  
         BNZ   DRXX20                                                           
         CLC   ATSTAPKD,C2STSTAP                                                
         BE    *+12                                                             
         AHI   R4,ACCUTABQ                                                      
         B     DRXX15                                                           
         MVC   C2STLKC2(8),ATCLCK    LOCKED DOLLARS                             
         MVC   C2STPCT,ATCS2         LOCKED CS2 FACTOR                          
*                                                                               
DRXX20   BAS   RE,NEXTEL                                                        
         BE    DRXX10                                                           
         DROP  R6                                                               
*                                                                               
DRXX50   GOTO1 PUTREC                                                           
*                                                                               
DRXXX    DS    0H                                                               
         CLI   MYINFCD,0           DO I WANT TO DISPLAY AN INFO MSG?            
         BNE   MYINFO               YEP                                         
         CLI   MYWRNCD,0           DO I WANT TO DISPLAY A  WARN MSG?            
         BNE   MYWARN               YEP                                         
*                                                                               
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*============================ DO PFKEY TASKS =========================*         
DOPFKEY  NTR1                                                                   
         SR    R2,R2                                                            
         LA    R3,BRDMTHTB         FIND DATE TO START DISPLAY                   
         USING BCSTTABD,R3                                                      
         CLI   PFKEY,2                                                          
         BE    PF02                                                             
         CLI   PFKEY,3                                                          
         BE    PF03                                                             
         CLI   PFKEY,5                                                          
         BE    PF05                                                             
         CLI   PFKEY,6                                                          
         BE    PF06                                                             
         CLI   PFKEY,7                                                          
         BE    PF07                                                             
         CLI   PFKEY,8                                                          
         BE    PF08                                                             
         CLI   PFKEY,9                                                          
         BE    PF09                                                             
         MVI   PFKEY,0                                                          
         B     DOPFKEYX                                                         
*                                                                               
** BUY LOCK/UNLOCK **                                                           
*                                                                               
PF02     DS    0H                                                               
         B     DOPFKEYX                                                         
*                                                                               
** PW  LOCK/UNLOCK **                                                           
*                                                                               
PF03     DS    0H                                                               
         B     DOPFKEYX                                                         
*                                                                               
** TOP **                                                                       
*                                                                               
PF05     DS    0H                                                               
         CLC   MNTHMRKS,BCSSTART   IF AT TOP ALREADY,                           
         BNE   *+12                                                             
         MVI   MYWRNCD,WTOPQ        GIVE WARNING MESSAGE                        
         B     NO                                                               
         MVC   MNTHMRKS,BCSSTART                                                
         B     DOPFKEYX                                                         
*                                                                               
** BOTTOM **                                                                    
*                                                                               
PF06     DS    0H                                                               
         CLI   BCSTTABQ(R3),XFF            GO TO LAST "VALID"                   
         BE    PF06A                                                            
         TM    BCSFLAG+BCSTTABQ,BCSFMXQ     MONTH IN BRDMTHTB                   
         BNZ   PF06A                                                            
         LA    R3,BCSTTABQ(R3)                                                  
         B     PF06                                                             
                                                                                
PF06A    CLC   MNTHMRKS,BCSSTART   AT LAST MONTH ALREADY?                       
         BNE   *+12                                                             
         MVI   MYWRNCD,WBOTQ        IF SO, DISPLAY WARNING MSG                  
         B     NO                                                               
         MVC   MNTHMRKS,BCSSTART    IF NOT, USE IT                              
         B     DOPFKEYX                                                         
*                                                                               
** SCROLL UP **                                                                 
*                                                                               
PF07     DS    0H                                                               
         CLC   MNTHMRKS,BCSSTART   AT TOP ALREADY?                              
         BNE   *+12                                                             
         MVI   MYWRNCD,WTOPQ        YES, DISPLAY "WARNING" MSG                  
         B     NO                                                               
                                                                                
         BAS   RE,LOCMMARK         GET R3-->1ST MONTH ON DISPLAY                
         SH    R3,=Y(BCSTTABQ)      AND BUMP TO PRIOR MONTH                     
         MVC   MNTHMRKS,BCSSTART                                                
         B     DOPFKEYX                                                         
*                                                                               
** SCROLL DOWN **                                                               
*                                                                               
PF08     DS    0H                                                               
         BAS   RE,LOCMMARK         GET R3-->1ST MONTH ON DISPLAY                
                                                                                
         CLI   BCSTTABQ(R3),XFF           IS IT THE LAST "VALID"                
         BE    PF08A                                                            
         TM    BCSFLAG+BCSTTABQ,BCSFMXQ    MONTH IN SCHED?                      
         BNZ   PF08A                                                            
         B     PF08B                                                            
                                                                                
PF08A    MVI   MYWRNCD,WBOTQ               YES, DISPLAY "WARNING" MSG           
         B     NO                                                               
PF08B    MVC   MNTHMRKS,BCSSTART+BCSTTABQ  NO, BUMP TO NEXT MONTH               
         B     DOPFKEYX                                                         
*                                                                               
** RE-DISPLAY **                                                                
*                                                                               
PF09     DS    0H                  NEEDN'T TO DO ANYTHING                       
         B     DOPFKEYX             USE THE SAME MNTHMRKS                       
*                                                                               
DOPFKEYX B     YES                                                              
         DROP  R3                                                               
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY'                    
***********************************************************************         
*==================== SUB-ROUTINE POOL INTERFACE =====================*         
                                                                                
GOSUB    NTR1  BASE=BASE1,LABEL=N                                               
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE               CLEAR RE JUST TO BE SAFE                     
                                                                                
         MVC   ASUBRTN,ASUBR01                                                  
         CLI   GOSUBN,R01#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR02                                                  
         CLI   GOSUBN,R02#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR03                                                  
         CLI   GOSUBN,R03#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR04                                                  
         CLI   GOSUBN,R04#                                                      
         BNH   GOSUBGO                                                          
         DC    H'0'                                                             
*                                                                               
GOSUBGO  GOTO1 ASUBRTN,DMCB,(GOSUBN,(RC)),(RA),(R9),(R8)                        
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
                                                                                
***********************************************************************         
*======================== CLEAR ERROR FIELDS =========================*         
CLRERRS  DS    0H                                                               
         MVI   MYERRCD,0                                                        
         MVI   ERROR,0                                                          
         BR    RE                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= LOCATE MONTH MARK =========================*         
                                                                                
* Points R3 to month, within BRDMTHTB, contained in MNTHMRKS                    
                                                                                
LOCMMARK DS    0H                                                               
         LA    R3,BRDMTHTB                                                      
         USING BCSTTABD,R3                                                      
LMM10    CLI   0(R3),XFF                                                        
         BE    LMMDIE                                                           
         CLC   MNTHMRKS,BCSSTART                                                
         BE    LMMX                                                             
LMM20    LA    R3,BCSTTABQ(R3)                                                  
         B     LMM10                                                            
*                                                                               
LMMX     DS    0H                                                               
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
LMMDIE   DC    H'0'                                                             
***********************************************************************         
                                                                                
                                                                                
***********************************************************************         
*======================== LOCATE MONTH ENTRY =========================*         
                                                                                
* Points R4 to the 1st week, within ACCUTAB, of month in DATE4.                 
                                                                                
         DS    0H                                                               
LOC1WMTH NTR1  BASE=BASE1,LABEL=N                                               
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(4,RE),=C'+LOC'                                                 
         SR    RE,RE               CLEAR RE JUST TO BE SAFE                     
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
L1WM10   CLC   =X'FFFFFF',0(R4)                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*****    CLC   ATMONTH,DATE4                                                    
*****    BE    L1WMX                                                            
         CLC   DATE4(2),ATMSTART      MAKE SURE THE MONTH IS IN                 
         BL    *+14                         THE ESTIMATE'S DATES                
         CLC   DATE4+2(2),ATMEND                                                
         BNH   L1WMX                                                            
         LA    R4,ACCUTABQ(R4)                                                  
         B     L1WM10                                                           
         DROP  R4                                                               
*                                                                               
L1WMX    DS    0H                                                               
         B     XREGS_R4            EXIT ROUTINE & PASS BACK R4                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* GET NUMBER OF LINES WE NEED TO DISPLAY                                        
*                                                                               
* ON EXIT:     RF                  # OF LINES                                   
***********************************************************************         
GTNWEEKS NTR1                                                                   
         SR    RF,RF                                                            
         L     RE,AACCUTAB                                                      
         USING ACCUTABD,RE                                                      
*                                                                               
GNW20    CLC   =X'FFFFFF',0(RE)                                                 
         BE    GNWX                                                             
         LA    RF,1(RF)                                                         
         LA    RE,ACCUTABQ(RE)     KEEP COUNTING THE NUMBER OF ENTRIES          
         B     GNW20                                                            
         DROP  RE                                                               
*                                                                               
GNWX     XIT1  REGS=(RF)                                                        
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* Points R4 to week entry corresponding to DATE2                                
***********************************************************************         
LOCDATE  NTR1  BASE=BASE1,LABEL=NO                                              
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
*                                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
LDT10    TM    ATFLAG,ATFSTTLQ                                                  
         BO    LDTXN                                                            
         CLC   ATWSTART,DATE2                                                   
         BH    LDTXN                                                            
         CLC   ATWEND,DATE2                                                     
         BNL   LDTX                                                             
*                                                                               
LDT20    LA    R4,ACCUTABQ(R4)                                                  
         B     LDT10                                                            
*                                                                               
LDTXN    SR    R4,R4               CAN'T LOCATE - PASS BACK NULLS IN R4         
*                                                                               
LDTX     B     XREGS_R4            EXIT ROUTINE & PASS BACK R4                  
         DROP  R4                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* Points R4 to schedule entry within ACCUTAB                                    
***********************************************************************         
LOCSKED  NTR1  BASE=BASE1,LABEL=N                                               
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
*                                                                               
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(4,RE),=C'+LOC'                                                 
         SR    RE,RE               CLEAR RE JUST TO BE SAFE                     
*                                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
LSK10    CLC   =X'FFFFFF',0(R4)                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    LSKX                                                             
         LA    R4,ACCUTABQ(R4)                                                  
         B     LSK10                                                            
*                                                                               
LSKX     B     XREGS_R4            EXIT ROUTINE & PASS BACK R4                  
         DROP  R4                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* Points R4 to bill adjustment entry in ACCUTAB of period                       
*  contained in DATE4.                                                          
* At entry,                                                                     
*    DATE4(4) = dates of period to match exactly,                               
*  or                                                                           
*    DATE4(2) = date w/in the month we want, and                                
*    DATE4+2(2) = NULLS.                                                        
* At exit,                                                                      
*    R4-->Bill Adj entry passed back to caller.                                 
***********************************************************************         
LOCBILAJ NTR1  BASE=BASE1,LABEL=NO                                              
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(4,RE),=C'+LOC'                                                 
         SR    RE,RE               CLEAR RE JUST TO BE SAFE                     
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
LBA10    CLC   =X'FFFFFF',0(R4)                                                 
         BE    LBADIE                                                           
         TM    ATFLAG,ATFBILAJ                                                  
         BZ    LBA20                                                            
         OC    DATE4+2(2),DATE4+2                                               
         BZ    LBA15                                                            
                                                                                
         DS    0H                  LOOK FOR EXACT MATCH                         
         CLC   ATMONTH,DATE4                                                    
         BE    LBAX                                                             
         B     LBA20                                                            
                                                                                
LBA15    DS    0H                  LOOK FOR WITHIN PERIOD                       
         CLC   ATMSTART,DATE4                                                   
         BH    LBA20                                                            
         CLC   ATMEND,DATE4                                                     
         BL    LBA20                                                            
         B     LBAX                                                             
*                                                                               
LBA20    LA    R4,ACCUTABQ(R4)                                                  
         B     LBA10                                                            
*                                                                               
LBAX     DS    0H                                                               
         B     XREGS_R4            EXIT ROUTINE & PASS BACK R4                  
*                                                                               
LBADIE   DC    H'0'                                                             
         DROP  R4                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= SCHEDULE CALCULATIONS =======================*         
                                                                                
*  Routines to pick out value enterred, and update corresponding                
*   value in ACCUTAB.                                                           
*  At entry to these routines,                                                  
*   R2-->respective line in TWA,                                                
*   R4-->respective entry in ACCUTAB.                                           
                                                                                
         USING MSLDSECT,R2                                                      
         USING ACCUTABD,R4                                                      
                                                                                
*------------------------ SCHEDL CS2 MODIFIED -----------------------*          
*                                                                               
MODSPW   NTR1                                                                   
         BAS   RE,GETPW            GET THE CS2 FROM SCHED LINE                  
         BNE   NO                   (TEMPPW HAS NEW CS2)                        
         L     RF,TEMPPW                                                        
         STCM  RF,15,ATCS2          AND UPDATE SKED CS2 IN ACCUTAB              
*                                                                               
         L     R4,AACCUTAB         UPDATE ENTIRE ACCUTAB                        
MODSPW10 TM    ATFLAG,ATFSTTLQ                                                  
         BO    MODSPWX                                                          
** BILLING ADJ SHOWN FOR MONTH **                                               
*                                                                               
MODSPW30 DS    0H                                                               
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWBUY#                                                    
         GOTO1 AGOSUB                                                           
         MVC   ATAJBUY,TEMPAJB                                                  
                                                                                
         ICM   RF,15,ATAJBUY                                                    
         ICM   RE,15,(ATAJBUY-ACCUTABD)(R3)                                     
         SR    RF,RE                                                            
         BNZ   *+8                                                              
         O     RF,=X'80000000'                                                  
         CLM   RF,15,(ATDRCR-ACCUTABD)(R3)   SEE IF CHANGE TO SKED CS2          
         BE    *+14                                                             
         OI    (ATFLAG2-ACCUTABD)(R3),ATF2CHDC  CHANGED DR/CR VALUE             
         MVC   (ATBILD-ACCUTABD)(,R3),CTODAY SET DATE OF CHANGE                 
         STCM  RF,15,(ATDRCR-ACCUTABD)(R3)                                      
*                                                                               
         MVC   ATCS2,TEMPPW        UPDATE THE CS2                               
         LA    R4,ACCUTABQ(R4)                                                  
         B     MODSPW10                                                         
*                                                                               
MODSPWX  DS    0H                                                               
         B     YES                                                              
         SPACE 2                                                                
*---------------------- SCHEDL ADJBUY MODIFIED -----------------------*         
*                                                                               
MODSAJB  NTR1                                                                   
         BAS   RE,GETAJB           GET THE ADJBUY FROM SCHED LINE               
         BNE   NO                   (TEMPAJB HAS NEW ADJBUY)                    
         L     RF,TEMPAJB                                                       
         STCM  RF,15,ATAJBUY        AND UPDATE SKED AJBUY IN ACCUTAB            
                                                                                
         BAS   RE,PW_AJB           GET CS2 FROM ADJBUY                          
         BNE   MODSAJNO             (SKED CS2 IN ACCUTAB UPDATED),              
         XC    MSLCOS2,MSLCOS2                                                  
         MVI   MSLCOS2,C'0'                                                     
         LA    R0,1                                                             
         ICM   RF,15,ATCS2                                                      
         BZ    MODSAJ15                                                         
         EDIT  (RF),(8,MSLCOS2),6,ALIGN=LEFT,FLOAT=-                            
MODSAJ15 STC   R0,MSLCOS2H+5        FUDGE IT INTO TWA, AND                      
                                                                                
         BAS   RE,MODSPW            PROCEED AS IF SCHED CS2 ENTERRED            
         BE    YES                                                              
MODSAJNO MVI   MYERRCD,OUTRNG2Q                                                 
         B     NO                                                               
         SPACE 2                                                                
*----------------------- MNTHLY CS2 MODIFIED ------------------------*          
*                                                                               
MODMPW   NTR1                                                                   
         LA    R3,ATMSTART         GET # OF WEEKS IN MONTH                      
         BAS   RE,GTNWEEKS                                                      
         ST    RF,FULL             FULL=# OF WEEKS IN BRDCST MONTH              
                                                                                
         BAS   RE,GETPW            GET THE CS2 FROM MONTH LINE                  
         BNE   NO                   (TEMPPW HAS NEW CS2)                        
         L     RF,TEMPPW                                                        
         STCM  RF,15,ATCS2          AND UPDATE MNTH CS2 IN ACCUTAB              
                                                                                
         LR    R3,R4                                                            
         SH    R3,=Y(ACCUTABQ)     R3-->BILL ADJ LINE OF MONTH                  
         TM    (ATFLAG2-ACCUTABD)(R3),ATF2DPLY                                  
         BO    MODMPW30                                                         
*                                                                               
** NO BILLING ADJUSTMENT **                                                     
*                                                                               
         L     R0,FULL                                                          
MODMPW10 SH    R4,=Y(ACCUTABQ)     BUMP BACKWARDS TO START OF MNTH              
         TM    ATFLAG,ATFSTTLQ+ATFBILAJ                                         
         BNO   *+8                                                              
         SH    R4,=Y(ACCUTABQ)      ONE MORE FOR BILL ADJ LINE                  
         BCT   R0,MODMPW10                                                      
                                                                                
         L     R0,FULL                                                          
MODMPW20 DS    0H                                                               
         TM    ESTFLAG,EFOWPW      IF OOW PW ESTIMATE,                          
         BO    MODMPW20G            SKIP WEEKLY-LEVEL TESTS                     
         TM    ATFLAG,ATFNOSPT     IF AT LEAST ONE SPOT IN WEEK,                
         BO    MODMPW22                                                         
         OC    ATACBUY,ATACBUY      AND THEY'RE NOT ZERO COST,                  
         BZ    MODMPW22                                                         
         TM    ATFLAG2,ATF2CCOD     AND CLCOST WAS NOT OVERRIDDEN,              
         BO    MODMPW22                                                         
MODMPW20G EQU  *                                                                
         STCM  RF,15,ATCS2          OVERRIDE THE CS2                            
                                                                                
MODMPW22 DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         BCT   R0,MODMPW20                                                      
         B     YES                                                              
*                                                                               
** BILLING ADJUSTMENT DISPLAYED **                                              
*                                                                               
MODMPW30 DS    0H                  R3-->BILL ADJ, R4-->MONTH TOTAL              
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVC   TEMPPW,ATCS2        ATCS2=PROJ CS2                               
         MVI   GOSUBN,PWBUY#                                                    
         GOTO1 AGOSUB                                                           
         MVC   ATAJBUY,TEMPAJB     ATAJBUY=PROJ FINAL BILL FOR MONTH            
                                                                                
         DS    0H                  CALCULATE DR/CR VALUE                        
         ICM   RF,15,ATAJBUY                                                    
         ICM   RE,15,(ATAJBUY-ACCUTABD)(R3)                                     
         SR    RF,RE                                                            
         BNZ   *+8                                                              
         O     RF,=X'80000000'                                                  
         CLM   RF,15,(ATDRCR-ACCUTABD)(R3)   SEE IF CHANGE TO MNTH CS2          
         BE    *+14                                                             
         OI    (ATFLAG2-ACCUTABD)(R3),ATF2CHDC  CHANGED DR/CR VALUE             
         MVC   (ATBILD-ACCUTABD)(,R3),CTODAY SET DATE OF CHANGE                 
         STCM  RF,15,(ATDRCR-ACCUTABD)(R3)                                      
         B     YES                                                              
         SPACE 2                                                                
*---------------------- MNTHLY ADJBUY MODIFIED -----------------------*         
*                                                                               
MODMAJB  NTR1                                                                   
         BAS   RE,GETAJB           GET THE ADJBUY FROM MONTH LINE               
         BNE   NO                   (TEMPAJB HAS NEW ADJBUY)                    
         MVC   ATAJBUY,TEMPAJB      AND UPDATE MNTH AJBUY IN ACCUTAB            
                                                                                
         BAS   RE,PW_AJB           GET CS2 FROM ADJBUY                          
         BNE   MODMAJNO             (MNTH CS2 IN ACCUTAB UPDATED),              
         XC    MSLCOS2,MSLCOS2                                                  
         MVI   MSLCOS2,C'0'                                                     
         LA    R0,1                                                             
         ICM   RF,15,ATCS2                                                      
         BZ    MODMAJ15                                                         
         EDIT  (RF),(8,MSLCOS2),6,ALIGN=LEFT                                    
MODMAJ15 STC   R0,MSLCOS2H+5        FUDGE IT INTO TWA, AND                      
                                                                                
         BAS   RE,MODMPW            PROCEED AS IF MONTH CS2 ENTERRED            
         BE    YES                                                              
MODMAJNO MVI   MYERRCD,OUTRNG2Q                                                 
         B     NO                                                               
         SPACE 2                                                                
*----------------------- WEEKLY CS2 MODIFIED ------------------------*          
*                                                                               
MODWPW   NTR1                                                                   
         BAS   RE,GETPW            GET THE CS2 FROM WEEK LINE                   
         BNE   NO                   (TEMPPW HAS NEW CS2)                        
         MVC   ATCS2,TEMPPW         AND UPDATE WEEK CS2 IN ACCUTAB              
         B     YES                                                              
         SPACE 2                                                                
*----------------------- WEEKLY ADJBUY MODIFIED ----------------------*         
*                                                                               
MODWAJB  NTR1                                                                   
         BAS   RE,GETAJB           GET ADJBUY FROM WEEK LINE                    
         BNE   NO                   (TEMPAJB HAS NEW ADJBUY)                    
         MVC   ATAJBUY,TEMPAJB      AND UPDATE WEEK AJBUY IN ACCUTAB            
                                                                                
         TM    ATFLAG2,ATF2CCOD    IF CLCOST WAS OVERRIDED,                     
         BZ    MWAJB10                                                          
         MVC   ATCS2,=X'80000000'   THEN ASSIGN FLAG TO CS2                     
         B     YES                                                              
                                                                                
MWAJB10  DS    0H                                                               
         BAS   RE,PW_AJB           GET CS2 FROM ADJBUY                          
         BE    YES                                                              
         MVI   MYERRCD,OUTRNG2Q     CS2 OUT OF RANGE                            
         B     NO                                                               
                                                                                
         SPACE 2                                                                
*--------------------- BILLING ADJUSTMENT MODIFIED -------------------*         
                                                                                
         DS    0H                                                               
MODDRCR  NTR1                                                                   
         MVI   MYERRCD,BAJZEROQ    ASSUME ERROR                                 
         CLI   MSLCCOSH+5,1        L(INPUT) MUST BE ONE                         
         BNE   NO                                                               
         CLI   MSLCCOSH+8,C'0'      AND INPUT MUST BE '0'                       
         BNE   NO                                                               
                                                                                
         XC    ATDRCR,ATDRCR       SET THE BILL ADJ VALUE                       
         OI    ATDRCR,X80           IN ACCUTAB                                  
         MVC   ATBILD,CTODAY        AND SET DATE OF CHANGE                      
                                                                                
         DS    0H                  UPDATE NEW PROJECTED VALUES                  
         ICM   R0,15,ATCS2                                                      
         ICM   R1,15,ATAJBUY                                                    
         LA    R4,ACCUTABQ(R4)     R4-->MONTH TOTAL NTRY IN ACCUTAB             
         STCM  R0,15,ATCS2          PROJECTED CS2                               
         STCM  R1,15,ATAJBUY        PROJECTED FINAL BILLING                     
                                                                                
         MVI   MYERRCD,0                                                        
         B     YES                                                              
         EJECT                                                                  
*------------------------- GET CS2 ENTERED -------------------------*           
                                                                                
* INPUTTED CS2 IS STORED IN TEMPPW AT EXIT                                      
                                                                                
GETPW    NTR1                                                                   
         MVI   MYERRCD,0           ASSUME NONE OF MY ERRORS                     
         ZIC   R0,MSLCOS2H+5                                                    
         GOTO1 CASHVAL,DMCB,(6,MSLCOS2),(R0)                                    
         CLI   DMCB,0                                                           
         BNE   NO                                                               
         MVI   MYERRCD,OUTRNGEQ    ASSUME BAD CS2 ENTERRED                      
         L     R0,4(R1)            MAKE SURE CS2 INPUTTED IS                    
         C     R0,=A(MINPWQ)                                                    
         BL    NO                   NOT LESS THAN MIN CS2                       
         C     R0,=A(MAXPWQ)                                                    
         BH    NO                   AND NOT BIGGER THAN MAX CS2                 
         ST    R0,TEMPPW           CS2 IS GOOD                                  
         B     YES                                                              
         SPACE 2                                                                
*------------------------ GET ADJBUY ENTERRED ------------------------*         
                                                                                
* Inputted ADJBUY is stored in TEMPAJB at exit                                  
                                                                                
         DS    0H                                                               
GETAJB   NTR1                                                                   
         XC    TEMPAJB,TEMPAJB                                                  
*                                                                               
         DS    0H                  CONVERT CLCOST INTO BINARY VALUE             
         MVI   MYERRCD,IFLDQ       ASSUME INVALID INPUT ERROR                   
         SR    R1,R1                                                            
         ICM   R1,1,MSLCCOSH+5                                                  
         BZ    GETAJBXN             IT'S INVALID IF NO INPUT                    
         LA    R3,MSLCCOS                                                       
         CLI   0(R3),C'='          CHECK FOR CLCOST OVERRIDES                   
         BNE   GETAJB10                                                         
         TM    ATFLAG,ATFSKMTH      ONLY APPLICABLE ON WEEKLY LINES             
         BNZ   GETAJBXN                                                         
         MVI   MYERRCD,ICCOVRDQ     ASSUME INVLD OVERRIDE FMT ERROR             
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNP   GETAJBXN                                                         
         LA    R3,MSLCCOS+1                                                     
                                                                                
GETAJB10 DS    0H                                                               
         LR    R0,R1                                                            
         MVI   MYERRCD,IFLDQ       ASSUME INVALID INPUT FIELD                   
                                                                                
         XC    WORK,WORK           MAKE SURE R3-->ALL NUMERICS                  
         STC   R1,WORK+5                                                        
         BCTR  R1,0                                                             
         EXMVC R1,WORK+8,0(R3)                                                  
         LA    R1,8+1(R1)                                                       
         STC   R1,WORK                                                          
         MVI   GOSUBN,VNUM#                                                     
         GOTO1 AGOSUB                                                           
         BNE   GETAJBXN                                                         
                                                                                
         GOTO1 CASHVAL,DMCB,(2,(R3)),(R0)                                       
         CLI   DMCB,0                                                           
         BNE   GETAJBXN                                                         
         L     R0,4(R1)            GET RESULT                                   
         LTR   R0,R0                                                            
         BM    GETAJBXN             ERROR IF INPUT IS NEGATIVE                  
         ST    R0,TEMPAJB                                                       
*                                                                               
         NI    ATFLAG2,XFF-ATF2CCOD                                             
         CLI   MSLCCOS,C'='        IF CLCOST OVERRIDE,                          
         BNE   GETAJB12                                                         
         OI    ATFLAG2,ATF2CCOD     TURN ON FLAG IN ACCUTAB                     
         B     GETAJBXY                                                         
                                                                                
GETAJB12 DS    0H                                                               
         OC    ATACBUY,ATACBUY     IF ACTUAL BUY IS ZERO,                       
         BNZ   GETAJB14                                                         
         LTR   R0,R0                AND ADJUSTED BUY INPUTTED ISN'T             
         BZ    GETAJBXY                                                         
         MVI   MYERRCD,NOACBQ       IT'S AN ERROR                               
         B     GETAJBXN                                                         
                                                                                
GETAJB14 DS    0H                  ACTUAL BUY IS NOT ZERO                       
         LTR   R0,R0                                                            
         BNZ   GETAJBXY             ADJ BUY CAN'T BE ZERO EITHER                
         MVI   MYERRCD,EAJB0Q                                                   
         B     GETAJBXN                                                         
*                                                                               
GETAJBXY DS    0H                  EXIT OKAY-LY                                 
         MVI   MYERRCD,0                                                        
         B     YES                                                              
GETAJBXN DS    0H                  DON'T EXIT OKAY-LY                           
         B     NO                                                               
         EJECT                                                                  
*------------------------- CS2 FROM ADJBUY --------------------------*          
                                                                                
* Calculates ATCS2 from ATAJBUY and updates ACCUTAB                             
                                                                                
PW_AJB   NTR1                                                                   
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPACB,ATACBUY     SET UP TO                                    
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#         CALCULATE CS2                               
         GOTO1 AGOSUB                                                           
         MVC   ATCS2,TEMPPW         AND UPDATE ACCUTAB                          
                                                                                
         ICM   R0,15,ATCS2         MAKE SURE CS2 CALCULATED                     
         C     R0,=A(MINPWQ)                                                    
         BL    NO                   NOT LESS THAN MIN VALUE                     
         C     R0,=A(MAXPWQ)                                                    
         BH    NO                   AND NOT BIGGER THAN MAX VALUE               
         B     YES                                                              
         SPACE 2                                                                
         DROP  R2,R4                                                            
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (MISC STUFF)'               
***********************************************************************         
*======================== MISCELLANEOUS STUFF ========================*         
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
XREGS_R4 XIT1  REGS=(R4)                                                        
                                                                                
*&&DO                                                                           
*---------------------------- SAVE ACCUTAB ---------------------------*         
SAVEACTB DS    0H                                                               
         ST    RE,HOLDRE                                                        
         L     R0,ATIA                                                          
         AH    R0,=Y(SVACTB-SPOTAREA)   DESTINATION                             
         LA    R1,L'SVACTB              L'DESTINATION                           
         L     RE,AACCUTAB              SOURCE                                  
         LR    RF,R1                    L'SOURCE                                
         MVCL  R0,RE                                                            
         L     RE,HOLDRE                                                        
         BR    RE                                                               
                                                                                
*--------------------------- RESTORE ACCUTAB -------------------------*         
RSTRACTB DS    0H                                                               
         ST    RE,HOLDRE                                                        
         L     R0,AACCUTAB              DESTINATION                             
         LA    R1,L'ACCUTAB             L'DESTINATION                           
         L     RE,ATIA                                                          
         AH    RE,=Y(SVACTB-SPOTAREA)   SOURCE                                  
         LR    RF,R1                    L'SOURCE                                
         MVCL  R0,RE                                                            
         L     RE,HOLDRE                                                        
         BR    RE                                                               
*&&                                                                             
         EJECT                                                                  
*---------------------------- GETEL MACRO ----------------------------*         
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*----------------- EXIT AND DISPLAY MESSAGE ROUTINES -----------------*         
                                                                                
MYERROR  DS    0H                                                               
         CLI   MYERRCD,PWNAVQ      IF IT'S THIS ERROR, DON'T DO                 
         BE    MYERROR2             ANYTHING BUT DISPLAY ERROR MESSAGE          
*****    BAS   RE,RSTRACTB                                                      
         MVI   GOSUBN,RTI#         KEEP INPUTTED BUT NOT YET VALIDATED          
         GOTO1 AGOSUB               FIELDS MODIFIED                             
         MVI   GOSUBN,MNI#         KEEP INPUTTED BUT NOT YET VALIDATED          
         GOTO1 (RF)                 FIELDS MODIFIED                             
MYERROR2 DS    0H                                                               
         MVI   BYTE,C'E'                                                        
         B     XMSGGO                                                           
                                                                                
MYWARN   DS    0H                                                               
         MVI   BYTE,C'W'                                                        
         B     XMSGGO                                                           
                                                                                
MYINFO   DS    0H                                                               
         MVI   BYTE,C'I'                                                        
         B     XMSGGO                                                           
                                                                                
XMSGGO   DS    0H                                                               
         GOTO1 AXMSGRTN,DMCB,(BYTE,(RC))                                        
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (LTORG && CONST)'           
***********************************************************************         
*======================== LTORG AND CONSTANTS ========================*         
         LTORG                                                                  
         SPACE 2                                                                
YESDC    DC    C'YES'                                                           
STAR3    DC    C'***'                                                           
PERIOD   DC    C'PER'                                                           
PF12DC   DC    CL7'12=Next'        APPEARS IF COMING FROM ACTN=LIST             
DCLIST   DS    0C                                                               
         DCDDL SP#TOTAL,6,L                                                     
         DCDDL SP#BLNG,7,L                                                      
         DCDDL SP#ADJ,3,L                                                       
         DCDDL SP#ALL,3,L                                                       
DCLISTX  DC    X'00'                                                            
DCRELOCK DC    BL2'0000000110100001'   X'01A1' CMPRSSD DATE FLG==>RELCK         
                                                                                
                                                                                
         DS    0CL(X'3000'-(*-T21759)+1)                                        
***********************************************************************         
         DROP  R5,R7,R8,R9,RA,RB,RC                                             
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01)'                              
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
SUBR01Q  EQU   (((*-T21759+4095)/4096)*4096)                                    
                                                                                
         ORG   T21759+SUBR01Q                                                   
SUBR01   NMOD1 0,**5901**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R01_00(R1)                                                       
                                                                                
BPK#     EQU   (R01_01-*)/4+1                                                   
GPR#     EQU   (R01_02-*)/4+1                                                   
*&&DO                                                                           
GBT#     EQU   (R01_03-*)/4+1                                                   
BPT#     EQU   (R01_04-*)/4+1                                                   
IAC#     EQU   (R01_05-*)/4+1                                                   
*&&                                                                             
GBG#     EQU   (R01_06-*)/4+1                                                   
SST#     EQU   (R01_07-*)/4+1                                                   
FLK#     EQU   (R01_08-*)/4+1                                                   
FDC#     EQU   (R01_09-*)/4+1                                                   
                                                                                
R01_00   DS    0H                                                               
R01_01   B     BC2KEY                                                           
R01_02   B     GETC2REC                                                         
*&&DO                                                                           
R01_03   B     GETBMTAB                                                         
R01_04   B     BPWTAB                                                           
R01_05   B     INITACTB                                                         
*&&                                                                             
R01_03   B     XIT_01                                                           
R01_04   B     XIT_01                                                           
R01_05   B     XIT_01                                                           
R01_06   B     GTBUYGOL                                                         
R01_07   B     SETSTATB                                                         
R01_08   B     FILLCKN                                                          
R01_09   B     FILLDRCR                                                         
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--BPK#)'                        
*---------------------------- BUILD PW KEY ---------------------------*         
*                                                                               
BC2KEY   DS    0H                                                               
         XC    KEY,KEY                                                          
C2K      USING PWRECD,KEY                                                       
         MVC   C2K.PWKTYP,=X'0D7A'                                              
         MVC   C2K.PWKAGMD,BAGYMD                                               
         MVC   C2K.PWKCLT,BCLT                                                  
         MVC   C2K.PWKPRD,BPRD                                                  
         MVC   C2K.PWKEST,BEST                                                  
         MVC   C2K.PWKMKT,BMKT                                                  
         DROP  C2K                                                              
         B     XIT_01                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--GPR#)'                        
*---------------------------- GET PW RECORD --------------------------*         
*                                                                               
GETC2REC DS    0H                                                               
         MVI   GOSUBN,BPK#         SET UP COST2 KEY                             
         GOTO1 AGOSUB                                                           
*                                                                               
         L     RE,AMYC2REC                                                      
         CLC   KEY(L'PWFKEY),0(RE) DO WE HAVE COST2 RECD ALREADY?               
         BNE   GPR05                  NOPE, GO READ IT FROM FILE                
         ZICM  RF,(PWLEN-PWRECD)(RE),(3)                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,AIO                 YES, GET FROM STORAGE TO SAVE I/O         
         LHI   R1,LIOS             R1 IS THIS MUCH SO IT'LL CLEAR AFTER         
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AIO                                                           
         CLI   MYIOFLAG,IOFADD     IF STATUS ALREADY SET TO ADD                 
         BE    GPR20               THEN LEAVE IT ALONE                          
         MVI   MYIOFLAG,IOFSTORG   COS2 RECORD EXISTS IN STORAGE                
         B     GPR20                                                            
*                                                                               
GPR05    L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         GOTO1 HIGH                                                             
         MVI   MYIOFLAG,IOFADD     ASSUME RECORD DOESN'T EXIST                  
         CLC   KEY(L'PWFKEY),KEYSAVE                                            
         BE    GPR10                                                            
*                                                                               
** NO PW RECD ON FILE--BUILD ONE **                                             
*                                                                               
         MVC   KEY(L'PWFKEY),KEYSAVE   CREATE NEW PW RECORD                     
         LR    RE,R6                    CLEAR I/O AREA                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVC   PWFKEY,KEY                                                       
         MVC   PWAGYA,TWAAGY                                                    
                                                                                
         DS    0H                       PWGNEL ELEMENT                          
         XC    PWGNEL(PWGNLENQ),PWGNEL                                          
         MVI   PWGNEL,PWGNELQ                                                   
         MVI   PWGNLEN,PWGNLENQ                                                 
         OI    PWGNFLG,PWGNPLKQ+PWGNBPLK  1ST TIME DEFAULTS TO LOCK             
                                                                                
         DS    0H                       PWDTIEL ELEMENT                         
         LA    RF,PWGNEL+PWGNLENQ                                               
         USING PWDTIEL,RF                                                       
         XC    PWDTICD(PWDTILNQ),PWDTICD                                        
         MVI   PWDTICD,PWDTICDQ                                                 
         MVI   PWDTILEN,PWDTILNQ                                                
         MVC   PWDTIPLD,CTODAY            1ST TIME DEFAULTS TO LOCK             
         DROP  RF                                                               
                                                                                
         DS    0H                       EORECORD MARKER                         
         MVI   PWDTILNQ(RF),0                                                   
         LA    RE,(PWEL-PWFKEY)+PWGNLENQ+PWDTILNQ+1                             
         STCM  RE,3,PWLEN               L(RECORD)                               
         B     GPR15               AND COPY INTO OUR STORAGE                    
*                                                                               
** PW RECD FOUND IN FILE--GET IT **                                             
*                                                                               
GPR10    MVI   MYIOFLAG,IOFWRITE   PW RECORD DOES EXIST                         
         GOTO1 GETREC              GET IT                                       
*                                                                               
GPR15    MVI   GOSUBN,PPR#         PUT RETRIEVED COS2 RECD INTO STORAGE         
         GOTO1 AGOSUB                                                           
*                                                                               
GPR20    NI    LOCKFLAG,XFF-LKFBPLK2                                            
         TM    PWGNFLG,PWGNBPLK    HAS BUY OR PW BEEN LOCKED BEFORE?            
         BZ    *+8                  NOPE                                        
         OI    LOCKFLAG,LKFBPLK2    YEP, FLAG IT                                
*                                                                               
         CLI   MYIOFLAG,IOFWRITE                                                
         BL    GPRX                                                             
         BE    GPR21                                                            
         MVI   MYIOFLAG,IOFWRITE                                                
         B     GPRX                                                             
*                                                                               
GPR21    LA    R6,ELEMENT          SET DEFAULT COST2 ELEMENT                    
         USING C2STEL,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   C2STCD,C2STCODQ     X'04'                                        
         MVI   C2STLEN,C2STLENQ                                                 
         MVC   C2STFCTR,OC2PCT                                                  
         CLC   =X'40404040',C2STFCTR                                            
         BE    *+14                                                             
         OC    C2STFCTR,C2STFCTR                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         L     R6,AMYC2REC         COPY C2STEL INTO ELEMENT TO SAVE             
         MVI   ELCODE,C2STCODQ        THE FACTORS                               
         BRAS  RE,GETEL                                                         
         BNE   GPR25                                                            
         USING C2STEL,R6                                                        
GPR23    XC    ELEMENT,ELEMENT                                                  
         XR    RE,RE                                                            
         ICM   RE,1,C2STLEN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         BRAS  RE,NEXTEL                                                        
         BE    GPR23                                                            
         LA    R6,ELEMENT                                                       
         OC    C2STFCTR,C2STFCTR   NOTHING IN THE FACTOR?                       
         BNZ   GPR25                                                            
         MVC   C2STFCTR,OC2PCT     ONE OF MEL'S CONVERSION BUGS                 
*                                                                               
GPR25    L     R6,AMYC2REC         COPY THE C2STEL TO GET FACTORS               
         MVI   ELEMENT,0                                                        
         MVI   GOSUBN,DLM#         ELEM CODE SET IN ELCODE                      
         GOTO1 AGOSUB              DELETE C2STEL TO ADD STATION ONES            
         MVI   ELEMENT,C2STCODQ                                                 
         MVI   ELEMENT+1,C2STLENQ                                               
*                                                                               
GPR30D   USING C2STEL,ELEMENT                                                   
GPR30    GOTO1 SEQ                 LOOK FOR STATION LEVEL RECORDS               
         CLC   KEY(PWKSTA-PWFKEY),KEYSAVE                                       
         BNE   GPR50                                                            
         GOTO1 GETREC                                                           
******                                                                          
         L     R4,AACCUTAB         FIND STATION IN OUR ACCUM TBL                
         USING ACCUTABD,R4                                                      
         LA    RE,ACCUTABX-ACCUTAB-1(R4)                                        
GPR40    TM    ATFLAG,ATFSTTLQ     HIT TOTALS OF THE ACCUM TABLE?               
         BNZ   GPR43                                                            
         CLC   ATSTAPKD,PWKSTA-PWFKEY+KEY   OUR STATION?                        
         BE    GPR46                                                            
         AHI   R4,ACCUTABQ         R4 = A(NEXT ENTRY)                           
         CR    R4,RE                                                            
         BL    GPR40                                                            
         DC    H'0'                HIT END OF THIS TABLE                        
*                                                                               
GPR43    LA    R3,ELEMENT+64       TEMP STORAGE                                 
         USING ACCUTABD,R4                                                      
         OC    ATLKCS2,ATLKCS2                                                  
         BNZ   *+10                                                             
         MVC   ATLKCS2,GPR30D.C2STPCT    MOVE IN LOCKED CS2                     
         OC    ATCS2,ATCS2                                                      
         BNZ   *+10                                                             
         MVC   ATCS2,GPR30D.C2STFCTR     MOVE IN CURRENT CS2                    
         MVC   ATLKCS2+ACCUTABQ,ATLKCS2                                         
         MVC   ATCS2+ACCUTABQ,ATCS2                                             
*                                                                               
*        MVC   0(ACCUTABQ*2+1,R3),0(R4)   BACKUP 2 TOTAL ENTRIES                
         MVC   0(ACCUTABQ*3,R3),0(R4)   BACKUP 2 TOTAL ENTRIES & FFFFFF         
         XC    0(ACCUTABQ,R4),0(R4)                                             
         MVC   ATSTAPKD,KEY+PWKSTA-PWFKEY                                       
         GOTO1 DATCON,DMCB,(0,ESDATE),(2,ATMSTART),0                            
         GOTO1 DATCON,DMCB,(0,EEDATE),(2,ATMEND),0                              
         MVC   ATWSTART(L'ATWEEK),ATMSTART                                      
         OI    ATFLAG,ATFNOSPT     INITIALIZE TO NO SPOTS                       
         MVC   ATLKCS2,GPR30D.C2STPCT    MOVE IN LOCKED CS2                     
         MVC   ATCS2,GPR30D.C2STFCTR     MOVE IN CURRENT CS2                    
*        MVC   ACCUTABQ(ACCUTABQ*2+1,R4),0(R3)                                  
         MVC   ACCUTABQ(ACCUTABQ*3,R4),0(R3)                                    
*                                                                               
GPR46    L     R6,AIO                                                           
         MVI   ELCODE,C2STCODQ     STATION ELEMENT (X'04')                      
         BRAS  RE,GETEL                                                         
         BNE   GPR47               NO LOCKED $, BUT HAS FACTORS                 
*                                                                               
         USING C2STEL,R6                                                        
GPR46A   MVC   GPR30D.C2STLKC2(8),C2STLKC2   COPY THE LOCKED $$                 
         TM    MISCFLG2,MF2RQADD   STATION LOCKIN REQUEST?                      
         BZ    *+14                                                             
         MVC   GPR30D.C2STLKC2(8),ATCLCK     COPY THE LOCKED $$                 
         B     *+10                                                             
         MVC   ATCLCK(8),C2STLKC2            COPY THE LOCKED $$                 
*****                                                                           
         BRAS  RE,NEXTEL           WE HAVE MORE THAN ONE?                       
         BE    GPR46A              YES, DUPLICATE CONV REQS                     
         DROP  R4,R6                                                            
*****                                                                           
GPR47    L     R6,AIO                                                           
         MVC   GPR30D.C2STSTAP,PWKSTA-PWRECD(R6)                                
         L     R6,AMYC2REC                                                      
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),ELEMENT,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    GPR30D.C2STLKC2(8),GPR30D.C2STLKC2                               
         XC    GPR30D.C2STSTAP,GPR30D.C2STSTAP                                  
         B     GPR30                                                            
*                                                                               
GPR50    L     R6,AMYC2REC         IF NO C2STEL IN STORAGE ADD ONE FOR          
         MVI   ELCODE,C2STCODQ        THE MARKET                                
         BRAS  RE,GETEL                                                         
         BE    GPR60                                                            
         L     R6,AMYC2REC                                                      
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),ELEMENT,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  GPR30D                                                           
*                                                                               
GPR60    MVI   GOSUBN,BPK#         SET UP COST2 KEY                             
         GOTO1 AGOSUB                                                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AMYC2REC                                                      
         L     R0,AIO              GET FROM STORAGE TO SAVE I/O                 
         ZICM  R1,(PWLEN-PWRECD)(RE),(3)                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   GOSUBN,BPK#         SET UP COST2 KEY                             
         GOTO1 AGOSUB                                                           
*                                                                               
GPRX     B     XIT_01                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--GBG#)'                        
*------------------------- GET BUYS AND GOALS ------------------------*         
                                                                                
*  Get actual buys and goals into ACCUTAB,                                      
*   and determine whether month is final billed                                 
                                                                                
GTBUYGOL DS    0H                                                               
         OI    MISCFLG1,MF1GOL0Q   ASSUME NO GOAL RECORDS                       
         OI    MISCFLG2,MF2BUY0Q   ASSUME NO BUYS EITHER                        
         XC    GOALGRP,GOALGRP     CLEAR OUT GRP                                
         L     R2,ASBLOCKD                                                      
         USING SBLOCKD,R2                                                       
         MVI   SPTIOMDE,SIOMBUY    START WITH READING BUYS ONLY                 
                                                                                
GBG002   DS    0H                  START OF A NEW SPOTIO MODE                   
         LA    RE,SBLOCK                                                        
         LA    RF,SPBKLEN                                                       
         XCEF                                                                   
                                                                                
         MVC   SBCOMFAC,ACOMFACS                                                
         MVC   SBAIO1(12),AIO1                                                  
                                                                                
         LA    R1,IOHOOK               SET ADDRESS OF IOHOOK,                   
         ST    R1,SBIOHOOK                                                      
         L     R0,ATIA                                                          
         LH    R1,=Y(ESTAB-SPOTAREA)                                            
         AR    R1,R0                                                            
         ST    R1,SBASVETB              ESTIMATE TABLE,                         
*&&DO                                                                           
         MVC   SBACHUNK,ASPCHUNK        CHUNKS TABLE,                           
*&&                                                                             
         LH    R1,=Y(SVSPARE-T217FFD)                                           
         LA    R1,T217FFD(R1)                                                   
         ST    R1,SBACHUNK              CHUNKS TABLE,                           
         LH    R1,=Y(SPTTAB-SPOTAREA)                                           
         AR    R1,R0                                                            
         ST    R1,SBASPTTB              AND SPOT TABLE                          
         LH    R1,=Y(SPTTABX-SPTTAB)                                            
         ST    R1,SBLSPTTB             L(SPOT TABLE)                            
                                                                                
         DS    0H                  1ST PASS-READ BUY & STTN BILL                
         NI    SBEFLAG,X'FF'-SBEWIPW                                            
         OI    SBEFLAG,SBE2COS                                                  
*&&DO                                                                           
         CLI   SPTIOMDE,SIOMBUY    PROCESSING BUY?                              
         BE    *+8                                                              
         OI    SBEFLAG,SBEWIPW     COMMENTED BECAUSE OF ROUNDING PROB           
*&&                                                                             
         CLI   USETAX,C'Y'                                                      
         BE    *+8                                                              
         OI    SBEFLAG,SBEPWNTX                                                 
         TM    ESTFLAG,EFBILEST    IF THIS IS AN E ESTIMATE,                    
         BZ    *+8                                                              
         OI    SBEFLAG,SBEEEST      TURN ON THIS FLAG FOR SPOTBUY               
         MVI   SBEPAID,C'Y'        EXTRACT PAID DATA                            
         MVI   SBEBYDT,C'Y'        GET AFFID DATE                               
                                                                                
         MVI   SBQSKIP,SBQSKGL+SBQSKBIL+SBQSKMED+SBQSKMKT                       
         MVI   SBQREAD,0                                                        
         CLI   SPTIOMDE,SIOMBUY    READING BUYS ONLY?                           
         BE    GBG005X                                                          
         MVI   SBQSKIP,SBQSKMED+SBQSKMKT                                        
         MVI   SBQREAD,SBQRDBH                                                  
         CLI   SPTIOMDE,SIOMNORM   READING NORMALLY?                            
         BE    GBG005X                                                          
         DC    H'0'                                                             
GBG005X  EQU   *                                                                
                                                                                
         MVC   SBQAGY,AGENCY                                                    
         MVC   SBQMED,QMED                                                      
         MVC   SBQCLT,QCLT                                                      
                                                                                
         MVC   SBQPRD,QPRD                                                      
         MVC   SBQBPRD,BPRD                                                     
         MVC   SBEPRD,BPRD                                                      
         TM    CLTFLAG,CFWSTRAD    IS IT A TRADE CLIENT?                        
         BO    GBGPRDG              YEP, TREAT PRODUCT DIFFERENTLY              
         TM    ESTFLAG,EFWSTRAD    IS IT A TRADE ESTIMATE?                      
         BO    GBGPRDG              YEP, TREAT PRODUCT DIFFERENTLY              
         B     GBGPRDX             ELSE, LEAVE PRODUCT ALONE                    
GBGPRDG  EQU   *                                                                
         MVC   SBQPRD,=C'POL'      SET TO PRODUCT TO POL                        
         MVI   SBQBPRD,XFF          TO READ ALL BRANDS                          
         MVI   SBEPRD,XFF                                                       
GBGPRDX  EQU   *                                                                
                                                                                
         MVC   SBQMKT,QMKT                                                      
         MVC   SBQSTA,QSTA                                                      
         MVC   SBBAGYMD,BAGYMD                                                  
         ZIC   R0,BEST                                                          
         STC   R0,SBQEST                                                        
         STC   R0,SBQESTND                                                      
         L     R1,SBASVETB                                                      
         XC    0(256,R1),0(R1)     CLEAR ESTIM TABLE                            
         AR    R1,R0                                                            
         MVI   0(R1),1                                                          
                                                                                
         MVC   SBAMKEST,AMKESTAB   A(MKT/EST TABLE)                             
                                                                                
         L     RE,SBACHUNK         CLEAR CHUNK TABLE                            
         LH    RF,=Y(CHUNKX-CHUNK)                                              
         XCEF                                                                   
         L     RE,SBASPTTB         CLEAR SPOTTAB TABLE                          
         L     RF,SBLSPTTB                                                      
         XCEF                                                                   
                                                                                
         LA    R1,BRDWKTB2                                                      
         ST    R1,SBADATE          SET A(DATES),                                
         ZIC   R1,NUMWEEKS                                                      
         ST    R1,SBNDATES          AND THE NUMBER OF THEM                      
                                                                                
         XC    ELEM,ELEM           CLEAR ELEM FIELD,                            
         LA    R1,ELEM              AND USE IT AS AN 110-BYTE WRK AREA          
         STCM  R1,7,SBAWIPW         FOR SPOTIO TO DO ITS PW STUFF               
                                                                                
         MVI   GOSUBN,STW#         SAVE TWA DATA BEFORE SPOTIO CALL             
         GOTO1 AGOSUB                                                           
*                                                                               
         GOTO1 ASPOTIO,DMCB,SBLOCK                                              
*                                                                               
         MVI   GOSUBN,RTW#         RESTORE TWA DATA AFTER SPOTIO CALL           
         GOTO1 AGOSUB                                                           
                                                                                
                                                                                
*                                                                               
** POST-SPOTIO TASKS **                                                         
*                                                                               
         CLI   SPTIOMDE,SIOMNORM                                                
         BE    GBG009                                                           
         CLI   SPTIOMDE,SIOMBUY                                                 
         BE    GBG100                                                           
         DC    H'0'                                                             
                                                                                
*                                                                               
GBG009   DS    0H                                                               
         NI    MISCFLG2,XFF-MF2GRPC                                             
         TM    MISCFLG1,MF1GOL0Q   IF GOALS EXIST ON FILE,                      
         BO    GBG09D                                                           
         NI    MISCFLG3,XFF-MF3UGRP USER COULD NOT INPUT GRP AMOUNT             
         CLC   PWGRP,GOALGRP        COMPARE PW/REC & GOAL/REC GRP               
         BE    GBG09X                                                           
         OI    MISCFLG2,MF2GRPC    IF THEY ARE DIFFERENT,                       
         MVC   PWGRP,GOALGRP        GET TOTAL GRP                               
         B     GBG09X                                                           
                                                                                
GBG09D   DS    0H                  NO GOALS ON FILE                             
         TM    MISCFLG3,MF3UGRP     IF GRP WAS USER-INPUTTED,                   
         BNZ   *+10                  THEN LEAVE IT ALONE                        
         XC    PWGRP,PWGRP                                                      
GBG09X   EQU   *                                                                
                                                                                
                                                                                
GBG10    DS    0H                  TRANSFER BILL FLAGS TO SKED LINE             
*&&DO                                                                           
         MVI   BYTE,ATFESTBQ+ATFFNLBQ   ASSUME SKED IS BOTH ESTIM               
         L     R4,AACCUTAB               AND FINAL BILLED                       
         USING ACCUTABD,R4                                                      
GBG10A   TM    ATFLAG,ATFSTTLQ                                                  
         BO    GBG10C                                                           
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    GBG10B                                                           
                                                                                
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILLING ADJ LINE                   
         TM    ATFLAG,ATFFNLBQ     IF ANY MONTH DOES NOT HAVE                   
         BO    *+8                                                              
         NI    BYTE,XFF-ATFFNLBQ    A BILLING (EST OR FINAL),                   
         TM    ATFLAG,ATFESTBQ      THEN THE SKED TOTAL LINE                    
         BO    *+8                                                              
         NI    BYTE,XFF-ATFESTBQ    WILL ALSO NOT HAVE THAT BILLING             
                                                                                
GBG10B   LA    R4,ACCUTABQ(R4)                                                  
         B     GBG10A                                                           
GBG10C   OC    ATFLAG,BYTE         TRANSFER FLAGS TO SKED FLAGS                 
         OC    ATFLAG+ACCUTABQ,BYTE                                             
         DROP  R4                                                               
                                                                                
                                                                                
GBB20    DS    0H                  CHECK FOR ANY SPOTS IN SKED                  
         MVI   BYTE,XFF             SET FLAG FOR SKED'S ATFLAG                  
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
GBB20A   TM    ATFLAG,ATFSTTLQ                                                  
         BO    GBB20X                                                           
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    GBB20C                                                           
                                                                                
         TM    ATFLAG,ATFNOSPT     ANY SPOTS FOR THIS WEEK?                     
         BO    GBB20D                                                           
*                                                                               
GBB20B   LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ      YES, FAST FOWARD TO MONTH LINE              
         BZ    GBB20B                                                           
         NI    ATFLAG,XFF-ATFNOSPT   AND INDICATE SPOT FOR MNTH BL ADJ          
         NI    ATFLAG+ACCUTABQ,XFF-ATFNOSPT  AND TOTAL LINES                    
         MVI   BYTE,XFF-ATFNOSPT   SET FLAG FOR SKED LINE                       
*                                                                               
GBB20C   LA    R4,ACCUTABQ(R4)                                                  
GBB20D   LA    R4,ACCUTABQ(R4)                                                  
         B     GBB20A                                                           
*                                                                               
GBB20X   NC    ATFLAG,BYTE           SET FLAG ON SKED BILL ADJ                  
         NC    ATFLAG+ACCUTABQ,BYTE   AND SKED TOTAL ENTRIES                    
         DROP  R4                                                               
                                                                                
                                                                                
         DS    0H                  CHECK FOR ENTIRE SKED PAID                   
         MVI   BYTE,0              ASSUME ENTIRE SKED IS PAID                   
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
GBB30A   TM    ATFLAG,ATFSTTLQ                                                  
         BO    GBB30X                                                           
         TM    ATFLAG,ATFMTTLQ     IF MONTH LINE IS REACHED HERE,               
         BO    GBB30C               BUMP TO NEXT MONTH                          
                                                                                
         TM    ATFLAG2,ATF2UNPD    WAS ANYTHING UNPAID IN THIS WEEK?            
         BZ    GBB30D               NO, BUMP TO NEXT WEEK                       
                                                                                
         MVI   BYTE,ATF2UNPD        YES, CHANGE SKED'S FLAG, AND                
GBB30B   LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    GBB30B                                                           
         OI    ATFLAG2,ATF2UNPD           MONTHLY BILL ADJ LINE                 
         OI    ATFLAG2+ACCUTABQ,ATF2UNPD  MONTHLY TOTAL LINE                    
                                                                                
GBB30C   LA    R4,ACCUTABQ(R4)                                                  
GBB30D   LA    R4,ACCUTABQ(R4)                                                  
         B     GBB30A                                                           
                                                                                
GBB30X   DS    0H                                                               
         OC    ATFLAG2,BYTE               SKED BILL ADJ LINE                    
         OC    ATFLAG2+ACCUTABQ,BYTE      SKED TOTAL LINE                       
         DROP  R4                                                               
*&&                                                                             
         B     GBG500                                                           
                                                                                
                                                                                
GBG100   DS    0H                  SPTIOMDE = SIOMBUY                           
         MVI   BYTE,0               ASSUME ENTIRE SKED UNPAID                   
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
                                                                                
*                                                                               
GBG102   DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    GBG108                                                           
         TM    ATFLAG,ATFMTTLQ      IF MONTH LINE IS REACHED HERE,              
         BO    GBG105                BUMP TO NEXT MONTH                         
                                                                                
         TM    ATFLAG2,ATF2A1PS     WAS ANYTHING PAID IN THIS WEEK?             
         BZ    GBG106                NO, BUMP TO NEXT WEEK                      
                                                                                
         MVI   BYTE,ATF2A1PS         YES, CHANGE SKED'S FLAG, AND               
GBG104   LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    GBG104                                                           
         OI    ATFLAG2,ATF2A1PS           MONTHLY BILL ADJ LINE                 
         OI    ATFLAG2+ACCUTABQ,ATF2A1PS  MONTHLY TOTAL LINE                    
                                                                                
GBG105   LA    R4,ACCUTABQ(R4)                                                  
GBG106   LA    R4,ACCUTABQ(R4)                                                  
         B     GBG102                                                           
                                                                                
GBG108   DS    0H                                                               
         OC    ATFLAG2,BYTE               SKED BILL ADJ LINE                    
         OC    ATFLAG2+ACCUTABQ,BYTE      SKED TOTAL LINE                       
         DROP  R4                                                               
*                                                                               
         B     GBG500                                                           
         EJECT                                                                  
*                                                                               
** END OF A SPOTIO MODE **                                                      
*                                                                               
GBG500   DS    0H                                                               
         CLI   SPTIOMDE,SIOMBUY    IF PROCESSING ONLY BUY,                      
         BNE   *+12                                                             
         MVI   SPTIOMDE,SIOMNORM    GO BACK AND DO NORMAL PROCESSING            
         B     GBG002                                                           
*                                                                               
         CLI   SPTIOMDE,SIOMNORM   IF NORMAL PROCESSING,                        
         BNE   *+8                                                              
         B     GBGX                 WE CAN EXIT NOW                             
*                                                                               
         DC    H'0'                                                             
                                                                                
*                                                                               
GBGX     B     XIT_01                                                           
         EJECT                                                                  
*                                                                               
** SPOTIO HOOK **                                                               
*                                                                               
IOHOOK   NTR1                                                                   
         L     R6,SBAIO1           CHECK IF RECORD IS DELETED                   
         TM    (BUYRCNTL-BUYRECD)(R6),X80                                       
         BNZ   IOHKXIT              YES IT IS                                   
                                                                                
         CLI   SBMODE,SBPROCSP                                                  
         BE    PROCBUY                                                          
         CLI   SBMODE,SBPROCBL                                                  
         BE    PROCSBIL                                                         
         CLI   SBMODE,SBPROCGL                                                  
         BE    PROCGOAL                                                         
         CLI   SBMODE,SBPROCBH                                                  
         BE    PROCBLHD                                                         
         B     IOHKXIT                                                          
         EJECT                                                                  
PROCBUY  DS    0H                  R6-->BUY RECORD                              
         MVI   GOSUBN,PPDS#                                                     
         CLI   SPTIOMDE,SIOMBUY                                                 
         BE    PROCBUYG                                                         
         MVI   GOSUBN,PBUY#                                                     
         CLI   SPTIOMDE,SIOMNORM                                                
         BE    PROCBUYG                                                         
         DC    H'0'                                                             
PROCBUYG EQU   *                                                                
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         B     IOHKXIT                                                          
         EJECT                                                                  
PROCSBIL DS    0H                  R6-->STATION BILL RECORD                     
                                                                                
         USING STABUCKD,R6                                                      
         CLC   STABKMKT,BMKT       MATCH ON MKT NUMBER                          
         BNE   IOHKXIT                                                          
         OC    BSTA,BSTA            AND STATION IF DDS TESTING                  
         BZ    *+14                                                             
         CLC   STABKSTA,BSTA                                                    
         BNE   IOHKXIT                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'0E'                                                     
         MVC   DATADISP,=Y(STABELEM-STABUCKD)                                   
         BRAS  RE,GETEL                                                         
PSBL05B  DS    0H                                                               
         BNE   PSBLX                                                            
         USING STABELEM,R6                                                      
                                                                                
         DS    0H                  FIND MONTH NTRY W/IN ACCUTAB                 
         MVC   STARTEND(12),ESDATE JUST USE THE ESTIMATE'S START/END            
         TM    ESTFLAG,EFBILEST     IF THIS IS AN E ESTIMATE,                   
         BO    PSBL07               AS PER GRANT                                
                                                                                
         MVC   WORK(2),STABPER     SET UP BINARY YEAR, MONTH                    
         MVI   WORK+2,1             DAY                                         
         GOTO1 DATCON,DMCB,(3,WORK),(0,MYDATE6),0                               
PSBL06   GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
*****                                                                           
         CLI   DMCB,X'FF'           ERROR FROM GETBROAD                         
         BNE   PSBL06A                                                          
         CLC   =C'13',MYDATE6+2                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYDATE6+2(2),=C'12'                                              
         B     PSBL06                                                           
*****                                                                           
PSBL06A  CLC   STARTEND(6),EEDATE     START DATE AFTER ESTIMATE END?            
         BNH   *+10                                                             
         MVC   STARTEND(12),ESDATE                                              
*                                                                               
         CLC   STARTEND(6),ESDATE     FORCE DATE TO START ON OR                 
         BNL   *+10                    AFTER ESTIMATE'S START DATE              
         MVC   STARTEND(6),ESDATE                                               
*                                                                               
         CLC   STARTEND+6(6),EEDATE   FORCE DATE TO END ON OR                   
         BNH   *+10                    BEFORE ESTIMATE'S END DATE               
         MVC   STARTEND+6(6),EEDATE                                             
                                                                                
PSBL07   DS    0H                                                               
         GOTO1 DATCON,DMCB,(X'10',STARTEND),(2,WORK),0                          
         MVC   DATE4(2),WORK                                                    
         MVC   DATE4+2(2),WORK+3   REMOVE HYPHEN FROM DATCON OUTPUT             
*                                                                               
         DS    0H                  DATE4=STRT/END OF TARGET BCAST MNTH          
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
PSBL10   CLC   =X'FFFFFF',0(R4)                                                 
         BNE   PSBL13X                                                          
         BRAS  RE,NEXTEL           CAN'T MATCH MONTH--TRY NEXT ELEMENT          
         B     PSBL05B                                                          
PSBL13X  EQU   *                                                                
                                                                                
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    PSBL20                                                           
         CLC   ATMONTH,DATE4                                                    
         BE    PSBL30                                                           
PSBL20   LA    R4,ACCUTABQ(R4)                                                  
         B     PSBL10                                                           
                                                                                
PSBL30   DS    0H                                                               
         OI    ATFLAG,ATFESTBQ     AUTOMATICALLY ESTIM BILLED                   
         OI    ATFLAG+ACCUTABQ,ATFESTBQ                                         
*                                                                               
PSBLX    DS    0H                                                               
         B     IOHKXIT                                                          
         DROP  R4,R6                                                            
         EJECT                                                                  
PROCGOAL DS    0H                  R6-->GOAL RECORD                             
                                                                                
         MVI   BYTE,C'N'           ASSUME NO X'21' ELEM IN RECD                 
         MVI   ELCODE,X'21'        GET GRP FROM GOAL WEEK ELEMENT               
         MVC   DATADISP,=Y(GDELEM-GOALREC)                                      
         BRAS  RE,GETEL                                                         
         B     PGOL05A                                                          
PGOL05   BRAS  RE,NEXTEL                                                        
PGOL05A  BNE   PGOL05X                                                          
         USING GLEMENT,R6                                                       
         MVI   BYTE,C'Y'           X'21' ELEM EXIST IN THIS RECD                
         ICM   R1,15,GLGRP                                                      
         N     R1,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         TM    GLGRP,GLGRP2DEC                                                  
         BZ    PGOL05B                                                          
* ROUND 2-DEC GOAL TO 1-DEC                                                     
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
PGOL05B  A     R1,GOALGRP                                                       
         ST    R1,GOALGRP                                                       
         B     PGOL05                                                           
         DROP  R6                                                               
PGOL05X  DS    0H                                                               
*                                                                               
         CLI   BYTE,C'Y'                IF A X'21' ELEM FOUND,                  
         BNE   IOHKXIT                                                          
         NI    MISCFLG1,XFF-MF1GOL0Q     MARKET HAS GOALS                       
*                                                                               
         MVC   DMCB+4,OC2PCT       PASS THE VALUE, NOT THE A(VALUE)             
         MVI   DMCB+4,X'FD'                                                     
         GOTO1 ASPOTGL,DMCB,SBLOCK,,0                                           
         L     R3,SBACHUNK                                                      
         USING SGLCHNKD,R3                                                      
PGOL10   OC    SGNEXT,SGNEXT                                                    
         BZ    IOHKXIT                                                          
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
PGOL10A  TM    ATFLAG,ATFSTTLQ+ATFMTTLQ                                         
         BNZ   PGOL20                                                           
         CLC   ATWSTART,SGDATE     FIND WITHIN DATE RANGE                       
         BH    PGOL30                                                           
         CLC   ATWEND,SGDATE                                                    
         BNL   PGOL20               FOUND RANGE!!                               
PGOL10B  LA    R4,ACCUTABQ(R4)                                                  
         B     PGOL10A                                                          
*                                                                               
PGOL20   ICM   R1,15,ATACGOAL                                                   
         A     R1,SGDOL                                                         
         STCM  R1,15,ATACGOAL                                                   
*                                                                               
         L     R1,SGDOL                                                         
         CVD   R1,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         SRP   WORK(16),6,0        MULTIPLY BY 1,000,000                        
         XR    R1,R1                                                            
         L     R1,TEMPPW                                                        
         CVD   R1,DUB                                                           
*                                                                               
         LA    RE,8                NUMBER OF BYTES OF DIVISOR                   
         LA    RF,DUB              1ST BYTE OF DIVISOR                          
PGOL22   CLI   0(RF),0             FIRST BYTE OF DIVISOR                        
         BNE   PGOL24                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,PGOL22           LAST BYTE SHOULD NEVER BE 0                  
*                                                                               
PGOL24   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         DP    WORK(16),0(0,RF)                                                 
*                                                                               
         AHI   RE,2                HIGH ORDER PORTION IS THE QUOTIENT           
         LA    RF,16                                                            
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,WORK(0)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         ICM   RE,15,ATAJGOAL      WIMBUDG$ (ADJUSTED GOAL) ACCUMULATOR         
         AR    RE,R1                                                            
         STCM  RE,15,ATAJGOAL                                                   
*                                                                               
         TM    ATFLAG,ATFSTTLQ+ATFMTTLQ                                         
         BNO   PGOL30                                                           
         MVC   ATACGOAL+ACCUTABQ,ATACGOAL                                       
         MVC   ATAJGOAL+ACCUTABQ,ATAJGOAL                                       
*                                                                               
PGOL30   L     R3,SGNEXT                                                        
         B     PGOL10                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
PROCBLHD DS    0H                  R6-->BILL RECORD                             
                                                                                
         USING BILLRECD,R6                                                      
         CLC   BLMKT,SPACES                                                     
         BE    *+14                                                             
         CLC   BLMKT,QMKT          IS IT THE RIGHT MARKET?                      
         BNE   PBHDX                                                            
                                                                                
         DS    0H                  FIND MONTH ENTRY W/IN ACCUTAB                
         MVC   STARTEND,ESDATE     JUST USE THE ESTIMATE'S START/END            
         TM    ESTFLAG,EFBILEST     IF THIS IS AN E ESTIMATE,                   
         BO    PBHD07                                                           
                                                                                
         MVC   WORK(2),BKEYYSRV    SET UP BINARY YEAR, MONTH                    
         MVI   WORK+2,1             DAY                                         
         GOTO1 DATCON,DMCB,(3,WORK),(0,MYDATE6),0                               
PBHD06   GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
*****                                                                           
         CLI   DMCB,X'FF'           ERROR FROM GETBROAD                         
         BNE   PBHD06A                                                          
         CLC   =C'13',MYDATE6+2                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYDATE6+2(2),=C'12'                                              
         B     PBHD06                                                           
*****                                                                           
PBHD06A  CLC   STARTEND(6),EEDATE     START DATE AFTER ESTIMATE END?            
         BNH   *+10                                                             
         MVC   STARTEND(12),ESDATE                                              
*                                                                               
         CLC   STARTEND(6),ESDATE     FORCE DATE TO START ON OR                 
         BNL   *+10                    AFTER ESTIMATE'S START DATE              
         MVC   STARTEND(6),ESDATE                                               
*                                                                               
         CLC   STARTEND+6(6),EEDATE   FORCE DATE TO END ON OR                   
         BNH   *+10                    BEFORE ESTIMATE'S END DATE               
         MVC   STARTEND+6(6),EEDATE                                             
                                                                                
PBHD07   DS    0H                                                               
         GOTO1 DATCON,DMCB,(X'10',STARTEND),(2,WORK),0                          
         MVC   DATE4(2),WORK                                                    
         MVC   DATE4+2(2),WORK+3   REMOVE HYPHEN FROM DATCON OUTPUT             
*&&DO                                                                           
         DS    0H                  ADJST TO USE CORRECT MTH IN CASE EST         
         DS    0H                   SPANS INTO NXT MTH NOT IN ACCUTAB           
         LA    R3,BRDMTHTB                                                      
         USING BCSTTABD,R3                                                      
PBHD08B  CLI   0(R3),XFF           END OF BROADCAST MONTH TABLE?                
         BNE   PBHD08E              NO, GO CHECK THIS B'CST MONTH               
         MVC   WORK+0(2),BKEYYSRV   YEP, GET YEAR/MONTH FOR ERROR MSG           
         MVI   WORK+2,0                                                         
         GOTO1 DATCON,DMCB,(X'83',WORK),(6,MYTEXT+1),0                          
         MVC   MYTEXT(1),DMCB+4      LENGTH OF REPLACE TEXT                     
         MVI   MYERRCD,BHDATQ                                                   
         B     PBHDX                                                            
PBHD08E  EQU   *                                                                
                                                                                
         CLC   BCSSTART(4),DATE4   MATCH ON B'CST MTH START/END                 
         BE    *+12                                                             
         LA    R3,BCSTTABQ(R3)                                                  
         B     PBHD08B                                                          
                                                                                
         TM    BCSFLAG,BCSFMXQ     IF B'CST MTH IS EFFECTIVE EOT,               
         BZ    *+8                                                              
         SH    R3,=Y(BCSTTABQ)      THEN USE PREVIOUS B'CST MONTH               
         MVC   DATE4,BCSSTART      SET B'CST MTH START/END IN FULL              
         DROP  R3                                                               
*&&                                                                             
         DS    0H                  DATE4=STRT/END OF TARGET BCAST MNTH          
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
PBHD10   CLC   =X'FFFFFF',0(R4)                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ATMSTART,DATE4                                                   
         BH    PBHD20                                                           
         CLC   ATMEND,DATE4                                                     
         BNL   PBHD30                                                           
PBHD20   LA    R4,ACCUTABQ(R4)                                                  
         B     PBHD10                                                           
                                                                                
PBHD30   DS    0H                                                               
         OI    ATFLAG,ATFESTBQ     AUTOMATICALLY ESTIMATE BILLED                
         OI    ATFLAG+ACCUTABQ,ATFESTBQ                                         
         TM    BILSTAT2,BSTCLRDQ   LOOK TO SEE IF FINAL BILLED                  
         BZ    PBHDX                                                            
         OI    ATFLAG,ATFFNLBQ             YES IT IS                            
         OI    ATFLAG+ACCUTABQ,ATFFNLBQ                                         
*                                                                               
PBHDX    B     IOHKXIT                                                          
         DROP  R4,R6                                                            
                                                                                
                                                                                
IOHKXIT  DS    0H                                                               
         XIT1                                                                   
         DROP  R2                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--SST#)'                        
***********************************************************************         
* STATION ACCUMULATOR TABLE ROUTINES                                            
*                                                                               
* ON ENTRY:    FULL(3)             STATION PACKED                               
*              (R3)                A(SPOTBUY DATA CHUNK)                        
*              (R4)                A(CORR WEEK IN ACCUTAB)                      
***********************************************************************         
SETSTATB DS    0H                                                               
         USING SCHUNKD,R3                                                       
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*&&DO                                                                           
         MVC   DATE2,ATWSTART      GET DATE OF WEEK SPOT IN                     
                                                                                
         LA    R5,BRDWKTAB         FIRST MAKE SURE DATE IS W/IN SCHED           
         TM    ESTFLAG,EFBILEST    IF ONLY ONE PW PERIOD,                       
         BZ    SST05                                                            
         LA    R5,BRDMTHTB          USE MONTH TABLE                             
*                                                                               
SST05    CLI   0(R5),XFF                                                        
         BE    SST05DIE                                                         
*                                                                               
         CLC   0(2,R5),SCDATE       STRT DATE CAN'T BE HIGHER THAN IT           
*                                                                               
         CLC   0(2,R5),DATE2        STRT DATE CAN'T BE HIGHER THAN IT           
         BH    SST05DIE                                                         
*                                                                               
         CLC   2(2,R5),SCDATE                                                   
*                                                                               
         CLC   2(2,R5),DATE2                                                    
         BNL   SST05X                                                           
         LA    R5,BCSTTABQ(R5)                                                  
         B     SST05                                                            
SST05DIE DC    H'0'                 NOT W/IN SCHED, DIE                         
SST05X   MVC   DUB(4),0(R5)        DUB(4)=START/END OF WEEK                     
*&&                                                                             
         DS    0H                  LOOK FOR STATION IN ACCUM TBL                
         L     R5,TSARBLK+(TSAREC-TSARD)                                        
         LA    R5,0(R5)            POINT TO STATION ACCUM RECD AREA             
         USING STACRECD,R5                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVC   STACSTA,FULL                                                     
         MVI   GOSUBN,TSR_RDH#                                                  
                                                                                
SST10    DS    0H                                                               
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     EOF IN STTN ACCUM TABLE?                     
         BNZ   SST20                NO, NOT YET                                 
*&&DO                                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
                                                                                
         CLC   STACSTA,FULL                                                     
*****    BNE   SST10A                                                           
*****    CLC   STACSTRT(4),DUB     FIND WEEK                                    
         BE    SST30                                                            
SST10A   MVI   GOSUBN,TSR_NXT#                                                  
         B     SST10                                                            
*                                                                               
SST20    DS    0H                  ADD ENTRY TO TABLE                           
         XC    STACRECD(STACRECL),STACRECD                                      
         MVC   STACSTA,FULL         MOVE IN STATION                             
*****    MVC   STACSTRT(4),DUB      MOVE IN START/END DATES OF WEEK             
*                                                                               
SST30    DS    0H                  UPDATE TABLE                                 
         ICM   R1,15,STACSPT                                                    
         A     R1,SCSPOTS           # OF SPOTS                                  
         STCM  R1,15,STACSPT                                                    
         ICM   R1,15,STACGRS                                                    
         A     R1,SCGROSS           GROSS $                                     
         STCM  R1,15,STACGRS                                                    
         ICM   R1,15,STACNET                                                    
         A     R1,SCNET             NET $                                       
         STCM  R1,15,STACNET                                                    
         ICM   R1,15,STACTAX                                                    
         A     R1,SCTAX             TAX $                                       
         STCM  R1,15,STACTAX                                                    
         ICM   R1,15,STACCTX                                                    
         A     R1,SCPWCLTX          CLIENT TAX $                                
         STCM  R1,15,STACCTX                                                    
         ICM   R1,15,STACCGRS      UPDATE GROSS CLCOST$ (ADJ BUY)               
         A     R1,SCPWGRS                                                       
         STCM  R1,15,STACCGRS       AND UPDATE STACTB                           
         ICM   R1,15,STACCNET      UPDATE NET   CLCOST$ (ADJ BUY)               
         A     R1,SCPWNET                                                       
         STCM  R1,15,STACCNET       AND UPDATE STACTB                           
*                                                                               
         MVI   GOSUBN,TSR_PUT#     PUT STTN ACCUM RECD BACK TO TABLE            
         TM    MYTSERRS,TSEEOF                                                  
         BZ    *+8                                                              
         MVI   GOSUBN,TSR_ADD#      OR ADD IT IF NOT IN TABLE YET               
         GOTO1 AGOSUB                                                           
         BE    SST039                                                           
         MVI   MYWRNCD,WECSTQ      NO MORE ROOM IN STTN ACCUM TABLE?            
         TM    MYTSERRS,TSEEOF                                                  
         BNZ   SST039                                                           
         DC    H'0'                                                             
SST039   EQU   *                                                                
*                                                                               
SSTX     DS    0H                                                               
         B     XIT_01                                                           
         DROP  R3,R4,R5                                                         
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--FLK#)'                        
*--------------------------- FILL IN LOCKINS -------------------------*         
                                                                                
* Reads market-level PW recrd and puts gross $ into ACCUTAB                     
                                                                                
FILLCKN  DS    0H                                                               
                                                                                
         CLI   MYIOFLAG,IOFADD     MUST HAVE PW RECD BEFORE HAVING              
         BE    FLKX                 LOCKINS                                     
                                                                                
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB              BUILD PW KEY                                 
                                                                                
         LA    R6,KEY                                                           
         USING PWRECD,R6                                                        
         XC    PWKSTA,PWKSTA       READ MKT-LEVEL PW RECORD                     
         OC    BSTA,BSTA                                                        
         BZ    FLK007X                                                          
         MVC   PWKSTA,BSTA          UNLESS DDS TESTING                          
*        TM    BSTA,X'F0'                                                       
*        BNO   *+10                                                             
         CLI   MYCANADA,C'C'        CANADIAN?                                   
         BE    FLK007X              YES                                         
         CLI   BSTA,X'E8'                                                       
         BL    *+10                                                             
         NC    PWKSTA,CBLSCMSK                                                  
FLK007X  EQU   *                                                                
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(PKYMKTL),KEYSAVE                                             
         BNE   FLKX                                                             
         OC    BSTA,BSTA           IF DDS TESTING, AND                          
         BZ    FLK10B                                                           
         MVC   TEMPBSTA,BSTA                                                    
*        TM    BSTA,X'F0'                                                       
*        BNO   *+10                                                             
         CLI   MYCANADA,C'C'        CANADIAN?                                   
         BE    FLK10A               YES                                         
         CLI   BSTA,X'E8'                                                       
         BL    *+10                                                             
         NC    TEMPBSTA,CBLSCMSK                                                
*&&DO                                                                           
         CLC   PWKSTA,BSTA          STATION-LEVEL PW RECD NOT FOUND             
*&&                                                                             
FLK10A   CLC   PWKSTA,TEMPBSTA      STATION-LEVEL PW RECD NOT FOUND             
         BNE   FLKX                 THEN EXIT                                   
         DROP  R6                                                               
*                                                                               
FLK10B   GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         MVI   ELCODE,PWDOLCDQ                                                  
                                                                                
         BRAS  RE,GETEL                                                         
         B     FLK20A                                                           
FLK20    BRAS  RE,NEXTEL                                                        
FLK20A   BNE   FLK30                                                            
                                                                                
         USING PWDOLEL,R6                                                       
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
FLK20B   TM    ATFLAG,ATFSTTLQ     IF SKED LINE, GET NEXT ELEM                  
         BO    FLK20                                                            
         TM    ATFLAG,ATFMTTLQ     IF MNTH LINE, GET NEXT WEEK                  
         BO    FLK20C                                                           
                                                                                
         CLC   PWDOLWK,ATWSTART    IF DATE IS NOT WITHIN                        
         BL    FLK20C                                                           
         CLC   PWDOLWK,ATWEND       RANGE, TRY NEXT WEEK'S DATE                 
         BH    FLK20C                                                           
                                                                                
         DS    0H                  FOUND WEEK ENTRY FOR PWDOLEL                 
         ICM   R1,15,PWDOLWG                                                    
         A     R1,ATWLCK                                                        
         ST    R1,ATWLCK            WIM LOCKIN                                  
         ICM   R1,15,PWDOLCG                                                    
         A     R1,ATCLCK                                                        
         ST    R1,ATCLCK            CLT LOCKIN                                  
         CLI   USETAX,C'Y'         TAX WANTED IN THE FIGURES?                   
         BE    FLK20                YES, DON'T TAKE TAXES OUT                   
                                                                                
         DS    0H                   NO, TAKE TAXES OUT                          
                                                                                
* ATWLCK w/o tax = PWDOLWG - PWDOLTAX,                                          
* ATCLCK w/o tax = PWDOLCG - PWDOLCTX.                                          
                                                                                
         ICM   R1,15,PWDOLTAX      TAX DOLLARS ON NET                           
         L     R0,ATWLCK           GET WIM LOCK                                 
         SR    R0,R1                LESS TAX ON WIM NET                         
         ST    R0,ATWLCK            GIVES WIM LOCK W/O TAX                      
                                                                                
         ICM   R1,15,PWDOLCTX      CLT TAX DOLLARS                              
         L     R0,ATCLCK           GET CLT LOCK                                 
         SR    R0,R1                LESS CLT TAX DOLLARS                        
         ST    R0,ATCLCK            GIVES CLT LOCK W/O TAX                      
                                                                                
         B     FLK20                                                            
*                                                                               
FLK20C   LA    R4,ACCUTABQ(R4)                                                  
         B     FLK20B                                                           
         DROP  R4,R6                                                            
*                                                                               
** OVERRIDING LOCKED DOLLAR AMOUNTS **                                          
*                                                                               
FLK30    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PWCLLCDQ                                                  
                                                                                
         BRAS  RE,GETEL                                                         
         B     FLK30B                                                           
FLK30A   BRAS  RE,NEXTEL                                                        
FLK30B   BNE   FLK40                                                            
                                                                                
         USING PWCLLEL,R6                                                       
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
FLK30C   TM    ATFLAG,ATFSTTLQ     IF SKED LINE, GET NEXT ELEM                  
         BO    FLK30A                                                           
         TM    ATFLAG,ATFMTTLQ     IF MNTH LINE, GET NEXT WEEK                  
         BO    FLK30D                                                           
                                                                                
         CLC   PWCLLWK,ATWSTART    IF DATE IS NOT WITHIN                        
         BL    FLK30D                                                           
         CLC   PWCLLWK,ATWEND       RANGE, TRY NEXT WEEK'S DATE                 
         BH    FLK30D                                                           
         ICM   R1,15,PWCLLAMT                                                   
         A     R1,ATCLCK                                                        
         ST    R1,ATCLCK           CLT LOCKIN OVERRIDE                          
         OI    ATFLAG2,ATF2CLOD     AND TURN FLAG ON IN ACCUTAB                 
         B     FLK30A                                                           
*                                                                               
FLK30D   LA    R4,ACCUTABQ(R4)                                                  
         B     FLK30C                                                           
         DROP  R4,R6                                                            
                                                                                
*                                                                               
FLK40    DS    0H                                                               
*                                                                               
FLKX     B     XIT_01                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--FDC#)'                        
*---------------------------- FILL IN DR/CR --------------------------*         
                                                                                
* Fills in the Adjusted DR/CR amounts into ACCUTAB                              
                                                                                
FILLDRCR DS    0H                                                               
         CLI   MYIOFLAG,IOFADD     MUST HAVE PW RECD BEFORE HAVING              
         BE    FDCX                 ADJ DR/CR AMOUNTS                           
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         L     R6,AMYC2REC                                                      
         MVI   ELCODE,PWDOLCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         BRAS  RE,GETEL                                                         
         BNE   FDCX                EXIT IF NO DOLLAR ELEMENT                    
         USING PWDOLEL,R6                                                       
*                                                                               
FDC10    DS    0H                  R4-->1ST WEEK OF A MONTH (ACCUTAB)           
         CLC   ATWSTART,PWDOLWK    LOOK FOR FIRST WEEK IN RECD                  
         BNE   FDC14                                                            
*                                                                               
         DS    0H                   FOUND IT!                                   
FDC12    LA    R4,ACCUTABQ(R4)      FAST FORWARD TO BILL ADJ LINE               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   FDC12                                                            
         MVC   ATDRCR,PWDOLBIL       AND MOVE ADJ BILLING INFO                  
         MVC   ATBILD,PWDOLBLD       INTO ACCUTAB                               
         LA    R4,ACCUTABQ(R4)      BUMP TO MONTH TOTAL LINE                    
         B     FDC20                                                            
                                                                                
FDC14    DS    0H                   NO 1ST WEEK FOR MONTH IN RECD               
         LA    R4,ACCUTABQ(R4)      FAST FORWARD TO MONTH TOTAL LINE            
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    FDC14                                                            
         TM    ATFLAG,ATFBILAJ                                                  
         BO    FDC14                                                            
*                                                                               
FDC20    BRAS  RE,NEXTEL           GET 1ST DOLLAR ELEMENT OF NEXT MONTH         
         BNE   FDCX                                                             
         CLC   ATWEND,PWDOLWK      ATWEND = END DATE OF LAST BDCST WK           
         BNL   FDC20                IN THE CURRENT BDCST MONTH                  
                                                                                
         LA    R4,ACCUTABQ(R4)     R4-->1ST WEEK OF NEXT MNTH (ACCUTAB)         
         TM    ATFLAG,ATFSTTLQ     STOP IF SKED LINE REACHED                    
         BZ    FDC10                                                            
FDC30    DS    0H                                                               
*                                                                               
FDCX     DS    0H                                                               
         B     XIT_01                                                           
         DROP  R4,R6                                                            
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--LTORG && CONSTANTS)'          
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
         SPACE 2                                                                
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR01--MISC STUFF)'                  
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
         PRINT OFF                                                              
*&&DO                                                                           
         GETEL R6,DATADISP,ELCODE                                               
*&&                                                                             
         PRINT ON                                                               
*                                                                               
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(4096-SUBR01L+1)                                              
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02)'                              
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
SUBR02Q  EQU   ((((*-T21759)/4096)+1)*4096)                                     
                                                                                
         ORG   T21759+SUBR02Q                                                   
SUBR02   NMOD1 0,**5902**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE,                     
         SH    R1,=Y(R01#)          SUBTRACT FOR SUB-RTN # 2,                   
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R02_00(R1)                                                       
                                                                                
PWBUY#   EQU   ((R02_01-*)/4+1)+R01#                                            
PWPW#    EQU   ((R02_02-*)/4+1)+R01#                                            
PWGOL#   EQU   ((R02_03-*)/4+1)+R01#                                            
PDS#     EQU   ((R02_04-*)/4+1)+R01#                                            
RUP#     EQU   ((R02_05-*)/4+1)+R01#                                            
SUM#     EQU   ((R02_06-*)/4+1)+R01#                                            
STI#     EQU   ((R02_07-*)/4+1)+R01#                                            
RTI#     EQU   ((R02_08-*)/4+1)+R01#                                            
RFG#     EQU   ((R02_09-*)/4+1)+R01#                                            
*&&DO                                                                           
CPRTF#   EQU   ((R02_10-*)/4+1)+R01#                                            
*&&                                                                             
CGL#     EQU   ((R02_11-*)/4+1)+R01#                                            
LMD#     EQU   ((R02_12-*)/4+1)+R01#                                            
UPT#     EQU   ((R02_13-*)/4+1)+R01#                                            
LKC#     EQU   ((R02_14-*)/4+1)+R01#                                            
DDC#     EQU   ((R02_15-*)/4+1)+R01#                                            
CAN#     EQU   ((R02_16-*)/4+1)+R01#                                            
*&&DO                                                                           
CEDC#    EQU   ((R02_17-*)/4+1)+R01#                                            
*&&                                                                             
FXR#     EQU   ((R02_18-*)/4+1)+R01#                                            
                                                                                
R02_00   DS    0H                                                               
R02_01   B     PWCBUY                                                           
R02_02   B     PWCPW                                                            
R02_03   B     PWCGOL                                                           
R02_04   B     PAIDSP                                                           
R02_05   B     ROUNDUP                                                          
R02_06   B     SUMUP                                                            
R02_07   B     SAVETIA                                                          
R02_08   B     RSTRTIA                                                          
R02_09   B     RESETFLG                                                         
*&&DO                                                                           
R02_10   B     CPRTFLD                                                          
*&&                                                                             
R02_11   B     CALCGOAL                                                         
R02_12   B     LOCMDFY                                                          
R02_13   B     UPWTAB                                                           
R02_14   B     LOCKCOST                                                         
R02_15   B     DODRCR                                                           
R02_16   B     CHKALFNM                                                         
*&&DO                                                                           
R02_17   B     CHKESTDT                                                         
*&&                                                                             
R02_18   B     FXRECD                                                           
R02#     EQU   ((*-R02_00)/4+1)+R01#                                            
R02_DIE  DC    H'0'                                                             
                                                                                
YES_02   SR    RC,RC                                                            
NO_02    LTR   RC,RC                                                            
XIT_02   XIT1                                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--PW CALCULATIONS)'             
*        At entry, TEMPAJB, TEMPACB, TEMPACG, TEMPTAX, & TEMPPW                 
*         has corresponding values into respective routines                     
                                                                                
*--------------------------- ADJUSTED BUYS ---------------------------*         
*                                                                               
PWCBUY   DS    0H                                                               
         XC    TEMPAJB,TEMPAJB     SET DEFAULT VALUED                           
         OC    TEMPACB,TEMPACB     IF NO ACTUAL BUY,                            
         BZ    XIT_02               PASS BACK ZERO ADJ BUY                      
*                                                                               
         XR    R0,R0                                                            
         L     R1,TEMPACB                                                       
         L     RE,TEMPPW                                                        
         MR    R0,RE                                                            
         MVC   FULL,=F'1000000'                                                 
         D     R0,FULL                                                          
         ST    R1,TEMPAJB                                                       
         C     R0,=F'500000'                                                    
         BL    PWCBUYX                                                          
         AHI   R1,1                                                             
         ST    R1,TEMPAJB                                                       
*                                                                               
PWCBUYX  B     XIT_02                                                           
                                                                                
*--------------------------- PW PERCENTAGE ---------------------------*         
*                                                                               
PWCPW    DS    0H                                                               
         MVC   TEMPPW,OC2PCT       SET DEFAULT VALUE                            
         OC    TEMPAJB,TEMPAJB     IF NO ADJUSTED BUYS,                         
         BZ    PWCPWX               THEN EXIT                                   
*                                                                               
         XR    R0,R0                                                            
         L     R1,TEMPAJB                                                       
         M     R0,=F'1000000'                                                   
         L     RE,TEMPACB                                                       
         DR    R0,RE                                                            
         ST    R1,TEMPPW                                                        
         SRL   RE,1                                                             
         CR    R0,RE               REMAINDER > HALF OF DIVISOR?                 
         BL    PWCPWX                                                           
         AHI   R1,1                YES, THEN ROUND UP                           
         ST    R1,TEMPPW                                                        
*                                                                               
PWCPWX   B     XIT_02                                                           
                                                                                
*-------------------------- ADJUSTED GOALS ---------------------------*         
*                                                                               
PWCGOL   DS    0H                                                               
         XC    TEMPAJG,TEMPAJG     SET DEFAULT VALUE                            
         OC    TEMPACG,TEMPACG     IF NO ACTUAL GOAL,                           
         BZ    XIT_02               PASS BACK ZERO ADJ GOAL                     
                                                                                
         L     R2,AIO3             USE 3RD I/O FOR PWBLOCK                      
         XC    0(PWBLKL,R2),0(R2)                                               
         USING PWBLKD,R2                                                        
         MVI   PWACT,PWGETGOL                                                   
         MVC   PWACTGOL,TEMPACG                                                 
         MVC   PWPCT,TEMPPW                                                     
         MVC   PWTAXRT,TEMPTAX                                                  
         CLI   USETAX,C'Y'                                                      
         BE    *+8                                                              
         OI    PWFLAG,PWFLAG_NOTAX                                              
                                                                                
         GOTO1 APWCALC,DMCB,(R2)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPAJG,PWVAL                                                    
         B     XIT_02                                                           
*                                                                               
**----------------------------- PWTRACE -----------------------------**         
*                                                                               
*        Writes the PW values we're interested in before and after              
*         every call to PWCALC                                                  
                                                                                
PWTRACE  NTR1                                                                   
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'ON'                                  
                                                                                
         MVI   WORK,24                                                          
         MVC   WORK+1(2),=C'PW'                                                 
         MVC   WORK+3(1),PWACT                                                  
         MVC   WORK+4(1),PWFLAG                                                 
         MVC   WORK+5(4),PWACTBUY                                               
         MVC   WORK+9(4),PWADJBUY                                               
         MVC   WORK+13(4),PWPCT                                                 
         MVC   WORK+17(4),PWTAX                                                 
         MVC   WORK+21(4),PWVAL                                                 
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',WORK                           
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'OFF'                                 
*&&                                                                             
         B     XIT_02                                                           
         DROP  R2                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--PDS#)'                        
*------------------------------ PAID SPOTS ---------------------------*         
                                                                                
* Turns on unpaid (ATF2UNPD) flags in ACCUTAB                                   
* At entry, R6-->buy record                                                     
                                                                                
PAIDSP   DS    0H                                                               
                                                                                
         LR    R3,R6                                                            
BUY      USING BUYREC,R3                                                        
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,(BDELEM-BUYREC)(R6)                                           
*                                                                               
PDS20    BAS   RE,NXTEL                                                         
         BNE   PDSX                                                             
*                                                                               
         USING REGELEM,R6                                                       
         MVC   DATE2,RDATE          NO PAID DATE==>SPOT IS UNPAID               
         L     RF,ALOCDATE                                                      
         BASR  RE,RF                                                            
                                                                                
         OR    R4,R4               A(ENTRY) PASSED BACK?                        
         BNZ   PDS22                YES                                         
         LA    R2,MYTEXT+1                                                      
         GOTO1 MSUNPK,DMCB,BUY.BUYMSTA,DUB,(R2)                                 
         MVI   9(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(X'82',DATE2),(5,11(R2)),0                           
         MVI   MYTEXT,19                                                        
         MVI   MYERRCD,SPDATQ      SET ERROR MSG CODE                           
         B     PDSX                                                             
                                                                                
*                                                                               
PDS22    DS    0H                                                               
         USING ACCUTABD,R4         R4-->WEEK CONTAINING DATE2                   
                                                                                
         TM    RSTATUS,X40         TEST IS SPOT HAS BEEN MINUSED                
         BO    PDS30                                                            
                                                                                
*                                                                               
         DS    0H                  SPOT HAS NOT BEEN MINUSED                    
         OC    RPAY,RPAY           CHECK FOR SPOTS PAID                         
         BNZ   PDS50                                                            
                                                                                
         DS    0H                  NO PAID DATE--CHECK FOR DEFERMENT            
         CLI   SPOTPROF+8,0         CHECK OUT-OF-WEEK ROTATOR                   
         BE    PDS24                 NONE==>NO DEFERMENTS WAS ALLOWED           
         ZIC   RF,RLEN                                                          
         AR    RF,R6                                                            
         CLI   0(RF),X'10'          TEST SPOT MATCHED                           
         BE    PDS24                 MATCHED==>NO DEFERMENTS DONE               
                                                                                
         GOTO1 DATCON,DMCB,(2,ATMEND),(0,WORK)                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 (RF),(R1),(0,WORK),(2,SDATE2)  START OF CALNDR MONTH             
         GOTO1 ADDAY,(R1),WORK,WORK+6,31                                        
         MVC   WORK+6+4(2),=C'01'                                               
         GOTO1 (RF),(R1),WORK+6,WORK,F'-1'                                      
         GOTO1 DATCON,(R1),(0,WORK),(2,EDATE2)  END OF CALNDR MONTH             
                                                                                
         CLC   RDATE,EDATE2         TEST SPOT AFTER MONTH                       
         BH    PDS24X                YES, IGNORE (AS IN PAY PROGRAM)            
         GOTO1 DATCON,DMCB,(2,RDATE),WORK                                       
         SR    R0,R0                                                            
         IC    R0,BUY.BDSEDAY                                                   
         SRDL  R0,4                 R0 = START DAY OF ROTATION                  
         SRL   R1,28                R1 =  END   "  "     "                      
         CR    R0,R1                SHOULD INDICATE OUT-OF-WEEK ROT             
         BNH   PDS24                                                            
         LA    R1,7(R1)                                                         
         SR    R1,R0                                                            
         LR    R0,R1                R0 = # OF DAYS DIFFERENCE                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,HALF)                                      
                                                                                
         DS    0H                   HALF = LATEST DATE SPOT COULD RUN           
         CLC   HALF,SDATE2          TEST BEFORE START                           
         BL    PDS24X                                                           
         CLC   HALF,EDATE2           OR AFTER END                               
         BH    PDS24X                                                           
         B     PDS24                                                            
                                                                                
PDS24    DS    0H                                                               
         OI    ATFLAG2,ATF2UNPD     NO PAID DATE==>SPOT IS UNPAID               
PDS24X   EQU   *                                                                
                                                                                
         B     PDS50                                                            
                                                                                
*                                                                               
PDS30    DS    0H                  SPOT HAS BEEN MINUSED                        
         OC    RPAY,RPAY           IS MINUSED SPOT PAID?                        
         BZ    PDS50                NOPE, EVERYTHING OKAY                       
                                                                                
         DS    0H                  MINUSED SPOT WAS PAID                        
         LR    R5,R6               SAVE A(CURRENT POOL ORIG BUY ELEM)           
         ZIC   R0,RLEN                                                          
         AR    R6,R0                                                            
         CLI   RCODE,X'0C'         LOOK AT POOL OTO ELEM                        
         BNE   PDS35                                                            
         TM    RSTATUS,X80          IT SHOULD BE THE MINUS SPOT                 
         BZ    PDS35                                                            
         OC    RPAY,RPAY           SEE IF PAID YET                              
         BNZ   PDS50                YES, EVERYTHING OKAY                        
         OI    ATFLAG2,ATF2UNPD     NO, SPOT IS UNPAID                          
                                                                                
PDS35    DS    0H                                                               
         LR    R6,R5               RESTORE A(POOL ORIG) INTO R6                 
         B     PDS50                                                            
*                                                                               
PDS50    DS    0H                                                               
         B     PDS20                                                            
         DROP  R4,R6                                                            
*                                                                               
PDSX     B     XIT_02                                                           
         DROP  BUY                                                              
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--RUP#)'                        
*------------------------------ ROUNDING -----------------------------*         
                                                                                
*  Rounds off to the nearest 100s                                               
*   At entry, R0 contains the number to be rounded                              
*   At exit,  FULL = the result                                                 
                                                                                
ROUNDUP  DS    0H                                                               
         AR    R0,R0               2N (N = THE NUMBER)                          
         SRDA  R0,32                                                            
         D     R0,=F'100'          N/50                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'            (N/50)+1                                     
         SRA   R1,1                (N/100)+0.5                                  
         ST    R1,FULL                                                          
         B     XIT_02                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--SUM#)'                        
*------------------------- TOTAL UP SCHEDULE -------------------------*         
                                                                                
*  BY THIS TIME, ACTUAL & ADJUSTED GOALS AND BUYS, CS2, TAX,                    
*   and lockins should be set in ACCUTAB already                                
                                                                                
SUMUP    DS    0H                                                               
         XC    MTOTALS(MTOTALQ),MTOTALS                                         
         XC    STOTALS(STOTALQ),STOTALS                                         
         XC    SKEDDRCR,SKEDDRCR                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         TM    ATFLAG,ATFSTTLQ      NO STATIONS!!??!!                           
         BZ    SUM10                                                            
         MVC   STOTALS(8),ATACGOAL  YES, THEN KEEP THE GOALS                    
*                                                                               
SUM10    DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    SUM30                                                            
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    SUM20                                                            
*                                                                               
** R4-->WEEK ENTRY IN ACCUTAB **                                                
*                                                                               
         LA    R0,ATACVALN         R0 = # OF ACCUM VALUES TO SUM UP             
         SR    R1,R1                                                            
SUM12    LA    R2,ATACVALS(R1)     R2-->ACCUMULATOR VALUE                       
         ICM   RE,15,0(R2)         RE=ACCUMULATOR VALUE                         
         L     RF,MTOTALS(R1)                                                   
         AR    RF,RE                                                            
         ST    RF,MTOTALS(R1)                                                   
         L     RF,STOTALS(R1)                                                   
         AR    RF,RE                                                            
         ST    RF,STOTALS(R1)                                                   
         LA    R1,L'ATACVALS(R1)                                                
         BCT   R0,SUM12                                                         
         B     SUM50                                                            
*                                                                               
** R4-->MONTH ENTRY IN ACCUTAB **                                               
*                                                                               
SUM20    DS    0H                  MONTH LINE REACHED                           
         TM    ATFLAG,ATFBILAJ     IS THIS A BILL ADJ LINE?                     
         BZ    SUM23                NOPE, SHOULD BE MONTH TOTAL ENTRY           
*                                                                               
*** BILLING ADJUSTMENT ENTRY ***                                                
*                                                                               
         MVC   ATACVALS(MTOTALQ),MTOTALS   TOTALS W/O DR/CR AMOUNT              
                                                                                
         CLI   SHOWBA,C'Y'         IF WE ARE TO SHOW BILLING ADJ,               
         BNE   SUM29                                                            
         TM    ATFLAG,ATFNOSPT            AND THERE ARE SPOTS IN MONTH,         
         BO    SUM29                                                            
         ICM   R0,15,ATDRCR               AND EITHER DR/CR <> NULLS             
         BNZ   SUM20A                                                           
         TM    ATFLAG2,ATF2UNPD            OR ALL SPOTS ARE PAID,               
         BO    SUM29                                                            
                                                                                
SUM20A   DS    0H                  THEN DISPLAY BILL ADJ AMOUNT                 
         CLM   R0,15,=X'80000000'  IF DR/CR VALUE IS ZERO,                      
         BNE   *+6                                                              
         SR    R0,R0                ADJUST FOR IT IN R0                         
         LR    R1,R0                                                            
         LR    RF,R0                                                            
         A     RF,SKEDDRCR         UPDATE SKED'S DR/CR AMOUNT                   
         ST    RF,SKEDDRCR                                                      
         A     R1,MTTLAJB          UPDATE CLCOST AMOUNT FOR MONTH               
         ST    R1,MTTLAJB                                                       
         A     R0,SKEDAJB          UPDATE CLCOST AMOUNT FOR SKED                
         ST    R0,SKEDAJB                                                       
         B     SUM29                                                            
*                                                                               
*** NON-BILLING ADJUSTMENT ENTRY ***                                            
*                                                                               
SUM23    DS    0H                  MONTH TOTALS LINE                            
         MVC   ATACVALS(MTOTALQ),MTOTALS   MONTH'S SUM INTO MONTH ENTRY         
                                                                                
         DS    0H                  CALCULATE CS2 FOR MONTH                      
         TM    ATFLAG,ATFNOSPT                                                  
         BO    SUM23A                                                           
         OC    ATACBUY,ATACBUY                                                  
         BZ    SUM23A                                                           
                                                                                
         MVC   TEMPACB,ATACBUY     USE PWCALC IF WIM$<>0                        
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#                                                     
         GOTO1 AGOSUB                                                           
         MVC   ATCS2,TEMPPW         AND UPDATE ACCUTAB                          
         B     SUM23B                                                           
                                                                                
SUM23A   DS    0H                  ACTUAL WIM$ = 0                              
         MVC   ATCS2,OC2PCT                                                     
         OC    ATAJBUY,ATAJBUY     IF CLCOST <> 0,                              
         BZ    SUM23B                                                           
         MVC   ATCS2,=X'80000000'   FORCE FLAG IN CS2                           
         B     SUM23B                                                           
                                                                                
SUM23B   DS    0H                                                               
         XC    MTOTALS(MTOTALQ),MTOTALS    RESET FOR NEXT MONTH                 
         B     SUM29                                                            
*                                                                               
SUM29    B     SUM50                                                            
*                                                                               
** R4-->SKED ENTRY IN ACCUTAB **                                                
*                                                                               
SUM30    DS    0H                                                               
         MVC   ATDRCR,SKEDDRCR     MOVE IN SKED TOTAL DR/CR                     
         MVC   ATACVALS(STOTALQ),STOTALS   GET SUMS TO SKED LINE                
         ICM   R0,15,ATAJBUY                                                    
         S     R0,SKEDDRCR                                                      
         STCM  R0,15,ATAJBUY                W/O DR/CR TOTAL                     
         LA    R4,ACCUTABQ(R4)      AND BUMP PAST BILL ADJ ENTRY                
                                                                                
         MVC   ATACVALS(STOTALQ),STOTALS   GET SUMS TO SKED ENTRY               
         TM    ATFLAG,ATFNOSPT                                                  
         BO    SUM30A                                                           
         OC    ATACBUY,ATACBUY                                                  
         BZ    SUM30A                                                           
                                                                                
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#           CALC CS2                                  
         GOTO1 AGOSUB                                                           
         MVC   ATCS2,TEMPPW           AND UPDATE ACCUTAB                        
         B     SUM32                                                            
                                                                                
SUM30A   DS    0H                  ACTUAL WIM$ = 0                              
         MVC   ATCS2,OC2PCT                                                     
         OC    ATAJBUY,ATAJBUY     IF CLCOST <> 0,                              
         BZ    SUM32                                                            
         MVC   ATCS2,=X'80000000'   FORCE SPECIAL FLAG IN CS2                   
         B     SUM32                                                            
                                                                                
SUM32    DS    0H                  ACCUTAB ALL SUMMED UP                        
         B     SUM60                                                            
*                                                                               
** BUMP TO NEXT ACCUTAB ENTRY **                                                
*                                                                               
SUM50    LA    R4,ACCUTABQ(R4)                                                  
         B     SUM10                                                            
                                                                                
                                                                                
SUM60    DS    0H                  PROCESS SKED GOALS                           
         TM    MISCFLG1,MF1GOL0Q   IF NO GOALS FROM FILE,                       
         BZ    SUM60A                                                           
                                                                                
         TM    MISCFLG3,MF3UGOAL    AND GOAL IS USER-INPUTTED,                  
         BZ    SUM60C                                                           
         MVC   ATACGOAL,SKEDCGTX    USE MANUALLY INPUTTED GOAL                  
         CLI   USETAX,C'Y'                                                      
         BE    *+10                                                             
         MVC   ATACGOAL,SKEDCGL                                                 
SUM60C   EQU   *                                                                
                                                                                
         MVC   TEMPPW,OC2PCT       SET UP TO                                    
         MVC   TEMPACG,ATACGOAL                                                 
         LH    R0,GLTXRATE                                                      
         STCM  R0,15,TEMPTAX        CALCULATE ADJUSTED GOAL,                    
         MVI   GOSUBN,PWGOL#                                                    
         GOTO1 AGOSUB                                                           
         MVC   ATAJGOAL,TEMPAJG                                                 
         B     SUM60T                                                           
                                                                                
SUM60A   DS    0H                                                               
         NI    MISCFLG3,XFF-MF3UGOAL USER COULD NOT INPUT GOAL AMOUNT           
         NI    MISCFLG2,XFF-MF2GOALC                                            
                                                                                
SUM60T   DS    0H                                                               
         LA    RF,SKEDCGTX                                                      
         CLI   USETAX,C'Y'                                                      
         BE    *+8                                                              
         LA    RF,SKEDCGL                                                       
                                                                                
         CLC   ATACGOAL,0(RF)      IF GOAL/REC GOAL <> PW/REC GOAL,             
         BE    SUM60X                                                           
         OI    MISCFLG2,MF2GOALC                                                
         MVC   0(4,RF),ATACGOAL     GET SUM OF GOALS FROM FILE                  
SUM60X   DS    0H                                                               
*                                                                               
SUM65    DS    0H                                                               
         MVI   GOSUBN,CGLNTX#      DERIVE GOAL W/O TAX OR                       
         CLI   USETAX,C'Y'                                                      
         BE    *+8                                                              
         MVI   GOSUBN,CGLTX#        GOAL+TAX                                    
         GOTO1 AGOSUB                                                           
         B     SUMUPX                                                           
*                                                                               
SUMUPX   B     XIT_02                                                           
         DROP  R4                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--STI# && RTI#)'                
*--------------------------- SAVE TIA TABLES -------------------------*         
                                                                                
* save TIA tables into TEMPSTR                                                  
                                                                                
SAVETIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATIA,0                       
*                                                                               
         DS    0H                  SAVE AWAY STTN ACCUM TABLE                   
         TM    TSARBLK+(TSINDS-TSARD),TSIINIOK                                  
         BZ    STI015X                                                          
                                                                                
         MVI   GOSUBN,TSR_SAV#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
STI015X  EQU   *                                                                
*                                                                               
         B     XIT_02                                                           
         SPACE 2                                                                
*-------------------------- RESTORE TIA TABLES -----------------------*         
                                                                                
* restore TIA tables from TEMPSTR                                               
                                                                                
RSTRTIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ        3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATIA,0                      
*                                                                               
         DS    0H                  RESTORE STTN ACCUM TABLE                     
         TM    TSARBLK+(TSINDS-TSARD),TSIINIOK                                  
         BZ    RTI015X                                                          
                                                                                
         MVI   GOSUBN,TSR_RES#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
RTI015X  EQU   *                                                                
*                                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--RFG#)'                        
*------------------------- RESET FLAGS IN TABLE ----------------------*         
RESETFLG DS    0H                                                               
         MVI   HALF,ATFRSET2       USE THESE FLAGS                              
         MVI   HALF+1,ATF2RST2                                                  
         TM    MISCFLG1,MF1ERRQ    IF NO PREV ERROR,                            
         BO    RFG05                                                            
         MVI   HALF,ATFRSET1        THEN USE THESE                              
         MVI   HALF+1,ATF2RST1                                                  
                                                                                
RFG05    DS    0H                                                               
         L     R4,AACCUTAB         RESET FLAGS IN ACCUTAB                       
         USING ACCUTABD,R4                                                      
         LR    R0,R4               R0 = EOT                                     
         AHI   R0,ACCUTABX-ACCUTAB-1                                            
RFG10    CLC   =X'FFFFFF',0(R4)                                                 
         BE    RFG20                                                            
         NC    ATFLAG,HALF         LEAVE THESE BITS ALONE                       
         NC    ATFLAG2,HALF+1                                                   
         LA    R4,ACCUTABQ(R4)                                                  
         CR    R4,R0                                                            
         BL    RFG10               DO FOR ENTIRE TABLE                          
         DROP  R4                                                               
*                                                                               
RFG20    DS    0H                  RESET FLAGS IN STTN ACCUM TABLE              
         L     R4,TSARBLK+(TSAREC-TSARD)                                        
         LA    R4,0(R4)                                                         
         USING STACRECD,R4                                                      
                                                                                
         XC    STACRECD(STACRECL),STACRECD                                      
         MVI   GOSUBN,TSR_RDH#                                                  
RFG25    GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     EOF IN STTN ACCUM TABLE?                     
         BNZ   RFG29                YEP                                         
         MVI   STACFLG,0                                                        
         MVI   GOSUBN,TSR_PUT#     PUT STTN ACCUM RECD BACK                     
         GOTO1 (RF)                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   GOSUBN,TSR_NXT#     GET NEXT STTN ACCUM RECD                     
         B     RFG25                                                            
         DROP  R4                                                               
RFG29    EQU   *                                                                
*                                                                               
RFGX     B     XIT_02                                                           
                                                                                
ATFRSET1 EQU   ATFMTTLQ+ATFSTTLQ+ATFBILAJ+ATFESTBQ+ATFFNLBQ+ATFNOSPT            
ATFRSET2 EQU   ATFRSET1+ATFAJBQ+ATFPWQ                                          
ATF2RST1 EQU   ATF2UNPD+ATF2DPLY+ATF2CCOD+ATF2CLOD+ATF2A1PS+ATF2OOWP            
ATF2RST2 EQU   ATF2RST1+ATF2CHDC                                                
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--CGL#)'                        
*--------------------------- CALCULATE GOALS -------------------------*         
CALCGOAL DS    0H                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
CGL10    CLC   =X'FFFFFF',0(R4)                                                 
         BE    XIT_02                                                           
         MVC   TEMPACG,ATACGOAL                                                 
         MVC   TEMPPW,OC2PCT                                                    
         LH    R0,GLTXRATE                                                      
         STCM  R0,15,TEMPTAX                                                    
         MVI   GOSUBN,PWGOL#                                                    
         GOTO1 AGOSUB                                                           
         MVC   ATAJGOAL,TEMPAJG                                                 
                                                                                
         LA    R4,ACCUTABQ(R4)                                                  
         B     CGL10                                                            
         DROP  R4                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--LMD#)'                        
*------------------------ LOCATE MODIFIED FIELD ----------------------*         
                                                                                
* Locates first data field modified                                             
* At entry, FULL is set up pointing inside ACCUTAB                              
* At exit,  FULL-->1st modified field                                           
                                                                                
LOCMDFY  DS    0H                                                               
         LA    R2,C2MGLTXH          TAX FIELD IS FIRST DATA FIELD               
         TM    4(R2),X80                                                        
         BO    LMDX                                                             
                                                                                
         LA    R2,C2MSTATH         LOCATE FIELD MODIFIED IN ACCUTAB             
         L     R4,FULL              SET UP R4 POINTING IN ACCUTAB               
         USING ACCUTABD,R4                                                      
LMD10    CLC   =X'FFFFFF',0(R4)    IF NOTHING CHANGED IN SKED,                  
         BE    LMD30                CHECK OTHER DATA FIELDS                     
LMD15    TM    ATFLAG,ATFAJBQ+ATFPWQ                                            
         BZ    LMD20                                                            
         LA    RF,MSLCCOSH-MSLDSECT   ASSUME THIS WAS MODIFIED                  
         TM    ATFLAG,ATFAJBQ                                                   
         BO    LMD25                                                            
         LA    RF,MSLCOS2H-MSLDSECT    IT WAS THIS MODIFIED                     
         B     LMD25                                                            
LMD20    TM    ATFLAG,ATFMTTLQ     IF WE REACH THE                              
         BZ    LMD20B                                                           
         CLC   ATMSTART,MNTHMRKE    END MONTH ON DISPLAY,                       
         BL    LMD20B                                                           
         BH    LMDDIE                                                           
         LA    R2,C2MSTTLH          NO WEEKS NOR MONTHS WERE CHANGED            
LMD20A   LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFSTTLQ      CHECK THE SKED TOTALS LINE                  
         BZ    LMD20A                                                           
         B     LMD10                                                            
LMD20B   LA    R2,MSLLENQ(R2)                                                   
         LA    R4,ACCUTABQ(R4)                                                  
         B     LMD10                                                            
LMD25    AR    R2,RF                                                            
         B     LMDX                                                             
*                                                                               
LMD30    LA    R2,C2MTGRPH          TRY TOTAL GRP FIELD,                        
         TM    4(R2),X80                                                        
         BO    LMDX                                                             
         LA    R2,C2MSACGH          AND SKED ACTUAL GOALS FIELD                 
         TM    4(R2),X80                                                        
         BO    LMDX                                                             
LMDDIE   DC    H'0'                  IT BETTER BE LOCATED                       
*                                                                               
LMDX     ST    R2,FULL                                                          
         B     XIT_02                                                           
         DROP  R4                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--UPT#)'                        
*-------------------------- UPDATE CS2 TABLE ------------------------*          
                                                                                
* TRANSFERS CS2S FROM ACCUTAB AT END OF EVERY "GOOD" MODIFICATION *             
                                                                                
UPWTAB   DS    0H                                                               
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         LA    R6,PWTAB                                                         
                                                                                
UPT10    TM    ATFLAG,ATFSTTLQ     IF SCHED TOTALS ENTRY                        
         BZ    UPT20                NO                                          
         TM    ATFLAG,ATFBILAJ     BILL ADJ ENTRY                               
         BO    UPT20                THEN SKIP                                   
         MVC   4(4,R6),ATCS2       UPDATE 1ST WEEK IN PW TABLE ONLY             
         B     UPTX                                                             
                                                                                
UPT20    LA    R4,ACCUTABQ(R4)     BUMP ACCUTAB POINTER                         
         B     UPT10                                                            
*                                                                               
UPTX     B     XIT_02                                                           
         DROP  R4                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--LKC#)'                        
*------------------------------ LOCK COSTS ---------------------------*         
                                                                                
* Routine is executed only when PWDOLEL elements are updated                    
                                                                                
LOCKCOST DS    0H                                                               
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
LKC20    CLC   =X'FFFFFF',0(R4)                                                 
         BE    LKCX                                                             
         MVC   ATCLCK(8),ATAJBUY    MOVE COSTS TO LOCKS                         
                                                                                
         NI    ATFLAG2,XFF-ATF2CLOD SET CLLOCK OVERRIDE FLAG                    
         TM    ATFLAG2,ATF2CCOD                                                 
         BZ    *+8                                                              
         OI    ATFLAG2,ATF2CLOD                                                 
                                                                                
         LA    R4,ACCUTABQ(R4)                                                  
         B     LKC20                                                            
         DROP  R4                                                               
*                                                                               
LKCX     B     XIT_02                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--DDC#)'                        
*------------------------------- DR/CR $ -----------------------------*         
                                                                                
* Updates weekly dollar elements (PWDOLEL) for DR/CR$.  We only                 
*  want the bill amount and date to be in 1st existing week for                 
*  each month                                                                   
* At entry,                                                                     
*   R6-->PW record (mkt- or station-level)                                      
                                                                                
DODRCR   DS    0H                                                               
         ST    R6,FULL             HOLD ONTO A(RECORD)                          
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         MVI   ELCDLO,PWDOLCDQ                                                  
         MVI   ELCDHI,PWDOLCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
                                                                                
DDC10    DS    0H                                                               
         L     R6,FULL             POINT R6 TO START OF RECORD                  
         BRAS  RE,GTEL             FIND ELEM                                    
         BNE   DDC30                CAN'T FIND IT, SO ADD IT                    
                                                                                
         USING PWDOLEL,R6                                                       
DDC22    AHI   R4,ACCUTABQ          YES, FAST FOWARD TO BILL ADJ LINE           
         TM    ATFLAG,ATFSTTLQ                                                  
         BNO   DDC22                                                            
         MVC   PWDOLBIL,ATDRCR     MOVE IN $ AMOUNT                             
         MVC   PWDOLBLD,ATBILD      AND BILL DATE                               
         B     DDC40               GET NEXT BILL ADJ ENTRY                      
*                                                                               
DDC30    DS    0H                  ADD NEW ELEMENT TO RECORD                    
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   PWDOLCD,PWDOLCDQ     ELEM CODE                                   
         MVI   PWDOLLEN,PWDOLLNQ    ELEM LENGTH                                 
*                                                                               
DDC32    AHI   R4,ACCUTABQ          FAST FOWARD TO BILL ADJ LINE                
         TM    ATFLAG,ATFSTTLQ                                                  
         BNO   DDC32                                                            
                                                                                
         MVC   PWDOLBIL,ATDRCR      $ AMOUNT                                    
         MVC   PWDOLBLD,ATBILD      BILL DATE                                   
         L     R0,FULL             R0-->PW RECORD                               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R0),(R6),0                             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DDC40    DS    0H                  BUMP TO 1ST WEEK OF NEXT MONTH               
         B     XIT_02                                                           
                                                                                
         DROP  R4,R6                                                            
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--CAN#)'                        
*-------------------------- CHECK ALPHANUMERIC -----------------------*         
                                                                                
* Checks TWA fields for valid alphanumeric input.                               
* At entry: R2-->TWA field                                                      
* At exit : CC=EQ if input is valid alphanumeric                                
*           CC=NEQ if it isn't                                                  
                                                                                
CHKALFNM DS    0H                                                               
         ZIC   R0,5(R2)            R0=L(INPUT)                                  
         LA    R3,8(R2)            R3-->INPUT DATA                              
CAN20    LA    RF,ALPHANUM         RF-->TABLE OF VALID ALPHANUMERICS            
CAN20A   CLI   0(RF),C'\'          IF AT END OF THIS TABLE,                     
         BE    CANXN                THEN INPUT NOT ALPHANUMERIC                 
         CLC   0(1,R3),0(RF)       IF MATCH FOUND                               
         BE    CAN20B               VALIDATE NEXT CHAR OF INPUT                 
         LA    RF,1(RF)             ELSE, KEEP LOOKING FOR MATCH                
         B     CAN20A                                                           
CAN20B   LA    R3,1(R3)            R3-->NEXT CHAR OF INPUT                      
         BCT   R0,CAN20                                                         
         B     CANXY                                                            
*                                                                               
CANXY    B     YES_02                                                           
CANXN    B     NO_02                                                            
         PRINT OFF                                                              
*&&DO                                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--CEDC#)'                       
*---------------- CHECK ESTIMATE HEADER'S DATE CHANGED ---------------*         
                                                                                
* Routine checks the C2STEL elements against the broadcast periods to           
*  see if the dates in the estimate header had changed since the C2STEL         
*  elements were last written to the file.                                      
* At entry,                                                                     
*   AIO = A(mkt-level PW record)                                                
                                                                                
CHKESTDT DS    0H                                                               
                                                                                
*                                                                               
** CHECK FIRST WEEK OF SCHEDULE **                                              
*                                                                               
         DS    0H                                                               
         LA    R3,BRDWKTAB         R3-->WEEK DATES OF SCHEDULE                  
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    R3,BRDMTHTB         USE PERIOD DATES IF ONE PERIOD               
         USING BCSTTABD,R3                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCDLO,C2STCODQ                                                  
         MVI   ELCDHI,C2STCODQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         BAS   RE,GTEL                                                          
         BNE   CEDCXN                                                           
         USING C2STEL,R6                                                        
                                                                                
*                                                                               
         CLC   C2STSTAP,BCSSTART                                                
         BNE   CEDCXN                                                           
                                                                                
*                                                                               
** CHECK FINAL WEEK OF SCHEDULE **                                              
*                                                                               
         DS    0H                                                               
         CLI   BCSTTABQ(R3),XFF                                                 
         BE    *+12                                                             
         LA    R3,BCSTTABQ(R3)                                                  
         B     *-12                                                             
                                                                                
         DS    0H                  R3-->FINAL WEEK OF SCHEDULE                  
         ST    R6,FULL             HOLD ONTO (ADDR OF C2STEL)                   
         BAS   RE,NXTEL                                                         
         BE    *-8                                                              
         L     R6,FULL             R6-->A(LAST C2STEL IN RECORD)                
*                                                                               
         CLC   C2STSTAP,BCSSTART                                                
         BNE   CEDCXN                                                           
                                                                                
*                                                                               
         B     CEDCXY                                                           
         DROP  R3,R6                                                            
                                                                                
*                                                                               
CEDCXN   DS    0H                                                               
         B     NO_02                                                            
                                                                                
CEDCXY   DS    0H                                                               
         B     YES_02                                                           
*&&                                                                             
         PRINT ON                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--FXR#)'                        
*--------------------------- FIX PW RECORDS --------------------------*         
                                                                                
* Routine to fix broadcast dates in PW records.  The only way this rtn          
*  can be called is if the dates in the PW records are out of sync w/           
*  the dates in the estimate header (such as dates in est hdr changing)         
*  (or if an estimate is changed to an E-estimate)                              
                                                                                
FXRECD   DS    0H                                                               
*&&DO                                                                           
* THIS CODE TO TRAP AN UGLY LOOP                                                
         CLC   KEY(13),FXKEY                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   FXKEY,KEY                                                        
         B     FXR000                                                           
         DC    CL5'FXKEY'                                                       
FXKEY    DC    XL13'00'                                                         
*                                                                               
FXR000   DS    0H                                                               
*&&                                                                             
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         OI    GENSTAT1,RDUPAPPL   ROUTINE NEED TO UPDATE FILE                  
                                                                                
*                                                                               
** READ AND GET RECORD **                                                       
*                                                                               
         DS    0H                                                               
         MVI   RDUPDATE,C'Y'       READING FILE FOR UPDATE                      
         GOTO1 HIGH                                                             
         B     FXR024                                                           
*                                                                               
FXR022   DS    0H                                                               
         MVI   RDUPDATE,C'Y'       READING FILE FOR UPDATE                      
         GOTO1 SEQ                                                              
*                                                                               
FXR024   DS    0H                                                               
         CLC   KEY(PKYMKTL),KEYSAVE                                             
         BNE   FXRX                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'       READING FILE FOR UPDATE                      
         GOTO1 GETREC                                                           
                                                                                
*                                                                               
** BODY OF THE RECORD FIX **                                                    
*                                                                               
         DS    0H                                                               
         L     R6,AIO                                                           
         LA    R6,(PWEL-PWRECD)(R6)                                             
                                                                                
*                                                                               
FXR052   DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    FXR099                                                           
                                                                                
*                                                                               
         L     RE,AELDATTB                                                      
FXR062   DS    0H                                                               
         CLI   0(RE),EOT           AT END OF TABLE?                             
         BE    FXR090               YES, BUMP TO NEXT ELEMENT                   
         CLC   0(1,R6),0(RE)       SHOULD WE PROCESS THIS ELEMENT?              
         BE    *+12                 YES                                         
         LA    RE,L'ELDATTAB(RE)                                                
         B     FXR062                                                           
*                                                                               
         ZIC   RF,1(RE)                                                         
         AR    RF,R6               RF-->(COMPRESSED) DATE FIELD IN ELEM         
*                                                                               
         LA    RE,BRDWKTAB                                                      
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    RE,BRDMTHTB                                                      
         USING BCSTTABD,RE                                                      
                                                                                
FXR074   DS    0H                  FIND MATCHING WEEK FOR DATE IN ELEM          
         CLI   0(RE),XFF                                                        
         BE    FXR074X                                                          
         CLC   BCSSTART,0(RF)                                                   
         BH    FXR074D                                                          
         CLC   BCSEND,0(RF)                                                     
         BL    FXR074D                                                          
         MVC   0(2,RF),BCSSTART                                                 
         B     FXR074X                                                          
                                                                                
FXR074D  DS    0H                                                               
         LA    RE,BCSTTABQ(RE)                                                  
         B     FXR074                                                           
FXR074X  EQU   *                                                                
         DROP  RE                                                               
                                                                                
*                                                                               
FXR090   DS    0H                  BUMP TO NEXT ELEMENT                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FXR052                                                           
FXR099   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                 MAKE SURE WE HAVE C2STEL FOR ALL WKS          
         L     RF,AIO                                                           
         USING PWRECD,RF                                                        
         OC    PWKSTA,PWKSTA       APPLICABLE TO MKT-LEVEL RECDS ONLY           
         BNZ   FXR117                                                           
         DROP  RF                                                               
*                                                                               
         DS    0H                  BUILD SKELETON C2STEL ELEMENT                
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING C2STEL,R6                                                        
         MVI   C2STCD,C2STCODQ                                                  
         MVI   C2STLEN,C2STLENQ                                                 
*                                                                               
         DS    0H                  DRIVE OFF OF BROADCAST TABLE                 
         LA    R3,BRDWKTAB                                                      
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    R3,BRDMTHTB                                                      
         USING BCSTTABD,R3                                                      
*                                                                               
FXR113   DS    0H                                                               
         CLI   0(R3),XFF                                                        
         BE    FXR117                                                           
                                                                                
         MVC   C2STSTAP,BCSSTART                                                
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('C2STCODQ',AIO),              +        
               (L'C2STSTAP,C2STSTAP),0                                          
         CLI   DMCB+12,0                                                        
         BE    FXR115                                                           
                                                                                
******** MVC   C2STPCT,OC2PCT                                                   
         MVC   C2STFCTR,OC2PCT                                                  
*                                                                               
         CLC   =X'40404040',C2STFCTR                                            
         BE    *+14                                                             
         OC    C2STFCTR,C2STFCTR                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,C2STEL,0                            
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FXR115   DS    0H                  BUMP TO NEXT WEEK IN SCHEDULE                
         LA    R3,BCSTTABQ(R3)                                                  
         B     FXR113                                                           
         EJECT                                                                  
* MAKE SURE NO ELEMENTS OUTSIDE OF ESTIMATE DATES                               
         SPACE 1                                                                
FXR117   EQU   *                                                                
         LA    R3,BRDWKTAB                                                      
         CLI   BCSTTABQ(R3),X'FF'  TEST EOT                                     
         BE    *+12                                                             
         LA    R3,BCSTTABQ(R3)                                                  
         B     *-12                                                             
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
*                                                                               
FXR119   CLI   0(R6),0                                                          
         BE    FXR127                                                           
         CLI   0(R6),5                                                          
         BE    FXR123                                                           
         CLI   0(R6),6                                                          
         BE    FXR123                                                           
         CLI   0(R6),7                                                          
         BE    FXR123                                                           
         CLI   0(R6),X'15'                                                      
         BE    FXR123                                                           
         CLI   0(R6),X'16'                                                      
         BE    FXR123                                                           
         CLI   0(R6),X'86'                                                      
         BE    FXR123                                                           
         CLI   0(R6),X'96'                                                      
         BE    FXR123                                                           
* NEXT ELEMENT                                                                  
FXR121   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FXR119                                                           
*                                                                               
FXR123   CLC   2(2,R6),BRDWKTAB    TEST BEFORE START DATE                       
         BL    FXR125              GET RID OF ELEMENT                           
         CLC   2(2,R6),0(R3)       TEST PAST END DATE                           
         BH    FXR125              GET RID OF ELEMENT                           
         B     FXR121                                                           
         DROP  R3,R6                                                            
*                                                                               
FXR125   GOTO1 RECUP,DMCB,(0,AIO),(R6)                                          
         B     FXR119              R6 NOW POINTS TO NEXT ELEMENT                
*                                                                               
FXR127   EQU   *                                                                
                                                                                
*                                                                               
** MERGE DOLLAR ELEMENTS WITH SAME DATES **                                     
*                                                                               
         L     R6,AIO                                                           
         LA    R6,(PWEL-PWRECD)(R6)                                             
                                                                                
FXR132   DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    FXR159                                                           
*                                                                               
         L     RE,AELDATTB                                                      
FXR134   DS    0H                  MATCH ELEMENT CODE TO TABLE                  
         CLI   0(RE),EOT                                                        
         BE    FXR155                                                           
         CLC   0(1,R6),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'ELDATTAB(RE)                                                
         B     FXR134                                                           
                                                                                
         MVC   LNCNTDWN,2(RE)      LNCNTDWN = # OF BUCKETS                      
         ZIC   R0,1(RE)                                                         
         STH   R0,FULL             FULL(2) = DISPL TO DATE                      
         IC    R0,3(RE)                                                         
         STH   R0,FULL+2           FULL+2(2) = DISPL TO 1ST BUCKET              
                                                                                
*                                                                               
         DS    0H                  LOOK FOR SAME ELCODE W/ SAME DATE            
         ZIC   R3,1(R6)                                                         
         AR    R3,R6               START W/ ELEM AFTER R6                       
*                                                                               
FXR138   DS    0H                                                               
         CLI   0(R3),0             IF END OF RECORD,                            
         BE    FXR155               BUMP R6 TO NEXT ELEMENT                     
         CLC   0(1,R3),0(R6)       SAME ELEMENT CODE?                           
         BNE   FXR138D                                                          
         LH    RE,FULL                                                          
         AR    RE,R6               RE-->(COMPRESSED) DATE FIELD                 
         LH    RF,FULL                                                          
         AR    RF,R3               RF-->(COMPRESSED) DATE FIELD                 
         CLC   0(2,RE),0(RF)       DATES MATCH?                                 
         BNE   FXR138D                                                          
         B     FXR138X                                                          
*                                                                               
FXR138D  DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FXR138                                                           
FXR138X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CONSOLIDATE ELEMENTS                         
         MVC   HALF,FULL+2          HALF = DISPL TO 1ST BUCKET                  
         ZICM  R2,LNCNTDWN,(1)      R2 = NUMBER OF BUCKETS                      
         BZ    FXR149                NO BUCKETS==>NOTHING TO MERGE              
*                                                                               
FXR142   DS    0H                                                               
         LH    RE,HALF                                                          
         AR    RE,R6               RE-->1ST BUCKET OF DEST. ELEM                
         ICM   R0,15,0(RE)                                                      
         LH    RF,HALF                                                          
         AR    RF,R3               RF-->1ST BUCKET OF SOURCE ELEM               
         ICM   R1,15,0(RF)                                                      
                                                                                
         AR    R0,R1                                                            
         STCM  R0,15,0(RE)                                                      
                                                                                
         LA    R1,4                                                             
         AH    R1,HALF                                                          
         STH   R1,HALF             HALF = DISPL TO NEXT BUCKET                  
         BCT   R2,FXR142                                                        
                                                                                
*                                                                               
         DS    0H                  MERGE NON-BUCKET ITEMS                       
         CLI   0(R6),PWDOLCDQ                                                   
         BE    FXR146                                                           
         CLI   0(R6),PWBAKDOL                                                   
         BE    FXR146                                                           
         CLI   0(R6),PWCURCDQ                                                   
         BE    FXR148                                                           
         B     FXR149                                                           
*                                                                               
FXR146   DS    0H                  LOCKED DOLLAR ELEMENT                        
         USING PWDOLEL,R6                                                       
         MVI   BYTE,C'N'                                                        
         ICM   R0,15,PWDOLBIL                                                   
         C     R0,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R0,R0                                                            
         ICM   R1,15,(PWDOLBIL-PWDOLEL)(R3)                                     
         C     R1,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R1,R1                                                            
         AR    R0,R1                                                            
         LTR   R0,R0                                                            
         BNZ   *+16                                                             
         CLI   BYTE,C'Y'                                                        
         BNE   *+8                                                              
         ICM   R0,15,=X'80000000'                                               
         STCM  R0,15,PWDOLBIL       BILL OVERRIDE DOLLARS                       
                                                                                
         CLC   PWDOLBLD,(PWDOLBLD-PWDOLEL)(R3)                                  
         BH    *+10                                                             
         MVC   PWDOLBLD,(PWDOLBLD-PWDOLEL)(R3)                                  
                                                                                
         B     FXR149                                                           
         DROP  R6                                                               
*                                                                               
FXR148   DS    0H                  CURRENT DOLLAR ELEMENT                       
         USING PWCUREL,R6                                                       
         MVI   BYTE,C'N'                                                        
         ICM   R0,15,PWCURBIL                                                   
         C     R0,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R0,R0                                                            
         ICM   R1,15,(PWCURBIL-PWCUREL)(R3)                                     
         C     R1,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R1,R1                                                            
         AR    R0,R1                                                            
         LTR   R0,R0                                                            
         BNZ   *+16                                                             
         CLI   BYTE,C'Y'                                                        
         BNE   *+8                                                              
         ICM   R0,15,=X'80000000'                                               
         STCM  R0,15,PWCURBIL       BILL OVERRIDE DOLLARS                       
                                                                                
         CLC   PWCURBLD,(PWCURBLD-PWCUREL)(R3)                                  
         BH    *+10                                                             
         MVC   PWCURBLD,(PWCURBLD-PWCUREL)(R3)                                  
                                                                                
         B     FXR149                                                           
         DROP  R6                                                               
FXR149   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  DELETE SOURCE ELEMENT                        
         GOTO1 RECUP,DMCB,(0,AIO),(R3)   R3 NOW POINTS TO NEXT ELEMENT          
         B     FXR138                                                           
                                                                                
*                                                                               
FXR155   DS    0H                  BUMP R6 TO NEXT ELEMENT                      
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FXR132                                                           
FXR159   EQU   *                                                                
         EJECT                                                                  
*                                                                               
** WRITE RECORD BACK TO FILE **                                                 
*                                                                               
         DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         L     RF,AIO                                                           
         USING PWRECD,RF                                                        
         OC    PWKSTA,PWKSTA       IF MARKET-LEVEL PW RECORD,                   
         BNZ   FXRPUTX                                                          
         DROP  RF                                                               
                                                                                
         MVI   GOSUBN,PPR#          COPY RECORD INTO MY PW RECD AREA            
         GOTO1 AGOSUB                                                           
FXRPUTX  EQU   *                                                                
*                                                                               
         B     FXR022                                                           
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
FXRX     DS    0H                                                               
         NI    GENSTAT1,XFF-RDUPAPPL     LET GENCON CONTROL READS-FOR-          
         MVI   RDUPDATE,C'N'              UPDATE NOW                            
*                                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--LTORG && CONSTANTS)'          
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\'                         
                                                                                
                                                                                
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR02--MISC STUFF)'                  
*--------------------- SUBR02 MISCELLANEOUS STUFF --------------------*         
                                                                                
GTEL     AH    R6,DATADISP                                                      
FRSTEL   CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                  RETURN CC NOT EQUAL                          
         CLI   ELCDLO,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLI   ELCDHI,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLC   ELCDLO,ELCDHI                                                    
         BHR   RE                  RETURN CC NOT EQUAL                          
         B     NXTEL2                                                           
NXTEL    CLI   0(R6),0                                                          
         BE    NXTELX                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NXTEL2   CLI   0(R6),0                                                          
         BE    NXTELX                                                           
         CLC   ELCDLO,0(R6)                                                     
         BH    NXTEL                                                            
         CLC   ELCDHI,0(R6)                                                     
         BL    NXTEL                                                            
         CR    RB,RB                                                            
         B     *+6                                                              
NXTELX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(4096-SUBR02L+1)                                              
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03)'                              
***********************************************************************         
*======================= SUBROUTINE POOL THREE =======================*         
SUBR03Q  EQU   ((((*-T21759)/4096)+1)*4096)                                     
                                                                                
         ORG   T21759+SUBR03Q                                                   
SUBR03   NMOD1 0,**5903**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE,                     
         SH    R1,=Y(R02#)          SUBTRACT FOR SUB-RTN # 3,                   
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R03_00(R1)                                                       
                                                                                
GRQ#     EQU   ((R03_01-R03_00)/4+1)+R02#                                       
SSL#     EQU   ((R03_02-R03_00)/4+1)+R02#                                       
SBL#     EQU   ((R03_03-R03_00)/4+1)+R02#                                       
SBU#     EQU   ((R03_04-R03_00)/4+1)+R02#                                       
SPL#     EQU   ((R03_05-R03_00)/4+1)+R02#                                       
SRE#     EQU   ((R03_06-R03_00)/4+1)+R02#                                       
DIV#     EQU   ((R03_07-R03_00)/4+1)+R02#                                       
CGLNTX#  EQU   ((R03_08-R03_00)/4+1)+R02#                                       
CGLTX#   EQU   ((R03_09-R03_00)/4+1)+R02#                                       
CIS#     EQU   ((R03_10-R03_00)/4+1)+R02#                                       
CLF#     EQU   ((R03_11-R03_00)/4+1)+R02#                                       
TRF#     EQU   ((R03_12-R03_00)/4+1)+R02#                                       
MNI#     EQU   ((R03_13-R03_00)/4+1)+R02#                                       
DLM#     EQU   ((R03_14-R03_00)/4+1)+R02#                                       
BLM#     EQU   ((R03_15-R03_00)/4+1)+R02#                                       
RLM#     EQU   ((R03_16-R03_00)/4+1)+R02#                                       
VNUM#    EQU   ((R03_17-R03_00)/4+1)+R02#                                       
DCD#     EQU   ((R03_18-R03_00)/4+1)+R02#                                       
RFB#     EQU   ((R03_19-R03_00)/4+1)+R02#                                       
DFR#     EQU   ((R03_20-R03_00)/4+1)+R02#                                       
                                                                                
R03_00   DS    0H                                                               
R03_01   B     GENREQ                                                           
R03_02   B     SLAVESL                                                          
R03_03   B     SLAVEBL                                                          
R03_04   B     SLAVEBU                                                          
R03_05   B     SLAVEPL                                                          
R03_06   B     SLAVRLM                                                          
R03_07   B     DIVIDE                                                           
R03_08   B     CGLNOTAX                                                         
R03_09   B     CGLTAX                                                           
R03_10   B     CHINTSY                                                          
R03_11   B     CLRFLD                                                           
R03_12   B     TRSFLD                                                           
R03_13   B     MODNXTIN                                                         
R03_14   B     DELEM                                                            
R03_15   B     BELEM                                                            
R03_16   B     RELEM                                                            
R03_17   B     VNUMERIC                                                         
R03_18   B     DOCURD                                                           
R03_19   B     RFNLBIL                                                          
R03_20   B     DEFERSPT                                                         
R03#     EQU   ((*-R03_00)/4+1)+R02#                                            
DIE_03   DC    H'0'                                                             
                                                                                
YES_03   SR    RC,RC                                                            
NO_03    LTR   RC,RC                                                            
XIT_03   XIT1                                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--GRQ#)'                        
*--------------------------- GENERATE REQUEST ------------------------*         
                                                                                
GENREQ   DS    0H                                                               
         CLC   REQID,=C'DDS'       IF REQUEST ID IS DDS                         
         BE    GRQX                                                             
         CLC   REQID,=C'XXX'        OR XXX,                                     
         BE    GRQX                 DON'T GENERATE REQUEST                      
                                                                                
*                                                                               
* Saving PW/MAINT's TWA into page 1 of TEMPSTR.  This will clobber the          
*  TIA info that's saved in TEMPSTR, but it's okay since we are still           
*  in the middle of a transaction.  If everything is done correctly,            
*  the TIA stuff will be written to TEMPSTR upon exit of program.               
*                                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATWA,0,0                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                  LOAD SPOTWRITER SL SCREEN                    
         LA    R0,CONTAGH                                                       
         GOTO1 CALLOV,DMCB,(R0),X'D90204EB'                                     
         OC    DMCB+9(3),DMCB+9                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                  FIRST FIND ENTRY FOR SL REQUEST              
         L     R2,ARQTWATB                                                      
         USING RQTWATBD,R2                                                      
*                                                                               
GRQ10    DS    0H                                                               
         CLI   0(R2),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RQTRQTYP,=C'SL'                                                  
         BE    GRQ20                                                            
         ZIC   R0,RQTLEN                                                        
         AR    R2,R0                                                            
         B     GRQ10                                                            
*                                                                               
GRQ20    DS    0H                  FILL IN DATA INTO SL SCREEN                  
         MVC   WHENDS(3),=C'OV,'                                                
         MVC   WHENDS+3(L'REQID),REQID                                          
         MVC   ALLDS,SP@ALL                                                     
         OC    ALLDS,SPACES                                                     
                                                                                
         MVC   LNCNTDWN,RQTFDNUM   USE LNCNTDWN AS COUNTER                      
         LA    R3,RQTFLDTA                                                      
         USING RQFDDSCT,R3                                                      
                                                                                
GRQ22    DS    0H                                                               
         ZICM  RE,RQFDFDSP,(3)                                                  
         A     RE,ATWA                                                          
         ZICM  RF,RQFDDDSP,(3)                                                  
         A     RF,ASYSD                                                         
         ZIC   R1,RQFDDLN1         R1 = L(DATA) - 1                             
         AR    R1,RF               POINT TO LAST BYTE OF DATA FIELD             
                                                                                
GRQ24    DS    0H                  COMPUTE L(ACTUAL DATA) IN DATA FLD           
         CR    R1,RF               ERROR IF R1 BECOMES LESS THAN RF             
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),C' '                                                       
         BH    GRQ26                                                            
         BCT   R1,GRQ24                                                         
                                                                                
GRQ26    DS    0H                                                               
         SR    R1,RF                                                            
         EXMVC R1,8(RE),0(RF)      MOVE DATA INTO TWA FIELD                     
         LA    R1,1(R1)                                                         
         STC   R1,5(RE)                                                         
                                                                                
         ZIC   R0,LNCNTDWN                                                      
         SH    R0,=H'1'                                                         
         BZ    GRQ28                                                            
         STC   R0,LNCNTDWN                                                      
         LA    R3,RQFDLENQ(R3)                                                  
         B     GRQ22                                                            
         DROP  R2,R3                                                            
                                                                                
GRQ28    DS    0H                                                               
         XC    CONREC,CONREC                                                    
         MVC   CONREC(2),=C'SL'                                                 
         MVI   CONRECH+5,2                                                      
                                                                                
         XC    CONACT,CONACT                                                    
         MVC   CONACT(6),=C'REPORT'                                             
         MVI   CONACTH+5,6                                                      
                                                                                
         B     GRQ30                                                            
*                                                                               
GRQ30    DS    0H                                                               
         MVI   GCMODE,C'S'         FORCE SLAVE MODE TO RETURN                   
         GOTO1 BLDREQST                                                         
         MVI   GCMODE,0             CONTROL TO ME, AND UNSLAVE IT               
         B     GRQ40                                                            
*                                                                               
* Restore PW/Maint screen.                                                      
*                                                                               
GRQ40    DS    0H                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=H'18432'                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATWA,0                      
         CLI   8(R1),0                                                          
         BE    GRQ42                                                            
         DC    H'0'                                                             
                                                                                
GRQ42    DS    0H                  TRANSMIT WHOLE SCREEN                        
         L     R2,ATWA                                                          
         LA    R2,64(R2)                                                        
         CLI   0(R2),0                                                          
         BE    *+16                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'                                                 
                                                                                
         B     GRQX                                                             
*                                                                               
GRQX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--SSL#)'                        
*------------------------ SLAVE STATION LOCKIN -----------------------*         
                                                                                
SLAVESL  DS    0H                                                               
                                                                                
         DS    0H                  SLAVE VK                                     
         LA    RE,SLDR             SET RETURN ADDRESS                           
SLVK05   NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVK              VALIDATE KEY                                 
         BR    RF                                                               
*                                                                               
SLDR     DS    0H                  SLAVE DR                                     
         LA    RE,SLVR             SET RETURN ADDRESS                           
SLDR05   NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,ADR              DISPLAY RECORD                               
         BR    RF                                                               
*                                                                               
SLVR     DS    0H                  SLAVE VR                                     
         NI    MISCFLG1,XFF-MF1ERRQ-MF1KYOPT                                    
         LA    RE,SLXIT            SET RETURN ADDRESS                           
SLVR05   NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVR              VALIDATE RECORD                              
         BR    RF                                                               
*                                                                               
SLXIT    DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--SBL#)'                        
*--------------------------- SLAVE BUY LOCK -------------------------*          
                                                                                
SLAVEBL  DS    0H                                                               
                                                                                
         DS    0H                  SLAVE VK                                     
         LA    RE,SL6DR            SET RETURN ADDRESS                           
S6VK     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVK              VALIDATE KEY                                 
         BR    RF                                                               
*                                                                               
SL6DR    DS    0H                  SLAVE DR                                     
         LA    RE,SL6VR            SET RETURN ADDRESS                           
S6DR     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,ADR              DISPLAY RECORD                               
         BR    RF                                                               
*                                                                               
SL6VR    DS    0H                  SLAVE VR                                     
         NI    MISCFLG1,XFF-MF1ERRQ-MF1KYOPT                                    
         NI    LOCKFLAG,XFF-LKFUBLKQ   FOR BUY TO BE UNLOCKED,                  
         MVI   PFKEY,2                  AND SIMULATE PF2 TO LOCK BUY            
         LA    RE,SL6XIT           SET RETURN ADDRESS                           
S6VR     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVR              VALIDATE RECORD                              
         BR    RF                                                               
*                                                                               
SL6XIT   DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--SBU#)'                        
*-------------------------- SLAVE BUY UNLOCK ------------------------*          
                                                                                
SLAVEBU  DS    0H                                                               
                                                                                
         DS    0H                  SLAVE VK                                     
         LA    RE,SL7DR            SET RETURN ADDRESS                           
S7VK     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVK              VALIDATE KEY                                 
         BR    RF                                                               
*                                                                               
SL7DR    DS    0H                  SLAVE DR                                     
         LA    RE,SL7VR            SET RETURN ADDRESS                           
S7DR     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,ADR              DISPLAY RECORD                               
         BR    RF                                                               
*                                                                               
SL7VR    DS    0H                  SLAVE VR                                     
         NI    MISCFLG1,XFF-MF1ERRQ-MF1KYOPT                                    
         OI    LOCKFLAG,LKFUBLKQ   FOR BUY TO BE LOCKED,                        
         MVI   PFKEY,2              AND SIMULATE PF2 TO UNLOCK BUY              
         LA    RE,SL7XIT           SET RETURN ADDRESS                           
S7VR     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVR              VALIDATE RECORD                              
         BR    RF                                                               
*                                                                               
SL7XIT   DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--SPL#)'                        
*--------------------------- SLAVE PW  LOCK -------------------------*          
                                                                                
SLAVEPL  DS    0H                                                               
                                                                                
         DS    0H                  SLAVE VK                                     
         LA    RE,SL8DR            SET RETURN ADDRESS                           
S8VK     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVK              VALIDATE KEY                                 
         BR    RF                                                               
*                                                                               
SL8DR    DS    0H                  SLAVE DR                                     
         LA    RE,SL8VR            SET RETURN ADDRESS                           
S8DR     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,ADR              DISPLAY RECORD                               
         BR    RF                                                               
*                                                                               
SL8VR    DS    0H                  SLAVE VR                                     
         NI    MISCFLG1,XFF-MF1ERRQ-MF1KYOPT                                    
         NI    LOCKFLAG,XFF-LKFUPLKQ   FOR PW  TO BE UNLOCKED,                  
         MVI   PFKEY,3                  AND SIMULATE PF3 TO LOCK PW             
         LA    RE,SL8XIT           SET RETURN ADDRESS                           
S8VR     NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVR              VALIDATE RECORD                              
         BR    RF                                                               
*                                                                               
SL8XIT   DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--SRE#)'                        
*--------------------------- SLAVE RESTORE --------------------------*          
                                                                                
SLAVRLM  DS    0H                                                               
                                                                                
         DS    0H                  SLAVE VK                                     
         LA    RE,SRE20            SET RETURN ADDRESS                           
SRE10    NTR1                                                                   
         L     RB,BASE1                                                         
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
         L     RF,AVK              VALIDATE KEY (TO GET KEY INFO)               
         BR    RF                                                               
*                                                                               
SRE20    DS    0H                                                               
         MVI   GOSUBN,GPR#         GET MKT-LEVEL PW RECORD                      
         GOTO1 AGOSUB                                                           
         L     R6,AIO                                                           
*                                                                               
SRE30    DS    0H                                                               
         MVI   GOSUBN,DLM#                                                      
         MVI   ELEMENT,0           NO SEARCH ARGUMENTS                          
         MVI   ELCODE,PWDOLCDQ                                                  
         GOTO1 AGOSUB              DELETE EXISTING PWDOLEL'S                    
         MVI   ELCODE,PWCLLCDQ                                                  
         GOTO1 (RF)                 AND PWCLLEL'S                               
                                                                                
         MVI   GOSUBN,RLM#                                                      
         MVI   ELCODE,PWDOLCDQ                                                  
         OI    ELCODE,X80                                                       
         GOTO1 AGOSUB              RESTORE BACKUP ELEMENTS TO                   
         MVI   ELCODE,PWCLLCDQ                                                  
         OI    ELCODE,X80                                                       
         GOTO1 (RF)                 THEIR ORIGINAL ELCODES                      
                                                                                
         GOTO1 PUTREC              WRITE RECORD BACK TO FILE                    
                                                                                
         DS    0H                  DO STATION-LEVEL PW RECORDS                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(PKYMKTL),KEYSAVE                                             
         BNE   SREX                 IF THERE ARE ANY                            
         GOTO1 GETREC                                                           
         B     SRE30                                                            
*                                                                               
SREX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--DIV#)'                        
*------------------------ GENERIC DIVIDE LOGIC -----------------------*         
                                                                                
* At entry, DIVIDEND, DIVISOR are set.                                          
* At exit, QUOTIENT and REMAINDR are set.                                       
                                                                                
DIVIDE   DS    0H                                                               
         XC    QUOTIENT,QUOTIENT                                                
         XC    REMAINDR,REMAINDR                                                
                                                                                
         ICM   RF,15,DIVISOR       IF DIVISOR IS ZERO,                          
         BZ    XIT_03               CALLER GETS ZERO BACK                       
         OC    DIVIDEND,DIVIDEND   IF DIVIDEND IS ZERO                          
         BZ    XIT_03               CALLER GETS ZERO BACK ALSO                  
                                                                                
         DS    0H                  CALCULATE QUOTIENT                           
         LM    R0,R1,DIVIDEND                                                   
         SLDA  R0,1                DOUBLE DIVIDEND                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,QUOTIENT                                                      
                                                                                
         DS    0H                  CALCULATE REMAINDER                          
         AH    R0,=H'1'                                                         
         SRA   R0,1                                                             
         ST    R0,REMAINDR                                                      
                                                                                
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--CGLNTX# && CGLTX#)'           
*----------------------- SKEDCGL FROM SKEDCGTX -----------------------*         
                                                                                
* Derives SKEDCGL (goal w/o tax) from SKEDCGTX (goal+tax) given                 
*  the tax rate in GLTXRATE.                                                    
                                                                                
CGLNOTAX DS    0H                                                               
         SR    RE,RE                                                            
         L     RF,SKEDCGTX                                                      
         L     R0,=F'100000'       100% = 100000                                
         MR    RE,R0                                                            
         STM   RE,RF,DIVIDEND      DIVIDEND SCALED UP FOR DIVISION              
         AH    R0,GLTXRATE         R0 = (1 + TAXRATE)                           
         ST    R0,DIVISOR                                                       
         MVI   GOSUBN,DIV#                                                      
         GOTO1 AGOSUB                                                           
         MVC   SKEDCGL,QUOTIENT                                                 
                                                                                
         B     XIT_03                                                           
                                                                                
                                                                                
*----------------------- SKEDCGTX FROM SKEDCGL -----------------------*         
                                                                                
* Derives SKEDCGTX (goal+tax) from SKEDCGL (goal w/o tax) given                 
*  the tax rate in GLTXRATE.                                                    
                                                                                
CGLTAX   DS    0H                                                               
         SR    RE,RE                                                            
         L     RF,SKEDCGL                                                       
         L     R0,=F'100000'       100% = 100000                                
         ST    R0,DIVISOR          SET UP DIVISOR (SAVE AN INSTRUCTION)         
                                                                                
         AH    R0,GLTXRATE         R0 = (1 + TAXRATE)                           
         MR    RE,R0                                                            
                                                                                
         STM   RE,RF,DIVIDEND      SCALE PRODUCT DOWN (DIVISOR ABOVE)           
         MVI   GOSUBN,DIV#                                                      
         GOTO1 AGOSUB                                                           
         MVC   SKEDCGTX,QUOTIENT                                                
                                                                                
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--CIS#, CLF#, && TRF#)'         
* At entry to these routines, R2-->line                                         
                                                                                
*-------------------------- CHANGE INTENSITY -------------------------*         
                                                                                
* At entry,                                                                     
*   BYTE = 'H' for high   intensity                                             
*   BYTE = 'N' for normal intensity                                             
                                                                                
CHINTSY  DS    0H                                                               
         LA    R3,MSLLENQ(R2)                                                   
CIS10    CR    R2,R3                                                            
         BNL   XIT_03                                                           
         NI    1(R2),X'F3'         TURN OFF INTENSITY BITS                      
         CLI   BYTE,C'H'           HIGH INTENSITY WANTED?                       
         BNE   *+8                  NO                                          
         OI    1(R2),X08            YES                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     CIS10                                                            
                                                                                
                                                                                
*---------------------------- CLEAR FIELDS ---------------------------*         
                                                                                
CLRFLD   DS    0H                                                               
         LA    R3,MSLLENQ(R2)                                                   
CLF10    CR    R2,R3                                                            
         BNL   XIT_03                                                           
         ZIC   R0,0(R2)                                                         
         LR    R1,R0                                                            
         SH    R1,=H'9'            L(FIELD HDR) + 1                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR DATA FIELD                             
         AR    R2,R0                AND BUMP TO NEXT FIELD                      
         B     CLF10                                                            
                                                                                
                                                                                
*-------------------------- TRANSMIT FIELDS --------------------------*         
                                                                                
TRSFLD   DS    0H                                                               
         LA    R3,MSLLENQ(R2)                                                   
TRF10    CR    R2,R3                                                            
         BNL   XIT_03                                                           
         OI    6(R2),X80           TURN ON TRANSMIT BIT                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     TRF10                                                            
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--MNI#)'                        
*----------------------- MODIFY FOR NEXT INPUT -----------------------*         
                                                                                
* Bumps through the PW/Maint screen (starting w/ media field) and               
*  turns on the "change to modified field for next input" flag (x'01'           
*  in FLDHDR+6) for those fields which were inputted this time.                 
*  Routine is usually called when an error occurred during a validation         
*  so that we know which fields to validate in the next transaction.            
                                                                                
MODNXTIN DS    0H                                                               
         LA    R2,C2MMEDH                                                       
*                                                                               
MNI10    DS    0H                                                               
         CLI   0(R2),0             AT END OF SCREEN?                            
         BE    MNIX                                                             
         TM    2(R2),X20           SKIP OVER PROTECTED FIELDS                   
         BO    MNI20                                                            
                                                                                
         TM    4(R2),X80           WAS IT MODIFIED THIS TIME?                   
         BZ    MNI20                                                            
         OI    6(R2),X01            YES, KEEP IT MODFIED UNTL VLDATED           
*                                                                               
MNI20    DS    0H                  BUMP TO NEXT FIELD                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     MNI10                                                            
*                                                                               
MNIX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--DLM#)'                        
*-------------------------- DELETE ELEMENTS --------------------------*         
                                                                                
* Deletes all occurences of an element with a given element code & an           
*  optional search argument from a record.                                      
* At entry,                                                                     
*   SYSFIL = name of file,                                                      
*   R6    -->record,                                                            
*   ELCODE = code of elements to be removed.                                    
*   ELEMENT(1) = l(optional search argument)--zero means none,                  
*   ELEMENT+1  = optional search argument                                       
                                                                                
DELEM    DS    0H                                                               
         CLI   ELCODE,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                  SET UP OPTNL SRCH ARG PARAMS                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R2,1,ELEMENT                                                     
         BZ    *+8                                                              
         LA    R3,ELEMENT+1                                                     
*                                                                               
DLM10    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,(R6)),((R2),(R3)),0             
         CLI   12(R1),0                                                         
         BE    DLM20                                                            
         CLI   12(R1),X'06'                                                     
         BE    DLMX                                                             
         DC    H'0'                                                             
                                                                                
DLM20    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,(R6)),((R2),(R3)),0             
         CLI   12(R1),0            KEEP DELETING UNTIL                          
         BE    DLM10                ELEMENT NOT FOUND                           
         DC    H'0'                                                             
*                                                                               
DLMX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--BLM#)'                        
*-------------------------- BACKUP ELEMENTS --------------------------*         
                                                                                
* Backs up all occurences of an element of a given element code by              
*  turning on the x'80' bit in their element codes.                             
* At entry,                                                                     
*   SYSFIL   = name of file record resides in,                                  
*   R6      -->record,                                                          
*   ELCODE   = code of elements to be removed.                                  
                                                                                
BELEM    DS    0H                                                               
         CLI   ELCODE,0                                                         
         BNE   BLM10                                                            
         DC    H'0'                SOMETHING WENT WRONG SOMEWHERE!              
*                                                                               
BLM10    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,(R6)),0,0                       
         CLI   12(R1),0                                                         
         BE    BLM20                                                            
         CLI   12(R1),X'06'                                                     
         BE    BLMX                                                             
         DC    H'0'                                                             
*                                                                               
BLM20    DS    0H                                                               
         L     R3,12(R1)           R3-->ELEMENT TO BE BACKED UP                 
         ZIC   R4,1(R3)                                                         
         BCTR  R4,0                                                             
         EXMVC R4,ELEM,0(R3)       PUT IT SOMEWHERE IN STORAGE                  
         OI    ELEM,X80             AND, TURN ON X'80' BIT IN ELCODE            
                                                                                
         BCTR  R4,0                R4=L(SEARCH ARGUMENT)                        
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,(R6)),((R4),ELEM+2),0           
         CLI   12(R1),0            DELETE ELEMENT FROM RECORD                   
         BE    BLM30                                                            
         DC    H'0'                                                             
*                                                                               
BLM30    DS    0H                  PUT BACKUP ELEMENT INTO RECORD               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),ELEMENT,0                          
         CLI   12(R1),0                                                         
         BE    BLM10                AND GO BACK FOR SOME MORE                   
         DC    H'0'                                                             
*                                                                               
BLMX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--RLM#)'                        
*-------------------------- RESTORE ELEMENTS -------------------------*         
                                                                                
* Restores the backup elements to their original element codes.  The            
*  current elements with the original elcode are deleted.                       
* So far, the elements that can be restored are PWDOLEL and PWCLLEL.            
* At entry,                                                                     
*   SYSFIL   = name of file record resides in,                                  
*   R6      -->record,                                                          
*   ELCODE   = code of elements to be removed.                                  
                                                                                
RELEM    DS    0H                                                               
         CLI   ELCODE,0                                                         
         BE    RLMDIE1                                                          
         TM    ELCODE,X80                                                       
         BZ    RLMDIE1                                                          
         B     RLM10                                                            
RLMDIE1  DC    H'0'                SOMETHING WENT WRONG SOMEWHERE!              
*                                                                               
RLM10    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,(R6)),0,0                       
         CLI   12(R1),0                                                         
         BE    RLM20                                                            
         CLI   12(R1),X'06'                                                     
         BE    RLMX                                                             
         DC    H'0'                                                             
*                                                                               
RLM20    DS    0H                                                               
         L     R3,12(R1)           R3-->ELEMENT TO BE RESTORED                  
         ZIC   R4,1(R3)                                                         
         BCTR  R4,0                                                             
         EXMVC R4,ELEM,0(R3)       PUT IT SOMEWHERE IN STORAGE                  
         NI    ELEM,XFF-X80         AND, TURN OFF X'80' BIT IN ELCODE           
                                                                                
         BCTR  R4,0                R4=L(SEARCH ARGUMENT)                        
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,(R6)),((R4),ELEM+2),0           
         CLI   12(R1),0            DELETE ELEMENT FROM RECORD                   
         BE    RLM30                                                            
         DC    H'0'                                                             
*                                                                               
RLM30    DS    0H                  PUT RESTORED ELEMENT INTO RECORD             
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),ELEMENT,0                          
         CLI   12(R1),0                                                         
         BE    RLM10                AND GO BACK FOR SOME MORE                   
         DC    H'0'                                                             
*                                                                               
RLMX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--VNUM#)'                       
*-------------------------- VALIDATE NUMERIC -------------------------*         
                                                                                
* Validates input for numerics.                                                 
* At entry,                                                                     
*   WORK = simulated TWA field containing input.                                
* At exit,                                                                      
*   CC set to equal if valid,                                                   
*   CC set ti not-equal if invalid.                                             
                                                                                
VNUMERIC DS    0H                                                               
         ZIC   RF,WORK+5           RF = L(INPUT)                                
         LA    RE,WORK+8           RE-->INPUT                                   
*                                                                               
VNUM10   DS    0H                  LOOP THRU & CHECK EACH CHARACTER             
         CLI   0(RE),C'0'                                                       
         BL    VNUMXN                                                           
         CLI   0(RE),C'9'                                                       
         BH    VNUMXN                                                           
         BCT   RF,VNUM10                                                        
                                                                                
                                                                                
VNUMXY   DS    0H                                                               
         B     YES_03                                                           
*                                                                               
VNUMXN   DS    0H                                                               
         B     NO_03                                                            
                                                                                
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--DCD#)'                        
*------------------------- DO CURRENT DOLLARS ------------------------*         
                                                                                
* Updates PWCUREL elements for those months whose Adj DR/CR $                   
*  values were modified.  These are the elements billing reads to               
*  report current dollar amounts.  PWDOLSPT for each week in those              
*  months gets updated to the current number of spots, for both mkt-            
*  and station-level PW records (per Grant).                                    
* At entry,                                                                     
*   R6-->PW record                                                              
***********************************************************************         
DOCURD   DS    0H                                                               
         LR    R3,R6               HOLD ONTO A(PW RECORD) IN R3                 
         USING PWRECD,R3                                                        
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
DCD10    DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ     IF SKED ENTRY REACHED,                       
         BO    DCDX                 GO EXIT                                     
*                                                                               
         LR    R5,R4               HOLD ONTO A(1ST WEEK OF MONTH)               
DCD12    LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DCD12                                                            
         TM    ATFLAG,ATFAJBQ      WAS ADJ DR/CR MODIFIED?                      
         BO    DCD20                YES, UPDATE CURRENT $'s                     
         TM    ATFLAG2,ATF2CHDC    WAS ADJ DR/CR VALUE CHANGED?                 
         BO    DCD20                YES, UPDATE CURRENT $'s                     
         B     DCDBUMP             ELSE, DON'T UPDATE CURRENT $'S               
*                                                                               
DCD20    DS    0H                  UPDATE PWCUREL'S FOR THIS MONTH              
         XR    R4,R5               SWAP R4 AND R5                               
         XR    R5,R4                                                            
         XR    R4,R5               R4-->1ST WEEK, R5-->BILL ADJ ENTRY           
         MVI   BYTE,0              FLAG (0=1ST WEEK, 1=NOT 1ST WEEK)            
*                                                                               
DCD22    DS    0H                                                               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ   DID ALL WEEKS IN MONTH YET?           
         BO    DCD70                       YEP, NO MORE PWCUREL'S               
*                                                                               
         DS    0H                  DELETE PWCUREL ELEMENT FROM RECORD           
         LR    R6,R3                POINT R6 TO PW RECORD                       
         MVI   ELCODE,PWCURCDQ      ELEMENT CODE                                
         MVI   ELEMENT,0            NO SRCH ARG                                 
*****    MVC   ELEMENT+1(L'ATWSTART),ATWSTART                                   
         MVI   GOSUBN,DLM#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                  GET DATA TO PUT INTO PWCUREL                 
         XC    TEMPNSPT,TEMPNSPT                                                
         XC    TEMPACB,TEMPACB                                                  
         XC    TEMPACBN,TEMPACBN                                                
         XC    TEMPAJB,TEMPAJB                                                  
         XC    TEMPAJBN,TEMPAJBN                                                
         XC    TEMPTAX,TEMPTAX                                                  
*                                                                               
         XC    SVTSRNUM,SVTSRNUM   SAVE TSAR RECD#                              
         ZICM  RF,TSARBLK+(TSRNUM-TSARD),(3)                                    
         BZ    *+16                                                             
         CLM   RF,3,TSARBLK+(TSPRECN-TSARD)                                     
         BH    *+8                                                              
         STCM  RF,3,SVTSRNUM                                                    
*                                                                               
         L     RF,TSARBLK+(TSAREC-TSARD)                                        
         LA    RF,0(RF)                                                         
         USING STACRECD,RF                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         DROP  RF                                                               
         MVI   GOSUBN,TSR_RDH#                                                  
*                                                                               
DCD42    DS    0H                                                               
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF                                                  
         BNZ   DCD48                                                            
*                                                                               
         L     RF,TSARBLK+(TSAREC-TSARD)                                        
         LA    RF,0(RF)                                                         
         USING STACRECD,RF                                                      
         OC    PWKSTA,PWKSTA                                                    
         BZ    *+14                                                             
         CLC   STACSTA,PWKSTA                                                   
         BNE   DCD44                                                            
*****    CLC   STACSTRT,ATWSTART                                                
*****    BNE   DCD44                                                            
*                                                                               
         L     R1,TEMPNSPT         NUMBER OF SPOTS                              
         A     R1,STACSPT                                                       
         ST    R1,TEMPNSPT                                                      
         L     R1,TEMPACB          WIM GROSS $                                  
         A     R1,STACGRS                                                       
         ST    R1,TEMPACB                                                       
         L     R1,TEMPACBN         WIM NET   $                                  
         A     R1,STACNET                                                       
         ST    R1,TEMPACBN                                                      
         L     R1,TEMPAJB          CLT GROSS $                                  
         A     R1,STACCGRS                                                      
         ST    R1,TEMPAJB                                                       
         L     R1,TEMPAJBN         CLT NET   $                                  
         A     R1,STACCNET                                                      
         ST    R1,TEMPAJBN                                                      
         L     R1,TEMPTAX          WIM TAX   $                                  
         A     R1,STACTAX                                                       
         ST    R1,TEMPTAX                                                       
         L     R1,TEMPCTX          CLT TAX   $                                  
         A     R1,STACCTX                                                       
         ST    R1,TEMPCTX                                                       
         DROP  RF                                                               
*                                                                               
DCD44    DS    0H                                                               
         MVI   GOSUBN,TSR_NXT#                                                  
         B     DCD42                                                            
*                                                                               
DCD48    DS    0H                  DONE READING THROUGH STTN ACCUM TBL          
         OC    SVTSRNUM,SVTSRNUM    NEED TO RESTORE ORIG TSAR RECD?             
         BZ    DCD48X                NOPE                                       
         MVC   TSARBLK+(TSRNUM-TSARD)(L'TSRNUM),SVTSRNUM                        
         MVI   GOSUBN,TSR_GET#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DCD48X   EQU   *                                                                
         B     DCD50                                                            
*                                                                               
DCD50    DS    0H                  UPDATE PW RECORD                             
         OC    PWKSTA,PWKSTA       IF STATION-LEVEL RECD,                       
         BNZ   DCD55                DON'T PROCESS PWCUREL                       
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING PWCUREL,R6                                                       
         MVI   PWCURCD,PWCURCDQ                                                 
         MVI   PWCURLEN,PWCURLNQ                                                
*****    MVC   PWCURWK,ATWSTART                                                 
         MVC   PWCURSPT,TEMPNSPT                                                
         MVC   PWCURWG,TEMPACB                                                  
         MVC   PWCURWN,TEMPACBN                                                 
         MVC   PWCURCG,TEMPAJB                                                  
         MVC   PWCURCN,TEMPAJBN                                                 
         MVC   PWCURTAX,TEMPTAX                                                 
         MVC   PWCURCTX,TEMPCTX                                                 
         CLI   BYTE,0                                                           
         BNE   *+20                                                             
         MVC   PWCURBIL,(ATDRCR-ACCUTABD)(R5)                                   
         MVC   PWCURBLD,(ATBILD-ACCUTABD)(R5)                                   
         MVI   BYTE,1              CHANGE FLAG TO NOT 1ST WK FOR NEXT           
         DROP  R6                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R3),(R6),0                             
         CLI   12(R1),0                                                         
         BE    DCD55                                                            
         DC    H'0'                                                             
*                                                                               
DCD55    DS    0H                  UPDATE PWDOLSPT (AS PER GRANT)               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWDOLCDQ',(R3)),0,0                   
         L     R6,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    DCD57                                                            
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         CLI   12(R1),6                                                         
         BE    DCD57                                                            
         DC    H'0'                                                             
*                                                                               
DCD57    DS    0H                                                               
         USING PWDOLEL,R6                                                       
         MVI   PWDOLCD,PWDOLCDQ                                                 
         MVI   PWDOLLEN,PWDOLLNQ                                                
*****    MVC   PWDOLWK,ATWSTART                                                 
         MVC   PWDOLSPT,TEMPNSPT   UPDATE # OF SPOTS                            
         DROP  R6                                                               
*                                                                               
         CLI   12(R1),0            DO WE NEED TO ADD ELEM TO RECD               
         BE    DCD60                NOPE                                        
*                                                                               
         DS    0H                   YEP                                         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R3),(R6),0                             
         CLI   12(R1),0                                                         
         BE    DCD60                                                            
         DC    H'0'                                                             
*                                                                               
DCD60    DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         B     DCD22                                                            
*                                                                               
DCD70    DS    0H                  PROCESS NEXT MONTH'S STUFF                   
         B     DCDBUMP                                                          
*                                                                               
DCDBUMP  LA    R4,ACCUTABQ(R4)     R4 MUST BE @ BILL ADJ ENTRY                  
         B     DCD10                                                            
*                                                                               
DCDX     DS    0H                                                               
         B     XIT_03                                                           
         DROP  R3,R4                                                            
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--RFB#)'                        
*-------------------------- RE-FINAL BILLING -------------------------*         
* Routine modifies PW records in case final billing needs to be                 
*  re-generated.                                                                
***********************************************************************         
RFNLBIL  DS    0H                                                               
         NI    MISCFLG3,XFF-MF3RFBQ  ASSUME NO RE-FINAL BILLING NEEDED          
         MVC   AIO,AIO2              USE 2ND I/O AREA IN THIS ROUTINE           
         MVI   GOSUBN,BPK#           BUILD KEY OF MKT PW RECORD                 
         GOTO1 AGOSUB                                                           
                                                                                
         GOTO1 HIGH                                                             
*                                                                               
** Go through ACCUTAB for each PW recd **                                       
*                                                                               
RFB10    DS    0H                                                               
         CLC   KEY(PKYMKTL),KEYSAVE   SAME MED/CLT/PRD/EST/MKT?                 
         BNE   RFBX                    NOPE, EXIT NOW                           
                                                                                
         GOTO1 GETREC                  YEP, GET THE PW RECORD                   
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
*** See if month will need re-final billing ***                                 
*                                                                               
RFB20    DS    0H                  R4-->1ST WEEK ENTRY                          
         LR    R5,R4                AND HAVE R5 HOLD ON TO IT                   
         TM    ATFLAG,ATFSTTLQ     IF SKED ENTRY                                
         BO    RFB80                THEN TIME TO PUT RECD BACK                  
RFB20A   TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BO    RFB22                                                            
         LA    R4,ACCUTABQ(R4)                                                  
         B     RFB20A              GET R4 TO BILL ADJ ENTRY                     
                                                                                
RFB22    DS    0H                                                               
         OC    PWKSTA,PWKSTA       IF STATION-LEVEL PW RECD,                    
         BZ    *+16                                                             
         TM    ATFLAG2,ATF2RFB      WE KNOW IF MNTH IS TO BE RE-FNL BIL         
         BO    RFB50                 MONTH NEEDS RE-FINAL BILLING               
         B     RFB70                 ELSE, BUMP TO NEXT MONTH                   
*                                                                               
         DS    0H                  PW RECD IS MKT-LEVEL                         
         TM    ATFLAG,ATFFNLBQ     IS MONTH FINAL BILLED AT ALL?                
         BZ    RFB70                NOPE, DON'T WORRY ABOUT THIS MONTH          
         TM    ATFLAG2,ATF2UNPD    AN UNPAID SPOT IN MONTH?                     
         BO    RFB50                YES, CHANGE RECD FOR RE-FINAL BILL          
*                                                                               
         DS    0H                  GET WIMCOST AND # SPOTS FROM PWCUREL         
         XC    TEMPACB,TEMPACB                                                  
         XC    TEMPNSPT,TEMPNSPT                                                
         LR    R4,R5               POINT R4 TO 1ST WEEK OF MONTH AGAIN          
RFB32    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWCURCDQ',(R6)),0,0                   
         CLI   DMCB+12,0                                                        
         BNE   RFB34                                                            
                                                                                
         L     RF,DMCB+12                                                       
         USING PWCUREL,RF                                                       
         ICM   R1,15,PWCURWG                                                    
         A     R1,TEMPACB                                                       
         ST    R1,TEMPACB          SUM UP WIMCOST FROM FILE                     
         ICM   R1,15,PWCURSPT                                                   
         A     R1,TEMPNSPT                                                      
         ST    R1,TEMPNSPT         SUM UP # SPOTS FROM FILE                     
         DROP  RF                                                               
                                                                                
RFB34    DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   RFB32                                                            
                                                                                
         CLC   ATNSPT,TEMPNSPT     DID # OF SPOTS FOR MONTH CHANGE?             
         BNE   RFB50                YEP, CHANGE RECD FOR RE-FNL BILL            
         CLC   ATACBUY,TEMPACB     DID WIMCOST FOR MONTH CHANGE?                
         BNE   RFB50                YEP, CHANGE RECD FOR RE-FNL BILL            
         B     RFB70               ELSE, BUMP TO NEXT MONTH                     
*                                                                               
*** Change PW recd for re-final billing for month ***                           
*                                                                               
RFB50    DS    0H                                                               
         XR    R4,R5               SWAP R4 & R5                                 
         XR    R5,R4                                                            
         XR    R4,R5               R4-->1ST WEEK, R5-->BILL ADJ ENTRY           
         MVI   HALF,0              FLAG: 0=1ST WEEK, 1=NOT 1ST WEEK             
         MVI   HALF+1,0            FLAG: 0=NO CHANGE, 1=RECD CHANGED            
                                                                                
RFB52    DS    0H                                                               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BO    RFB59                                                            
         CLI   HALF,0              1ST WEEK?                                    
         BNE   RFB55                NO, JUST DELETE PWCUREL ELEM                
                                                                                
         DS    0H                   YES, ZERO OUT PWDOLBIL & PWDOLSPT           
         MVI   HALF,1                AND CHANGE FLAG FOR NOT 1ST WEEK           
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWDOLCDQ',(R6)),0,0                   
         CLI   DMCB+12,0                                                        
         BNE   RFB58               CAN'T NULL OUT PWDOLBIL & PWDOLSPT           
                                                                                
         L     RF,DMCB+12                                                       
         USING PWDOLEL,RF                                                       
         OC    PWDOLBIL,PWDOLBIL   IF ADJ DR/CR AMT ALREADY NULLS,              
         BZ    RFB58                THEN LEAVE ELEMENT ALONE                    
         XC    PWDOLSPT,PWDOLSPT   NULL OUT # OF SPOTS                          
         XC    PWDOLBIL,PWDOLBIL   NULL OUT ADJ DR/CR AMOUNT                    
         MVC   PWDOLBLD,CTODAY      AND REMEMBER WHEN IT WAS DONE               
         OI    MISCFLG3,MF3RFBQ    RE-FINAL BILLING NEEDED                      
         OI    ATFLAG2-ACCUTABD(R5),ATF2RFB                                     
         MVI   HALF+1,1             MADE CHANGES TO MONTH IN RECD               
         DROP  RF                                                               
                                                                                
RFB55    DS    0H                  DELETE CORRESPONDING PWCUREL ELEM            
         OC    PWKSTA,PWKSTA        (FOR MKT-LVL PW RECD ONLY)                  
         BNZ   RFB57                                                            
         CLI   HALF+1,1            RECORD CHANGED?                              
         BNE   RFB57                NO, THEN DON'T DELETE ELEMS                 
         MVI   ELCODE,PWCURCDQ                                                  
         MVI   ELEMENT,0                                                        
*****    MVC   ELEMENT+1(L'ATWSTART),ATWSTART                                   
         MVI   GOSUBN,DLM#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
RFB57    DS    0H                  BUMP TO NEXT WEEK                            
         LA    R4,ACCUTABQ(R4)                                                  
         B     RFB52                                                            
                                                                                
RFB58    DS    0H                  DID NOT NULL OUT PWDOLBIL & PWDOLSPT         
         LR    R4,R5               POINT R4 BACK TO BILL ADJ ENTRY              
         B     RFB70                AND BUMP TO NEXT MONTH                      
                                                                                
RFB59    DS    0H                  MONTH CHANGED,                               
         B     RFB70                BUMP TO NEXT MONTH                          
                                                                                
*                                                                               
RFB70    DS    0H                  BUMP TO NEXT MONTH (R4-->BILL ADJ)           
         LA    R4,2*ACCUTABQ(R4)                                                
         B     RFB20                                                            
                                                                                
*                                                                               
RFB80    DS    0H                  WENT THRU ENTIRE ESTIMATE PERIOD             
         TM    MISCFLG3,MF3RFBQ    WILL RE-FINAL BILLING BE NEEDED?             
         BZ    RFBX                 NOPE, EXIT NOW                              
                                                                                
         GOTO1 PUTREC               YES, PUT UPDATED RECORD BACK                
                                                                                
         GOTO1 SEQ                   AND GET THE NEXT ONE                       
         B     RFB10                                                            
         DROP  R4,R6                                                            
*                                                                               
RFBX     DS    0H                                                               
         MVC   AIO,AIO1            RESTORE I/O AREA#1 AS DEFAULT                
         B     XIT_03                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--DFR#)'                        
*------------------ CHECK IF SPOT SHOULD BE DEFERRED -----------------*         
                                                                                
* AT ENTRY,                                                                     
*   R3    -->  SCHUNK                                                           
*   R6    -->  buy record                                                       
*   SDATE2  =  start date of broadcast month                                    
*   EDATE2  =  end   date of broadcast month                                    
*                                                                               
* At exit,                                                                      
*   CC set to equal if spot should be deferred,                                 
*   CC set to not equal otherwise                                               
                                                                                
DEFERSPT DS    0H                                                               
                                                                                
         USING SCHUNKD,R3                                                       
         USING BUYRECD,R6                                                       
                                                                                
*                                                                               
** SET RUN DATE OF SPOTS IN CHUNK **                                            
*                                                                               
         DS    0H                                                               
         MVC   RUNDATE,SCDATE      SET DATE                                     
*                                                                               
         DS    0H                                                               
         OC    SCADATE,SCADATE     IF SPOT IS MATCHED,                          
         BZ    *+14                                                             
         MVC   RUNDATE,SCADATE      USE AFFID DATE                              
         B     DFR019X                                                          
*                                                                               
         DS    0H                  ADVANCE TO LAST DAY OF ROTATION              
         CLC   RUNDATE,EDATE2       IF ALREADY PAST END OF B'CST MNTH,          
         BH    DFRXN                 CAN NOT BE DEFERRED                        
                                                                                
         SR    R0,R0                R0 = DAYS TO END OF ROTATION                
         ZIC   RE,BDSEDAY                                                       
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         CR    RE,RF                                                            
         BNH   *+12                                                             
         LA    RF,7(RF)                                                         
         SR    RF,RE                                                            
         LR    R0,RF                SET DAYS TO END OF ROTATION                 
                                                                                
         GOTO1 DATCON,DMCB,(2,SCDATE),(0,STARTEND)                              
         GOTO1 ADDAY,DMCB,STARTEND,STARTEND+6,(R0)                              
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(2,RUNDATE)                           
*                                                                               
DFR019X  EQU   *                                                                
                                                                                
*                                                                               
** TEST RUN DATE WITHIN BROADCAST MONTH **                                      
*                                                                               
         CLC   RUNDATE,SDATE2      IF BEFORE START,                             
         BL    DFRXN                IT CAN'T BE DEFERRED                        
         CLC   RUNDATE,EDATE2      IF AFTER END,                                
         BH    DFRXY                IT CAN BE DEFERRED                          
                                                                                
*                                                                               
         DS    0H                                                               
         B     DFRXN                                                            
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
DFRXN    DS    0H                                                               
         B     NO_03                                                            
*                                                                               
DFRXY    DS    0H                                                               
         B     YES_03                                                           
         DROP  R3,R6                                                            
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--LTORG && CONSTANTS)'          
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR03--MISC STUFF)'                  
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(4096-SUBR03L+1)                                              
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04)'                              
***********************************************************************         
*======================== SUBROUTINE POOL FOUR =======================*         
SUBR04Q  EQU   ((((*-T21759)/X'1000')+1)*X'1000')                               
                                                                                
         ORG   T21759+SUBR04Q                                                   
SUBR04   NMOD1 0,**5904**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE,                     
         SH    R1,=Y(R03#)          SUBTRACT FOR SUB-RTN # 4,                   
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R04_00(R1)                                                       
                                                                                
PBUY#    EQU   ((R04_01-R04_00)/4+1)+R03#                                       
STW#     EQU   ((R04_02-R04_00)/4+1)+R03#                                       
RTW#     EQU   ((R04_03-R04_00)/4+1)+R03#                                       
CEDC#    EQU   ((R04_04-R04_00)/4+1)+R03#                                       
TSR_INI# EQU   ((R04_05-R04_00)/4+1)+R03#                                       
TSR_ADD# EQU   ((R04_06-R04_00)/4+1)+R03#                                       
TSR_RDH# EQU   ((R04_07-R04_00)/4+1)+R03#                                       
TSR_WRT# EQU   ((R04_08-R04_00)/4+1)+R03#                                       
TSR_GET# EQU   ((R04_09-R04_00)/4+1)+R03#                                       
TSR_PUT# EQU   ((R04_10-R04_00)/4+1)+R03#                                       
TSR_NXT# EQU   ((R04_11-R04_00)/4+1)+R03#                                       
TSR_SAV# EQU   ((R04_12-R04_00)/4+1)+R03#                                       
TSR_RES# EQU   ((R04_13-R04_00)/4+1)+R03#                                       
FCO#     EQU   ((R04_14-R04_00)/4+1)+R03#                                       
PPDS#    EQU   ((R04_15-R04_00)/4+1)+R03#                                       
PPR#     EQU   ((R04_16-R04_00)/4+1)+R03#                                       
GBT#     EQU   ((R04_17-R04_00)/4+1)+R03#                                       
BPT#     EQU   ((R04_18-R04_00)/4+1)+R03#                                       
IAC#     EQU   ((R04_19-R04_00)/4+1)+R03#                                       
CPRTF#   EQU   ((R04_20-R04_00)/4+1)+R03#                                       
FOWP#    EQU   ((R04_21-R04_00)/4+1)+R03#                                       
OKUPD#   EQU   ((R04_22-R04_00)/4+1)+R03#                                       
                                                                                
R04_00   DS    0H                                                               
R04_01   B     PBUY                                                             
R04_02   B     SAVETWA             SAVE TWA INTO TEMPSTR                        
R04_03   B     RSTRTWA             RESTORE TWA FROM TEMPSTR                     
R04_04   B     CHKESTDT                                                         
R04_05   B     TSR_INI             INITIALIZE     TSAR BUFFER                   
R04_06   B     TSR_ADD             ADD RECORD TO  TSAR BUFFER                   
R04_07   B     TSR_RDH             READ HIGH FROM TSAR BUFFER                   
R04_08   B     TSR_WRT             WRITE RECD TO  TSAR BUFFER                   
R04_09   B     TSR_GET             GET RECD FROM  TSAR BUFFER                   
R04_10   B     TSR_PUT             PUT RECD INTO  TSAR BUFFER                   
R04_11   B     TSR_NXT             NEXT RECD FROM TSAR BUFFER                   
R04_12   B     TSR_SAV             SAVE           TSAR BUFFER                   
R04_13   B     TSR_RES             RESTORE        TSAR BUFFER                   
R04_14   B     FILLCLC             FILL CLCOST OVERRIDES INTO ACCUTAB           
R04_15   B     PPDS                PROCESS PAID SPOTS                           
R04_16   B     PUTMYCS2            PUT CS2 RECD INTO MY CS2 RECD AREA           
R04_17   B     GETBMTAB            GET BROADCAST MONTH TABLES                   
R04_18   B     BPWTAB              BUILD CS2 TABLE                              
R04_19   B     INITACTB            INITIALIZE ACCUTAB                           
R04_20   B     CPRTFLD             CHANGE PROTECTION OF FIELDS                  
R04_21   B     FILLOOWP            FILL IN OOW PW PAID$ INFO                    
R04_22   B     OKUPDFIL            OKAY TO UPDATE FILE?                         
R04#     EQU   ((*-R04_00)/4+1)+R03#                                            
DIE_04   DC    H'0'                                                             
                                                                                
YES_04   SR    RC,RC                                                            
NO_04    LTR   RC,RC                                                            
XIT_04   XIT1                                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--PBUY#)'                       
*----------------------------- PROCESS BUYS --------------------------*         
                                                                                
* This is the PROCBUY routine from the SPOTIO hook                              
* At entry,                                                                     
*   R2--SPOTBLOCK                                                               
*   R6--BUY RECORD                                                              
                                                                                
PBUY     DS    0H                                                               
         USING BUYRECD,R6                                                       
         CLC   BUYMSTA(2),BMKT                                                  
         BNE   PBUYX                                                            
         NI    MISCFLG2,XFF-MF2BUY0Q     AT LEAST 1 BUY EXISTS                  
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(3),BUYMSTA+2   GET MSPACKED STATION                         
*        TM    BUYMSTA+2,X'F0'     IF CABLE STATION,                            
*        BNO   *+10                                                             
         CLI   MYCANADA,C'C'        CANADIAN?                                   
         BE    PBUY01               YES                                         
         CLI   BUYMSTA+2,X'E8'     IF CABLE STATION,                            
         BL    *+10                                                             
         NC    FULL(3),CBLSCMSK     KEEP SYSCODE & IGNORE NETWORK               
PBUY01   CLI   USETAX,C'N'         DOES USER WANT TAX?                          
         BNE   *+10                 YES                                         
         XC    BDNTAX,BDNTAX        NOPE, SO CLEAR TAX FIELD                    
*                                                                               
         L     R4,AACCUTAB         FIND STATION IN OUR ACCUM TBL                
         USING ACCUTABD,R4                                                      
         LA    RE,ACCUTABX-ACCUTAB-1(R4)                                        
PBUY01A  TM    ATFLAG,ATFSTTLQ     HIT TOTALS OF THE ACCUM TABLE?               
         BNZ   PBUY01D                                                          
         CLC   ATSTAPKD,FULL       MATCH ON OUR STATION?                        
         BE    PBUY02                                                           
         AHI   R4,ACCUTABQ         R4 = A(NEXT ENTRY)                           
         CR    R4,RE                                                            
         BL    PBUY01A                                                          
         DC    H'0'                HIT END OF THIS TABLE                        
*                                                                               
PBUY01D  GOTO1 =A(NWACNTRY),RR=RELO  INSERT THE ENTRY WHERE TOTALS ARE          
*                                                                               
PBUY02   LA    R1,BDELEM           ANY COST2 FACTOR?                            
         SR    R0,R0                                                            
PBUY02A  CLI   0(R1),0             NONE, ADD ONE TO BUY                         
         BE    PBUY04                                                           
         CLI   0(R1),X'73'         COS2 IN THE BUY IS ALWAYS CURRENT            
         BE    PBUY05                                                           
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     PBUY02A                                                          
*                                                                               
PBUY04   XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         MVI   0(R1),X'73'                                                      
         MVI   1(R1),6                                                          
         MVC   2(L'ATCS2,R1),ATCS2                                              
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),WORK,0                             
         CLI   12(R1),0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
         PRINT OFF                                                              
*&&DO                                                                           
         MVI   GOSUBN,PDS#         SET SPOT PAID FLAGS                          
         GOTO1 AGOSUB                                                           
         CLI   MYERRCD,0           CHECK IF ANYTHING WENT WRONG                 
         BNE   PBUYX                                                            
*&&                                                                             
         PRINT ON                                                               
         USING SBLOCKD,R2                                                       
*^^gyl   BAS   RE,PBUYAFD          FUDGE AFFID DATES                            
PBUY05   TM    BDSTAT2,X'30'       TRADE BUY?                                   
         BZ    *+8                                                              
         MVI   BDPURP,X'FE'         YEP, MAKE $$ GO UNDER CASH PRODUCT          
                                                                                
         NI    MISCFLG3,XFF-MF3SSCHK  DON'T SKIP SPOT CHECK                     
         TM    BDCIND,X01             TEST FOR MINUS SPOT IN BUYLINE            
         BZ    *+8                     IF IT IS, SKIP #-OF-SPOTS TEST           
         OI    MISCFLG3,MF3SSCHK      SKIP SPOT CHECK                           
*                                                                               
         DS    0H                 PROCESS BUY RECORD                            
         OI    SBEFLAG2,SBENOMIN   IGNORE MINUS SPOTS (SAVES TBL SPACE)         
         OI    SBEFLAG2,SBESPLBY   SPLIT BUY (IF CHUNKS NOT BIG ENOUGH)         
         MVI   SBESPOTS,SBESPAID   GET PAID SPOTS FIRST                         
*                                                                               
PBUY08G  DS    0H                                                               
         MVC   WORK,BRDWKTB2                                                    
         MVC   WORK+L'BRDWKTB2(1),SBNDATES+3                                    
         MVI   SBNDATES+3,1        ONE PERIOD                                   
         XC    BRDWKTB2,BRDWKTB2                                                
         GOTO1 DATCON,DMCB,(0,ESDATE),(2,BRDWKTB2),0                            
         GOTO1 DATCON,DMCB,(0,EEDATE),(2,BRDWKTB2+2),0                          
         OI    SBEFLAG,SBE2COS     CALL FOR SECOND COST DATA                    
         L     RF,AMYWORK                                                       
         USING MYWORKD,RF                                                       
         MVC   SBACUNT,SBACONT                                                  
         DROP  RF                                                               
         GOTO1 ASPOTBUY,DMCB,SBLOCK                                             
         DROP  R6                                                               
         L     RF,AMYWORK                                                       
         USING MYWORKD,RF                                                       
         MVC   SBACONT,SBACUNT                                                  
         DROP  RF                                                               
*                                                                               
PBUY08GA L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
*                                                                               
PBUY08H  ICM   RE,15,SCNEXT                                                     
         BZ    PBUY08I                                                          
         MVC   SCGROSS2,SCGROSS                                                 
         MVC   SCNET2,SCNET                                                     
         LR    R5,RE                                                            
         B     PBUY08H                                                          
         DROP  R5                                                               
*                                                                               
PBUY08I  NI    SBEFLAG,XFF-SBE2COS     RESET SECOND COST FLAG                   
         OI    SBEFLAG,SBEA2COS                                                 
         GOTO1 ASPOTBUY,DMCB,SBLOCK                                             
PBUY08J  NI    SBEFLAG,XFF-SBEA2COS    RESET AFTER SECOND COST FLG              
*                                                                               
         MVC   BRDWKTB2,WORK                                                    
         MVC   SBNDATES+3(1),WORK+L'BRDWKTB2                                    
         TM    SBEFLAG2,SBENOMIN   IF THIS FLAG NOT TURNED OFF,                 
         BNZ   PBUY08X                                                          
         MVI   MYERRCD,TBOFQ        WE ENCOUNTERED A TABLE OVERFLOW             
         B     PBUYX                                                            
PBUY08X  EQU   *                                                                
*                                                                               
         L     R3,SBACHUNK                                                      
         USING SCHUNKD,R3                                                       
*                                                                               
PBUY10   OC    SCNEXT,SCNEXT                                                    
         BZ    PBUY090                                                          
*                                                                               
         DS    0H                                                               
         TM    MISCFLG3,MF3SSCHK   SKIP SPOT CHECK?                             
         BO    PBUYSPTS             YEP!                                        
*                                                                               
         ICM   R0,15,SCSPOTS       TEST IF THERE ARE ANY SPOTS                  
         BZ    PBUY30               IGNORE CHUNK IF THERE ARE NONE              
PBUYSPTS EQU   *                                                                
*                                                                               
         CLI   SBESPOTS,SBESPAID    CAN'T DEFER PAID SPOTS                      
         BE    PBUY14X                                                          
*                                                                               
         LA    R1,ACCUTABQ(R4)               BUMP TO NEXT ACCUTAB ENTRY         
         TM    ATFLAG-ACCUTABD(R1),ATFMTTLQ  WAS R4-->LAST WEEK IN MTH?         
         BZ    PBUY14X                        NO, CAN'T DEFER SPOT              
         TM    ATFLAG2-ACCUTABD(R1),ATF2A1PS ANY PAID SPOT IN MTH?              
         BZ    PBUY14X                        NO, CAN'T DEFER SPOT              
         MVC   SDATE2,ATMSTART                SET START AND                     
         MVC   EDATE2,ATMEND                   END DATES OF B'CST MNTH          
         MVI   GOSUBN,DFR#                    DEFER SPOT?                       
         GOTO1 AGOSUB                                                           
         BNE   PBUY14X                         NO, CAN'T DEFER SPOT             
*                                                                               
         LR    R1,R4                                                            
PBUY14G  LA    R1,ACCUTABQ(R1)                                                  
         TM    ATFLAG-ACCUTABD(R1),ATFSTTLQ                                     
         BNZ   PBUY14X                                                          
         TM    ATFLAG-ACCUTABD(R1),ATFMTTLQ                                     
         BNZ   PBUY14G                                                          
         LR    R4,R1               DEFER SPOT TO THE NEXT MONTH                 
PBUY14X  EQU   *                                                                
*                                                                               
         CLI   SBESPOTS,SBESUNPD   IF UNPAID SPOTS ARE BEING PASSED,            
         BNE   *+8                                                              
         OI    ATFLAG2,ATF2UNPD     MARK SPOT UNPAID IN WEEK                    
*                                                                               
PBUY20   DS    0H                                                               
         NI    ATFLAG,XFF-ATFNOSPT  WEEK HAS AT LEAST 1 SPOT                    
                                                                                
         ICM   R1,15,ATACBUY       TAKE WHAT'S IN ACCUTAB,                      
         A     R1,SCGROSS           ADD IT TO WHAT'S IN CHUNK,                  
         STCM  R1,15,ATACBUY        AND PUT IT BACK INTO ACCUTAB                
         ICM   R1,15,ATTAX         DO IT AGAIN FOR TAX DOLLARS                  
         A     R1,SCTAX                                                         
         STCM  R1,15,ATTAX                                                      
         ICM   R1,15,ATAJBUY       UPDATE ADJUSTED BUY ACCUMULATOR              
         A     R1,SCPWGRS                                                       
         STCM  R1,15,ATAJBUY                                                    
         ICM   R1,15,ATCLTAX       UPDATE CLIENT TAX DOLLARS                    
         A     R1,SCPWCLTX                                                      
         STCM  R1,15,ATCLTAX                                                    
         ICM   R1,15,ATNSPT        UPDATE # OF SPOTS                            
         A     R1,SCSPOTS                                                       
         STCM  R1,15,ATNSPT                                                     
*                                                                               
         MVI   GOSUBN,SST#                                                      
         GOTO1 AGOSUB              SET VALUES INTO STAT ACCUM TABLE             
*                                                                               
PBUY30   L     R3,SCNEXT                                                        
         B     PBUY10                                                           
         DROP  R3,R4                                                            
                                                                                
*                                                                               
** FINISH PROCESSING DATA IN CHUNKS AREA **                                     
*                                                                               
PBUY090  DS    0H                                                               
         OC    SBACONT,SBACONT     ANY BUY CONTINUATION?                        
         BZ    PBUY100              NO, DONE WITH BUY RECORD                    
         B     PBUY08G             GO GET THE REST OF THE BUY                   
                                                                                
*                                                                               
** FINISH PROCESSING BUY RECORD **                                              
*                                                                               
PBUY100  DS    0H                                                               
         CLI   SBESPOTS,SBESPAID   IF WE WERE GETTING PAID SPOTS                
         BNE   *+12                                                             
         MVI   SBESPOTS,SBESUNPD    GO BACK AND GET UNPAID SPOTS                
         B     PBUY08G                                                          
*                                                                               
         CLI   SBESPOTS,SBESUNPD   IF WE WERE GETTING UNPAID SPOTS              
         BNE   *+12                                                             
         MVI   SBESPOTS,0           WE'VE FINISHED PROCESSING BUY REC           
         B     PBUY200                                                          
         DC    H'0'                                                             
*                                                                               
PBUY200  DS    0H                                                               
         DROP  R2                                                               
*                                                                               
PBUYX    DS    0H                                                               
         B     XIT_04                                                           
         EJECT                                                                  
*&&DO                                                                           
* This routine was a desperation fix.  When SPOTBUY was asked to break          
*  out affid dates, my allocated CHUNK space was not enough to store            
*  all the CHUNK entries broken out for each different affid date.              
*  Since all that is really needed in the PROCBUY routine was whether           
*  a spot was matched or not, the affid date is going to be exploited           
*  as a flag.  The affid date, if present, will be set to a dummy               
*  compressed date (x'0001').  This will hopefully separate the                 
*  matched spots from the unmatched spots without having to break out           
*  all the different CHUNK entries.                                             
                                                                                
PBUYAFD  NTR1                                                                   
         AH    R6,=Y(BDELEM-BUYREC)                                             
         SR    R0,R0                                                            
                                                                                
PBUYAFD2 DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    XIT_04                                                           
         CLI   0(R6),X'10'                                                      
         BNE   *+10                                                             
         MVC   ADATE-AFFELEM(,R6),=X'0001'                                      
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PBUYAFD2                                                         
*&&                                                                             
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--STW# && RTW#)'                
*------------------------------ SAVE TWA -----------------------------*         
                                                                                
* save TWA into TEMPSTR                                                         
                                                                                
SAVETWA  DS    0H                                                               
         MVI   DMCB+8,PAGE00                                                    
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATWA,0,0                     
         B     XIT_04                                                           
         SPACE 2                                                                
*----------------------------- RESTORE TWA ---------------------------*         
                                                                                
* restore TWA from TEMPSTR                                                      
                                                                                
RSTRTWA  DS    0H                                                               
         MVI   DMCB+8,PAGE00       3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=H'18432'                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATWA,0                      
         B     XIT_04                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--CEDC#)'                       
*---------------- CHECK ESTIMATE HEADER'S DATE CHANGED ---------------*         
                                                                                
* Routine checks the C2STEL elements against the broadcast periods to           
*  see if the dates in the estimate header had changed since the C2STEL         
*  elements were last written to the file.                                      
* At entry,                                                                     
*   AIO = A(mkt-level PW record)                                                
                                                                                
CHKESTDT DS    0H                                                               
                                                                                
*                                                                               
** CHECK FIRST WEEK OF SCHEDULE **                                              
*                                                                               
         DS    0H                                                               
         LA    R3,BRDWKTAB         R3-->WEEK DATES OF SCHEDULE                  
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    R3,BRDMTHTB         USE PERIOD DATES IF ONE PERIOD               
         USING BCSTTABD,R3                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCDLO,C2STCODQ                                                  
         MVI   ELCDHI,C2STCODQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         BAS   RE,GTEL4                                                         
         BNE   CEDCXN                                                           
         USING C2STEL,R6                                                        
                                                                                
*                                                                               
         CLC   C2STSTAP,BCSSTART                                                
         BNE   CEDCXN                                                           
                                                                                
*                                                                               
** CHECK FINAL WEEK OF SCHEDULE **                                              
*                                                                               
         DS    0H                                                               
         CLI   BCSTTABQ(R3),XFF                                                 
         BE    *+12                                                             
         LA    R3,BCSTTABQ(R3)                                                  
         B     *-12                                                             
                                                                                
         DS    0H                  R3-->FINAL WEEK OF SCHEDULE                  
         ST    R6,FULL             HOLD ONTO (ADDR OF C2STEL)                   
         BAS   RE,NXTEL4                                                        
         BE    *-8                                                              
         L     R6,FULL             R6-->A(LAST C2STEL IN RECORD)                
*                                                                               
         CLC   C2STSTAP,BCSSTART                                                
         BNE   CEDCXN                                                           
                                                                                
*                                                                               
         B     CEDCXY                                                           
         DROP  R3,R6                                                            
                                                                                
*                                                                               
CEDCXN   DS    0H                                                               
         B     NO_04                                                            
                                                                                
CEDCXY   DS    0H                                                               
         B     YES_04                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--TSR_#''s)'                    
*--------------------------- TSAR INTERFACE --------------------------*         
         USING TSARD,R3                                                         
TSR_INI  DS    0H                  INITIALIZE     TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         XC    TSARD(TSARDL),TSARD                                              
         MVI   TSACTN,TSAINI                                                    
         LA    R0,STACREC           TSAR USED FOR STTN ACCUM TABLE              
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,PAGETSRQ      LOW TEMPSTR PAGE TO USE                     
         MVI   TSPAGN,NMPGTSRQ      ONE PAGE SHOULD BE ENOUGH                   
         MVI   TSKEYL,STACKEYL      KEY LENGTH                                  
         LA    R0,STACRECL                                                      
         STH   R0,TSRECL            (FIXED) RECORD LENGTH                       
         OI    TSINDS,TSIREUSE+TSIXTTWA                                         
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
*                                                                               
TSR_ADD  DS    0H                  ADD RECORD TO  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAADD                                                    
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
*                                                                               
TSR_RDH  DS    0H                  READ HIGH FROM TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
*                                                                               
TSR_WRT  DS    0H                  WRITE RECD TO  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAWRT                                                    
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
*                                                                               
TSR_GET  DS    0H                  GET RECD FROM  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAGET                                                    
         B     TSR_GO                                                           
*                                                                               
TSR_PUT  DS    0H                  PUT RECD INTO  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAPUT                                                    
         B     TSR_GO                                                           
*                                                                               
TSR_NXT  DS    0H                  NEXT RECD FROM TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSANXT                                                    
         B     TSR_GO                                                           
*                                                                               
TSR_SAV  DS    0H                  SAVE           TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSASAV                                                    
         MVI   TSPAGL,PAGETSRQ      LOW TEMPSTR PAGE TO USE                     
         MVI   TSPAGN,NMPGTSRQ      ONE PAGE SHOULD BE ENOUGH                   
         B     TSR_GO                                                           
*                                                                               
TSR_RES  DS    0H                  RESTORE        TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSARES                                                    
         MVI   TSPAGL,PAGETSRQ      LOW TEMPSTR PAGE TO USE                     
         MVI   TSPAGN,NMPGTSRQ      ONE PAGE SHOULD BE ENOUGH                   
         B     TSR_GO                                                           
*                                                                               
TSR_GO   DS    0H                                                               
         GOTO1 ATSAR,TSARD                                                      
         MVC   MYTSERRS,TSERRS     PASS BACK ERRORS                             
         CLI   TSERRS,0                                                         
         BE    YES_04                                                           
         B     NO_04                                                            
         DROP  R3                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--FCO#)'                        
*---------------------- FILL IN CLCOST OVERRIDES ---------------------*         
                                                                                
FILLCLC  DS    0H                                                               
         MVI   GOSUBN,GPR#         GET CS2 RECORD                               
         GOTO1 AGOSUB                                                           
*                                                                               
         MVI   ELCDLO,PWCLCCDQ                                                  
         MVI   ELCDHI,PWCLCCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
*                                                                               
         DS    0H                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
FCO022   DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    FCO029                                                           
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    FCO026                                                           
                                                                                
         CLC   ATCS2,=X'80000000'  IF CS2 HAS THIS VALUE,                       
         BNE   FCO026                                                           
                                                                                
         OI    ATFLAG2,ATF2CCOD     TURN ON OVERRIDE FLAG AND                   
         L     R6,AIO               GET CORRESPONDING CLCOST OVERRIDE           
         BAS   RE,GTEL4                                                         
         B     FCO024B                                                          
FCO024A  BAS   RE,NXTEL4                                                        
FCO024B  BNE   FCO026                                                           
                                                                                
         USING PWCLCEL,R6                                                       
         CLC   PWCLCWK,ATWSTART                                                 
         BNE   FCO024A                                                          
         MVC   ATAJBUY,PWCLCAMT                                                 
         DROP  R6                                                               
*                                                                               
FCO026   DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         B     FCO022                                                           
         DROP  R4                                                               
*                                                                               
FCO029   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_04                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--PPDS#)'                       
*------------------------- PROCESS PAID SPOTS ------------------------*         
* This routine process buys for paid spots.  The idea behind this is            
*  so that it helps determine whether broadcast months have any                 
*  paid spots at all.                                                           
* At entry,                                                                     
*   R2--SPOTBLOCK                                                               
*   R6--BUY RECORD                                                              
*---------------------------------------------------------------------*         
PPDS     DS    0H                                                               
         USING SBLOCKD,R2                                                       
         USING BUYRECD,R6                                                       
*                                                                               
         CLC   BUYMSTA(2),BMKT                                                  
         BNE   PPDSX                                                            
         NI    MISCFLG2,XFF-MF2BUY0Q     AT LEAST 1 BUY EXISTS                  
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(3),BUYMSTA+2   GET MSPACKED STATION                         
*        TM    BUYMSTA+2,X'F0'     IF CABLE STATION,                            
*        BNO   *+10                                                             
         CLI   MYCANADA,C'C'        CANADIAN?                                   
         BE    PPDS01               YES                                         
         CLI   BUYMSTA+2,X'E8'     IF CABLE STATION,                            
         BL    *+10                                                             
         NC    FULL(3),CBLSCMSK     KEEP SYSCODE & IGNORE NETWORK               
PPDS01   TM    BDSTAT2,X'30'       TRADE BUY?                                   
         BZ    *+8                                                              
         MVI   BDPURP,X'FE'         YEP, MAKE $$ GO UNDER CASH PRODUCT          
*                                                                               
         NI    MISCFLG3,XFF-MF3SSCHK  DON'T SKIP SPOT CHECK                     
         TM    BDCIND,X01             TEST FOR MINUS SPOT IN BUYLINE            
         BZ    *+8                     IF IT IS, SKIP #-OF-SPOTS TEST           
         OI    MISCFLG3,MF3SSCHK      SKIP SPOT CHECK                           
         DROP  R6                                                               
*                                                                               
         OI    SBEFLAG2,SBENOMIN   IGNORE MINUS SPOTS (SAVES TBL SPACE)         
         OI    SBEFLAG2,SBESPLBY   SPLIT BUY (IF CHUNKS NOT BIG ENOUGH)         
         MVI   SBESPOTS,SBESPAID   GET PAID SPOTS FIRST                         
*                                                                               
PPDS008G MVC   WORK,BRDWKTB2                                                    
         MVC   WORK+L'BRDWKTB2(1),SBNDATES+3                                    
         MVI   SBNDATES+3,1        ONE PERIOD                                   
         XC    BRDWKTB2,BRDWKTB2                                                
         GOTO1 DATCON,DMCB,(0,ESDATE),(2,BRDWKTB2),0                            
         GOTO1 DATCON,DMCB,(0,EEDATE),(2,BRDWKTB2+2),0                          
         OI    SBEFLAG,SBE2COS     CALL FOR SECOND COST DATA                    
         GOTO1 ASPOTBUY,DMCB,SBLOCK                                             
*                                                                               
PPDS008H L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
PPDS009  ICM   RE,15,SCNEXT                                                     
         BZ    PPDS009A                                                         
         MVC   SCGROSS2,SCGROSS                                                 
         MVC   SCNET2,SCNET                                                     
         LR    R5,RE                                                            
         B     PPDS009                                                          
         DROP  R5                                                               
*                                                                               
PPDS009A NI    SBEFLAG,XFF-SBE2COS     RESET SECOND COST FLAG                   
         OI    SBEFLAG,SBEA2COS                                                 
         GOTO1 ASPOTBUY,DMCB,SBLOCK                                             
*                                                                               
PPDS009B NI    SBEFLAG,XFF-SBEA2COS    RESET AFTER SECOND COST FLG              
         MVC   BRDWKTB2,WORK                                                    
         MVC   SBNDATES+3(1),WORK+L'BRDWKTB2                                    
         TM    SBEFLAG2,SBENOMIN   IF THIS FLAG NOT TURNED OFF,                 
         BNZ   PPDS08X                                                          
         MVI   MYERRCD,TBOFQ        WE ENCOUNTERED A TABLE OVERFLOW             
         B     PPDSX                                                            
*                                                                               
PPDS08X  L     R3,SBACHUNK                                                      
         USING SCHUNKD,R3                                                       
PPDS10   OC    SCNEXT,SCNEXT                                                    
         BZ    PPDS090                                                          
*                                                                               
         TM    MISCFLG3,MF3SSCHK   SKIP SPOT CHECK?                             
         BO    PPDS20              YEP!                                         
         ICM   R0,15,SCSPOTS       TEST IF THERE ARE ANY SPOTS                  
         BZ    PPDS30               IGNORE CHUNK IF THERE ARE NONE              
*                                                                               
PPDS20   MVC   DATE2,SCDATE                                                     
         L     RF,ALOCDATE                                                      
         BASR  RE,RF               FIND ENTRY WITHIN ACCUTAB                    
         OR    R4,R4               A(ENTRY) RETURNED IN  R4                     
         BZ    PPDS30               UNLESS IT COULD NOT FIND AN ENTRY           
*                                                                               
         USING ACCUTABD,R4                                                      
         OI    ATFLAG2,ATF2A1PS    THIS WEEK HAS AT LEAST 1 PAID SPOT           
*                                                                               
PPDS30   L     R3,SCNEXT                                                        
         B     PPDS10                                                           
         DROP  R3,R4                                                            
*                                                                               
** FINISH PROCESSING DATA IN CHUNKS AREA **                                     
*                                                                               
PPDS090  OC    SBACONT,SBACONT     ANY BUY CONTINUATION?                        
         BZ    PPDS100              NO, DONE WITH BUY RECORD                    
         B     PPDS008G            GO GET THE REST OF THE BUY                   
*                                                                               
** FINISH PROCESSING BUY RECORD **                                              
*                                                                               
PPDS100  L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
PPDS102  TM    ATFLAG,ATFSTTLQ     IF SKED ENTRY REACHED,                       
         BNO   *+12                 ALL MNTHS HAVE AT LEAST 1 PAID SPT,         
         MVI   SBMODE,SBSTOP        AND TELL SPOTIO TO STOP                     
         B     PPDS109                                                          
*                                                                               
         TM    ATFLAG,ATFMTTLQ     MONTH ENTRY?                                 
         BNO   PPDS106              NO, BUMP TO NEXT ENTRY                      
         TM    ATFLAG2,ATF2A1PS     YES, HAS AT LEAST 1 PAID SPOT?              
         BZ    PPDS109               NO, DON'T TELL SPOTIO TO STOP              
*                                                                               
PPDS106  LA    R4,ACCUTABQ(R4)                                                  
         B     PPDS102                                                          
*                                                                               
PPDS109  EQU   *                                                                
         DROP  R4                                                               
*                                                                               
PPDSX    B     XIT_04                                                           
         DROP  R2                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--PPR#)'                        
*--------------------------- PUT PW RECORD ---------------------------*         
                                                                                
* At entry,                                                                     
*   AIO = A(PW record)                                                          
                                                                                
PUTMYCS2 DS    0H                                                               
         L     RE,AIO                                                           
         ZICM  RF,(PWLEN-PWRECD)(RE),(3)                                        
         BZ    PPRX                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         L     R0,AMYC2REC                                                      
         LA    R1,L'MYPWREC                                                     
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2               CLEAR MY PW RECORD AREA                      
*                                                                               
         L     R0,AMYC2REC                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE PW RECORD INTO MY AREA                  
                                                                                
*                                                                               
PPRX     DS    0H                                                               
         B     XIT_04                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--GBT#)'                        
*------------------------ GET BROADCAST MONTHS -----------------------*         
                                                                                
GETBMTAB DS    0H                                                               
*                                                                               
** BROADCAST WEEKS **                                                           
*                                                                               
         XC    BRDWKTAB,BRDWKTAB     GENERATE WEEKS OF SCHEDULE                 
         XC    BRDWKTB2,BRDWKTB2                                                
         MVI   BYTE,C'W'                                                        
         GOTO1 AMOBILE,DMCB,(14,ESDATE),(4,BRDWKTB2),MOBINPAD,SPOTPROF          
         MVC   NUMWEEKS,DMCB       SAVE # OF DATE-PAIRS GENERATED               
         LA    R2,BRDWKTB2                                                      
         LA    R3,BRDWKTAB                                                      
         BAS   RE,BLDBTAB          BUILD B'CAST WEEK TABLE FOR MY USE           
*                                                                               
** BROADCAST MONTHS **                                                          
*                                                                               
         DS    0H                  GENERATE MONTHS OF SCHEDULE                  
         L     R2,AIO3             USE IO3 FOR MOBILE OUTPUT                    
         XC    BRDMTHTB,BRDMTHTB                                                
         LA    R3,BRDMTHTB                                                      
         MVI   BYTE,C'M'                                                        
         MVC   WORK(6),ESDATE      WORK = START DATE OF EST (EBCDIC)            
                                                                                
         ZIC   RE,NUMWEEKS         RE=# OF WEEKS GENERATED                      
         BCTR  RE,0                                                             
         MH    RE,=Y(BCSTTABQ)                                                  
         LA    RE,BRDWKTAB(RE)     RE-->LAST DATE PAIR                          
         USING BCSTTABD,RE                                                      
         CLI   BCSTTABQ(RE),XFF     SHOULD BE FOLLOWED BY X'FF'                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HALF,BCSEND         HALF = BRDCST WEEK END-DATE (CMPRSS)         
         DROP  RE                                                               
                                                                                
         TM    ESTFLAG,EFBILEST    IF THIS IS ONLY ONE PERIOD, CONSIDER         
         BZ    GBT10                THE ENTIRE PERIOD AS ONE MONTH              
         GOTO1 DATCON,DMCB,(0,WORK),(2,(R2)),0     START OF "MONTH"             
         MVC   2(2,R2),HALF                        END   OF "MONTH"             
         MVI   4(R2),XFF                           EOTABLE MARKER               
         BAS   RE,BLDBTAB          BUILD BRDMTHTB FOR MYSELF                    
         B     GBT20                                                            
*                                                                               
GBT10    GOTO1 DATCON,DMCB,(2,HALF),(0,WORK+6),0                                
                                                                                
         MVC   ESTOOWSD,SPOTPROF+8     SAVE PROFILE SETTING                     
         MVI   SPOTPROF+8,0            CLEAR IT TO GET B'CST MONTHS             
         GOTO1 AMOBILE,DMCB,(5,WORK),(0,(R2)),MOBINPAD,SPOTPROF                 
         MVC   SPOTPROF+8(1),ESTOOWSD  RESTORE PROFILE SETTINGS                 
                                                                                
GBT014   DS    0H                                                               
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW      OUT-OF-WEEK PW BILLING ESTIMATE:             
         BZ    GBT014X                                                          
                                                                                
*&&                                                                             
         CLC   2(2,R2),BRDWKTAB+2  B'CST MNTH END VS. 1ST WK'S END DATE         
         BNL   GBT014X                                                          
         LA    R2,4(R2)            IF LOWER, BUMP TO NEXT MONTH                 
         CLI   0(R2),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     GBT014              GO BACK AND CHECK AGAIN                      
GBT014X  EQU   *                                                                
                                                                                
         BAS   RE,BLDBTAB          BUILD BRDMTHTB FOR MYSELF                    
         B     GBT20                                                            
*                                                                               
GBT20    TM    MISCFLG1,MF1KYCHG   IF ACTION<>SELECT,                           
         BZ    GBTX                 AND KEY WASN'T CHANGED, EXIT                
GBT30    MVC   MNTHMRKS,BRDMTHTB    ELSE, SET MONTH-MARK TO START DATE          
                                                                                
GBTX     J     XIT                                                              
                                                                                
                                                                                
         DS    0H                  BLD ACTUAL B'CAST TABLES FOR MYSELF          
BLDBTAB  NTR1                                                                   
         USING BCSTTABD,R3                                                      
BLDBTABA CLI   0(R2),XFF           R2-->MOBILE OUTPUT, R3-->MY TABLE            
         BE    BLDBTABX                                                         
                                                                                
         MVC   BCSSTART(4),0(R2)    MOVE IN START & END DATES                   
         CLI   BYTE,C'M'                                                        
         BNE   *+8                                                              
         OI    BCSFLAG,BCSFMXQ     TURN FLAG ON FOR MONTH TABLE                 
                                                                                
         LA    R2,4(R2)                                                         
         LA    R3,BCSTTABQ(R3)                                                  
         B     BLDBTABA                                                         
         DROP  R3                                                               
*                                                                               
BLDBTABX MVI   0(R3),XFF           EOTABLE MARKER                               
         J     XIT                 EXIT BLDBTAB ROUTINE                         
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--BPT#)'                        
*-------------------------- BUILD CS2 TABLE -------------------------*          
                                                                                
BPWTAB   DS    0H                                                               
         MVI   ELCODE,C2STCODQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
                                                                                
         LA    R3,BRDWKTAB         R3-->WEEK DATES OF SCHEDULE                  
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    R3,BRDMTHTB         USE PERIOD DATES IF ONE PERIOD               
         LA    R4,PWTAB                                                         
         XC    PWTAB,PWTAB                                                      
                                                                                
         USING BCSTTABD,R3                                                      
BPT10    MVC   TEMPPW,OC2PCT       SET DEFAULT                                  
         L     R6,AIO               IN CASE NOT FOUND IN RECORD                 
         BRAS  RE,GETEL                                                         
         B     BPT20B                                                           
BPT20A   BRAS  RE,NEXTEL                                                        
BPT20B   BNE   BPT30                                                            
                                                                                
         USING C2STEL,R6                                                        
         CLC   C2STSTAP,BCSSTART   GET ELEM FOR CORRESPONDING WEEK              
*&&DO                                                                           
         BNE   BPT20A                                                           
*&&                                                                             
         BL    BPT20A                                                           
******** MVC   TEMPPW,C2STPCT       AND GET CS2 FROM IT                         
         MVC   TEMPPW,C2STFCTR      AND GET CS2 FROM IT                         
         DROP  R6                                                               
                                                                                
BPT30    MVC   0(4,R4),BCSSTART    DATE FOR PW TABLE                            
         MVC   4(4,R4),TEMPPW      CS2 FOR PW TABLE                             
         LA    R4,8(R4)            BUMP POINTER IN PWTAB                        
         LA    R3,BCSTTABQ(R3)     BUMP POINTER IN BRDWKTAB/BRDMTHTB            
         CLI   0(R3),XFF                                                        
         BNE   BPT10                                                            
         DROP  R3                                                               
                                                                                
         MVI   0(R4),XFF                                                        
         J     XIT                                                              
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--IAC#)'                        
*----------------------- INITIALIZE ACCUM TABLE ----------------------*         
                                                                                
*  FIRST WE GET DATES OF ALL WEEKS IN PERIOD                                    
*   THEN WE FILL IN THE PROFIT WITHIN %                                         
                                                                                
INITACTB DS    0H                                                               
*                                                                               
** INITIALIZE TABLES **                                                         
*                                                                               
         L     RE,AACCUTAB                                                      
         LA    RF,L'ACCUTAB                                                     
         XCEF                                                                   
         MVI   BYTE,ATFBILAJ+ATFSTTLQ   SET UP AN ENTRY FOR TOTALS W/           
         L     R4,AACCUTAB                   BILLING ADJ                        
         USING ACCUTABD,R4                                                      
IAC30    GOTO1 DATCON,DMCB,(0,ESDATE),(2,ATMSTART)                              
         GOTO1 DATCON,DMCB,(0,EEDATE),(2,ATMEND)                                
         MVC   ATWSTART,ATMSTART                                                
         MVC   ATWEND,ATMEND                                                    
         MVC   ATFLAG,BYTE                                                      
         AHI   R4,ACCUTABQ                                                      
         TM    BYTE,ATFBILAJ                                                    
         BZ    *+12                                                             
         NI    BYTE,XFF-ATFBILAJ        AND ONE FOR JUST TOTALS                 
         B     IAC30                                                            
IAC50    MVC   0(3,R4),=X'FFFFFF'       NO ENTRIES AT THIS MOMENT               
         DROP  R4                                                               
*                                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         JZ    XIT                                                              
         MVI   GOSUBN,TSR_INI#            INITIALIZE STATION                    
         GOTO1 AGOSUB                      ACCUMULATOR TABLE                    
         JE    XIT                                                              
         DC    H'0'                                                             
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--CPRTF#)'                      
*----------------------- CHANGE FIELD PROTECTION ---------------------*         
                                                                                
*  Changes the protection of those non-key fields where data is                 
*   enterrable.                                                                 
                                                                                
CPRTFLD  DS    0H                                                               
                                                                                
         SR    R0,R0               WANT R0=ZERO IN THIS WHOLE ROUTINE           
*                                                                               
** TOTAL LINE                                                                   
*                                                                               
         MVC   LNCNTDWN,NUMLNDSP                                                
         MVI   BYTE,C'N'           AM1STREC HASN'T BEEN CHANGED                 
         LA    R2,C2MSTTLH         R3=END OF LOOP CONDITION                     
         USING MSLDSECT,R2                                                      
         TM    LOCKFLAG,LKFPWQ     USER WANTS CS2 LOCK?                         
         BNZ   CPF05                                                            
         NI    MSLCCOSH+1,X'FF'-X'20'                                           
         NI    MSLCOS2H+1,X'FF'-X'20'                                           
         B     CPFX                                                             
CPF05    OI    MSLCCOSH+1,X'20'                                                 
         OI    MSLCOS2H+1,X'20'                                                 
         B     CPFX                                                             
*&&DO                                                                           
*****    MVC   DATE4,MNTHMRKS                                                   
*****    L     RF,ALC1WMTH         GET R4-->1ST MONTH ON DISPLAY                
*****    BASR  RE,RF                                                            
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
CPF10    DS    0H                                                               
         LA    R7,MSLCCOSH                                                      
         LA    R6,PRTFLD           THE FOLLOWING IMPLIES PROTECTION:            
         TM    LOCKFLAG,LKFUPLKQ       USER WANTS PW LOCK                       
         BO    CPF10B                                                           
         TM    ATFLAG,ATFNOSPT         NO SPOTS                                 
         BO    CPF10B                                                           
         TM    ATFLAG2,ATF2OOWP        IF MTH CLEARED W/ OOW PW PAID$           
         BO    CPF10B                                                           
         TM    ATFLAG,ATFSKMTH         IF MONTH OR SKED ENTRY,                  
         BZ    CPF10A                                                           
         OC    ATACBUY,ATACBUY          AND BUY IS ZERO COST                    
         BZ    CPF10B                                                           
                                                                                
CPF10A   DS    0H                   OTHERWISE, UNPROTECT FIELD                  
         LA    R6,UPRTFLD                                                       
         CLI   BYTE,C'Y'           IF AM1STREC WASN'T RESETTED,                 
         BE    CPF10B                                                           
         ST    R7,AM1STREC          SET AM1STREC TO THIS FIELD                  
         MVI   BYTE,C'Y'                                                        
                                                                                
CPF10B   DS    0H                  CLIENT COST                                  
         OI    6(R7),X80                                                        
         EX    R0,0(R6)             CHANGE TO PROTECT/UNPROTECT                 
                                                                                
CPF12    DS    0H                  NOW DO CS2                                   
         LA    R7,MSLCOS2H                                                      
         LA    R6,PRTFLD           THE FOLLOWING IMPLIES PROTECTION:            
         TM    LOCKFLAG,LKFUPLKQ       USER WANTS PW LOCK                       
         BO    CPF12A                                                           
         TM    ATFLAG,ATFBILAJ         IF IT'S A BILL ADJ LINE                  
         BO    CPF12A                                                           
         TM    ATFLAG,ATFNOSPT         NO SPOT IN WEEK/MONTH/SKED               
         BO    CPF12A                                                           
         OC    ATACBUY,ATACBUY         BUY IS ZERO COST                         
         BZ    CPF12A                                                           
         TM    ATFLAG2,ATF2OOWP        IF OOW PW PAID$ IS BEING USED            
         BO    CPF12A                                                           
         LA    R6,UPRTFLD                                                       
                                                                                
CPF12A   DS    0H                  CS2 FIELD                                    
         OI    6(R7),X80                                                        
         EX    R0,0(R6)             CHANGE TO PROTECT/UNPROTECT                 
*                                                                               
CPF14    DS    0H                 BUMP POINTERS                                 
         CLI   LNCNTDWN,0          CHECK IF WE NEED TO FIRST                    
         BE    CPF20                NOPE DON'T NEED TO                          
                                                                                
         LA    R2,MSLLENQ(R2)      TWA POINTER                                  
         CR    R2,R3               IT HAD BETTER NOT                            
         BNH   *+6                                                              
         DC    H'0'                 PASS SCHED LINE ON SCREEN                   
                                                                                
         LA    R4,ACCUTABQ(R4)     ACCUTAB POINTER                              
         TM    ATFLAG2,ATF2DPLY                                                 
         BZ    *-8                                                              
                                                                                
         ZIC   R1,LNCNTDWN         DECREMENT LINE COUNT-DOWN                    
         BCTR  R1,0                                                             
         STC   R1,LNCNTDWN                                                      
         OR    R1,R1               HAVE WE DONE ALL LINES ON DISPLAY?           
         BNZ   CPF10                NOPE, GO BACK AND DO THEM                   
                                                                                
         LA    R6,PRTFLD            YES, NOW GO PROTECT UNUSED LINES            
CPF16    CR    R2,R3               ARE WE AT SCREEN'S SCHED LINE?               
         BE    CPF18                YES WE ARE                                  
         LA    R7,MSLCCOSH                                                      
         OI    6(R7),X80                                                        
         EX    R0,0(R6)            CLIENT COST                                  
         LA    R7,MSLCOS2H                                                      
         OI    6(R7),X80                                                        
         EX    R0,0(R6)            CS2                                          
         LA    R2,MSLLENQ(R2)                                                   
         B     CPF16                                                            
*                                                                               
CPF18    DS    0H                  READY TO DO SCHED LINE ON SCREEN             
         L     RF,ALOCSKED         GET R4-->SKED ENTRY OF ACCUTAB               
         BASR  RE,RF                                                            
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ ENTRY                     
         B     CPF10               DO PROTECTION FOR SKED TOTALS                
*                                                                               
** GOALS-RELATED FIELDS **                                                      
*                                                                               
CPF20    DS    0H                                                               
         LA    R6,PRTFLD           THE FOLLOWING IMPLIES PROTECTION:            
         TM    MISCFLG1,MF1GOL0Q      GOALS EXIST ON FILE                       
         BZ    CPF22                                                            
         LA    R6,UPRTFLD          OTHERWISE UNPROTECT FIELD                    
                                                                                
CPF22    DS    0H                                                               
         LA    R7,C2MSACGH          SCHEDULE'S GOAL                             
         OI    6(R7),X80                                                        
         EX    R0,0(R6)                                                         
         LA    R7,C2MTGRPH          TOTAL GRP                                   
         OI    6(R7),X80                                                        
         EX    R0,0(R6)                                                         
*                                                                               
         DS    0H                  MORE OTHER FIELDS                            
         LA    R6,PRTFLD                                                        
         TM    MISCFLG2,MF2BUY0Q                                                
         BO    CPF30                                                            
         LA    R6,UPRTFLD                                                       
*                                                                               
CPF30    DS    0H                                                               
****     LA    R7,C2MLOCKH          'XFR MKT LOCKED COSTS' FIELD                
****     OI    6(R7),X80                                                        
****     EX    R0,0(R6)                                                         
         LA    R7,C2MRQIDH          'STATION LOCK' FIELD                        
         OI    6(R7),X80                                                        
         EX    R0,0(R6)                                                         
         B     CPF50                                                            
*                                                                               
CPF50    DS    0H                                                               
         B     CPFX                                                             
PRTFLD   OI    1(R7),X20           SHOULD NOT FALL THROUGH AND                  
UPRTFLD  NI    1(R7),XFF-X20        EXECUTE THESE INSTRUCTIONS                  
*&&                                                                             
CPFX     DS    0H                                                               
         J     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--FOWP#)'                       
*------------------- FILL IN OUT-OF-WEEK PAID FLAGS ------------------*         
                                                                                
* If OOW estimate paid dollars element exist, turn on the OOW PW flag           
*  for the month in ACCUTAB.                                                    
* At entry,                                                                     
*   AIO = A(PW record)                                                          
                                                                                
FILLOOWP DS    0H                                                               
         L     R6,AIO                                                           
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         MVI   ELCODE,PWOOWCDQ                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         BRAS  RE,GETEL            DO WE HAVE OOW EST PAID $ ELEM?              
         BNE   FOWP039              NOPE, EXIT NOW                              
         USING PWOOWEL,R6                                                       
                                                                                
*                                                                               
FOWP020  DS    0H                  R6-->PWOOWEL ELEMENT                         
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
FOWP022  DS    0H                  R4-->1ST WEEK OF B'CAST MONTH                
         TM    ATFLAG,ATFSTTLQ     IF SKED LINE, GET NEXT ELEM                  
         BO    FOWP033                                                          
*                                                                               
         DS    0H                  CONVERT DATE TO BINARY FORM                  
         GOTO1 DATCON,DMCB,(X'02',ATMSTART),(0,MYDATE6),0                       
         GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(X'03',FULL),0                        
*                                                                               
         CLC   PWOOWYM,FULL        DO THE MONTHS MATCH?                         
         BNE   FOWP030              NO, FAST FORWARD TO NEXT B'CST MNTH         
*                                                                               
FOWP026  DS    0H                                                               
         OI    ATFLAG2,ATF2OOWP           TURN ON FLAG INDICATING               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BM    FOWP026X                                                         
         LA    R4,ACCUTABQ(R4)                                                  
         B     FOWP026                                                          
FOWP026X EQU   *                                                                
         B     FOWP033                                                          
*                                                                               
FOWP030  DS    0H                                                               
         LA    R4,ACCUTABQ(R4)     FAST FORWARD TO NEXT B'CAST MONTH            
         TM    ATFLAG,ATFMTTLQ      MONTH LINE YET?                             
         BZ    *-8                   NOPE, KEEP BUMPING                         
         LA    R4,2*ACCUTABQ(R4)    BUMP PAST MONTHLY TOTALS LINE               
         B     FOWP022                                                          
*                                                                               
FOWP033  DS    0H                                                               
         BRAS  RE,NEXTEL           GET NEXT OOW EST PAID$ ELEMENT               
         BE    FOWP020                                                          
         DROP  R4,R6                                                            
FOWP039  EQU   *                                                                
                                                                                
*                                                                               
FOWPX    DS    0H                                                               
         J     XIT                                                              
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--OKUPD#)'                      
*------------------------ OKAY TO UPDATE FILE? -----------------------*         
                                                                                
*  Routine attempts to catch any potential errors during execution.             
*   Motivation behind this is to catch them BEFORE the program updates          
*   the file instead of during.                                                 
                                                                                
OKUPDFIL DS    0H                                                               
*                                                                               
** CHECK STATION ACCUMULATOR TABLE **                                           
*                                                                               
         DS    0H                                                               
         TM    MISCFLG2,MF2BUY0Q   ANY BUYS AT ALL?                             
         BO    OKUPSTX              NOPE, DON'T CHECK STTN ACCUM TABLE          
                                                                                
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         OC    TSPRECN,TSPRECN     ANY ENTRIES IN STTN ACCUM TABLE?             
         BZ    OKUPSTX              NOPE, DON'T CHECK STTN ACCUM TABLE          
         DROP  R1                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         ZICM  R4,TSARBLK+((TSAREC+1)-TSARD),(7)                                
         BZ    OKUPDXN                                                          
                                                                                
*                                                                               
         USING STACRECD,R4                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVI   GOSUBN,TSR_RDH#                                                  
*                                                                               
OKUPST10 DS    0H                  LOOP THROUGH STTN ACCUM TABLE                
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF      IF WE REACHED THIS,                         
         BZ    OKUPSTX               STTN ACCUM TABLE SEEMS OKAY                
         CLI   MYTSERRS,0           IF WE GET ANY OTHER ERRORS,                 
         BE    OKUPDXN               SOMETHING'S WRONG                          
         OC    STACSTA,STACSTA      IF STATION CODE IS NULLS,                   
         BZ    OKUPDXN               SOMETHING'S WRONG                          
         MVI   GOSUBN,TSR_NXT#                                                  
         B     OKUPST10                                                         
*                                                                               
OKUPSTX  EQU   *                                                                
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
OKUPDXY  DS    0H                                                               
         J     YES                                                              
*                                                                               
OKUPDXN  DS    0H                                                               
         J     NO                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--MISC STUFF)'                  
*--------------------- SUBR04 MISCELLANEOUS STUFF --------------------*         
                                                                                
GTEL4    AH    R6,DATADISP                                                      
FRSTEL4  CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                  RETURN CC NOT EQUAL                          
         CLI   ELCDLO,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLI   ELCDHI,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLC   ELCDLO,ELCDHI                                                    
         BHR   RE                  RETURN CC NOT EQUAL                          
         B     NXTEL42                                                          
NXTEL4   CLI   0(R6),0                                                          
         BE    NXTEL4X                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NXTEL42  CLI   0(R6),0                                                          
         BE    NXTEL4X                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NXTEL4                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NXTEL4                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NXTEL4X  LTR   RB,RB                                                            
         BR    RE                                                               
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBR04--LTORG && CONSTANTS)'          
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR04L  EQU   *-SUBR04                                                         
         DS    0CL(X'1000'-SUBR04L+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBRXM)'                              
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave T21759 entirely and displays a message go through           
*  this routine.                                                                
                                                                                
*&&DO                                                                           
SUBRXMQ  EQU   ((((*-T21759)/4096)+1)*4096)                                     
*&&                                                                             
SUBRXMQ  EQU   ((((*-T21759)/X'100')+1)*X'100')                                 
                                                                                
         ORG   T21759+SUBRXMQ                                                   
XMSGRTN  NMOD1 0,**3EXM**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         XC    CONHEAD,CONHEAD     CLEAR THE WAY FOR THE MESSAGE                
                                                                                
         CLI   0(R1),C'E'          EXIT W/ AN ERROR MSG                         
         BE    XMERR                                                            
         CLI   0(R1),C'W'          EXIT W/ A WARNING MSG                        
         BE    XMWRN                                                            
         CLI   0(R1),C'I'          EXIT W/ AN INFO  MSG                         
         BE    XMINF                                                            
         DC    H'0'                                                             
                                                                                
                                                                                
XIT_XM   XIT1                                                                   
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBRXM-ERR MSGS)'                     
*--------------------------- ERROR MESSAGES --------------------------*         
                                                                                
* At entry, R2-->field in error.                                                
                                                                                
XMERR    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         OI    MISCFLG1,MF1ERRQ                                                 
         OI    6(R2),X'81'         FIELD IN ERROR IS MODIFIED FOR NEXT          
         CLI   MYERRCD,PWNAVQ                                                   
         BE    XMERR2                                                           
         MVI   GOSUBN,STI#         SAVE OFF TIA                                 
         GOTO1 AGOSUB                                                           
                                                                                
XMERR2   DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   MYERRCD,ERRX#                                                    
         BL    XMERRGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         MVC   CONHEAD(9),=C'**ERROR**'                                         
         LA    R1,CONHEAD+10                                                    
         CLI   MYERRCD,ERRX2#                                                   
         BL    XMERRGO                                                          
         DC    H'0'                                                             
                                                                                
XMERRGO  DS    0H                                                               
         CLI   MYERRCD,0                                                        
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   MYERRCD,XMERRQ                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,MYERRCD          BRANCH OFF TO SET ERROR MESSAGE              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMERR00(RF)                                                      
                                                                                
MFLDQ    EQU   ((XMERR01-XMERR00)/4)+1                                          
IFLDQ    EQU   ((XMERR02-XMERR00)/4)+1                                          
INVOQ    EQU   ((XMERR03-XMERR00)/4)+1                                          
DUPOQ    EQU   ((XMERR04-XMERR00)/4)+1                                          
IOPDQ    EQU   ((XMERR05-XMERR00)/4)+1                                          
MPCTQ    EQU   ((XMERR06-XMERR00)/4)+1                                          
NLKSTAQ  EQU   ((XMERR07-XMERR00)/4)+1                                          
EAJB0Q   EQU   ((XMERR08-XMERR00)/4)+1                                          
BPNLQ    EQU   ((XMERR09-XMERR00)/4)+1                                          
BAJZEROQ EQU   ((XMERR10-XMERR00)/4)+1                                          
ICCOVRDQ EQU   ((XMERR11-XMERR00)/4)+1                                          
EMKTQ    EQU   ((XMERR12-XMERR00)/4)+1                                          
EAJBPWQ  EQU   ((XMERR13-XMERR00)/4)+1                                          
NOACBQ   EQU   ((XMERR14-XMERR00)/4)+1                                          
NMODLCKQ EQU   ((XMERR15-XMERR00)/4)+1                                          
NMODPLKQ EQU   ((XMERR16-XMERR00)/4)+1                                          
NLKNTXQ  EQU   ((XMERR17-XMERR00)/4)+1                                          
NPOLQ    EQU   ((XMERR18-XMERR00)/4)+1                                          
PWNAVQ   EQU   ((XMERR19-XMERR00)/4)+1                                          
TBOFQ    EQU   ((XMERR24-XMERR00)/4)+1                                          
SPDATQ   EQU   ((XMERR25-XMERR00)/4)+1                                          
BHDATQ   EQU   ((XMERR26-XMERR00)/4)+1                                          
NCOWPQ   EQU   ((XMERR27-XMERR00)/4)+1                                          
IPRD2Q   EQU   ((XMERR28-XMERR00)/4)+1                                          
XCUERQ   EQU   ((XMERR29-XMERR00)/4)+1                                          
                                                                                
OUTRNGEQ EQU   ((XMERR20-XMERR00)/4)+1                                          
OUTRNG2Q EQU   ((XMERR21-XMERR00)/4)+1                                          
EHIPWQ   EQU   ((XMERR22-XMERR00)/4)+1                                          
ELOPWQ   EQU   ((XMERR23-XMERR00)/4)+1                                          
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     INVO                INVALID OPTION KEYWORD                       
XMERR04  B     DUPO                DUPLICATE OPTION SPECIFIED                   
XMERR05  B     IOPD                INVALID OPTION DATA VALUE                    
XMERR06  B     MPCT                MISSING CS2                                  
XMERR07  B     NLKSTA              CAN'T LOCK ON STATION LEVEL                  
XMERR08  B     EAJB0               CLCOST CAN'T BE 0 WHEN WIMCOST<>0            
XMERR09  B     BPNL                BUY/PW NOT LOCKED YET                        
XMERR10  B     BAJZERO             ONLY VALID INPUT FOR BLL ADJ IS 0            
XMERR11  B     ICCOVRD             INVALID OVVERID INPUT                        
XMERR12  B     EMKT                INVALID MARKET CODE                          
XMERR13  B     EAJBPW              CAN'T ENTER CLCOST & CS2 AT SAME             
XMERR14  B     NOACB               CAN'T HAVE CLCOST W/O WIMCOST                
XMERR15  B     NMODLCK             CAN'T LOCK/UNLOCK BUY AND MODIFY             
XMERR16  B     NMODPLK             CAN'T LOCK/UNLOCK PW  AND MODIFY             
XMERR17  B     NLKNTX              CAN'T XFR COSTS WHEN TAX=NO                  
XMERR18  B     NPOL                PRODUCT CAN'T BE POL                         
XMERR19  B     PWNAV               PW FUNCTIONS NOT AVAIL TO BUY SVCE           
XMERR24  B     TBOF                TABLE OVERFLOW.  PLEASE CONTACT DDS          
XMERR25  B     SPDAT               SPOT DATE (&T) NOT IN ESTIMATE PERIO         
XMERR26  B     BHDAT               YR/MTH OF SERVICE (&T) NOT IN ESTIMA         
XMERR27  B     NCOWP               MAY NOT CHANGE CS2 IN OOWR ESTIMA            
XMERR28  B     IPRD2               INVALID PRODUCT CODE &T                      
XMERR29  B     XCUER               EXECUTION ERROR AT X&T.  PLS ... DDS         
ERRX#    EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERR20  B     OUTRNGE                                                          
XMERR21  B     OUTRNG2                                                          
XMERR22  B     EHIPW                                                            
XMERR23  B     ELOPW                                                            
ERRX2#   EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERRQ   EQU   ((*-XMERR00)/4)+1                                                
         EJECT                                                                  
                                                                                
MFLD     MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
IFLD     MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
INVO     MVC   MSGNUM2,=H'206'     INVALID OPTION KEYWORD                       
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
DUPO     MVC   MSGNUM2,=H'208'     DUPLICATE OPTION SPECIFIED                   
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IOPD     MVC   MSGNUM2,=H'209'     INVALID OPTION DATA VALUE                    
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
MPCT     MVC   MSGNUM2,=H'900'          MISSING COST2%                          
         B     ERRGTXT                                                          
*                                                                               
NLKSTA   MVC   MSGNUM2,=AL2(SE#NLSTL)   CAN'T LOCK ON STATION LEVEL             
         B     ERRGTXT                                                          
*                                                                               
EAJB0    MVC   MSGNUM2,=AL2(SE#CLCN0)   CLCOST NO 0 WHEN WIMCOST<>0             
         B     ERRGTXT                                                          
*                                                                               
BPNL     MVC   MSGNUM2,=AL2(901)        BUY/C2 NOT LOCKED YET                   
         B     ERRGTXT                                                          
*                                                                               
BAJZERO  DS    0H                                                               
         MVC   MSGNUM2,=AL2(SE#VIBA0)   ONLY VALID INPUT FOR BLL ADJ 0          
         B     ERRGTXT                                                          
*                                                                               
ICCOVRD  DS    0H                                                               
         MVC   MSGNUM2,=AL2(SE#IOVRD)   INVALID OVERRIDE INPUT                  
         B     ERRGTXT                                                          
*                                                                               
EMKT     MVC   MSGNUM2,=AL2(SE#INVMK)   INVALID MARKET CODE                     
         B     ERRGTXT                                                          
*                                                                               
EAJBPW   MVC   MSGNUM2,=AL2(SE#NCPSM)   CAN'T ENTER CLCOST & CS2                
         B     ERRGTXT                                                          
*                                                                               
NOACB    MVC   MSGNUM2,=AL2(SE#NCWOW)   CAN'T HAVE CLCOST W/O WIMCOST           
         B     ERRGTXT                                                          
*                                                                               
NMODLCK  MVC   MSGNUM2,=AL2(SE#NMBLK)   CAN'T LOCK/UNLOCK BUY & MODFY           
         B     ERRGTXT                                                          
*                                                                               
NMODPLK  MVC   MSGNUM2,=AL2(SE#NMPLK)   CAN'T LOCK/UNLOCK PW  & MODFY           
         B     ERRGTXT                                                          
*                                                                               
NLKNTX   MVC   MSGNUM2,=AL2(SE#NXTXN)   CAN'T XFR COSTS WHEN TAX=NO             
         B     ERRGTXT                                                          
*                                                                               
NPOL     MVC   MSGNUM2,=AL2(SE#NOPOL)   PRODUCT CAN'T BE POL                    
         B     ERRGTXT                                                          
*                                                                               
PWNAV    MVC   MSGNUM2,=AL2(SE#PWNAV)   PW FUNCTIONS NOT AVAILABLE              
         B     ERRGTXT                                                          
*                                                                               
TBOF     DS    0H                  TABLE OVERFLOW.  PLEASE CONTACT DDS          
         MVC   MSGNUM2,=H'65'                                                   
         MVI   MSGSYS,GTGENSYS                                                  
         LA    R0,C2MMKTH                                                       
         ST    R0,ACURFORC                                                      
         B     ERRGTXT                                                          
*                                                                               
SPDAT    DS    0H                  &T SPOT NOT IN ESTIMATE PERIOD               
         MVC   MSGNUM2,=AL2(SE#SPDT2)                                           
         LA    R0,C2MMKTH                                                       
         ST    R0,ACURFORC                                                      
         B     ERRGTXT                                                          
*                                                                               
BHDAT    DS    0H                  YR/MTH OF SERVICE (&T) NOT IN ESTIMA         
         MVC   MSGNUM2,=AL2(SE#YMNEP)                                           
         LA    R0,C2MMKTH                                                       
         ST    R0,ACURFORC                                                      
         B     ERRGTXT                                                          
*                                                                               
NCOWP    DS    0H                  MAY NOT CHANGE CS2 IN OOWR ESTIMA            
         MVC   MSGNUM2,=AL2(SE#NCOWP)                                           
         B     ERRGTXT                                                          
*                                                                               
IPRD2    DS    0H                  INVALID PRODUCT CODE &T                      
         MVC   MSGNUM2,=AL2(SE#INVP2)                                           
         B     ERRGTXT                                                          
*                                                                               
XCUER    DS    0H                  INVALID PRODUCT CODE &T                      
         MVC   MSGNUM2,=AL2(GE$XCUER)                                           
         MVI   MSGSYS,GTGENSYS                                                  
         L     R0,ALOCERR                                                       
         S     R0,BASE1                                                         
         BP    *+6                                                              
         DC    H'0'                                                             
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL+1,MYTEXT+1,L'FULL-1,=C'TOG'                     
         ICM   R0,15,DMCB+16                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R0,MYTEXT+0                                                      
         B     ERRGTXT                                                          
*                                                                               
OUTRNGE  LR    R3,R1                                                            
         MVC   0(32,R3),=C'COS2 NOT IN RANGE (0.000001 ... '                    
         AHI   R3,32                                                            
         L     RE,=A(MAXPWQ)                                                    
         EDIT  (RE),(8,0(R3)),6,ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R3,R0                                                            
         MVI   0(R3),C')'                                                       
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
OUTRNG2  LR    R3,R1                                                            
         MVC   0(31,R3),=C'CLCOST MAKES COS2 OUT OF RANGE '                     
         MVC   31(14,R3),=C'(0.000001 ... '                                     
         AHI   R3,45                                                            
         L     RE,=A(MAXPWQ)                                                    
         EDIT  (RE),(8,0(R3)),6,ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R3,R0                                                            
         MVI   0(R3),C')'                                                       
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
EHIPW    DS    0H                                                               
         LR    R3,R1                                                            
         MVC   0(34,R3),=C'VALUE MAKES CS2 GREATER THAN MAX ('                  
         L     RE,=A(MAXPWQ)                                                    
         EDIT  (RE),(7,34(R3)),5,ALIGN=LEFT                                     
         AR    R3,R0                                                            
         MVI   34(R3),C')'                                                      
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
ELOPW    DS    0H                                                               
         LR    R3,R1                                                            
         MVC   0(34,R3),=C'VALUE MAKES CS2 SMALLER THAN MIN ('                  
         L     RE,=A(MINPWQ)                                                    
         EDIT  (RE),(7,34(R3)),5,ALIGN=LEFT                                     
         AR    R3,R0                                                            
         MVI   34(R3),C')'                                                      
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
         EJECT                                                                  
ERRGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMERR        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         MVI   ERROR,0                                                          
*                                                                               
ERREXIT  DS    0H                                                               
         LR    R0,R2               SAVE DISPL OF FIELD                          
         S     R0,ATWA                                                          
         STH   R0,PRVFDSP                                                       
         OC    ACURFORC,ACURFORC   WANT TO FORCE CURSOR ELSEWHERE?              
         BZ    *+8                                                              
         L     R2,ACURFORC          YEP                                         
                                                                                
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBRXM-WRN MSGS)'                     
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* At entry, R2-->C2MMEDH.                                                       
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVI   GOSUBN,STI#         SAVE OFF TIA                                 
         GOTO1 AGOSUB                                                           
                                                                                
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   MYWRNCD,WRNX#                                                    
         BL    XMWRNGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         CLI   MYWRNCD,WRNX2#                                                   
         BL    XMWRNGO                                                          
         DC    H'0'                                                             
                                                                                
XMWRNGO  DS    0H                                                               
         CLI   MYWRNCD,0                                                        
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   MYWRNCD,XMWRNQ                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,MYWRNCD          BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMWRN00(RF)                                                      
                                                                                
WTOPQ    EQU   ((XMWRN01-XMWRN00)/4)+1                                          
WBOTQ    EQU   ((XMWRN02-XMWRN00)/4)+1                                          
WECSTQ   EQU   ((XMWRN03-XMWRN00)/4)+1                                          
RSLMKTQ  EQU   ((XMWRN04-XMWRN00)/4)+1                                          
                                                                                
XMWRN00  DS    0H                                                               
XMWRN01  B     WTOP                TOP OF SCHEDULE                              
XMWRN02  B     WBOT                BOTTOM OF SCHEDULE                           
XMWRN04  B     RSLMKT              BUY HAS BEEN CHANGED, RE-STTN LK MKT         
WRNX#    EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRN03  B     WECST               EXCEEDED CAPACITY OF STATION TABLE           
WRNX2#   EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRNQ   EQU   ((*-XMWRN00)/4)+1                                                
         EJECT                                                                  
WTOP     MVC   MSGNUM2,=AL2(SW#TOPSK)   TOP OF SCHEDULE                         
         B     WRNGTXT                                                          
*                                                                               
WBOT     MVC   MSGNUM2,=AL2(SW#BOTSK)   BOTTOM OF SCHEDULE                      
         B     WRNGTXT                                                          
*                                                                               
RSLMKT   DS    0H                  BUY HAS BEEN CHANGED, RE-STTN LK MKT         
         MVC   MSGNUM2,=AL2(SW#RSLMK)                                           
         LA    R0,C2MRQIDH                                                      
         ST    R0,ACURFORC                                                      
         B     WRNGTXT                                                          
*                                                                               
WECST    DS    0H                                                               
         MVC   CONHEAD(34),=C'Exceeded capacity of STATION table'               
         MVC   ACURFORC,AFRSTKEY                                                
         OI    C2MMEDH+6,X80+X01   FORCE KEY CHANGE FOR NEXT TIME               
         B     WRNEXIT                                                          
         EJECT                                                                  
WRNGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMWRN        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         DROP  R1                                                               
         B     WRNEXIT                                                          
                                                                                
                                                                                
WRNEXIT  DS    0H                                                               
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBRXM-INF MSGS)'                     
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
                                                                                
         CLI   MYINFCD,0                                                        
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   MYINFCD,XMINFQ                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,MYINFCD          BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMINF00(RF)                                                      
                                                                                
NBYMKTQ  EQU   ((XMINF01-XMINF00)/4)+1                                          
RQADDQ   EQU   ((XMINF02-XMINF00)/4)+1                                          
                                                                                
XMINF00  DS    0H                                                               
XMINF01  B     NBYMKT                                                           
XMINF02  B     RQADD                                                            
INFX#    EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
         EJECT                                                                  
NBYMKT   MVC   MSGNUM2,=AL2(SI#NBYMK)   RECRD DISPLAYED - NO BUY IN MKT         
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFGTXT                                                          
*                                                                               
RQADD    MVC   MSGNUM2,=AL2(SI#SLKAD)   STATION LOCKIN REQUEST ADDED            
         B     INFGTXT                                                          
         EJECT                                                                  
INFGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMINF        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         DROP  R1                                                               
         B     INFEXIT                                                          
                                                                                
                                                                                
INFEXIT  DS    0H                                                               
         OC    ACURFORC,ACURFORC   NEED TO SET CURSOR?                          
         BNZ   INFEXITX             NOPE                                        
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
         TM    LOCKFLAG,LKFUPLKQ                                                
         BO    INFEXITX                                                         
         TM    MISCFLG2,MF2BUY0Q                                                
         BO    INFEXITX                                                         
         MVC   ACURFORC,AM1STREC    UNLESS 1ST DATA FLD IS PREFERRABLE          
                                                                                
INFEXITX DS    0H                                                               
         GOTO1 ERREX                                                            
         B     XIT_XM                                                           
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBRXM--LTORG && CONSTANTS)'          
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
         TITLE 'SPSFM3E - PW MAINTENANCE (SUBRXM--MISC STUFF)'                  
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(4096-SUBRXML+1)                                              
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* WE HAVE A NEW ENTRY GOING INTO ACCUTAB                                        
*                                                                               
* ON ENTRY:    (R4)                ENTRY IN ACCUTAB                             
*              FULL   BYTES 0-2    STATION PACKED                               
*                                                                               
*  ***WARNING: WORK WILL GET CLOBBERED                                          
***********************************************************************         
NWACNTRY NTR1  BASE=*,LABEL=*                                                   
         USING ACCUTABD,R4                                                      
*        MVC   ELEM(ACCUTABQ*2+1),0(R4)  COPY THE 2 TOTAL ENTRIES & FF          
         MVC   ELEM(ACCUTABQ*3),0(R4)  COPY 2 TOTAL ENTRIES & FFFFFF'S          
*********                                                                       
         XC    0(ACCUTABQ,R4),0(R4)                                             
         MVC   ATSTAPKD,FULL                                                    
         GOTO1 DATCON,DMCB,(0,ESDATE),(2,ATMSTART),0                            
         GOTO1 DATCON,DMCB,(0,EEDATE),(2,ATMEND),0                              
         MVC   ATWSTART,ATMSTART                                                
         MVC   ATWEND,ATMEND                                                    
         OI    ATFLAG,ATFNOSPT     INITIALIZE TO NO SPOTS                       
         MVC   ATLKCS2(8),ELEM+ATLKCS2-ACCUTABD     MOVE IN  CS2                
*                                                                               
         OC    AMYC2REC,AMYC2REC   ANY COST2 RECORD OUT THERE?                  
         BZ    NTRY20                                                           
         L     RE,AMYC2REC         SEE IF WE HAVE A %-AGE FOR THIS STA          
         USING PWRECD,RE                                                        
         SR    R0,R0                                                            
         LA    R1,PWEL                                                          
NTRY10   CLI   0(R1),0             END OF RECORD?                               
         BNE   NTRY11              YES, WE'LL ADD IT LATER                      
         MVC   ATCS2,OC2PCT                                                     
         B     NTRY20                                                           
*                                                                               
NTRY11   CLI   0(R1),C2STCODQ      STATION PERCENT ELEM?                        
         BE    NTRY15                                                           
NTRY12   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     NTRY10                                                           
*                                                                               
         USING C2STEL,R1                                                        
NTRY15   OC    C2STSTAP,C2STSTAP   MARKET LEVEL ONLY                            
         BZ    *+14                                                             
         CLC   C2STSTAP,FULL                                                    
         BNE   NTRY12                                                           
*                                                                               
         MVC   ATCS2,C2STPCT                                                    
         CLI   C2STLEN,C2STLENQ    TEMP CODE, FORGOT ABOUT CURRENT              
         BL    *+10                                                             
         MVC   ATCS2,C2STFCTR                                                   
*                                                                               
         MVC   OC2PCT,ATCS2                                                     
         MVC   ATLKCS2,C2STPCT                                                  
         MVC   ATCLCK(8),C2STLKC2                                               
         DROP  R1,RE                                                            
*********                                                                       
NTRY20   AHI   R4,ACCUTABQ                                                      
         L     RF,AACCUTAB                                                      
         AHI   RF,ACCUTABX-ACCUTAB-1-ACCUTABQ*2                                 
         CR    R4,RF                                                            
         BNH   NTRY30                                                           
         MVC   CONHEAD(37),=C'TOO MANY STATIONS!!  PLEASE CALL DDS!'            
         OI    CONHEADH+6,X'80'                                                 
         DC    H'0',C'$ABEND'                                                   
*                                                                               
*NTRY30   MVC   0(ACCUTABQ*2+1,R4),ELEM   COPY THE 2 TOTAL ENTRIES & FF         
NTRY30   MVC   0(ACCUTABQ*3,R4),ELEM   COPY 2 TOTAL ENTRIES & FFFFFF'S          
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (TABLES)'                   
***********************************************************************         
*========================== T21759's TABLES ==========================*         
                                                                                
DSPCSECT DS    0H                  DISPLACEMENTS FROM T21759 CSECT              
         DC    Y(TIADSPTB-T21759)                                               
         DC    Y(LBLTAB-T21759)                                                 
         DC    Y(COREQLST-T21759)                                               
         DC    Y(EDITFLD-T21759)                                                
         DC    Y(ELDATTAB-T21759)                                               
         DC    Y(OPTABLES-T21759)                                               
         DC    Y(PFTABLST-T21759)                                               
         DC    Y(PFTABMNT-T21759)                                               
         DC    Y(REQTWATB-T21759)                                               
         DC    Y(VK-T21759)                                                     
         DC    Y(VR-T21759)                                                     
         DC    Y(DR-T21759)                                                     
         DC    Y(GOSUB-T21759)                                                  
         DC    Y(SUBR01-T21759)                                                 
         DC    Y(SUBR02-T21759)                                                 
         DC    Y(SUBR03-T21759)                                                 
         DC    Y(SUBR04-T21759)                                                 
         DC    Y(XMSGRTN-T21759)                                                
         DC    Y(LOCDATE-T21759)                                                
         DC    Y(LOC1WMTH-T21759)                                               
         DC    Y(LOCSKED-T21759)                                                
DSPCSCTQ EQU   (*-DSPCSECT)/(L'DSPCSECT)                                        
                                                                                
         DS    0CL(DSPCSCTQ-AINCSCTQ+1)                                         
         DS    0CL(AINCSCTQ-DSPCSCTQ+1)                                         
                                                                                
                                                                                
TIADSPTB DS    0H                  DISP OF LABELS IN SPOT STORAGE               
         DC    Y(ACTBLABL-SPOTAREA)                                             
         DC    Y(ESTBLABL-SPOTAREA)                                             
TIADSPQ  EQU   (*-TIADSPTB)/(L'TIADSPTB)                                        
                                                                                
                                                                                
LBLTAB   DS    0CL8                LABEL NAMES FOR SPOT STORAGE                 
         DC    CL8'*ACTB3E*'                                                    
         DC    CL8'**ESTAB*'                                                    
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
         DS    0CL(TIADSPQ-LBLTABQ+1)                                           
         DS    0CL(LBLTABQ-TIADSPQ+1)                                           
                                                                                
                                                                                
INIWRKTB DS    0XL(2+2+8)          TABLE FOR INITIALIZING WORK AREA             
         DC    AL2(SPBKLABL-MYWORKD,ASBLOCKD-SYSD),CL8'*SPTBLK*'                
         DC    AL2(CHNKLABL-MYWORKD,ASPCHUNK-SYSD),CL8'**CHUNK*'                
         DC    AL2(MYPWLABL-MYWORKD,AMYC2REC-SYSD),CL8'*PWRECD*'                
         DC    AL2(MKESLABL-MYWORKD,AMKESTAB-SYSD),CL8'*MKESTB*'                
INIWRKTQ EQU   (*-INIWRKTB)/(L'INIWRKTB)                                        
                                                                                
                                                                                
COREQLST DS    0AL1                LIST OF CORE-RESIDENT PHASE TO GET           
         DC    AL1(QTSAR)                                                       
         DC    AL1(QPWCALC)                                                     
         DC    AL1(QSPOTIO)         THE ORDER OF THIS LIST SHOULD               
         DC    AL1(QSPOTBUY)        BE CHANGED WITH CARE                        
         DC    AL1(QSPOTGL)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QGETBROD)                                                    
COREQLSQ EQU   *-COREQLST                                                       
                                                                                
                                                                                
EDITFLD  DS    0H                  LIST OF FIELDS DSPLYING ACCUM VALUES         
         DC    Y(MSLCBUD-MSLDSECT)  ATACGOAL                                    
         DC    Y(MSLWBUD-MSLDSECT)  ATAJGOAL                                    
         DC    Y(MSLCLCK-MSLDSECT)  ATCLCK                                      
         DC    Y(MSLWLCK-MSLDSECT)  ATWLCK                                      
         DC    Y(MSLCCOS-MSLDSECT)  ATAJBUY                                     
         DC    Y(MSLWCOS-MSLDSECT)  ATACBUY                                     
EDITFLDQ EQU   (*-EDITFLD)/(L'EDITFLD)                                          
                                                                                
                                                                                
ELDATTAB DS    0XL(1+1+2)          ELEMENTS W/ BROADCAST DATES IN THEM          
         DS     0XL1                ELCODE                                      
         DS     0XL1                DATE DISPL INTO ELEM                        
         DS     0XL2                # BUCKETS, 1ST BUCKET DISPL                 
         DC     AL1(C2STCODQ),AL1(C2STSTAP-C2STEL),AL1(0,0)                     
         DC     AL1(PWDOLCDQ),AL1(PWDOLWK-PWDOLEL),AL1(7,4)                     
         DC     AL1(PWCURCDQ),AL1(PWCURWK-PWCUREL),AL1(7,4)                     
         DC     AL1(PWBAKDOL),AL1(PWDOLWK-PWDOLEL),AL1(7,4)                     
         DC     AL1(PWCLCCDQ),AL1(PWCLCWK-PWCLCEL),AL1(1,4)                     
         DC     AL1(PWCLLCDQ),AL1(PWCLLWK-PWCLLEL),AL1(1,4)                     
         DC     AL1(PWBAKCLL),AL1(PWCLLWK-PWCLLEL),AL1(1,4)                     
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
YESNO    DS    0C                                                               
         DC    AL1(3),C'YES'                                                    
         DC    AL1(2),C'NO'                                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*--------------------------- OPTIONS TABLES --------------------------*         
                                                                                
OPTABLES DS    0C                  SEE OPTDSECT                                 
*                                                                               
OPT1     DC    AL1(1),AL1(OPT2-OPT1)                                            
         DC    AL1(OPUTAX,OPFOTHAB)                                             
         DC    AL1(1),AL2(USETAX-SYSD)                                          
OPT1NAME DC    AL1(OPT1V1-OPT1NAME-1),C'TAX'                                    
OPT1V1   DC    AL2(YESNO-T21759)                                                
*                                                                               
OPT2     DS    0C                  No Billing Adjustment                        
         DC    AL1(2),AL1(OPT3-OPT2)                                            
         DC    AL1(OPUNBA,OPFKYWRD)                                             
         DC    AL1(L'SHOWBA),AL2(SHOWBA-SYSD)                                   
OPT2NAME DC    AL1(OPT2V1-OPT2NAME-1),C'NBA'                                    
OPT2V1   DC    AL1(1),C'N'                                                      
*                                                                               
OPT3     DS    0C                                                               
*                                                                               
OPTX     DC    AL1(EOT)                                                         
         EJECT                                                                  
*---------------------------- PFKEY TABLES ---------------------------*         
                                                                                
PFTABLES DS    0C                  ****** REFER TO PFTABD & KEYD ******         
*                                                                               
** PROCESS THE FOLLOWING ONLY IF FROM LIST SCREEN **                            
*                                                                               
PFTABLST DS    0C                                                               
*                                                                               
PFT12    DS    0C                  Return to LIST screen                        
         DC    AL1(PFT12X-PFT12,12)                                             
         DC    AL1(PFTRPROG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    3C' ',8C' ',8C' '                                                
PFT12X   EQU   *                                                                
*                                                                               
** THE FOLLOWING ALWAYS GET PROCESSED **                                        
*                                                                               
PFTABMNT DS    0C                                                               
*                                                                               
PFT02    DS    0C                  Lock/Unlock Buy                              
         DC    AL1(PFT02X-PFT02,02)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT02X   EQU   *                                                                
*                                                                               
PFT03    DS    0C                  Lock/Unlock PW                               
         DC    AL1(PFT03X-PFT03,03)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT03X   EQU   *                                                                
*                                                                               
PFT05    DS    0C                  Top (page to top of sked)                    
         DC    AL1(PFT05X-PFT05,05)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT05X   EQU   *                                                                
*                                                                               
PFT06    DS    0C                  Bottom (page to bottom of sked)              
         DC    AL1(PFT06X-PFT06,06)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT06X   EQU   *                                                                
*                                                                               
PFT07    DS    0C                  Up (scroll up)                               
         DC    AL1(PFT07X-PFT07,07)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT07X   EQU   *                                                                
*                                                                               
PFT08    DS    0C                  Down (scroll down)                           
         DC    AL1(PFT08X-PFT08,08)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT08X   EQU   *                                                                
*                                                                               
PFT09    DS    0C                  Redisplay                                    
         DC    AL1(PFT09X-PFT09,09)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT09X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*--------------------------- REQTWA TABLES ---------------------------*         
                                                                                
REQTWATB DS    0C                  SEE RQTWATBD                                 
                                                                                
RQT1     DS    0C                  SL - station-lockin request                  
         DC    AL1(1),AL1(RQT2-RQT1)                                            
         DC    C'SL',AL1((RQT1X-RQT1FD)/(RQFDLENQ))                             
RQT1FD   DS    0C                                                               
         DC    AL2(CONWHENH-T217FFD,WHENDS-SYSD),AL1(L'WHENDS-1)                
         DS    0CL(L'CONWHEN-L'WHENDS+1)                                        
         DC    AL2(SLOMEDH-T217FFD,QMED-SYSD),AL1(L'QMED-1)                     
         DS    0CL(L'SLOMED-L'QMED+1)                                           
         DC    AL2(SLOCLTH-T217FFD,QCLT-SYSD),AL1(L'QCLT-1)                     
         DS    0CL(L'SLOCLT-L'QCLT+1)                                           
         DC    AL2(SLOPRDH-T217FFD,QPRD-SYSD),AL1(L'QPRD-1)                     
         DS    0CL(L'SLOPRD-L'QPRD+1)                                           
         DC    AL2(SLOESTH-T217FFD,QEST-SYSD),AL1(L'QEST-1)                     
         DS    0CL(L'SLOEST-L'QEST+1)                                           
         DC    AL2(SLOMKTH-T217FFD,QMKT-SYSD),AL1(L'QMKT-1)                     
         DS    0CL(L'SLOMKT-L'QMKT+1)                                           
         DC    AL2(SLOSTAH-T217FFD,ALLDS-SYSD),AL1(L'ALLDS-1)                   
         DS    0CL(L'SLOSTA-L'ALLDS+1)                                          
RQT1X    DS    0C                                                               
*                                                                               
RQT2     DS    0C                                                               
                                                                                
         DC    AL1(EOT)                                                         
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE'                            
***********************************************************************         
*========================== T21759's EQUATES =========================*         
EOT      EQU   X'00'               END OF TABLE MARKER                          
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
LSTSCRNQ EQU   X'65'               CS2/LIST SCREEN PHASE NUMBER                 
PAGE00   EQU   0                   TWA PAGE # FOR TEMPSTR                       
PAGEQ    EQU   1                   TWA PAGE # FOR TEMPSTR                       
PAGETSRQ EQU   4                   TEMPSTR PAGE FOR TSAR                        
NMPGTSRQ EQU   1                   ONE PAGE SHOULD BE ENOUGH                    
MINPWQ   EQU   1                   MIN CS2  = 0.000001                          
MAXPWQ   EQU   9999999             MAX CS2  = 9.999999                          
SPBKLEN  EQU   SBLOCKX-SBLOCK      L(SPOTBLOCK)                                 
PWBLKL   EQU   PWBLKX-PWBLKD       L(PWBLOCK)                                   
PKYMKTL  EQU   PWKSTA-PWFKEY       L(PW KEY UNTIL & INCLUDING MKT)              
NMSLINEQ EQU   ((C2MSTTLH-C2MSTATH)/MSLLENQ)  # OF MAINT SCRN LINES             
TIASVLEN EQU   SPTENDSV-SPOTAREA   LEN TO SAVE INTO TEMPSTR                     
         EJECT                                                                  
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (SPSFMWORKD)'               
***********************************************************************         
*============================= SPSFMWORKD ============================*         
       ++INCLUDE SPSFMWORKD                                                     
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (TWA DSECTS)'               
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*-------------------- SPOTWRITER SL REPORT SCREEN --------------------*         
                                                                                
         DS    0C                  USED FOR STATION-LOCKIN REPORT               
         ORG   CONTAGH                                                          
* INCLUDE SPWRIEBD                                                              
         PRINT OFF                                                              
       ++INCLUDE SPWRIEBD                                                       
         PRINT ON                                                               
*------------------------COST2 LIST SCREEN ---------------------------*         
         ORG   CONTAGH                                                          
* INCLUDE SPSFM65D                                                              
         PRINT OFF                                                              
       ++INCLUDE SPSFM65D                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*-------------------------- PW BILL SCREEN ---------------------------*         
         ORG   CONTAGH                                                          
* INCLUDE SPSFM98D                                                              
         PRINT OFF                                                              
       ++INCLUDE SCSFM98D                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*-------------------------- PW LOCK SCREEN ---------------------------*         
         ORG   CONTAGH                                                          
* INCLUDE SPSFM97D                                                              
         PRINT OFF                                                              
       ++INCLUDE SCSFM97D                                                       
         PRINT ON                                                               
         EJECT                                                                  
*------------------------COST2 MAINT SCREEN --------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM66D                                                       
         EJECT                                                                  
                                                                                
         ORG                                                                    
         DS    0F                                                               
TSARBLK  DS    XL(TSARDL)          TSAR PARAMETER BLOCK                         
                                                                                
                                                                                
MYTWAL   EQU   *-CONHEADH                                                       
         DS    0CL(3520-MYTWAL)    CHECK AGAINST GENCON'S TWA LIMIT             
         EJECT                                                                  
*------------------------- OTHER TWA STORAGES ------------------------*         
                                                                                
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
                                                                                
                                                                                
* SPSFMSAVED                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSFMSAVED                                                     
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY'                    
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
* DDTSARD                                                                       
* GEMSGEQUS                                                                     
* SPDDEQUS                                                                      
* SPMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE GEMSGEQUS                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE SPMSGEQUS                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (SPGEN DSECTS)'             
***********************************************************************         
*============================ SPGEN DSECTS ===========================*         
                                                                                
*----------------------------- SPGENWIPW -----------------------------*         
       ++INCLUDE SPGENWIPW         WESTERN INTL. PROFIT WITHIN REC              
         EJECT                                                                  
*------------------------------ SPGENAGY -----------------------------*         
AGYHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENCLT -----------------------------*         
CLTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENEST -----------------------------*         
ESTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENBUY -----------------------------*         
BUYRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENGOAL ----------------------------*         
GOALRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENBILL ----------------------------*         
BILLRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBILL                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENSTAB ----------------------------*         
* STABUCKD DSECT                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENSTAB                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY'                    
***********************************************************************         
*============================== PW BLOCK =============================*         
                                                                                
       ++INCLUDE SPPWBLOCK                                                      
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY (SYSD)'             
***********************************************************************         
*========================== SAVED WORK AREA ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
* The following area is shared by the PW phases under SPSFM.  The               
*  purpose is to enable communication among themselves when needed.             
                                                                                
PVACTEQU DS    XL(L'ACTEQU)        ACTEQU OF LAST TRANSACTION                   
CTODAY   DS    XL2                 TODAY'S DATE COMPRESSED                      
CALLPFK  DS    XL(L'PFKEY)                                                      
BUYRID   DS    CL3                 BUYER'S INITIALS                             
BINYRMT  DS    XL2                 A BINARY YEAR/MONTH FIELD                    
SVOPTFLD DS    CL(L'C2MOPTN)       SAVE TEXT IN OPTIONS FIELD                   
                                                                                
* The following fields are owned by this program only                           
                                                                                
*                                 ************** ADDRESSES ************         
RELO     DS    F                                                                
BASE1    DS    A                   A(1ST BASE REG OF MAIN NMOD)                 
BASE2    DS    A                   A(2ND BASE REG OF MAIN NMOD)                 
BASE3    DS    A                   A(3RD BASE REG OF MAIN NMOD)                 
                                                                                
AINCSECT DS    0A                  ADDRS NEEDED W/IN CSECT                      
ATIADSPT DS    A                    A(TIADSPTB)                                 
ALBLTAB  DS    A                    A(LBLTAB)                                   
ACORQLST DS    A                    A(CORE-RES EQUATE LIST)                     
AEDITFLD DS    A                    A(EDITFLD)                                  
AELDATTB DS    A                    A(ELDATTAB)                                 
AOPTABLE DS    A                    A(OPTABLES)                                 
APFLIST  DS    A                    A(PFTABLST)                                 
APFMAINT DS    A                    A(PFTABMNT)                                 
ARQTWATB DS    A                    A(REQTWATBT)                                
AVK      DS    A                    A(VK)                                       
AVR      DS    A                    A(VR)                                       
ADR      DS    A                    A(DR)                                       
AGOSUB   DS    A                    A(SUBRTN INTERFACE)                         
ASUBR01  DS    A                    A(1st SUBROUTINE POOL)                      
ASUBR02  DS    A                    A(2nd SUBROUTINE POOL)                      
ASUBR03  DS    A                    A(3rd SUBROUTINE POOL)                      
ASUBR04  DS    A                    A(4th SUBROUTINE POOL)                      
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
ALOCDATE DS    A                    A(LOCDATE)                                  
ALC1WMTH DS    A                    A(LOC1WMTH)                                 
ALOCSKED DS    A                    A(LOCSKED)                                  
AINCSCTQ EQU   (*-AINCSECT)/(L'AINCSECT)                                        
                                                                                
ASUBRTN  DS    A                   A(SUBROUTINE POOL) TO USE                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
ALOCERR  DS    A                   A(LOCATION WHERE ERROR OCCURRED)             
AM1STREC DS    A                   MY OWN AFRSTREC                              
AMYWORK  DS    A                   A(MY D-CHAIN WORK AREA)                      
ASBLOCKD DS    A                   A(SPOTBLOCK)                                 
ASPCHUNK DS    A                   A(SPOT CHUNK AREA)                           
AMYC2REC DS    A                   A(MYPWREC)                                   
AMKESTAB DS    A                   A(MKT/EST TABLE FOR SPOTIO)                  
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
         DS    H                   SPARE TO KEEP FULL-WORD ALIGNMENT            
                                                                                
AACCUTAB DS    A                   A(ACCUMULATOR TABLE)                         
ACORQADD DS    0A                  **** CORE-RES ADDRESSES GO HERE ****         
ATSAR    DS    A                    A(TSAR)                                     
APWCALC  DS    A                    A(PWCALC)                                   
ASPOTIO  DS    A                    A(SPOTIO)                                   
ASPOTBUY DS    A                    A(SPOTBUY)                                  
ASPOTGL  DS    A                    A(SPOTGOAL)                                 
AMOBILE  DS    A                    A(MOBILE)                                   
MOBINPAD DS    0F                   ******* MOBILE ADCON LIST *********         
AGETBROD DS    A                     A(GETBROAD)                                
AADDAY   DS    A                     A(ADDAY)                                   
AGETDAY  DS    A                     A(GETDAY)                                  
ADATCON  DS    A                     A(DATCON)                                  
                                                                                
DSLIST   DS    0C                 ******** DATA DICTIONARY TERMS ******         
         DSDDL PRINT=YES                                                        
         DS    0CL(L'MSLSTAT-(L'SP@BLNG+1+L'SP@ADJ)+1)                          
                                                                                
*                                 ************* CONSTANTS *************         
RELOCKSW DS    XL2                 SWITCH TO DO A RE-LOCK (SFM42)               
                                                                                
*                                 **************** MISC ***************         
HOLDRE   DS    F                                                                
DIVIDEND DS    D                   DIVIDEND                                     
DIVISOR  DS    F                   DIVISOR                                      
QUOTIENT DS    F                   QUOTIENT                                     
REMAINDR DS    F                   REMAINDER                                    
TEMPPW   DS    F                   TEMP STORAGE FOR CURRENT COST2               
TEMPACB  DS    F                    "      "     "  ACTUAL   BUYS               
TEMPAJB  DS    F                    "      "     "  ADJUSTED BUYS               
TEMPACG  DS    F                    "      "     "  ACTUAL   GOALS              
TEMPAJG  DS    F                    "      "     "  ADJUSTED GOALS              
TEMPGRP  DS    F                    "      "     "  TOTAL GRP                   
TEMPDRCR DS    F                   TEMP FOR ADJUSTED DR/CR                      
TEMPTAX  DS    F                   TEMP FOR TAX (RATE OR DOLLARS)               
TEMPCTX  DS    F                   TEMP FOR CLT TAX DOLLARS                     
TEMPNSPT DS    F                   TEMP FOR # OF SPOTS                          
TEMPACBN DS    F                    "      "     "  ACTUAL   BUYS (NET)         
TEMPAJBN DS    F                    "      "     "  ADJUSTED BUYS (NET)         
TEMPBLD  DS    H                   TEMP FOR BILL DATE                           
         DS    H                   (SPARE TO KEEP FULL ALIGNMENT)               
                                                                                
GNELVALS DS    0F                                                               
SKEDACG2 DS    F                                                                
PWGRP    DS    F                   GRP FROM AND FOR PW RECORD                   
GLTXRATE DS    H                                                                
GNELVALQ EQU   *-GNELVALS                                                       
SKEDCGL  DS    F                   SKED ACTUAL GOAL                             
SKEDCGTX DS    F                   SKED ACTUAL GOAL + TAX                       
GOALGRP  DS    F                   GRP FROM GOAL RECORDS                        
*                                                                               
MTOTALS  DS    0F                                                               
MTTLACG  DS    F                   MONTH TOTAL FOR ACTUAL   GOALS               
MTTLAJG  DS    F                     "     "    "  ADJUSTED GOALS               
MTTLCLK  DS    F                     "     "    "  CLIENT   LOCKS               
MTTLWLK  DS    F                     "     "    "  WIM      LOCKS               
MTTLAJB  DS    F                     "     "    "  ADJUSTED BUYS                
MTTLACB  DS    F                     "     "    "  ACTUAL   BUYS                
MTTLTAX  DS    F                     "     "    "  TAX      $                   
MTTLCTX  DS    F                     "     "    "  CLT TAX  $                   
MTTLSPT  DS    F                     "     "    "  # OF SPOTS                   
MTOTALQ  EQU   *-MTOTALS                                                        
*                                                                               
STOTALS  DS    0F                                                               
SKEDACG  DS    F                   SCHED TOTAL FOR ACTUAL   GOALS               
SKEDAJG  DS    F                     "     "    "  ADJUSTED GOALS               
SKEDCLK  DS    F                     "     "    "  CLIENT   LOCKS               
SKEDWLK  DS    F                     "     "    "  WIM      LOCKS               
SKEDAJB  DS    F                     "     "    "  ADJUSTED BUYS                
SKEDACB  DS    F                     "     "    "  ACTUAL   BUYS                
SKEDTAX  DS    F                     "     "    "  TAX      $                   
SKEDCTX  DS    F                     "     "    "  CLT TAX  $                   
SKEDSPT  DS    F                     "     "    "  # OF SPOTS                   
STOTALQ  EQU   *-STOTALS                                                        
         DS    0CL(STOTALQ-MTOTALQ+1)                                           
         DS    0CL(MTOTALQ-STOTALQ+1)                                           
SKEDDRCR DS    F                   SCHED TOTAL FOR DEBIT/CREDIT AMOUNT          
                                                                                
OC2PCT   DS    F                   ORIG SKED CS2                                
ESDATE   DS    CL(L'ESTART)        EBCDIC START DATE OF ESTIMATE                
EEDATE   DS    CL(L'EEND)            "    END    "   "     "                    
STARTEND DS    CL12                TEMP STRGE FOR STRT-END DATE                 
MYDATE6  DS    CL6                 TEMP STORAGE FOR AN EBCDIC DATE              
MNTHMRKS DS    XL4                 START MONTH-MARK (COMPRESSED DATE)           
MNTHMRKE DS    XL4                 END   MONTH-MARK (COMPRESSED DATE)           
DATE2    DS    XL2                 TEMP BINARY DATE STORAGE                     
         ORG   DATE2                                                            
DATE4    DS    0XL4                TEMP START/END DATES IN BINARY               
SDATE2   DS    XL2                  START DATE                                  
EDATE2   DS    XL2                  END DATE                                    
RUNDATE  DS    XL2                                                              
MYAGYID  DS    CL(L'AGYID)         AGENCY ID FROM AGYHDR                        
MYSELSTN DS    XL(L'SELLISTN)                                                   
MYSELCD  DS    XL(L'THISLSEL)                                                   
ESTOOWSD DS    XL(L'EOWSDAY)       OUT-OF-WEEK ROTATOR START DAY                
TEMPBSTA DS    XL(L'BSTA)          TEMP STORAGE FOR BINARY STATION              
CBLSCMSK DS    XL(L'BSTA)          CABLE STATION SYSCODE MASK                   
REQID    DS    CL3                 REQUEST ID FIELD                             
****MYCALLSP DS    XL2                                                          
NUMLNDSP DS    XL1                 # OF LINES DISPLAYED                         
DISPACTB DS    XL1                 NTH ENTRY IN ACCUTAB TO CONTINUE DSP         
LNCNTDWN DS    XL1                 COUNT DOWN LINE NUMBER IN DISPREC            
NUMWEEKS DS    XL1                 NUMBER OF WEEKS IN PERIOD                    
MSGSYS   DS    XL1                                                              
MSGNUM2  DS    XL2                                                              
MYERRCD  DS    XL1                 MY ERROR   CODE                              
MYWRNCD  DS    XL1                 MY WARNING CODE                              
MYINFCD  DS    XL1                 MY INFO    CODE                              
GOSUBN   DS    XL1                 ROUTINE # FOR SUB-RTN INTERFACES             
ELCDLO   DS    XL1                 LOW ELCODE                                   
ELCDHI   DS    XL1                 HIGH ELCODE                                  
USETAX   DS    CL1                 (Y)ES OR (N)O--CALC W/ TAX OR NOT            
SVUSETAX DS    CL(L'USETAX)                                                     
SHOWBA   DS    CL1                 (Y)ES OR (N)O--SHOW BILL ADJUSTMENT          
SVSHOWBA DS    CL(L'SHOWBA)                                                     
WHENDS   DS    CL6                                                              
ALLDS    DS    CL(L'SP@ALL)                                                     
SVTSRNUM DS    XL(L'TSRNUM)        SAVE RECD# FOR NESTED TSAR CALLS             
                                                                                
*                                 *************** FLAGS ***************         
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1KYCHG EQU   X'80'                KEY FIELDS HAVE BEEN CHANGED                
MF1MDFY  EQU   X'40'                SOMETHING WAS MODIFIED                      
MF1MDFY2 EQU   X'20'                SOMETHING WAS MODIFIED                      
MF1N1TSL EQU   X'10'                NOT 1ST SELECT OF RECORD                    
MF1GOL0Q EQU   X'08'                NO GOALS FROM FILE                          
MF1ERRQ  EQU   X'04'                PREV TRANSACTION HAD ERROR                  
MF1OPTN  EQU   X'02'                OPTION INPUTTED THIS TIME                   
MF1ADNOW EQU   X'01'                ADD MKT-LEVEL PW RECD NOW!                  
MF1MODFY EQU   MF1MDFY+MF1MDFY2             SOMETHING WAS MODIFIED              
MF1NKFS  EQU   MF1N1TSL+MF1GOL0Q+MF1ERRQ    RESET THESE ON NEW KEYS             
MF1KYOPT EQU   MF1KYCHG+MF1OPTN             KEY OR OPTION CHANGED               
                                                                                
MISCFLG2 DS    XL1                 MISC FLAG #2                                 
MF2RRDCH EQU   X'80'                RE-READ FILE ON CHANGE                      
MF2DELEL EQU   X'40'                ELEMS DELETED FROM RECD ALREADY             
MF2BUY0Q EQU   X'20'                NO BUYS  FROM FILE                          
MF2BILAJ EQU   X10                  AT LEAST 1 BILL ADJ LINE SHOWN              
MF2GOALC EQU   X08                  GOALS DIFFER ON PW & GOAL RECD              
MF2GRPC  EQU   X04                  GRPs  DIFFER ON PW & GOAL RECD              
MF2RQADD EQU   X02                  STATION LOCKIN REQUEST ADDED                
MF2SLAVE EQU   X01                  SLAVED FOR LOCKING (FROM LIST SCRN)         
MF2GOGRC EQU   MF2GOALC+MF2GRPC                                                 
MF2NKFS  EQU   MF2BUY0Q+MF2BILAJ+MF2GOGRC+MF2RQADD   RESET ON NEW KEY           
                                                                                
MISCFLG3 DS    XL1                 MISC FLAG #3                                 
MF3RFBQ  EQU   X80                  PROCESS(ED) FOR RE-FINAL BILLING            
MF3GDPLY EQU    X40                 GO TO DISPLAY LOGIC                         
MF3UGOAL EQU    X20                 GOAL IS USER-INPUTTED                       
MF3UGRP  EQU    X10                 GRP  "   "      "                           
MF31STIM EQU    X08                 FIRST TIME (THAT SCREEN WAS LOADED)         
MF3SSCHK EQU    X04                 SKIP #-OF-SPOTS CHECK                       
MF3NDSCR EQU    X02                 HIT END OF SCREEN                           
MF3NKFS  EQU   MF3RFBQ+MF3UGOAL+MF3UGRP    RESET ON NEW KEY                     
                                                                                
LOCKFLAG DS    XL1                 LOCK FLAG                                    
LKFFBLKQ EQU   X'80'                BUY LOCKED ON FILE ALREADY                  
LKFUBLKQ EQU   X'40'                USER'S CHOICE OF BUY LOCK/UNLOCK            
LKFBPLK2 EQU   X'20'                PWGNBPLK FLAG ON IN PW RECORDS              
LKFFPLKQ EQU   X'08'                FILE HAS CS2 LOCKED                         
LKFUPLKQ EQU   X'04'                USER'S CHOICE OF CS2 LOCK/UNLOCK            
LKFCOPYQ EQU   X'02'                COSTS HAVE BEEN COPIED TO LOCK              
LKFCOLKQ EQU   X'01'                USER WANTS COSTS COPIED TO LOCK             
LKFBUYQ  EQU   LKFFBLKQ+LKFUBLKQ                                                
LKFPWQ   EQU   LKFFPLKQ+LKFUPLKQ                                                
LKFBYPWQ EQU   LKFUBLKQ+LKFUPLKQ                                                
LKFNKFS  EQU   LKFCOPYQ+LKFCOLKQ    RESET THESE WHEN KEY CHANGES                
*                                                                               
MYIOFLAG DS    XL1                 MY IO FLAG                                   
IOFADD   EQU   1                    ADD RECORD                                  
IOFWRITE EQU   2                    WRITE RECORD                                
IOFSTORG EQU   3                    RECORD FROM STORAGE                         
*                                                                               
OPTUSED  DS    XL1                 OPTIONS USED FLAG                            
OPUTAX   EQU   X80                  TAX= OPTION SPECIFIED                       
OPUNBA   EQU   X40                  NBA  OPTION SPECIFIED                       
*                                                                               
CLTFLAG  DS    XL1                 CLIENT   HEADER FLAG                         
CFWSTRAD EQU    X80                 WESTERN TRADING CLIENT                      
*                                                                               
ESTFLAG  DS    XL1                 ESTIMATE HEADER FLAG                         
EFBILEST EQU   X'80'                ECONTROL HAS EBILESTQ ON                    
EFOWPW   EQU    X40                 OUT-OF-WEEK PW BILLING                      
EFWSTRAD EQU    X20                 WESTERN TRADING ESTIMATE                    
                                                                                
SPTIOMDE DS    XL1                 SPOTIO MODE                                  
SIOMBUY  EQU    C'B'                READING BUYS ONLY                           
SIOMNORM EQU    C'N'                READ ALL NECESSARY RECORDS (NORMAL)         
                                                                                
MYTSERRS DS    XL(L'TSERRS)        REMEMBER ERROR RETURNED FROM TSAR            
                                                                                
*                                 *************** TABLES **************         
DTIELEM  DS    XL(PWDTILNQ)        THE PWDTIEL ELEMENT                          
                                                                                
BRDMTHQ  EQU   5                       MAX OF 5 BROADCAST MONTHS                
BRDMTHTB DS    XL(BRDMTHQ*BCSTTABQ+1)  TABLE OF BROADCAST MONTHS                
                                                                                
BRDWKQ   EQU   15                      MAX OF 5 BROADCAST WEEKS                 
BRDWKTAB DS    XL(BRDWKQ*BCSTTABQ+1)   TABLE OF BROADCAST WEEKS (MINE)          
BRDWKTB2 DS    XL(BRDWKQ*4+1)            "   "      "       "                   
                                                                                
PWTAB    DS    XL(15*(4+4)+1)      PW TABLE (DATES & CS2) BY WEEK               
                                                                                
*                                 ************** BUFFERS **************         
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
MYTEXTL  EQU   MYTEXTX-MYTEXT                                                   
                                                                                
STACREC  DS    XL(STACRECL)        I/O AREA TO HOLD STACTAB ENTRY               
                                                                                
MYCANADA DS    CL1                                                              
MYSSPREL EQU   *-SYSSPARE                                                       
         DS    0CL(1024-MYSSPREL)  CHECK AGAINST AVAIL SYSSPARE AMT             
***********************************************************************         
         EJECT                                                                  
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE OVERLAY (MISC DSECT+        
               S)'                                                              
***********************************************************************         
* ACCUMULATOR TABLE DSECT                                                       
***********************************************************************         
ACCUTABD DSECT                                                                  
ATSTAPKD DS    XL3                 STATION PACKED                               
         DS    XL1                                                              
ATMONTH  DS    0XL4                BROADCAST MONTH                              
ATMSTART DS    XL2                  START DATE                                  
ATMEND   DS    XL2                  END   DATE                                  
ATWEEK   DS    0XL4                BROADCAST WEEK                               
ATWSTART DS    XL2                  START DATE                                  
ATWEND   DS    XL2                  END   DATE                                  
*                                                                               
ATACVALS DS    0XL4                ACCUMULATOR VALUES                           
ATACGOAL DS    XL4                  ACTUAL GOAL                                 
ATAJGOAL DS    XL4                  ADJUSTED SPOT GOALS                         
ATCLCK   DS    XL4                  LOCK IN (PREVIOUS ADJUSTED BUY)             
ATWLCK   DS    XL4                  LOCK IN (PREVIOUS ACTUAL   BUY)             
ATAJBUY  DS    XL4                  ADJUSTED SPOT BUYS                          
ATACBUY  DS    XL4                  ACTUAL BUY                                  
ATTAX    DS    XL4                  TAX DOLLARS                                 
ATCLTAX  DS    XL4                  CLT TAX DOLLARS                             
ATNSPT   DS    XL4                  # OF SPOTS                                  
ATACVALQ EQU   *-ATACVALS                                                       
ATACVALN EQU   (ATACVALQ/L'ATACVALS)                                            
ATLKCS2  DS    XL4                  LOCK IN CS2 FACTOR                          
ATCS2    DS    XL4                  CS2 FACTOR                                  
ATDRCR   DS    XL4                 CLT$ ADJ DR/CR AMOUNT                        
ATBILD   DS    XL2                 ADJUSTMENT BILLING DATE                      
*                                                                               
ATFLAG   DS    XL1                 FLAGS                                        
ATFMTTLQ EQU   X80                  ENTRY IS MONTHLY  TOTAL                     
ATFSTTLQ EQU   X40                  ENTRY IS SCHEDULE TOTAL                     
ATFBILAJ EQU   X20                  ENTRY IS BILLING ADJ                        
ATFAJBQ  EQU   X10                  ADJBUY/(BILL ADJ) WAS MODIFIED              
ATFPWQ   EQU   X08                  PW WAS MODIFIED                             
ATFESTBQ EQU   X04                  ESTIM BILL (FOR ATFMTTLQ ONLY)              
ATFFNLBQ EQU   X02                  FINAL BILL (FOR ATFMTTLQ ONLY)              
ATFNOSPT EQU   X01                  NO SPOT FOR THIS WK/MNTH/SKED               
ATFSKMTH EQU   ATFMTTLQ+ATFSTTLQ                                                
*                                                                               
ATFLAG2  DS    XL1                 MORE FLAGS                                   
ATF2UNPD EQU   X80                  AT LEAST ONE UNPAID SPOT                    
ATF2DPLY EQU   X40                  LINE DISPLAYED ON SCREEN                    
ATF2CCOD EQU   X20                  CLCOST OVERRIDED (eg. =5000)                
ATF2CLOD EQU   X10                  CLLOCK OVERRIDED (eg. =5000)                
ATF2RFB  EQU   X08                  MONTH CHANGED FOR RE-FINAL BILLING          
ATF2CHDC EQU   X04                  ADJ DR/CR VALUE CHANGED                     
ATF2A1PS EQU   X02                  AT LEAST ONE PAID SPOT                      
ATF2OOWP EQU   X01                  OOW PW MONTH IS CLEARED                     
ATF2COVD EQU   ATF2CCOD+ATF2CLOD                                                
*                                                                               
ACCUTABQ EQU   ((*-ACCUTABD+3)/4)*4                                             
         DS    0CL(ATACVALQ-STOTALQ+1)                                          
         DS    0CL(STOTALQ-ATACVALQ+1)                                          
         EJECT                                                                  
***********************************************************************         
* STATION ACCUMULATOR TABLE DSECT                                               
***********************************************************************         
STACRECD DSECT                                                                  
STACKEYD DS    0X                  KEY                                          
STACSTA  DS     XL3                 MSPACKED STATION                            
STACSTRT DS     XL2                 START DATE OF WEEK                          
STACEND  DS     XL2                 END   DATE OF WEEK                          
STACKEYL EQU   *-STACKEYD                                                       
*                                                                               
STACFLG  DS    XL1                 FLAG                                         
STAFMKTQ EQU   X'80'                ENTRY ACCOUNTED FOR @ MKT LEVEL             
STAFPRCQ EQU   X'40'                ENTRY ACCOUNTED FOR @ STAT LEVEL            
STAFATBQ EQU   X'20'                TRANSFERRED TO ACCUTAB ALREADY              
STACSPT  DS    XL4                 # OF SPOTS                                   
STACGRS  DS    XL4                 GROSS DOLLARS                                
STACNET  DS    XL4                 NET   DOLLARS                                
STACTAX  DS    XL4                 TAX   DOLLARS                                
STACCTX  DS    XL4                 CLT TAX DOLLARS                              
STACCGRS DS    XL4                 CLIENT GROSS DOLLARS                         
STACCNET DS    XL4                 CLIENT NET   DOLLARS                         
STACRECL EQU   *-STACRECD                                                       
         EJECT                                                                  
*========================= BROADCAST TABLE DSECT =====================*         
BCSTTABD DSECT                                                                  
BCSSTART DS    XL2                 START DATE OF A BROADCAST PERIOD             
BCSEND   DS    XL2                 END    "   "  "    "        "                
BCSFLAG  DS    XL1                 FLAG                                         
BCSFMXQ  EQU   X80                  ENTRY IS EFFECTIVELY THE EOTABLE            
*                                    (FOR MONTH TABLE ONLY)                     
BCSTTABQ EQU   *-BCSTTABD                                                       
         SPACE 2                                                                
*========================== REQTWA TABLE DSECT =======================*         
RQTWATBD DSECT                                                                  
RQTNUM   DS    XL1                 INTERNAL ENTRY NUMBER                        
RQTLEN   DS    XL1                 ENTRY LENGTH                                 
RQTRQTYP DS    CL2                 REQUEST TYPE                                 
RQTFDNUM DS    XL1                 # OF FIELD & DATA ENTRIES                    
RQTFLDTA DS    0C                  FIELD & DATA INFO (SEE RQFDDSCT)             
*                                                                               
RQFDDSCT DSECT                                                                  
RQFDFDSP DS    AL2                 TWA FIELD HEADER DISPLACEMENT                
RQFDDDSP DS    AL2                 DATA DISPLACEMENT FROM SYSD                  
RQFDDLN1 DS    XL1                 L(DATA) - 1                                  
RQFDLENQ EQU   *-RQFDDSCT                                                       
         SPACE 2                                                                
*============================= REQUEST DSECT =========================*         
REQDSECT DSECT                                                                  
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
REQLENQ  EQU   *-REQDSECT                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*================= LINE (ON MAINTENANCE SCREEN) DSECT ================*         
MSLDSECT DSECT                                                                  
MSLSTATH DS    CL8                                                              
MSLSTAT  DS    CL12     9    2   11  A  P N             T        Y              
MSLCBUDH DS    CL8                                                              
MSLCBUD  DS    CL7      9   16    7  A  P N             T        Y              
MSLWBUDH DS    CL8                                                              
MSLWBUD  DS    CL7      9   25    7  A  P N             T        Y              
MSLCLCKH DS    CL8                                                              
MSLCLCK  DS    CL7      9   34    7  A  P N             T        Y              
MSLWLCKH DS    CL8                                                              
MSLWLCK  DS    CL7      9   43    7  A  P N             T        Y              
MSLCCOSH DS    CL8                                                              
MSLCCOS  DS    CL7      9   52    7  A  U N             T        Y              
MSLWCOSH DS    CL8                                                              
MSLWCOS  DS    CL7      9   61    7  A  P N             T        Y              
MSLCOS2H DS    CL8                                                              
MSLCOS2  DS    CL8      9   70    7  A  U N             T        Y              
MSLLENQ  EQU   *-MSLDSECT                                                       
         DS    0CL(MSLLENQ-(C2MCOS2+L'C2MCOS2-C2MSTATH)+1)                      
         DS    0CL((C2MCOS2+L'C2MCOS2-C2MSTATH)-MSLLENQ+1)                      
MSLNEXT  DS    0C                                                               
***********************************************************************         
                                                                                
***********************************************************************         
*======================== OPTIONS TABLE DSECT  =======================*         
                                                                                
OPTDSECT DSECT                                                                  
OPTNUMB  DS    XL1                 INTERNAL OPTION NUMBER                       
OPTLEN   DS    XL1                 LENGTH OF OPTION TABLE                       
OPTBIT   DS    XL1                 OPTION BIT                                   
OPTFLAG  DS    XL1                 FLAGS ABOUT OPTION ENTRY                     
OPFOTHAB EQU   X80                  VALUES FOUND IN ANOTHER TABLE               
OPFKYWRD EQU   X40                  OPTION SPECIFIED BY KEYWORD ONLY            
OPTOLEN  DS    XL1                 OUTPUT LENGTH                                
OPTOADDR DS    XL2                 DISPL FROM SYSD OF OUTPUT FIELD              
OPTNAME  DS    0C                  AL1(L'NAME),C'NAME'                          
OPTVALS  DS    0C                  AL1(L'VALUE),C'VALUE' OR A(TABLE)            
***********************************************************************         
                                                                                
***********************************************************************         
*============================= SPOTBLOCK =============================*         
                                                                                
SBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPOTBLOCK                                                      
         PRINT ON                                                               
SBLKWRKL EQU   SBLOCKX-SBLOCKD     LENGTH FOR NMOD WORKING AREA                 
                                                                                
                                                                                
         EJECT                                                                  
*============================== SPOTTABD =============================*         
         PRINT OFF                                                              
       ++INCLUDE SPOTTABD                                                       
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (MY WRKNG STRGE)'           
***********************************************************************         
*========================= MY WORKING STORAGE ========================*         
MYWORKD  DSECT                                                                  
*                                                                               
SBACUNT  DS    F                                                                
*                                                                               
SPBKLABL DS    D                   *SPTBLK*                                     
SPBLK    DS    XL(SBLKWRKL)                                                     
SPBLKX   EQU   *                                                                
*                                                                               
CHNKLABL DS    D                   **CHUNK*                                     
CHUNK    DS    XL2500                                                           
CHUNKX   EQU   *                                                                
*                                                                               
MYPWLABL DS    D                   *PWRECD*                                     
MYPWREC  DS    XL2000                                                           
MYPWRECX EQU   *                                                                
*                                                                               
MKESLABL DS    D                   *MKESTB*                                     
MKESTTAB DS    XL256                                                            
MKESTBX  EQU   *                                                                
*                                                                               
MYWORKL  EQU   *-MYWORKD                                                        
***********************************************************************         
         TITLE 'SPSFM3E - PROFIT WITHIN MAINTENANCE (SPOT STRG AREA)'           
***********************************************************************         
*========================== SPOT STORAGE AREA ========================*         
                                                                                
*        THESE ARE STORAGE AREAS WHICH SPOTIO, SPOTBUY, AND SPOTGOAL            
*         USE.  THESE AREAS WILL BE PUT IN TIA, SINCE THEY ARE HUGE             
                                                                                
SPOTAREA DSECT                                                                  
ACTBLABL DS    D                   *ACCUTB*                                     
ACTMXNTR EQU   58                                                               
ACCUTAB  DS    XL((ACTMXNTR+1)*ACCUTABQ)  1 MORE ENTRY FOR OUR FFFFFF           
ACCUTABX EQU   *                                                                
SPTENDSV EQU   *                                                                
*                                                                               
ESTBLABL DS    D                   **ESTAB*                                     
ESTAB    DS    XL256                                                            
ESTABX   EQU   *                                                                
*                                                                               
SPTTAB   DS    200XL(SPTTABL)                                                   
SPTTABX  EQU   *                                                                
MYTIALEN EQU   *-SPOTAREA                                                       
         DS    0CL((X'4800'-MYTIALEN)+1)                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPSFM59   03/06/18'                                      
         END                                                                    
