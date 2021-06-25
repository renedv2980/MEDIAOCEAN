*          DATA SET TAREP35    AT LEVEL 068 AS OF 06/15/16                      
*PHASE T70335B,*                                                                
*INCLUDE DLFLD                                                                  
*                                                                               
       ++INCLUDE TAREP35TXT                                                     
         TITLE 'T70335 - RETROACTIVE PAYMENT GENERATOR'                         
T70335   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70335,RA,R5                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)            R7=A(LOCAL W/S)                              
         USING TRD,R7                                                           
         LA    R6,TRSRTREC         R6=A(SORT RECORD)                            
         USING SORTD,R6                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LH    RF,=AL2(TRDLNQ)     CLEAR LOCAL STORAGE                          
         XCEFL TRD                                                              
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         SPACE 1                                                                
         L     R4,ATWA             R4=A(SCREEN)                                 
         USING T703FFD,R4                                                       
         SPACE 1                                                                
         LA    R2,SCRPDH           VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TIQPSTR,PVALPSTA    SET PERIOD FOR SYSIO                         
         MVC   TIQPEND,PVALPEND                                                 
         SPACE 1                                                                
         LA    R2,SCROFFH          OFFICE                                       
         XC    SCROFFN,SCROFFN                                                  
         OI    SCROFFNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK20                                                             
*        GOTO1 RECVAL,DMCB,TLOFCDQ,(X'08',(R2)),SCROFFNH                        
         GOTO1 RECVAL,DMCB,TLOFCDQ,(R2)                                         
         MVC   TIFOFF,TGOFF                                                     
         SPACE 1                                                                
VK20     LA    R2,SCRAGYH          AGENCY                                       
         XC    SCRAGYN,SCRAGYN                                                  
         OI    SCRAGYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK30                                                             
*        GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',(R2)),SCRAGYNH                
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(R2)                                 
         CLI   8(R2),C'@'          TEST FOR LEADING 'AT' SIGN                   
         BE    *+14                                                             
         MVC   TIFAGY,TGAGY                                                     
         B     *+14                                                             
         MVC   TIFAGY,TGLST                                                     
         NI    TIFAGY,X'7F'        SET AGENCY FILTER IS FLIST RECORD            
         SPACE 1                                                                
VK30     LA    R2,SCRCLIH          CLIENT                                       
         XC    SCRCLIN,SCRCLIN                                                  
         OI    SCRCLINH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK35                                                             
*        GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCRCLINH                        
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)                                         
         MVC   TIFCLI,TGCLI                                                     
         SPACE 1                                                                
VK35     LA    R2,SCRCIDH          COMMERCIAL ID                                
         XC    SCRCIDN,SCRCIDN                                                  
         OI    SCRCIDNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK40                                                             
*        GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SCRCIDNH                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(R2)                                        
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TIFCOM,TLCOCOM      SET INTERNAL COMML NUMBER FILTER             
         SPACE 1                                                                
VK40     LA    R2,SCREMPH          EMPLOYER                                     
         XC    SCREMPN,SCREMPN                                                  
         OI    SCREMPNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK50                                                             
*        GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SCREMPNH                        
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2)                                         
         MVC   TIFEMP,TGEMP                                                     
         SPACE 1                                                                
VK50     LA    R2,SCRCURH          CURRENCY                                     
*        MVI   TIFCUR,C'U'         DEFAULT TO TAKE US$ PAYMENTS ONLY            
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         CLI   8(R2),C'U'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'C'                                                       
         BNE   FLDINV                                                           
         MVC   TIFCUR,8(R2)                                                     
         SPACE 1                                                                
VK60     LA    R2,SCRINVH          INVOICE NUMBER                               
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         GOTO1 TINVCON,DMCB,8(R2),TIQSTART,DATCON                               
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    TIQSTART(6),=6X'FF'                                              
         SPACE 1                                                                
VK70     GOTO1 =A(VALOPT),DMCB,(RC)  VALIDATE OPTIONS                           
         SPACE 1                                                                
         TM    TROPTS,TRCURRNT     IF WRITING TO CURRENT AGY/CLI/PRD            
         BZ    *+16                                                             
         LA    R2,SCRAGYH                                                       
         CLI   5(R2),0             REQUIRE INPUT IN AGENCY FIELD                
         BE    FLDMISS                                                          
         SPACE 1                                                                
         LA    R2,SCRCAYH          CURRENT AGENCY                               
         XC    SCRCAYN,SCRCAYN                                                  
         OI    SCRCAYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BNE   VK75                                                             
         TM    TROPTS,TRCURRNT     REQUIRED IF OPTION SELECTED                  
         BO    FLDMISS                                                          
         B     VK80                                                             
VK75     TM    TROPTS,TRCURRNT     ELSE INVALID                                 
         BZ    FLDINV                                                           
*        GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SCRCAYNH                        
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)                                         
         MVC   TRCAGY,TGAGY                                                     
         SPACE 1                                                                
VK80     LA    R2,SCRCCLH          CURRENT CLIENT                               
         XC    SCRCCLN,SCRCCLN                                                  
         OI    SCRCCLNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK90                                                             
*        GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCRCCLNH                        
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)                                         
         MVC   TRCCLI,TGCLI                                                     
         SPACE 1                                                                
VK90     LA    R2,SCRCCOH          CURRENT COMMERCIAL ID                        
         XC    SCRCCON,SCRCCON                                                  
         OI    SCRCCONH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK100                                                            
*        GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SCRCCONH                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(R2)                                        
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TIFCOM,TLCOCOM      SET INTERNAL COMML NUMBER FILTER             
         SPACE 1                                                                
VK100    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         BAS   RE,INIT             INTIALIZE                                    
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
         TM    TRSTAT,TRADDINV     TEST INVOICE ADD PENDING                     
         BZ    *+8                                                              
         BAS   RE,ADDINV           ADD LAST INVOICE LEVEL SORT RECORD           
         SPACE 1                                                                
         OPEN  (ERRFILE,OUTPUT)    OPEN ERROR FILE                              
         SPACE 1                                                                
         TM    TROPTS,TRWRIDSK     IF NOT WRITING TO DISK                       
         BO    PR10                                                             
         OPEN  (TALTAPE,OUTPUT)    OPEN TAPES                                   
         OPEN  (CHKTAPE,OUTPUT)                                                 
         SPACE 1                                                                
PR10     BAS   RE,REPORT           READ SORT RECS - PRINT REPORT                
                                                                                
         CLOSE ERRFILE             CLOSE ERROR FILE                             
                                                                                
         TM    TROPTS2,TRDOWN                                                   
         BZ    PR20                                                             
         BRAS  RE,ENDDOWN                                                       
                                                                                
PR20     TM    TROPTS,TRWRIDSK     IF NOT WRITING TO DISK                       
         BO    PRX                                                              
         CLOSE TALTAPE             CLOSE TAPES                                  
         CLOSE CHKTAPE                                                          
         SPACE 1                                                                
PRX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES FOR REPORT                               
         SPACE 1                                                                
INIT     NTR1                                                                   
         L     R1,=A(SAVERC)                                                    
         ST    RC,0(R1)            SAVE RC FOR HEADHOOK                         
                                                                                
         TM    TROPTS2,TRDOWN      DOWNLOAD?                                    
         BO    INIT20                                                           
         MVC   HEADHOOK,=A(HOOK)   SET A(HEADLINE HOOK)                         
         MVC   SPECS,=A(MYSPECS)                                                
         B     INIT50                                                           
                                                                                
INIT20   DS    0H                                                               
                                                                                
INIT50   MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         L     R4,ATWA             R4=A(SCREEN)                                 
         USING T703FFD,R4                                                       
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVC   TIQSTAFF,TGCTSTAF   STAFF ID                                     
         MVI   TIREAD,TLINCDQ      READ ACTIVE INVOICE RECORDS                  
         OC    TIFCOM,TIFCOM       IF FILTERING ON COMMERCIAL                   
         BNZ   *+12                                                             
         TM    TROPTS,TRCURRNT     OR WRITING TO CURRENT AGY/CLI/PRD            
         BZ    *+8                                                              
         MVI   TIREAD,TLINHCDQ     SET TO READ COMML HISTORY POINTERS           
         MVI   TISUBRD,TLCKCDQ     SUB-READ CHECK RECORDS                       
         MVI   TIQDTYPE,TIQDPAY    SET FILTERING ON PAY DATE                    
**       TM    TROPTS2,TRCABLE     IF NOT RUNNING CABLE OPTION                  
**       BO    *+10                                                             
**       MVC   TIFYEAR,=C'97 '     SET FILTERING ON UNION YEAR 97               
         TM    TROPTS2,TRCLA1      IF RUNNING CLA OPTION                        
         BZ    *+10                                                             
         MVC   TIFUSE,=C'CLA'      SET FILTERING ON CLA USE                     
         MVC   TIFUN,=C'RET'       SET RETRO UNION FLIST RECORD                 
         NI    TIFUN,X'7F'         TURN OFF X'80' BIT TO INDICATE FLIST         
         MVI   TIFCTYPE,CTYSOAP                                                 
         NI    TIFCTYPE,X'BF'      SET ALL COMML TYPES EXCEPT SOAPS             
         MVI   TIFINSTN,TAINSCIN+TAINSCAN  IGNORE CANCELLED INVOICES            
         OI    TIFPDSN,TAPDSCNL    IGNORE CANCELLED (PAYMENT STATUS)            
         SPACE 1                                                                
*        OC    TIFAGY,TIFAGY       IF RUNNING FOR ALL AGENCIES                  
*        BNZ   *+14                                                             
*        MVC   TIFAGY,=C'RETXAY'   THEN SET EXCLUDE AGENCY FLIST RECORD         
*        NI    TIFAGY,X'3F'        TURN OFF X'80'/FLIST & X'40'/EXCLUDE         
         SPACE 1                                                                
**NO-OP* TM    TROPTS,TRPHONLY     IF RUNNING P&H ONLY OPTION                   
**NO-OP* BZ    *+14                                                             
**NO-OP* MVC   TIFUN,=C'AFT'       SET UNION AFT                                
**NO-OP* MVI   TIFMED,TACOMEDR     AND MEDIA RADIO                              
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A8D'  LOAD RATE CALC. MODULE                
         MVC   TRACALC,0(R1)                                                    
         SPACE 1                                                                
         XC    TGAGY,TGAGY         PRE-CLEAR GLOBAL AGENCY                      
         XC    TGCOM,TGCOM                   GLOBAL INT. COMML NUMBER           
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO                                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCINV      PROCESS INVOICE                              
         BNE   IOH600                                                           
         L     R4,TIAREC           R4=A(RECORD)                                 
         USING TLIND,R4                                                         
         CLI   TLINCD,TLINCDQ      INSURE WE HAVE INVOICE RECORD                
         BNE   IOH20                                                            
         TM    TRSTAT,TRADDINV     TEST INVOICE ADD PENDING                     
         BZ    *+8                                                              
         BAS   RE,ADDINV           ADD INVOICE LEVEL SORT RECORD                
         SPACE 1                                                                
         CLC   TIAGY,TGAGY         IF AGENCY CHANGED                            
         BE    *+12                                                             
         BAS   RE,IOAGY            HANDLE FIRST FOR AGENCY                      
         BNE   IOH20                                                            
         SPACE 1                                                                
         CLC   TICOM,TGCOM         IF INTERNAL COMMERCIAL NUMBER CHG'D          
         BE    *+12                                                             
         BAS   RE,IOCOM            HANDLE FIRST FOR COMMERCIAL                  
         BNE   IOH20                                                            
         SPACE 1                                                                
         TM    TRSTAT,TRREREAD     TEST WE NEED TO RE-READ SYSIO'S KEY          
         BZ    IOH10                                                            
         XI    TRSTAT,TRREREAD     TURN IT OFF                                  
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                AND RE-READ                                  
         SPACE 1                                                                
IOH10    OC    TIQSTART(6),TIQSTART  IF INVOICE FILTER INPUT                    
         BZ    IOH30                                                            
         MVC   WORK(6),TIQSTART    SET TO FILTER                                
         XC    WORK(6),=6X'FF'                                                  
         CLC   TIINV,WORK          IF THIS IS NOT CORRECT INVOICE               
         BE    IOH30                                                            
         SPACE 1                                                                
IOH20    LA    R4,KEY              FORCE SYSIO TO SKIP TO NEXT HIGH LVL         
         USING TLIND,R4                                                         
         MVC   TLINKEY,TIKEY                                                    
         CLI   TLINCD,TLINHCDQ     IF NOT READING VIA COMMERCIAL                
         BE    *+14                                                             
         MVC   TLININV,=6X'FF'     SET TO SKIP TO NEXT AGENCY                   
         B     *+10                                                             
         USING TLINPD,R4                                                        
         MVC   TLINHINV,=6X'FF'    ELSE SET TO SKIP TO NEXT COMMERCIAL          
         GOTO1 HIGH                                                             
         MVI   TIMODE,PROCNOCK                                                  
         B     IOHX                                                             
         SPACE 1                                                                
IOH30    MVC   SORTOFF,TIOFF       BUILD SORT RECORD - OFFICE                   
         MVC   SORTAGY,TIAGY                           AGENCY                   
         MVC   SORTCLI,TICLI                           CLIENT                   
         MVC   SORTPRD,TIPRD                           PRODUCT                  
         SPACE 1                                                                
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    IOH40                                                            
         MVC   SORTOFF,TROFF       USE SAVED VALUES - OFFICE                    
         MVC   SORTAGY,TRAGY                          AGENCY                    
         MVC   SORTCLI,TRCLI                          CLIENT                    
         MVC   SORTPRD,TRPRD                          PRODUCT                   
         SPACE 1                                                                
IOH40    MVC   SORTCOM,TICOM       SET INT COMML NO.                            
         MVC   SORTINV,TIINV           INVOICE                                  
         SPACE 1                                                                
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTEST  GET ESTIMATE NUMBER             
         MVC   TRINEST,TGNAME                                                   
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         MVC   TRAINV,TIAREC       SET A(INVOICE RECORD)                        
         MVC   TRINVDA,TIDSKADD    SAVE D/A OF INVOICE RECORD                   
                                                                                
         BRAS  RE,INVSTAT          INVOICE FROM VITA / HAS TIMESHEETS?          
                                                                                
         BRAS  RE,RETMFIXD         RETRO MANUALLY FIXED?                        
                                                                                
         BRAS  RE,FILTINV          MAY BE ABLE TO FILTER OUT THIS INV.          
         BNE   IOHX                IN ERROR ALREADY, SO DON'T CALC              
         TM    SORTSTAT,SORTMFIX                                                
         BO    IOHX                                                             
                                                                                
         BAS   RE,CALCINV          PREPARE FOR RATE CALCULATION                 
         B     IOHX                                                             
*=====================================================================          
IOH600   CLI   TIMODE,PROCREC      PROCESS CHECK                                
         BNE   IOHX                                                             
         MVC   TRACHK,TIAREC       SET A(CHECK RECORD)                          
                                                                                
         USING TLCKD,R4                                                         
         LA    R4,TIKEY                                                         
         MVC   TGSSN,TLCKSSN                                                    
                                                                                
         TM    SORTSTAT,SORTMFIX                                                
         BZ    IOH640                                                           
                                                                                
         LA    RF,TRNITAB                                                       
IOH605   ST    RF,TRNIPTR                                                       
         CLI   0(RF),X'FF'                                                      
         BE    IOHX                                                             
         CLI   0(RF),0                                                          
         BE    IOHX                                                             
                                                                                
         MVC   SORTMINV,0(RF)                                                   
         MVC   SORTFIXN,6(RF)                                                   
                                                                                
         LA    R4,KEY                                                           
         MVC   TRSVKEY,TIKEY       SAVE KEY TO RESTORE SEQUENCE                 
         XC    KEY,KEY                                                          
         MVC   KEY,TIKEY                                                        
         MVC   TLCKINV,SORTMINV                                                 
                                                                                
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(TLCKLEN-TLCKD),KEYSAVE    IF CHECKS MATCH, OK                
         BE    IOH630                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         GOTO1 HIGH                                                             
         B     IOH620                                                           
IOH610   GOTO1 SEQ                                                              
IOH620   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         BNE   IOH800                                                           
         CLC   TLCKSORT+4(L'TLCKSORT-4),TIKEY+(TLCKSORT+4-TLCKD)                
         BNE   IOH610                                                           
                                                                                
IOH630   MVC   TIKEY,KEY                                                        
         MVC   TIDSKADD,KEY+(TLDRDA-TLDRD)                                      
                                                                                
         MVC   AIO,TIAREC                                                       
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO1                                                         
         BRAS  RE,RETRCHK          PUT AMOUNTS IN SORTD                         
         B     IOH680                                                           
                                                                                
IOH640   DS    0H                                                               
         BAS   RE,CALCCHK          CALCULATE CHECK                              
         BNE   IOHX                                                             
         TM    TROPTS,TRPHONLY     IF NOT RUNNING P&H ONLY OPTION               
         BO    IOH650                                                           
         TM    TGUSSTA2,NORATES    AND NO RATES DEFINED IN TABLE                
         BO    IOH700              DON'T BOTHER ADDING CHECK LEVEL              
         B     IOH680              ELSE ADD CHECK LEVEL                         
         SPACE 1                                                                
*                                  RUNNING P&H ONLY OPTION                      
IOH650   TM    TROPTS2,TRNORATE    IF OPTION NORATE                             
         BZ    IOH670                                                           
         TM    TGUSSTA2,NORATES    OK TO ADD CHECK LEVEL IF NORATE              
         BO    IOH670                                                           
         TM    TRSTAT,TROVSCAM     OR IF OVSC AMT ON CAST                       
         BO    IOH670                                                           
         CLI   TGUSEQU,UBSR        OR IF RADIO SESSION                          
         BNE   IOH700                                                           
         CLI   SORTCSEQ,SORTCZER   AND FTRACK BALANCE IS ZERO                   
         BE    IOH670                                                           
         B     IOH700              ELSE DON'T ADD CHECK LEVEL BECAUSE           
*                                  ALREADY PROCESSED IN RUN W/O NORATE          
*                                                                               
IOH670   MVI   SORTCSEQ,SORTCSOK   RESET SEQUENCE FOR P&H ONLY                  
*                                                                               
IOH680   BAS   RE,ADDCHK           ADD CHECK LEVEL SORT RECORD                  
*                                                                               
IOH700   OI    TRSTAT,TRADDINV     SET INVOICE ADD PENDING                      
                                                                                
IOH800   TM    SORTSTAT,SORTMFIX                                                
         BZ    XIT                                                              
IOH900   MVC   KEY,TRSVKEY                                                      
         GOTO1 HIGH                                                             
         MVC   TIKEY,KEY                                                        
         MVC   TIDSKADD,KEY+(TLDRDA-TLDRD)                                      
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         L     RF,TRNIPTR          GET NEXT ONE IF THERE IS ANY                 
         AHI   RF,7                                                             
         B     IOH605                                                           
                                                                                
IOHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE NEW AGENCY DURING IOHOOK                       
         SPACE 1                                                                
IOAGY    NTR1                                                                   
         MVC   TGAGY,TIAGY         SET NEW AGENCY                               
         SPACE 1                                                                
         OI    TRSTAT,TRREREAD     SET WE'LL NEED TO REREAD SYSIO'S KEY         
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',0)  READ NEW AGENCY                   
         BNE   NO                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
*                                                                               
         TM    TROPTS,TRFORCE      IF OPTION TO FORCE ALL AGYS NOT REQ          
         BO    *+12                                                             
         TM    TAAYSTA2,TAAYSRET   IF ALREADY PROCESSED RETROS                  
         BO    NO                  THEN SET TO SKIP TO NEXT AGENCY              
*                                                                               
         SPACE 1                                                                
         MVC   TROFF,TAAYTPOF      SAVE CURRENT TP OFFICE                       
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE NEW COMMERCIAL DURING IOHOOK                   
         SPACE 1                                                                
IOCOM    NTR1                                                                   
         MVC   TGCOM,TICOM         SET NEW COMMERCIAL                           
         SPACE 1                                                                
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    IOCX                                                             
         OI    TRSTAT,TRREREAD     SET WE'LL NEED TO REREAD SYSIO'S KEY         
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',0)  READ NEW COMMERCIAL              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         CLC   TRCAGY,TLCOAGY      INSURE CURRENT AGENCY MATCHES                
         BNE   NO                                                               
         OC    TRCCLI,TRCCLI       IF CURRENT CLIENT REQUESTED                  
         BZ    *+14                                                             
         CLC   TRCCLI,TLCOCLI      INSURE CURRENT CLIENT MATCHES                
         BNE   NO                                                               
         MVC   TRAGY,TLCOAGY       SAVE CURRENT AGENCY                          
         MVC   TRCLI,TLCOCLI                    CLIENT                          
         MVC   TRPRD,TLCOPRD                    PRODUCT                         
         SPACE 1                                                                
IOCX     B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP FOR RATE CALCULATION AT INVOICE LEVEL          
         SPACE 1                                                                
CALCINV  NTR1                                                                   
         XC    TCUDETS(TCUDTLNQ),TCUDETS  INITIALIZE RATE CALC. FIELDS          
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     IF RUNNING P&H ONLY OPTION                   
         BZ    CINV5                                                            
         TM    TROPTS2,TRCLA1      AND IF RUNNING CLA OPTION                    
         BZ    CINVX                                                            
         L     R4,TRAINV                                                        
         MVI   ELCODE,TANDELQ      GET CLA DETAILS ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANDD,R4                                                         
         LH    R1,TANDSTUS         STARTING USE NUMBER FOR MAIN                 
         AH    R1,TANDSTUL         + STARTING USE NUMBER FOR LIFT               
         SH    R1,=H'2'            - 2                                          
         BNM   *+6                                                              
         XR    R1,R1                                                            
         STH   R1,TRNUSES          = TOTAL N'USES PAID PREVIOUSLY               
         SPACE 1                                                                
         LH    R1,TANDSTUL         STARTING USE NUMBER FOR LIFT                 
         SH    R1,=H'1'            - 1                                          
         BNM   *+6                                                              
         XR    R1,R1                                                            
         STH   R1,TRNUSESL         = N'USES PAID PREVIOUSLY TO LIFT             
         B     CINVX                                                            
         SPACE 1                                                                
CINV5    L     R4,TRAINV                                                        
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMML DETAILS ELEMENT)                  
         MVC   TRTACOEL,TACOEL     SAVE IT IN LOCAL W/S                         
         LA    R1,TRTACOEL                                                      
         ST    R1,TCATACO          PASS ITS ADDRESS TO RATE CALC.               
         SPACE 1                                                                
         GOTO1 MEDVAL,DMCB,TACOMED VALIDATE MEDIA                               
         SPACE 1                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TALFD,R4            R4=A(LIFT DETAILS ELEMENT)                   
         MVC   TCLFTSEC,TALFSEC    SAVE L'LIFT                                  
         SPACE 1                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         SPACE 1                                                                
*&&DO                                                                           
         CLC   TAPDUSE,=C'CAB'     IF USE TYPE WAS CAB                          
         BNE   CINV10                                                           
         MVC   TAPDUSE,=C'CBL'     IT'S NOW CBL                                 
         MVI   TAPDTYPE,0                                                       
         MVC   TAPDUNIT,=H'1000'   WITH MAXIMUM UNITS                           
         B     CINV20                                                           
         SPACE 1                                                                
CINV10   CLC   TAPDUSE,=C'CBL'     IF USE TYPE IS CBL                           
         BNE   CINV20                                                           
         CLC   TAPDUNIT,=H'3000'   BUT LESS THAN NEW MAX                        
         BNL   CINV20                                                           
         MVC   TAPDUNIT,=H'3000'   SET NEW MAXIMUM UNITS                        
         SPACE 1                                                                
*&&                                                                             
CINV20   GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         SPACE 1                                                                
         MVC   TCUNITS,TAPDUNIT                                                 
         MVC   TCMAJORS,TAPDMAJ                                                 
         MVC   TCDEMO,TAPDDEMS                                                  
         MVC   TCTAGS,TAPDTAGS                                                  
         MVC   TCINSRTS,TAPDINS                                                 
         SPACE 1                                                                
         TM    TGUSTYST,UPGRADE    IF THIS IS AN UPGRADE                        
         BZ    *+8                                                              
         BAS   RE,SETUPG           SET UPGRADE DETAILS                          
         SPACE 1                                                                
         CLI   TGUSEQU,UCLA        IF USE TYPE IS CLA                           
         BNE   *+8                                                              
         BAS   RE,SETICLA          SET INVOICE LEVEL CLA DETAILS                
CINVX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UPGRADE DETAILS                                   
         SPACE 1                                                                
SETUPG   NTR1                                                                   
         GOTO1 UPGRVAL,DMCB,TGUSCDE,TGUSTYP  SET GLOBAL UPGRADE VALUES          
         SPACE 1                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAUPELQ      GET UPGRADE DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   SUPGX                                                            
         USING TAUPD,R4                                                         
         LA    R3,TCTAUHEL         USE IT TO BUILD USAGE HISTORY EL.            
         USING TAUHD,R3                                                         
         CLI   TGUSEQU,UWSP        FOR WILDSPOT UPGRADES                        
         BNE   SUPG4                                                            
         MVC   TAUHUNT,TAUPIUNT    SET INITIAL UNITS                            
         MVC   TAUHMAJ,TAUPIMAJ            AND MAJORS                           
         B     SUPGX                                                            
SUPG4    CLI   TGUSEQU,UIFB        FOR IFB UPGRADES                             
         BNE   SUPG5                                                            
         MVC   TAUHINS,TAUPIINS    SET INITIAL INSERTS                          
         B     SUPGX                                                            
SUPG5    CLI   TGUSEQU,UCBL        FOR CBL UPGRADES                             
         BNE   SUPG10                                                           
         MVC   TAUHCBUN,TAUPICBU   SET INITIAL UNITS                            
         B     SUPGX                                                            
SUPG10   CLI   TGUSEQU,ULCB        FOR LCB UPGRADES                             
         BNE   SUPGX                                                            
*        MVC   TAUHLCBM,TAUPILCM   SET INITIAL MAJORS                           
         SPACE 1                                                                
SUPGX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET INVOICE LEVEL CLA USE DETAILS                     
         SPACE 1                                                                
SETICLA  NTR1                                                                   
         L     R4,TRAINV                                                        
         MVI   ELCODE,TANDELQ      GET CLA DETAILS ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANDD,R4                                                         
         LH    R1,TANDUSEL         N'USES TO LIFT                               
         STH   R1,TCTUSESL         = TOTAL N'USES PAID TO LIFT                  
         AH    R1,TANDUSES         + N'USES TO MAIN                             
         STH   R1,TCTUSES          = TOTAL N'USES PAID                          
         SPACE 1                                                                
         LH    R1,TANDSTUS         STARTING USE NUMBER FOR MAIN                 
         AH    R1,TANDSTUL         + STARTING USE NUMBER FOR LIFT               
         SH    R1,=H'2'            - 2                                          
         BNM   *+6                                                              
         XR    R1,R1                                                            
         STH   R1,TRNUSES          = TOTAL N'USES PAID PREVIOUSLY               
         SPACE 1                                                                
         LH    R1,TANDSTUL         STARTING USE NUMBER FOR LIFT                 
         SH    R1,=H'1'            - 1                                          
         BNM   *+6                                                              
         XR    R1,R1                                                            
         STH   R1,TRNUSESL         = N'USES PAID PREVIOUSLY TO LIFT             
         SPACE 1                                                                
         MVC   TCUSETAB(3),TCPCYCS SET CYCLE START AS DEFAULT USE DATE          
         OI    TCOPTS,TCONUMUS     DEFAULT IS N'USES OVERRIDDEN                 
         OC    TCTUSESL,TCTUSESL   IF ANY USES TO LIFT                          
         BZ    *+8                                                              
         OI    TCUSETAB+3,TCLFTPRO SET USE TABLE DEFAULT TO LIFT                
         SPACE 1                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TANPELQ      GET CLA PROGRAM DETAILS ELEMENTS             
         BAS   RE,GETEL                                                         
         BNE   SICLAX                                                           
         NI    TCOPTS,ALL-TCONUMUS                                              
         USING TANPD,R4                                                         
         LA    R3,TCUSETAB         SET TO BUILD USE TABLE FOR RATE CALC         
         SPACE 1                                                                
SICLA60  MVC   0(3,R3),TANPDATE    SET USE DATE                                 
         MVC   3(1,R3),TANPLFT     AND LIFT STATUS                              
         LA    R3,4(R3)            BUMP USE TABLE                               
         BAS   RE,NEXTEL                                                        
         BE    SICLA60             HANDLE ADD'L ELEMENTS                        
         SPACE 1                                                                
SICLAX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CALCULATE NEW CHECK                                   
         SPACE 1                                                                
CALCCHK  NTR1                                                                   
         L     R4,TRACHK                                                        
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
                                                                                
         CLC   TGSSN,=C'000004938'   AFTRA FOUNDATION OR                        
         BE    CCHK3                                                            
         CLC   TGSSN,=C'953967876'   SAG FOUNDATION                             
         BE    CCHK3                                                            
                                                                                
         CLC   TACAUN,=C'SAG'      ONLY SAG AND AFTRA                           
         BE    CCHK3                                                            
         CLC   TACAUN,=C'AFT'                                                   
         BNE   CCHK4                                                            
CCHK3    CLC   TACAYEAR,=C'16 '    SKIP IF NOT 16 UNION YRS                     
         BE    CCHK5                                                            
                                                                                
CCHK4    TM    TRSTAT,ACT04AB      HAS TO BE 2404 COMML TO USE ACT              
         BZ    CCHKNO                                                           
         CLC   TACAUN,=C'ACT'      OR ACTRA/14 FOR 2404 COMML                   
         BNE   CCHKNO                                                           
         CLC   TACAYEAR,=C'14 '                                                 
         BNE   CCHKNO                                                           
*                                                                               
CCHK5    TM    TROPTS2,TRSESS      IF RUNNING SESS OPTION                       
         BZ    CCHK8                                                            
         L     R4,TRACHK                                                        
         MVI   ELCODE,TASDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CCHKNO                                                           
         USING TASDD,R4            R4=A(SESSION DETAILS ELEMENT)                
         CLC   SORTUSE,=C'BSR'     IF BSR USE                                   
         BNE   CCHK6                                                            
         ZIC   R1,TASDRSP          IF N'SPOTS > 1 THEN OK                       
         CH    R1,=H'1'                                                         
         BH    CCHK8                                                            
         LH    R1,TASDRHM          OR IF HR/MIN > 130 THEN OK                   
         CH    R1,=H'130'                                                       
         BH    CCHK8                                                            
         OC    TASDRTG,TASDRTG                                                  
         BNZ   CCHK8               OR IF HAVE TAGS, THEN OK                     
         B     CCHKNO                                                           
         SPACE                                                                  
CCHK6    ZIC   R1,TASDSP           IF N'SPOTS > 1 THEN OK                       
         CH    R1,=H'1'                                                         
         BH    CCHK8                                                            
         ZIC   R1,TASDDAY          OR IF N'DAYS > 1 THEN OK                     
         CH    R1,=H'1'                                                         
         BH    CCHK8                                                            
         OC    TASDOT(TASDTAA-TASDOT),TASDOT OK IF HAVE OTHER XTRA DET          
         BZ    CCHKNO                                                           
         SPACE 1                                                                
CCHK8    NI    TRSTAT,ALL-TROVSCAM-TRCLAOV1  CLEAR CAST RELATED FLAGS           
         XC    SORTCHK(SORTCHKL),SORTCHK     INIT CHECK-RELATED FLDS            
         MVI   SORTCSEQ,SORTCSOK                                                
         SPACE 1                                                                
         TM    TROPTS2,TREXTRAS    IF ONLY WANT EXTRAS - CHECK LATER            
         BO    *+12                   IN CASE NO EXTRAS ON PAYMENT              
         CLI   SORTERR,0           IF ERROR STATUS ALREADY SET                  
         BE    CCHK8C                                                           
*        TM    TROPTS2,TRFIX7                                                   
*        BO    CCHK8C                                                           
*        TM    TROPTS3,TRFIX7B                                                  
*        BO    CCHK8C                                                           
         B     CCHKX               DON'T BOTHER CALCULATING CHECK               
         SPACE 1                                                                
CCHK8C   L     R4,TRACHK                                                        
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4            R4=A(CHECK DETAILS ELEMENT)                  
         TM    TACDSTAT,TACDSLIN                                                
         BO    CCHKNO              DON'T WANT LIEN PAYEE CHECKS                 
         TM    TACDSTAT,TACDSTRS                                                
         BO    CCHKNO              DON'T WANT TRUSTEE CHECKS                    
         SPACE 1                                                                
         XC    TCCAST(TCCSTLNQ),TCCAST  INITIALIZE RATE CALC. FIELDS            
         SPACE 1                                                                
         L     R4,TRACHK                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         SPACE 1                                                                
         TM    TAPDSTAT,TAPDSMAN   MANUAL OVERRIDE?                             
         BZ    CCHK9Z                                                           
         LR    R2,R4               SAVE R4                                      
         L     R4,TRACHK           SEE IF NO CHECK COMMENT                      
         USING TACMD,R4            R4=A(COMMENT ELEMENT)                        
         MVI   ELCODE,TACMELQ                                                   
         BAS   RE,GETEL                                                         
         B     CCHK8G                                                           
CCHK8E   BAS   RE,NEXTEL                                                        
CCHK8G   BNE   CCHK8J                                                           
         CLI   TACMTYPE,TACMTYPC   CHECK COMMENT                                
         BNE   CCHK8E                                                           
         B     CCHK9X                                                           
                                                                                
CCHK8J   MVI   SORTERR,ERR11       MANUAL OVERRIDE, NO CHK CMT                  
                                                                                
CCHK9X   LR    R4,R2               RESTORE R4 TO TAPDEL                         
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
CCHK9Z   TM    TROPTS,TRPHONLY     IF RUNNING P&H ONLY OPTION                   
         BZ    CCHK10                                                           
         BAS   RE,PNHONLY          PROCESS IN SPECIAL ROUTINE                   
         BNE   CCHKNO                                                           
         B     CCHKX               THAT'S IT                                    
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
CCHK10   TM    TROPTS2,TRSESS      SKIP IF SESS OPTN - DON'T WANT ERR9          
         BO    CCHK30                                                           
         OC    TAPDAPPL,TAPDAPPL   IF THERE WERE APPLIED CRS ORIGINALLY         
         BZ    CCHK30                                                           
         CLI   TAPDACDE,APPLSESS   AND APPLIED CODE WAS SESSION                 
         BE    CCHK24                                                           
         CLI   TAPDACDE,APPLHLD                     OR HLD                      
         BE    CCHK24                                                           
         CLI   TAPDACDE,APPLGUAR   OR IF APPLIED CODE IS GUARANTEE              
         BNE   CCHK30                                                           
         TM    TGUSTYST,UPGRADE    AND THIS IS NOT AN UPGRADE                   
         BO    CCHK30                                                           
         SPACE 1                                                                
CCHK24   XC    SORTINV,SORTINV     ADD DUMMY SORT REC ==> REUSE W/ CRS          
         BAS   RE,ADDCHK                                                        
         MVC   SORTINV,TIINV       RESTORE INVOICE NUMBER                       
         SPACE 1                                                                
CCHK30   TM    TGUSSTA2,NORATES    IF NO RATES DEFINED IN TABLE                 
         BO    CCHKX               HAVEN'T SET ERROR YET, BUT GET OUT           
         SPACE 1                                                                
         BAS   RE,SETTC            SET RATE CALCULATION VARIABLES               
         BNE   CCHKNO                                                           
         SPACE 1                                                                
         TM    TROPTS2,TREXTRAS    IF ONLY WANT EXTRAS                          
         BZ    *+12                                                             
         CLI   SORTERR,0           IF ERROR STATUS ALREADY SET                  
         BNE   CCHKX               DON'T BOTHER CALCULATING CHECK               
         SPACE 1                                                                
         BAS   RE,GETCAST          GET CAST RECORD                              
         BNE   CCHKNO                                                           
         SPACE 1                                                                
         BAS   RE,GETRATE          GET NEW AND OLD RATES                        
         BNE   CCHKNO                                                           
         SPACE 1                                                                
*&&UK                                                                           
         CLI   TGUSEQU,UBSS        IF TV SESSION                                
         BE    CCHK34                                                           
         CLI   TGUSEQU,USSS        OR SPANISH SESSION                           
         BE    CCHK34                                                           
         CLI   TGUSEQU,ULFT        OR LIFT                                      
         BE    CCHK34                                                           
         CLI   TGUSEQU,USLF        OR SPANISH LIFT                              
         BE    CCHK34                                                           
         CLI   TGUSEQU,UBSR        IF RADIO SESSION                             
         BNE   CCHK35                                                           
CCHK34   CLI   SORTCSEQ,SORTCZER   AND FTRACK BALANCE IS ZERO                   
         BE    CCHKNO              IGNORE - SEE COMMENT IN APPLY                
*&&                                                                             
         SPACE 1                                                                
CCHK35   L     R1,TRNEW            CALC NEW RATE LESS OLD RATE                  
         S     R1,TROLD            R1=RETRO PAYMENT AMOUNT                      
         BP    CCHK60              BRANCH IF POSITIVE                           
         BZ    CCHK50              OR ZERO                                      
         SPACE 1                                                                
         CLI   TGUSEQU,UDEM        IF THIS IS A DEMO PAYMENT                    
         BNE   CCHK40                                                           
         MVI   BYTE,UBSS           SET TO TRY USING SESSION RATES               
         TM    TGMEEQU,RADIO                                                    
         BZ    *+8                                                              
         MVI   BYTE,UBSR                                                        
         GOTO1 USEVAL,DMCB,(X'C0',BYTE),0                                       
         SPACE 1                                                                
         BAS   RE,GETRATE          GET NEW AND OLD RATES AGAIN                  
         SPACE 1                                                                
         MVI   BYTE,UDEM           RESTORE DEM TO GLOBAL                        
         GOTO1 USEVAL,DMCB,(X'C0',BYTE),0                                       
         SPACE 1                                                                
         L     R1,TRNEW            CALC NEW RATE LESS OLD RATE                  
         S     R1,TROLD            R1=RETRO PAYMENT AMOUNT                      
         BP    CCHK60              BRANCH IF POSITIVE                           
         BZ    CCHK50              OR ZERO                                      
         SPACE 1                                                                
         USING ACCD,R3                                                          
CCHK40   MVI   SORTERR,ERR7        PAYMENT IS NEGATIVE                          
         TM    TROPTS2,TRFIX7      FIX ERROR 7'S                                
         BZ    CCHK55                                                           
         L     R1,TROLD            JUST DO P&H RATE INC ON THIS                 
         LA    R3,SORTACCS                                                      
         ST    R1,ACCSPNH          ADD ORIGINAL PAY TO SBJ PNH ACCUM            
         B     CCHK80                                                           
CCHK50   MVI   SORTERR,ERR6        PAYMENT IS ZERO                              
CCHK55   TM    TROPTS,TROKNEG      IF OPTION TO REPORT NEG/ZERO INV'S           
         BZ    CCHKX               NOT REQUESTED THEN GET OUT NOW               
         MVI   SORTERR,0           ELSE CLEAR ERROR AND CONTINUE                
         B     CCHK70              MAX PERCENT TEST NOT APPROPRIATE             
         SPACE 1                                                                
CCHK60   CLI   TGUSEQU,UCBL        IF NOT CABLE PAYMENT                         
         BE    CCHK70                                                           
         L     RE,TROLD            TEST WHETHER RETRO PAYMENT IS MORE           
         TM    TROPTS2,TRSESS        IF RUNNING SESS OPTION                     
         BZ    *+8                                                              
         L     RE,TRSVSPNH           USE SAVED ORIG PYMT AMT                    
         MH    RE,=H'16'           THAN 16 PERCENT INCREASE OVER                
         LR    R0,R1               ORIGINAL PAYMENT                             
         MH    R0,=H'100'                                                       
         CR    R0,RE                                                            
         BNH   CCHK70                                                           
         CLC   TGCAT,=C'GD9'       SKIP THIS CHECK FOR GD9                      
         BE    CCHK70                                                           
         MVI   SORTERR,ERR8                                                     
         B     CCHKX                                                            
         SPACE 1                                                                
CCHK70   BAS   RE,SETACCS          SET ACCUMS IN SORT REC (PASS R1=PMT)         
*                                                                               
*        TM    TROPTS,TRBOTH       IF BOTH OPTION                               
*        BZ    CCHKX                                                            
                                                                                
         LA    R3,SORTACCS                                                      
         ICM   R1,15,TAPDSPNH                                                   
         BZ    CCHKX                                                            
CCHK80   STCM  R1,15,ACCOSPNH      SAVE ORIGINAL SUBJ TO PNH                    
         LH    R0,=H'0120'         1800-1680=0120, P&H RATE INCREASE            
         BAS   RE,MULT                                                          
         ST    R1,ACCPNHDF                                                      
         TM    TROPTS,TRBOTH       IF BOTH OPTION                               
         BZ    CCHKX                                                            
         A     R1,ACCPNH                                                        
         ST    R1,ACCPNH           ADD INCREASE TO PNH ACCUM                    
                                                                                
         TM    TROPTS2,TRFIX7      FIX ERROR 7'S                                
         BZ    CCHK90                                                           
         CLI   SORTERR,ERR7                                                     
         BE    CCHKX                                                            
CCHK90   ICM   R1,15,TAPDSPNH                                                   
         BZ    CCHKX                                                            
         A     R1,ACCSPNH                                                       
         ST    R1,ACCSPNH          ADD ORIGINAL PAY TO SBJ PNH ACCUM            
CCHKX    B     YES                                                              
         SPACE 1                                                                
CCHKNO   B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS CHECKS FOR PNHONLY OPTION - SETS CC           
*              R4=A(TAPD EL)                                                    
         SPACE                                                                  
         USING TAPDD,R4                                                         
PNHONLY  NTR1                                                                   
         ICM   R1,15,TAPDSPNH      SET R1=SUBJ. TO P&H                          
         BZ    NO                  (DON'T BOTHER IF NOTHING SUBJ P&H)           
         SPACE                                                                  
         TM    TROPTS2,TRCLA1      IF RUNNING CLA OPTION                        
         BZ    PNHO10                                                           
         L     R4,TRACHK                                                        
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SETCCLA                                                       
         BE    NO                  SET CC NO IF PAYING FIRST USE                
         SPACE 1                   (ALREADY DONE BY BOTH RUN)                   
PNHO10   L     R4,TRACHK                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TCPCYC,TAPDCYCS     SET CYCLE DATES FOR GETCAST                  
         MVC   TCPNHR,=H'0120'     1800-1680=0120, P&H RATE INCREASE            
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         BAS   RE,PNHCALC          CALCULATE P&H                                
         L     R4,TRACHK                                                        
         USING TLCKD,R4            R4=A(CHECK RECORD)                           
         MVC   TGSSN,TLCKSSN       SET TG INFO FOR CAST RECORD                  
         MVC   TGCAT,TLCKCAT                                                    
         MVC   TGCSORT,TLCKSORT                                                 
         BAS   RE,GETCAST          GET CAST RECORD AND INFO                     
         BE    *+8                                                              
         OI    TRSTAT,TROVSCAM     IF CC NOT EQ, SET BIT FOR LATER              
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO SET RATE CALCULATION VARIABLES                        
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
SETTC    NTR1                                                                   
         MVC   TCPCYC,TAPDCYCS     CYCLE DATES                                  
         MVC   TCOV1,TAPDOV1       1ST OVERSCALE RATE                           
         MVC   TCW4TYPE,TAPDW4TY   W4 TYPE                                      
         MVC   TRGRS,TAPDGRS       SAVE GROSS FOR CABLE TEST BELOW              
         MVC   TRSPNH,TAPDSPNH     SAVE SUBJ TO P&H FOR BSS TEST BELOW          
         SPACE 1                                                                
         MVC   AIO,TRACHK                                                       
         GOTO1 EXTRACT             EXTRACT GLOBAL VALUES FROM RECORD            
                                                                                
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         GOTO1 CATVAL,DMCB,TGCAT   SET CATEGORY INFO                            
         SPACE 1                                                                
         TM    TROPTS2,TREXTRAS    IF RUNNING EXTRAS OPTION                     
         BZ    STC10                                                            
         TM    TGCATYPE,EXTRA      ONLY EXTRAS ARE ELIGIBLE                     
         BZ    STCNO                                                            
         B     STC15                                                            
         SPACE                                                                  
STC10    TM    TROPTS2,TRNOXTRA    IF RUNNING NOXTRA OPTION                     
         BZ    STC15                                                            
         TM    TGCATYPE,EXTRA      EXTRAS AREN'T ELIGIBLE FOR RETROS            
         BO    STCNO                                                            
         SPACE 1                                                                
STC15    L     R4,TRACHK                                                        
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         SPACE 1                                                                
**NO-OP* CLI   TGUSEQU,UCBL        IF THIS IS CABLE                             
**NO-OP* BNE   STC40                                                            
**NO-OP* CLC   =C'ON',TACAONOF     AND THIS IS ON-CAMERA PERF.                  
**NO-OP* BNE   STC30                                                            
**NO-OP* CLC   TRGRS,=F'20600'     AND ORIGINAL PAYMENT LESS THAN SCALE         
**NO-OP* BL    STCNO               IGNORE THIS CHECK                            
**NO-OP* B     STC40                                                            
**NO-OP* CLC   TRGRS,=F'11500'     OFF-CAMERA CHECKS DIFFERENT RATE             
**NO-OP* BL    STCNO                                                            
         SPACE 1                                                                
STC40    MVC   TCCAONOF,TACAONOF        ON/OFF CAMERA                           
         MVC   TCCADBL,TACADBL          N'DOUBLES                               
         MVC   TCOV2,TACAOV2            2ND OVERSCALE RATE                      
         MVC   TCCASTAT,TACASTAT        STATUS BYTE                             
         MVC   TCCASTA2,TACASTA2        2ND STATUS BYTE                         
         MVC   TCCASTA3,TACASTA3        3RD STATUS BYTE                         
         MVC   TCCAFRST,TACAFRST        1ST SERVICES DATE                       
         GOTO1 UNIVAL,DMCB,TACAUN       UNION INFORMATION                       
         GOTO1 YRVAL,DMCB,TACAYEAR      YEAR                                    
         MVC   TGGUA,TACAGUA            GUARANTEE CODE                          
         MVC   SORTNCDE,TACANCDE   SAVE AGENT NUMBER IN SORT RECORD             
         SPACE 1                                                                
         OC    TGGUA,TGGUA         IF NOT ON GUARANTEE                          
         BNZ   STC60                                                            
         CLI   TGUSEQU,UBSS        AND THIS IS TV SESSION                       
         BNE   STC60                                                            
         OC    TRSPNH,TRSPNH       AND SUBJ TO P&H IS ZERO                      
         BZ    STCNO               THEN IGNORE                                  
         SPACE 1                                                                
STC60    CLI   TGUSEQU,UCLA        IF USE TYPE IS CLA                           
         BNE   *+12                                                             
         BAS   RE,SETCCLA          SET CLA USE DETAILS                          
         BNE   STCNO                                                            
         SPACE 1                                                                
         L     R4,TRACHK                                                        
         MVI   ELCODE,TASDELQ      IF DEFINED, SET A(SESSION XDTLS EL)          
         BAS   RE,GETEL                                                         
         BNE   STCX                                                             
         ST    R4,TCATASD                                                       
         USING TASDD,R4                                                         
         L     R1,TASDFEE          THEN BASIC SESSION                           
         L     R0,TCOV1            PLUS OVERSCALE                               
         BAS   RE,MULT                                                          
         A     R1,TASDFEE                                                       
         LR    R2,R1                                                            
         L     R0,TCOV2                                                         
         BAS   RE,MULT                                                          
         AR    R2,R1                                                            
         ST    R2,TRBSSFEE         WILL BE USED AS BASIS FOR APPLD AMT          
STCX     B     YES                                                              
         SPACE 1                                                                
STCNO    B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO SET CHECK LEVEL CLA USE DETAILS                       
         SPACE 1                                                                
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
SETCCLA  NTR1                                                                   
         TM    TACASTAT,TACASTLO   IF PERFORMER ONLY ON LIFT                    
         BZ    SCCLA10                                                          
         MVC   TCNUSES,TRNUSESL    TOTAL PREV N'USES PAID=LAST LIFT #           
         MVC   TCNUSESL,TRNUSESL   PREV N'USES PAID TO LFT=LAST LIFT #          
         B     SCCLAX                                                           
         SPACE 1                                                                
SCCLA10  TM    TACASTAT,TACASTLF   ELSE IF PERFORMER ON LIFT AND MAIN           
         BZ    SCCLA20                                                          
         MVC   TCNUSES,TRNUSES     TOTAL PREV N'USES PAID=LAST USE NUM          
         MVC   TCNUSESL,TRNUSESL   PREV N'USES PAID TO LFT=LAST LIFT #          
         B     SCCLAX                                                           
         SPACE 1                                                                
SCCLA20  LH    R1,TRNUSES          ELSE LAST USE NUMBER                         
         SH    R1,TRNUSESL         - LAST USE # FOR LIFT                        
         STH   R1,TCNUSES          = TOTAL PREV N'USES PAID                     
         XC    TCNUSESL,TCNUSESL   NO PREV USES PAID TO LFT                     
         SPACE 1                                                                
*CCLAX   OC    TCNUSES,TCNUSES     AS ONLY FIRST USE RATE INCREASED             
*        BNZ   NO                  THEN IF NOT PAYING IT THEN IGNORE            
*        B     YES                                                              
SCCLAX   B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO GET CAST RECORD                                       
         SPACE 1                                                                
GETCAST  NTR1                                                                   
         MVC   TCACAST,AIO         PASS RATE CALC. A(CAST RECORD)               
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCACCDQ,(X'20',0)                                   
         BE    GCST20                                                           
         OI    SORTSTAT,SORTXCST   NOT FOUND - SET STATUS BIT                   
         XC    DMWORK,DMWORK                                                    
         L     R4,AIO              BUILD DUMMY RECORD                           
         USING TLCAD,R4                                                         
         XC    TLCAKEY,TLCAKEY                                                  
         MVC   TLCALEN,DATADISP                                                 
         MVI   TLCAELEM,0                                                       
         SPACE 1                                                                
GCST20   GOTO1 GETOV1,DMCB,TGUSCDE,FULL  LOOK UP OVERSCALE ON CAST REC          
         SPACE 1                                                                
         CLI   0(R1),X'FF'         IF AMOUNT OVERRIDE DEFINED ON CAST           
         BNE   GCST50                                                           
*        CLI   TGUSEQU,UBSR        AND THIS IS RADIO SESSION                    
*        BE    GCST30              THEN NOT ELIGIBLE                            
*        CLI   TGUSEQU,UGRR        AND THIS IS GRR                              
*        BNE   GCSTNO              THEN NOT ELIGIBLE                            
                                                                                
GCST30   MVI   SORTERR,ERR12       BSR/GRR PAID WITH CAST OVERRIDE AMT          
                                                                                
GCST50   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         MVC   SORTNCDE,TACANCDE   SET CURRENT AGENT NUMBER IN SORT REC         
         SPACE 1                                                                
**NO-OP* TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
**NO-OP* BZ    GCSTX                                                            
         BRAS  RE,GETTACR          GET RELEVANT APPLIED CREDIT EL.              
         BNE   GCSTX                                                            
         L     R4,TGELEM           RETURNS A(ELEMENT) IN TGELEM                 
         USING TACRD,R4                                                         
         OC    TACRBAL,TACRBAL     IF THERE'S NO REMAINING BALANCE THEN         
         BNZ   GCSTX                                                            
         MVI   SORTCSEQ,SORTCZER   SET SORT SEQUENCE INDICATING SO              
         SPACE 1                                                                
GCSTX    B     YES                                                              
         SPACE 1                                                                
GCSTNO   B     NO                                                               
         EJECT                                                                  
*              ROUTINE SETS OLD AND NEW CONTRACT RATES                          
         SPACE 1                                                                
GETRATE  NTR1                                                                   
         XC    TCPAY,TCPAY         CLEAR AMOUNTS                                
         XC    TCTOTS(TCTOTLNQ),TCTOTS                                          
         MVI   TCINPUT,0                                                        
         SPACE 1                                                                
         TM    TROPTS2,TRSESS          IF RUNNING SESS OPTION                   
         BZ    GRATE10                                                          
         GOTO1 =A(SESSRATE),DMCB,(RC)  HANDLE IN SPECIAL ROUTINE                
         B     GRATEX                                                           
         SPACE 1                                                                
GRATE10  GOTO1 TRACALC,DMCB,(RC),TCD,SYSCOMM  ** GET NEW RATES **               
         SPACE 1                                                                
         L     R4,TRACHK                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         SPACE 1                                                                
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         USING ACCD,R3                                                          
         L     R1,TCGROSS          R1=PAYMENT AMOUNT UNDER NEW CONTRACT         
         L     RF,TAPDGRS          RF=PAYMENT AMOUNT UNDER OLD CONTRACT         
         SPACE 1                                                                
         OC    TGGUA,TGGUA         IF NOT ON GUARANTEE                          
         BNZ   *+16                                                             
         CLI   TGUSEQU,UBSS        AND THIS IS TV SESSION                       
         BNE   *+8                                                              
         L     RF,TAPDSPNH         RF=SUBJ TO P&H INSTEAD OF GROSS              
         SPACE 1                                                                
         ST    R1,TRNEW            SAVE RATE UNDER NEW CONTRACT                 
         ST    RF,TROLD             AND RATE UNDER OLD CONTRACT                 
         SPACE 1                                                                
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         BZ    GRATEX                                                           
         SR    R1,RF               SET DEFAULT APPLIED AMOUNT                   
         SPACE 1                                                                
         OC    TCATASD,TCATASD     IF THERE'S EXTRA DETAILS                     
         BZ    *+12                                                             
         L     R1,TCAPPLIC         THEN NEW BASIC SESSION FEE                   
         S     R1,TRBSSFEE         LESS OLD BASIC SESSION FEE                   
         ST    R1,SORTAPPL         SAVE AMOUNT TO UPGRADE FTRACK BY             
         SPACE 1                                                                
GRATEX   B     YES                                                              
         SPACE 1                                                                
GRATENO  B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO SET ACCUMS IN SORT RECORD                             
         SPACE 1                                                                
*                                  R1=RETRO PAYMENT AMOUNT                      
SETACCS  NTR1                                                                   
         LA    R3,SORTACCS                                                      
         USING ACCD,R3             R3=A(SORT RECORD ACCUMS)                     
         SPACE 1                                                                
         L     RF,TROLD                                                         
         TM    TROPTS2,TRSESS      IF RUNNING SESS OPTION                       
         BZ    *+8                                                              
         L     RF,TRSVSPNH         USE SAVED ORIG PYMT AMT                      
         LCR   RF,RF               MOVE (COMPLEMENTED) ORIGINAL PMT AMT         
         ST    RF,ACCAPPL          TO APPLIED AMOUNT                            
         MVI   SORTACDE,APPLOTH    SET OTHER APPLIED CODE                       
         SPACE 1                                                                
         OC    TAPDAPPL,TAPDAPPL   IF THERE WERE APPLIED CRS ORIGINALLY         
         BZ    SETA40                                                           
         CLI   TAPDACDE,APPLSESS   AND APPLIED CODE WAS SESSION                 
         BE    SETA20                                                           
         CLI   TAPDACDE,APPLHLD                     OR HLD                      
         BE    SETA20                                                           
         CLI   TAPDACDE,APPLGUAR   OR IF APPLIED CODE IS GUARANTEE              
         BNE   SETA40                                                           
         TM    TGUSTYST,UPGRADE    AND THIS IS NOT AN UPGRADE                   
         BO    SETA40                                                           
         SPACE 1                                                                
SETA20   BAS   RE,APPLY            HANDLE APPLIED CREDITS                       
*                                  RETURNS R1=PAYMENT AMOUNT                    
         SPACE 1                                                                
SETA40   LA    RF,ACCPAYC          RF=A(RETRO PAY ACCUM IN SORT REC.)           
         CLI   TAPDW4TY,TAW4TYIN                                                
         BE    *+12                                                             
         CLI   TAPDW4TY,TAW4TYES                                                
         BNE   *+8                                                              
         LA    RF,ACCPAYI                                                       
         ST    R1,0(RF)            SET RETRO PAYMENT AMOUNT                     
         SPACE 1                                                                
         BAS   RE,PNHCALC          CALCULATE P&H (PASS R1=SUBJ TO P&H)          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES SETTING APPLIED CREDITS                          
         SPACE 1                                                                
*                                  R1=RETRO PAYMENT AMOUNT                      
         USING ACCD,R3             R3=A(SORT RECORD ACCUMS)                     
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
APPLY    NTR1                                                                   
         TM    TROPTS2,TRSESS      GET OUT IF RUNNING SESS OPTION               
         BO    APPLX                                                            
         CLI   SORTCSEQ,SORTCZER   IF FTRACK = 0                                
         BNE   APPL10                                                           
         B     APPLX               DON'T TAKE APPLIED CREDITS                   
* FOR 13 CONTRACT, ALL REUSE RATE INCREASE <= SESSION, NOT JUST RADIO           
         TM    TGMEEQU,RADIO       AND IF RADIO WSP                             
         BZ    APPL10                                                           
         CLI   TGUSEQU,UWSP                                                     
         BE    APPLX               DON'T TAKE APPLIED CREDITS                   
**** BUG - SHOULDN'T TAKE APPLIED CREDITS FOR ALL REUSE WHOSE RATE ****         
**** INCREASE % IS LESS THAN BSR, NOT JUST FOR WSP *****                        
         SPACE 1                                                                
APPL10   LR    R2,R1               SAVE RETRO PAYMENT                           
         SPACE 1                                                                
         MVI   BYTE,UBSS           SET TO LOOKUP BASIC SESSION/HLD RATE         
         TM    TGMEEQU,RADIO                                                    
         BZ    *+8                                                              
         MVI   BYTE,UBSR                                                        
         CLI   TAPDACDE,APPLHLD                                                 
         BNE   *+8                                                              
         MVI   BYTE,UHLD                                                        
         BAS   RE,ANYAMT                                                        
         BE    APPL30              DON'T BOTHER WITH LOOKUP                     
         SPACE 1                                                                
         MVC   TRSVUSES,TCTUSES    ELSE SAVE CURRENT USE DETAILS                
         MVC   TRSVUNIT,TCUNITS                                                 
         MVC   TRSVMAJ,TCMAJORS                                                 
         MVC   TRSVOV1,TCOV1                                                    
         XC    TCTUSES,TCTUSES                                                  
         XC    TCUNITS,TCUNITS                                                  
         MVI   TCMAJORS,0                                                       
         MVC   TCOV1,TRNEW                                                      
         SPACE 1                                                                
         BAS   RE,GETRATE          GET BASIC SESSION/HLD RATE                   
         SPACE 1                                                                
         MVC   TCTUSES,TRSVUSES    RESTORE USE DETAILS                          
         MVC   TCUNITS,TRSVUNIT                                                 
         MVC   TCMAJORS,TRSVMAJ                                                 
         MVC   TCOV1,TRSVOV1                                                    
         SPACE 1                                                                
APPL30   GOTO1 USEVAL,DMCB,SORTUSE,SORTUTYP  RESTORE ACTUAL USE VALUES          
         SPACE 1                                                                
         LR    R1,R2               RESTORE RETRO PAYMENT AMOUNT                 
         SPACE 1                                                                
         TM    SORTSTAT,SORTAMCA   IF FIXED CYC AMOUNT ON CAST REC              
         BZ    *+12                                                             
         BAS   RE,APPLYNOW         SET TO APPLY THE CREDITS NOW                 
         B     APPLX               RETURNS R1=PAYMENT AMOUNT                    
         SPACE 1                                                                
         L     R0,TRNEW            R0=NEW BASIC SESSION/HLD RATE                
         L     RE,TAPDAPPL         RE=OLD APPLIED AMOUNT                        
         LCR   RE,RE               (MAKE POSITIVE)                              
         SR    R0,RE               CALCULATE DIFFERENCE                         
         BM    APPLX               NOTHING ADDITIONAL TO CREDIT                 
         SPACE 1                                                                
         CR    R0,R1               COMPARE CREDITS TO RETRO PAYMENT AMT         
         BNH   APPL50                                                           
         SR    R0,R1               MORE CREDITS THAN PAYMENT, SO...             
         ST    R0,SORTAPPL         SAVE EXCESS                                  
         LR    R0,R1               MAKE CREDIT AMOUNT = PAYMENT AMOUNT          
         SPACE 1                                                                
APPL50   LCR   R0,R0               REVERSE SIGN                                 
         ST    R0,ACCAPPL          AND SAVE ACTUAL APPLIED AMOUNT               
         AR    R1,R0               R1=ADJUSTED RETRO PAYMENT                    
         SPACE 1                                                                
         MVC   SORTACDE,TAPDACDE   SAVE APPLIED CODE IN SORT REC.               
         CLI   SORTACDE,APPLGUAR   IF APPLIED CODE WAS GUARANTEE                
         BNE   *+8                                                              
         MVI   SORTACDE,APPLSESS   SET APPLY CODE=SESSION                       
         SPACE 1                                                                
APPLX    B     XITR1               RETURN R1                                    
         EJECT                                                                  
*              ROUTINE CHECKS FOR PAY AMOUNT OVERRIDES FOR USE IN BYTE          
         SPACE 1                                                                
ANYAMT   NTR1                                                                   
         GOTO1 USEVAL,DMCB,(X'C0',BYTE),0                                       
         SPACE 1                                                                
         GOTO1 GETOV1,DMCB,TGUSCDE,TRNEW  LOOK UP OVERSCALE ON CAST REC         
         SPACE 1                                                                
         CLI   0(R1),X'FF'         IF PAYMENT AMOUNT DEFINED ON CAST            
         BNE   NO                                                               
         OI    SORTSTAT,SORTAMCA   SET SO IN SORT RECORD                        
         B     YES                 AND RETURN CC EQUAL                          
         EJECT                                                                  
*              ROUTINE HANDLES APPLIED CREDITS IF FIXED PAY AMT ON CAST         
         SPACE 1                                                                
*                                  R1=RETRO PAYMENT AMOUNT                      
         USING ACCD,R3             R3=A(SORT RECORD ACCUMS)                     
APPLYNOW NTR1                                                                   
         TM    TCRTRN,TCRTTACR     IF RATE CALC SAYS IT CHANGED TACREL          
         BZ    APNOWX                                                           
         BRAS  RE,GETTACR          LOOK UP TACR ELEMENT ON CAST RECORD          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE IF CREDITS WERE TAKEN          
         L     R4,TGELEM                                                        
         USING TACRD,R4                                                         
         L     R0,TACRBAL                                                       
         A     R0,TCAPPLCR         RESTORE CREDITS TAKEN BY RATE CALC           
         ST    R0,TACRBAL                                                       
         SPACE 1                                                                
         C     R1,TCAPPLCR         IF PAYMENT AMT IS LESS THAN CREDITS          
         BNL   *+8                                                              
         ST    R1,TCAPPLCR         THEN MAKE APPLIED CREDITS=PAYMENT            
         SPACE 1                                                                
         S     R1,TCAPPLCR         R1=ADJUSTED PAYMENT AMOUNT                   
         SPACE 1                                                                
         L     R0,TCAPPLCR         SAVE COMPLEMENTED APPLIED CREDITS            
         LCR   R0,R0                                                            
         ST    R0,ACCAPPL                                                       
         MVC   SORTACDE,TCAPPLCD   AND APPLIED CODE FROM RATE CALC.             
         SPACE 1                                                                
         L     R0,TACRBAL          NOW REDUCE TACR BALANCE BY ACTUAL            
         S     R0,TCAPPLCR         APPLIED CREDITS TAKEN                        
         ST    R0,TACRBAL                                                       
         ST    R0,SORTBAL          SAVE NEW BALANCE IN SORT RECORD              
         MVC   SORTCYC,TACRSTRT    SAVE TACR PERIOD AS WELL                     
         SPACE 1                                                                
         BAS   RE,WRICAST          WRITE BACK CAST RECORD                       
         SPACE 1                                                                
APNOWX   B     XITR1               RETURN R1                                    
         EJECT                                                                  
*              ROUTINE CALCULATES PENSION AND HEALTH                            
         SPACE 1                                                                
*                                  R1=AMOUNT SUBJECT TO P&H                     
         USING ACCD,R3             R3=A(SORT ACCUMS)                            
PNHCALC  NTR1                                                                   
         ST    R1,ACCSPNH          SAVE SUBJECT TO P&H                          
         LH    R0,TCPNHR           SET RATE                                     
         BAS   RE,MULT             MULTIPLY                                     
         ST    R1,ACCPNH           SAVE P&H                                     
         B     XIT                                                              
         SPACE 3                                                                
*                                  R1=AMOUNT, R0=RATE                           
*                                  RETURNS R1=RESULT                            
MULT     DS    0H                                                               
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE ADDS CHECK LEVEL SORT RECORDS                            
         SPACE 1                                                                
ADDCHK   NTR1                                                                   
         CLI   SORTERR,0           IF ERROR STATUS ALREADY SET                  
         BE    ACHK300                                                          
         TM    TROPTS2,TRFIX7      FIX ERR 7'S                                  
         BZ    ACHKX                                                            
         CLI   SORTERR,ERR7                                                     
         BE    ACHK300                                                          
         B     ACHKX                                                            
                                                                                
ACHK300  LA    R4,TIKEY            BUILD CHECK-RELATED SORT FIELDS              
         USING TLCKD,R4                                                         
         MVC   SORTEST,TRINEST     ESTIMATE NUMBER                              
         MVC   SORTCAST,TLCKSORT   CAST SORT KEY                                
         MVC   SORTDA,TIDSKADD     DISK ADDRESS                                 
         SPACE 1                                                                
         BAS   RE,PUTSORT                                                       
         SPACE 1                                                                
ACHKX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE ADDS INVOICE LEVEL SORT RECORDS                          
         SPACE 1                                                                
ADDINV   NTR1                                                                   
         XC    SORTCAST,SORTCAST   CLEAR CAST SORT KEY                          
         MVI   SORTCSEQ,0                                                       
         MVC   SORTEST,TRINEST     ESTIMATE NUMBER                              
         MVC   SORTDA,TRINVDA      DISK ADDRESS                                 
                                                                                
         LA    RF,TRNITAB                                                       
         ST    RF,TRNIPTR                                                       
         TM    SORTSTAT,SORTMFIX                                                
         BZ    ADDINV08                                                         
                                                                                
ADDINV05 ST    RF,TRNIPTR                                                       
         CLI   0(RF),X'FF'                                                      
         BE    ADDINV40                                                         
         CLI   0(RF),X'00'                                                      
         BE    ADDINV40                                                         
ADDINV08 MVC   SORTMINV,0(RF)                                                   
         MVC   SORTFIXN,6(RF)                                                   
                                                                                
         TM    TROPTS,TRPHONLY     IF NOT RUNNING P&H ONLY OPTION               
         BO    ADDINV10                                                         
         TM    TGUSSTA2,NORATES    AND NO RATES DEFINED IN TABLE                
         BZ    ADDINV10                                                         
         TM    SORTSTAT,SORTMFIX                                                
         BO    ADDINV10                                                         
         MVI   SORTERR,ERR2        SET ERROR                                    
                                                                                
ADDINV10 CLI   SORTERR,0                                                        
         BE    ADDINV30                                                         
         TM    TROPTS2,TRFIX7      FIX AND ONLY REPORT ERR 7'S                  
         BZ    ADDINV20                                                         
         CLI   SORTERR,ERR7                                                     
         BE    ADDINV30                                                         
                                                                                
ADDINV20 TM    SORTSTAT,SORTTSHT   COUNT ERROR INV WITH TIMESHEETS              
         BZ    *+10                                                             
         AP    TSHTCNT,=P'1'                                                    
                                                                                
         TM    SORTSTAT,SORTVITA   COUNT ERROR INV FROM VITA                    
         BZ    *+10                                                             
         AP    VITACNT,=P'1'                                                    
                                                                                
ADDINV30 BAS   RE,PUTSORT          WRITE OUT INVOICE LEVEL SORT RECORD          
         L     RF,TRNIPTR                                                       
         AHI   RF,7                                                             
         B     ADDINV05                                                         
                                                                                
ADDINV40 NI    TRSTAT,ALL-TRADDINV                                              
         MVI   SORTERR,0           CLEAR ERROR CODE                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS REPORT                                            
         SPACE 1                                                                
REPORT   NTR1                                                                   
         XC    TRNUMCK,TRNUMCK                                                  
                                                                                
         TM    TROPTS2,TRDOWN                                                   
         BZ    REP05                                                            
         BRAS  RE,INITDOWN         INITIALIZE DOWNLOAD FORMAT                   
         BRAS  RE,HEADDOWN         HEADLINES FOR DOWNLOAD FORMAT                
                                                                                
REP05    TM    TRSTAT,TRSORTNG     TEST SORT ACTIVE                             
         BZ    REPX                                                             
         XC    TRSRTREC,TRSRTREC   PRE-CLEAR CURRENT SORT RECORD                
         XC    TGAGY,TGAGY                   GLOBAL AGENCY                      
         SPACE 1                                                                
REP10    BAS   RE,GETSORT          GET A SORT RECORD                            
         BNE   REP90                                                            
                                                                                
REP20    OC    SORTINV,SORTINV     BRANCH IF DUMMY RECORD FOR CAST              
         BZ    REP28                                                            
         CLI   SORTINV,0           OR IF DUMMY RECORD FOR RETRO INV             
         BE    REP28                                                            
         CLC   SORTKEY(SORTILNQ),TRLSTREC  ELSE TEST IF INVOICE CHANGED         
         BE    REP60                                                            
REP28    TM    TRSTAT,TRADDINV     IF INVOICE ADD PENDING                       
         BZ    REP30                                                            
         BAS   RE,NEWINV           ADD NEW INVOICE RECORD                       
REP30    NI    TRSTAT,X'FF'-TRMFIX   RESET MANUALLY FIXED STAT                  
         XC    TRNUMCK,TRNUMCK       RESET NUMBER OF CHECKS                     
                                                                                
REP35    TM    SORTSTAT,SORTMFIX                                                
         BZ    *+8                                                              
         OI    TRSTAT,TRMFIX                                                    
                                                                                
REP40    OC    SORTINV,SORTINV     UNLESS THIS IS DUMMY RECORD                  
         BZ    *+14                                                             
         OC    SORTCAST,SORTCAST   IF THIS IS CHECK LEVEL                       
         BNZ   REP70               THEN DIDN'T GET INV - SKIP                   
                                                                                
         CLC   SORTOFF,TRLSTREC+SORTOFF-SORTD  IF OFFICE CHANGED                
         BE    *+8                                                              
         BAS   RE,NEWOFF                       HANDLE NEW OFFICE                
                                                                                
         CLC   SORTAGY,TRLSTREC+SORTAGY-SORTD  IF AGENCY CHANGED                
         BE    *+12                                                             
         BAS   RE,NEWAGY                       HANDLE NEW AGENCY                
         MVI   TRLSTREC+SORTCLI-SORTD,X'FF'    SET CLIENT CHANGED               
                                                                                
         CLC   SORTCLI,TRLSTREC+SORTCLI-SORTD  IF CLIENT CHANGED                
         BE    *+8                                                              
         BAS   RE,NEWCLI                       HANDLE NEW CLIENT                
                                                                                
         CLC   SORTKEY(SORTPLNQ),TRLSTREC IF HIGH LEVEL KEY CHANGED             
         BE    REP50                                                            
         TM    TROPTS2,TRDOWN                                                   
         BO    REP50                                                            
         MVI   FORCEHED,C'Y'              SET TO START NEW PAGE                 
                                                                                
REP50    CLC   SORTKEY(SORTCLNQ),TRLSTREC IF COMMERCIAL CHANGED                 
         BE    REP60                                                            
         BAS   RE,NEWCOM                  HANDLE NEW COMMERCIAL                 
         BE    REP60                                                            
         CH    R1,=H'1'                   IF RETURNED R1=1, END OF SORT         
         BE    REP90                                                            
         B     REP20                      ELSE RESTART CNTL BREAK PROC          
         SPACE 1                                                                
REP60    DS    0H                                                               
         TM    TROPTS2,TRFIX7                                                   
         BZ    REP63                                                            
         CLI   SORTERR,ERR7                                                     
         BNE   REP10                                                            
                                                                                
*EP60    TM    SORTSTAT,SORTMFIX   IF MANUAL RETRO                              
*        BZ    REP63                                                            
*        TM    TROPTS2,TRDOWN      DOWNLOAD FORMAT?                             
*        BZ    REP65               NO, BUT STILL HAVE TO PRINT IT               
*        BRAS  RE,INVDOWN                                                       
*        BRAS  RE,EOLDOWN                                                       
*        B     REP10                                                            
REP63    OC    SORTCAST,SORTCAST   IF NO CAST SET                               
         BNZ   REP68                                                            
REP65    BAS   RE,INITINV          HANDLE INVOICE RECORD INITIALIZATION         
         BE    REP70                                                            
         CH    R1,=H'1'            IF RETURNED R1=1, END OF SORT                
         BE    REP90                                                            
         B     REP20               ELSE RESTART CNTL BREAK PROC                 
         SPACE 1                                                                
REP68    BAS   RE,NEWCHK           CAST SET - ADD NEW CHECK RECORD              
         SPACE 1                                                                
REP70    B     REP10               AND GO PROCESS NEXT                          
         SPACE 1                                                                
REP90    TM    TRSTAT,TRADDINV     IF INVOICE ADD PENDING                       
         BZ    *+8                                                              
         BAS   RE,NEWINV           ADD LAST NEW INVOICE RECORD                  
         SPACE 1                                                                
         BAS   RE,WRIAGY           UPDATE LAST AGENCY RECORD                    
         SPACE 1                                                                
         BAS   RE,CLITOTS          PRINT ALL HIGH LEVEL TOTALS                  
         BAS   RE,AGYTOTS                                                       
         BAS   RE,OFFTOTS                                                       
         SPACE 1                                                                
REPX     BAS   RE,REQTOTS          PRINT REQUEST TOTALS                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES NEW TP OFFICE                                    
         SPACE 1                                                                
NEWOFF   NTR1                                                                   
         BAS   RE,CLITOTS          PRINT APPROPRIATE HIGH LEVEL TOTALS          
         BAS   RE,AGYTOTS                                                       
         BAS   RE,OFFTOTS                                                       
         SPACE 1                                                                
         MVC   TGOFF,SORTOFF       SET TO READ NEW OFFICE                       
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'20',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(MYTRACE),DMCB,=C'OFFICE'                                      
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAADD,R4                                                         
         ZIC   R1,TAADLNES         GET TO LAST LINE                             
         BCTR  R1,0                                                             
         MH    R1,=AL2(L'TAADADD)                                               
         LA    R1,TAADADD(R1)                                                   
         LA    R2,TROFNAME                                                      
         MVC   TROFNAME,SPACES                                                  
         LA    R0,L'TAADADD                                                     
         SPACE 1                                                                
NOFF20   CLI   0(R1),C','          TAKE CITY AS NAME                            
         BE    NOFFX                                                            
         MVC   0(1,R2),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,NOFF20                                                        
         SPACE 1                                                                
NOFFX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES NEW AGENCY                                       
         SPACE 1                                                                
NEWAGY   NTR1                                                                   
         BAS   RE,WRIAGY           UPDATE PREVIOUS AGENCY RECORD                
         SPACE 1                                                                
         BAS   RE,CLITOTS          PRINT APPROPRIATE HIGH LEVEL TOTALS          
         BAS   RE,AGYTOTS                                                       
         SPACE 1                                                                
         MVC   TGAGY,SORTAGY       SET NEW GLOBAL AGENCY                        
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(MYTRACE),DMCB,=C'AGENCY'                                      
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   TRAYNAME,TGNAME                                                  
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4            R4=A(AGENCY ELEMENT)                         
         MVC   TRAYSTAT,TAAYSTAT   SAVE AGENCY STATUS                           
         SPACE 1                                                                
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS COD AGENCY                        
         BZ    *+14                                                             
         AP    CODACNT,=P'1'       ADD TO COD AGENCY COUNT                      
         B     *+10                                                             
         AP    REGACNT,=P'1'       ELSE ADD TO REGULAR AGENCY COUNT             
         AP    AGYCNT,=P'1'        ADD TO TOTAL AGENCIES                        
         SPACE 1                                                                
         BAS   RE,GETINV           GET NEXT INVOICE NUMBER                      
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE HANDLES NEW CLIENT                                       
         SPACE 1                                                                
NEWCLI   NTR1                                                                   
         BAS   RE,CLITOTS          PRINT TOTALS                                 
         SPACE 1                                                                
         MVC   TGCLI,SORTCLI                                                    
         MVC   TRCLNAME,NOREC      INIT NAME IN CASE NO LONGER VALID            
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',0)                                    
         BNE   XIT                                                              
         GOTO1 =A(MYTRACE),DMCB,=C'CLIENT'                                      
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   TRCLNAME,TGNAME                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES WRITING BACK UPDATED AGENCY RECORDS              
         SPACE 1                                                                
WRIAGY   NTR1                                                                   
         OC    TGAGY,TGAGY         GET OUT IF FIRST TIME                        
         BZ    WAGYX                                                            
         BAS   RE,UPDAGY           UPDATE AGENCY RECORD                         
         GOTO1 =A(MYTRACE),DMCB,=C'UPDATED AGENCY'                              
         SPACE 1                                                                
         OC    TRSVAGY,TRSVAGY     IF WE HAVE SAVED MASTER AGENCY               
         BZ    WAGYX                                                            
         MVC   TRAGY,TGAGY         SAVE ACTUAL AGENCY                           
         MVC   TGAGY,TRSVAGY       MOVE SAVED MASTER AGY TO GLOBAL              
         BAS   RE,UPDAGY           UPDATE AGENCY RECORD                         
         GOTO1 =A(MYTRACE),DMCB,=C'UPDATED MASTER AGY'                          
         MVC   TGAGY,TRAGY         RESTORE ACTUAL AGENCY                        
         SPACE 1                                                                
WAGYX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE UPDATES NEXT INVOICE NUMBER ON AGENCY RECORDS            
         SPACE 1                                                                
UPDAGY   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',0)  GET AGENCY RECORD                 
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   TAAYNINV,TRINV      UPDATE NEXT INVOICE NUMBER                   
         SPACE 1                                                                
         TM    TROPTS,TRNOMARK     UNLESS OPTION REQUESTED OTHERWISE            
         BO    UAGYX                                                            
         CLC   TGAGY,TRSVAGY       IF THIS ISN'T MASTER AGENCY                  
         BE    *+8                                                              
         OI    TAAYSTA2,TAAYSRET   SET RETROS PROCESSED FOR THIS AGY            
         SPACE 1                                                                
UAGYX    GOTO1 PUTREC              WRITE IT BACK                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES NEW COMMERCIAL                                   
*              SETS CC EQ IF OK, ELSE SETS CC NOT EQ AND R1=1 IF END            
*              OF SORT, ELSE R1=2                                               
         SPACE 1                                                                
NEWCOM   NTR1                                                                   
         MVC   TGCOM,SORTCOM                                                    
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(MYTRACE),DMCB,=C'COMMERCIAL'                                  
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMML DETAILS ELEMENT)                  
         MVC   TRCOMEXP,TACOEXP    SAVE EXPIRATION DATE                         
         MVC   TRCOMML,TACOCID                                                  
                                                                                
         TM    TROPTS2,TRDOWN                                                   
         BO    NCOM10                                                           
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINCID,TACOCID      MOVE CID TO PRINT LINE 1ST TIME ONLY         
                                                                                
NCOM10   XC    TRRETTAB,TRRETTAB   CLEAR TABLE OF RETRO INVOICES                
         MVI   TRRETTX,X'FF'                                                    
         LH    RF,=AL2(L'TRCSTTAB) CLEAR TABLE OF CAST WITH REUSE IN PD         
         XCEFL TRCSTTAB                                                         
         LA    R2,TRCSTTAB         NOW REBUILD CAST TABLE AT R2                 
         LA    R4,TRRETTAB         NOW REBUILD RETRO TABLE AT R4                
         SPACE 1                                                                
NCOM20   OC    SORTINV,SORTINV     IF THERE'S NO INVOICE NUMBER                 
         BNZ   NCOM30                                                           
         MVC   0(2,R2),SORTCAST+4  SAVE INTERNAL CAST SEQ. NUMBER               
         LA    R2,2(R2)                                                         
         B     NCOM40                                                           
         SPACE 1                                                                
NCOM30   CLI   SORTINV,0           IF 1ST BYTE OF INVOICE NUMBER IS 0           
         BNE   NCOMX                                                            
         MVC   0(4,R4),SORTINV+2   SAVE LAST 4 BYTES OF INVOICE NUMBER          
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
NCOM40   BAS   RE,GETSORT          GET NEXT SORT RECORD                         
         BNE   NCOMNO                                                           
         SPACE 1                                                                
         CLC   SORTKEY(SORTCLNQ),TRLSTREC  AS LONG AS COMML DIDN'T CHG          
         BE    NCOM20                      CONTINUE LOOPING                     
         LA    R1,2                        ELSE RETURN CC NE  - RESTART         
         B     *+8                         SET R1=2                             
         SPACE 1                                                                
NCOMNO   LA    R1,1                END OF SORT, SET R1=1                        
         LTR   RC,RC                                                            
         XIT1  REGS=(R1)                                                        
NCOMX    B     YES                 RETURN CC EQ - COMML OK                      
         EJECT                                                                  
*              ROUTINE GETS NEXT INVOICE NUMBER FOR AGENCY                      
         SPACE 1                                                                
*                                  RETURNS IN TRINV                             
GETINV   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   TRSVAGY,TAAYIAGY                                                 
         OC    TAAYIAGY,TAAYIAGY   IF USING INV NUMBERS OF ANOTHER AGY          
         BZ    GINV20                                                           
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TAAYIAGY)  GET ITS RECORD             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(MYTRACE),DMCB,=C'MASTER AGY - GETINV'                         
         SPACE 1                                                                
         MVC   TGAGY,SORTAGY       RETURN REGULAR AGENCY TO GLOBAL              
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BAS   RE,GETEL                                                         
         OC    TAAYIAGY,TAAYIAGY   IF USING INV NUMBERS OF ANOTHER AGY          
         BZ    *+6                                                              
         DC    H'0'                DIE                                          
         SPACE 1                                                                
GINV20   DS    0H                                                               
*        CLI   TGTODAY1,X'90'           IF TODAY'S YEAR < 90                    
*        BNL   *+12                                                             
*        CLI   TAAYNINV,X'20'           AND IF NOT 21ST CENTURY                 
*        BNE   GIN50                                                            
*        CLC   TAAYNINV+1(1),TGTODAY1   OR IF YEAR                              
*        BNE   GIN50                                                            
*        CLC   TAAYNINV+2(1),TGTODAY1+1 OR MONTH DOESN'T MATCH                  
*        BE    GIN60                                                            
*IN50    MVC   TAAYNINV+3(2),TAAYRINV   USE RESET INVOICE NUMBER                
*        MVC   TAAYNINV(2),=X'2013'       TODAY'S YEAR                          
*        MVC   TAAYNINV+2(1),TGTODAY1+1             MONTH                       
*        MVI   TAAYNINV,X'19'           SET 20TH CENTURY                        
*        CLI   TGTODAY1,X'90'           IF TODAY'S YEAR < 90                    
*        BNL   *+8                                                              
*        MVI   TAAYNINV,X'20'           SET 21ST CENTURY                        
                                                                                
GIN60    MVI   TAAYNINV+5,0        ALWAYS ADD NEW INVOICE TYPE                  
                                                                                
         MVC   TRINV,TAAYNINV      SET THIS INVOICE NUMBER                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE RETRO INVOICE RECORDS                      
*              SETS CC EQUAL IF OK, ELSE CC NOT EQ AND R1=1 IF END              
*              OF SORT, ELSE R1=2                                               
         SPACE 1                                                                
INITINV  NTR1                                                                   
         NI    TRSTAT,ALL-TRADDINV                                              
         TM    TROPTS2,TRSESS      IF RUNNING SESS OPTION                       
         BZ    IINV30                                                           
         LA    R2,TRRETTAB         LOOK IN RETRO TABLE AT R2                    
IINV20   OC    0(4,R2),0(R2)       IF END OF TABLE, THEN INVOICE IS OK          
         BZ    IINV30                                                           
         CLC   0(4,R2),SORTINV+2   CHECK LAST 4 BYTES OF INVOICE NUMBER         
         BE    IINV25              IF FOUND, SKIP THIS INVOICE                  
         LA    R2,4(R2)            BUMP TO NEXT TABLE ENTRY                     
         B     IINV20              AND KEEP LOOKING                             
         SPACE 1                                                                
IINV25   BAS   RE,GETSORT          GET NEXT SORT RECORD                         
         BNE   IINVNO                                                           
         CLC   SORTKEY(SORTILNQ),TRLSTREC  AS LONG AS INV DIDN'T CHG            
         BE    IINV25                      CONTINUE LOOPING                     
         LA    R1,2                        ELSE RETURN CC NE  - RESTART         
         B     *+8                         SET R1=2                             
         SPACE 1                                                                
IINVNO   LA    R1,1                                                             
         LTR   RC,RC                                                            
         XIT1  REGS=(R1)                                                        
         SPACE 1                             NOT RUNNING SESS OPTION            
IINV30   GOTO1 USEVAL,DMCB,SORTUSE,SORTUTYP  SET GLOBAL USE VALUES              
         SPACE 1                                                                
         MVC   TRINVERR,SORTERR    SAVE INVOICE ERROR STATUS                    
         TM    TROPTS2,TRFIX7                                                   
         BZ    TINV35                                                           
         CLI   SORTERR,ERR7                                                     
         BNE   TINV35                                                           
         MVI   TRINVERR,0                                                       
                                                                                
TINV35   MVC   TRINVOLD,SORTINV         OLD INVOICE NUMBER                      
         GOTO1 TINVCON,DMCB,TRINVOLD,TRINVOLC,DATCON  SAVE EBCDIC VERS.         
                                                                                
         MVC   KEY+TLDRDA-TLDRD(4),SORTDA SET D/A OF INVOICE RECORD             
         MVC   AIO,=A(INVIO)                                                    
         GOTO1 GETREC              READ ORIGINAL INVOICE RECORD                 
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         CLI   TRINVERR,0          IF INVOICE IN ERROR STATUS                   
         BNE   IINV80              SKIP TO PRINT                                
         TM    SORTSTAT,SORTMFIX   OR FIXED, SKIP TO PRINT                      
         BO    IINV80                                                           
         SPACE 1                                                                
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    IINV40                                                           
         MVC   TROFF,SORTOFF       SET SAVED VALUES - OFFICE                    
         MVC   TRAGY,SORTAGY                          AGENCY                    
         MVC   TRCLI,SORTCLI                          CLIENT                    
         MVC   TRPRD,SORTPRD                          PRODUCT                   
         SPACE 1                                                                
IINV40   L     R4,=A(INVIO)                                                     
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMML DETAILS ELEMENT)                  
         MVC   TGCID,TACOCID       SET COMMERCIAL ID FOR FTRACKS                
         SPACE 1                                                                
         GOTO1 MEDVAL,DMCB,TACOMED VALIDATE MEDIA FOR FTRACKS                   
         SPACE 1                                                                
*                                  MOVE SOME DATA TO PRINT LINE                 
IINV80   TM    TROPTS2,TRDOWN                                                   
         BO    IINV90                                                           
                                                                                
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINOINV,TRINVOLC    OLD INVOICE                                  
         MVC   LINUSE,TGUSCDE      USE CODE                                     
         MVC   LINUTYCD,TGUSTYCD   USE TYPE CODE                                
         MVC   LINEST,SORTEST      ESTIMATE NUMBER                              
         MVC   TRINEST,SORTEST                                                  
         SPACE 1                                                                
         L     R4,=A(INVIO)                                                     
         MVI   ELCODE,TAVRELQ      IF VERSION PAID                              
         BAS   RE,GETEL                                                         
         BNE   IINV85                                                           
         MVI   LINUTYCD+4,C'V'     DISPLAY "V" AT END OF USE NAME               
                                                                                
IINV85   TM    SORTSTAT,SORTMFIX   IF MANUAL RETRO                              
         BZ    IINV88                                                           
         GOTO1 TINVCON,DMCB,SORTMINV,LINNINV,DATCON   FIX INVOICE               
         MVI   BC2,C'~'                                                         
         BAS   RE,PRNTIT           PRINT THE LINE                               
         B     IINVX                                                            
                                                                                
IINV88   CLI   TRINVERR,0          IF THERE'S NO ERROR SET                      
         BNE   IINV90                                                           
         GOTO1 TINVCON,DMCB,TRINV,LINNINV,DATCON   NEW INVOICE                  
         B     IINVX                                                            
                                                                                
IINV90   BAS   RE,ERRPROC          ELSE PROCESS ERROR                           
IINVX    B     YES                                                              
         EJECT                                                                  
*              ROUTINE PROCESS ERROR INVOICES                                   
         SPACE 1                                                                
ERRPROC  NTR1                                                                   
         TM    TROPTS2,TRDOWN      DOWNLOAD FORMAT?                             
         BZ    ERRP100                                                          
         CLI   TRINVERR,0          IF THERE'S NO ERROR SET                      
         BZ    XIT                                                              
*        CLI   TRINVERR,ERR7                                                    
*        BNZ   ERRP100                                                          
                                                                                
         BRAS  RE,INVDOWN          YES, DOWNLOAD DATA                           
         BRAS  RE,EOLDOWN                                                       
         B     ERRP200                                                          
*                                                                               
ERRP100  LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINNINV(4),=C'ERR ' DISPLAY ERROR                                
         LA    RE,LINNINV+4                                                     
         EDIT  (1,TRINVERR),(2,(RE)),ALIGN=LEFT                                 
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT           AND PRINT THE LINE NOW                       
         SPACE 1                                                                
*                                  * SET TO WRITE ERROR RECORD                  
ERRP200  MVC   WORK(6),TGAGY       RECORD IS AGENCY                             
         MVC   WORK+6(6),TRINVOLC  AND OLD INVOICE NUMBER                       
         PUT   ERRFILE,WORK        WRITE TO ERROR FILE                          
*                                                                               
         ZIC   R1,TRINVERR         SET TO ADD TO ITS COUNTER                    
         BCTR  R1,0                                                             
         MH    R1,=AL2(L'CNTTAB)                                                
         LA    R1,ERRTAB(R1)                                                    
         AP    0(4,R1),=P'1'                                                    
         AP    ERRCNT,=P'1'        ALSO ADD TO TOTAL ERRORS                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GENERATES RETROACTIVE CHECK RECORDS                      
         SPACE 1                                                                
NEWCHK   NTR1                                                                   
         TM    TROPTS3,TRFIX7B     SPECIAL FIX TO NOT ADD FIRST CHECK           
         BZ    NCHK03                                                           
                                                                                
         OC    TRNUMCK,TRNUMCK     NUMBER OF CHECKS NOT ZERO?                   
         BNZ   NCHK03              YES, CONTINUE                                
         XR    RF,RF                                                            
         ICM   RF,3,TRNUMCK        FIRST TIME IN, ADD 1 AND LEAVE               
         AHI   RF,1                                                             
         STCM  RF,3,TRNUMCK                                                     
         B     NCHKX                                                            
                                                                                
NCHK03   MVI   TRW4TYP,C' '                                                     
         TM    SORTSTAT,SORTMFIX                                                
         BO    NCHK20                                                           
         CLI   TRINVERR,0          IF ERROR STATUS SET                          
         BNE   NCHKX               DON'T BOTHER - GET OUT                       
*        CLI   TRINVERR,ERR7                                                    
*        BNE   NCHKX               DON'T BOTHER - GET OUT                       
*        TM    SORTSTAT,SORTFIX7                                                
*        BZ    NCHKX               DON'T BOTHER - GET OUT                       
         SPACE 1                                                                
NCHK05   TM    TROPTS2,TRSESS      SKIP IF SESS OPTN - DON'T WANT ERR9          
         BO    NCHK20                                                           
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         BZ    NCHK20                                                           
         CLI   SORTCSEQ,SORTCZER   AND TACRBAL WAS ZERO                         
         BNE   NCHK20                                                           
         LA    R2,TRCSTTAB         DETERMINE WHETHER THIS CAST MEMBER           
*                                  WAS PAID REUSE IN RETRO PERIOD               
NCHK10   OC    0(2,R2),0(R2)       TEST REACHED END OF TABLE                    
         BNZ   NCHK15                                                           
         TM    TROPTS2,TRFIX7                                                   
         BZ    NCHK13                                                           
         CLI   SORTERR,ERR7                                                     
         BE    NCHK15                                                           
                                                                                
NCHK13   MVI   TRINVERR,ERR9       SET ERROR STATUS NOW                         
         BAS   RE,ERRPROC          PROCESS ERROR                                
         B     NCHKX               AND GET OUT                                  
         SPACE 1                                                                
NCHK15   CLC   0(2,R2),SORTCAST+4  MATCH AGAINST INTERNAL CAST SEQ.             
         BE    NCHK20              OK TO CONTINUE IF SO                         
         LA    R2,2(R2)            TRY NEXT TABLE ENTRY                         
         B     NCHK10                                                           
         SPACE 1                                                                
NCHK20   MVC   AIO,=A(CHKIO)       SET I/O AREA TO A(CHECK RECORD)              
                                                                                
         MVC   KEY+TLDRDA-TLDRD(4),SORTDA SET D/A OF CHECK RECORD               
         MVC   FILENAME,=C'CHKFIL'                                              
         GOTO1 GETREC              GET OLD CHECK RECORD                         
         XC    FILENAME,FILENAME                                                
         L     R4,AIO                                                           
         USING TLCKD,R4            R4=A(RECORD)                                 
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    *+10                                                             
         MVC   TLCKAGY,TRAGY       SET NEW AGENCY IN KEY                        
         MVC   TLCKINV,TRINV       SET NEW INVOICE NUMBER                       
                                                                                
         GOTO1 EXTRACT             EXTRACT GLOBAL VALUES FROM RECORD            
                                                                                
         MVC   TRSVKEY,KEY                                                      
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TLCKSSN)                              
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNAME,TAW4CRPN                                                  
                                                                                
         MVC   AIO,=A(CHKIO)       SET I/O AREA TO A(CHECK RECORD)              
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NCHK25                                                           
         MVC   TRW4TYP,TAPDW4TY                                                 
                                                                                
NCHK25   TM    SORTSTAT,SORTMFIX                                                
         BO    NCHK30                                                           
                                                                                
         BRAS  RE,ADDTARP          ADD RETRO P&H AMOUNT ELEMENT                 
         BRAS  RE,ELPROC           PROCESS ELEMENTS                             
         SPACE 1                                                                
NCHK30   GOTO1 =A(MYTRACE),DMCB,=C'RETRO CHECK'                                 
         TM    SORTSTAT,SORTMFIX                                                
         BZ    NCHK35                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NCHK40                                                           
         USING TACAD,R4            R4=A(CAST ELEMENT)                           
         MVC   TRCSTEXP,TACAEXP    SAVE CAST EXPIRY FOR FTRACKS                 
         MVC   TRONOF,TACAONOF     SAVE CAMERA STATUS FOR PRINTING              
         B     NCHK40                                                           
                                                                                
NCHK35   MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         BAS   RE,ADDIT            ADD THE RECORD                               
NCHK40   MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
         SPACE 1                                                                
         OI    TRSTAT,TRADDINV     SET INVOICE ADD PENDING                      
         SPACE 1                                                                
         BAS   RE,ADDUP            ADD TO HIGHER LEVEL ACCUMS                   
         SPACE 1                                                                
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         BO    NCHK60                                                           
         CLI   SORTACDE,APPLSESS   OR APPLIED CODE IS SESSION                   
         BE    NCHK60                                                           
         CLI   SORTACDE,APPLHLD                    OR HLD                       
         BNE   NCHK70                                                           
NCHK60   BAS   RE,FTRACK           HANDLE CAST/FTRACK RECORD MAINT.             
                                                                                
NCHK70   AP    CASTCNT,=P'1'       AND ADD TO COUNTER                           
                                                                                
         TM    TROPTS2,TRDOWN      DOWNLOAD FORMAT?                             
         BZ    NCHK80                                                           
         BRAS  RE,INVDOWN          DOWNLOAD INV INFO                            
                                                                                
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         GOTO1 =A(OUTPDOWN),DMCB,(C'T',TGPID),L'TGPID                           
         GOTO1 (RF),DMCB,(C'T',TRNAME),L'TRNAME                                 
         GOTO1 (RF),DMCB,(C'T',TRW4TYP),L'TRW4TYP                               
         GOTO1 (RF),DMCB,(C'T',TGCAT),L'TGCAT                                   
         GOTO1 (RF),DMCB,(C'T',TRONOF),L'TRONOF                                 
                                                                                
         MVI   WORK,C' '                                                        
         CLI   TRICODE,C' '                                                     
         BNH   *+10                                                             
         MVC   WORK(1),TRICODE                                                  
                                                                                
         GOTO1 (RF),DMCB,(C'T',WORK),1                                          
         EDIT  (4,TRPDREXP),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-    REXP             
         GOTOR OUTPDOWN,DMCB,(C'N',BLOCK),12                                    
         GOTO1 (RF),DMCB,(C'T',=C' '),1                                         
         MVC   WORK,SORTACDE                                                    
         TM    SORTSTAT,SORTXCST   IF CAST RECORD WASN'T FOUND                  
         BZ    *+8                                                              
         MVI   WORK,C'*'           INDICATE WITH ASTERISK                       
         GOTO1 (RF),DMCB,(C'T',WORK),1                                          
                                                                                
         BRAS  RE,ACCMDOWN         DOWNLOAD ACCUMS                              
         BRAS  RE,EOLDOWN                                                       
         B     NCHK99                                                           
                                                                                
NCHK80   LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINSSN,TGSSN        SOCIAL SECURITY NUMBER                       
         MVC   LINCAT,TGCAT        CATEGORY                                     
         MVC   LINONOF,TRONOF      ON/OFF CAMERA                                
         MVC   LINACDE,SORTACDE    APPLIED CODE                                 
         TM    SORTSTAT,SORTXCST   IF CAST RECORD WASN'T FOUND                  
         BZ    *+8                                                              
         MVI   LINACDE,C'*'        INDICATE WITH ASTERISK                       
         LA    R3,SORTACCS         SET R3=A(SORT RECORD ACCUMS)                 
         GOTO1 =A(FORMAT),DMCB,0   FORMAT ACCUMS TO PRINT LINE                  
         BAS   RE,PRNTIT           PRINT THE LINE                               
                                                                                
NCHK99   TM    TRAYSTAT,TAAYSCOD   IF THIS IS A COD AGENCY                      
         BZ    *+14                                                             
         AP    CODCCNT,=P'1'       ADD TO COD CHECK COUNT                       
         B     *+10                                                             
         AP    REGCCNT,=P'1'       ELSE ADD TO REGULAR CHECK COUNT              
         AP    CHKCNT,=P'1'        ADD TO TOTAL CHECKS                          
         SPACE 1                                                                
NCHKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADDS CHECK LEVEL ACCUMS TO ALL HIGHER LEVELS             
         SPACE 1                                                                
ADDUP    NTR1                                                                   
         LA    R2,NACCS            R2=N'ACCUMS                                  
         LA    RE,SORTACCS         ADD FROM SORT RECORD                         
         LA    R3,TRACCS           TO ALL HIGHER LEVELS                         
         SPACE 1                                                                
ADD10    LR    RF,R3                                                            
         LA    R0,TRNLVLS          R0=N'HIGHER LEVELS                           
         SPACE 1                                                                
ADD20    L     R1,0(RF)                                                         
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         SPACE 1                                                                
         LA    RF,ACCLNQ(RF)       BUMP TO NEXT LEVEL                           
         BCT   R0,ADD20                                                         
         SPACE 1                                                                
         LA    RE,4(RE)            BUMP TO NEXT ACCUM                           
         LA    R3,4(R3)                                                         
         BCT   R2,ADD10                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GENERATES RETROACTIVE INVOICE RECORDS                    
         SPACE 1                                                                
NEWINV   NTR1                                                                   
         OI    TRSTAT,TRPROCIN     SET PROCESSING RETRO INVOICE                 
         SPACE 1                                                                
         MVC   AIO,=A(INVIO)       SET I/O AREA TO A(INVOICE RECORD)            
                                                                                
         TM    TRSTAT,TRMFIX                                                    
         BO    NEWINV30                                                         
                                                                                
         L     R4,AIO                                                           
         USING TLIND,R4                                                         
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    *+10                                                             
         MVC   TLINAGY,TRAGY       SET NEW AGENCY IN KEY                        
         MVC   TLININV,TRINV       SET NEW INVOICE NUMBER IN KEY                
         XC    TLININV,=6X'FF'     (COMPLEMENTED)                               
                                                                                
         BRAS  RE,ADDRET4          ADD RETRO FOR INVOICE                        
         BRAS  RE,ADDTARP          ADD RETRO P&H AMOUNT ELEMENT                 
         BRAS  RE,ELPROC           PROCESS ELEMENTS                             
                                                                                
         BAS   RE,ADDCMNT          ADD SPECIAL COMMENTS TO INVOICE              
                                                                                
NEWINV30 GOTO1 =A(MYTRACE),DMCB,=C'RETRO INVOICE'                               
         TM    TRSTAT,TRMFIX                                                    
         BO    NEWINV50                                                         
         BAS   RE,ADDIT            ADD THE RECORD                               
NEWINV50 MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
         NI    TRSTAT,ALL-TRADDINV TURN OFF INVOICE ADD PENDING                 
                                                                                
         BAS   RE,INVTOTS          FORMAT/PRINT LINE                            
                                                                                
         TM    TRSTAT,TRMFIX       MANUALLY FIXED?                              
         BO    NEWINV70            DON'T NEED NEW INV NUMBER                    
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,TRINV+3(2)      CALC NEXT - CVT TO PACKED WITH SIGN          
         AP    DUB,=P'1'                                                        
         MVO   WORK(3),DUB+5(3)                                                 
         MVC   TRINV+3(2),WORK                                                  
                                                                                
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS A COD AGENCY                      
         BZ    *+14                                                             
         AP    CODCNT,=P'1'        ADD TO COD COUNT                             
         B     *+10                                                             
         AP    REGCNT,=P'1'        ELSE ADD TO REGULAR COUNT                    
         AP    INVCNT,=P'1'        ADD TO TOTAL INVOICES                        
         SPACE 1                                                                
NEWINV70 NI    TRSTAT,ALL-TRPROCIN TURN OFF PROCESSING RETRO INV.               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADDS SPECIAL COMMENTS TO RETRO INVOICES                  
         SPACE 1                                                                
ADDCMNT  NTR1                                                                   
         BAS   RE,HISTCMNT         ADD HISTORY COMMENT FIRST                    
*                                  RETURNS R2=A(REMAINING COMMENT)              
*                                          TGDUB=ORIGINAL INVOICE NO.           
         SPACE 1                                                                
         LA    R4,ELEMENT          R4=A(HISTORY COMMENT JUST BUILT)             
         USING TACMD,R4                                                         
         SPACE 1                                                                
         ZIC   R3,TACMLEN          ADJUST L'ELEMENT FOR BIGGER COMMENT          
         LA    R3,L'INVGCMNT-L'INVHCMNT(R3)                                     
         STC   R3,TACMLEN                                                       
         MVI   TACMTYPE,TACMTYPG          NOW SET GENERAL TYPE                  
         MVC   0(L'INVGCMNT,R2),INVGCMNT  MOVE IN REMAINING COMMENT             
         MVC   L'INVGCMNT(6,R2),TRINVOLC  ADD ORIGINAL INV NUMBER               
         SPACE 1                                                                
         GOTO1 ADDELEM                    ADD IT TO INVOICE                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE ADDS HISTORY COMMENT TO RECORD IN AIO                    
         SPACE 1                                                                
HISTCMNT NTR1                                                                   
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         LA    R4,ELEMENT                                                       
         USING TACMD,R4                                                         
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMTYPE,TACMTYPH   SET HISTORY TYPE                             
         L     R1,AIO                                                           
         CLI   0(R1),TLFTCDQ       IF THIS IS FTRACK RECORD                     
         BNE   *+8                                                              
         MVI   TACMTYPE,TACMTYPG   SET GENERAL TYPE                             
                                                                                
         MVC   TACMCOMM(L'RETROCMT),RETROCMT  START COMMENT                     
                                                                                
         LA    R2,TACMCOMM+L'RETROCMT                                           
         LA    R3,TACMLNQ+L'RETROCMT+L'INVHCMNT+6                               
         TM    TROPTS,TRPHONLY                    IF RUNNING P&H ONLY           
         BZ    *+14                                                             
         MVC   TACMCOMM+L'RETROCMT-4(3),=C'P&&H'  CHANGE COMMENT                
         BCTR  R2,0                                                             
         BCTR  R3,0                                                             
                                                                                
         STC   R3,TACMLEN                     SET L'ELEMENT                     
         MVC   0(L'INVHCMNT,R2),INVHCMNT      MOVE IN REMAINING COMMENT         
         MVC   L'INVHCMNT(6,R2),TRINVOLC      ADD ORIGINAL INV NUMBER           
                                                                                
         GOTO1 ADDELEM                        ADD IT TO THE RECORD              
                                                                                
         XIT1  REGS=(R2)           RETURN R2=A(REMAINING COMMENT)               
         EJECT                                                                  
*              ROUTINE HANDLES CAST/FTRACK RECORD MAINTENANCE                   
         SPACE 1                                                                
FTRACK   NTR1                                                                   
         TM    TROPTS2,TRSESS      GET OUT IF RUNNING SESS OPTION               
         BO    FTX                                                              
         TM    TROPTS,TRPHONLY     GET OUT IF RUNNING P&H ONLY OPTION           
         BO    FTX                                                              
         TM    SORTSTAT,SORTXCST   IF CAST RECORD WASN'T FOUND GET OUT          
         BO    FTX                                                              
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         USING ACCD,R3                                                          
         TM    TROPTS,TRBOTH       IF BOTH OPTION                               
         BZ    FT1                                                              
         OC    ACCAPPL,ACCAPPL     GET OUT IF ONLY P&H INCREASE                 
         BZ    FTX                                                              
         SPACE 1                                                                
FT1      TM    SORTSTAT,SORTAMCA   IF FIXED CYC PMT DEFINED ON CAST             
         BZ    FT3                                                              
         MVC   TCAPPLCR,ACCAPPL    SET APPLIED AMOUNT FROM SORT REC             
         MVC   TRBAL,SORTBAL       AND BALANCE AS WELL                          
         MVC   TRCRCYC,SORTCYC     SET TACR CYCLE FROM SORT REC                 
         B     FT40                SKIP CAST UPDATE (DONE ALREADY)              
         SPACE 1                                                                
FT3      GOTO1 RECVAL,DMCB,TLCACCDQ,(X'20',0)  GET CAST RECORD                  
         BNE   FTX                                                              
         SPACE 1                                                                
FT5      BRAS  RE,GETTACR          GET RELEVANT APPLIED CREDIT EL.              
         BE    FT10                                                             
         BAS   RE,WSPRADG          TEST FOR SPECIAL WSP RADIO CONDITION         
         BNE   FTX                                                              
         XC    ELEMENT,ELEMENT     SET TO BUILD NEW TACREL                      
         LA    R4,ELEMENT                                                       
         USING TACRD,R4                                                         
         MVI   TACREL,TACRELQ      ELEMENT CODE                                 
         MVI   TACRLEN,TACRLNQ     ELEMENT LENGTH                               
         MVC   TACRSTRT,TCPCYCS    CYCLE START                                  
         MVC   TACREND,TRCSTEXP    END DATE IS CAST EXPIRATION DATE             
         OC    TACREND,TACREND                                                  
         BNZ   *+10                                                             
         MVC   TACREND,TRCOMEXP    OR COMMERCIAL EXPIRATION                     
         OC    TACREND,TACREND                                                  
         BNZ   *+10                                                             
         MVC   TACREND,TCPCYCE     ELSE CYCLE END                               
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         B     FT5                 GO BACK AND FIND IT ON THE RECORD            
         SPACE 1                                                                
FT10     L     R4,TGELEM           RETURNS A(ELEMENT) IN TGELEM                 
         MVC   TRCRCYC,TACRSTRT    SAVE PERIOD FOR TRACKING                     
         L     R2,TACRBAL          R2=REMAINING BALANCE                         
         SPACE 1                                                                
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         BZ    FT20                                                             
         LTR   R2,R2               THEN IF BALANCE IS ZERO DON'T CHANGE         
         BZ    FTX                                                              
         A     R2,SORTAPPL         ADD APPLIED AMOUNT TO BALANCE                
         ST    R2,TACRBAL          SAVE NEW BALANCE                             
         XC    TCAPPLCR,TCAPPLCR   CLEAR APPLIED AMOUNT FOR FTRACK REC.         
         MVC   TACRINV,TRINV       SET NEW INVOICE NUMBER                       
         B     FT30                                                             
         SPACE 1                                                                
FT20     MVC   TCAPPLCR,ACCAPPL    SAVE APPLIED AMOUNT                          
         LTR   R2,R2               ELSE TEST FOR REMAINING BALANCE              
         BNP   FT25                                                             
         A     R2,ACCAPPL          ADD APPLIED CREDITS TO BALANCE               
         BNM   *+6                                                              
         XR    R2,R2               INSURE BALANCE DOESN'T GO NEGATIVE           
         ST    R2,TACRBAL                                                       
         B     FT30                                                             
         SPACE 1                                                                
FT25     MVC   TRBAL,TACRBAL       SAVE CURRENT BALANCE                         
         BAS   RE,WSPRADG          TEST FOR SPECIAL WSP RADIO CONDITION         
         BNE   FT40                                                             
         A     R2,SORTAPPL         YES - ADD EXCESS CREDITS TO BALANCE          
         ST    R2,TACRBAL                                                       
         B     *+10                                                             
FT30     MVC   TRBAL,TACRBAL       SAVE NEW BALANCE                             
         SPACE 1                                                                
         BAS   RE,WRICAST          WRITE BACK CAST RECORD                       
         SPACE 1                                                                
FT40     BAS   RE,TRACK            * ALWAYS TRACK RETRO CHANGES *               
         SPACE 1                                                                
         BAS   RE,WSPRADG          TEST FOR SPECIAL WSP RADIO CONDITION         
         BNE   FTX                                                              
         MVC   TCAPPLCR,SORTAPPL   YES - SET ADD'L APPLIED AMOUNT               
         MVC   TRBAL,TACRBAL       SET NEW BALANCE                              
         BAS   RE,TRACK            ADD SPECIAL FTRACK RECORD                    
         SPACE 1                                                                
FTX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TESTS WHETHER SPECIAL WSP RADIO CONDITION EXISTS         
         SPACE 1                                                                
WSPRADG  DS    0H                                                               
**NO-OP* CLI   TGUSEQU,UWSP        IF THIS IS A WILDSPOT PAYMENT                
**NO-OP* BNE   WSPNO                                                            
**NO-OP* TM    TGMEEQU,RADIO       FOR RADIO                                    
**NO-OP* BZ    WSPNO                                                            
**NO-OP* CLI   TGCAT,C'G'          AND CATEGORY BEGINS WITH 'G'                 
**NO-OP* BNE   WSPNO                                                            
**NO-OP* OC    SORTAPPL,SORTAPPL   THEN IF THERE WAS EXCESS CREDITS             
**NO-OP* BZ    WSPNO                                                            
**NO-OP* CR    RE,RE               RETURN CC EQ                                 
**NO-OP* BR    RE                                                               
WSPNO    LTR   RE,RE               NO - RETURN CC NE                            
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE WRITES CAST RECORDS TO FILE                              
         SPACE 1                                                                
WRICAST  NTR1                                                                   
         GOTO1 PUTREC              WRITE BACK CAST RECORD                       
         SPACE 1                                                                
         GOTO1 =A(MYTRACE),DMCB,=C'UPDATED CAST'                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS/ADDS FTRACK TRACKING RECORDS                      
         SPACE 1                                                                
TRACK    NTR1                                                                   
         MVC   AIO,AIO2            SET SECONDARY I/O AREA                       
         XC    KEY,KEY                                                          
         LA    R3,KEY              BUILD BASIC KEY                              
         USING TLFTD,R3                                                         
         MVI   TLFTCD,TLFTCDQ      RECORD CODE                                  
         MVC   TLFTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLFTCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLFTCAST,TGCSORT+4  CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT(6),TRCRCYC CYCLE DATES                                  
         XC    TLFTSTRT(6),=6X'FF' (COMPLEMENTED)                               
         SPACE 1                                                                
         MVC   TGDUB(4),TCAPPLCR   SET APPLIED AMOUNT FOR BLDTRK                
         MVC   TGDUB+4(4),TRBAL    SET NEW BALANCE AS WELL                      
         SPACE 1                                                                
         MVC   TGINV,TRINV                        SET INVOICE IN GLOBAL         
         GOTO1 BLDTRK,DMCB,TLFTTRK-TLFTD,A(CHKIO) BUILD TRACKING RECORD         
         SPACE 1                                                                
         L     R3,AIO              R3=A(RECORD)                                 
         MVC   TLFTINV,TRINV       ADD INVOICE NUMBER TO KEY                    
         SPACE 1                                                                
         BAS   RE,HISTCMNT         ADD HISTORY COMMENT TO RECORD                
         SPACE 1                                                                
         GOTO1 =A(MYTRACE),DMCB,=C'FTRACK'                                      
         SPACE 1                                                                
         BAS   RE,ADDIT            ADD TRACKING RECORD TO FILE                  
         SPACE 1                                                                
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADDS RECORDS TO THE FILE OR TAPES                        
         SPACE 1                                                                
*                                  AIO=A(RECORD)                                
ADDIT    NTR1                                                                   
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLRCD,R4                                                         
         CLI   TLRCCD,TLCKCDQ      SET TO ADD TO CORRECT ACTIVITY COUNT         
         BNE   *+14                                                             
         AP    CHKCOUNT,=P'1'                                                   
         B     *+10                                                             
         AP    TALCOUNT,=P'1'                                                   
         SPACE 1                                                                
         CLI   TLRCCD,TLFTCDQ       IF ADDING FTRACK RECORD                     
         BE    *+12                                                             
         TM    TROPTS,TRWRIDSK      OR IF WRITING ALL RECORDS TO DISK           
         BZ    ADDI20                                                           
         GOTO1 =A(MYADDREC),DMCB,(RC)  ADD NEW RECORD                           
         XC    ELEM,ELEM                                                        
         XR    RF,RF                                                            
         TM    TROPTS,TRTRACE      SET TRACE PARAM BIT IF REQUESTED             
         BZ    *+8                                                              
         LA    RF,X'10'                                                         
         GOTO1 AADDPTRS,DMCB,((RF),ELEM)  ADD PASSIVE POINTERS AS WELL          
         B     ADDIX                                                            
         SPACE 1                                                                
ADDI20   LA    R2,TALTAPE          ELSE SET TO WRITE TO TAPE                    
         CLI   TLRCCD,TLCKCDQ                                                   
         BNE   *+8                                                              
         LA    R2,CHKTAPE                                                       
         LH    RF,TLRCLEN          SET VARIABLE RECORD LENGTH                   
         LA    RF,4(RF)                                                         
         SH    R4,=H'4'                                                         
         XC    0(4,R4),0(R4)                                                    
         STH   RF,0(R4)                                                         
         PUT   (R2),(R4)           WRITE TO TAPE                                
         SPACE 1                                                                
ADDIX    B     XIT                                                              
         EJECT                                                                  
*              TOTAL ROUTINES                                                   
         SPACE 1                                                                
INVTOTS  NTR1                                                                   
         LA    R3,TRINACCS                                                      
         LA    R2,=CL14'INVOICE TOTALS'                                         
         BAS   RE,ALLTOTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
CLITOTS  NTR1                                                                   
         LA    R3,TRCLACCS                                                      
         LA    R2,=CL14'CLIENT TOTALS'                                          
         BAS   RE,ALLTOTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
AGYTOTS  NTR1                                                                   
         LA    R3,TRAYACCS                                                      
         LA    R2,=CL14'AGENCY TOTALS'                                          
         MVI   RCSUBPRG,1                                                       
         BAS   RE,ALLTOTS                                                       
         BNE   *+8                                                              
         BAS   RE,SUMMARY                                                       
         MVI   RCSUBPRG,0                                                       
         B     XIT                                                              
         SPACE 1                                                                
OFFTOTS  NTR1                                                                   
         LA    R3,TROFACCS                                                      
         LA    R2,=CL14'OFFICE TOTALS'                                          
         MVI   RCSUBPRG,2                                                       
         BAS   RE,ALLTOTS                                                       
         BNE   *+8                                                              
         BAS   RE,SUMMARY                                                       
         MVI   RCSUBPRG,0                                                       
         B     XIT                                                              
         SPACE 1                                                                
REQTOTS  NTR1                                                                   
         LA    R3,TRRQACCS                                                      
         LA    R2,=CL14'REPORT TOTALS'                                          
         MVI   RCSUBPRG,3                                                       
         BAS   RE,ALLTOTS                                                       
         BAS   RE,SUMMARY                                                       
         B     XIT                                                              
         SPACE 3                                                                
ALLTOTS  NTR1                                                                   
         TM    TROPTS2,TRDOWN      DOWNLOAD FORMAT, NO NEED                     
         BZ    ALLTOTS5                                                         
         XC    0(ACCLNQ,R3),0(R3)                                               
         B     YES                                                              
ALLTOTS5 OC    0(ACCLNQ,R3),0(R3)                                               
         BZ    NO                                                               
         MVC   P,SPACES            CLEAR PRINT LINE                             
         GOTO1 =A(FORMAT),DMCB,(R2)                                             
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE PRINTS SUMMARY TOTALS                                    
         SPACE 1                                                                
SUMMARY  NTR1                                                                   
         TM    TROPTS2,TRDOWN      DOWNLOAD FORMAT, NO NEED                     
         BO    XIT                                                              
         BAS   RE,BXBOT            FORCE IN BOTTOM OF BOX                       
                                                                                
         LA    R2,CNTTAB           PRINT ALL TOTALS                             
         L     R3,=A(LITTAB)       R3=A(LITERALS)                               
         SPACE 1                                                                
         CLI   RCSUBPRG,2          IF THIS IS OFFICE TOTALS                     
         BNE   *+8                                                              
         LA    R2,4(R2)            BUMP TO OFFICE COUNTS                        
         SPACE 1                                                                
         CLI   RCSUBPRG,3          IF THIS IS REQUEST TOTALS                    
         BNE   *+8                                                              
         LA    R2,8(R2)            BUMP TO REQUEST COUNTS                       
         SPACE 1                                                                
SU10     CLI   0(R3),X'FF'                                                      
         BE    SUX                                                              
         CLC   =C'SKIP',0(R3)                                                   
         BE    SU20                                                             
         SPACE 1                                                                
         EDIT  (P4,0(R2)),(7,P+1),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+9(L'LITTAB),0(R3)                                              
         SPACE 1                                                                
         CLI   RCSUBPRG,3          UNLESS THIS IS REQUEST TOTALS                
         BE    *+10                                                             
         AP    4(4,R2),0(4,R2)     ADD TO NEXT HIGHEST LEVEL                    
         SPACE 1                                                                
         ZAP   0(4,R2),=P'0'       CLEAR THIS LEVEL                             
         SPACE 1                                                                
SU20     BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         LA    R2,L'CNTTAB(R2)                                                  
         LA    R3,L'LITTAB(R3)                                                  
         B     SU10                                                             
         SPACE 1                                                                
SUX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POP IN A BOX BOTTOM                                   
         SPACE 1                                                                
BXBOT    NTR1                                                                   
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         MVI   BOXINIT,0                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE PRINTS A LINE                                            
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              SORTER ROUTINES                                                  
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         TM    TRSTAT,TRSORTNG     TEST SORT INITIALIZED ALREADY                
         BO    PUTS10                                                           
         LA    R0,SORTKLNQ         SET SOFT L'SORT KEY                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         SPACE 1                                                                
         LA    R0,SORTLNQ          SET SOFT L'SORT RECORD                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         SPACE 1                                                                
SORTOPEN GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    TRSTAT,TRSORTNG                                                  
         MVC   SORTOPEN(2),=X'0000'                                             
         SPACE 1                                                                
PUTS10   GOTO1 SORTER,DMCB,=C'PUT',TRSRTREC                                     
         SPACE 1                                                                
         GOTO1 MYTRACE2,DMCB,=C'PUTSORT',TRSRTREC,SORTLNQ                       
         B     XIT                                                              
         SPACE 3                                                                
GETSORT  NTR1                                                                   
         MVC   TRLSTREC,TRSRTREC   SAVE CURRENT SORT RECORD                     
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)                                                      
         BZ    NO                                                               
         MVC   PUTS10(2),=X'0000'                                               
         MVC   TRSRTREC,0(RF)      MOVE TO LOCAL W/S                            
         SPACE 1                                                                
         GOTO1 MYTRACE2,DMCB,=C'GETSORT',TRSRTREC,SORTLNQ                       
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE HANDLES STORAGE TRACES                                   
         SPACE 1                                                                
MYTRACE2 NTR1                                                                   
         TM    TROPTS,TRTRACE                                                   
         BZ    XIT                                                              
         LM    R2,R4,0(R1)                                                      
         ZIC   RF,0(R1)                                                         
         GOTO1 TRACE,DMCB,(R3),(R4),(R2),(RF)                                   
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
RETROCMT DC    C'RETROACTIVE RATE'                                              
INVHCMNT DC    C' INCREASE FOR INV#'                                            
INVGCMNT DC    C' INCREASE FOR INVOICE NUMBER '                                 
NOREC    DC    CL36'***** RECORD NOT FOUND *****'                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS, CONT'D                                                
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=000'                                   
         SPACE 2                                                                
ERRFILE  DCB   DDNAME=ERRFILE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,LRECL=12,BLKSIZE=120                                    
         SPACE 2                                                                
TALTAPE  DCB   DDNAME=TALTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
         SPACE 1                                                                
CHKTAPE  DCB   DDNAME=CHKTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
*              CONSTANTS, CONT'D.                                               
         SPACE 1                                                                
CNTTAB   DS    0CL12                                                            
TALCOUNT DC    3PL4'0'                                                          
CHKCOUNT DC    3PL4'0'                                                          
***      DC    3PL4'0'                                                          
REGACNT  DC    3PL4'0'                                                          
CODACNT  DC    3PL4'0'                                                          
AGYCNT   DC    3PL4'0'                                                          
REGCNT   DC    3PL4'0'                                                          
CODCNT   DC    3PL4'0'                                                          
INVCNT   DC    3PL4'0'                                                          
REGCCNT  DC    3PL4'0'                                                          
CODCCNT  DC    3PL4'0'                                                          
CHKCNT   DC    3PL4'0'                                                          
****     DC    3PL4'0'                                                          
CASTCNT  DC    3PL4'0'                                                          
TSHTCNT  DC    3PL4'0'             # OF ERROR INVOICES USING TIMESHEETS         
VITACNT  DC    3PL4'0'             # OF ERROR INVOICES FROM VITA                
ERRCNT   DC    3PL4'0'                                                          
                                                                                
ERRTAB   EQU   *                                                                
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
         EJECT                                                                  
*              CONSTANTS, CONT'D                                                
         SPACE 2                                                                
*              CONSTANTS, CONT'D.                                               
         SPACE 2                                                                
LITTAB   DS    0CL40               ORDER CORRESPONDS TO CNTTAB!!                
         DC    CL40'TALFIL ADDS'                                                
         DC    CL40'CHKFIL ADDS'                                                
***      DC    CL40'SKIP'                                                       
         DC    CL40'REGULAR (NON-PUR) AGENCIES'                                 
         DC    CL40'PUR AGENCIES'                                               
         DC    CL40'TOTAL AGENCIES'                                             
*                                                                               
         DC    CL40'REGULAR (NON-PUR) INVOICES'                                 
         DC    CL40'PUR INVOICES'                                               
         DC    CL40'TOTAL INVOICES'                                             
         DC    CL40'REGULAR (NON-PUR) CHECKS'                                   
         DC    CL40'PUR CHECKS'                                                 
         DC    CL40'TOTAL CHECKS'                                               
***      DC    CL40'SKIP'                                                       
         DC    CL40'MISSING CAST RECORD WARNINGS'                               
         DC    CL40'ERROR INVOICES WITH TIMESHEETS'                             
         DC    CL40'ERROR INVOICES FROM VITA'                                   
         DC    CL40'TOTAL ERRORS'                                               
*                                                                               
         DC    CL40'ERROR  1 - CREDIT INVOICES'                                 
         DC    CL40'ERROR  2 - NO RATES DEFINED'                                
         DC    CL40'ERROR  3 - 4 WK CABLE INVOICES'                             
         DC    CL40'ERROR  4 - 52 WK CABLE INVOICES'                            
         DC    CL40'ERROR  5 - UNAPPROVED INVOICES'                             
         DC    CL40'ERROR  6 - ZERO PAYMENT'                                    
         DC    CL40'ERROR  7 - NEGATIVE PAYMENT'                                
         DC    CL40'ERROR  8 - EXCEEDED MAXIMUM UPGRADE'                        
         DC    CL40'ERROR  9 - REUSE PAID AFTER END OF PD'                      
         DC    CL40'ERROR 10 - CYCLE STARTS BEFORE END OF PD'                   
         DC    CL40'ERROR 11 - MANUAL OVERRIDE, NO CHK CMT'                     
         DC    X'FF'                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*INVIO**'                                                      
         DC    F'0'                                                             
INVIO    DC    2000X'00'           INVOICE RECORD                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*CHKIO**'                                                      
         DC    F'0'                                                             
CHKIO    DC    2000X'00'           CHECK RECORD                                 
         EJECT                                                                  
*              CONSTANTS, CONT'D.                                               
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SPROG 0,1,2,3                                                          
         SSPEC H1,2,RUN                                                         
         SSPEC H1,52,C'RETROACTIVE PAYMENT GENERATOR'                           
         SSPEC H2,52,29X'BF'                                                    
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,115,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H4,99,C'PERIOD'                                                  
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         SSPEC H3,2,C'OFFICE'                                                   
         SPROG 0,1                                                              
         SSPEC H4,2,C'AGENCY'                                                   
         SPROG 0                                                                
         SSPEC H5,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SSPEC H8,03,C'COMMERCIAL'                                              
         SSPEC H9,03,C'    ID'                                                  
         SPACE 1                                                                
         SSPEC H8,15,C' OLD    NEW     USE       ESTIMATE'                      
         SSPEC H9,15,C' INV    INV'                                             
         SPACE 1                                                                
         SSPEC H8,55,C' S/S     CAT CAM A'                                      
         SSPEC H9,55,C'NUMBER'                                                  
         SPACE 1                                                                
         SPROG 0,1,2,3                                                          
         SSPEC H8,76,C'APPLIED    INDIVIDUAL  CORPORATE'                        
         SSPEC H9,76,C'AMOUNT       PAYMENT    PAYMENT'                         
         SPACE 1                                                                
         SSPEC H8,112,C'SUBJ TO    PENSION'                                     
         SSPEC H9,112,CL19' P && H     && HEALTH'                               
         DC    X'00'                                                            
         SPACE                                                                  
         DROP  RA,R5               DROP SECONDARY BASE REGS                     
         EJECT                                                                  
*              ROUTINE FINDS RELEVANT TACREL IN CAST RECORD                     
         SPACE 1                                                                
GETTACR  NTR1  BASE=*,LABEL=*                                                   
         XC    TGELEM,TGELEM       RETURNS A(ELEMENT) IN TGELEM                 
         XC    TGDUB,TGDUB                                                      
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ      SET ELEMENT CODE                             
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GTCR10   BRAS  RE,NEXTEL                                                        
         JNE   GTCRX                                                            
                                                                                
         USING TACRD,R4                                                         
         CLC   TCPCYCS,TACRSTRT    CYCLE START MUST FALL BETWEEN START          
         JL    GTCR10                                                           
         CLC   TCPCYCS,TACREND     AND END                                      
         JH    GTCR10                                                           
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         JZ    *+14                                                             
         CLC   TCPCYC,TACRSTRT     DATES MUST MATCH EXACTLY                     
         JNE   GTCR10                                                           
                                                                                
         OC    TACRINV,TACRINV     IF THERE'S AN INVOICE NUMBER                 
         JZ    *+14                                                             
         CLC   TACRINV,TGDUB       TEST THIS INVOICE LATER THAN SAVED           
         JNH   GTCR10                                                           
                                                                                
         ST    R4,TGELEM           SAVE A(LAST ELEMENT)                         
         MVC   TGDUB(6),TACRINV    AND LAST INVOICE NUMBER                      
                                                                                
         J     GTCR10              KEEP LOOKING UNTIL NO MORE ELS.              
                                                                                
GTCRX    OC    TGELEM,TGELEM       RETURN CC                                    
         JNZ   YES                                                              
         J     NO                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
MYTRACE  NTR1  BASE=*,LABEL=*                                                   
         TM    TROPTS,TRTRACE                                                   
         JZ    XIT                                                              
         L     R2,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
         MVC   WORK,0(R2)          MOVE LITERAL TO WORK                         
         LA    R4,WORK(R3)                                                      
         MVC   0(7,R4),=C' (AIO?)' ADD I/O AREA LITERAL                         
         LA    R3,7(R3)            BUMP L'LITERAL                               
         CLC   AIO,AIO1                                                         
         JNE   *+8                                                              
         MVI   5(R4),C'1'          SET CURRENT I/O AREA LITERAL                 
         CLC   AIO,AIO2                                                         
         JNE   *+8                                                              
         MVI   5(R4),C'2'                                                       
         CLC   AIO,=A(INVIO)                                                    
         JNE   *+14                                                             
         MVC   2(6,R4),=C'INVIO)'                                               
         LA    R3,1(R3)                                                         
         CLC   AIO,=A(CHKIO)                                                    
         JNE   *+14                                                             
         MVC   2(6,R4),=C'CHKIO)'                                               
         LA    R3,1(R3)                                                         
         CLC   AIO,TIAREC                                                       
         JNE   *+14                                                             
         MVC   2(7,R4),=C'TIAREC)'                                              
         LA    R3,2(R3)                                                         
         GOTO1 TRACE,DMCB,AIO,0,WORK,(R3)                                       
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 3                                                                
RETRCHK  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R3,SORTACCS                                                      
         USING ACCD,R3             R3=A(SORT RECORD ACCUMS)                     
         XC    0(ACCLNQ,R3),0(R3)  CLEAR CHK ACCUMS                             
                                                                                
         L     R4,TRACHK                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   RETRCHK3                                                         
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         MVC   ACCAPPL,TAPDAPPL                                                 
         MVI   SORTACDE,APPLOTH    SET OTHER APPLIED CODE                       
         MVC   ACCPAYI,TAPDPAYI                                                 
         MVC   ACCPAYC,TAPDPAYC                                                 
         MVC   ACCSPNH,TAPDSPNH                                                 
         MVC   ACCPNH,TAPDPNH                                                   
                                                                                
RETRCHK3 L     R4,TRACHK                                                        
         MVI   ELCODE,TARPELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TARPD,R4            R4=A(RETRO P&H AMOUNT ELEMENT)               
         MVC   ACCOSPNH,TARPBASE                                                
         MVC   ACCPNHDF,TARPDIFF                                                
                                                                                
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADDOWN NTR1  BASE=*,LABEL=*                                                   
         LA    RE,=C'Office'                                                    
         GOTO1 =A(OUTPDOWN),DMCB,(C'T',(RE)),6                                  
         LA    RE,=C'Office Name'                                               
         GOTO1 (RF),DMCB,(C'T',(RE)),11                                         
         LA    RE,=C'Agency'                                                    
         GOTO1 (RF),DMCB,(C'T',(RE)),6                                          
         LA    RE,=C'Agency Name'                                               
         GOTO1 (RF),DMCB,(C'T',(RE)),11                                         
         LA    RE,=C'PUR'                                                       
         GOTO1 (RF),DMCB,(C'T',(RE)),3                                          
         LA    RE,=C'Client'                                                    
         GOTO1 (RF),DMCB,(C'T',(RE)),6                                          
         LA    RE,=C'Client Name'                                               
         GOTO1 (RF),DMCB,(C'T',(RE)),11                                         
         LA    RE,=C'Commercial ID'                                             
         GOTO1 (RF),DMCB,(C'T',(RE)),13                                         
         LA    RE,=C'Old Invoice'                                               
         GOTO1 (RF),DMCB,(C'T',(RE)),11                                         
         LA    RE,=C'New Invoice'                                               
         GOTO1 (RF),DMCB,(C'T',(RE)),11                                         
         LA    RE,=C'Manual Fix'                                                
         GOTO1 (RF),DMCB,(C'T',(RE)),10                                         
         LA    RE,=C'Use'                                                       
         GOTO1 (RF),DMCB,(C'T',(RE)),3                                          
         LA    RE,=C'Version'                                                   
         GOTO1 (RF),DMCB,(C'T',(RE)),7                                          
         LA    RE,=C'Estimate'                                                  
         GOTO1 (RF),DMCB,(C'T',(RE)),8                                          
         LA    RE,=C'Auth/PO'                                                   
         GOTO1 (RF),DMCB,(C'T',(RE)),7                                          
         LA    RE,=C'PID Number'                                                
         GOTO1 (RF),DMCB,(C'T',(RE)),10                                         
         LA    RE,=C'Performer'                                                 
         GOTO1 (RF),DMCB,(C'T',(RE)),9                                          
         LA    RE,=C'W4 Type'                                                   
         GOTO1 (RF),DMCB,(C'T',(RE)),7                                          
         LA    RE,=C'Category'                                                  
         GOTO1 (RF),DMCB,(C'T',(RE)),8                                          
         LA    RE,=C'Camera'                                                    
         GOTO1 (RF),DMCB,(C'T',(RE)),6                                          
         LA    RE,=C'Include'                                                   
         GOTO1 (RF),DMCB,(C'T',(RE)),7                                          
         LA    RE,=C'Reimb Exp'                                                 
         GOTO1 (RF),DMCB,(C'T',(RE)),9                                          
         LA    RE,=C'New Reimb Exp'                                             
         GOTO1 (RF),DMCB,(C'T',(RE)),13                                         
         LA    RE,=C'Apply Code'                                                
         GOTO1 (RF),DMCB,(C'T',(RE)),10                                         
         LA    RE,=C'Applied Amount'                                            
         GOTO1 (RF),DMCB,(C'T',(RE)),14                                         
         LA    RE,=C'Individual Payment'                                        
         GOTO1 (RF),DMCB,(C'T',(RE)),18                                         
         LA    RE,=C'Corporate Payment'                                         
         GOTO1 (RF),DMCB,(C'T',(RE)),17                                         
         LA    RE,=C'Subj to P&&H'                                              
         GOTO1 (RF),DMCB,(C'T',(RE)),11                                         
         LA    RE,=C'Pension && Health'                                         
         GOTO1 (RF),DMCB,(C'T',(RE)),16                                         
                                                                                
         BRAS  RE,EOLDOWN                                                       
         J     XIT                                                              
         USING DLCBD,R3                                                         
INITDOWN NTR1  BASE=*,LABEL=*                                                   
         LA    R3,DLBLOCK                                                       
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SENDDOWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
*---------------------------------------------------------------------          
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
*---------------------------------------------------------------------          
         SPACE 1                                                                
SENDDOWN NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         EJECT                                                                  
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT DOWNLOAD FIELD                                        *         
*        ON ENTRY ... P1 BYTE 0=TYPE TO PASS                          *         
*                     P1       =A(DATA)                               *         
*                     P2       =LENGTH                                *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLBLOCK                                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         CLI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         JE    OPD30                                                            
                                                                                
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         J     OPD50                                                            
                                                                                
OPD30    EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLD(0),0(RF)                                                 
                                                                                
OPD50    GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
                                                                                
*                                                                               
*              END OF DOWNLOAD                                                  
         SPACE 1                                                                
         USING DLCBD,R5                                                         
ENDDOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,DLBLOCK                                                       
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              END OF LINE - DOWNLOAD                                           
         SPACE 1                                                                
EOLDOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,DLBLOCK                                                       
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              INVOICE INFO - DOWNLOAD                                          
         SPACE 1                                                                
INVDOWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 =A(OUTPDOWN),DMCB,(C'T',TGOFF),L'TGOFF   OFFICE                  
         GOTO1 (RF),DMCB,(C'T',TROFNAME),L'TROFNAME                             
         GOTO1 (RF),DMCB,(C'T',TGAGY),L'TGAGY           AGENCY                  
         GOTO1 (RF),DMCB,(C'T',TRAYNAME),L'TRAYNAME                             
                                                                                
         MVC   WORK,SPACES                                                      
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS PUR AGENCY                        
         JZ    *+10                                                             
         MVC   WORK(5),=C'(PUR)'                                                
         GOTO1 (RF),DMCB,(C'T',WORK),5                                          
         GOTO1 (RF),DMCB,(C'T',TGCLI),L'TGCLI           CLIENT                  
         GOTO1 (RF),DMCB,(C'T',TRCLNAME),L'TRCLNAME                             
         GOTO1 (RF),DMCB,(C'T',TRCOMML),L'TRCOMML       COMML                   
         GOTO1 TINVCON,DMCB,SORTINV,BLOCK,DATCON                                
         GOTO1 =A(OUTPDOWN),DMCB,(C'T',BLOCK),6         OLD INV                 
                                                                                
         MVC   BLOCK(6),SPACES                                                  
         LA    R3,=C' '                                                         
         TM    SORTSTAT,SORTMFIX   MANUALLY FIXED?                              
         BZ    INVD20              NO, CONTINUE NORMALLY                        
         LA    R3,=C'Y'                                                         
         OC    SORTFIXN,SORTFIXN   IF FIX NUMBER SHOW IT INSTEAD                
         BZ    *+8                                                              
         LA    R3,SORTFIXN                                                      
                                                                                
         GOTO1 TINVCON,DMCB,SORTMINV,BLOCK,DATCON       FIXD INVOICE            
         B     INVD50                                                           
                                                                                
INVD20   CLI   TRINVERR,0          IF THERE'S NO ERROR SET                      
         JE    INVD30                                                           
         MVC   BLOCK(3),=C'ERR'    ERROR, SHOW IT       ERROR                   
         LA    RE,BLOCK+4                                                       
         EDIT  (1,TRINVERR),(2,(RE)),ALIGN=LEFT                                 
         J     INVD50                                                           
                                                                                
INVD30   GOTO1 TINVCON,DMCB,TRINV,BLOCK,DATCON          NEW INVOICE             
INVD50   GOTO1 =A(OUTPDOWN),DMCB,(C'T',BLOCK),6         NEW/MAN/ERROR           
         GOTO1 (RF),DMCB,(C'T',(R3)),1                  MANUAL FIX              
INVD59   GOTO1 (RF),DMCB,(C'T',TGUSCDE),L'TGUSCDE       USE                     
                                                                                
         MVI   BLOCK,C' '                                                       
         L     R4,=A(INVIO)                                                     
         MVI   ELCODE,TAVRELQ      IF VERSION PAID                              
         BRAS  RE,GETEL                                                         
         JNE   INVD60                                                           
         MVI   BLOCK,C'V'                                                       
                                                                                
INVD60   GOTO1 =A(OUTPDOWN),DMCB,(C'T',BLOCK),1                                 
*        GOTO1 (RF),DMCB,(C'T',TRINEST),L'TRINEST                               
         GOTO1 (RF),DMCB,(C'T',SORTEST),L'SORTEST                               
                                                                                
         MVC   BLOCK(16),SPACES                                                 
         L     R4,=A(INVIO)                                                     
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ      IF VERSION PAID                              
         BRAS  RE,GETEL                                                         
         B     INVD65                                                           
INVD65   BRAS  RE,NEXTEL                                                        
INVD68   JNE   INVD80                                                           
         CLI   TANUTYPE,TANUTAUT                                                
         BNE   INVD65                                                           
         SR    R1,R1                                                            
         IC    R1,TANULEN                                                       
         SH    R1,=Y(TANULNQ)                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
                                                                                
         MVC   BLOCK(0),TANUMBER                                                
                                                                                
INVD80   GOTO1 =A(OUTPDOWN),DMCB,(C'T',BLOCK),16                                
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ACCUMS - DOWNLOAD                                                
         SPACE 1                                                                
         USING ACCD,R3             R3=A(ACCUMS)                                 
ACCMDOWN NTR1  BASE=*,LABEL=*                                                   
         LA    R3,SORTACCS         SET R3=A(SORT RECORD ACCUMS)                 
         L     RE,ACCAPPL          ALWAYS DISPLAY APPLIED AMT AS POS.           
         LPR   RE,RE                                                            
         EDIT  (RE),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-    APPLIED AMOUNT           
         GOTOR OUTPDOWN,DMCB,(C'N',BLOCK),12                                    
         EDIT  (4,ACCPAYI),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-  INDIV.              
         GOTOR OUTPDOWN,DMCB,(C'N',BLOCK),12                                    
         EDIT  (4,ACCPAYC),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-  CORP                
         GOTOR OUTPDOWN,DMCB,(C'N',BLOCK),12                                    
         EDIT  (4,ACCSPNH),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-  SUB P&H             
         GOTOR OUTPDOWN,DMCB,(C'N',BLOCK),12                                    
         EDIT  (4,ACCPNH),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-   P&H                 
         GOTOR OUTPDOWN,DMCB,(C'N',BLOCK),12                                    
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              ROUTINE TO CHECK IF INVOICE HAS TIMESHEETS                       
*                      OR INVOICE CAME FROM VITA                                
*=====================================================================          
         SPACE 1                                                                
INVSTAT  NTR1  BASE=*,LABEL=*                                                   
         NI    SORTSTAT,X'FF'-SORTTSHT-SORTVITA                                 
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   ISTAT30                                                          
         USING TAIND,R4                                                         
         OC    TAINTMCO,TAINTMCO                                                
         JZ    ISTAT30                                                          
         OI    SORTSTAT,SORTTSHT   YES, HAS TIMESHEETS                          
                                                                                
ISTAT30  L     R4,TRAINV                                                        
         MVI   ELCODE,TAFNELQ                                                   
         BRAS  RE,GETEL                                                         
         B     ISTAT50                                                          
ISTAT40  BRAS  RE,NEXTEL                                                        
ISTAT50  JNE   XIT                                                              
         USING TAFND,R4                                                         
         CLI   TAFNTYPE,TAFNTWEB   WEB APPLICATION ID                           
         JNE   ISTAT40                                                          
         CLC   =C'VC',TAFNNAME     VITA WEB                                     
         JE    *+10                                                             
         CLC   =C'VS',TAFNNAME                                                  
         JE    *+10                                                             
         CLC   =C'RC',TAFNNAME                                                  
         JE    *+10                                                             
         CLC   =C'RS',TAFNNAME                                                  
         JNE   XIT                                                              
         OI    SORTSTAT,SORTVITA   YES, FROM VITA                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILTER INVOICE                                        
         SPACE 1                                                                
FILTINV  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 USEVAL,DMCB,TIUSE,TIUTYPE  SET GLOBAL USE VALUES                 
                                                                                
         TM    TGUSSTA3,ADDENUSE+SOAPUSE       NO ADDEN, SOAPS                  
         JO    FINVIGN                                                          
         TM    TGUSSTA4,INDUSTRL      IGNORE INDUSTRIALS USES                   
         JO    FINVIGN                                                          
                                                                                
         CLI   TGUSEQU,UPRM        IGNORE PROMOS                                
         JE    FINVIGN                                                          
         CLI   TGUSEQU,UPRR                                                     
         JE    FINVIGN                                                          
                                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4            R4=A(INVOICE DETAILS ELEMENT)                
         TM    TAINSTA3,TAINSRSK   SKIP THIS INVOICE FOR RETROS                 
         BO    FINVIGN                                                          
                                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
                                                                                
         TM    TAPDOPT3,TAPDORET   IGNORE RETRO PAYMENTS                        
         BO    FINVIGN                                                          
                                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FINV2                                                            
         USING TACOD,R4            R4=A(COMM'L DETAILS ELEMENT)                 
         CLI   TACOTYPE,C'M'       IGNORE MUSIC, PRINT AND PROMO                
         JE    FINVIGN                                                          
         CLI   TACOTYPE,C'P'                                                    
         JE    FINVIGN                                                          
         CLI   TACOTYPE,C'R'                                                    
         JE    FINVIGN                                                          
         CLI   TACOCTYP,CCTY2404   04 ACTRA COMML TYPE?                         
         JE    FINV1                                                            
         J     FINV2                                                            
*        CLI   TACOCTYP,X'10'      04A ACTRA COMML TYPE?                        
*        JE    FINV1                                                            
*        CLI   TACOCTYP,X'11'      04B ACTRA COMML TYPE?                        
*        JNE   FINV2                                                            
FINV1    OI    TRSTAT,ACT04AB                                                   
                                                                                
FINV2    TM    TROPTS2,TRSESS         IF RUNNING SESS OPTION                    
         JZ    FINV4                                                            
         GOTO1 =A(SESSOPT),DMCB,(RC)  HANDLE IN SPECIAL ROUTINE                 
         JNE   FINVIGN                                                          
                                                                                
FINV4    TM    TROPTS2,TRNOWSPU    IF RUNNING NOWSPU OPTION                     
         JZ    FINV5                                                            
         CLI   TGUSEQU,UWSP        IF WSP PAYMENT                               
         JNE   FINV8                                                            
         CLI   TGUSTYP,UWSPU13W    AND 13 WEEK UPGRADE TYPE                     
         JNE   FINV8                                                            
         L     R4,TRAINV                                                        
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FINV8                                                            
         USING TACOD,R4            R4=A(COMM'L DETAILS ELEMENT)                 
                                                                                
         CLI   TACOMED,TACOMEDT    IF MEDIA IS TV, IGNORE THE INVOICE           
         JE    FINVIGN             - NO UPGRADE AMOUNT                          
         CLI   TACOMED,TACOMEDI    OR IF MEDIA IS INTERNET                      
         JE    FINVIGN                                                          
         CLI   TACOMED,TACOMEDN    OR IF MEDIA IS NEW MEDIA                     
         JE    FINVIGN                                                          
                                                                                
FINV5    TM    TROPTS2,TREXTRAS    IF RUNNING EXTRAS OPTION                     
         JZ    FINV8                                                            
         TM    TGUSXCAT,EXTRA                                                   
         JO    FINVIGN             IGNORE IF USE EXCLUDES EXTRAS                
         TM    TGUSXCAT,NOHLD                                                   
         JO    FINVIGN             IGNORE IF NO HOLDS ALSO                      
                                                                                
FINV8    TM    TROPTS,TRBOTH       IF NOT RUNNING BOTH OPTION                   
         JO    FINV10                                                           
         TM    TROPTS,TRPHONLY     AND IF NOT RUNNING P&H ONLY OPTION           
         JO    FINV10                                                           
         CLI   TGUSEQU,UGRT        IGNORE GUARANTEES                            
         JE    FINVIGN                                                          
                                                                                
**NV10   TM    TROPTS2,TRCABLE     IF RUNNING CABLE OPTION                      
**       BZ    FINV15                                                           
*INV10   CLI   TGUSEQU,UCBL        IF CBL AND LCB                               
*        JE    FINV12                                                           
*        CLI   TGUSEQU,ULCB                                                     
*        JNE   FINV15                                                           
FINV10   DS    0H                                                               
FINV12   CLC   TICSDATE,=X'B60401' IGNORE INVOICE IF CYC START < 4/1/16         
         JNL   FINV15                                                           
         OC    TICSDATE,TICSDATE   NO CYCLE START?                              
         JNZ   FINVIGN                                                          
         TM    SORTSTAT,SORTMFIX   UNLESS INVOICE IS FIXED ALREADY              
         JZ    FINVIGN             DON'T TAKE IT                                
FINV15   MVC   SORTUSE,TGUSCDE     SET USE CODE IN SORT RECORD                  
         MVC   SORTUTYP,TGUSTYP        USE TYPE                                 
         MVI   SORTERR,0           CLEAR ERROR BYTE                             
*&&DO                                                                           
         OC    TICSDATE,TICSDATE                                                
         JZ    FINV17                                                           
         CLC   TICSDATE,=X'B60401' ERROR IF CYC START < 4/1/16                  
         JNL   FINV17                                                           
         MVI   SORTERR,ERR10       ELSE SET ERROR STATUS                        
         J     FINVERR                                                          
*&&                                                                             
FINV17   L     R4,TRAINV                                                        
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4            R4=A(INVOICE DETAILS ELEMENT)                
         TM    TAINSTAT,TAINSAPR   INVOICE MUST BE APPROVED                     
         JO    *+12                                                             
         MVI   SORTERR,ERR5        ELSE SET ERROR STATUS                        
         J     FINVERR                                                          
         CLC   TAINPDTE,=X'B60612'   PAYMENT MUST BE BEFORE JUNE 13th           
         BH    FINVIGN                                                          
                                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
                                                                                
         TM    TAPDPST1,TAPDPCRD   IF CREDIT PAYMENT                            
         JZ    FINV20                                                           
         MVI   SORTERR,ERR1        SET ERROR                                    
         J     FINVERR                                                          
                                                                                
FINV20   TM    TROPTS,TRPHONLY     SKIP IF RUNNING P&H ONLY OPTION              
         JO    FINVX                                                            
         MVC   TGCOM,TAPDCOM       INTERNAL COMML NUMBER FOR CAST READ          
                                                                                
         CLI   TGUSEQU,UCAB        IF CABLE PAYMENT                             
         JNE   FINVX                                                            
         CLI   TGUSTYP,UCAB4W      AND 4 WEEK TYPE                              
         JNE   FINV30                                                           
         MVI   SORTERR,ERR3        SET ERROR                                    
         J     FINVERR                                                          
                                                                                
FINV30   CLI   TGUSTYP,UCAB52W     OR 52 WEEK TYPE                              
         JNE   FINVX                                                            
         MVI   SORTERR,ERR4        SET ERROR                                    
         J     FINVERR                                                          
                                                                                
FINVX    J     YES                                                              
                                                                                
FINVERR  DS    0H                  INVOICE IN ERROR, ALTHOUGH MAY NOT           
         J     NO                  HAVE ANY ELIGIBLE CHECKS                     
                                                                                
FINVIGN  MVI   TIMODE,PROCNOCK     IGNORE THIS INV - DON'T PASS CHECKS          
         J     NO                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              RETRO INVOICE MANUALLY FIXED                                     
*=====================================================================          
         SPACE 1                                                                
RETMFIXD NTR1  BASE=*,LABEL=*                                                   
         NI    SORTSTAT,X'FF'-SORTMFIX                                          
         XC    SORTMINV,SORTMINV                                                
         MVI   SORTFIXN,0                                                       
         XC    TRNITAB,TRNITAB                                                  
         MVI   TRNITBX,X'FF'                                                    
         LA    RF,TRNITAB                                                       
         ST    RF,TRNIPTR                                                       
                                                                                
         USING TLINPD,R3                                                        
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLINPCD,TLINRCDQ     CHECK IF THIS INV WAS FIXED                 
         MVC   TLINRAGY,SORTAGY                                                 
         MVC   TLINROIN,SORTINV                                                 
         GOTO1 HIGH                                                             
         B     RMFIX110                                                         
                                                                                
RMFIX100 GOTO1 SEQ                                                              
RMFIX110 CLC   KEY(TLINRRIN-TLINPD),KEYSAVE                                     
         BNE   RETMFIXX                                                         
                                                                                
         MVC   TRSVKEY,KEY         SAVE KEY                                     
         MVC   TRSVMINV,TLINRRIN   YES, SAVE THE FIXED RETRO INV                
         TM    TLINRRIN+5,X'80'    CANCEL INVOICE?                              
         BO    RMFIX600                                                         
                                                                                
         USING TLIND,R3                                                         
RMFIX200 XC    KEY,KEY                                                          
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,SORTAGY                                                  
         MVC   TLININV,TRSVMINV                                                 
         XC    TLININV,=X'FFFFFFFFFFFF'                                         
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(TLINLEN-TLIND),KEYSAVE                                       
         BNE   RMFIX600                                                         
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   RMFIX300                                                         
         TM    TAINSTAT,TAINSCIN+TAINSCAN    CANCEL?                            
         BZ    RMFIX250                                                         
         B     RMFIX600                                                         
                                                                                
RMFIX250 MVI   BYTE,0                                                           
         OC    TAINIDTE,TAINIDTE                                                
         BNZ   RMFIX300                                                         
         CLC   TAINPDTE,=X'B30608'    FIRST TIME RETRO GEN RAN                  
         BNE   RMFIX252                                                         
         MVI   BYTE,C'1'                                                        
         B     RMFIX300                                                         
RMFIX252 CLC   TAINPDTE,=X'B30611'    2ND TIME RETRO GEN RAN                    
         BNE   RMFIX253                                                         
         MVI   BYTE,C'2'                                                        
         B     RMFIX300                                                         
RMFIX253 CLC   TAINPDTE,=X'B30613'    3RD TIME RETRO GEN RAN                    
         BNE   RMFIX254                                                         
         TM    TROPTS3,TRFIX7B        FIX FIX7 BUG                              
         BO    RMFIX600                                                         
         MVI   BYTE,C'3'                                                        
         B     RMFIX300                                                         
RMFIX254 CLC   TAINPDTE,=X'B30626'    4TH TIME RETRO GEN RAN                    
         BNE   RMFIX259                                                         
         MVI   BYTE,C'4'                                                        
         B     RMFIX300                                                         
RMFIX259 MVI   BYTE,C'Y'              COULD HAVE BEEN REOPENED                  
                                                                                
         USING TAPDD,R4                                                         
RMFIX300 L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   RMFIX600                                                         
         TM    TAPDPST1,TAPDPCRD             CREDIT                             
         BO    RMFIX600                                                         
         TM    TAPDSTA2,TAPDSSUB             SKIP SUBSIDIARY INVS               
         BO    RMFIX600                                                         
                                                                                
         MVC   SORTMINV,TRSVMINV   YES, SAVE THE FIXED RETRO INV                
         OI    SORTSTAT,SORTMFIX   SET STATUS FLAG                              
         MVC   SORTFIXN,BYTE                                                    
                                                                                
         L     RF,TRNIPTR                                                       
         MVC   0(L'TRSVMINV,RF),TRSVMINV                                        
         MVC   6(1,RF),BYTE                                                     
                                                                                
*MFIX400 NI    SORTSTAT,X'FF'-SORTMFIX                                          
*        XC    SORTMINV,SORTMINV                                                
*        MVI   SORTFIXN,0                                                       
                                                                                
RMFIX500 L     RF,TRNIPTR                                                       
         AHI   RF,7                LENGTH OF INVOICE + 1                        
         ST    RF,TRNIPTR                                                       
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+6                 NO, KEEP FILLING IT UP                       
         DC    H'0'                                                             
                                                                                
RMFIX600 MVC   KEY,TRSVKEY                                                      
         GOTO1 HIGH                                                             
         B     RMFIX100                                                         
                                                                                
RETMFIXX DS    0H                                                               
         MVC   SORTMINV,TRNITAB                                                 
         MVC   SORTFIXN,TRNITAB+6                                               
                                                                                
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*              ROUTINE PROCESSES ELS IN RETROACTIVE INVOICE/CHECK RECS.         
*----------------------------------------------------------------------         
         SPACE 1                                                                
ELPROC   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMENTS             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ELP10    L     R2,=A(ELTAB)        R2=A(ELEMENT PROCESSING TABLE)               
                                                                                
ELP20    CLC   0(1,R4),0(R2)       MATCH ELCODE AGAINST 1ST BYTE IN TAB         
         JE    ELP30                                                            
         LA    R2,4(R2)            TRY NEXT TABLE ENTRY                         
         CLI   0(R2),X'FF'                                                      
         JNE   ELP20                                                            
         DC    H'0'                UNDEFINED ELEMENT                            
                                                                                
ELP30    XR    RF,RF                                                            
         ICM   RF,7,1(R2)          A(PROCESSING ROUTINE)                        
         JZ    ELP40               NONE DEFINED, SO SKIP                        
         LA    RE,ELP40                                                         
         C     RF,=A(DELPROC)                                                   
         BNE   *+8                                                              
         LA    RE,ELP50                                                         
         NTR1  ,                   COMMON NTR1                                  
         BR    RF                                                               
                                                                                
ELP40    BRAS  RE,NEXTEL           GET NEXT ELEMENT                             
         JE    ELP10                                                            
         J     XIT                                                              
ELP50    CLI   0(R4),0                                                          
         JE    XIT                                                              
         J     ELP10                                                            
         EJECT                                                                  
*              ELEMENT PROCESSING ROUTINES                                      
         SPACE 1                                                                
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
CAPROC   DS    0H                                                               
         MVC   TACANCDE,SORTNCDE   SET CURRENT AGENT NUMBER                     
         MVC   TRCSTEXP,TACAEXP    SAVE CAST EXPIRY FOR FTRACKS                 
         MVC   TRONOF,TACAONOF     SAVE CAMERA STATUS FOR PRINTING              
         J     XIT                                                              
                                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
PDPROC   DS    0H                                                               
         MVC   TAPDINV,TRINV       SET NEW INVOICE NUMBER                       
         OI    TAPDOPT3,TAPDORET   SET RETROACTIVE PAYMENT STATUS               
         NI    TAPDSTAT,TAPDSCAN+TAPDSLFT   TURN OFF ALL BUT THESE              
         XC    TAPDAMTS(TAPDAMTL),TAPDAMTS  PRE-CLEAR ACCUMS                    
         MVC   TRICODE,TAPDICDE                                                 
         MVC   TRPDREXP,TAPDREXP                                                
         MVI   TAPDICDE,0          CLEAR INCLUDE CODE                           
         MVC   TGCOM,TAPDCOM       INTERNAL COMML NUMBER FOR CAST READ          
         MVC   TCPCYC,TAPDCYCS     CYLE DATES FOR TACREL LOOKUP                 
         XC    TAPDREXP,TAPDREXP   NO REIMB EXP                                 
         XC    TAPDTXNW,TAPDTXNW   TAXABLE NON-WAGE                             
         XC    TAPDNTNW,TAPDNTNW   NON-TAXABLE NON-WAGE                         
                                                                                
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         MVC   TAPDACDE,SORTACDE   SET APPLY CODE FROM SORT RECORD              
                                                                                
         TM    TRSTAT,TRPROCIN     IF HANDLING INVOICE RECORD                   
         JZ    *+12                                                             
         LA    R3,TRINACCS         R3=A(INVOICE LEVEL ACCUMS)                   
         MVI   TAPDACDE,APPLOTH    APPLY CODE IS ALWAYS OTHER                   
                                                                                
         TM    TROPTS,TRPHONLY     IF RUNNING P&H ONLY OPTION                   
         JZ    *+8                                                              
         MVI   TAPDACDE,0          THERE IS NO APPLY CODE                       
                                                                                
         USING ACCD,R3                                                          
         MVC   TAPDAPPL,ACCAPPL    SET ACCUMS IN ELEMENT                        
         L     R1,ACCPAYI                                                       
         ST    R1,TAPDPAYI                                                      
         L     R0,ACCPAYC                                                       
         ST    R0,TAPDPAYC                                                      
         AR    R1,R0                                                            
         ST    R1,TAPDGRS                                                       
         MVC   TAPDSPNH,ACCSPNH                                                 
         MVC   TAPDPNH,ACCPNH                                                   
                                                                                
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         JZ    PDPX                                                             
         MVC   TAPDOFF,TROFF       SET SAVED VALUES - OFFICE                    
         MVC   TAPDCLI,TRCLI                          CLIENT                    
         MVC   TAPDPRD,TRPRD                          PRODUCT                   
PDPX     J     XIT                                                              
         EJECT                                                                  
         USING TARPD,R4            R4=A(RETRO P&H AMOUNT ELEMENT)               
RPPROC   DS    0H                                                               
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         TM    TRSTAT,TRPROCIN     IF HANDLING INVOICE RECORD                   
         JZ    *+8                                                              
         LA    R3,TRINACCS         R3=A(INVOICE LEVEL ACCUMS)                   
                                                                                
         MVC   TARPBASE,ACCOSPNH                                                
         MVC   TARPINC,ACCPNH                                                   
         MVC   TARPDIFF,ACCPNHDF                                                
*        MVC   TARPBASE,ACCSPNH    PNHONLY TO MATCH $PAY MANUAL RETRO           
*        MVC   TARPINC,ACCPNHDF                                                 
*        MVC   TARPDIFF,ACCPNH                                                  
                                                                                
RPPX     J     XIT                                                              
         EJECT                                                                  
*              ELEMENT PROCESSING ROUTINES                                      
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS EL.)                     
INPROC   DS    0H                                                               
         XC    TAINIINF,TAINIINF   CLEAR ASSIGMENT INFO                         
         MVC   TAINPDTE,TGTODAY1   SET PAID TODAY                               
         XC    TAINPTIM,TAINPTIM   CLEAR TIME                                   
         MVC   TAINQDTE,TGTODAY1   SET APPROVED TODAY                           
         XC    TAINQTIM,TAINQTIM   CLEAR TIME                                   
         XC    TAINBDTE,TAINBDTE   CLEAR BILL DATE                              
         XC    TAINCDTE,TAINCDTE   CLEAR CHECK DATE                             
         XC    TAINTMCO,TAINTMCO   CLEAR TIMESHEET                              
         NI    TAINSTAT,ALL-TAINSBIL-TAINSCHK-TAINSERR-TAINSHLD                 
         NI    TAINSTA2,ALL-TAINSHLR-TAINSHLP  TURN OFF COD REL/PRINT           
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS A COD AGENCY                      
         JZ    *+12                                                             
         OI    TAINSTAT,TAINSHLD   SET COD HOLD STATUS                          
         J     *+8                                                              
         OI    TAINSTA2,TAINSRTH   ELSE SET RETRO HOLD STATUS                   
         MVI   TAINTERR,0          CLEAR ERROR                                  
         MVI   TAINFPG,0           CLEAR SCREEN REC PAGES                       
         MVI   TAINLPG,0                                                        
         MVI   TAINCKID,0          CLEAR CHECK RUN ID                           
         XC    TAINCKRN,TAINCKRN   CLEAR CHECK RUN DATE                         
         XC    TAINHDTE,TAINHDTE   CLEAR COD PRINT DATE                         
         J     XIT                                                              
         SPACE 2                                                                
CMPROC   DS    0H                                                               
         TM    TRSTAT,TRPROCIN     IF HANDLING INVOICE RECORD                   
         JZ    CMPX                                                             
         MVI   ELCODE,TACMELQ      DELETE EXISTING COMMENT ELS.                 
         GOTO1 REMELEM                                                          
CMPX     J     XIT                                                              
                                                                                
CLRPROC  DS    0H                  CLEAR ELEMENT                                
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         JNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         J     XIT                                                              
         XC    2(0,R4),2(R4)                                                    
         SPACE 2                                                                
DELPROC  DS    0H                  DELETE ELEMENT                               
         MVC   ELCODE,0(R4)                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,0                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
                                                                                
ELTAB    DS    0F                  * INVOICE RECORD ELEMENTS                    
         DC    AL1(TAINELQ),AL3(INPROC)  INVOICE STATUS                         
         DC    AL1(TABDELQ),AL3(CLRPROC) BILLING DETAILS                        
         DC    AL1(TADDELQ),AL3(0)       DUE DATE                               
         DC    AL1(TANUELQ),AL3(0)       AUTH, ESTIMATE, ERROR INV.             
         DC    AL1(TAFNELQ),AL3(0)       COMML TITLE                            
         DC    AL1(TAULELQ),AL3(0)       UNION LOCALS                           
         DC    AL1(TALFELQ),AL3(0)       LIFT DETAILS                           
         DC    AL1(TACOELQ),AL3(0)       COMML DETAILS                          
         DC    AL1(TACSELQ),AL3(0)       COMML STUDIO                           
         DC    AL1(TACCELQ),AL3(0)       COMML CONTRACT                         
         DC    AL1(TAUPELQ),AL3(0)       UPGRADE DETAILS                        
         DC    AL1(TANDELQ),AL3(0)       NETWORK/CLA DETAILS                    
         DC    AL1(TANPELQ),AL3(0)       NETWORK/CLA PROGRAM DETAILS            
         DC    AL1(TAPAELQ),AL3(0)       PAY OPTIONS                            
         DC    AL1(TASDELQ),AL3(0)       SESSION DETAILS                        
******** DC    AL1(TAMKELQ),AL3(0)       WILDSPOT MARKETS                       
         DC    AL1(TASIELQ),AL3(0)       SUBSIDIARY INVOICES                    
         DC    AL1(TAVRELQ),AL3(0)       SUBSIDIARY INVOICES                    
         DC    AL1(TASYELQ),AL3(DELPROC) SYSTEM CONTROL (BAD EL.)               
         DC    AL1(TAMTELQ),AL3(0)       CNET/CSYS/MARKET ELEMENT               
         DC    AL1(TABDELQ2),AL3(0)      EMS BILLING DETAIL ELEMENT             
         DC    AL1(TATUELQ),AL3(0)       TAX UNIT                               
         DC    AL1(TAXTELQ),AL3(0)       EXTENDED TAXES                         
         DC    AL1(TAMDELQ),AL3(0)       MEDIA (INTERNET/NEW MEDIA)             
         DC    AL1(TARAELQ),AL3(0)       BILLING RATES                          
         DC    AL1(TAPGELQ),AL3(0)       POSTING DETAILS                        
         DC    AL1(TAUCELQ),AL3(DELPROC) US/CANADIAN INVOICE                    
*                                  * CHECK RECORD ELEMENTS                      
         DC    AL1(TABYELQ),AL3(CLRPROC) BILLING YTD                            
         DC    AL1(TACAELQ),AL3(CAPROC)  CAST DETAILS                           
         DC    AL1(TACDELQ),AL3(CLRPROC) CHECK DETAILS                          
         DC    AL1(TACWELQ),AL3(CLRPROC) CHECK WITHHOLDING                      
         DC    AL1(TACYELQ),AL3(CLRPROC) CHECK YTD                              
         DC    AL1(TADWELQ),AL3(DELPROC) DUE COMPANY WITHHOLDING                
         DC    AL1(TALWELQ),AL3(DELPROC) LIEN WITHHOLDING                       
         DC    AL1(TAOCELQ),AL3(0)       OLD INVOICE                            
         DC    AL1(TAODELQ),AL3(DELPROC) OTHER DEDUCTION                        
         DC    AL1(TAOKELQ),AL3(0)       OLD CHECK                              
         DC    AL1(TASDELQ),AL3(0)       SESSION DETAILS                        
         DC    AL1(TASOELQ),AL3(0)       SOAP EPISODE NUMBERS                   
         DC    AL1(TAYEELQ),AL3(0)       YEAR TO DATE ELEMENT                   
         DC    AL1(TACXELQ),AL3(0)       EXTRA CHECK DETAILS                    
         DC    AL1(TAFCELQ),AL3(0)       FIELD NUMBERS CHANGED                  
         DC    AL1(TACRELQ),AL3(0)       CAST APPLIED HISTORY                   
         DC    AL1(TATIELQ),AL3(0)       TAX ID                                 
         DC    AL1(TARNELQ),AL3(0)       RETURNED CHECK                         
         DC    AL1(TABKELQ),AL3(0)       BREAKOUT                               
         DC    AL1(TAATELQ),AL3(0)       CANADIAN TAX WITHHOLDING               
         DC    AL1(TAXCELQ),AL3(0)       EXTENDED COMMENTS                      
         DC    AL1(TAKPELQ),AL3(DELPROC) CHECK STOP                             
         DC    AL1(TAKLELQ),AL3(DELPROC) CHECK PULL                             
         DC    AL1(TAAIELQ),AL3(0)       ADVICE/INVOICE ASSIGN ELEMENT          
         DC    AL1(TACQELQ),AL3(0)       QTD WITHHOLDINGS                       
         DC    AL1(TAADELQ),AL3(0)       ADDRESS                                
         DC    AL1(TACNELQ),AL3(DELPROC) CHECK STOP/PULL CANCEL                 
*                                  * COMMON ELEMENTS                            
         DC    AL1(TAACELQ),AL3(DELPROC) ACTIVITY                               
         DC    AL1(TACMELQ),AL3(CMPROC)  COMMENTS                               
         DC    AL1(TAPDELQ),AL3(PDPROC)  PAY DETAILS                            
         DC    AL1(TAPSELQ),AL3(0)       PRE-PYMT/LIST                          
         DC    AL1(TAPPELQ),AL3(0)       PRE-PYMT/LIST                          
         DC    AL1(TARPELQ),AL3(RPPROC)  RETRO P&H AMOUNT                       
         DC    X'FF'                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD RETRO P&H AMOUNT ELEMENT                                           
*----------------------------------------------------------------------         
                                                                                
         USING TARPD,R4                                                         
ADDTARP  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
                                                                                
         MVI   TARPEL,TARPELQ                                                   
         MVI   TARPLEN,TARPLNQ                                                  
         GOTO1 ADDELEM                                                          
                                                                                
         J     XIT                                                              
*----------------------------------------------------------------------         
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD FREE FORM ELEM THAT RETRO IS FOR OLD INVOICE                       
*----------------------------------------------------------------------         
         USING TANUD,R4                                                         
ADDRET4  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
                                                                                
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANULEN,TANULNQ+L'TGINV                                          
         MVI   TANUTYPE,TANURT4I                                                
         MVC   TANUMBER(L'TRINVOLD),TRINVOLD                                    
         GOTO1 ADDELEM                                                          
                                                                                
         J     XIT                                                              
*----------------------------------------------------------------------         
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FORMAT ACCUMS TO PRINT LINE                           
*                                  P1=A(LITERAL) OR ZEROS                       
         DS    0D                                                               
         USING ACCD,R3             R3=A(ACCUMS)                                 
FORMAT   NTR1  BASE=*,LABEL=*                                                   
         L     RF,0(R1)            SAVE P1 IN RF                                
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         LTR   RF,RF               IF A(LITERAL PASSED)                         
         JZ    FMT10                                                            
         MVC   LINEST(14),0(RF)    MOVE TO PRINT LINE                           
         SPACE 1                                                                
         CLI   RCSUBPRG,0          IF HIGHER THAN DETAIL                        
         JE    FMT10                                                            
         MVC   LINCID(14),LINEST   SLIDE LITERAL OVER                           
         MVC   LINEST(14),SPACES                                                
         MVI   FORCEHED,C'Y'       AND SET TO START NEW PAGE                    
         SPACE 1                                                                
FMT10    L     RE,ACCAPPL          ALWAYS DISPLAY APPLIED AMT AS POS.           
         LPR   RE,RE                                                            
         EDIT  (RE),(12,LINAPPL),2,FLOAT=-         APPLIED AMOUNT               
         EDIT  (4,ACCPAYI),(12,LINPAYI),2,FLOAT=-  INDIV. PAYMENT               
         EDIT  (4,ACCPAYC),(12,LINPAYC),2,FLOAT=-  CORP. PAYMENT                
         EDIT  (4,ACCSPNH),(12,LINSPNH),2,FLOAT=-  SUBJ. TO PNH                 
         EDIT  (4,ACCPNH),(11,LINPNH),2,FLOAT=-    PNH                          
         SPACE 1                                                                
         XC    ACCD(ACCLNQ),ACCD   CLEAR THIS LEVEL ACCUMS                      
         J     XIT                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE HANDLES THE SESS OPTION                                  
*              RETURNS CC NOT EQ TO IGNORE THE INVOICE, ELSE CC EQ              
         SPACE                                                                  
         DS    0D                                                               
         USING SORTD,R6            R4=A(SORT RECORD)                            
SESSOPT  NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RC=A(WORKING STORAGE)                        
         USING GEND,RC                                                          
         SPACE                                                                  
         CLI   TGUSEQU,UBSS        ONLY WANT BSS, FGS, SFS, SSS AND BSR         
         JE    SESSO5                                                           
         CLI   TGUSEQU,UFGS        BECAUSE THEY HAVE SESSION XTRA DET.          
         JE    SESSO5                                                           
         CLI   TGUSEQU,USFS        AND THEY APPLY TO REUSE                      
         JE    SESSO5                                                           
         CLI   TGUSEQU,USSS                                                     
         JE    SESSO5                                                           
         CLI   TGUSEQU,UBSR                                                     
         JNE   SESSNO                                                           
         SPACE                                                                  
         USING TAPDD,R4            R4=A(COMM'L DETAILS ELEMENT)                 
SESSO5   L     R4,TRAINV                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TAPDOPT3,TAPDORET   IF THIS IS A RETRO INVOICE                   
         JZ    SESSO10                                                          
         MVC   AIO,TRAINV          SET AIO TO A(INVOICE RECORD)                 
         MVI   ELCODE,TACMELQ      GET HISTORY COMMENT                          
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPH))                                     
         JNE   SESSNO                                                           
         USING TACMD,R4                                                         
         L     R4,TGELEM                                                        
         CLC   TACMCOMM+30(4),=C'INV#' MAKE SURE IT'S THE RETRO COMMENT         
         JNE   SESSNO                                                           
         GOTO1 TINVCON,DMCB,TACMCOMM+34,DUB,DATCON CVT TO INTERNAL FMT          
         CLI   0(R1),X'FF'                                                      
         JE    SESSNO                                                           
         MVC   SORTINV,DUB         SAVE IN SORT RECORD                          
         MVI   SORTINV,0           CLEAR FIRST BYTE SO WILL SORT FIRST          
         MVC   AIO,AIO1            RESET AIO TO AIO1                            
         OI    TRSTAT,TRADDINV     SET INVOICE ADD PENDING TO ADD DUMMY         
         J     SESSNO              RETURN CC NO SO WON'T GET CHECKS             
         SPACE                                                                  
SESSO10  CLC   TIPADATE,=X'970518' NOT A RETRO INVOICE - SKIP IF PAID           
         JH    SESSNO              AFTER NEW RATES WENT LIVE                    
         XR    RC,RC               ELSE RETURN CC EQ                            
SESSNO   LTR   RC,RC                                                            
         J     XIT                                                              
         SPACE 2                                                                
         GETEL2 R4,DATADISP,ELCODE                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE FOR SESS OPTION TO SET OLD/NEW CONTRACT RATES            
         SPACE                                                                  
         DS    0D                                                               
         SPACE 1                                                                
SESSRATE NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RC=A(WORKING STORAGE)                        
         USING GEND,RC                                                          
         SPACE                                                                  
         L     R4,TRACHK                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         MVC   TRSVSPNH,TAPDGRS    SAVE ORIG PYMT AMT IN TRSVSPNH               
         SPACE                                                                  
         USING TASDD,R4                                                         
         MVI   TGYREQU,CN94        SET YEAR TO 94 TO RECALC OLD RATES           
         XC    TRSVOV1,TRSVOV1                                                  
         L     R4,TCATASD             R4=A(SESS XTRA DET EL)                    
         CLI   TGUSEQU,UBSR           IF BSR                                    
         JNE   SESSR5                                                           
         MVC   TRSVUNIT+1(1),TASDRTG  SAVE N'TAGS                               
         CLI   TASDRSP,1              IF N'SPOTS = 1                            
         JNE   SESSR8                                                           
         CLI   TASDRTG,0              AND N'TAGS <> 0                           
         JE    SESSR8                                                           
         MVC   TRSVOV1,=F'615'        SAVE THE INCREASE IN TAG RATE             
         MVC   TRSVMAJ,TASDRSP        SAVE N'SPOTS                              
         MVI   TASDRSP,0              CLEAR OUT SPOTS AND TAGS                  
         MVI   TASDRTG,0                                                        
         J     SESSR10                                                          
         SPACE                        TV SESSION                                
SESSR5   MVC   TRSVUNIT+1(1),TASDTAG  SAVE N'TAGS                               
         XR    R1,R1                                                            
         IC    R1,TASDSP                                                        
         CLC   TASDSP,TASDDAY      SET R1=HIGHER OF SPOTS OR DAYS               
         JNL   *+8                                                              
         IC    R1,TASDDAY                                                       
         CH    R1,=H'1'            IF SPOT/DAY = 1                              
         JNE   SESSR7                                                           
         CLI   TASDTAG,0           AND N'TAGS <> 0                              
         JE    SESSR7                                                           
         MVC   TRSVOV1,=F'1045'    SAVE THE INCREASE IN TAG RATE                
         CLC   TCCAONOF,=C'OFF'                                                 
         JNE   *+10                                                             
         MVC   TRSVOV1,=F'795'                                                  
         MVC   TRSVMAJ,TASDSP         SAVE N'SPOTS                              
         MVC   TRSVUNIT(1),TASDDAY    SAVE N'DAYS                               
         MVI   TASDSP,0               CLEAR OUT SPOTS AND DAYS AND TAGS         
         MVI   TASDDAY,0                                                        
         MVI   TASDTAG,0                                                        
         J     SESSR10                                                          
SESSR7   ZIC   R1,TASDDAY                                                       
         STC   R1,TRSVUNIT         SAVE ORIG DAYS IN TRSVUNIT                   
         LTR   R1,R1                                                            
         JZ    SESSR8                                                           
         SH    R1,=H'1'            SUBTRACT 1                                   
         STC   R1,TASDDAY          AND SAVE IN TASD ELEM                        
         SPACE                                                                  
SESSR8   ZIC   R1,TASDSP                                                        
         STC   R1,TRSVMAJ          SAVE ORIG SPOTS IN TRSVMAJ                   
         LTR   R1,R1                                                            
         JZ    SESSR10                                                          
         SH    R1,=H'1'                                                         
         STC   R1,TASDSP                                                        
SESSR10  GOTO1 TRACALC,DMCB,(RC),TCD,SYSCOMM  ** GET RATES **                   
         L     R1,TCGROSS                                                       
         ST    R1,TROLD            SAVE OLD RATE                                
         SPACE                                                                  
         MVI   TGYREQU,CN16        RESET YEAR TO 13 TO CALC NEW RATES           
         XC    TCPAY,TCPAY         CLEAR AMOUNTS                                
         XC    TCTOTS(TCTOTLNQ),TCTOTS                                          
         MVI   TCINPUT,0                                                        
         SPACE 1                                                                
         GOTO1 TRACALC,DMCB,(RC),TCD,SYSCOMM  ** GET NEW RATES **               
         L     R1,TRSVOV1          INCREASE IN TAG FEE                          
         L     R0,TCOV1            X OVERSCALE                                  
         BAS   RE,MULTR0             (RETURNED IN R1)                           
         A     R1,TRSVOV1          INCREASE IN TAG FEE                          
         ZIC   R0,TRSVUNIT+1       X N'TAGS                                     
         MR    R0,R0                                                            
         A     R1,TCGROSS          + PAYMENT AMOUNT UNDER NEW CONTRACT          
         ST    R1,TRNEW            = RATE UNDER NEW CONTRACT                    
         MVC   TASDSP,TRSVMAJ      RESTORE ORIG N'SPOTS                         
         CLI   TGUSEQU,UBSR        IF BSR                                       
         JNE   SESSR20                                                          
         MVC   TASDRTG,TRSVUNIT+1  RESTORE ORIG N'TAGS                          
         J     SESSX                                                            
SESSR20  MVC   TASDTAG,TRSVUNIT+1  RESTORE ORIG N'TAGS                          
         MVC   TASDDAY,TRSVUNIT    RESTORE ORIG N'DAYS                          
SESSX    J     XIT                                                              
         SPACE 3                                                                
MULTR0   MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         JM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE                                                                  
*        GETELN (R4),DATADISP,ELCODE,3                                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         DS    0D                                                               
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RC=A(WORKING STORAGE)                        
         USING GEND,RC                                                          
         L     R4,ATWA             R4=A(SCREEN)                                 
         USING T703FFD,R4                                                       
         SPACE 1                                                                
*        OI    TROPTS,TRPHONLY     PNHONLY 1ST RUN (2016)                       
         LA    R2,SCROPTH                                                       
         CLI   5(R2),0                                                          
         JE    VOPTX                                                            
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(10,(R3)),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         JZ    VOPTINV                                                          
         USING SCAND,R3            R3=A(SCAN BLOCK)                             
         SPACE 1                                                                
VOPT10   CLC   =C'TRACE',SCDATA1   TRACE                                        
         JNE   *+12                                                             
         OI    TROPTS,TRTRACE                                                   
         J     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'OKNEG',SCDATA1   OK TO REPORT ON NEGATIVE/ZERO INV.           
         JNE   *+12                                                             
         OI    TROPTS,TROKNEG                                                   
         J     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'WRIDSK',SCDATA1  WRITE TO DISK                                
         JNE   *+12                                                             
         OI    TROPTS,TRWRIDSK                                                  
         J     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'BOTH',SCDATA1     BOTH PNH AND PYMT INCREASE                  
         JNE   *+12                                                             
         OI    TROPTS,TRBOTH                                                    
         J     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'PNHONLY',SCDATA1  P&H ONLY                                    
         JNE   *+12                                                             
         OI    TROPTS,TRPHONLY                                                  
         J     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'SESS',SCDATA1     SESSIONS WITH EXTRA DETAILS ONLY            
         JNE   *+12                                                             
         OI    TROPTS2,TRSESS                                                   
         J     VOPT90                                                           
         SPACE 1                                                                
*DON'T   CLC   =C'CABLE',SCDATA1    CABLE ONLY                                  
*NEED    BNE   *+12                                                             
*ANYMORE OI    TROPTS2,TRCABLE                                                  
*****    B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'NORATE',SCDATA1  NO RATE ONLY                                 
         JNE   VOPT15                                                           
         OI    TROPTS2,TRNORATE                                                 
         OI    TROPTS,TRPHONLY     ALSO SET P&H ONLY FLAG                       
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT15   CLC   =C'CLA>1',SCDATA1   CLA USES > 1 ONLY                            
         JNE   VOPT17                                                           
         OI    TROPTS2,TRCLA1                                                   
         OI    TROPTS,TRPHONLY     ALSO SET P&H ONLY FLAG                       
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT17   CLC   =C'NOWSPU',SCDATA1  DON'T WANT TV WSP UPGRADES(FOR 1997)         
         JNE   VOPT18                                                           
         OI    TROPTS2,TRNOWSPU    BECAUSE ZERO AMOUNT                          
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT18   CLC   =C'EXTRAS',SCDATA1  ONLY WANT EXTRAS                             
         JNE   VOPT19                                                           
         OI    TROPTS2,TREXTRAS    BECAUSE DIDN'T PICK THEM UP BEFORE           
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT19   CLC   =C'NOXTRA',SCDATA1  DON'T WANT EXTRAS                            
         JNE   VOPT20                                                           
         OI    TROPTS2,TRNOXTRA                                                 
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT20   CLC   =C'NOMARK',SCDATA1  DON'T MARK RETROS DONE ON AGENCY REC         
         JNE   VOPT21                                                           
         OI    TROPTS,TRNOMARK                                                  
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT21   CLC   =C'AFT',SCDATA1     UNION AFT ONLY                               
         JNE   VOPT22                                                           
         MVC   TIFUN,=C'AFT'       SET UNION AFT                                
         MVI   TIFMED,TACOMEDR     AND MEDIA RADIO                              
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT22   CLC   =C'DOWN',SCDATA1    DOWNLOAD FORMAT (CSV)                        
         JNE   VOPT25                                                           
         OI    TROPTS2,TRDOWN                                                   
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT25   CLC   =C'FIX7 ',SCDATA1    FIX ERROR 7'S                               
         JNE   VOPT27                                                           
         OI    TROPTS2,TRFIX7                                                   
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT27   CLC   =C'FIX7B',SCDATA1    FIX FIX7'S BUGS                             
         JNE   VOPT30                                                           
         OI    TROPTS2,TRFIX7       DOES FIX7                                   
         OI    TROPTS3,TRFIX7B        AND SKIPS FIRST CHK                       
         J     VOPT90                                                           
         SPACE 1                                                                
VOPT30   CLC   =C'FORCE',SCDATA1   FORCE PROCESS ALL AGENCIES                   
         JNE   *+12                                                             
         OI    TROPTS,TRFORCE                                                   
         J     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'CURRENT',SCDATA1  ADD RETRO PMTS TO CURRENT AGY/CLI           
         JNE   *+12                                                             
         OI    TROPTS,TRCURRNT                                                  
         J     VOPT90                                                           
         SPACE 1                                                                
         J     VOPTINV                                                          
         SPACE 1                                                                
VOPT90   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT10                                                        
         SPACE 1                                                                
VOPTX    J     XIT                                                              
         SPACE 1                                                                
VOPTINV  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADDS RECORDS (OR WRITES OVER DELETED ONE)                
         SPACE                                                                  
         DS    0D                                                               
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RC=A(WORKING STORAGE)                        
         USING GEND,RC                                                          
         SPACE                                                                  
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLRCD,R4                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLDRD,R3                                                         
         MVC   TLDRKEY,TLRCKEY     SET ACTIVE KEY                               
         MVC   TLDRSTAT,TLRCSTAT   AND STATUS FROM RECORD                       
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
         SPACE 1                                                                
         CLC   TLDRKEY,KEYSAVE     TEST WE FOUND RECORD                         
         JNE   MYADR4                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         JO    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDRKEY,KEYSAVE     RESTORE ORIGINAL KEY                         
         MVC   TLDRSTAT,KEYSAVE+TLDRSTAT-TLDRD  AND STATUS                      
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
         MVC   AIO,=A(TMPIO)       NOW SET TO GET THE RECORD                    
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R4,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         J     MYADRX                                                           
         SPACE 1                                                                
MYADR4   MVC   KEY,KEYSAVE                                                      
         GOTO1 ADDREC              ADD FILE RECORD                              
         SPACE 1                                                                
MYADRX   NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*TMPIO**'                                                      
         DC    F'0'                                                             
TMPIO    DC    2000X'00'           TEMP RECORD AREA                             
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
         DS    0D                                                               
HOOK     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SAVERC           RC=A(WORKING STORAGE)                        
*                                                                               
         L     R4,ATWA             R4=A(SCREEN)                                 
         USING T703FFD,R4                                                       
         TM    TROPTS,TRCURRNT                                                  
         JO    *+14                                                             
         MVC   HEAD4+106(17),SCRPD  PERIOD                                      
         J     *+10                                                             
         MVC   HEAD4+110(17),SCRPD                                              
         SPACE 1                                                                
         CLI   RCSUBPRG,2          SKIP IF HIGHER THAN OFFICE TOTALS            
         JH    *+16                                                             
         MVC   HEAD3+9(1),TGOFF    TP OFFICE                                    
         MVC   HEAD3+17(36),TROFNAME                                            
         SPACE 1                                                                
         CLI   RCSUBPRG,1          SKIP IF HIGHER THAN AGENCY TOTALS            
         JH    HK10                                                             
         MVC   HEAD4+9(6),TGAGY    AGENCY                                       
         MVC   HEAD4+17(36),TRAYNAME                                            
         SPACE 1                                                                
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS COD AGENCY                        
         JZ    HK5                                                              
         LA    R1,HEAD4+52         SET R1=A(LAST POSSIBLE CHAR IN NAME)         
         BAS   RE,SHUFFLE          SLIDE BACK TO END OF NAME                    
         MVC   0(5,R1),=C'(PUR)'   DISPLAY COD INDICATOR IN HEADS               
         SPACE 1                                                                
HK5      TM    TROPTS,TRCURRNT     IF WRITING TO CURRENT AGY/CLI/PRD            
         JZ    HK10                                                             
         MVC   HEAD5+98(11),=C'ORIG AGENCY'                                     
         MVC   HEAD5+110(6),SCRAGY                                              
         MVC   HEAD5+117(15),SCRAGYN                                            
         SPACE 1                                                                
HK10     CLI   RCSUBPRG,0          SKIP IF HIGHER THAN DETAIL                   
         JH    *+16                                                             
         MVC   HEAD5+9(6),TGCLI    CLIENT                                       
         MVC   HEAD5+17(36),TRCLNAME                                            
         SPACE 1                                                                
         L     R4,ABOX             HANDLE BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING LINED,R2                                                         
         MVI   BL,C'L'                                                          
         CLI   RCSUBPRG,0          SKIP IF HIGHER THAN DETAIL                   
         JH    HK20                                                             
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
HK20     MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BC11,C'C'                                                        
         MVI   BC12,C'C'                                                        
         MVI   BC13,C'C'                                                        
         MVI   BR,C'R'                                                          
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXYORN,C'Y'                                                     
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE FINDS FIRST AVAILABLE SLOT PREVIOUS TO R1                
         SPACE 1                                                                
SHUFFLE  DS    0H                                                               
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,2(R1)                                                         
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
SAVERC   DC    A(0)                                                             
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TRD      DSECT                                                                  
TRACALC  DS    V                   A(RATE CALCULATION ROUTINE)                  
TRAINV   DS    A                   A(INVOICE RECORD)                            
TRACHK   DS    A                   A(CHECK RECORD)                              
TRSVAIO  DS    A                   A(CURRENT IOAREA)                            
*                                                                               
TROPTS   DS    XL1                 OPTIONS                                      
TRTRACE  EQU   X'80'               TRACE                                        
TROKNEG  EQU   X'40'               NEGATIVE PMTS OK                             
TRWRIDSK EQU   X'20'               WRITE TO DISK, NOT TO TAPES                  
TRPHONLY EQU   X'10'               CALCULATE P&H INCREASE ONLY                  
TRNOMARK EQU   X'08'               DON'T MARK RETROS DONE ON AGENCY REC         
TRFORCE  EQU   X'04'               FORCE PROCESS ALL AGENCIES                   
TRCURRNT EQU   X'02'               ADD RETRO PMTS TO CURRENT AGY/CLI            
TRBOTH   EQU   X'01'               PH INCREASE AND PYMT INCREASE                
*                                                                               
TROPTS2  DS    XL1                 2ND OPTIONS BYTE                             
TRNORATE EQU   X'80'               ONLY PROCESS USES WITH NO RATES              
TRSESS   EQU   X'40'               ONLY WANT SESS W/XTRA DETAILS                
TRCLA1   EQU   X'20'               ONLY DO CLA WITH STARTING USE > 1            
TRNOWSPU EQU   X'10'               DON'T WANT TV WSP UPGRADES                   
TREXTRAS EQU   X'08'               ONLY WANT EXTRAS                             
TRNOXTRA EQU   X'04'               DON'T WANT EXTRAS                            
TRDOWN   EQU   X'02'               DOWNLOAD FORMAT (CSV)                        
TRFIX7   EQU   X'01'               FIX ERROR 7'S                                
*                                                                               
TROPTS3  DS    XL1                 3RD OPTIONS BYTE                             
TRFIX7B  EQU   X'80'               FIX FIX7 BUG - OTHER CHECKS                  
*                                                                               
TRSTAT   DS    XL1                 STATUS                                       
TRSORTNG EQU   X'80'               SORT IS ACTIVE                               
TRADDINV EQU   X'40'               INVOICE ADD PENDING                          
TRPROCIN EQU   X'20'               PROCESSING RETRO INVOICE RECORD              
TRREREAD EQU   X'10'               NEED TO REREAD SYSIO'S KEY                   
TROVSCAM EQU   X'08'               CAST HAS OVERSCALE AMOUNT DEFINED            
TRCLAOV1 EQU   X'04'               CAST HAS CLA STARTING USE > 1                
ACT04AB  EQU   X'02'               04A /04B ACTRA COMML TYPE                    
TRMFIX   EQU   X'01'               MANUAL FIXED                                 
*                                                                               
TRSRTREC DS    CL(SORTLNQ)         SORT RECORD                                  
TRLSTREC DS    CL(SORTLNQ)         LAST SORT RECORD                             
*                                                                               
TRCAGY   DS    CL6                 CURRENT REQUESTED AGENCY                     
TRCCLI   DS    CL6                                   CLIENT                     
*                                                                               
TROFF    DS    CL1                 CURRENT OFFICE WHEN RUNNING TRCURRNT         
TRAGY    DS    CL6                         AGENCY                               
TRCLI    DS    CL6                         CLIENT                               
TRPRD    DS    CL6                         PRODUCT                              
*                                                                               
TRACCS   DS    0F                                                               
TRINACCS DS    CL(ACCLNQ)          INVOICE LEVEL ACCUMS                         
TRCLACCS DS    CL(ACCLNQ)          CLIENT                                       
TRAYACCS DS    CL(ACCLNQ)          AGENCY                                       
TROFACCS DS    CL(ACCLNQ)          OFFICE                                       
TRRQACCS DS    CL(ACCLNQ)          REQUEST                                      
TRNLVLS  EQU   (*-TRACCS)/ACCLNQ                                                
*                                                                               
TRW4TYP  DS    C                   W4 TYPE                                      
*                                                                               
TRINVDA  DS    XL4                 D/A OF INVOICE RECORD                        
TRINEST  DS    CL15                ESTIMATE NUMBER                              
TRINVERR DS    XL1                 INVOICE ERROR CODE                           
TRINVOLD DS    XL6                 OLD INVOICE NUMBER                           
TRINVOLC DS    CL6                 OLD INVOICE NUMBER (EBCDIC)                  
TRNUSES  DS    H                   TOTAL N'USES PAID PREVIOUSLY                 
TRNUSESL DS    H                   N'USES PAID PREVIOUSLY TO LIFT               
*                                                                               
TROFNAME DS    CL36                OFFICE NAME                                  
TRAYNAME DS    CL36                AGENCY NAME                                  
TRCLNAME DS    CL36                CLIENT NAME                                  
*                                                                               
TRAYSTAT DS    XL1                 AGENCY STATUS BYTE                           
TRSVAGY  DS    CL6                 SAVED MASTER AGY FOR INVOICE NUMBERS         
TRINV    DS    XL6                 NEXT INVOICE NUMBER                          
*                                                                               
TRTACOEL DS    CL(TACOLNQ)         COMMERCIAL DETAILS ELEMENT                   
TRCOMEXP DS    XL3                 COMMERCIAL EXPIRATION DATE                   
TRCOMML  DS    CL(L'TACOCID)       COMMERCIAL EXPIRATION DATE                   
TRCSTEXP DS    XL3                 CAST EXPIRATION DATE                         
TRONOF   DS    CL3                 CAMERA STATUS FOR PRINTING                   
TRGRS    DS    F                   TEMPORARY FIELD FOR OLD GROSS AMT            
TRSPNH   DS    F                   TEMPORARY FIELD FOR OLD SUBJ TO P&H          
TRBSSFEE DS    F                   OLD SESSION FEE AMOUNT                       
TRBAL    DS    F                   TACRBAL DURING FTRACK HANDLING               
TRCRCYC  DS    XL6                 TACR CYCLE DATES FOR TRACKING REC.           
*                                                                               
TRNEW    DS    F                   RATE UNDER NEW CONTRACT                      
TROLD    DS    F                   RATE UNDER OLD CONTRACT                      
TRPDREXP DS    F                   REIMB EXP                                    
*                                  USED FOR BOTH OPTION ONLY                    
TRSVSPNH DS    F                   SUBJ PNH AMOUNT                              
TRSVPNH  DS    F                   PNH RATE INCREASE                            
*                                                                               
TRSVOV1  DS    F                   SAVED DATA DURING SPECIAL GETRATE            
TRSVUSES DS    H                                                                
TRSVUNIT DS    H                                                                
TRSVMAJ  DS    XL1                                                              
*                                                                               
TRICODE  DS    C                   INCLUDE CODE                                 
TRRETTAB DS    CL(4*50)            TABLE OF RETRO INVOCES FOR COMM'L            
TRRETTX  DS    X                   SET TO X'FF' TO MARK END                     
TRSVKEY  DS    CL(L'KEY)                                                        
TRSVMINV DS    XL6                                                              
TRNUMCK  DS    XL2                 CHECK NUMBER FOR SKIPPING FIRST ONE          
                                                                                
TRNAME   DS    CL32                                                             
TRNITAB  DS    XL70                TABLE FOR 10 INVOICES + 10 BYTES             
TRNITBX  DS    X                   EOT                                          
TRNIPTR  DS    A                   POINTER FOR TABLE                            
*                                                                               
TRCSTTAB DS    CL(2*300)           TABLE OF CAST MEMBERS PAID REUSE             
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
         EJECT                                                                  
       ++INCLUDE TASYSCALCD                                                     
         SPACE 2                                                                
TRDLNQ   EQU   *-TRD               *** END OF TRD ***                           
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTKEY  EQU   *                   BEGINNING OF KEY                             
SORTOFF  DS    CL1                 TP OFFICE                                    
SORTAGY  DS    CL6                 AGENCY                                       
SORTCLI  DS    CL6                 CLIENT                                       
SORTPLNQ EQU   *-SORTD             PAGE BREAK AT KEY CHANGE                     
*                                                                               
SORTCOM  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
SORTCLNQ EQU   *-SORTD                                                          
SORTINV  DS    XL6                 ORIGINAL INVOICE NUMBER                      
SORTILNQ EQU   *-SORTD                                                          
SORTCSEQ DS    XL1                 SEQUENCE                                     
SORTCZER EQU   0                   TACRBAL EQ ZERO                              
***TCZER EQU   0                   APPREUS INVOICE AND TACRBAL EQ ZERO          
SORTCSOK EQU   1                   NORMAL                                       
SORTCAST DS    XL6                 CAST SORT SEQ (ZEROS FOR INV)                
SORTKLNQ EQU   *-SORTD             KEY LENGTH                                   
*                                                                               
SORTDA   DS    XL4                 DISK ADDRESS                                 
SORTPRD  DS    CL6                 PRODUCT                                      
SORTUSE  DS    CL3                 USE CODE                                     
SORTUTYP DS    XL1                 USE TYPE                                     
SORTERR  DS    XL1                 ERROR NUMBER                                 
ERR1     EQU   1                   CREDIT INVOICES                              
ERR2     EQU   2                   NO RATES DEFINED                             
ERR3     EQU   3                   4 WK CABLE INVOICES                          
ERR4     EQU   4                   52 WK CABLE INVOICES                         
ERR5     EQU   5                   UNAPPROVED INVOICES                          
ERR6     EQU   6                   ZERO PAYMENT                                 
ERR7     EQU   7                   NEGATIVE PAYMENT                             
ERR8     EQU   8                   EXCEEDED MAXIMUM UPGRADE                     
ERR9     EQU   9                   REUSE PAID AFTER END OF PERIOD               
ERR10    EQU   10                  CYCLE START BEFORE PERIOD                    
ERR11    EQU   11                  MANUAL OVERRIDE, NO CHK CMT                  
ERR12    EQU   12                  BSR/GRR WITH CAST OVERRIDE AMOUNT            
SORTEST  DS    CL15                ESTIMATE NUMBER                              
SORTMINV DS    XL6                 MANUALLY FIXED INVOICE                       
*                                                                               
SORTCHK  DS    0F                  * CHECK RECORD FIELDS                        
SORTACCS DS    CL(ACCLNQ)          ACCUMS                                       
SORTAPPL DS    F                   AMOUNT TO BE ADDED TO FTRACK                 
SORTBAL  DS    F                   TACR BALANCE WHEN SORTAMCA SET               
SORTCYC  DS    XL6                 TACR CYCLE DATES WHEN SORTAMCA SET           
SORTNCDE DS    XL2                 AGENT NUMBER                                 
SORTACDE DS    CL1                 APPLIED CODE                                 
SORTSTAT DS    XL1                 STATUS                                       
SORTXCST EQU   X'80'               CAST RECORD NOT FOUND                        
SORTAMCA EQU   X'40'               FIXED CYC AMOUNT DEFINED ON CAST REC         
SORTTSHT EQU   X'20'               HAS MAINFRAME TIMESHEETS                     
SORTVITA EQU   X'10'               CAME FROM VITA                               
SORTMFIX EQU   X'08'               MANUALLY FIXED                               
SORTFIX7 EQU   X'04'               FIX ERROR 7, ONLY P&H RATE INC (1.3)         
SORTFIXN DS    CL1                 FIX NUMBER                                   
SORTCHKL EQU   *-SORTCHK                                                        
         ORG   SORTCHK             * INVOICE RECORD FIELDS                      
         ORG                                                                    
SORTLNQ  EQU   *-SORTD                                                          
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*              DSECT TO COVER ACCUMULATORS                                      
         SPACE 1                                                                
ACCD     DSECT                                                                  
ACCAPPL  DS    F                   APPLIED AMOUNT                               
ACCPAY   DS    0CL8                                                             
ACCPAYI  DS    F                   INDIV. PAYMENT                               
ACCPAYC  DS    F                   CORP. PAYMENT                                
ACCSPNH  DS    F                   SUBJ. TO P&H                                 
ACCOSPNH DS    F                   ORIG SUBJ. TO P&H                            
ACCPNH   DS    F                   P&H                                          
ACCPNHDF DS    F                   P&H DIFFERENCE                               
ACCLNQ   EQU   *-ACCD                                                           
NACCS    EQU   ACCLNQ/4                                                         
         SPACE 3                                                                
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
LINED    DSECT                                                                  
BL       DS    CL1                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
BC1      DS    CL1                                                              
LINOINV  DS    CL6                 OLD INVOICE                                  
BC2      DS    CL1                                                              
LINNINV  DS    CL6                 NEW INVOICE                                  
BC3      DS    CL1                                                              
LINUSE   DS    CL3                 USE CODE                                     
LINUTYCD DS    CL5                 USE TYPE                                     
BC4      DS    CL1                                                              
LINEST   DS    CL15                ESTIMATE NUMBER                              
BC5      DS    CL1                                                              
LINSSN   DS    CL9                 SOCIAL SECURITY NUMBER                       
BC6      DS    CL1                                                              
LINCAT   DS    CL3                 CATEGORY                                     
BC7      DS    CL1                                                              
LINONOF  DS    CL3                 ON/OFF CAMERA                                
BC8      DS    CL1                                                              
LINACDE  DS    CL1                 APPLIED CODE                                 
BC9      DS    0CL1                                                             
LINAPPL  DS    CL12                APPLIED AMOUNT                               
BC10     DS    0CL1                                                             
LINPAYI  DS    CL12                INDIV PAYMENT                                
BC11     DS    0CL1                                                             
LINPAYC  DS    CL12                CORP PAYMENT                                 
BC12     DS    0CL1                                                             
LINSPNH  DS    CL12                SUBJ TO P&H                                  
BC13     DS    0CL1                                                             
LINPNH   DS    CL11                P&H                                          
BR       DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD5D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068TAREP35   06/15/16'                                      
         END                                                                    
