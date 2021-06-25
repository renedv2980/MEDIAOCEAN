*          DATA SET TAREP35OS  AT LEVEL 064 AS OF 05/01/02                      
*PHASE T70335A,*                                                                
*                                                                               
*******  HANDLES SOAPS OPTION - DO NOT DELETE                                   
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
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'08',(R2)),SCROFFNH                        
         MVC   TIFOFF,TGOFF                                                     
         SPACE 1                                                                
VK20     LA    R2,SCRAGYH          AGENCY                                       
         XC    SCRAGYN,SCRAGYN                                                  
         OI    SCRAGYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',(R2)),SCRAGYNH                
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
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCRCLINH                        
         MVC   TIFCLI,TGCLI                                                     
         SPACE 1                                                                
VK35     LA    R2,SCRCIDH          COMMERCIAL ID                                
         XC    SCRCIDN,SCRCIDN                                                  
         OI    SCRCIDNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SCRCIDNH                       
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TIFCOM,TLCOCOM      SET INTERNAL COMML NUMBER FILTER             
         SPACE 1                                                                
VK40     LA    R2,SCREMPH          EMPLOYER                                     
         XC    SCREMPN,SCREMPN                                                  
         OI    SCREMPNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SCREMPNH                        
         MVC   TIFEMP,TGEMP                                                     
         SPACE 1                                                                
VK50     LA    R2,SCRCURH          CURRENCY                                     
         MVI   TIFCUR,C'U'         DEFAULT TO TAKE US$ PAYMENTS ONLY            
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
VK70     BAS   RE,VALOPT           VALIDATE OPTIONS                             
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
         BNE   *+16                                                             
         TM    TROPTS,TRCURRNT     REQUIRED IF OPTION SELECTED                  
         BO    FLDMISS                                                          
         B     *+12                                                             
         TM    TROPTS,TRCURRNT     ELSE INVALID                                 
         BZ    FLDINV                                                           
         OC    TGLST,TGLST         IF NOT PROCESSING FLIST                      
         BNZ   VK80                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SCRCAYNH                        
         MVC   TRCAGY,TGAGY                                                     
         SPACE 1                                                                
VK80     LA    R2,SCRCCLH          CURRENT CLIENT                               
         XC    SCRCCLN,SCRCCLN                                                  
         OI    SCRCCLNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK90                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCRCCLNH                        
         MVC   TRCCLI,TGCLI                                                     
         SPACE 1                                                                
VK90     LA    R2,SCRCCOH          CURRENT COMMERCIAL ID                        
         XC    SCRCCON,SCRCCON                                                  
         OI    SCRCCONH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SCRCCONH                       
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TIFCOM,TLCOCOM      SET INTERNAL COMML NUMBER FILTER             
         SPACE 1                                                                
VK100    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         LA    R2,SCROPTH                                                       
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(10,(R3)),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    FLDINV                                                           
         USING SCAND,R3            R3=A(SCAN BLOCK)                             
         SPACE 1                                                                
VOPT10   CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   *+12                                                             
         OI    TROPTS,TRTRACE                                                   
         B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'SOAPS',SCDATA1   PROCESS SOAP RETROS ONLY                     
         BNE   *+12                                                             
         OI    TROPTS,TRSOAPS                                                   
         B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'OKNEG',SCDATA1   OK TO REPORT ON NEGATIVE/ZERO INV.           
         BNE   *+12                                                             
         OI    TROPTS,TROKNEG                                                   
         B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'WRIDSK',SCDATA1  WRITE TO DISK                                
         BNE   *+12                                                             
         OI    TROPTS,TRWRIDSK                                                  
         B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'PNHONLY',SCDATA1  P&H ONLY                                    
         BNE   *+12                                                             
         OI    TROPTS,TRPHONLY                                                  
         B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'NOMARK',SCDATA1  DON'T MARK RETROS DONE ON AGENCY REC         
         BNE   *+12                                                             
         OI    TROPTS,TRNOMARK                                                  
         B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'FORCE',SCDATA1   FORCE PROCESS ALL AGENCIES                   
         BNE   *+12                                                             
         OI    TROPTS,TRFORCE                                                   
         B     VOPT90                                                           
         SPACE 1                                                                
         CLC   =C'CURRENT',SCDATA1  ADD RETRO PMTS TO CURRENT AGY/CLI           
         BNE   FLDINV                                                           
         OI    TROPTS,TRCURRNT                                                  
         SPACE 1                                                                
VOPT90   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT10                                                        
         SPACE 1                                                                
VOPTX    B     XIT                                                              
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
         SPACE 1                                                                
         CLOSE ERRFILE             CLOSE ERROR FILE                             
         SPACE 1                                                                
         TM    TROPTS,TRWRIDSK     IF NOT WRITING TO DISK                       
         BO    PRX                                                              
         CLOSE TALTAPE             CLOSE TAPES                                  
         CLOSE CHKTAPE                                                          
         SPACE 1                                                                
PRX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES FOR REPORT                               
         SPACE 1                                                                
INIT     NTR1                                                                   
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
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
         MVI   TIFINSTN,TAINSCIN+TAINSCAN  IGNORE CANCELLED INVOICES            
*                                                                               
         TM    TROPTS,TRSOAPS      PROCESS SOAP RETROS ONLY                     
         BZ    INIT10                                                           
******** MVC   TIFUN,=C'AFT'       ** USE AFT UNION FOR NOW**                   
         MVC   TIFUSE,=C'SOP'      ONLY INTERESTED IN SOAP                      
         MVI   TIFCTYPE,CTYSOAP                                                 
         MVI   TIQDTYPE,TIQDBILL   SET FILTERING ON BILL DATES                  
         B     INIT20                                                           
*                                                                               
INIT10   MVI   TIQDTYPE,TIQDPAY    SET FILTERING ON PAY DATE                    
         MVC   TIFYEAR,=C'91 '     SET FILTERING ON UNION YEAR 91               
         MVC   TIFUN,=C'RET'       SET RETRO UNION FLIST RECORD                 
         NI    TIFUN,X'7F'         TURN OFF X'80' BIT TO INDICATE FLIST         
         MVI   TIFCTYPE,CTYSOAP                                                 
         NI    TIFCTYPE,X'BF'      SET ALL COMML TYPES EXCEPT SOAPS             
         SPACE 1                                                                
INIT20   OC    TIFAGY,TIFAGY       IF RUNNING FOR ALL AGENCIES                  
         BNZ   *+14                                                             
         MVC   TIFAGY,=C'RETXAY'   THEN SET EXCLUDE AGENCY FLIST RECORD         
         NI    TIFAGY,X'3F'        TURN OFF X'80'/FLIST & X'40'/EXCLUDE         
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     IF RUNNING P&H ONLY OPTION                   
         BZ    *+14                                                             
         MVC   TIFUN,=C'AFT'       SET UNION AFT                                
         MVI   TIFMED,TACOMEDR     AND MEDIA RADIO                              
         SPACE 1                                                                
         MVC   HEADHOOK,=A(HOOK)   SET A(HEADLINE HOOK)                         
         MVC   SPECS,=A(MYSPECS)                                                
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A8D'  LOAD RATE CALC. MODULE                
         MVC   TRACALC,0(R1)                                                    
         SPACE 1                                                                
         MVC   MYTRACE,=A(VMYTRACE)  SET A(TRACE ROUTINE)                       
         MVC   MYTRACE2,=A(VMYTRAC2) SET A(TRACE ROUTINE)                       
         XC    TGAGY,TGAGY         PRE-CLEAR GLOBAL AGENCY                      
         XC    TGCOM,TGCOM                   GLOBAL INT. COMML NUMBER           
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO                                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCINV      PROCESS INVOICE                              
         BNE   IOH60                                                            
         L     R4,TIAREC           R4=A(RECORD)                                 
         USING TLIND,R4                                                         
         CLI   TLINCD,TLINCDQ      INSURE WE HAVE INVOICE RECORD                
         BNE   IOH20                                                            
         SPACE 1                                                                
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
         SPACE 1                                                                
         BAS   RE,FILTINV          MAY BE ABLE TO FILTER OUT THIS INV.          
         BNE   IOHX                IN ERROR ALREADY, SO DON'T CALC              
         SPACE 1                                                                
         BAS   RE,CALCINV          PREPARE FOR RATE CALCULATION                 
         B     IOHX                                                             
         SPACE 1                                                                
IOH60    CLI   TIMODE,PROCREC      PROCESS CHECK                                
         BNE   IOHX                                                             
         SPACE 1                                                                
         MVC   TRACHK,TIAREC       SET A(CHECK RECORD)                          
         BAS   RE,CALCCHK          CALCULATE CHECK                              
         BNE   IOHX                                                             
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     IF NOT RUNNING P&H ONLY OPTION               
         BO    *+12                                                             
         TM    TGUSSTA2,NORATES    AND NO RATES DEFINED IN TABLE                
         BO    IOH70               DON'T BOTHER ADDING CHECK LEVEL              
         SPACE 1                                                                
         BAS   RE,ADDCHK           ADD CHECK LEVEL SORT RECORD                  
         SPACE 1                                                                
IOH70    OI    TRSTAT,TRADDINV     SET INVOICE ADD PENDING                      
         SPACE 1                                                                
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
         TM    TROPTS,TRFORCE      IF OPTION TO FORCE ALL AGYS NOT REQ          
         BO    *+12                                                             
         TM    TAAYSTA2,TAAYSRET   IF ALREADY PROCESSED RETROS                  
         BO    NO                  THEN SET TO SKIP TO NEXT AGENCY              
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
         OC    TRCAGY,TRCAGY       IF CURRENT AGENCY REQUESTED                  
         BZ    *+14                                                             
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
*              ROUTINE TO FILTER INVOICE                                        
         SPACE 1                                                                
FILTINV  NTR1                                                                   
         GOTO1 USEVAL,DMCB,TIUSE,TIUTYPE  SET GLOBAL USE VALUES                 
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     IF NOT RUNNING P&H ONLY OPTION               
         BO    *+12                                                             
         CLI   TGUSEQU,UGRT        IGNORE GUARANTEES                            
         BE    FINVIGN                                                          
         SPACE 1                                                                
         MVC   SORTUSE,TGUSCDE     SET USE CODE IN SORT RECORD                  
         MVC   SORTUTYP,TGUSTYP        USE TYPE                                 
         MVI   SORTERR,0           CLEAR ERROR BYTE                             
         SPACE 1                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4            R4=A(INVOICE DETAILS ELEMENT)                
         TM    TAINSTAT,TAINSAPR   INVOICE MUST BE APPROVED                     
         BO    *+12                                                             
         MVI   SORTERR,ERR5        ELSE SET ERROR STATUS                        
         B     FINVERR                                                          
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     SKIP IF RUNNING P&H ONLY OPTION              
         BO    FINVX                                                            
         SPACE 1                                                                
         L     R4,TRAINV                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         MVC   TGCOM,TAPDCOM       INTERNAL COMML NUMBER FOR CAST READ          
         SPACE 1                                                                
         TM    TAPDPST1,TAPDPCRD   IF CREDIT PAYMENT                            
         BZ    FINV20                                                           
         MVI   SORTERR,ERR1        SET ERROR                                    
         B     FINVERR                                                          
         SPACE 1                                                                
FINV20   TM    TROPTS,TRSOAPS      IF SOAP RETROS                               
         BZ    *+12                                                             
         CLI   TGUSEQU,USOP        ONLY WANT SOP PAYMENTS                       
         BNE   FINVERR                                                          
*                                                                               
         CLI   TGUSEQU,UCAB        IF CABLE PAYMENT                             
         BNE   FINVX                                                            
         CLI   TGUSTYP,UCAB4W      AND 4 WEEK TYPE                              
         BNE   FINV30                                                           
         MVI   SORTERR,ERR3        SET ERROR                                    
         B     FINVERR                                                          
         SPACE 1                                                                
FINV30   CLI   TGUSTYP,UCAB52W     OR 52 WEEK TYPE                              
         BNE   FINVX                                                            
         MVI   SORTERR,ERR4        SET ERROR                                    
         B     FINVERR                                                          
         SPACE 1                                                                
FINVX    B     YES                                                              
         SPACE 1                                                                
FINVERR  DS    0H                  INVOICE IN ERROR, ALTHOUGH MAY NOT           
         B     NO                  HAVE ANY ELIGIBLE CHECKS                     
         SPACE 1                                                                
FINVIGN  MVI   TIMODE,PROCNOCK     IGNORE THIS INV - DON'T PASS CHECKS          
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO SET UP FOR RATE CALCULATION AT INVOICE LEVEL          
         SPACE 1                                                                
CALCINV  NTR1                                                                   
         XC    TCUDETS(TCUDTLNQ),TCUDETS  INITIALIZE RATE CALC. FIELDS          
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     GET OUT IF RUNNING P&H ONLY OPTION           
         BO    CINVX                                                            
         SPACE 1                                                                
         L     R4,TRAINV                                                        
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
         CLC   TAPDUSE,=C'CAB'     IF USE TYPE WAS CAB                          
         BNE   CINV20                                                           
         MVC   TAPDUSE,=C'CBL'     IT'S NOW CBL                                 
         MVI   TAPDTYPE,0                                                       
         MVC   TAPDUNIT,=H'160'    WITH MAXIMUM UNITS                           
         SPACE 1                                                                
CINV20   GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         SPACE 1                                                                
         MVC   TCUNITS,TAPDUNIT                                                 
         MVC   TCMAJORS,TAPDMAJ                                                 
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
         BNE   SUPGX                                                            
         MVC   TAUHUNT,TAUPIUNT    SET INITIAL UNITS                            
         MVC   TAUHMAJ,TAUPIMAJ            AND MAJORS                           
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
         MVI   TCUSETAB+3,C'Y'     SET USE TABLE DEFAULT TO LIFT                
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
         XC    SORTCHK(SORTCHKL),SORTCHK  INITIALIZE CHECK-RELATED FLDS         
         MVI   SORTCSEQ,SORTCSOK                                                
         SPACE 1                                                                
         CLI   SORTERR,0           IF ERROR STATUS ALREADY SET                  
         BNE   CCHKX               DON'T BOTHER CALCULATING CHECK               
         SPACE 1                                                                
         L     R4,TRACHK                                                        
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4            R4=A(CHECK DETAILS ELEMENT)                  
         TM    TACDSTAT,TACDSLIN                                                
         BO    NO                  DON'T WANT LIEN PAYEE CHECKS                 
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
         TM    TROPTS,TRPHONLY     IF RUNNING P&H ONLY OPTION                   
         BZ    CCHK20                                                           
         ICM   R1,15,TAPDSPNH      SET R1=SUBJ. TO P&H                          
         BZ    NO                  (DON'T BOTHER IF NOTHING SUBJ P&H)           
         MVC   TCPNHR,=H'100'      SET RATE INCREASE                            
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         BAS   RE,PNHCALC          CALCULATE P&H                                
         B     CCHKX               THAT'S IT                                    
         SPACE 1                                                                
CCHK20   OC    TAPDAPPL,TAPDAPPL   IF THERE WERE APPLIED CRS ORIGINALLY         
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
         BNE   NO                                                               
         SPACE 1                                                                
         BAS   RE,GETCAST          GET CAST RECORD                              
         BNE   NO                                                               
         SPACE 1                                                                
         BAS   RE,GETRATE          GET NEW AND OLD RATES                        
         BNE   NO                                                               
         SPACE 1                                                                
         L     R1,TRNEW            CALC NEW RATE LESS OLD RATE                  
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
CCHK40   MVI   SORTERR,ERR7        PAYMENT IS NEGATIVE                          
         B     *+8                                                              
CCHK50   MVI   SORTERR,ERR6        PAYMENT IS ZERO                              
         TM    TROPTS,TROKNEG      IF OPTION TO REPORT NEG/ZERO INV'S           
         BZ    CCHKX               NOT REQUESTED THEN GET OUT NOW               
         MVI   SORTERR,0           ELSE CLEAR ERROR AND CONTINUE                
         B     CCHK70              MAX PERCENT TEST NOT APPROPRIATE             
         SPACE 1                                                                
CCHK60   CLI   TGUSEQU,UCBL        IF NOT CABLE PAYMENT                         
         BE    CCHK70                                                           
         TM    TROPTS,TRSOAPS      AND NOT SOAP RETROS                          
         BO    CCHK70                                                           
         L     RE,TROLD            TEST WHETHER RETRO PAYMENT IS MORE           
         MH    RE,=H'16'           THAN 16 PERCENT INCREASE OVER                
         LR    R0,R1               ORIGINAL PAYMENT                             
         MH    R0,=H'100'                                                       
         CR    R0,RE                                                            
         BNH   CCHK70                                                           
         MVI   SORTERR,ERR8                                                     
         B     CCHKX                                                            
         SPACE 1                                                                
CCHK70   BAS   RE,SETACCS          SET ACCUMS IN SORT REC (PASS R1=PMT)         
         SPACE 1                                                                
CCHKX    B     YES                                                              
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
         TM    TGCATYPE,EXTRA      EXTRAS AREN'T ELIGIBLE FOR RETROS            
         BO    NO                                                               
         TM    TROPTS,TRSOAPS      IF SOAP RETROS                               
         BZ    SETC20                                                           
         CLI   TGCAEQU,CTP         PROCESS PART-TIME                            
         BE    SETC20                                                           
         CLI   TGCAEQU,CTANN       PROCESS ANNOUNCERS                           
         BE    SETC20                                                           
         CLI   TGCAEQU,CTU5        AND UNDER FIVE LINES ONLY                    
         BNE   NO                                                               
         SPACE 1                                                                
SETC20   L     R4,TRACHK                                                        
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         SPACE 1                                                                
         CLI   TGUSEQU,UCBL        IF THIS IS CABLE                             
         BNE   STC40                                                            
         CLC   =C'ON',TACAONOF     AND THIS IS ON-CAMERA PERF.                  
         BNE   STC30                                                            
         CLC   TRGRS,=F'20600'     AND ORIGINAL PAYMENT LESS THAN SCALE         
         BL    NO                  IGNORE THIS CHECK                            
         B     STC40                                                            
STC30    CLC   TRGRS,=F'11500'     OFF-CAMERA CHECKS DIFFERENT RATE             
         BL    NO                                                               
         SPACE 1                                                                
STC40    MVC   TCCAONOF,TACAONOF        ON/OFF CAMERA                           
         MVC   TCCADBL,TACADBL          N'DOUBLES                               
         MVC   TCOV2,TACAOV2            2ND OVERSCALE RATE                      
         MVC   TCCASTAT,TACASTAT        STATUS BYTE                             
         MVC   TCCASTA2,TACASTA2        2ND STATUS BYTE                         
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
         BZ    NO                  THEN IGNORE                                  
         SPACE 1                                                                
STC60    CLI   TGUSEQU,UCLA        IF USE TYPE IS CLA                           
         BNE   *+12                                                             
         BAS   RE,SETCCLA          SET CLA USE DETAILS                          
         BNE   NO                                                               
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
SCCLAX   OC    TCNUSES,TCNUSES     AS ONLY FIRST USE RATE INCREASED             
         BNZ   NO                  THEN IF NOT PAYING IT THEN IGNORE            
         B     YES                                                              
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
GCST20   TM    TROPTS,TRSOAPS      IF NOT SOAPS                                 
         BO    GCST40                                                           
         GOTO1 GETOV1,DMCB,TGUSCDE,FULL  LOOK UP OVERSCALE ON CAST REC          
         CLI   0(R1),X'FF'         IF AMOUNT OVERRIDE DEFINED ON CAST           
         BE    NO                  THEN NOT ELIGIBLE                            
         SPACE 1                                                                
GCST40   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         MVC   SORTNCDE,TACANCDE   SET CURRENT AGENT NUMBER IN SORT REC         
         SPACE 1                                                                
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         BZ    GCSTX                                                            
         BAS   RE,GETTACR          GET RELEVANT APPLIED CREDIT EL.              
         BNE   GCSTX                                                            
         L     R4,TGELEM           RETURNS A(ELEMENT) IN TGELEM                 
         USING TACRD,R4                                                         
         OC    TACRBAL,TACRBAL     IF THERE'S NO REMAINING BALANCE THEN         
         BNZ   GCSTX                                                            
         MVI   SORTCSEQ,SORTCZER   SET SORT SEQUENCE INDICATING SO              
         SPACE 1                                                                
GCSTX    B     YES                                                              
         EJECT                                                                  
*              ROUTINE FINDS RELEVANT TACREL IN CAST RECORD                     
         SPACE 1                                                                
GETTACR  NTR1                                                                   
         XC    TGELEM,TGELEM       RETURNS A(ELEMENT) IN TGELEM                 
         XC    TGDUB,TGDUB                                                      
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ      SET ELEMENT CODE                             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GTCR10   BAS   RE,NEXTEL                                                        
         BNE   GTCRX                                                            
         SPACE 1                                                                
         USING TACRD,R4                                                         
         CLC   TCPCYCS,TACRSTRT    CYCLE START MUST FALL BETWEEN START          
         BL    GTCR10                                                           
         CLC   TCPCYCS,TACREND     AND END                                      
         BH    GTCR10                                                           
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         BZ    *+14                                                             
         CLC   TCPCYC,TACRSTRT     DATES MUST MATCH EXACTLY                     
         BNE   GTCR10                                                           
         SPACE 1                                                                
         OC    TACRINV,TACRINV     IF THERE'S AN INVOICE NUMBER                 
         BZ    *+14                                                             
         CLC   TACRINV,TGDUB       TEST THIS INVOICE LATER THAN SAVED           
         BNH   GTCR10                                                           
         SPACE 1                                                                
         ST    R4,TGELEM           SAVE A(LAST ELEMENT)                         
         MVC   TGDUB(6),TACRINV    AND LAST INVOICE NUMBER                      
         SPACE 1                                                                
         B     GTCR10              KEEP LOOKING UNTIL NO MORE ELS.              
         SPACE                                                                  
GTCRX    OC    TGELEM,TGELEM       RETURN CC                                    
         BNZ   YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE SETS OLD AND NEW CONTRACT RATES                          
         SPACE 1                                                                
GETRATE  NTR1                                                                   
         XC    TCPAY,TCPAY         CLEAR AMOUNTS                                
         XC    TCTOTS(TCTOTLNQ),TCTOTS                                          
         MVI   TCINPUT,0                                                        
         SPACE 1                                                                
         TM    TROPTS,TRSOAPS      IF SOAP RETROS                               
         BZ    GETRE15                                                          
         GOTO1 =A(SOPCALC),DMCB,(RC) CALCULATE NEW RATE                         
         BNE   NO                                                               
         B     GETRE30                                                          
         SPACE 1                                                                
GETRE15  GOTO1 TRACALC,DMCB,(RC),TCD,SYSCOMM  ** GET NEW RATES **               
         SPACE 1                                                                
GETRE30  L     R4,TRACHK                                                        
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
         EJECT                                                                  
*              ROUTINE TO SET ACCUMS IN SORT RECORD                             
         SPACE 1                                                                
*                                  R1=RETRO PAYMENT AMOUNT                      
SETACCS  NTR1                                                                   
         LA    R3,SORTACCS                                                      
         USING ACCD,R3             R3=A(SORT RECORD ACCUMS)                     
         SPACE 1                                                                
         L     RF,TROLD                                                         
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
**NO-OP**TM    TROPTS,TRSOAPS      IF SOAPS                                     
**NO-OP**BO    *+8                 DON'T BOTHER WITH PNH                        
         BAS   RE,PNHCALC          CALCULATE P&H (PASS R1=SUBJ TO P&H)          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES SETTING APPLIED CREDITS                          
         SPACE 1                                                                
*                                  R1=RETRO PAYMENT AMOUNT                      
         USING ACCD,R3             R3=A(SORT RECORD ACCUMS)                     
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
APPLY    NTR1                                                                   
         LR    R2,R1               SAVE RETRO PAYMENT                           
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
         BAS   RE,GETTACR          LOOK UP TACR ELEMENT ON CAST RECORD          
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
         BNE   ACHKX               DON'T BOTHER WRITING SORT REC.               
         SPACE 1                                                                
         LA    R4,TIKEY            BUILD CHECK-RELATED SORT FIELDS              
         USING TLCKD,R4                                                         
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
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     IF NOT RUNNING P&H ONLY OPTION               
         BO    *+16                                                             
         TM    TGUSSTA2,NORATES    AND NO RATES DEFINED IN TABLE                
         BZ    *+8                                                              
         MVI   SORTERR,ERR2        SET ERROR                                    
         SPACE 1                                                                
         BAS   RE,PUTSORT          WRITE OUT INVOICE LEVEL SORT RECORD          
         SPACE 1                                                                
         NI    TRSTAT,ALL-TRADDINV                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS REPORT                                            
         SPACE 1                                                                
REPORT   NTR1                                                                   
         TM    TRSTAT,TRSORTNG     TEST SORT ACTIVE                             
         BZ    REPX                                                             
         XC    TRSRTREC,TRSRTREC   PRE-CLEAR CURRENT SORT RECORD                
         XC    TGAGY,TGAGY                   GLOBAL AGENCY                      
         SPACE 1                                                                
REP10    BAS   RE,GETSORT          GET A SORT RECORD                            
         BNE   REP90                                                            
         SPACE 1                                                                
REP20    CLC   SORTKEY(SORTILNQ),TRLSTREC  IF INVOICE CHANGED                   
         BE    REP60                                                            
         TM    TRSTAT,TRADDINV     AND INVOICE ADD PENDING                      
         BZ    *+8                                                              
         BAS   RE,NEWINV           ADD NEW INVOICE RECORD                       
         SPACE 1                                                                
         OC    SORTINV,SORTINV     UNLESS THIS IS DUMMY RECORD                  
         BZ    *+14                                                             
         OC    SORTCAST,SORTCAST   IF THIS IS CHECK LEVEL                       
         BNZ   REP70               THEN DIDN'T GET INV - SKIP                   
         SPACE 1                                                                
         CLC   SORTOFF,TRLSTREC+SORTOFF-SORTD  IF OFFICE CHANGED                
         BE    *+8                                                              
         BAS   RE,NEWOFF                       HANDLE NEW OFFICE                
         SPACE 1                                                                
         CLC   SORTAGY,TRLSTREC+SORTAGY-SORTD  IF AGENCY CHANGED                
         BE    *+12                                                             
         BAS   RE,NEWAGY                       HANDLE NEW AGENCY                
         MVI   TRLSTREC+SORTCLI-SORTD,X'FF'    SET CLIENT CHANGED               
         SPACE 1                                                                
         CLC   SORTCLI,TRLSTREC+SORTCLI-SORTD  IF CLIENT CHANGED                
         BE    *+8                                                              
         BAS   RE,NEWCLI                       HANDLE NEW CLIENT                
         SPACE 1                                                                
         CLC   SORTKEY(SORTPLNQ),TRLSTREC  IF HIGH LEVEL KEY CHANGED            
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'               SET TO START NEW PAGE                
         SPACE 1                                                                
         CLC   SORTKEY(SORTCLNQ),TRLSTREC  IF COMMERCIAL CHANGED                
         BE    *+12                                                             
         BAS   RE,NEWCOM                   HANDLE NEW COMMERCIAL                
         BNE   REP20                       RESTART CONTROL BREAK PROC           
         SPACE 1                                                                
REP60    OC    SORTCAST,SORTCAST   IF NO CAST SET                               
         BNZ   *+12                                                             
         BAS   RE,INITINV          HANDLE INVOICE RECORD INITIALIZATION         
         B     *+8                                                              
         BAS   RE,NEWCHK           ELSE ADD NEW CHECK RECORD                    
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
         GOTO1 MYTRACE,DMCB,(RC),=C'OFF'                                        
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
         GOTO1 MYTRACE,DMCB,(RC),=C'AGY'                                        
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
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MYTRACE,DMCB,(RC),=C'CLI'                                        
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
         GOTO1 MYTRACE,DMCB,(RC),=C'UPD AGY'                                    
         SPACE 1                                                                
         OC    TRSVAGY,TRSVAGY     IF WE HAVE SAVED MASTER AGENCY               
         BZ    WAGYX                                                            
         MVC   TRAGY,TGAGY         SAVE ACTUAL AGENCY                           
         MVC   TGAGY,TRSVAGY       MOVE SAVED MASTER AGY TO GLOBAL              
         BAS   RE,UPDAGY           UPDATE AGENCY RECORD                         
         GOTO1 MYTRACE,DMCB,(RC),=C'UPD MASTER AGY'                             
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
         SPACE 1                                                                
NEWCOM   NTR1                                                                   
         MVC   TGCOM,SORTCOM                                                    
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MYTRACE,DMCB,(RC),=C'COMML'                                      
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMML DETAILS ELEMENT)                  
         MVC   TRCOMEXP,TACOEXP    SAVE EXPIRATION DATE                         
         SPACE 1                                                                
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINCID,TACOCID      MOVE CID TO PRINT LINE 1ST TIME ONLY         
         SPACE 1                                                                
         LH    RF,=AL2(L'TRCSTTAB) CLEAR TABLE OF CAST WITH REUSE IN PD         
         XCEFL TRCSTTAB                                                         
         LA    R2,TRCSTTAB         NOW REBUILD IT                               
         SPACE 1                                                                
NCOM20   OC    SORTINV,SORTINV     IF THERE'S NO INVOICE NUMBER                 
         BNZ   YES                                                              
         MVC   0(2,R2),SORTCAST+4  SAVE INTERNAL CAST SEQ. NUMBER               
         LA    R2,2(R2)                                                         
         SPACE 1                                                                
         BAS   RE,GETSORT          GET NEXT SORT RECORD                         
         SPACE 1                                                                
         CLC   SORTKEY(SORTCLNQ),TRLSTREC  AS LONG AS COMML DIDN'T CHG          
         BE    NCOM20                      CONTINUE LOOPING                     
         B     NO                          ELSE RETURN CC NE - RESTART          
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
         GOTO1 MYTRACE,DMCB,(RC),=C'MASTER AGY - GETINV'                        
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
GINV20   CLI   TGTODAY1,X'90'           IF TODAY'S YEAR < 90                    
         BNL   *+12                                                             
         CLI   TAAYNINV,X'20'           AND IF NOT 21ST CENTURY                 
         BNE   GIN50                                                            
         CLC   TAAYNINV+1(1),TGTODAY1   OR IF YEAR                              
         BNE   GIN50                                                            
         CLC   TAAYNINV+2(1),TGTODAY1+1 OR MONTH DOESN'T MATCH                  
         BE    GIN60                                                            
GIN50    MVC   TAAYNINV+3(2),TAAYRINV   USE RESET INVOICE NUMBER                
         MVC   TAAYNINV+1(1),TGTODAY1       TODAY'S YEAR                        
         MVC   TAAYNINV+2(1),TGTODAY1+1             MONTH                       
         MVI   TAAYNINV,X'19'           SET 20TH CENTURY                        
         CLI   TGTODAY1,X'90'           IF TODAY'S YEAR < 90                    
         BNL   *+8                                                              
         MVI   TAAYNINV,X'20'           SET 21ST CENTURY                        
         SPACE 1                                                                
GIN60    MVI   TAAYNINV+5,0        ALWAYS ADD NEW INVOICE TYPE                  
         SPACE 1                                                                
         MVC   TRINV,TAAYNINV      SET THIS INVOICE NUMBER                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE RETRO INVOICE RECORDS                      
         SPACE 1                                                                
INITINV  NTR1                                                                   
         NI    TRSTAT,ALL-TRADDINV                                              
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,SORTUSE,SORTUTYP  SET GLOBAL USE VALUES              
         SPACE 1                                                                
         MVC   TRINVERR,SORTERR    SAVE INVOICE ERROR STATUS                    
         MVC   TRINVOLD,SORTINV         OLD INVOICE NUMBER                      
         GOTO1 TINVCON,DMCB,TRINVOLD,TRINVOLC,DATCON  SAVE EBCDIC VERS.         
         SPACE 1                                                                
         CLI   TRINVERR,0          IF INVOICE IN ERROR STATUS                   
         BNE   IINV80              SKIP TO PRINT                                
         SPACE 1                                                                
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    IINV40                                                           
         MVC   TROFF,SORTOFF       SET SAVED VALUES - OFFICE                    
         MVC   TRAGY,SORTAGY                          AGENCY                    
         MVC   TRCLI,SORTCLI                          CLIENT                    
         MVC   TRPRD,SORTPRD                          PRODUCT                   
         SPACE 1                                                                
IINV40   MVC   KEY+TLDRDA-TLDRD(4),SORTDA SET D/A OF INVOICE RECORD             
         MVC   AIO,=A(INVIO)                                                    
         GOTO1 GETREC              READ ORIGINAL INVOICE RECORD                 
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R4,=A(INVIO)                                                     
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
IINV80   LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINOINV,TRINVOLC    OLD INVOICE                                  
         MVC   LINUSE,TGUSCDE      USE CODE                                     
         MVC   LINUTYCD,TGUSTYCD   USE TYPE CODE                                
         MVC   LINEST,SORTEST      ESTIMATE NUMBER                              
         SPACE 1                                                                
         CLI   TRINVERR,0          IF THERE'S NO ERROR SET                      
         BNE   IINV90                                                           
         GOTO1 TINVCON,DMCB,TRINV,LINNINV,DATCON   NEW INVOICE                  
         B     IINVX                                                            
         SPACE 1                                                                
IINV90   BAS   RE,ERRPROC          ELSE PROCESS ERROR                           
         SPACE 1                                                                
IINVX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESS ERROR INVOICES                                   
         SPACE 1                                                                
ERRPROC  NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINNINV(4),=C'ERR ' DISPLAY ERROR                                
         LA    RE,LINNINV+4                                                     
         EDIT  (1,TRINVERR),(2,(RE)),ALIGN=LEFT                                 
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT           AND PRINT THE LINE NOW                       
         SPACE 1                                                                
*                                  * SET TO WRITE ERROR RECORD                  
         MVC   WORK(6),TGAGY       RECORD IS AGENCY                             
         MVC   WORK+6(6),TRINVOLC  AND OLD INVOICE NUMBER                       
         PUT   ERRFILE,WORK        WRITE TO ERROR FILE                          
         SPACE 1                                                                
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
         CLI   TRINVERR,0          IF ERROR STATUS SET                          
         BNE   NCHKX               DON'T BOTHER - GET OUT                       
         SPACE 1                                                                
         TM    TGUSSTA2,APPREUSE   IF USE TYPE APPLIES TOWARDS REUSE            
         BZ    NCHK20                                                           
         CLI   SORTCSEQ,SORTCZER   AND TACRBAL WAS ZERO                         
         BNE   NCHK20                                                           
         LA    R2,TRCSTTAB         DETERMINE WHETHER THIS CAST MEMBER           
*                                  WAS PAID REUSE IN RETRO PERIOD               
NCHK10   OC    0(2,R2),0(R2)       TEST REACHED END OF TABLE                    
         BNZ   *+16                                                             
         MVI   TRINVERR,ERR9       SET ERROR STATUS NOW                         
         BAS   RE,ERRPROC          PROCESS ERROR                                
         B     NCHKX               AND GET OUT                                  
         SPACE 1                                                                
         CLC   0(2,R2),SORTCAST+4  MATCH AGAINST INTERNAL CAST SEQ.             
         BE    NCHK20              OK TO CONTINUE IF SO                         
         LA    R2,2(R2)            TRY NEXT TABLE ENTRY                         
         B     NCHK10                                                           
         SPACE 1                                                                
NCHK20   MVC   AIO,=A(CHKIO)       SET I/O AREA TO A(CHECK RECORD)              
         SPACE 1                                                                
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
         SPACE 1                                                                
         GOTO1 EXTRACT             EXTRACT GLOBAL VALUES FROM RECORD            
         SPACE 1                                                                
         BAS   RE,ELPROC           PROCESS ELEMENTS                             
         SPACE 1                                                                
         GOTO1 MYTRACE,DMCB,(RC),=C'RETRO CHK'                                  
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         BAS   RE,ADDIT            ADD THE RECORD                               
         MVC   SYSFIL,=CL8'TALFIL'                                              
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
         SPACE 1                                                                
NCHK70   LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LINSSN,TGSSN        SOCIAL SECURITY NUMBER                       
         MVC   LINCAT,TGCAT        CATEGORY                                     
         MVC   LINONOF,TRONOF      ON/OFF CAMERA                                
         MVC   LINACDE,SORTACDE    APPLIED CODE                                 
         TM    SORTSTAT,SORTXCST   IF CAST RECORD WASN'T FOUND                  
         BZ    *+14                                                             
         MVI   LINACDE,C'*'        INDICATE WITH ASTERISK                       
         AP    CASTCNT,=P'1'       AND ADD TO COUNTER                           
         SPACE 1                                                                
         LA    R3,SORTACCS         SET R3=A(SORT RECORD ACCUMS)                 
         XR    RF,RF                                                            
         BAS   RE,FORMAT           FORMAT ACCUMS TO PRINT LINE                  
         BAS   RE,PRNTIT           PRINT THE LINE                               
         SPACE 1                                                                
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS A COD AGENCY                      
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
         L     R4,AIO                                                           
         USING TLIND,R4                                                         
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    *+10                                                             
         MVC   TLINAGY,TRAGY       SET NEW AGENCY IN KEY                        
         MVC   TLININV,TRINV       SET NEW INVOICE NUMBER IN KEY                
         XC    TLININV,=6X'FF'     (COMPLEMENTED)                               
         SPACE 1                                                                
         BAS   RE,ELPROC           PROCESS ELEMENTS                             
         SPACE 1                                                                
         BAS   RE,ADDCMNT          ADD SPECIAL COMMENTS TO INVOICE              
         SPACE 1                                                                
         GOTO1 MYTRACE,DMCB,(RC),=C'RETRO INV'                                  
         SPACE 1                                                                
         BAS   RE,ADDIT            ADD THE RECORD                               
         MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
         SPACE 1                                                                
         BAS   RE,INVTOTS          FORMAT/PRINT LINE                            
         SPACE 1                                                                
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,TRINV+3(2)      CALC NEXT - CVT TO PACKED WITH SIGN          
         AP    DUB,=P'1'                                                        
         MVO   WORK(3),DUB+5(3)                                                 
         MVC   TRINV+3(2),WORK                                                  
         SPACE 1                                                                
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS A COD AGENCY                      
         BZ    *+14                                                             
         AP    CODCNT,=P'1'        ADD TO COD COUNT                             
         B     *+10                                                             
         AP    REGCNT,=P'1'        ELSE ADD TO REGULAR COUNT                    
         AP    INVCNT,=P'1'        ADD TO TOTAL INVOICES                        
         SPACE 1                                                                
         NI    TRSTAT,ALL-TRPROCIN TURN OFF PROCESSING RETRO INV.               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES ELS IN RETROACTIVE INVOICE/CHECK RECS.         
         SPACE 1                                                                
ELPROC   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMENTS             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
ELP10    L     R2,=A(ELTAB)        R2=A(ELEMENT PROCESSING TABLE)               
         SPACE 1                                                                
ELP20    CLC   0(1,R4),0(R2)       MATCH ELCODE AGAINST 1ST BYTE IN TAB         
         BE    ELP30                                                            
         LA    R2,L'ELTAB(R2)      TRY NEXT TABLE ENTRY                         
         CLI   0(R2),X'FF'                                                      
         BNE   ELP20                                                            
         DC    H'0'                UNDEFINED ELEMENT                            
         SPACE 1                                                                
ELP30    XR    RF,RF                                                            
         ICM   RF,7,1(R2)          A(PROCESSING ROUTINE)                        
         BZ    ELP40               NONE DEFINED, SO SKIP                        
         LA    RE,ELP40                                                         
         NTR1  ,                   COMMON NTR1                                  
         BR    RF                                                               
         SPACE 1                                                                
ELP40    BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    ELP10                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ELEMENT PROCESSING ROUTINES                                      
         SPACE 1                                                                
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
CAPROC   DS    0H                                                               
         MVC   TACANCDE,SORTNCDE   SET CURRENT AGENT NUMBER                     
         MVC   TRCSTEXP,TACAEXP    SAVE CAST EXPIRY FOR FTRACKS                 
         MVC   TRONOF,TACAONOF     SAVE CAMERA STATUS FOR PRINTING              
         B     XIT                                                              
         SPACE 2                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
PDPROC   DS    0H                                                               
         MVC   TAPDINV,TRINV       SET NEW INVOICE NUMBER                       
         OI    TAPDOPT3,TAPDORET   SET RETROACTIVE PAYMENT STATUS               
         NI    TAPDSTAT,TAPDSCAN+TAPDSLFT   TURN OFF ALL BUT THESE              
         XC    TAPDAMTS(TAPDAMTL),TAPDAMTS  PRE-CLEAR ACCUMS                    
         MVI   TAPDICDE,0          CLEAR INCLUDE CODE                           
         MVC   TGCOM,TAPDCOM       INTERNAL COMML NUMBER FOR CAST READ          
         MVC   TCPCYC,TAPDCYCS     CYLE DATES FOR TACREL LOOKUP                 
         SPACE 1                                                                
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         MVC   TAPDACDE,SORTACDE   SET APPLY CODE FROM SORT RECORD              
         SPACE 1                                                                
         TM    TRSTAT,TRPROCIN     IF HANDLING INVOICE RECORD                   
         BZ    *+12                                                             
         LA    R3,TRINACCS         R3=A(INVOICE LEVEL ACCUMS)                   
         MVI   TAPDACDE,APPLOTH    APPLY CODE IS ALWAYS OTHER                   
         SPACE 1                                                                
         TM    TROPTS,TRPHONLY     IF RUNNING P&H ONLY OPTION                   
         BZ    *+8                                                              
         MVI   TAPDACDE,0          THERE IS NO APPLY CODE                       
         SPACE 1                                                                
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
         SPACE 1                                                                
         TM    TROPTS,TRCURRNT     IF WE NEED CURRENT COMML VALUES              
         BZ    PDPX                                                             
         MVC   TAPDOFF,TROFF       SET SAVED VALUES - OFFICE                    
         MVC   TAPDCLI,TRCLI                          CLIENT                    
         MVC   TAPDPRD,TRPRD                          PRODUCT                   
PDPX     B     XIT                                                              
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
         NI    TAINSTAT,ALL-TAINSBIL-TAINSCHK-TAINSERR-TAINSHLD                 
         NI    TAINSTA2,ALL-TAINSHLR-TAINSHLP  TURN OFF COD REL/PRINT           
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS A COD AGENCY                      
         BZ    *+12                                                             
         OI    TAINSTAT,TAINSHLD   SET COD HOLD STATUS                          
         B     *+8                                                              
         OI    TAINSTA2,TAINSRTH   ELSE SET RETRO HOLD STATUS                   
         MVI   TAINTERR,0          CLEAR ERROR                                  
         MVI   TAINFPG,0           CLEAR SCREEN REC PAGES                       
         MVI   TAINLPG,0                                                        
         MVI   TAINCKID,0          CLEAR CHECK RUN ID                           
         XC    TAINCKRN,TAINCKRN   CLEAR CHECK RUN DATE                         
         XC    TAINHDTE,TAINHDTE   CLEAR COD PRINT DATE                         
         B     XIT                                                              
         SPACE 2                                                                
CMPROC   DS    0H                                                               
         TM    TRSTAT,TRPROCIN     IF HANDLING INVOICE RECORD                   
         BZ    CMPX                                                             
         MVI   ELCODE,TACMELQ      DELETE EXISTING COMMENT ELS.                 
         GOTO1 REMELEM                                                          
CMPX     B     XIT                                                              
         SPACE 2                                                                
CLRPROC  DS    0H                  CLEAR ELEMENT                                
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         XC    2(0,R4),2(R4)                                                    
         SPACE 2                                                                
DELPROC  DS    0H                  DELETE ELEMENT                               
         MVC   ELCODE,0(R4)                                                     
         GOTO1 REMELEM                                                          
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
         SPACE 1                                                                
         MVC   TACMCOMM(L'RETROCMT),RETROCMT  START COMMENT                     
         SPACE 1                                                                
         LA    R2,TACMCOMM+L'RETROCMT                                           
         LA    R3,TACMLNQ+L'RETROCMT+L'INVHCMNT+6                               
         TM    TROPTS,TRPHONLY                    IF RUNNING P&H ONLY           
         BZ    *+14                                                             
         MVC   TACMCOMM+L'RETROCMT-4(3),=C'P&&H'  CHANGE COMMENT                
         BCTR  R2,0                                                             
         BCTR  R3,0                                                             
         SPACE 1                                                                
         STC   R3,TACMLEN                     SET L'ELEMENT                     
         MVC   0(L'INVHCMNT,R2),INVHCMNT      MOVE IN REMAINING COMMENT         
         MVC   L'INVHCMNT(6,R2),TRINVOLC      ADD ORIGINAL INV NUMBER           
         SPACE 1                                                                
         GOTO1 ADDELEM                        ADD IT TO THE RECORD              
         SPACE 1                                                                
         XIT1  REGS=(R2)           RETURN R2=A(REMAINING COMMENT)               
         EJECT                                                                  
*              ROUTINE HANDLES CAST/FTRACK RECORD MAINTENANCE                   
         SPACE 1                                                                
FTRACK   NTR1                                                                   
         TM    TROPTS,TRPHONLY     GET OUT IF RUNNING P&H ONLY OPTION           
         BO    FTX                                                              
         TM    SORTSTAT,SORTXCST   IF CAST RECORD WASN'T FOUND GET OUT          
         BO    FTX                                                              
         LA    R3,SORTACCS         R3=A(SORT RECORD ACCUMS)                     
         USING ACCD,R3                                                          
         SPACE 1                                                                
         TM    SORTSTAT,SORTAMCA   IF FIXED CYC PMT DEFINED ON CAST             
         BZ    FT3                                                              
         MVC   TCAPPLCR,ACCAPPL    SET APPLIED AMOUNT FROM SORT REC             
         MVC   TRBAL,SORTBAL       AND BALANCE AS WELL                          
         MVC   TRCRCYC,SORTCYC     SET TACR CYCLE FROM SORT REC                 
         B     FT40                SKIP CAST UPDATE (DONE ALREADY)              
         SPACE 1                                                                
FT3      GOTO1 RECVAL,DMCB,TLCACCDQ,(X'20',0)  GET CAST RECORD                  
         BNE   FTX                                                              
         SPACE 1                                                                
FT5      BAS   RE,GETTACR          GET RELEVANT APPLIED CREDIT EL.              
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
         CLI   TGUSEQU,UWSP        IF THIS IS A WILDSPOT PAYMENT                
         BNE   WSPNO                                                            
         TM    TGMEEQU,RADIO       FOR RADIO                                    
         BZ    WSPNO                                                            
         CLI   TGCAT,C'G'          AND CATEGORY BEGINS WITH 'G'                 
         BNE   WSPNO                                                            
         OC    SORTAPPL,SORTAPPL   THEN IF THERE WAS EXCESS CREDITS             
         BZ    WSPNO                                                            
         CR    RE,RE               RETURN CC EQ                                 
         BR    RE                                                               
         SPACE 1                                                                
WSPNO    LTR   RE,RE               NO - RETURN CC NE                            
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE WRITES CAST RECORDS TO FILE                              
         SPACE 1                                                                
WRICAST  NTR1                                                                   
         GOTO1 PUTREC              WRITE BACK CAST RECORD                       
         SPACE 1                                                                
         GOTO1 MYTRACE,DMCB,(RC),=C'UPD CAST'                                   
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
         GOTO1 MYTRACE,DMCB,(RC),=C'FTRK'                                       
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
         CLI   TLRCCD,TLFTCDQ      IF ADDING FTRACK RECORD                      
         BE    *+12                                                             
         TM    TROPTS,TRWRIDSK     OR IF WRITING ALL RECORDS TO DISK            
         BZ    ADDI20                                                           
         GOTO1 ADDREC              DO ADDREC                                    
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
         LA    RF,=CL14'INVOICE TOTALS'                                         
         BAS   RE,ALLTOTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
CLITOTS  NTR1                                                                   
         LA    R3,TRCLACCS                                                      
         LA    RF,=CL14'CLIENT TOTALS'                                          
         BAS   RE,ALLTOTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
AGYTOTS  NTR1                                                                   
         LA    R3,TRAYACCS                                                      
         LA    RF,=CL14'AGENCY TOTALS'                                          
         MVI   RCSUBPRG,1                                                       
         BAS   RE,ALLTOTS                                                       
         BNE   *+8                                                              
         BAS   RE,SUMMARY                                                       
         MVI   RCSUBPRG,0                                                       
         B     XIT                                                              
         SPACE 1                                                                
OFFTOTS  NTR1                                                                   
         LA    R3,TROFACCS                                                      
         LA    RF,=CL14'OFFICE TOTALS'                                          
         MVI   RCSUBPRG,2                                                       
         BAS   RE,ALLTOTS                                                       
         BNE   *+8                                                              
         BAS   RE,SUMMARY                                                       
         MVI   RCSUBPRG,0                                                       
         B     XIT                                                              
         SPACE 1                                                                
REQTOTS  NTR1                                                                   
         LA    R3,TRRQACCS                                                      
         LA    RF,=CL14'REPORT TOTALS'                                          
         MVI   RCSUBPRG,3                                                       
         BAS   RE,ALLTOTS                                                       
         BAS   RE,SUMMARY                                                       
         B     XIT                                                              
         SPACE 3                                                                
ALLTOTS  NTR1                                                                   
         OC    0(ACCLNQ,R3),0(R3)                                               
         BZ    NO                                                               
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT ACCUMS TO PRINT LINE                           
         SPACE 1                                                                
*                                  RF=A(LITERAL) OR ZEROS                       
         USING ACCD,R3             R3=A(ACCUMS)                                 
FORMAT   NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         LTR   RF,RF               IF A(LITERAL PASSED)                         
         BZ    FMT10                                                            
         MVC   LINEST(14),0(RF)    MOVE TO PRINT LINE                           
         SPACE 1                                                                
         CLI   RCSUBPRG,0          IF HIGHER THAN DETAIL                        
         BE    FMT10                                                            
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
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE PRINTS A LINE                                            
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS SUMMARY TOTALS                                    
         SPACE 1                                                                
SUMMARY  NTR1                                                                   
         BAS   RE,BXBOT            FORCE IN BOTTOM OF BOX                       
         SPACE 1                                                                
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
         BE    XIT                                                              
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
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    TRSTAT,TRSORTNG                                                  
         SPACE 1                                                                
PUTS10   GOTO1 SORTER,DMCB,=C'PUT',TRSRTREC                                     
         SPACE 1                                                                
         GOTO1 MYTRACE2,DMCB,(RC),=C'PUTSRT',TRSRTREC,SORTLNQ                   
         B     XIT                                                              
         SPACE 3                                                                
GETSORT  NTR1                                                                   
         MVC   TRLSTREC,TRSRTREC   SAVE CURRENT SORT RECORD                     
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)                                                      
         BZ    NO                                                               
         MVC   TRSRTREC,0(RF)      MOVE TO LOCAL W/S                            
         SPACE 1                                                                
         GOTO1 MYTRACE2,DMCB,(RC),=C'GETSRT',TRSRTREC,SORTLNQ                   
         B     YES                                                              
         SPACE 3                                                                
*              ROUTINE FINDS FIRST AVAILABLE SLOT PREVIOUS TO R1                
         SPACE 1                                                                
SHUFFLE  DS    0H                                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,2(R1)                                                         
         BR    RE                                                               
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R4,ATWA             R4=A(SCREEN)                                 
         USING T703FFD,R4                                                       
         TM    TROPTS,TRCURRNT                                                  
         BO    *+14                                                             
         MVC   HEAD4+106(17),SCRPD  PERIOD                                      
         B     *+10                                                             
         MVC   HEAD4+110(17),SCRPD                                              
         SPACE 1                                                                
         CLI   RCSUBPRG,2          SKIP IF HIGHER THAN OFFICE TOTALS            
         BH    *+16                                                             
         MVC   HEAD3+9(1),TGOFF    TP OFFICE                                    
         MVC   HEAD3+17(36),TROFNAME                                            
         SPACE 1                                                                
         CLI   RCSUBPRG,1          SKIP IF HIGHER THAN AGENCY TOTALS            
         BH    HK10                                                             
         MVC   HEAD4+9(6),TGAGY    AGENCY                                       
         MVC   HEAD4+17(36),TRAYNAME                                            
         SPACE 1                                                                
         TM    TRAYSTAT,TAAYSCOD   IF THIS IS COD AGENCY                        
         BZ    HK5                                                              
         LA    R1,HEAD4+52         SET R1=A(LAST POSSIBLE CHAR IN NAME)         
         BAS   RE,SHUFFLE          SLIDE BACK TO END OF NAME                    
         MVC   0(5,R1),=C'(COD)'   DISPLAY COD INDICATOR IN HEADS               
         SPACE 1                                                                
HK5      TM    TROPTS,TRCURRNT     IF WRITING TO CURRENT AGY/CLI/PRD            
         BZ    HK10                                                             
         MVC   HEAD5+98(11),=C'ORIG AGENCY'                                     
         MVC   HEAD5+110(6),SCRAGY                                              
         MVC   HEAD5+117(15),SCRAGYN                                            
         SPACE 1                                                                
HK10     CLI   RCSUBPRG,0          SKIP IF HIGHER THAN DETAIL                   
         BH    *+16                                                             
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
         BH    HK20                                                             
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
         SPACE 2                                                                
         EJECT                                                                  
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
         DC    AL1(TASOELQ),AL3(0)       SOAP DETAILS                           
         DC    AL1(TAUPELQ),AL3(0)       UPGRADE DETAILS                        
         DC    AL1(TANDELQ),AL3(0)       NETWORK/CLA DETAILS                    
         DC    AL1(TANPELQ),AL3(0)       NETWORK/CLA PROGRAM DETAILS            
         DC    AL1(TASYELQ),AL3(DELPROC) SYSTEM CONTROL (BAD EL.)               
*                                  * CHECK RECORD ELEMENTS                      
         DC    AL1(TABYELQ),AL3(CLRPROC) BILLING YTD                            
         DC    AL1(TACAELQ),AL3(CAPROC)  CAST DETAILS                           
         DC    AL1(TACDELQ),AL3(CLRPROC) CHECK DETAILS                          
         DC    AL1(TACRELQ),AL3(0)       CAST APPLIED CREDIT HISTORY            
         DC    AL1(TACWELQ),AL3(CLRPROC) CHECK WITHHOLDING                      
         DC    AL1(TACYELQ),AL3(CLRPROC) CHECK YTD                              
         DC    AL1(TADWELQ),AL3(DELPROC) DUE COMPANY WITHHOLDING                
         DC    AL1(TAFCELQ),AL3(0)       FIELD NUMBERS CHANGED ELEMENT          
         DC    AL1(TALWELQ),AL3(DELPROC) LIEN WITHHOLDING                       
         DC    AL1(TAOCELQ),AL3(0)       OLD INVOICE                            
         DC    AL1(TAODELQ),AL3(DELPROC) OTHER DEDUCTION                        
         DC    AL1(TAOKELQ),AL3(0)       OLD CHECK                              
         DC    AL1(TARNELQ),AL3(DELPROC) RETURNED CHECK INFORMATION             
         DC    AL1(TASDELQ),AL3(0)       SESSION DETAILS                        
         DC    AL1(TATIELQ),AL3(0)       TAX ID                                 
         DC    AL1(TAYEELQ),AL3(0)       YTD EARNINGS ELEMENT                   
*                                  * COMMON ELEMENTS                            
         DC    AL1(TAACELQ),AL3(DELPROC) ACTIVITY                               
         DC    AL1(TACMELQ),AL3(CMPROC)  COMMENTS                               
         DC    AL1(TAPDELQ),AL3(PDPROC)  PAY DETAILS                            
         DC    X'FF'                                                            
         EJECT                                                                  
*              CONSTANTS, CONT'D.                                               
         SPACE 1                                                                
CNTTAB   DS    0CL12                                                            
TALCOUNT DC    3PL4'0'                                                          
CHKCOUNT DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
REGACNT  DC    3PL4'0'                                                          
CODACNT  DC    3PL4'0'                                                          
AGYCNT   DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
REGCNT   DC    3PL4'0'                                                          
CODCNT   DC    3PL4'0'                                                          
INVCNT   DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
REGCCNT  DC    3PL4'0'                                                          
CODCCNT  DC    3PL4'0'                                                          
CHKCNT   DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
CASTCNT  DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
ERRCNT   DC    3PL4'0'                                                          
         DC    3PL4'0'                                                          
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
         EJECT                                                                  
*              CONSTANTS, CONT'D.                                               
         SPACE 2                                                                
LITTAB   DS    0CL40               ORDER CORRESPONDS TO CNTTAB!!                
         DC    CL40'TALFIL ADDS'                                                
         DC    CL40'CHKFIL ADDS'                                                
         DC    CL40'SKIP'                                                       
         DC    CL40'REGULAR (NON-COD) AGENCIES'                                 
         DC    CL40'COD AGENCIES'                                               
         DC    CL40'TOTAL AGENCIES'                                             
         DC    CL40'SKIP'                                                       
*                                                                               
         DC    CL40'REGULAR (NON-COD) INVOICES'                                 
         DC    CL40'COD INVOICES'                                               
         DC    CL40'TOTAL INVOICES'                                             
         DC    CL40'SKIP'                                                       
         DC    CL40'REGULAR (NON-COD) CHECKS'                                   
         DC    CL40'COD CHECKS'                                                 
         DC    CL40'TOTAL CHECKS'                                               
         DC    CL40'SKIP'                                                       
         DC    CL40'MISSING CAST RECORD WARNINGS'                               
         DC    CL40'SKIP'                                                       
         DC    CL40'TOTAL ERRORS'                                               
         DC    CL40'SKIP'                                                       
*                                                                               
         DC    CL40'ERROR 1 - CREDIT INVOICES'                                  
         DC    CL40'ERROR 2 - NO RATES DEFINED'                                 
         DC    CL40'ERROR 3 - 4 WK CABLE INVOICES'                              
         DC    CL40'ERROR 4 - 52 WK CABLE INVOICES'                             
         DC    CL40'ERROR 5 - UNAPPROVED INVOICES'                              
         DC    CL40'ERROR 6 - ZERO PAYMENT'                                     
         DC    CL40'ERROR 7 - NEGATIVE PAYMENT'                                 
         DC    CL40'ERROR 8 - EXCEEDED MAXIMUM UPGRADE'                         
         DC    CL40'ERROR 9 - REUSE PAID AFTER END OF PERIOD'                   
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
         EJECT                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
         DS    0D                                                               
VMYTRACE NMOD1 0,*TRAC*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         TM    TROPTS,TRTRACE                                                   
         BZ    XIT                                                              
         L     R2,4(R1)                                                         
         ZIC   R3,4(R1)                                                         
         MVC   WORK,0(R2)          MOVE LITERAL TO WORK                         
         LA    R4,WORK(R3)                                                      
         MVC   0(7,R4),=C' (AIO?)' ADD I/O AREA LITERAL                         
         LA    R3,7(R3)            BUMP L'LITERAL                               
         CLC   AIO,AIO1                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'1'          SET CURRENT I/O AREA LITERAL                 
         CLC   AIO,AIO2                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'2'                                                       
         CLC   AIO,=A(INVIO)                                                    
         BNE   *+14                                                             
         MVC   2(6,R4),=C'INVIO)'                                               
         LA    R3,1(R3)                                                         
         CLC   AIO,=A(CHKIO)                                                    
         BNE   *+14                                                             
         MVC   2(6,R4),=C'CHKIO)'                                               
         LA    R3,1(R3)                                                         
         CLC   AIO,TIAREC                                                       
         BNE   *+14                                                             
         MVC   2(7,R4),=C'TIAREC)'                                              
         LA    R3,2(R3)                                                         
         GOTO1 TRACE,DMCB,AIO,0,WORK,(R3)                                       
         XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         SPACE 3                                                                
*              ROUTINE HANDLES STORAGE TRACES                                   
         SPACE 1                                                                
         DS    0D                                                               
VMYTRAC2 NMOD1 0,*TRC2*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         TM    TROPTS,TRTRACE                                                   
         BZ    XIT                                                              
         LM    R2,R4,4(R1)                                                      
         ZIC   RF,4(R1)                                                         
         GOTO1 TRACE,DMCB,(R3),(R4),(R2),(RF)                                   
         XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              ROUTINE TO SET TCGROSS & TCPNHR FOR SOAP PAYMENTS                
         SPACE 1                                                                
         DS    0D                                                               
SOPCALC  NMOD1 0,*SCLC*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         MVC   TCPNHR,=H'1200'     PNH RATE IS THE SAME                         
*                                                                               
         L     R4,TRACHK                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         OC    TAPDGRS,TAPDGRS                                                  
         BZ    SOPCALN                                                          
*                                                                               
         LA    R3,SOPTAB           R3=A(SOAP TABLE)                             
         USING SOPTABD,R3                                                       
*                                                                               
SOPCAL10 CLI   SOPCATQ,X'FF'       TEST END OF TABLE                            
         BE    SOPCALN                                                          
         CLC   TGCAEQU,SOPCATQ     MATCH ON CATEGORY                            
         BNE   SOPCAL30                                                         
         CLC   TIBIDATE,SOPSDATE   MATCH ON PERIOD                              
         BL    SOPCAL30                                                         
         CLC   TIBIDATE,SOPEDATE                                                
         BH    SOPCAL30                                                         
*                                                                               
         XR    R0,R0                                                            
         L     R1,TAPDGRS          ORIGINAL PAYMENT AMOUNT                      
         ICM   RE,15,SOPSCALE      SCALE RATE                                   
         BNZ   *+10                                                             
         SR    R1,R1                                                            
         B     SOPCAL20                                                         
*                                                                               
         DR    R0,RE               PAYMENT AMOUNT MUST BE MULT OF RATE          
         LTR   R0,R0                                                            
         BNZ   SOPCAL30                                                         
         MVC   HALF,SOPINC                                                      
         MH    R1,HALF             MULTIPLY MULTIPLE BY INCREASE AMOUNT         
*                                                                               
SOPCAL20 L     RE,TAPDGRS          ADD TO ORIGINAL PYMT AMOUNT                  
         AR    RE,R1                                                            
         ST    RE,TCGROSS          FOR NEW GROSS                                
         B     SOPCALY                                                          
*                                                                               
SOPCAL30 LA    R3,SOPTABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         B     SOPCAL10            LOOP                                         
         SPACE                                                                  
SOPCALY  XR    RC,RC                                                            
SOPCALN  LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
SOPTAB   DS    0CL13               * SOAP TABLE INCREASES                       
         DC    AL1(CTP),XL3'971116',XL3'980309',AL4(64300),AL2(1900)            
         DC    AL1(CTP),XL3'971116',XL3'980309',AL4(70730),AL2(2090)            
         DC    AL1(CTU5),XL3'971116',XL3'980309',AL4(28000),AL2(800)            
         DC    AL1(CTU5),XL3'971116',XL3'980309',AL4(30800),AL2(880)            
         DC    AL1(CTANN),XL3'971116',XL3'980309',AL4(120600),AL2(3600)         
         DC    AL1(CTANN),XL3'971116',XL3'980309',AL4(132660),AL2(3960)         
         DC    X'FF'                                                            
*                                                                               
*OPTAB   DS    0CL13               * SOAP TABLE INCREASES (OLD TABLE)           
*        DC    AL1(CTP),XL3'950115',XL3'950310',AL4(58000),AL2(2000)            
*        DC    AL1(CTP),XL3'950115',XL3'950310',AL4(63800),AL2(2200)            
*        DC    AL1(CTU5),XL3'950115',XL3'950310',AL4(25300),AL2(900)            
*        DC    AL1(CTU5),XL3'950115',XL3'950310',AL4(27830),AL2(990)            
*        DC    AL1(CTU5),XL3'950115',XL3'950310',AL4(000),AL2(000)              
*                                                                               
*OPTAB   DS    0CL13               * SOAP TABLE INCREASES (OLD TABLE)           
*        DC    AL1(CTP),XL3'920101',XL3'921115',AL4(53600),AL2(1600)            
*        DC    AL1(CTP),XL3'920101',XL3'921115',AL4(58960),AL2(1760)            
*        DC    AL1(CTP),XL3'921116',XL3'930315',AL4(53600),AL2(2700)            
*        DC    AL1(CTP),XL3'921116',XL3'930315',AL4(58960),AL2(2970)            
*        DC    AL1(CTP),XL3'920101',XL3'921115',AL4(23400),AL2(700)             
*        DC    AL1(CTP),XL3'920101',XL3'921115',AL4(25740),AL2(770)             
*        DC    AL1(CTP),XL3'921116',XL3'930315',AL4(23400),AL2(1200)            
*        DC    AL1(CTP),XL3'921116',XL3'930315',AL4(25740),AL2(1320)            
*        DC    AL1(CTU5),XL3'920101',XL3'921115',AL4(23400),AL2(700)            
*        DC    AL1(CTU5),XL3'920101',XL3'921115',AL4(25740),AL2(770)            
*        DC    AL1(CTU5),XL3'921116',XL3'930315',AL4(23400),AL2(1200)           
*        DC    AL1(CTU5),XL3'921116',XL3'930315',AL4(25740),AL2(1320)           
*        DC    AL1(CTU5),XL3'920101',XL3'930315',AL4(000),AL2(000)              
*        DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TRD      DSECT                                                                  
TRACALC  DS    V                   A(RATE CALCULATION ROUTINE)                  
MYTRACE  DS    V                   A(TRACE ROUTINE)                             
MYTRACE2 DS    V                   A(SECOND TRACE ROUTINE)                      
TRAINV   DS    A                   A(INVOICE RECORD)                            
TRACHK   DS    A                   A(CHECK RECORD)                              
*                                                                               
TROPTS   DS    XL1                 OPTIONS                                      
TRTRACE  EQU   X'80'               TRACE                                        
TROKNEG  EQU   X'40'               NEGATIVE PMTS OK                             
TRWRIDSK EQU   X'20'               WRITE TO DISK                                
TRPHONLY EQU   X'10'               CALCULATE P&H INCREASE ONLY                  
TRNOMARK EQU   X'08'               DON'T MARK RETROS DONE ON AGENCY REC         
TRFORCE  EQU   X'04'               FORCE PROCESS ALL AGENCIES                   
TRCURRNT EQU   X'02'               ADD RETRO PMTS TO CURRENT AGY/CLI            
TRSOAPS  EQU   X'01'               PROCESS SOAP RETROS ONLY                     
*                                                                               
TRSTAT   DS    XL1                 STATUS                                       
TRSORTNG EQU   X'80'               SORT IS ACTIVE                               
TRADDINV EQU   X'40'               INVOICE ADD PENDING                          
TRPROCIN EQU   X'20'               PROCESSING RETRO INVOICE RECORD              
TRREREAD EQU   X'10'               NEED TO REREAD SYSIO'S KEY                   
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
*                                                                               
TRSVOV1  DS    F                   SAVED DATA DURING SPECIAL GETRATE            
TRSVUSES DS    H                                                                
TRSVUNIT DS    H                                                                
TRSVMAJ  DS    XL1                                                              
*                                                                               
TRCSTTAB DS    CL(2*300)           TABLE OF CAST MEMBERS PAID REUSE             
         EJECT                                                                  
       ++INCLUDE TASYSCALCD                                                     
         SPACE 2                                                                
TRDLNQ   EQU   *-TRD               *** END OF TRD ***                           
         EJECT                                                                  
*              DSECT TO COVER SOAP TABLE                                        
         SPACE 1                                                                
SOPTABD  DSECT                                                                  
SOPCATQ  DS    XL1                 CATEGORY EQUATE                              
SOPSDATE DS    XL3                 START PERIOD                                 
SOPEDATE DS    XL3                 END PERIOD                                   
SOPSCALE DS    XL4                 SCALE AMOUNT                                 
SOPINC   DS    XL2                 INCREASE AMOUNT                              
SOPTABL  EQU   *-SOPTABD                                                        
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
SORTCZER EQU   0                   APPREUS INVOICE AND TACRBAL EQ ZERO          
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
SORTCHKL EQU   *-SORTCHK                                                        
         ORG   SORTCHK             * INVOICE RECORD FIELDS                      
SORTEST  DS    CL15                ESTIMATE NUMBER                              
         ORG                                                                    
SORTLNQ  EQU   *-SORTD                                                          
         EJECT                                                                  
*              DSECT TO COVER ACCUMULATORS                                      
         SPACE 1                                                                
ACCD     DSECT                                                                  
ACCAPPL  DS    F                   APPLIED AMOUNT                               
ACCPAY   DS    0CL8                                                             
ACCPAYI  DS    F                   INDIV. PAYMENT                               
ACCPAYC  DS    F                   CORP. PAYMENT                                
ACCSPNH  DS    F                   SUBJ. TO P&H                                 
ACCPNH   DS    F                   P&H                                          
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
**PAN#1  DC    CL21'064TAREP35OS 05/01/02'                                      
         END                                                                    
