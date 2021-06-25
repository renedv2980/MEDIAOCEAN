*          DATA SET TAGENE7    AT LEVEL 051 AS OF 03/02/17                      
*PHASE T702E7B,*                                                                
         TITLE 'T702E7 - TIME MAINTENANCE'                                      
T702E7   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T702E7,R7,RR=R2                                           
         LR    R3,RC               R3=A(TEMPORARY STORAGE)                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    R3,ATMPAREA         SAVE R3 IN WORKING STORAGE                   
         USING TEMPD,R3                                                         
         LH    RF,=AL2(SVPTRBLK-SYSD)                                           
         AR    RF,R9                                                            
         ST    RF,ASVPBLK          SET A(SAVED PASSIVE PTR BLOCK)               
         AHI   RF,L'SVPTRBLK                                                    
         ST    RF,AADPBLK          SET A(ADD PASSIVE PTR BLOCK)                 
         DROP  R3                                                               
         ST    R2,RELO                                                          
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              MODE CONTROLLED ROUTINES                                         
*--------------------------------------------------------------------*          
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   STMSPID(7),=C'Pid Num'                                           
         OI    STMSPIDH+6,X'80'                                                 
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   MODE10                                                           
         BRAS  RE,INIT            INITIALIZE                                    
         BRAS  RE,DELREC                                                        
*                                                                               
MODE10   CLI   ACTNUM,ACTDIS       IF WE CAME FROM THE LIST, SET TO             
         BNE   MODE15              RETURN TO LIST AT HIT OF ENTER               
         TM    PROSTAT,DISPLAYD                                                 
         BZ    MODE20                                                           
         CLI   PFAID,0                                                          
         BNE   MODE15              IF STACK NOT EMPTY, WE CAME                  
         CLI   CALLSP,0            FROM THE LIST SCREEN                         
         BE    MODE20                                                           
         MVI   THISLSEL,0                                                       
         OI    TRNSTAT,OKINTPFK                                                 
         MVI   PFAID,24                                                         
MODE15   BRAS  RE,INIT             INITIALIZE                                   
*                                                                               
MODE20   CLI   MODE,VALKEY         IF MODE IS VALKEY                            
         BE    VKEY                VALIDATE KEY                                 
         CLI   MODE,DISPKEY        IF MODE IS DISPLAY KEY                       
         BE    DKEY                                                             
         CLI   MODE,DISPREC                                                     
         BNE   MODE30                                                           
*                                                                               
         BAS   RE,DISPLAY                                                       
         B     XIT                                                              
MODE30   CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BE    VREC                                                             
*                                                                               
         USING WEBREQD,RE                                                       
         CLI   MODE,XRECADD        IF WE JUST ADDED A RECORD                    
         BNE   MODE40                                                           
         TM    TGFASTAT,TGFROMFA   AND TIMESHEET NOT COMING FROM                
         BZ    MODE31              WEB                                          
         L     RE,TGAFAREQ         OR IS COMING FROM WEB IN                     
         CLI   WBMODE,WBMDEXE      EXECUTE MODE ...                             
         BNE   MODE40                                                           
         DROP  RE                                                               
                                                                                
MODE31   GOTO1 TINVCON,DMCB,STMINV,INVNO,DATCON  CONVERT INVOICE NUMBER         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    INVNO,ALLFF           COMPLEMENT INVOICE NUMBER                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',INVNO)                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      STORE INTERNAL COMMID IN INVOICE             
         BAS   RE,GETEL                                                         
         USING TAIND,R4                                                         
         OC    TAINTMCO,TAINTMCO   IF WE ALREADY HAVE COMMID,                   
         BZ    MODE35              THEY MUST MATCH OR DIE                       
         CLC   TAINTMCO,TGCOM                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         B     MODE38                                                           
MODE35   MVC   TAINTMCO,TGCOM                                                   
         GOTO1 PUTREC                                                           
MODE38   MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
MODE40   CLI   MODE,XRECDEL        IF WE JUST DELETED A RECORD                  
         BNE   XIT                                                              
*                                                                               
         XC    KEY,KEY             IF NO MORE TIMESHEET RECORDS EXIST           
         LA    R4,KEY              FOR THIS INVOICE AND COMMERCIAL,             
         USING TLTMD,R4            WE NEED TO CLEAR INTERNAL COMMID             
         MVI   TLTMCD,TLTMCDQ      IN INVOICE RECORD                            
         MVC   TLTMCOM,TGCOM                                                    
         MVC   TLTMINV,TGINV                                                    
         NI    DMINBTS,X'FF'-X'08' DO NOT PASS DELETED RECORDS                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLTMSSN-TLTMD),KEYSAVE                                       
         BE    XIT                                                              
*                                                                               
         GOTO1 TINVCON,DMCB,STMINV,INVNO,DATCON  CONVERT INVOICE NUMBER         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    INVNO,ALLFF           COMPLEMENT INVOICE NUMBER                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B0',INVNO)                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET RID OF INTERNAL COMM# IN INVOICE         
         BAS   RE,GETEL                                                         
         USING TAIND,R4                                                         
         XC    TAINTMCO,TAINTMCO                                                
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO VALIDATE THE KEY                                      
*--------------------------------------------------------------------*          
VKEY     DS    0H                                                               
         BRAS  RE,SETHYPO          IF VALIDATING A HYPO TIMESHEET               
         JE    VK115               PERFORM SETUP AND SKIP AHEAD                 
                                                                                
         LA    R2,STMAGYH          VALIDATE START AT KEY                        
         TM    STMAGYH+4,X'20'                                                  
         BO    VK10                                                             
         GOTO1 CHKCLG,DMCB,STMAGYH,STMCIDH    CHECK/HANDLE CLI GRP              
         MVI   BYTE,0              ENFORCE AGENCY LIMIT RESTRICTIONS            
         BE    *+8                                                              
         MVI   BYTE,X'80'          SKIP AGENCY LIMIT RESTRICTIONS               
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'20',STMAGYH)                       
         OI    STMAGYH+4,X'20'     VALIDATED                                    
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK10     TM    STMCIDH+4,X'20'     WAS COMMERCIAL ID VALIDATED?                 
         BO    VK20                                                             
         LA    R2,STMCIDH                                                       
         GOTO1 RECVAL,DMCB,(X'30',TLCOICDQ),(X'28',STMCIDH),STMCOMNH            
         LA    R4,KEY                                                           
         USING TLCOPD,R4                                                        
         MVC   TGCOM,TLCOICOM      INTERNAL COMMERCIAL NUMBER                   
         OI    STMCIDH+4,X'20'     COMMERCIAL ID VALIDATED                      
         DROP  R4                                                               
*                                                                               
VK20     LA    R2,STMINVH          VALIDATE INVOICE NUMBER                      
         CLI   STMINVH+5,0                                                      
         BNE   VK40                                                             
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'24',STMINVH) USE GLOBAL INVOICE           
*                                                                               
         CLI   ERROR,MISSING       IF THERE'S AN ERROR                          
         BE    THEEND                                                           
         XC    STMINV,ALLFF        UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,STMINV,INVNO,DATCON CONVERT FOR DISPLAY             
         MVC   STMINV,INVNO                                                     
         CLI   0(R1),X'FF'         IF INVALID                                   
         BNE   VK30                                                             
         XC    STMINV,STMINV       CLEAR FROM SCREEN                            
         B     FLDINV              REQUIRE INVOICE INPUT                        
*                                                                               
VK30     CLI   ERROR,NOTFOUND                                                   
         BE    THEEND                                                           
         B     VK50                                                             
*                                                                               
VK40     MVI   WBERRFLD,D#TMINV                                                 
         GOTO1 TINVCON,DMCB,STMINV,INVNO,DATCON                                 
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    INVNO,ALLFF         COMPLEMENT INVOICE NUMBER                    
         MVC   TGINV,INVNO                                                      
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',INVNO)                                
         BNE   RECNTFND                                                         
*                                                                               
         USING TAIND,R4                                                         
VK50     L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            GET INVOICE STATUS EL                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   SVTAINST,TAINSTAT                                                
         TM    TAINSTAT,TAINSPAY   WAS INVOICE PAID?                            
         BNO   VK70                                                             
         CLI   ACTNUM,ACTCHA       IF INVOICE WAS PAID, CANNOT CHANGE,          
         BE    INVPAID             ADD, OR DELETE A TIMESHEET                   
         CLI   ACTNUM,ACTADD                                                    
         BE    INVPAID                                                          
         CLI   ACTNUM,ACTDEL                                                    
         BE    INVPAID                                                          
*                                                                               
VK70     OC    TAINTMCO,TAINTMCO   IF INVOICE ALREADY HAS A TIMESHEET           
         BZ    VK80                INTERNAL COMM ID, IT MUST MATCH              
         CLC   TAINTMCO,TGCOM                                                   
         BNE   INVUSED                                                          
VK80     OI    STMINVH+4,X'20'     INVOICE VALIDATED                            
*                                                                               
         TM    STMSSNH+4,X'20'     WAS SS# VALIDATED?                           
         BO    VK100                                                            
         NI    STMCATH+4,X'FF'-X'20'                                            
         LA    R2,STMSSNH                                                       
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK90                                                             
         CLI   STMSSNH+5,6                                                      
         BH    VK90                                                             
         MVC   TGPID,STMSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK90                                                             
         MVC   STMSSN,TGSSN                                                     
         MVI   STMSSNH+5,9                                                      
VK90     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),STMNAMEH                        
         OI    STMSSNH+4,X'20'     SS# VALIDATED                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK95                                                             
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   STMSSN,SPACES                                                    
         MVC   STMSSN(L'TGPID),TGPID                                            
         MVI   STMSSNH+5,6          INPUT LENGTH                                
         OI    STMSSNH+6,X'80'      TRANSMIT                                    
*                                                                               
VK95     L     R4,AIO                                                           
         MVI   ELCODE,TAWXELQ       GET DATE OF BIRTH                           
         BAS   RE,GETEL                                                         
         BNE   VK100                                                            
         USING TAWXD,R4                                                         
         OC    TAWXDOB,TAWXDOB      IF NO DOB, NOT MINOR                        
         BZ    VK100                                                            
         GOTOR SETMINOR,DMCB,TAWXDOB                                            
         DROP  R4                                                               
                                                                                
VK100    TM    STMCATH+4,X'20'     WAS CATEGORY VALIDATE?                       
         BO    VK110                                                            
         NI    STMDATEH+4,X'FF'-X'20'                                           
         LA    R2,STMCATH                                                       
         GOTO1 ANY                                                              
         GOTO1 CATVAL,DMCB,WORK                                                 
         BNE   FLDINV                                                           
         OI    STMCATH+4,X'20'     VALIDATED CATEGORY                           
*                                                                               
VK110    LA    R2,STMSSNH          VALIDATE CAST                                
         MVI   WBERRFLD,D#TMSEQ                                                 
         GOTO1 RECVAL,DMCB,TLCACCDQ,0                                           
         CLC   KEY(TLCAGSEQ-TLCAPD),KEYSAVE                                     
         BNE   RECNTFND                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING TLCAD,R4                                                         
         MVC   TGCSORT,TLCASORT                                                 
         DROP  R4                                                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ    GET CAST DETAILS ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6               MUST EXIST                                     
         DC    H'00'                                                            
         USING TACAD,R4                                                         
         MVI   US2404,0          SAG RATE FOR 2404 FLAG                         
         GOTOR ONCAINV,DMCB,TACAEL  IF ON US INVOICE, USE SAG RATES             
         BE    *+8                                                              
         MVI   US2404,C'Y'                                                      
         MVC   SVCNYR,TACAYEAR                                                  
         CLC   TACAUN,=C'AFM'    UNION AFM, NOT ALLOWED                         
         BE    INVMSUNI                                                         
         DROP  R4                                                               
*                                                                               
VK115    TM    STMDATEH+4,X'20'    WAS DATE VALIDATED?                          
         BO    VK120                                                            
         LA    R2,STMDATEH                                                      
         CLI   STMDATEH+5,0                                                     
         BE    VK120                                                            
         GOTO1 DTVAL,DMCB,TMSHDATE                                              
         OI    STMDATEH+4,X'20'                                                 
*                                                                               
VK120    TM    TGFASTAT,TGFROMFA   IF TIMESHEET IS COMING FROM WEB              
         BZ    VK130               JUST BUILD KEY AND EXIT                      
         GOTO1 RECVAL,DMCB,TLTMCDQ,(X'C0',0)                                    
         B     XIT                                                              
*                                                                               
VK130    CLI   RECNUM,DM           ELSE, READ FOR TIMESHEET                     
         BNE   *+8                                                              
         OI    TGFASTAT,TGRDWBTS                                                
         GOTO1 RECVAL,DMCB,TLTMCDQ,(X'A4',0)                                    
         BE    VK150                                                            
                                                                                
         CLI   ACTNUM,ACTADD       IF TIMESHEET DOES NOT EXIST                  
         BE    VK140               ENSURE THAT ACTION IS ADD                    
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         BE    VK140                                                            
         BRAS  RE,INIT2                                                         
         LA    R2,STMAGYH                                                       
         B     RECNTFND                                                         
                                                                                
VK140    CLI   RECNUM,DM           IF ACTION IS ADD OR RESTORE                  
         BNE   *+8                 RE-BUILD KEY AND EXIT                        
         OI    TGFASTAT,TGRDWBTS                                                
         GOTO1 RECVAL,DMCB,TLTMCDQ,(X'C0',0)                                    
         B     XIT                                                              
*                                                                               
VK150    CLI   ACTNUM,ACTADD       IF TIMESHEET IS NOT FOUND                    
         BNE   XIT                                                              
         MVI   ELCODE,TATMELQ                                                   
         GOTO1 GETL,DMCB,(3,TMSHDATE)                                           
         BE    RECONFIL            RECORD ON FILE ALREADY                       
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO DISPLAY THE KEY                                       
*--------------------------------------------------------------------*          
DKEY     DS    0H                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         CLI   RECNUM,DM                                                        
         BNE   *+8                                                              
         OI    TGFASTAT,TGRDWBTS                                                
         GOTO1 RECVAL,DMCB,TLTMCDQ,(X'A0',0)                                    
         BNE   RECNTFND                                                         
*                                                                               
         L     R4,AIO                                                           
         USING TLTMD,R4                                                         
         MVC   STMSSN,TLTMSSN                                                   
*                                                                               
         TM    TGSYSTAT,TASYSPID  ARE WE USING PID#                             
         BZ    DK10                                                             
         GOTO1 SSNPACK,DMCB,TLTMSSN,TGPID                                       
         MVC   STMSSN,SPACES                                                    
         MVC   STMSSN(L'TGPID),TGPID                                            
         MVI   STMSSNH+5,6        INPUT LENGTH                                  
*                                                                               
DK10     MVC   STMCAT,TLTMCAT                                                   
         OI    STMSSNH+6,X'80'                                                  
         OI    STMCATH+6,X'80'                                                  
         MVC   KEY,SVKEY                                                        
DKX      B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              DISPLAY THE RECORD                                               
*--------------------------------------------------------------------*          
DISPLAY  NTR1                                                                   
         MVI   PROSTAT,0           PROGRAM STATUS                               
*                                                                               
*&&DO                                                                           
         L     R4,AIO                                                           
         USING TLTMD,R4                                                         
         MVC   STMSSN,TLTMSSN      SSN                                          
*                                                                               
         MVC   TGSSN,TLTMSSN       INSURE CAST SSN IN GLOBAL                    
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    DR20                                                             
         CLI   STMSSNH+5,6                                                      
         BH    DR20                                                             
         MVC   TGSSN,SPACES                                                     
         MVC   TGPID,STMSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
*                                                                               
DR20     MVC   STMCAT,TLTMCAT      CATEGORY                                     
         OI    STMSSNH+6,X'80'                                                  
         OI    STMCATH+6,X'80'                                                  
*&&                                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)  GET COMM'L RECORD                
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TACOD,R4                                                         
         MVC   AIO,AIO1            RESET AIO                                    
         MVC   SVCOTYPE,TACOTYPE                                                
         MVC   SVACTRA,TACOCTYP                                                 
         MVC   SVCOADST,TACOADST                                                
         CLI   SVACTRA,0           CANADIAN COMMERCIAL?                         
         BE    DR30                                                             
         CLI   SVACTRA,CCTY04B     2404B = SAG RATES, SAG RULES                 
         BE    DR30                                                             
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    DR30                                                             
         MVC   STM10P,=C'20%'      NP ACTRA = 20%                               
         XC    STM20P,STM20P                                                    
         CLI   SVACTRA,CCTY04A     IF 2404A, 25%  OR,                           
         BNE   DR40                                                             
***      BE    DR25                                                             
***      CLC   SVCNYR(2),=C'14'    IF CONTRACT YEAR IS EARLIER THAN 14          
***      BNL   DR40                                                             
DR25     MVC   STM10P,=C'25%'      NP ACTRA = 25%                               
         MVC   STM20P,=C'25%'      NP SAG = 25%                                 
         B     DR40                                                             
         DROP  R4                                                               
*                                                                               
DR30     MVC   STM10P,=C'10%'       NP10 = 10%                                  
         MVC   STM20P,=C'20%'       NP20 = 20%                                  
DR40     OI    STM10PH+6,X'80'                                                  
         OI    STM20PH+6,X'80'                                                  
*                                                                               
         BAS   RE,SETSCRN          IF PFKEY HIT, GET NEXT/PREV TIMESHT          
         BE    DR80                R4 WILL RETURN POINTING TO ELEMENT           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TATMELQ      TIMESHEET ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TATMD,R4                                                         
         MVI   NTHTSHT,1                                                        
         CLI   STMDATEH+5,0        IF NO DATE, SHOW FIRST ONE                   
         BE    DR80                                                             
*                                                                               
         LA    R2,STMDATEH                                                      
         GOTO1 DTVAL,DMCB,TMSHDATE                                              
         L     R4,AIO              GET MATCHING TIMESHEET                       
         MVI   ELCODE,TATMELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR60     BAS   RE,NEXTEL                                                        
         BNE   RECNTFND            EOR, NO MATCHING TIMESHEET                   
         CLC   TATMDATE,TMSHDATE                                                
         BE    DR80                                                             
         SR    R1,R1                                                            
         IC    R1,NTHTSHT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NTHTSHT          BUMP AND SAVE NTH TIMESHEET NUMBER           
         B     DR60                                                             
*                                                                               
DR80     MVC   TMSHDATE,TATMDATE                                                
         GOTO1 DATCON,DMCB,(1,TATMDATE),(10,STMDATE)                            
         MVI   STMDATEH+5,8                                                     
         OI    STMDATEH+6,X'80'                                                 
         OI    STMDATEH+4,X'20'    SET VALID BIT - NOT SET IF FROM LIST         
*                                                                               
         BRAS  RE,DISPDAY          DISPLAY DAY OF THE WEEK                      
*                                                                               
*                                  WORK TIME                                    
         GOTOR TIMDISP,DMCB,TATMWTST,STMWTSTH,STMWTNTH,STMWTTHH                 
*                                                                               
         GOTOR TIMDISP,DMCB,TATMM1ST,STMM1STH,STMM1NTH,STMM1THH  MEAL 1         
         GOTOR TIMDISP,DMCB,TATMM2ST,STMM2STH,STMM2NTH,STMM2THH  MEAL 2         
         GOTOR TIMDISP,DMCB,TATMM3ST,STMM3STH,STMM3NTH,STMM3THH  MEAL 3         
         GOTOR TIMDISP,DMCB,TATMPDST,STMPDSTH,STMPDNTH,STMPDTHH  WARDRB         
         GOTOR TIMDISP,DMCB,TATMTTDP,STMTTDPH,STMTTARH,STMTTTHH  TRV TO         
         GOTOR TIMDISP,DMCB,TATMTFDP,STMTFDPH,STMTFARH,STMTFTHH  TRV FM         
         GOTOR TIMDISP,DMCB,TATMTIDP,STMTIDPH,STMTIARH,STMTITHH  TR INT         
*                                                                               
         GOTO1 DATCON,DMCB,(1,TATMPWDT),(10,STMPDDT) PDW DATE                   
         MVI   STMPDDTH+5,8                                                     
         OI    STMPDDTH+6,X'80'                                                 
*                                                                               
         MVI   STMNHTP,0                                                        
         TM    TATMSTAT,TATMSNPY   CALCULATE NIGHT PREMIUM?                     
         BNO   *+12                                                             
         MVI   STMNHTP,C'Y'                                                     
         B     DR90                                                             
         TM    TATMSTAT,TATMSNPN   DON'T CALCULATE NIGHT PREMIUM?               
         BNO   *+8                                                              
         MVI   STMNHTP,C'N'                                                     
DR90     OI    STMNHTPH+6,X'80'                                                 
*                                                                               
         MVI   STMSMPY,C'N'                                                     
         TM    TATMSTA2,TATMS2SP   SMOKE PAY?                                   
         BNO   *+8                                                              
         MVI   STMSMPY,C'Y'                                                     
         OI    STMSMPYH+6,X'80'     TRANSMIT                                    
*                                                                               
         MVI   STM16YN,0                                                        
         TM    TATMSTAT,TATMS16Y   CALCULATE 16HR RULE?                         
         BNO   *+12                                                             
         MVI   STM16YN,C'Y'                                                     
         B     DR100                                                            
         TM    TATMSTAT,TATMS16N   DON'T CALCULATE 16HR RULE?                   
         BNO   *+8                                                              
         MVI   STM16YN,C'N'                                                     
DR100    OI    STM16YNH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   STMMLPH,0                                                        
         TM    TATMSTAT,TATMSMPY   CALCULATE MEAL PENALTY 1?                    
         BNO   *+12                                                             
         MVI   STMMLPH,C'Y'                                                     
         B     DR110                                                            
         TM    TATMSTAT,TATMSMPN   DON'T CALCULATE MEAL PENALTY 1?              
         BNO   *+8                                                              
         MVI   STMMLPH,C'N'                                                     
DR110    OI    STMMLPHH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   STMMLP2,0                                                        
         TM    TATMSTAT,TATMSM2Y   CALCULATE MEAL PENALTY 2?                    
         BNO   *+12                                                             
         MVI   STMMLP2,C'Y'                                                     
         B     DR120                                                            
         TM    TATMSTAT,TATMSM2N   DON'T CALCULATE MEAL PENALTY 2?              
         BNO   *+8                                                              
         MVI   STMMLP2,C'N'                                                     
DR120    OI    STMMLP2H+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   STMMLP3,0                                                        
         TM    TATMSTA3,TATMSM3Y   CALCULATE MEAL PENALTY 3?                    
         BNO   *+12                                                             
         MVI   STMMLP3,C'Y'                                                     
         B     DR125                                                            
         TM    TATMSTA3,TATMSM3N   DON'T CALCULATE MEAL PENALTY 3?              
         BNO   *+8                                                              
         MVI   STMMLP3,C'N'                                                     
DR125    OI    STMMLP3H+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   STMM1ND,C'N'                                                     
         TM    TATMSTA2,TATMS2NM   NON DEDUCTIBLE MEAL?                         
         BNO   *+8                                                              
         MVI   STMM1ND,C'Y'                                                     
         OI    STMM1NDH+6,X'80'     TRANSMIT                                    
*                                                                               
         MVI   STMRHSL,C'N'                                                     
         TM    TATMSTA2,TATMS2RD   REHEARSAL DAY?                               
         BNO   *+8                                                              
         MVI   STMRHSL,C'Y'                                                     
         OI    STMRHSLH+6,X'80'     TRANSMIT                                    
*                                                                               
         MVI   STMWTCN,C'N'                                                     
         TM    TATMSTA2,TATMS2WC   WEATHER CANCELLATION?                        
         BNO   *+8                                                              
         MVI   STMWTCN,C'Y'                                                     
         OI    STMWTCNH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   STMDSLC,C'N'                                                     
         TM    TATMSTA2,TATMS2DL   DISTANT LOCATION?                            
         BNO   *+8                                                              
         MVI   STMDSLC,C'Y'                                                     
         OI    STMDSLCH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   STMRPVL,0                                                        
         TM    TATMSTA3,TATMSRPY   REST PERIOD VIOLATION?                       
         BNO   *+12                                                             
         MVI   STMRPVL,C'Y'                                                     
         B     DR130                                                            
         TM    TATMSTA3,TATMSRPN   NO REST PERIOD VIOLATION?                    
         BNO   *+8                                                              
         MVI   STMRPVL,C'N'                                                     
DR130    OI    STMRPVLH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   STMNCWD,C'N'                                                     
         TM    TATMSTA2,TATMS2NC   NON-CONSECUTIVE WORK DAY?                    
         BNO   *+8                                                              
         MVI   STMNCWD,C'Y'                                                     
         OI    STMNCWDH+6,X'80'    TRANSMIT                                     
*                                                                               
         EDIT  TATMNP10,STMNP10,2  DISPLAY NIGHT PREMIUM 10%                    
         OI    STMNP10H+6,X'80'                                                 
                                                                                
         CLI   SVACTRA,0                                                        
         BE    DR150                                                            
         CLI   SVACTRA,CCTY04B                                                  
         BE    DR150                                                            
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    DR150                                                            
         CLI   SVACTRA,CCTY04A                                                  
         BE    DR150                                                            
***      CLC   SVCNYR(2),=C'14'                                                 
***      BL    DR150                                                            
         EDIT  TATMNP20,STMNP20,2,ZERO=BLANK  (NO SAG NP FOR ACTRA)             
         B     DR160                                                            
DR150    EDIT  TATMNP20,STMNP20,2  AND NIGHT PREMIUM 20%                        
DR160    OI    STMNP20H+6,X'80'                                                 
*                                                                               
         EDIT  TATM16HR,STM16HR    DISPLAY 16 HOUR TOTAL                        
         OI    STM16HRH+6,X'80'                                                 
*                                                                               
         BRAS  RE,DISPREIM        DISPLAY REIMBURSEMENTS                        
*                                                                               
         BRAS  RE,DISPCOM         DISPLAY COMMENT                               
*                                                                               
         BRAS  RE,DISPTOT         DISPLAY TOTALS                                
*                                                                               
         GOTO1 ACTVOUT,DMCB,STMLCHGH   DISPLAY LAST CHANGED ELEMENT             
*                                                                               
         MVC   KEY,SVKEY                                                        
         OI    PROSTAT,DISPLAYD                                                 
         OI    STMWTSTH+6,X'40'   PLACE CURSOR AT WORK START                    
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO VALIDATE THE RECORD                                   
*--------------------------------------------------------------------*          
VREC     DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    VRX                                                              
         MVC   SVKEY,KEY                                                        
*                                                                               
         TM    TGFASTAT,TGFROMFA   IF TIMESHEET NOT COMING FROM WEB             
         BO    VR01                                                             
         L     R4,AIO                                                           
         CLI   ACTNUM,ACTADD       AND ADDING A RECORD                          
         BNE   VR02                                                             
         MVC   0(L'KEY,R4),KEY     BUILD KEY                                    
*                                                                               
VR01     TM    PROSTAT,HYPOTIME                                                 
         JO    VR02                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)  GET COMM'L RECORD                
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TACOD,R4                                                         
         MVC   AIO,AIO1            RESET AIO                                    
         MVC   SVCOTYPE,TACOTYPE   SET COMMERCIAL TYPE                          
         MVC   SVACTRA,TACOCTYP    SET ACTRA TYPE                               
         MVC   SVCOADST,TACOADST   SET ADDENDUM STATE                           
*                                                                               
VR02     TM    SVTAINST,TAINSPAY   CANNOT CHANGE IF INVOICE HAS                 
         BO    INVPAID             BEEN PAID                                    
*                                                                               
         MVI   DAYTYPE,0                                                        
         LA    R2,STMDATEH                                                      
         GOTO1 DTVAL,DMCB,TMSHDATE                                              
*                                                                               
         MVI   STMHLPY,C'N'        DEFAULT - NO HOLIDAY PAY                     
         CLI   SVACTRA,0           CANADIAN COMMERCIAL?                         
         BE    VR05                                                             
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    VR05                                                             
         CLI   SVACTRA,CCTY04B     2404B = SAG RATES, SAG RULES                 
         BNE   VR10                OTHER ACTRA TYPES = NO HOLIDAYS              
*                                                                               
VR05     CLI   TMSHDATE,X'C4'      HOLIDAY TABLE ONLY GOES UP TO 2024           
         BH    DATEINV                                                          
         GOTO1 DATCON,DMCB,(1,TMSHDATE),(10,STMDATE)                            
         GOTO1 DATCON,DMCB,(1,TMSHDATE),(0,TMSEDATE)                            
         MVC   TEMPDATE,TMSHDATE                                                
         MVI   STMHLPY,C'N'        DEFAULT - NO HOLIDAY PAY                     
         BRAS  RE,LKHLDY           LOOK UP TO SEE IF HOLIDAY                    
         BNE   VR10                                                             
         OI    DAYTYPE,DAYHLDY     SET AS HOLIDAY                               
         MVI   STMHLPY,C'Y'                                                     
         B     VR20                                                             
VR10     GOTO1 GETDAY,DMCB,TMSEDATE,WORK                                        
         OC    SVACTRA,SVACTRA     ACTRA AND NOT 2404B, NO SPECIAL              
         BZ    VR13                PAY FOR FRI/SAT/SUN                          
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    VR13                                                             
         CLI   SVACTRA,CCTY04B                                                  
         BNE   VR20                                                             
VR13     CLI   0(R1),6             SATURDAY OR SUNDAY?                          
         BL    VR15                                                             
         BH    *+12                                                             
         OI    DAYTYPE,DAYSAT      SATURDAY=6                                   
         B     VR20                                                             
         OI    DAYTYPE,DAYSUN      SUNDAY=7                                     
*                                                                               
VR15     TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   VR20                                                             
         CLI   0(R1),5             FRIDAY?                                      
         BNE   VR20                                                             
         OI    DAYTYPE,DAYFRI      FRIDAY=5                                     
*                                                                               
VR20     OI    STMHLPYH+6,X'80'                                                 
         OI    STMDATEH+6,X'80'                                                 
*                                                                               
         XC    OLDTTEL,OLDTTEL                                                  
         XC    OLDPWDT,OLDPWDT                                                  
*                                                                               
         BRAS  RE,GETSPOT          SEE IF ANY DAY HAS SPOTS                     
*                                                                               
         CLI   SVACTRA,CCTY04A                                                  
         BNE   VR25                                                             
         MVI   WBERRFLD,D#TMDAT                                                 
         BRAS  RE,GET1STD          SEE IF 1ST DAY WAS ALREADY ADDED             
         BNE   FLDINV                                                           
*                                                                               
VR25     L     R4,AIO                                                           
         USING TATMD,R4                                                         
         MVI   ELCODE,TATMELQ      GET TATM ELEMENT                             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR30     BAS   RE,NEXTEL                                                        
         BE    VR40                DON'T HAVE IT, JUST ADD IT BLANK             
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING NEW REC, DON'T ADD BLANK           
         BE    VR70                                                             
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM           CLEAR NEW ELEMENT                            
         MVI   TATMEL,TATMELQ                                                   
         MVI   TATMLEN,TATMLNQ                                                  
         MVC   TATMDATE,TMSHDATE   TIME SHEET DATE                              
         GOTO1 ADDELEM                                                          
         OI    STMDATEH+4,X'20'    VALIDATE DATE FIELD                          
         TM    TGFASTAT,TGFROMFA                                                
         BO    VR25                                                             
         B     VRX                                                              
*                                                                               
VR40     CLC   TATMDATE,TMSHDATE                                                
         BNE   VR30                                                             
*                                  IF WE HAVE THE ELEMENT,                      
         TM    STMDATEH+4,X'20'    WAS DATE CHANGED MANUALLY?                   
         BO    VR50                                                             
         CLI   ACTNUM,ACTCHA       IF ACTION CHANGE, FORCE TO DISPLAY           
         BNE   VR50                                                             
         MVI   ACTNUM,ACTDIS                                                    
         MVC   CONACT(7),=C'DISPLAY'                                            
         OI    CONACTH+5,X'80'                                                  
         B     VRX                 DISPLAY THIS ELEMENT FIRST                   
*                                                                               
VR50     MVI   TATMEL,X'FF'        MARK FOR DELETION                            
         MVC   OLDPWDT,TATMPWDT    SAVE OLD PDW DATE                            
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'FF'        DELETE OLD TATM ELEM, ADD ONE LATER          
         GOTO1 REMELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         USING TATTD,R4                                                         
         MVI   ELCODE,TATTELQ      GET TOTALS ELEMENT                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR60     BAS   RE,NEXTEL                                                        
         BNE   VR70                                                             
         CLC   TATTDATE,TMSHDATE                                                
         BNE   VR60                                                             
*                                  IF WE HAVE THE ELEMENT,                      
         MVC   OLDTTEL,0(R4)       SAVE OLD TATT ELEMENT                        
         MVI   TATTEL,X'FF'        MARK FOR DELETION                            
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'FF'        DELETE OLD TATT ELEM ADD ONE LATER           
         GOTO1 REMELEM                                                          
*                                                                               
VR70     LA    R4,ELEM                                                          
         USING TATMD,R4                                                         
         XC    ELEM,ELEM           CLEAR NEW ELEMENT                            
         MVI   TATMEL,TATMELQ                                                   
         MVI   TATMLEN,TATMLNQ                                                  
         MVC   TATMDATE,TMSHDATE   TIME SHEET DATE                              
*                                                                               
         MVI   ACTRULES,0                                                       
         CLI   SVACTRA,0           CANADIAN?                                    
         BE    VR75                                                             
         CLI   SVACTRA,CCTY04B     2404B USES SAG RATES                         
         BE    VR75                                                             
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    VR75                                                             
         MVI   ACTRULES,C'Y'       USING ACTRA RULES                            
         BRAS  RE,BLDACT           BUILD AND CALC TIMES FOR ACTRA               
         B     *+8                                                              
VR75     BAS   RE,BLDSAG           BUILD AND CALCULATE TIMES FOR SAG            
*                                                                               
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
         BRAS  RE,MEALPEN          CALCULATE MEAL PENALTIES                     
         BRAS  RE,VALSMKP          VALIDATE SMOKE PAY                           
*                                                                               
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
         LA    R2,STMSPOTH         SPOTS                                        
         CLI   5(R2),0             IF SPOTS INPUT,                              
         BE    VR80                                                             
         GOTO1 VALINUM             MUST BE NUMERIC                              
         MVC   SVSPOTS,ACTUAL                                                   
         CLI   SVDAYS,0            IF SPOTS, MUST HAVE DAYS                     
         BNE   VR90                                                             
         MVI   SVDAYS,1                                                         
         B     VR90                                                             
VR80     OC    TATMWTST,TATMWTST   IF NO SPOTS, AND THERE IS WORK TIME          
         BZ    VR90                                                             
         CLI   SPOTFLG,0           IF SPOTS ARE ON ANOTHER DAY, OKAY            
         BNE   VR90                                                             
         CLI   PFAID,20            NO FTRACK REC WILL BE ADDED, OK=PF20         
         BE    VR90                                                             
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVSPFTR                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVSPF),0                                    
*                                                                               
VR90     LA    R2,STMTAGSH         TAGS                                         
         CLI   5(R2),0                                                          
         BE    VR100                                                            
         MVI   WBERRFLD,D#TMTAG                                                 
         CLI   ACTRULES,C'Y'       IF USING ACTRA RULES,                        
         BE    FLDINV              TAGS ARE NOT ALLOWED                         
         GOTO1 VALINUM                                                          
         MVC   SVTAG,ACTUAL                                                     
*                                                                               
VR100    BAS   RE,VALREIM          VALIDATE/CALCULATE REIMBURSEMENTS            
*                                                                               
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         BRAS  RE,VALADJS          VALIDATE ADJUSTMENTS                         
*                                                                               
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
         TM    TATMSTA2,TATMS2SP   PAY SMOKE PAY?                               
         BNO   *+8                                                              
         BRAS  RE,CALCSMKP         CALCULATE SMOKE PAY AMOUNT                   
         DROP  R4                                                               
*                                                                               
         USING TATTD,R4                                                         
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM           CLEAR NEW ELEMENT                            
         MVI   TATTEL,TATTELQ                                                   
         MVI   TATTLEN,TATTLNQ                                                  
         MVC   TATTDATE,TMSHDATE   TIME SHEET DATE                              
*                                                                               
         CLI   SVACTRA,0           CANADIAN?                                    
         BE    VR108                                                            
         CLI   SVACTRA,CCTY04B     2404B USES SAG RATES, SAG RULES              
         BE    VR108                                                            
         CLI   US2404,C'Y'         2404 US CAST USES SAG RATES/RULES            
         BE    VR108                                                            
         OI    TATTSTAT,TATTSART   ACTRA RATES FOR CANADIAN INVOICE             
         B     VR108                                                            
*                                                                               
VR103    CLI   SVACTRA,CCTY04A     2404A USES ACT RULES, SAG/ACT RATES          
         BNE   VR105                                                            
         OI    TATTSTAT,TATTSSRT   SAG RATES                                    
         CLI   FIRSTDY,C'Y'        1ST DAY OF SESSION? SAG/ACTRA RATES          
         BNE   VR150                                                            
VR105    OI    TATTSTAT,TATTSART   ACTRA RATES                                  
         B     VR150                                                            
*                                                                               
VR108    TM    DAYTYPE,DAYHLDY     IF HOLIDAY PAY, SET STATUS                   
         BNO   VR110                                                            
         CLI   NCONFLAG,C'Y'       IF NON-CONS, USE DIST LOC STATUS             
         BNE   VR109                                                            
         OI    TATTSTAT,TATTSTDL   NON-CONSECTIVE HOLIDAY                       
         B     VR130                                                            
VR109    OI    TATTSTAT,TATTSTHP   REGULAR HOLIDAY                              
         B     VR130                                                            
VR110    TM    DAYTYPE,DAYSAT      IF SATURDAY, SET STATUS                      
         BNO   VR120                                                            
         CLI   NCONFLAG,C'Y'       IF NON-CONS, USE DIST LOC STATUS             
         BNE   VR115                                                            
         OI    TATTSTAT,TATTSTDL   NON-CONSECUTIVE SATURDAY                     
         B     VR130                                                            
VR115    OI    TATTSTAT,TATTSTST   REGULAR SATURDAY                             
         B     VR130                                                            
VR120    TM    DAYTYPE,DAYSUN      IF SUNDAY, SET STATUS                        
         BNO   VR130                                                            
         CLI   NCONFLAG,C'Y'       IF NON-CONS, USE DIST LOC STATUS             
         BNE   VR125                                                            
         OI    TATTSTAT,TATTSTDL   NON-CONSECUTIVE SUNDAY                       
         B     VR130                                                            
VR125    OI    TATTSTAT,TATTSTSU   REGULAR SUNDAY                               
*                                                                               
VR130    TM    SVWTRST,SVWTRHF     IF WTHR CXL 1/2 SET STATUS                   
         BNO   VR135                                                            
         OI    TATTSTAT,TATTSWCH                                                
VR135    TM    SVWTRST,SVWTR3Q     IF WTHR CXL 3/4 SET STAT                     
         BNO   VR140                                                            
         OI    TATTSTAT,TATTSW3Q                                                
VR140    TM    SVSTAT2,TATMS2DL    IF DISTANT LOCATION, SET STATUS              
         BNO   VR150                                                            
         OI    TATTSTAT,TATTSTDL                                                
VR150    MVC   TATTSPOT(L'SVDATA),SVSPOTS                                       
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
         BRAS  RE,BLDTOTS          BUILD GRAND TOTAL AND SUBTOTAL ELEMS         
*                                                                               
         BRAS  RE,BLDPDWD          BUILD PRIOR DAY WARDROBE ELEMENT             
*                                                                               
         BRAS  RE,BLDCOM           COMMENT                                      
*                                                                               
VRX      TM    TGFASTAT,TGFROMFA                                                
         BO    *+8                                                              
         BAS   RE,DISPLAY                                                       
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING, EXIT                              
         BE    VRXXX                                                            
         MVC   AIO,AIO2            REGET RECORD                                 
         MVC   KEY(L'SVKEY),SVKEY                                               
         LA    R2,STMAGYH          SET CURSOR IF RECORD NOT FOUND               
         GOTO1 READ                                                             
                                                                                
         USING WEBREQD,RE                                                       
         TM    TGFASTAT,TGFROMFA   IF TIMESHEET IS COMING FROM WEB              
         BZ    VRXX                WEB                                          
         L     RE,TGAFAREQ         AND THIS IS THE FINAL DAY OF                 
         CLI   WBTMFDY,C'Y'        TIMESHEET, SET TO READ FOR                   
         BNE   VRXX                UPDATE                                       
         MVI   RDUPDATE,C'Y'                                                    
         DROP  RE                                                               
                                                                                
VRXX     GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VRXXX    GOTO1 ACTVIN,DMCB,STMLCHGH   UPDATE LAST CHANGED ELEMENT               
         B     XIT                                                              
                                                                                
                                                                                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE GETS NEXT OR PREVIOUS TIMESHEET IF PF14 OR 15 HIT              
*--------------------------------------------------------------------*          
SETSCRN  NTR1                                                                   
         MVI   PFKFLAG,0                                                        
         CLI   PFAID,14            PF14 HIT?                                    
         BNE   SSCRN20             GET PREVIOUS TIMESHEET                       
         ZIC   R1,NTHTSHT                                                       
         BCTR  R1,0                                                             
         STC   R1,NTHTSHT                                                       
         LTR   R1,R1               IF NO PREVIOUS ELEMENTS,                     
         BNZ   SSCRN30                                                          
         MVI   NTHTSHT,1           START WITH THE FIRST ELEMENT                 
         MVI   MYMSGNO1,117        DISPLAY MESSAGE, NO PREV TIMESHEETS          
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0         (NOT POSSIBLE FROM WEB)                      
*                                                                               
SSCRN20  CLI   PFAID,15            PF15 HIT?                                    
         BNE   SSCRN40             GET NEXT TIMESHEET                           
         ZIC   R1,NTHTSHT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NTHTSHT                                                       
SSCRN30  BAS   RE,FINDELEM                                                      
         BNE   PFNO                                                             
         MVI   PFKFLAG,1           SET FLAG THAT PF14 OR 15 WERE HIT            
*                                                                               
SSCRN40  CLI   MODE,DISPREC                                                     
         BNE   SSCRN50                                                          
         TWAXC STMWTSTH,STMLAST                                                 
SSCRN50  CLI   PFKFLAG,1                                                        
         BNE   PFNO                                                             
PFYES    XR    RC,RC               SET CC EQUAL IF PFKEY WAS HIT                
PFNO     LTR   RC,RC                                                            
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO FIND THE NTH ELEMENT USING NTHTSHT                    
*--------------------------------------------------------------------*          
FINDELEM NTR1                                                                   
         L     R4,AIO                                                           
         ZIC   R1,NTHTSHT          NTH ONE IN RECORD                            
         MVI   ELCODE,TATMELQ      FIND TIMESHEET ELEMENT                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FEL10    BAS   RE,NEXTEL                                                        
         BNE   FEL20                                                            
         BCT   R1,FEL10                                                         
         B     FELYES                                                           
FEL20    L     R4,AIO              IF WE DON'T HAVE A NEXT ELEMENT              
         ZIC   R1,NTHTSHT                                                       
         AHI   R1,-1               REDISPLAY LAST ELEMENT                       
         STC   R1,NTHTSHT          DECREMENT NUMBER BY ONE                      
         MVI   ELCODE,TATMELQ      FIND TIMESHEET ELEMENT                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FEL30    BAS   RE,NEXTEL                                                        
         BE    FEL40                                                            
         XR    R4,R4                                                            
         B     FELNO                                                            
FEL40    BCT   R1,FEL30                                                         
         MVI   MYMSGNO1,118        DISPLAY MESSAGE, NO MORE TIMESHEETS          
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0         (NOT POSSIBLE FROM WEB)                      
FELYES   XR    RC,RC                                                            
FELNO    LTR   RC,RC                                                            
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE UPDATES TIME AND CALCULATES AMOUNT OF TIME                     
*        SAG RATES ONLY                                                         
*        R4 ---> ELEM                                                           
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
BLDSAG   NTR1                                                                   
         XC    SVSPOTS(L'SVDATA),SVSPOTS                                        
         XC    EXTRVL,EXTRVL                                                    
         XC    WCXLTRVL,WCXLTRVL                                                
*                                                                               
         MVC   NPSTART,=H'2000'    SAG NIGHT PREMIUM STARTS AT 8PM              
*                                                                               
         GOTOR STENTIME,DMCB,STMWTSTH,STMWTNTH     WORK TIME                    
         MVC   TATMWTST,STRTTIME                   START                        
         MVC   TATMWTNT,ENDTIME                    END                          
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   BLDT20                                                           
         OC    STMTTDP,STMTTDP     AND EXTRA HAS TRAVEL TO TIME                 
         BZ    BLDT20              TRAVEL TO COUNTS AS WORK TIME                
         OC    STMWTNT,STMWTNT     AND THIS IS NOT A TRAVEL DAY                 
         BZ    BLDT20                                                           
         GOTOR STENTIME,DMCB,STMTTDPH,STMWTNTH                                  
BLDT20   MVC   WORKHRS,DIFFTHRS                                                 
*                                                                               
         BRAS  RE,VALMEAL          VALIDATE MEAL TIMES                          
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
         OC    MEALHRS,MEALHRS     ANY MEALS?                                   
         BZ    BLDT30                                                           
         MVC   DIFFTHRS,WORKHRS    WORK HOURS - CONVERT TO MINUTES              
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALHRS    MEAL HOURS - CONVERT TO MINUTES              
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         SR    R1,R0               DEDUCT MEAL HOURS FROM WORK HOURS            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         MVC   WORKHRS,DIFFTHRS                                                 
*                                                                               
*                                  PRIOR DAY WARDROBE                           
BLDT30   TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BZ    BLDT90              INPUT IS NOT ALLOWED                         
         LA    R2,STMPDDTH                                                      
         CLI   5(R2),0                                                          
         BE    BLDT31                                                           
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMPDD                              
BLDT31   LA    R2,STMPDSTH                                                      
         CLI   5(R2),0                                                          
         BE    BLDT32                                                           
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMPDS                              
BLDT32   LA    R2,STMPDNTH                                                      
         CLI   5(R2),0                                                          
         BE    BLDT90                                                           
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMPDE                              
*                                                                               
BLDT90   GOTOR STENTIME,DMCB,STMPDSTH,STMPDNTH                                  
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   BLDT100                                                          
         BRAS  RE,RNDHALF          ROUND PD-WD, NEAREST HALF HOUR               
         B     *+8                                                              
BLDT100  BRAS  RE,RNDQRTR          ROUND PD-WD, NEAREST QUARTER HOUR            
         OC    DIFFTHRS,DIFFTHRS   DO WE HAVE ANY PDWD?                         
         BZ    BLDT120                                                          
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   BLDT110                                                          
         CLC   DIFFTHRS,=H'200'    PAY AT LEAST 2 HOURS                         
         BNL   *+10                                                             
         MVC   DIFFTHRS,=H'200'                                                 
         B     BLDT120                                                          
*                                                                               
BLDT110  CLC   DIFFTHRS,=H'100'    PRINCIPALS, PAY AT LEAST ONE HOUR            
         BNL   *+10                                                             
         MVC   DIFFTHRS,=H'100'                                                 
BLDT120  MVC   SVPDWD,DIFFTHRS                                                  
         XC    PDWDDATE,PDWDDATE   INITIALIZE                                   
         OC    SVPDWD,SVPDWD       DO WE HAVE ANY PDWD?                         
         BZ    BLDT130                                                          
         LA    R2,STMPDDTH          PRIOR DAY WARDROBE DATE                     
         GOTO1 DTVAL,DMCB,PDWDDATE  VALIDATE AND STORE                          
         MVC   TATMPWDT,PDWDDATE                                                
*                                                                               
BLDT130  MVC   TATMPDST,STRTTIME   PRIOR DAY WARDROBE START                     
         MVC   TATMPDNT,ENDTIME    PRIOR DAY WARDROBE END                       
*                                                                               
         SR    R3,R3                                                            
         GOTOR STENTIME,DMCB,STMTTDPH,STMTTARH       TRAVEL TO                  
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA,                  
         BO    *+8                 TRAVEL TO DOESN'T COUNT AS                   
         LH    R3,DIFFTMIN         TRAVEL TIME                                  
         MVC   TATMTTDP,STRTTIME                                                
         MVC   TATMTTAR,ENDTIME                                                 
*                                                                               
         OC    TATMWTST,TATMWTST   IF THERE IS A WORK START,                    
         BZ    BLDT135                                                          
         OC    TATMTTAR,TATMTTAR                                                
         BZ    BLDT135                                                          
         LA    R2,STMTTARH         TRAVEL TO ARRIVE MUST = WORK START           
         CLC   TATMTTAR,TATMWTST                                                
         BE    BLDT135                                                          
         TM    TGFASTAT,TGFROMFA   INVALID TRAVEL TO ARRIVE                     
         BZ    INVTRTAR                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVTTA),0                                    
*                                                                               
BLDT135  GOTOR STENTIME,DMCB,STMTFDPH,STMTFARH       TRAVEL FROM                
         AH    R3,DIFFTMIN                                                      
         MVC   TATMTFDP,STRTTIME                                                
         MVC   TATMTFAR,ENDTIME                                                 
         OC    TATMTFDP,TATMTFDP   IF THERE IS TRAVEL FROM DEPART               
         BZ    BLDT136                                                          
         LA    R2,STMTFDPH                                                      
         MVI   WBERRFLD,D#TMTFD                                                 
         CLC   TATMTFDP,TATMTTDP   TRVL FROM DEP CANNOT = TRVL TO DEP           
         BE    FLDINV                                                           
BLDT136  OC    TATMTTDP,TATMTTDP   IF THERE IS TRAVEL TO DEPART                 
         BZ    BLDT137                                                          
         LA    R2,STMTFARH                                                      
         MVI   WBERRFLD,D#TMTFA                                                 
         CLC   TATMTTDP,TATMTFAR   TRVL TO DEP CANNOT = TRVL FROM ARR           
         BE    FLDINV                                                           
BLDT137  OC    TATMWTST,TATMWTST   IF THERE IS A WORK START,                    
         BZ    BLDT138                                                          
         LA    R2,STMTFARH         TRAVEL TO ARRIVE MUST = WORK START           
         MVI   WBERRFLD,D#TMTFA                                                 
         CLC   TATMTFAR,TATMWTST   WORK START CANNOT = TRVL FROM ARRIVE         
         BE    FLDINV                                                           
*                                                                               
BLDT138  GOTOR STENTIME,DMCB,STMTIDPH,STMTIARH     TRAVEL INTERV                
         AH    R3,DIFFTMIN                                                      
         STH   R3,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         MVC   TATMTIDP,STRTTIME                                                
         MVC   TRVLHRS2,DIFFTHRS   SAVE UNROUNDED TRAVEL HOURS                  
         BRAS  RE,RNDQRTR          ROUND TRAVEL, NEAREST QUARTER HOUR           
         MVC   TATMTIAR,ENDTIME                                                 
         MVC   TRVLHRS,DIFFTHRS                                                 
         MVC   HRMIN1,WORKHRS      ADD WORK HOURS AND TRVL HOURS                
         MVC   HRMIN2,TRVLHRS                                                   
         BRAS  RE,ADDHRS                                                        
         LH    R3,DIFFTHRS                                                      
         TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BZ    BLDT139                                                          
         CHI   R3,200              DON'T PAY TRVL IF TRVL+WORK <= 2 HRS         
         B     *+8                                                              
BLDT139  CHI   R3,800              DON'T PAY TRVL IF TRVL+WORK <= 8 HRS         
         BH    BLDT140                                                          
         MVC   WCXLTRVL,TRVLHRS    SAVE TRVL FOR WEATHER CANCELLATION           
         XC    TRVLHRS,TRVLHRS                                                  
         SH    R3,TRVLHRS                                                       
BLDT140  MVC   SVTRVL,TRVLHRS                                                   
*                                                                               
         OC    TATMWTNT,TATMWTNT   IF THERE IS A WORK END,                      
         BZ    BLDT145                                                          
         OC    TATMTIDP,TATMTIDP   INTERVENING TRAVEL?                          
         BZ    BLDT143                                                          
         LA    R2,STMTIDPH                                                      
         CLC   TATMTIDP,TATMWTNT   INT. TRVL DEPART MUST = WORK END             
         BE    BLDT141                                                          
         TM    TGFASTAT,TGFROMFA   INVALID INTERVENING TRAVEL FROM              
         BZ    INVTRIDP                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVTID),D#TMTID                              
BLDT141  LA    R2,STMTFDPH                                                      
         CLC   TATMTIAR,TATMTFDP   TRVL FROM DEP MUST = INT. TRVL ARR           
         BE    BLDT145                                                          
         TM    TGFASTAT,TGFROMFA   INVALID TRAVEL FROM DEPART                   
         BZ    INVTRFDP                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVTFD),0                                    
         B     BLDT145                                                          
BLDT143  LA    R2,STMTFDPH                                                      
         OC    TATMTFDP,TATMTFDP   TRAVEL FROM HOURS?                           
         BZ    BLDT145                                                          
         CLC   TATMTFDP,TATMWTNT   TRVL FROM DEPART MUST = WORK END             
         BE    BLDT145             INVALID TRAVEL FROM DEPART                   
         TM    TGFASTAT,TGFROMFA   INVALID INTERVENING TRAVEL FROM              
         BZ    INVTRIDP                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVTID),D#TMTFD                              
*                                                                               
BLDT145  TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   BLDT150                                                          
         MVC   HRMIN1,WORKHRS      ADD WORK HOURS AND MEAL HOURS                
         MVC   HRMIN2,MEALHRS                                                   
         BRAS  RE,ADDHRS                                                        
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,TRVLHRS2     ADD TRAVEL FROM HOURS UNROUNDED              
         BRAS  RE,ADDHRS                                                        
         XC    SMK16HR,SMK16HR                                                  
         LH    R3,DIFFTHRS                                                      
         CHI   R3,1600             CHECK FOR 16 HOUR RULE                       
         BNH   BLDT150                                                          
         SHI   R3,1600                                                          
         BRAS  RE,RNDHOUR          ROUND UP NEXT HOUR                           
         BRAS  RE,VAL16HR          VALIDATE 16 HOUR RULE                        
                                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
BLDT150  BRAS  RE,PREMEND          FIND NIGHT PREMIUM END TIME                  
         BRAS  RE,CALCPML          CALCULATE PREMIUM MEAL TIMES                 
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   *+8                                                              
         BRAS  RE,PREMTRV          CALC ANY PREMIUM TRAVEL FROM HOURS           
*                                                                               
         BAS   RE,DYOTDT           CALC DAYS, OVERTIME AND DOUBLE TIME          
         BRAS  RE,ADJTRMD          ADJUST TRAVEL HOURS AFTER MIDNIGHT           
*                                                                               
         OC    TRAVDED,TRAVDED     ANY MINS TO DEDUCT FROM TRAVEL?              
         BZ    BLDT160                                                          
         OC    TRVLHRS,TRVLHRS     ANY TRAVEL TIME?                             
         BZ    BLDT160                                                          
         MVC   DIFFTHRS,SVTRVL     CONVERT TRAVEL HRS TO MINS                   
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         LH    R0,TRAVDED          IF TRAVDED > TRAVEL TIME                     
         CR    R0,R1                                                            
         BNH   BLDT155                                                          
         XC    SVTRVL,SVTRVL       DEDUCT ALL TRAVEL TIME                       
         B     BLDT160                                                          
BLDT155  SH    R1,TRAVDED          SUBTRACT TRAVEL DEDUCTION FROM TRVL          
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT REMAINING TRVL TIME TO HRS           
         BRAS  RE,RNDQRTR          ROUND TO THE NEAREST QUARTER HOUR            
         MVC   TRVLHRS,DIFFTHRS                                                 
         MVC   SVTRVL,DIFFTHRS     NEW TRAVEL TIME                              
*                                                                               
BLDT160  OC    EXTRVL,EXTRVL       ANY EXTRA TRAVEL HOURS?                      
         BZ    BLDT170                                                          
         OC    TRVLHRS,TRVLHRS     IF NOT PAYING TRVL, DON'T CARE               
         BZ    BLDT165                                                          
         MVC   HRMIN1,EXTRVL                                                    
         MVC   HRMIN2,TRVLHRS      ADD EXTRA TRAVEL HOURS                       
         BRAS  RE,ADDHRS                                                        
         BRAS  RE,RNDQRTR                                                       
         MVC   TRVLHRS,DIFFTHRS                                                 
         MVC   SVTRVL,DIFFTHRS     NEW TRAVEL TIME                              
*                                                                               
BLDT165  MVC   HRMIN1,EXTRVL                                                    
         MVC   HRMIN2,WCXLTRVL     ADD TO WEATHER CXL TRVL HRS                  
         BRAS  RE,ADDHRS                                                        
         BRAS  RE,RNDQRTR                                                       
         MVC   WCXLTRVL,DIFFTHRS   NEW TRAVEL TIME                              
*                                                                               
BLDT170  BRAS  RE,WTHRCXL          VALIDATE WEATHER CANCELLATION                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
         MVI   NCONFLAG,0                                                       
         TM    TGCSORT,X'20'       IF CAST MEMBER IS A PRINCIPAL,               
         BO    BLDT180                                                          
         BRAS  RE,VALNCON          VALIDATE NON CONSECUTIVE WORK DAYS           
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
         BRAS  RE,GETRPV           SEE IF ANY REST PERIOD VIOLATIONS            
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
         BRAS  RE,VALRHSL          VALIDATE REHEARSAL DAY                       
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
*                                                                               
BLDT180  BRAS  RE,DISTLOC          VALIDATE DISTANT LOCATION                    
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         JE    XIT                 A WEB TRANSACTION, ABORT NOW                 
                                                                                
         BRAS  RE,NGHTPREM         CALCULATE NIGHT PREMIUM                      
*                                                                               
         MVC   SVSTAT2,TATMSTA2    SAVE 2ND STATUS                              
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE VALIDATES REIMBURSEMENTS                                       
*        R4 ---> ELEM                                                           
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
VALREIM  NTR1                                                                   
         LA    R2,STMOTHRH        OTHER MISC. REIMBURSED EXPENSES               
         CLI   5(R2),0                                                          
         BE    VREIM30                                                          
         MVI   WBERRFLD,D#TMOWA                                                 
         CLI   8(R2),C'-'         NEGATIVES ARE NOT ALLOWED                     
         BE    FLDINV                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(X'80',(R3))   VALIDATE OTHER AMOUNT          
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         MVC   TATMOTHR,4(R1)     SAVE OTHER AMOUNT                             
         ZICM  R1,SVREIM,4                                                      
         ZICM  RE,TATMOTHR,4                                                    
         AR    R1,RE              ADD TO REIMBURSED EXPENSES                    
         STCM  R1,15,SVREIM                                                     
         MVI   SVINCL,4           USE INCLUDE CODE FOR OTHER                    
*                                                                               
VREIM30  LA    R2,STMWANEH        WARDROBE - NON EVENING                        
         CLI   5(R2),0                                                          
         BE    VREIM40                                                          
         MVI   WBERRFLD,D#TMNWA                                                 
         CLI   ACTRULES,C'Y'      IF USING ACTRA RULES,                         
         BE    FLDINV             WARDROBE IS NOT ALLOWED                       
         MVI   SVINCL,1           ALWAYS USE LOWEST INCLUDE CODE                
         TM    TGCSORT,X'08'      IF CAST MEMBER OFF CAMERA,                    
         BZ    VREIM31            INPUT IS NOT ALLOWED                          
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMNWA                              
         B     VREIM40                                                          
VREIM31  GOTO1 VALINUM                                                          
         MVC   TATMWANE,ACTUAL                                                  
*                                                                               
         ZIC   RE,TATMWANE        NUMBER OF NON-EVENING WARDROBES               
         LA    R1,NONEVNRT        NON-EVENING RATE                              
         CLC   TGCAT,=C'EXD'      PRECISION DRIVERS GET PRINCIPAL RATE          
         BE    VREIM35                                                          
         TM    TGCSORT,X'20'      IS CAST MEMBER AN EXTRA?                      
         BNO   *+8                                                              
         AHI   R1,2               BUMP TO EXTRA RATE                            
VREIM35  MH    RE,0(R1)           MULTIPLY NUMBER OF WARDROBES BY RATE          
*                                                                               
         ZICM  R1,SVREIM,4                                                      
         AR    R1,RE              ADD TO REIMBURSED EXPENSES                    
         STCM  R1,15,SVREIM                                                     
*                                                                               
VREIM40  LA    R2,STMWAEVH        WARDROBE - EVENING                            
         CLI   5(R2),0                                                          
         BE    VREIMX                                                           
         MVI   WBERRFLD,D#TMEWA                                                 
         CLI   ACTRULES,C'Y'      IF USING ACTRA RULES,                         
         BE    FLDINV             WARDROBE IS NOT ALLOWED                       
         MVI   SVINCL,1           ALWAYS USE LOWEST INCLUDE CODE                
         TM    TGCSORT,X'08'      IF CAST MEMBER OFF CAMERA,                    
         BZ    VREIM41            INPUT IS NOT ALLOWED                          
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMEWA                              
         B     VREIMX                                                           
VREIM41  GOTO1 VALINUM                                                          
         MVC   TATMWAEV,ACTUAL                                                  
*                                                                               
         ZIC   RE,TATMWAEV        NUMBER OF EVENING WARDROBES                   
         LA    R1,EVNRATE         EVENING RATE                                  
         CLC   TGCAT,=C'EXD'      PRECISION DRIVERS GET PRINCIPAL RATES         
         BE    VREIM45                                                          
         TM    TGCSORT,X'20'      IS CAST MEMBER AN EXTRA?                      
         BNO   *+8                                                              
         AHI   R1,2               BUMP TO EXTRA RATE                            
VREIM45  MH    RE,0(R1)           MULTIPLY NUMBER OF WARDROBES BY RATE          
*                                                                               
         ZICM  R1,SVREIM,4                                                      
         AR    R1,RE              ADD TO REIMBURSED EXPENSES                    
         STCM  R1,15,SVREIM                                                     
*                                                                               
VREIMX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
SMOKRATE DC    F'502400'          SMOKE PAY RATE (MULTIPLIED BY 100)            
NONEVNRT DC    H'1921'            WARDROBE - NON-EVENING RATE                   
         DC    H'1921'            EXTRA RATE                                    
EVNRATE  DC    H'3199'            WARDROBE - EVENING RATE                       
         DC    H'3199'            EXTRA RATE                                    
*--------------------------------------------------------------------*          
*              ROUTINE TO CALCULATE DAYS, OVERTIME AND DOUBLE TIME   *          
*              R4 ---> ELEM                                          *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
DYOTDT   NTR1                                                                   
         XC    TRAVDED,TRAVDED                                                  
*                                                                               
         OC    TATMWTST,TATMWTST   IS THIS A TRAVEL DAY? (NO WORK)              
         BNZ   DYOD20                                                           
         MVC   STRTTIME,TATMTTDP                                                
         MVC   ENDTIME,TATMTTAR                                                 
         OC    TATMTTDP,TATMTTDP   TRAVEL TO?                                   
         BNZ   DYOD10                                                           
         MVC   STRTTIME,TATMTFDP                                                
         MVC   ENDTIME,TATMTFAR                                                 
         OC    TATMTFAR,TATMTFAR   TRAVEL FROM?                                 
         BZ    DYOD20                                                           
DYOD10   MVC   SVTRVL,=H'800'      COUNT TRAVEL DAY AS 8 HRS OF TRAVEL          
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   DYOD100                                                          
         CLC   STRTTIME,ENDTIME    IF TRAVEL START < TRAVEL END                 
         BNH   DYOD100             TRVL GOES PAST MIDNIGHT                      
         MVC   SVTRVL,=H'1600'     PAY 2 TRAVEL DAYS                            
         B     DYOD100                                                          
*                                  SET START AND END TIMES                      
DYOD20   MVC   STRTTIME,TATMTTDP                                                
         OC    TATMTTDP,TATMTTDP   TRAVEL TO - DEPART                           
         BNZ   *+10                                                             
         MVC   STRTTIME,TATMWTST   WORK START                                   
         MVC   ENDTIME,TATMTFAR                                                 
         OC    TATMTFAR,TATMTFAR   TRAVEL FROM - ARRIVE                         
         BNZ   *+10                                                             
         MVC   ENDTIME,TATMWTNT    WORK END                                     
         BRAS  RE,TIMDIFF          RETURNS NUMBER OF HOURS START TO END         
*                                                                               
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   DYOD23                                                           
         TM    TATMSTAT,TATMS16Y   AND USING 16HR RULE                          
         BNO   DYOD23                                                           
         CLC   DIFFTHRS,=H'1600'   IF MORE THAN 16 HOURS IN DAY,                
         BNH   DYOD23                                                           
         MVC   DIFFTHRS,=H'1600'   ONLY CALCULATE OVTIME AND DBLTIME            
         MVC   DIFFTMIN,=H'960'    ON 1ST 16 HOURS                              
*                                                                               
DYOD23   LH    R1,DIFFTMIN         # OF MINUTES FOR DAY                         
         LTR   R1,R1                                                            
         BZ    DYOD50                                                           
         MVI   SVDAYS,1                                                         
*                                                                               
         TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BZ    DYOD24                                                           
         MVC   DIFFTHRS,MEALHRS                                                 
         BRAS  RE,HRS2MIN          CONVERT MEAL HRS TO MINS                     
         LH    R0,DIFFTMIN         # OF MINUTES OF MEALS                        
         LTR   R0,R0                                                            
         BZ    DYOD23AC                                                         
         SR    R1,R0               SUBTRACT MEAL TIME                           
         BNP   DYOD100                                                          
DYOD23AC MVC   DIFFTHRS,TRVLHRS                                                 
         BRAS  RE,HRS2MIN          CONVERT TRAVEL HRS TO MINS                   
         LH    R0,DIFFTMIN         # OF MINUTES OF TRAVEL                       
         LTR   R0,R0                                                            
         BZ    DYOD23AA                                                         
DYOD23AB SR    R1,R0               SUBTRACT TRAVEL TIME                         
         BNP   DYOD100                                                          
DYOD23AA ZIC   RE,SVDAYS                                                        
DYOD23A  CHI   R1,120                                                           
         BNH   DYOD23B                                                          
         SHI   R1,120              SUBTRACT 2 HOUR DAY                          
         BP    *+6                                                              
         DC    H'00'                                                            
         AHI   RE,1                                                             
         B     DYOD23A                                                          
DYOD23B  LHI   R0,120                                                           
         SR    R0,R1                                                            
         STH   R0,TRAVDED          SAVE DIFFERENCE IN TRAV DED                  
         STC   RE,SVDAYS                                                        
         B     DYOD100                                                          
*                                  ON CAMERA                                    
DYOD24   SHI   R1,480              SUBTRACT 8 HOUR DAY                          
         BNP   DYOD100             LESS THAN 8 HRS?                             
*                                                                               
         MVC   DIFFTHRS,MEALHRS                                                 
         BRAS  RE,HRS2MIN          CONVERT MEAL HRS TO MINS                     
         LH    R0,DIFFTMIN         # OF MINUTES OF MEALS                        
         LTR   R0,R0                                                            
         BZ    DYOD25                                                           
         SR    R1,R0               SUBTRACT MEAL TIME                           
         BNP   DYOD100                                                          
*                                                                               
DYOD25   TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA,                  
         BNO   DYOD27                                                           
         BRAS  RE,ADJTR16          IF USING 16HR RULE, ADJUST TRAVEL            
         OC    TRVLHRS,TRVLHRS     HOURS AFTER 1ST 16 HOURS                     
         BZ    DYOD30                                                           
DYOD27   MVC   DIFFTHRS,TRVLHRS                                                 
         BRAS  RE,HRS2MIN          CONVERT TRAVEL HRS TO MINS                   
         LH    R0,DIFFTMIN         # OF MINUTES OF TRAVEL                       
         LTR   R0,R0                                                            
         BZ    DYOD30                                                           
         CR    R1,R0               IF REMAINING TIME < TRAVEL TIME              
         BH    DYOD28                                                           
         SR    R0,R1                                                            
         STCM  R0,3,TRAVDED        DEDUCT DIFFERENCE FROM TRAVEL TIME           
         B     DYOD100                                                          
DYOD28   SR    R1,R0               SUBTRACT TRAVEL TIME                         
         BNP   DYOD100                                                          
*                                                                               
DYOD30   MVI   SVOTIME,1           OVERTIME, 9TH AND 10TH HOURS                 
         CHI   R1,60               IF < 60 MINS LEFT, DEDUCT DIFFERENCE         
         BH    DYOD40              FROM TRAVEL                                  
         LHI   R0,60                                                            
         SR    R0,R1                                                            
         STCM  R0,3,TRAVDED        TRAVEL DEDUCTION                             
         B     DYOD100                                                          
DYOD40   MVI   SVOTIME,2                                                        
DYOD50   SHI   R1,60               SUBTRACT 1ST HOUR OF OVERTIME                
         CHI   R1,60               IF < 60 MINS LEFT, DEDUCT DIFFERENCE         
         BH    DYOD60              FROM TRAVEL                                  
         LHI   R0,60                                                            
         SR    R0,R1                                                            
         STCM  R0,3,TRAVDED        TRAVEL DEDUCTION                             
         B     DYOD100                                                          
DYOD60   SHI   R1,60               SUBTRACT 2ND HOUR OF OVERTIME                
*                                                                               
         LA    R2,1                                                             
         CHI   R1,60               IF < 60 MINS LEFT, DEDUCT DIFFERENCE         
         BNL   DYOD70              FROM TRAVEL, AND DBLTIME HRS = 1             
         LHI   R0,60                                                            
         SR    R0,R1                                                            
         B     DYOD80                                                           
DYOD70   SHI   R1,60               DEDUCT 1 HOUR                                
         LTR   R1,R1                                                            
         BZ    DYOD90                                                           
         CHI   R1,60               CHECK FOR FRACTION                           
         BNL   DYOD72                                                           
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE                           
         BZ    DYOD71                                                           
         SH    R1,SV16RND          DEDUCT MINUTES ROUNDED UP FOR 16HR           
         BNP   DYOD90                                                           
DYOD71   LHI   R0,60                                                            
         SR    R0,R1               GET DIFFERENCE                               
         AHI   R2,1                ADD 1 DOUBLETIME HR FOR FRACTION             
         B     DYOD80                                                           
DYOD72   LA    R2,1(R2)                                                         
         B     DYOD70                                                           
*                                                                               
DYOD80   STCM  R0,3,TRAVDED        DEDUCT EXTRA TIME FROM TRVL (MINS)           
DYOD90   STC   R2,SVDTIME          SAVE DOUBLETIME HOURS                        
*                                                                               
DYOD100  TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   *+8                                                              
         BRAS  RE,CHKFRI           CHECK FOR FRIDAY TO SATURDAY SESSION         
*                                                                               
         TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BZ    DYODX                                                            
         MVI   SVDTIME,0                                                        
*                                                                               
         CLI   SVCOTYPE,CTYADD     IF COMM'L TYPE IS ADDENDUM                   
         BNE   DYOD110                                                          
         CLC   SVCOADST,=C'KS'     AND ADD. STATE IS NOT KANSAS                 
         BE    DYODX                                                            
DYOD110  MVI   SVOTIME,0           ARE NOT ALLOWED IF OFF CAMERA                
*                                                                               
DYODX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*                                                                               
FLDINV   TM    TGFASTAT,TGFROMFA                                                
         BZ    FLDINVX                                                          
         GOTOR ADDGERR,DMCB,('EENINV',0),(WBERRFLD,0)                           
         B     XIT                                                              
FLDINVX  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
RECNTFND TM    TGFASTAT,TGFROMFA                                                
         BZ    RECNTFNX                                                         
         GOTOR ADDWERR,DMCB,=AL2(NOTFOUND),X'FF'                                
         B     XIT                                                              
RECNTFNX MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
RECONFIL MVI   ERROR,RECEXIST      RECORD ALREADY ON FILE                       
         B     THEEND                                                           
RCTOOLNG MVI   ERROR,TOOLONG       RECORD TOO LONG                              
         LH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         AR    R2,RA                                                            
         B     THEEND                                                           
*                                                                               
INVPAID  TM    TGFASTAT,TGFROMFA                                                
         BZ    INVPAIDX                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERRTMINP),0                                    
         B     XIT                                                              
INVPAIDX LA    R2,CONACTH                                                       
         MVC   MYMSGNO,=Y(ERRTMINP) INVOICE ALREADY PAID                        
         B     NTHEEND                                                          
*                                                                               
INVUSED  TM    TGFASTAT,TGFROMFA                                                
         BZ    INVUSEDX                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVUSD),0                                    
         B     XIT                                                              
INVUSEDX MVC   MYMSGNO,=Y(ERINVUSD) INVOICE USED ON DIFF COMMERCIAL             
         B     NTHEEND                                                          
*                                                                               
INVOFFCM MVC   MYMSGNO,=Y(EROFFCAM) INPUT NOT ALLOWED FOR OFF-CAM CAST          
         B     NTHEEND                                                          
*                                                                               
DATEINV  TM    TGFASTAT,TGFROMFA                                                
         BZ    DATEINVX                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERTMDATE),0                                    
         B     XIT                                                              
DATEINVX MVC   MYMSGNO,=Y(ERTMDATE) DATE > 2014, MUST UPDATE HOLIDAYTAB         
         B     NTHEEND                                                          
*                                                                               
INVTRTAR MVC   MYMSGNO,=Y(ERINVTTA) ARRIVAL TIME MUST EQUAL WORK START          
         B     NTHEEND                                                          
*                                                                               
INVTRIDP MVC   MYMSGNO,=Y(ERINVTID) DEPARTURE TIME MUST EQUAL WORK END          
         B     NTHEEND                                                          
*                                                                               
INVTRFDP MVC   MYMSGNO,=Y(ERINVTFD) DEPARTURE MUST EQUAL WORK END OR            
         B     NTHEEND              INTERVENING TRAVEL ARRIVAL                  
*                                                                               
INVMSUNI TM    TGFASTAT,TGFROMFA                                                
         BZ    INVMSUNX                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVUNI),0                                    
         B     XIT                                                              
INVMSUNX MVC   MYMSGNO,=Y(ERINVUNI) CANNOT ADD TIMESHEETS FOR MUSICIANS         
         B     NTHEEND              (UNION AFM)                                 
*                                                                               
INVSPFTR MVC   MYMSGNO,=Y(ERINVSPF) IF SPOTS = 0, NO FTRACK WILL BE             
         B     NTHEEND              ADDED, IF OKAY PRESS PF20                   
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     NTHEEND                                                          
                                                                                
NTHEEND  OI    GENSTAT2,USGETTXT    NEW THEEND FOR TWO BYTE ERROR MSGS          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR        TYPE = ERROR                                
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 EXIT,DMCB,0          (POSSIBLE FROM WEB)                         
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
ALLFF    DC    6X'FF'                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE DISPLAYS TOTALS                                          
*--------------------------------------------------------------------*          
DISPTOT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRSCRN          CLEAR SCREEN                                 
*                                                                               
         L     R4,AIO                                                           
         USING TATTD,R4                                                         
         MVI   ELCODE,TATTELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DTOT10   BRAS  RE,NEXTEL                                                        
         BNE   DTOTX               EOR, NO MATCHING TIMESHEET                   
         CLC   TATTDATE,TMSHDATE                                                
         BE    DTOT20                                                           
         CLC   TATTDATE,=X'FFFFFF' GRAND TOTAL OF ALL TIMESHEETS                
         BE    DTOT30                                                           
         B     DTOT10                                                           
*                                                                               
DTOT20   EDIT  TATTSPOT,STMSPOT,ALIGN=LEFT    SPOTS                             
         EDIT  TATTTAG,STMTAGS,ALIGN=LEFT     TAGS                              
*                                                                               
         EDIT  TATTNSPH,STMMLPN,2,ALIGN=LEFT,ZERO=BLANK   NON SUBJ P&H          
         OI    STMMLPNH+6,X'80'    (MEAL PENALTY)                               
*                                                                               
         EDIT  TATTADJ,STMADJ,2,ALIGN=LEFT,ZERO=BLANK   ADJUSTMENT AMT          
         OI    STMADJH+6,X'80'                                                  
*                                                                               
         MVI   STMHLPY,C'N'                                                     
         TM    TATTSTAT,TATTSTHP   HOLIDAY PAY?                                 
         BZ    *+8                                                              
         MVI   STMHLPY,C'Y'                                                     
         OI    STMHLPYH+6,X'80'                                                 
*                                                                               
         BRAS  RE,SHOWTHIS         SHOW THIS LINE TOTAL                         
         B     DTOT10                                                           
*                                                                               
DTOT30   BRAS  RE,SHOWTOT          SHOW ALL TOTALS                              
*                                                                               
DTOTX    J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE UPDATES TIME AND CALCULATES AMOUNT OF TIME                     
*        ACTRA RATES ONLY                                                       
*        R4 ---> ELEM                                                           
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
BLDACT   NTR1  BASE=*,LABEL=*                                                   
         XC    SVSPOTS(L'SVDATA),SVSPOTS                                        
*                                                                               
         MVC   NPSTART,=H'2300'    ACTRA NIGHT PREMIUM STARTS AT 11PM           
*                                                                               
         GOTOR STENTIME,DMCB,STMWTSTH,STMWTNTH     WORK TIME                    
         MVC   TATMWTST,STRTTIME                   START                        
         MVC   TATMWTNT,ENDTIME                    END                          
         MVC   WORKHRS,DIFFTHRS                                                 
*                                                                               
         MVC   PRMEND,=H'600'      SET NIGHT PREMIUM END TIME                   
         BRAS  RE,VALMEAL          VALIDATE MEAL TIMES                          
*                                                                               
         OC    MEALHRS,MEALHRS     ANY MEALS?                                   
         BZ    BLDA30                                                           
         MVC   DIFFTHRS,WORKHRS    WORK HOURS - CONVERT TO MINUTES              
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALHRS    MEAL HOURS - CONVERT TO MINUTES              
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         SR    R1,R0               DEDUCT MEAL HOURS FROM WORK HOURS            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         MVC   WORKHRS,DIFFTHRS                                                 
*                                                                               
*                                  PRIOR DAY WARDROBE                           
BLDA30   TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BZ    BLDA90              INPUT IS NOT ALLOWED                         
         LA    R2,STMPDDTH                                                      
         CLI   5(R2),0                                                          
         BE    BLDA31                                                           
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMPDD                              
BLDA31   LA    R2,STMPDSTH                                                      
         CLI   5(R2),0                                                          
         BE    BLDA32                                                           
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMPDS                              
BLDA32   LA    R2,STMPDNTH                                                      
         CLI   5(R2),0                                                          
         BE    BLDA90                                                           
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVOFFCM                                                         
         GOTOR ADDWERR,DMCB,=AL2(EROFFCAM),D#TMPDE                              
*                                                                               
BLDA90   GOTOR STENTIME,DMCB,STMPDSTH,STMPDNTH                                  
         BRAS  RE,RNDHR2           ROUND PD-WD, NEAREST HOUR                    
         OC    DIFFTHRS,DIFFTHRS   DO WE HAVE ANY PDWD?                         
         BZ    BLDA120                                                          
         CLC   DIFFTHRS,=H'100'    PAY AT LEAST 1 HOUR                          
         BNL   *+10                                                             
         MVC   DIFFTHRS,=H'100'                                                 
*                                                                               
BLDA120  MVC   SVPDWD,DIFFTHRS                                                  
         XC    PDWDDATE,PDWDDATE   INITIALIZE                                   
         OC    SVPDWD,SVPDWD       DO WE HAVE ANY PDWD?                         
         BZ    BLDA130                                                          
         LA    R2,STMPDDTH          PRIOR DAY WARDROBE DATE                     
         GOTO1 DTVAL,DMCB,PDWDDATE  VALIDATE AND STORE                          
         MVC   TATMPWDT,PDWDDATE                                                
*                                                                               
BLDA130  MVC   TATMPDST,STRTTIME   PRIOR DAY WARDROBE START                     
         MVC   TATMPDNT,ENDTIME    PRIOR DAY WARDROBE END                       
*                                                                               
         SR    R3,R3                                                            
         GOTOR STENTIME,DMCB,STMTTDPH,STMTTARH       TRAVEL TO                  
         LH    R3,DIFFTMIN         TRAVEL TIME                                  
         MVC   TATMTTDP,STRTTIME                                                
         MVC   TATMTTAR,ENDTIME                                                 
*                                                                               
         OC    TATMWTST,TATMWTST   IF THERE IS A WORK START,                    
         BZ    BLDA135                                                          
         OC    TATMTTAR,TATMTTAR                                                
         BZ    BLDA135                                                          
         LA    R2,STMTTARH         TRAVEL TO ARRIVE MUST = WORK START           
         CLC   TATMTTAR,TATMWTST                                                
         BE    BLDA135                                                          
         TM    TGFASTAT,TGFROMFA   INVALID TRAVEL TO ARRIVE                     
         BZ    INVTRTAR                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVTTA),0                                    
*                                                                               
BLDA135  GOTOR STENTIME,DMCB,STMTFDPH,STMTFARH       TRAVEL FROM                
         AH    R3,DIFFTMIN                                                      
         MVC   TATMTFDP,STRTTIME                                                
         MVC   TATMTFAR,ENDTIME                                                 
*                                                                               
         LA    R2,STMTIDPH         NO INTERVENING TRAVEL ALLOWED                
         MVI   WBERRFLD,D#TMTID                                                 
         CLI   5(R2),0                                                          
         BNE   FLDINV2                                                          
         LA    R2,STMTIARH                                                      
         MVI   WBERRFLD,D#TMTIA                                                 
         CLI   5(R2),0                                                          
         BNE   FLDINV2                                                          
*                                                                               
         STH   R3,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDHALF          ROUND TRAVEL, NEAREST HALF HOUR              
         MVC   TRVLHRS,DIFFTHRS                                                 
         MVC   HRMIN1,WORKHRS      ADD WORK HOURS AND TRVL HOURS                
         MVC   HRMIN2,TRVLHRS                                                   
         BRAS  RE,ADDHRS                                                        
         LH    R3,DIFFTHRS                                                      
*&&DO                                                                           
         TM    SVDOBST,MINOR16     MINOR UNDER 16?                              
         BO    BLDA137                                                          
         CLI   SVACTRA,CCTY04A     ACTRA TYPE 2404A?                            
         BNE   BLDA138                                                          
         CLI   FIRSTDY,C'Y'        NOT 1ST DAY OF SESSION?                      
         BE    BLDA138                                                          
*&&                                                                             
*                               ***ALL SESSIONS ARE 8 HOURS DEC/08***           
BLDA137  CHI   R3,800              DON'T PAY TRVL IF TRVL+WORK <= 8 HRS         
         BH    BLDA140                                                          
*                                                                               
*&&DO                                                                           
         B     *+12                                                             
BLDA138  CHI   R3,900              DON'T PAY TRVL IF TRVL+WORK <= 9 HRS         
         BH    BLDA140                                                          
*&&                                                                             
*                                                                               
         XC    TRVLHRS,TRVLHRS                                                  
         SH    R3,TRVLHRS                                                       
BLDA140  MVC   SVTRVL,TRVLHRS                                                   
*                                                                               
         OC    TATMWTNT,TATMWTNT   IF THERE IS A WORK END,                      
         BZ    BLDA150                                                          
         LA    R2,STMTFDPH                                                      
         OC    TATMTFDP,TATMTFDP   TRAVEL FROM HOURS?                           
         BZ    BLDA150                                                          
         CLC   TATMTFDP,TATMWTNT   TRVL FROM DEPART MUST = WORK END             
         BE    BLDA150             INVALID TRAVEL FROM DEPART                   
         TM    TGFASTAT,TGFROMFA                                                
         BZ    INVTRIDP                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERINVTID),D#TMTFD                              
*                                                                               
BLDA150  BRAS  RE,CALCPML          CALCULATE PREMIUM MEAL TIMES                 
         BRAS  RE,DYODACT          CALC DAYS, OVERTIME AND DOUBLE TIME          
*                                                                               
BLDA160  BRAS  RE,NGPRMACT         CALCULATE ACTRA NIGHT PREMIUM                
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
FLDINV2  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 EXIT,DMCB,0         (POSSIBLE FROM WEB)                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO CALCULATE DAYS, OVERTIME AND DOUBLE TIME   *          
*              FOR ACTRA ONLY                                                   
*              R4 ---> ELEM                                          *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
DYODACT  NTR1  BASE=*,LABEL=*                                                   
         XC    TRAVDED,TRAVDED                                                  
*                                                                               
         OC    TATMWTST,TATMWTST   IS THIS A TRAVEL DAY? (NO WORK)              
         BNZ   DYACT20                                                          
         OC    TATMTTDP,TATMTTDP   TRAVEL TO?                                   
         BNZ   DYACT10                                                          
         OC    TATMTFAR,TATMTFAR   TRAVEL FROM?                                 
         BZ    DYACT20                                                          
DYACT10  MVC   SVTRVL,=H'800'      COUNT TRAVEL DAY AS 8 HRS OF TRAVEL          
         B     DYACT120                                                         
*                                  SET START AND END TIMES                      
DYACT20  MVC   STRTTIME,TATMTTDP                                                
         OC    TATMTTDP,TATMTTDP   TRAVEL TO - DEPART                           
         BNZ   *+10                                                             
         MVC   STRTTIME,TATMWTST   WORK START                                   
         MVC   ENDTIME,TATMTFAR                                                 
         OC    TATMTFAR,TATMTFAR   TRAVEL FROM - ARRIVE                         
         BNZ   *+10                                                             
         MVC   ENDTIME,TATMWTNT    WORK END                                     
         BRAS  RE,TIMDIFF          RETURNS NUMBER OF HOURS START TO END         
*                                                                               
         LH    R1,DIFFTMIN         # OF MINUTES FOR DAY                         
         LTR   R1,R1                                                            
         BZ    DYACT50                                                          
         MVI   SVDAYS,1                                                         
*                                                                               
*                               ***ALL SESSIONS ARE 8 HOURS DEC/08***           
*&&DO                                                                           
         TM    SVDOBST,MINOR16     MINOR UNDER 16?                              
         BO    DYACT25                                                          
         CLI   SVACTRA,CCTY04A     ACTRA TYPE 2404A?                            
         BNE   DYACT30                                                          
         CLI   FIRSTDY,C'Y'        NOT 1ST DAY OF SESSION?                      
         BE    DYACT30                                                          
*&&                                                                             
DYACT25  SHI   R1,480              SUBTRACT 8 HOUR DAY                          
         BNP   DYACT120            LESS THAN 8 HRS?                             
         B     DYACT35                                                          
*                                                                               
*&&DO                                                                           
DYACT30  SHI   R1,540              SUBTRACT 9 HOUR DAY (NON 04A RULE)           
         BNP   DYACT120            LESS THAN 9 HRS?                             
*&&                                                                             
*                                                                               
DYACT35  MVC   DIFFTHRS,MEALHRS                                                 
         BRAS  RE,HRS2MIN          CONVERT MEAL HRS TO MINS                     
         LH    R0,DIFFTMIN         # OF MINUTES OF MEALS                        
         LTR   R0,R0                                                            
         BZ    DYACT40                                                          
         SR    R1,R0               SUBTRACT MEAL TIME                           
         BNP   DYACT120                                                         
*                                                                               
DYACT40  MVC   DIFFTHRS,TRVLHRS                                                 
         BRAS  RE,HRS2MIN          CONVERT TRAVEL HRS TO MINS                   
         LH    R0,DIFFTMIN         # ON MINS OF TRAVEL                          
         LTR   R0,R0                                                            
         BZ    DYACT43                                                          
         SR    R1,R0               SUBTRACT TRAVEL TIME                         
         BNP   DYACT120                                                         
*                                                                               
DYACT43  MVI   SVOTIME,1           OVERTIME, 9TH AND 10TH HOURS                 
         CHI   R1,60               ONLY 1 HOUR OF OVERTIME?                     
         BNH   DYACT50                                                          
*                                                                               
*                               ***ALL SESSIONS ARE 8 HOURS DEC/08***           
*&&DO                                                                           
         TM    SVDOBST,MINOR16     MINORS UNDER 16?                             
         BO    DYACT45                                                          
         CLI   SVACTRA,CCTY04A     OR ACTRA TYPE 2404A?                         
         BNE   DYACT50                                                          
         CLI   FIRSTDY,C'Y'        AND NOT 1ST DAY OF SESSION?                  
         BE    DYACT50                                                          
*&&                                                                             
DYACT45  MVI   SVOTIME,2           2 HOURS TIME AND A HALF                      
*                                                                               
DYACT50  SHI   R1,60               SUBTRACT 1ST HOUR OF OVERTIME                
         BNP   DYACT120                                                         
*&&DO                                                                           
         TM    SVDOBST,MINOR16     MINORS UNDER 16?                             
         BO    DYACT70                                                          
         CLI   SVACTRA,CCTY04A     OR ACTRA TYPE 2404A?                         
         BNE   DYACT80                                                          
         CLI   FIRSTDY,C'Y'        AND NOT 1ST DAY OF SESSION?                  
         BE    DYACT80                                                          
*&&                                                                             
DYACT70  SHI   R1,60               SUBTRACT 2ND HOUR OF OVERTIME                
         BNP   DYACT120                                                         
*                                                                               
DYACT80  LA    R2,1                                                             
         CHI   R1,60               IF < 60 MINS LEFT, DEDUCT DIFFERENCE         
         BNL   DYACT90             FROM TRAVEL, AND DBLTIME HRS = 1             
         B     DYACT110                                                         
DYACT90  SHI   R1,60               DEDUCT 1 HOUR                                
         LTR   R1,R1                                                            
         BZ    DYACT110                                                         
         CHI   R1,60               CHECK FOR FRACTION                           
         BNL   DYACT100                                                         
         AHI   R2,1                ADD 1 DOUBLETIME HR FOR FRACTION             
         B     DYACT110                                                         
DYACT100 LA    R2,1(R2)                                                         
         B     DYACT90                                                          
*                                                                               
DYACT110 STC   R2,SVDTIME          SAVE DOUBLETIME HOURS                        
*                                                                               
DYACT120 TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BZ    DYACTX                                                           
         MVI   SVDTIME,0                                                        
*                                                                               
         CLI   SVCOTYPE,CTYADD     IF COMM'L TYPE IS ADDENDUM                   
         BNE   DYACT130                                                         
         CLC   SVCOADST,=C'KS'     AND ADD. STATE IS NOT KANSAS                 
         BE    DYACTX                                                           
DYACT130 MVI   SVOTIME,0           ARE NOT ALLOWED IF OFF CAMERA                
*                                                                               
DYACTX   MVC   AIO,AIO1                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO SHOW THIS LINE                                        
*              R4 ---> THIS TIMESHEET TOTAL ELEMENT                             
*--------------------------------------------------------------------*          
         USING TATTD,R4                                                         
SHOWTHIS NTR1  BASE=*,LABEL=*                                                   
         EDIT  TATTSPOT,STMTHSP,ALIGN=LEFT,ZERO=BLANK                           
         TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BNO   *+12                                                             
         MVI   STMTHDY,C'1'        ONLY COUNT AS 1 DAY                          
         B     STHIS10                                                          
*                                                                               
         EDIT  TATTDAYS,STMTHDY,ALIGN=LEFT,ZERO=BLANK                           
STHIS10  EDIT  TATTOVTM,STMTHOT,ALIGN=LEFT,ZERO=BLANK                           
         EDIT  TATTDBTM,STMTHDT,ALIGN=LEFT,ZERO=BLANK                           
         EDIT  TATTTRVL,STMTHTR,2,ALIGN=LEFT,ZERO=BLANK                         
         EDIT  TATTPDWD,STMTHPW,2,ALIGN=LEFT,ZERO=BLANK                         
         EDIT  TATTTAG,STMTHT,ALIGN=LEFT,ZERO=BLANK                             
         EDIT  TATTREIM,STMTHRE,2,ALIGN=LEFT,ZERO=BLANK                         
*                                                                               
         OI    STMTHSPH+6,X'80'                                                 
         OI    STMTHDYH+6,X'80'                                                 
         OI    STMTHOTH+6,X'80'                                                 
         OI    STMTHDTH+6,X'80'                                                 
         OI    STMTHTRH+6,X'80'                                                 
         OI    STMTHPWH+6,X'80'                                                 
         OI    STMTHTH+6,X'80'                                                  
         OI    STMTHREH+6,X'80'                                                 
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO SHOW TOTAL LINE                                       
*              R4 ---> TIMESHEET TOTALS ELEMENT                                 
*--------------------------------------------------------------------*          
         USING TATTD,R4                                                         
SHOWTOT  NTR1  BASE=*,LABEL=*                                                   
         EDIT  TATTSPOT,STMALSP,ALIGN=LEFT,ZERO=BLANK                           
         EDIT  TATTDAYS,STMALDY,ALIGN=LEFT,ZERO=BLANK                           
         EDIT  TATTOVTM,STMALOT,ALIGN=LEFT,ZERO=BLANK                           
         EDIT  TATTDBTM,STMALDT,ALIGN=LEFT,ZERO=BLANK                           
         EDIT  TATTTRVL,STMALTR,2,ALIGN=LEFT,ZERO=BLANK                         
         EDIT  TATTPDWD,STMALPW,2,ALIGN=LEFT,ZERO=BLANK                         
         EDIT  TATTTAG,STMALT,ALIGN=LEFT,ZERO=BLANK                             
         EDIT  TATTREIM,STMALRE,2,ALIGN=LEFT,ZERO=BLANK                         
*                                                                               
         OI    STMALSPH+6,X'80'                                                 
         OI    STMALDYH+6,X'80'                                                 
         OI    STMALOTH+6,X'80'                                                 
         OI    STMALDTH+6,X'80'                                                 
         OI    STMALTRH+6,X'80'                                                 
         OI    STMALPWH+6,X'80'                                                 
         OI    STMALTH+6,X'80'                                                  
         OI    STMALREH+6,X'80'                                                 
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE CHECKS IF ANY OTHER TIMESHEET DAY HAS SPOTS                    
*--------------------------------------------------------------------*          
GETSPOT  NTR1  BASE=*,LABEL=*                                                   
         MVI   SPOTFLG,0              DEFAULT = NO SPOTS                        
         L     R4,AIO                                                           
         MVI   ELCODE,TATTELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GSPT10   BRAS  RE,NEXTEL                                                        
         BNE   GSPTX                                                            
*                                                                               
         USING TATTD,R4                                                         
         CLC   TATTDATE,=X'FFFFF0'    DON'T GET TOTALS                          
         BNL   GSPTX                                                            
         CLC   TATTDATE,TMSHDATE      GET ALL DATES EXCEPT THIS ONE             
         BE    GSPT10                                                           
         CLI   TATTSPOT,0                                                       
         BE    GSPT10                                                           
         MVI   SPOTFLG,C'Y'           SET SPOT FLAG                             
*                                                                               
GSPTX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE CHECKS IF FIRST TIMESHEET DAY WAS ALREADY ADDED                
*        IF ADDING A NEW DATE, MUST BE LATER THAN 1ST SESSION DATE              
*        FOR ACTRA TYPE 2404A ONLY                                              
*        SETS FIRSTDY = C'Y' IF FIRST DAY OF 2404A ACTRA SESSION                
*--------------------------------------------------------------------*          
GET1STD  NTR1  BASE=*,LABEL=*                                                   
         MVI   FIRSTDY,0                                                        
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TATMELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    G1ST10                                                           
         MVI   FIRSTDY,C'Y'           SET 1ST DAY OF SESSION STATUS             
         B     G1STYES                                                          
*                                                                               
         USING TATMD,R4                                                         
G1ST10   CLC   TMSHDATE,TATMDATE      NEW DATE CANNOT BE EARLIER                
         BL    G1STNO                 THAN FIRST DAY OF SESSION                 
         BH    G1STYES                                                          
         MVI   FIRSTDY,C'Y'           IF EQUAL, SET AS 1ST DAY                  
*                                                                               
G1STYES  XR    RC,RC                                                            
G1STNO   LTR   RC,RC                                                            
G1STX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE CHECKS IF THERE COULD BE A REST PERIOD VIOLATION               
*        IF ADDING A NEW DATE, IF WORK/TRVL START TIME IS LESS THAN 12          
*        HOURS AFTER WORK/TRVL END TIME FROM DAY BEFORE, GIVE MESSAGE           
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
GETRPV   NTR1  BASE=*,LABEL=*                                                   
         MVI   SVRPVL,0                                                         
         LA    R2,STMRPVLH                                                      
         CLI   5(R2),0               ANYTHING IN THE FIELD?                     
         BE    GRPV20                                                           
         CLI   8(R2),C'N'            IF NO, DON'T BOTHER CALCULATING            
         BNE   GRPV10                                                           
         OI    TATMSTA3,TATMSRPN     NO REST PERIOD VIOLATION                   
         B     GRPVX                                                            
GRPV10   CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         MVI   WBERRFLD,D#TMRPV                                                 
         TM    TGCSORT,X'08'         MUST BE ON CAMERA                          
         BO    FLDINV                                                           
         TM    TATMSTA2,TATMS2NC     IF NON-CONSECUTIVE DAY, NO RPV             
         BNZ   FLDINV                                                           
         OI    TATMSTA3,TATMSRPY     REST PERIOD VIOLATION                      
         MVI   SVRPVL,1              ADD 1 REST PERIOD VIOLATION                
         B     GRPVX                                                            
         DROP  R4                                                               
*                                                                               
GRPV20   GOTO1 ADDAY,DMCB,TMSEDATE,WORK,-1      GET YESTERDAY'S DATE            
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TATMELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GRPV30   BRAS  RE,NEXTEL                                                        
         BNE   GRPVX                                                            
*                                                                               
         USING TATMD,R4                                                         
         CLC   TATMDATE,WORK+6        LOOK FOR YESTERDAY'S DATE                 
         BNE   GRPV30                                                           
         MVC   STRTTIME,TATMTFAR                                                
         OC    TATMTFAR,TATMTFAR      SAVE YESTERDAY'S TRVL FROM ARRIVE         
         BNZ   GRPV40                                                           
         OC    TATMWTNT,TATMWTNT                                                
         BZ    GRPVX                                                            
         MVC   STRTTIME,TATMWTNT      SAVE YESTERDAY'S WORK END TIME            
         DROP  R4                                                               
*                                                                               
GRPV40   LA    R4,ELEM                TODAY'S ELEMENT                           
         USING TATMD,R4                                                         
         MVC   ENDTIME,TATMTTDP       TRAVEL TO DEPART                          
         OC    TATMTTDP,TATMTTDP                                                
         BNZ   *+10                                                             
         MVC   ENDTIME,TATMWTST       WORK START TIME                           
         OC    ENDTIME,ENDTIME        IF NO WORK/TRVL ENTERED, NO RPV           
         BZ    GRPVX                                                            
         BRAS  RE,TIMDIFF             HOW MUCH TIME IN BETWEEN?                 
         CLC   DIFFTHRS,=H'1200'      IF LESS THAN 12 HOURS,                    
         BNL   GRPVX                                                            
         TM    TGFASTAT,TGFROMFA      SEND WARNING MESSAGE                      
         BZ    YNRPVMSG                                                         
         GOTOR ADDWERR,DMCB,=AL2(ERRPVLYN),0                                    
*                                                                               
GRPVX    XIT1                                                                   
         DROP  R4                                                               
*                                                                               
YNRPVMSG MVC   MYMSGNO,=Y(ERRPVLYN) ENTER Y/N FOR REST PERIOD VIOLATION         
         OI    GENSTAT2,USGETTXT    NEW THEEND FOR TWO BYTE ERROR MSGS          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         GOTO1 EXIT,DMCB,0          (WEB COVERED BY ADDWERR CALL ABOVE)         
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE GETS LOWEST INCLUDE CODE FOR TOTALS ELEMENT                    
*--------------------------------------------------------------------*          
GETINCL  NTR1  BASE=*,LABEL=*                                                   
         MVI   MYINCL,0                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATTELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GINCL10  BRAS  RE,NEXTEL                                                        
         BNE   GINCLX                                                           
*                                                                               
         USING TATTD,R4                                                         
         CLC   TATTDATE,=X'FFFFF0'    DON'T GET TOTALS                          
         BNL   GINCLX                                                           
         CLI   TATTINCL,0             SKIP IF INCLUDE CODE = 0                  
         BE    GINCL10                                                          
         CLI   MYINCL,0               IF MYINCL=0, MOVE IN NEW CODE             
         BE    GINCL20                                                          
         CLC   TATTINCL,MYINCL        IF MYINCL DOES NOT = 0,                   
         BH    GINCL10                TAKE NEW CODE ONLY IF LOWER               
GINCL20  MVC   MYINCL,TATTINCL                                                  
         B     GINCL10                                                          
*                                                                               
GINCLX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO BUILD GRAND TOTAL AND SUBTOTALS ELEMENTS              
*--------------------------------------------------------------------*          
BLDTOTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,GETINCL          GET LOWEST INCLUDE CODE                      
*                                                                               
         L     R4,AIO                                                           
         USING TATTD,R4                                                         
         MVI   ELCODE,TATTELQ      TIMESHEET TOTAL ELEMENT                      
         BRAS  RE,GETEL            LOOK FOR GRAND TOTAL                         
         B     *+8                                                              
BTOT10   BRAS  RE,NEXTEL                                                        
         BNE   BTOT20              DON'T HAVE IT, JUST ADD                      
         CLC   TATTDATE,=X'FFFFFF' GRAND TOTAL                                  
         BNE   BTOT10                                                           
*                                                                               
         BRAS  RE,UPDTOTS          UPDATE TIMESHEET TOTAL ELEMENT               
         B     BTOT30              GRAND TOTAL                                  
*                                                                               
BTOT20   LA    R4,ELEM                                                          
         MVC   TATTDATE,=X'FFFFFF'                                              
         TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BNO   BTOT25                                                           
         MVC   BYTE,TATTDAYS                                                    
         CLC   TATTDAYS,=XL1'01'                                                
         BL    *+10                                                             
         MVC   TATTDAYS,=XL1'01'                                                
BTOT25   GOTO1 ADDELEM                                                          
         TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BNO   BTOT30                                                           
         MVC   TATTDAYS,BYTE       RESTORE DAYS FOR SUBTOTAL ELEM               
*                                                                               
BTOT30   L     R4,AIO                                                           
         USING TATTD,R4                                                         
         MVI   ELCODE,TATTELQ      TIMESHEET TOTAL ELEMENT                      
         BRAS  RE,GETEL            LOOK FOR SUBTOTAL                            
         B     *+8                                                              
BTOT40   BRAS  RE,NEXTEL                                                        
         BNE   BTOT90              DON'T HAVE IT, JUST ADD                      
         OC    SVACTRA,SVACTRA     ACTRA RATES?                                 
         BZ    BTOT45                                                           
         CLI   US2404,C'Y'         2404 US CAST USES SAG RATES/RULES            
         BE    BTOT45                                                           
         CLI   SVACTRA,CCTY04B     2404B USES SAG RATES                         
         BNE   BTOT73                                                           
BTOT45   TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF REGULAR DAY                    
         BNZ   BTOT50                                                           
         TM    SVWTRST,SVWTRHF+SVWTR3Q   WTHR CXL 1/2 OR 3/4?                   
         BZ    BTOT48                                                           
         TM    SVWTRST,SVWTRHF     WEATHER CXL 1/2?                             
         BZ    BTOT46                                                           
         CLC   TATTDATE,=X'FFFFF8' LOOK FOR REGULAR DAY/WCXL 1/2 TOT            
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT46   CLC   TATTDATE,=X'FFFFF4' LOOK FOR REGULAR DAY/WCXL 3/4 TOT            
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT48   CLC   TATTDATE,=X'FFFFFE' LOOK FOR REGULAR DAYS TOTAL                  
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT50   TM    DAYTYPE,DAYHLDY     IF HOLIDAY                                   
         BNO   BTOT60                                                           
         TM    SVWTRST,SVWTRHF+SVWTR3Q   WTHR CXL 1/2 OR 3/4?                   
         BZ    BTOT58                                                           
         TM    SVWTRST,SVWTRHF     WEATHER CXL 1/2?                             
         BZ    BTOT56                                                           
         CLC   TATTDATE,=X'FFFFF7' LOOK FOR HOLIDAY/WCXL 1/2 TOT                
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT56   CLC   TATTDATE,=X'FFFFF3' LOOK FOR HOLIDAY/WCXL 3/4 TOT                
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT58   CLI   NCONFLAG,C'Y'       NON-CONSECUTIVE?                             
         BNE   BTOT59                                                           
         CLC   TATTDATE,=X'FFFFF0' LOOK FOR HOLIDAY/NON-CONSECUTIVE TOT         
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT59   CLC   TATTDATE,=X'FFFFFD' LOOK FOR HOLIDAY TOTAL                       
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT60   TM    DAYTYPE,DAYSAT      IF SATURDAY                                  
         BNO   BTOT70                                                           
         TM    SVWTRST,SVWTRHF+SVWTR3Q   WTHR CXL 1/2 OR 3/4?                   
         BZ    BTOT65                                                           
         TM    SVWTRST,SVWTRHF     WTHR CXL 1/2?                                
         BZ    BTOT63                                                           
         CLC   TATTDATE,=X'FFFFF6' LOOK FOR SATURDAY/WCXL 1/2 TOT               
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT63   CLC   TATTDATE,=X'FFFFF2' LOOK FOR SATURDAY/WCXL 3/4 TOT               
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT65   CLI   NCONFLAG,C'Y'       NON-CONSECUTIVE?                             
         BE    BTOT66                                                           
         TM    SVSTAT2,TATMS2DL    DISTANT LOCATION?                            
         BNO   BTOT68                                                           
BTOT66   CLC   TATTDATE,=X'FFFFF0' LOOK FOR SATURDAY/DISTANT LOC TOT            
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT68   CLC   TATTDATE,=X'FFFFFC' LOOK FOR SATURDAY TOTAL                      
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT70   TM    DAYTYPE,DAYSUN      IF SUNDAY                                    
         BO    *+6                                                              
         DC    H'00'                                                            
         TM    SVWTRST,SVWTRHF+SVWTR3Q   WTHR CXL 1/2 OR 3/4?                   
         BZ    BTOT72                                                           
         TM    SVWTRST,SVWTRHF     WEATHER CXL 1/2?                             
         BZ    BTOT71                                                           
         CLC   TATTDATE,=X'FFFFF5' LOOK FOR SUNDAY/WCXL 1/2 TOT                 
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT71   CLC   TATTDATE,=X'FFFFF1' LOOK FOR SUNDAY/WCXL 3/4 TOT                 
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT72   CLI   NCONFLAG,C'Y'       NON-CONSECUTIVE?                             
         BNE   BTOT72A                                                          
         CLC   TATTDATE,=X'FFFFF0' LOOK FOR SUNDAY/NON-CONSECUTIVE TOT          
         BNE   BTOT40                                                           
         B     BTOT80                                                           
BTOT72A  CLC   TATTDATE,=X'FFFFFB' LOOK FOR SUNDAY TOTAL                        
         BNE   BTOT40                                                           
         B     BTOT80                                                           
*                                                                               
BTOT73   CLI   SVACTRA,CCTY04A     2404A?  ACTRA/SAG COMBINED                   
         BNE   BTOT75                                                           
         CLI   FIRSTDY,C'Y'                                                     
         BE    BTOT75                                                           
         CLC   TATTDATE,=X'FFFFFA' LOOK FOR SAG TOTAL                           
         BNE   BTOT40                                                           
         B     BTOT80                                                           
*                                                                               
BTOT75   CLC   TATTDATE,=X'FFFFF9' LOOK FOR ACTRA TOTAL                         
         BNE   BTOT40                                                           
*                                                                               
BTOT80   BRAS  RE,CHKSTAT          CHECK IF STATUS WAS CHANGED,                 
         BNE   BTOT85                                                           
         BRAS  RE,ADDTOTS          ADD AMOUNTS TO NEW TOTALS                    
         BRAS  RE,RTELSUB          RETURN A(ELEMENT) TO SUBTRACT TOTALS         
         BRAS  RE,SUBTOTS          SUBTRACT AMOUNTS FROM OLD TOTALS             
         B     BTOTX                                                            
*                                                                               
BTOT85   BRAS  RE,UPDTOTS          UPDATE TIMESHEET TOTAL ELEMENT               
         B     BTOTX               SUBTOTAL - REG, HOLIDAY, OR SAT/SUN          
*                                                                               
BTOT90   LA    R4,ELEM             ADD SUBTOTAL ELEMENT                         
         OC    SVACTRA,SVACTRA     ACTRA RATES?                                 
         BZ    BTOT95                                                           
         CLI   SVACTRA,CCTY04B     2404B USES SAG RATES                         
         BE    BTOT95                                                           
         CLI   US2404,C'Y'         2404 US CAST USES SAG RATES/RULES            
         BE    BTOT95                                                           
BTOT91   MVC   TATTDATE,=X'FFFFF9' ACTRA TOTAL                                  
         CLI   SVACTRA,CCTY04A     2404A?                                       
         BNE   BTOT100                                                          
         CLI   FIRSTDY,C'Y'        FIRST DAY?                                   
         BNE   BTOT93                                                           
         OI    TATTSTAT,TATTSART+TATTSSRT  1ST DAY = ACTRA/SAG RATES            
         B     BTOT100                                                          
BTOT93   MVC   TATTDATE,=X'FFFFFA' SAG TOTAL (2404A NOT 1ST DAY)                
         B     BTOT100                                                          
*                                                                               
BTOT95   MVC   TATTDATE,=X'FFFFFE'      REGULAR DAY SUBTOTAL                    
         TM    SVWTRST,SVWTRHF          WTHR CXL 1/2?                           
         BZ    *+10                                                             
         MVC   TATTDATE,=X'FFFFF8'      REGULAR DAY/WCXL 1/2                    
         TM    SVWTRST,SVWTR3Q          WTHR CXL 3/4?                           
         BZ    *+10                                                             
         MVC   TATTDATE,=X'FFFFF4'      REGULAR DAY/WCXL 3/4                    
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN                                    
         BZ    BTOT100                                                          
         MVC   TATTDATE,=X'FFFFFD'      HOLIDAY SUBTOTAL                        
         TM    SVWTRST,SVWTRHF          WTHR CXL 1/2?                           
         BZ    *+10                                                             
         MVC   TATTDATE,=X'FFFFF7'      HOLIDAY/WCXL 1/2                        
         TM    SVWTRST,SVWTR3Q          WTHR CXL 3/4?                           
         BZ    *+10                                                             
         MVC   TATTDATE,=X'FFFFF3'      HOLIDAY/WCXL 3/4                        
         CLI   NCONFLAG,C'Y'            NON-CONSECUTIVE?                        
         BNE   *+10                                                             
         MVC   TATTDATE,=X'FFFFF0'      HOLIDAY/NON-CON                         
         TM    DAYTYPE,DAYHLDY                                                  
         BO    BTOT100                                                          
         MVC   TATTDATE,=X'FFFFFC'      SATURDAY SUBTOTAL                       
         TM    SVWTRST,SVWTRHF          WTHR CXL 1/2?                           
         BZ    *+10                                                             
         MVC   TATTDATE,=X'FFFFF6'      SATURDAY/WCXL 1/2                       
         TM    SVWTRST,SVWTR3Q          WTHR CXL 3/4?                           
         BZ    BTOT97                                                           
         MVC   TATTDATE,=X'FFFFF2'      SATURDAY/WCXL 3/4                       
         B     BTOT98                                                           
BTOT97   CLI   NCONFLAG,C'Y'            NON-CONSECUTIVE?                        
         BE    BTOT97A                                                          
         TM    SVSTAT2,TATMS2DL         DISTANT LOCATION?                       
         BNO   *+10                                                             
BTOT97A  MVC   TATTDATE,=X'FFFFF0'      SATURDAY/DISTANT LOCATION               
BTOT98   TM    DAYTYPE,DAYSAT                                                   
         BO    BTOT100                                                          
         MVC   TATTDATE,=X'FFFFFB'      SUNDAY SUBTOTAL                         
         TM    SVWTRST,SVWTRHF          WTHR CXL 1/2?                           
         BZ    *+10                                                             
         MVC   TATTDATE,=X'FFFFF5'      SUNDAY/WCXL 1/2                         
         TM    SVWTRST,SVWTR3Q          WTHR CXL 3/4?                           
         BZ    *+10                                                             
         MVC   TATTDATE,=X'FFFFF1'      SUNDAY/WCXL 3/4                         
         CLI   NCONFLAG,C'Y'            NON-CONSECUTIVE?                        
         BNE   *+10                                                             
         MVC   TATTDATE,=X'FFFFF0'      SUNDAY/NON-CON                          
         TM    DAYTYPE,DAYSUN                                                   
         BO    *+6                                                              
         DC    H'00'                                                            
BTOT100  CLI   ACTNUM,ACTADD                                                    
         BE    BTOT110                                                          
         BRAS  RE,CHKSTAT          CHECK IF STATUS WAS CHANGED,                 
         BNE   BTOT110                                                          
         BRAS  RE,RTELSUB          RETURN A(ELEMENT) TO SUBTRSCT TOTALS         
         BRAS  RE,SUBTOTS          SUBTRACT AMOUNTS FROM OLD TOTALS             
BTOT110  GOTO1 ADDELEM                                                          
*                                                                               
BTOTX    XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE UPDATES TIMESHEET TOTALS ELEMENT                               
*--------------------------------------------------------------------*          
         USING TATTD,R4                                                         
UPDTOTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
OLDTT    USING TATTD,RF                                                         
NEWTT    USING TATTD,R2                                                         
*                                                                               
         LA    RF,OLDTTEL                                                       
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         LA    R2,ELEM                                                          
*                                                                               
         IC    R1,TATTSPOT         SPOT                                         
         IC    RE,OLDTT.TATTSPOT                                                
         SR    R1,RE                                                            
         IC    RE,NEWTT.TATTSPOT                                                
         AR    R1,RE                                                            
         STC   R1,TATTSPOT                                                      
*                                                                               
         IC    R1,TATTDAYS         DAYS                                         
         TM    TGCSORT,X'08'       IF CAST MEMBER OFF CAMERA,                   
         BNO   UPDTOT10                                                         
         CLC   TATTDATE,=X'FFFFFF' GRAND TOTAL?                                 
         BL    UPDTOT10                                                         
         IC    RE,OLDTT.TATTDAYS                                                
         CHI   RE,1                COUNT MULTIPLE DAYS AS 1 DAY                 
         BL    *+8                                                              
         LHI   RE,1                                                             
         SR    R1,RE                                                            
         IC    RE,NEWTT.TATTDAYS                                                
         CHI   RE,1                COUNT MULTIPLE DAYS AS 1 DAY                 
         BL    *+8                                                              
         LHI   RE,1                                                             
         B     UPDTOT20                                                         
                                                                                
UPDTOT10 IC    RE,OLDTT.TATTDAYS                                                
         SR    R1,RE                                                            
         IC    RE,NEWTT.TATTDAYS                                                
UPDTOT20 AR    R1,RE                                                            
         STC   R1,TATTDAYS                                                      
*                                                                               
         IC    R1,TATTOVTM         OVERTIME                                     
         IC    RE,OLDTT.TATTOVTM                                                
         SR    R1,RE                                                            
         IC    RE,NEWTT.TATTOVTM                                                
         AR    R1,RE                                                            
         STC   R1,TATTOVTM                                                      
*                                                                               
         IC    R1,TATTDBTM         DOUBLE TIME                                  
         IC    RE,OLDTT.TATTDBTM                                                
         SR    R1,RE                                                            
         IC    RE,NEWTT.TATTDBTM                                                
         AR    R1,RE                                                            
         STC   R1,TATTDBTM                                                      
*                                                                               
         IC    R1,TATTTAG          TAG                                          
         IC    RE,OLDTT.TATTTAG                                                 
         SR    R1,RE                                                            
         IC    RE,NEWTT.TATTTAG                                                 
         AR    R1,RE                                                            
         STC   R1,TATTTAG                                                       
*                                                                               
         MVC   DIFFTHRS,TATTTRVL   TRAVEL                                       
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         SR    R0,R0                                                            
         MVC   DIFFTHRS,OLDTT.TATTTRVL                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         SR    R1,R0                                                            
         MVC   DIFFTHRS,NEWTT.TATTTRVL                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         AR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT BACK TO HOURS                        
         MVC   TATTTRVL,DIFFTHRS                                                
*                                                                               
         MVC   DIFFTHRS,TATTPDWD   PRIOR DAY WARDROBE                           
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,OLDTT.TATTPDWD                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         SR    R1,R0                                                            
         MVC   DIFFTHRS,NEWTT.TATTPDWD                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         AR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT BACK TO HOURS                        
         MVC   TATTPDWD,DIFFTHRS                                                
*                                                                               
         ICM   R1,3,TATTNP10       NIGHT PREMIUM HOURS AT 10%                   
         ICM   RE,3,OLDTT.TATTNP10                                              
         SR    R1,RE                                                            
         ICM   RE,3,NEWTT.TATTNP10                                              
         AR    R1,RE                                                            
         STCM  R1,3,TATTNP10                                                    
*                                                                               
         ICM   R1,3,TATTNP20       NIGHT PREMIUM HOURS AT 20%                   
         ICM   RE,3,OLDTT.TATTNP20                                              
         SR    R1,RE                                                            
         ICM   RE,3,NEWTT.TATTNP20                                              
         AR    R1,RE                                                            
         STCM  R1,3,TATTNP20                                                    
*                                                                               
         ZIC   R1,TATT16HR         16 HOUR RULE - NUMBER OF HRS. OVER           
         ZIC   RE,OLDTT.TATT16HR                                                
         SR    R1,RE                                                            
         ZIC   RE,NEWTT.TATT16HR                                                
         AR    R1,RE                                                            
         STC   R1,TATT16HR                                                      
*                                                                               
         MVC   TATTINCL,MYINCL     INCLUDE CODE - ALWAYS USE LOWEST             
*                                                                               
         ICM   R1,15,TATTREIM      REIMBURSEMENT                                
         ICM   RE,15,OLDTT.TATTREIM                                             
         SR    R1,RE                                                            
         ICM   RE,15,NEWTT.TATTREIM                                             
         AR    R1,RE                                                            
         STCM  R1,15,TATTREIM                                                   
*                                                                               
         ICM   R1,15,TATTNSPH      NOT SUBJECT TO PNH (MEAL PENALTY)            
         ICM   RE,15,OLDTT.TATTNSPH                                             
         SR    R1,RE                                                            
         ICM   RE,15,NEWTT.TATTNSPH                                             
         AR    R1,RE                                                            
         STCM  R1,15,TATTNSPH                                                   
*                                                                               
         ICM   R1,15,TATTPYMT      ADDITION TO PAYMENT AMT (SMOKE PAY)          
         ICM   RE,15,OLDTT.TATTPYMT                                             
         SR    R1,RE                                                            
         ICM   RE,15,NEWTT.TATTPYMT                                             
         AR    R1,RE                                                            
         STCM  R1,15,TATTPYMT                                                   
*                                                                               
         ICM   R1,15,TATTADJ       ADDITION TO PAYMENT AMT (ADJUSTMENT)         
         ICM   RE,15,OLDTT.TATTADJ                                              
         SR    R1,RE                                                            
         ICM   RE,15,NEWTT.TATTADJ                                              
         AR    R1,RE                                                            
         STCM  R1,15,TATTADJ                                                    
*                                                                               
         ICM   R1,3,TATTXSAT      EXTRAS SATURDAY PAY ON FRIDAYS                
         ICM   RE,3,OLDTT.TATTXSAT                                              
         SR    R1,RE                                                            
         ICM   RE,3,NEWTT.TATTXSAT                                              
         AR    R1,RE                                                            
         STCM  R1,3,TATTXSAT                                                    
*                                                                               
         IC    R1,TATTRPVL         REST PERIOD VIOLATIONS                       
         IC    RE,OLDTT.TATTRPVL                                                
         SR    R1,RE                                                            
         IC    RE,NEWTT.TATTRPVL                                                
         AR    R1,RE                                                            
         STC   R1,TATTRPVL                                                      
*                                                                               
         XIT1                                                                   
         DROP  OLDTT,NEWTT                                                      
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE CHECKS IF STATUS WAS CHANGED IN TOTALS ELEMENT                 
*        CHANGE IN STATUS WOULD BE WEATHER CANCELLATION, OR                     
*        TRAVEL TO DISTANT LOCATION                                             
*        IF CCEQ RETURNS A(R4) = ELEMENT WITH TOTALS TO BE SUBTRACTED           
*--------------------------------------------------------------------*          
CHKSTAT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
OLDTT    USING TATTD,RF                                                         
NEWTT    USING TATTD,R2                                                         
*                                                                               
         LA    RF,OLDTTEL                                                       
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         LA    R2,ELEM                                                          
*                                                                               
         OC    OLDTTEL,OLDTTEL     EXIT, IF NO OLD ELEMENT                      
         BZ    CSTATNO                                                          
         CLC   NEWTT.TATTSTAT,OLDTT.TATTSTAT   CHANGE IN STATUS?                
         BE    CSTATNO                                                          
*                                                                               
         MVC   BYTE,NEWTT.TATTSTAT                                              
         NI    BYTE,TATTSWCH+TATTSW3Q+TATTSTDL   ISOLATE THESE BITS             
         MVC   HALF(1),OLDTT.TATTSTAT                                           
         NI    HALF,TATTSWCH+TATTSW3Q+TATTSTDL   ISOLATE THESE BITS             
         CLC   BYTE,HALF                         CHANGE IN STATUS?              
         BE    CSTATNO                                                          
*                                                                               
CSTATYES XR    RC,RC                                                            
CSTATNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  OLDTT,NEWTT                                                      
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE RETURNS A(R4) = ELEMENT WITH TOTALS TO BE SUBTRACTED           
*        FOR TOTAL ELEMENTS WITH CHANGE IN STATUS                               
*        CHANGE IN STATUS WOULD BE WEATHER CANCELLATION OR                      
*        TRAVEL TO DISTANT LOCATION                                             
*--------------------------------------------------------------------*          
RTELSUB  NTR1  BASE=*,LABEL=*                                                   
         LA    RF,OLDTTEL                                                       
         USING TATTD,RF                                                         
         MVC   OLDSTAT,TATTSTAT                                                 
         DROP  RF                                                               
*                                                                               
         L     R4,AIO                                                           
         USING TATTD,R4                                                         
         MVI   ELCODE,TATTELQ             TIMESHEET TOTAL ELEMENT               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
RSUB40   BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF REGULAR DAY                    
         BNZ   RSUB50                                                           
         TM    OLDSTAT,TATTSWCH+TATTSW3Q   WTHR CXL 1/2 OR 3/4?                 
         BZ    RSUB48                                                           
         TM    OLDSTAT,TATTSWCH            WTHR CXL 1/2?                        
         BNO   RSUB45                                                           
         CLC   TATTDATE,=X'FFFFF8' LOOK FOR REGULAR DAY/WCXL 1/2 TOT            
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB45   CLC   TATTDATE,=X'FFFFF4' LOOK FOR REGULAR DAY/WCXL 3/4 TOT            
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB48   CLC   TATTDATE,=X'FFFFFE' LOOK FOR REGULAR DAYS TOTAL                  
         BNE   RSUB40                                                           
         B     RSUBX                                                            
*                                                                               
RSUB50   TM    DAYTYPE,DAYHLDY     IF HOLIDAY                                   
         BNO   RSUB60                                                           
         TM    OLDSTAT,TATTSWCH+TATTSW3Q+TATTSTDL WTHR CXL 1/2 OR 3/4?          
         BZ    RSUB58                             OR NON-CONSECUTIVE            
         TM    OLDSTAT,TATTSTDL    HOLIDAY NON-CONSECUTIVE?                     
         BNO   RSUB53                                                           
         CLC   TATTDATE,=X'FFFFF0' LOOK FOR SUNDAY/NON-CONSECUTIVE TOT          
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB53   TM    OLDSTAT,TATTSWCH    HOLIDAY WTHR CXL 1/2?                        
         BNO   RSUB55                                                           
         CLC   TATTDATE,=X'FFFFF7' LOOK FOR HOLIDAY/WCXL 1/2 TOT                
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB55   CLC   TATTDATE,=X'FFFFF3' LOOK FOR HOLIDAY/WCXL 3/4 TOT                
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB58   CLC   TATTDATE,=X'FFFFFD' LOOK FOR HOLIDAY TOTAL                       
         BNE   RSUB40                                                           
         B     RSUBX                                                            
*                                                                               
RSUB60   TM    DAYTYPE,DAYSAT      IF SATURDAY                                  
         BNO   RSUB70                                                           
         TM    OLDSTAT,TATTSWCH+TATTSW3Q+TATTSTDL  WTHR CXL OR DIST LOC         
         BZ    RSUB68              NONE?                                        
         TM    OLDSTAT,TATTSTDL    SATURDAY DISTANT LOCATION?                   
         BNO   RSUB65                                                           
         CLC   TATTDATE,=X'FFFFF0' LOOK FOR SATURDAY/DIS-LOC TOT                
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB65   TM    OLDSTAT,TATTSWCH    SATURDAY WEATHER CXL 1/2?                    
         BNO   RSUB66                                                           
         CLC   TATTDATE,=X'FFFFF6' LOOK FOR SATURDAY/WCXL 1/2 TOT               
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB66   CLC   TATTDATE,=X'FFFFF2' LOOK FOR SATURDAY/WCXL 3/4 TOT               
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB68   CLC   TATTDATE,=X'FFFFFC' LOOK FOR SATURDAY TOTAL                      
         BNE   RSUB40                                                           
         B     RSUBX                                                            
*                                                                               
RSUB70   TM    DAYTYPE,DAYSUN      IF SUNDAY                                    
         BO    *+6                                                              
         DC    H'00'                                                            
         TM    OLDSTAT,TATTSWCH+TATTSW3Q+TATTSTDL  WTHR CXL 1/2 OR 3/4?         
         BZ    RSUB78                              OR NON-CONSECUTIVE?          
         TM    OLDSTAT,TATTSTDL    SUNDAY NON-CONSECUTIVE?                      
         BNO   RSUB73                                                           
         CLC   TATTDATE,=X'FFFFF0' LOOK FOR SUNDAY/NON-CONSECUTIVE TOT          
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB73   TM    OLDSTAT,TATTSWCH            WTHR CXL 1/2?                        
         BNO   RSUB75                                                           
         CLC   TATTDATE,=X'FFFFF5' LOOK FOR SUNDAY/WCXL 1/2 TOT                 
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB75   CLC   TATTDATE,=X'FFFFF1' LOOK FOR SUNDAY/WCXL 3/4 TOT                 
         BNE   RSUB40                                                           
         B     RSUBX                                                            
RSUB78   CLC   TATTDATE,=X'FFFFFB' LOOK FOR SUNDAY TOTAL                        
         BNE   RSUB40                                                           
         B     RSUBX                                                            
*                                                                               
RSUBX    XIT1  REGS=(R4)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE UPDATES TIMESHEET TOTALS ELEMENT BY SUBTRACTING                
*        TOTALS FROM OLD TOTAL ELEMENT NO LONGER BEING USED                     
*        BECAUSE OF CHANGE IN STATUS - WEATHER CXL OR DISTANT TRAVEL            
*--------------------------------------------------------------------*          
         USING TATTD,R4                                                         
SUBTOTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
OLDTT    USING TATTD,RF                                                         
*                                                                               
         LA    RF,OLDTTEL                                                       
         SR    R1,R1                                                            
         SR    RE,RE                                                            
*                                                                               
         IC    R1,TATTSPOT         SPOT                                         
         IC    RE,OLDTT.TATTSPOT                                                
         SR    R1,RE                                                            
         STC   R1,TATTSPOT                                                      
*                                                                               
         IC    R1,TATTDAYS         DAYS                                         
         IC    RE,OLDTT.TATTDAYS                                                
         SR    R1,RE                                                            
         STC   R1,TATTDAYS                                                      
*                                                                               
         IC    R1,TATTOVTM         OVERTIME                                     
         IC    RE,OLDTT.TATTOVTM                                                
         SR    R1,RE                                                            
         STC   R1,TATTOVTM                                                      
*                                                                               
         IC    R1,TATTDBTM         DOUBLE TIME                                  
         IC    RE,OLDTT.TATTDBTM                                                
         SR    R1,RE                                                            
         STC   R1,TATTDBTM                                                      
*                                                                               
         IC    R1,TATTTAG          TAG                                          
         IC    RE,OLDTT.TATTTAG                                                 
         SR    R1,RE                                                            
         STC   R1,TATTTAG                                                       
*                                                                               
         MVC   DIFFTHRS,TATTTRVL   TRAVEL                                       
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         SR    R0,R0                                                            
         MVC   DIFFTHRS,OLDTT.TATTTRVL                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         SR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT BACK TO HOURS                        
         MVC   TATTTRVL,DIFFTHRS                                                
*                                                                               
         MVC   DIFFTHRS,TATTPDWD   PRIOR DAY WARDROBE                           
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,OLDTT.TATTPDWD                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         SR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT BACK TO HOURS                        
         MVC   TATTPDWD,DIFFTHRS                                                
*                                                                               
         ICM   R1,3,TATTNP10       NIGHT PREMIUM HOURS AT 10%                   
         ICM   RE,3,OLDTT.TATTNP10                                              
         SR    R1,RE                                                            
         STCM  R1,3,TATTNP10                                                    
*                                                                               
         ICM   R1,3,TATTNP20       NIGHT PREMIUM HOURS AT 20%                   
         ICM   RE,3,OLDTT.TATTNP20                                              
         SR    R1,RE                                                            
         STCM  R1,3,TATTNP20                                                    
*                                                                               
         ZIC   R1,TATT16HR         16 HOUR RULE - NUMBER OF HRS. OVER           
         ZIC   RE,OLDTT.TATT16HR                                                
         SR    R1,RE                                                            
         STC   R1,TATT16HR                                                      
*                                                                               
         MVC   TATTINCL,MYINCL     INCLUDE CODE - ALWAYS USE LOWEST             
*                                                                               
         ICM   R1,15,TATTREIM      REIMBURSEMENT                                
         ICM   RE,15,OLDTT.TATTREIM                                             
         SR    R1,RE                                                            
         STCM  R1,15,TATTREIM                                                   
*                                                                               
         ICM   R1,15,TATTNSPH      NOT SUBJECT TO PNH (MEAL PENALTY)            
         ICM   RE,15,OLDTT.TATTNSPH                                             
         SR    R1,RE                                                            
         STCM  R1,15,TATTNSPH                                                   
*                                                                               
         ICM   R1,15,TATTPYMT      ADDITION TO PAYMENT AMT (SMOKE PAY)          
         ICM   RE,15,OLDTT.TATTPYMT                                             
         SR    R1,RE                                                            
         STCM  R1,15,TATTPYMT                                                   
*                                                                               
         ICM   R1,15,TATTADJ       ADDITION TO PAYMENT AMT (ADJUSTMENT)         
         ICM   RE,15,OLDTT.TATTADJ                                              
         SR    R1,RE                                                            
         STCM  R1,15,TATTADJ                                                    
*                                                                               
         ICM   R1,3,TATTXSAT       EXTRAS SATURDAY PAY ON FRIDAYS               
         ICM   RE,3,OLDTT.TATTXSAT                                              
         SR    R1,RE                                                            
         STCM  R1,3,TATTXSAT                                                    
*                                                                               
         IC    R1,TATTRPVL      REST PERIOD VIOLATIONS                          
         IC    RE,OLDTT.TATTRPVL                                                
         SR    R1,RE                                                            
         STC   R1,TATTRPVL                                                      
*                                                                               
         XIT1                                                                   
         DROP  OLDTT                                                            
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE UPDATES TIMESHEET TOTALS ELEMENT BY ADDING                     
*        TOTALS TO A NEW TOTAL ELEMENT                                          
*        BECAUSE OF CHANGE IN STATUS - WEATHER CXL OR DISTANT TRAVEL            
*--------------------------------------------------------------------*          
         USING TATTD,R4                                                         
ADDTOTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
NEWTT    USING TATTD,R2                                                         
*                                                                               
         LA    RF,OLDTTEL                                                       
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         LA    R2,ELEM                                                          
*                                                                               
         IC    R1,TATTSPOT         SPOT                                         
         IC    RE,NEWTT.TATTSPOT                                                
         AR    R1,RE                                                            
         STC   R1,TATTSPOT                                                      
*                                                                               
         IC    R1,TATTDAYS         DAYS                                         
         IC    RE,NEWTT.TATTDAYS                                                
         AR    R1,RE                                                            
         STC   R1,TATTDAYS                                                      
*                                                                               
         IC    R1,TATTOVTM         OVERTIME                                     
         IC    RE,NEWTT.TATTOVTM                                                
         AR    R1,RE                                                            
         STC   R1,TATTOVTM                                                      
*                                                                               
         IC    R1,TATTDBTM         DOUBLE TIME                                  
         IC    RE,NEWTT.TATTDBTM                                                
         AR    R1,RE                                                            
         STC   R1,TATTDBTM                                                      
*                                                                               
         IC    R1,TATTTAG          TAG                                          
         IC    RE,NEWTT.TATTTAG                                                 
         AR    R1,RE                                                            
         STC   R1,TATTTAG                                                       
*                                                                               
         MVC   DIFFTHRS,TATTTRVL   TRAVEL                                       
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         SR    R0,R0                                                            
         MVC   DIFFTHRS,NEWTT.TATTTRVL                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         AR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT BACK TO HOURS                        
         MVC   TATTTRVL,DIFFTHRS                                                
*                                                                               
         MVC   DIFFTHRS,TATTPDWD   PRIOR DAY WARDROBE                           
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,NEWTT.TATTPDWD                                          
         BRAS  RE,HRS2MIN                                                       
         LH    R0,DIFFTMIN                                                      
         AR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT BACK TO HOURS                        
         MVC   TATTPDWD,DIFFTHRS                                                
*                                                                               
         ICM   R1,3,TATTNP10       NIGHT PREMIUM HOURS AT 10%                   
         ICM   RE,3,NEWTT.TATTNP10                                              
         AR    R1,RE                                                            
         STCM  R1,3,TATTNP10                                                    
*                                                                               
         ICM   R1,3,TATTNP20       NIGHT PREMIUM HOURS AT 20%                   
         ICM   RE,3,NEWTT.TATTNP20                                              
         AR    R1,RE                                                            
         STCM  R1,3,TATTNP20                                                    
*                                                                               
         ZIC   R1,TATT16HR         16 HOUR RULE - NUMBER OF HRS. OVER           
         ZIC   RE,NEWTT.TATT16HR                                                
         AR    R1,RE                                                            
         STC   R1,TATT16HR                                                      
*                                                                               
         MVC   TATTINCL,MYINCL     INCLUDE CODE - ALWAYS USE LOWEST             
*                                                                               
         ICM   R1,15,TATTREIM      REIMBURSEMENT                                
         ICM   RE,15,NEWTT.TATTREIM                                             
         AR    R1,RE                                                            
         STCM  R1,15,TATTREIM                                                   
*                                                                               
         ICM   R1,15,TATTNSPH      NOT SUBJECT TO PNH (MEAL PENALTY)            
         ICM   RE,15,NEWTT.TATTNSPH                                             
         AR    R1,RE                                                            
         STCM  R1,15,TATTNSPH                                                   
*                                                                               
         ICM   R1,15,TATTPYMT      ADDITION TO PAYMENT AMT (SMOKE PAY)          
         ICM   RE,15,NEWTT.TATTPYMT                                             
         AR    R1,RE                                                            
         STCM  R1,15,TATTPYMT                                                   
*                                                                               
         ICM   R1,15,TATTADJ       ADDITION TO PAYMENT AMT (ADJUSTMENT)         
         ICM   RE,15,NEWTT.TATTADJ                                              
         AR    R1,RE                                                            
         STCM  R1,15,TATTADJ                                                    
*                                                                               
         ICM   R1,3,TATTXSAT       EXTRAS SATURDAY PAY ON FRIDAYS               
         ICM   RE,3,NEWTT.TATTXSAT                                              
         AR    R1,RE                                                            
         STCM  R1,3,TATTXSAT                                                    
*                                                                               
         IC    R1,TATTRPVL      REST PERIOD VIOLATIONS                          
         IC    RE,NEWTT.TATTRPVL                                                
         AR    R1,RE                                                            
         STC   R1,TATTRPVL                                                      
*                                                                               
         XIT1                                                                   
         DROP  NEWTT                                                            
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO BUILD PRIOR DAY WARDROBE ELEMENT                      
*--------------------------------------------------------------------*          
BLDPDWD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   PDWDDAY,0                                                        
         OC    SVPDWD,SVPDWD                                                    
         BNZ   BPDWD10                                                          
         SR    R1,R1                                                            
         B     BPDWD60                                                          
*                                                                               
BPDWD10  GOTO1 DATCON,DMCB,(1,PDWDDATE),(0,TMSEDATE) CONVERT TO EBCDIC          
         MVC   TEMPDATE,PDWDDATE                                                
         BRAS  RE,LKHLDY           LOOK UP TO SEE IF HOLIDAY                    
         BNE   BPDWD20                                                          
         OI    PDWDDAY,DAYHLDY                                                  
         B     BPDWD50                                                          
*                                                                               
BPDWD20  GOTO1 GETDAY,DMCB,TMSEDATE,WORK                                        
         CLI   0(R1),6             SATURDAY OR SUNDAY?                          
         BL    BPDWD50                                                          
         BH    *+12                                                             
         OI    PDWDDAY,DAYSAT      SATURDAY=6                                   
         B     BPDWD50                                                          
         OI    PDWDDAY,DAYSUN      SUNDAY=7                                     
*                                                                               
BPDWD50  MVC   DIFFTHRS,SVPDWD                                                  
         BRAS  RE,HRS2DEC          CONVERT HRS.MINS TO DECIMAL                  
BPDWD60  STH   R1,PDWDDEC          CONTAINS DECIMAL PDWD                        
*                                                                               
         L     R4,AIO                                                           
         USING TATPD,R4                                                         
         MVI   ELCODE,TATPELQ      PRIOR DAY WARDROBE ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    BPDWD80             DON'T HAVE IT, JUST ADD                      
*                                                                               
         OC    SVPDWD,SVPDWD       IF WE DON'T HAVE OLD OR NEW PDWD             
         BZ    BPDWDX              DON'T ADD THIS ELEMENT                       
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TATPEL,TATPELQ      ELEMENT CODE                                 
         MVI   TATPLEN,TATPLNQ     LENGTH                                       
*                                  STORE AMOUNT IN APPROPRIATE PLACE            
         TM    PDWDDAY,DAYHLDY+DAYSAT+DAYSUN                                    
         BNZ   *+14                                                             
         MVC   TATPREG,PDWDDEC     REGULAR DAY TOTAL                            
         B     BPDWD70                                                          
         TM    PDWDDAY,DAYHLDY                                                  
         BNO   *+14                                                             
         MVC   TATPHOL,PDWDDEC     HOLIDAY TOTAL                                
         B     BPDWD70                                                          
         TM    PDWDDAY,DAYSAT                                                   
         BNO   *+14                                                             
         MVC   TATPSAT,PDWDDEC     SATURDAY TOTAL                               
         B     BPDWD70                                                          
         TM    PDWDDAY,DAYSUN                                                   
         BO    *+6                                                              
         DC    H'00'                                                            
         MVC   TATPSUN,PDWDDEC     SUNDAY TOTAL                                 
BPDWD70  GOTO1 ADDELEM                                                          
         B     BPDWDX                                                           
*                                                                               
*                                  POINT R2 TO APPROPRIATE TOTAL                
BPDWD80  LA    R2,TATPREG          REGULAR DAY TOTAL                            
         TM    PDWDDAY,DAYHLDY+DAYSAT+DAYSUN                                    
         BZ    BPDWD90                                                          
         LA    R2,TATPHOL          HOLIDAY TOTAL                                
         TM    PDWDDAY,DAYHLDY                                                  
         BO    BPDWD90                                                          
         LA    R2,TATPSAT          SATURDAY TOTAL                               
         TM    PDWDDAY,DAYSAT                                                   
         BO    BPDWD90                                                          
         LA    R2,TATPSUN          SUNDAY TOTAL                                 
         TM    PDWDDAY,DAYSUN                                                   
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
BPDWD90  ZICM  R1,0(R2),2          AMOUNT IN PRIOR DAY ELEMENT                  
         ZICM  R0,PDWDDEC,2        AMOUNT IN NEW TATT ELEMENT                   
         AR    R1,R0               ADD NEW AMOUNT                               
         STCM  R1,3,0(R2)          STORE NEW AMOUNT IN PDWD ELEMENT             
*                                                                               
*        SUBTRACT AMOUNT OF OLD PDW DAY FROM UPDATED ELEMENT                    
*                                                                               
         LA    R3,OLDTTEL                                                       
         OC    OLDTTEL,OLDTTEL     EXIT, IF NO OLD PDW ELEMENT                  
         BZ    BPDWDX                                                           
         USING TATTD,R3                                                         
         MVC   DIFFTHRS,TATTPDWD                                                
         BRAS  RE,HRS2DEC                                                       
         STH   R1,OLDPDWD          OLD PRIOR DAY WARDOBE IN DECIMAL             
         DROP  R3                                                               
*                                                                               
         OC    OLDPWDT,OLDPWDT     IF NO OLD PDW DAY, EXIT                      
         BZ    BPDWDX                                                           
*                                                                               
         MVI   OLDPWDAY,0          OLD PDW DAY STATUS                           
         GOTO1 DATCON,DMCB,(1,OLDPWDT),(0,TMSEDATE) CONVERT TO EBCDIC           
         MVC   TEMPDATE,OLDPWDT                                                 
         BRAS  RE,LKHLDY           LOOK UP TO SEE IF HOLIDAY                    
         BNE   BPDWD100                                                         
         OI    OLDPWDAY,DAYHLDY                                                 
         B     BPDWD110                                                         
*                                                                               
BPDWD100 GOTO1 GETDAY,DMCB,TMSEDATE,WORK                                        
         CLI   0(R1),6             SATURDAY OR SUNDAY?                          
         BL    BPDWD110                                                         
         BH    *+12                                                             
         OI    OLDPWDAY,DAYSAT      SATURDAY=6                                  
         B     BPDWD110                                                         
         OI    OLDPWDAY,DAYSUN      SUNDAY=7                                    
*                                                                               
*                                  POINT R2 TO APPROPRIATE TOTAL                
BPDWD110 LA    R2,TATPREG          REGULAR DAY TOTAL                            
         TM    OLDPWDAY,DAYHLDY+DAYSAT+DAYSUN                                   
         BZ    BPDWD120                                                         
         LA    R2,TATPHOL          HOLIDAY TOTAL                                
         TM    OLDPWDAY,DAYHLDY                                                 
         BO    BPDWD120                                                         
         LA    R2,TATPSAT          SATURDAY TOTAL                               
         TM    OLDPWDAY,DAYSAT                                                  
         BO    BPDWD120                                                         
         LA    R2,TATPSUN          SUNDAY TOTAL                                 
         TM    OLDPWDAY,DAYSUN                                                  
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
BPDWD120 ZICM  R1,0(R2),2          AMOUNT IN PRIOR DAY ELEMENT                  
         ZICM  R0,OLDPDWD,2        AMOUNT IN OLD TATT ELEMENT                   
         SR    R1,R0               SUBTRACT OLD AMOUNT                          
         STCM  R1,3,0(R2)          STORE NEW AMOUNT IN PDWD ELEMENT             
*                                                                               
BPDWDX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO CLEAR SCREEN                                          
*--------------------------------------------------------------------*          
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
         XC    STMSPOT,STMSPOT                                                  
         XC    STMTAGS,STMTAGS                                                  
         XC    STMMLPN,STMMLPN                                                  
         XC    STMADJ,STMADJ                                                    
*                                                                               
         OI    STMSPOTH+6,X'80'                                                 
         OI    STMTAGSH+6,X'80'                                                 
         OI    STMMLPNH+6,X'80'                                                 
         OI    STMADJH+6,X'80'                                                  
*                                                                               
*        CLEAR THIS LINE                                                        
*                                                                               
         XC    STMTHSP,STMTHSP                                                  
         XC    STMTHDY,STMTHDY                                                  
         XC    STMTHOT,STMTHOT                                                  
         XC    STMTHDT,STMTHDT                                                  
         XC    STMTHTR,STMTHTR                                                  
         XC    STMTHPW,STMTHPW                                                  
         XC    STMTHT,STMTHT                                                    
         XC    STMTHRE,STMTHRE                                                  
*                                                                               
         OI    STMTHSPH+6,X'80'                                                 
         OI    STMTHDYH+6,X'80'                                                 
         OI    STMTHOTH+6,X'80'                                                 
         OI    STMTHDTH+6,X'80'                                                 
         OI    STMTHTRH+6,X'80'                                                 
         OI    STMTHPWH+6,X'80'                                                 
         OI    STMTHTH+6,X'80'                                                  
         OI    STMTHREH+6,X'80'                                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO VALIDATE AND CALCULATE 2 TIME FIELDS                  
*--------------------------------------------------------------------*          
STENTIME NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         CLI   5(R3),0                                                          
         BNE   STEN10                                                           
         CLI   5(R4),0                                                          
         BNE   STEN10                                                           
         XC    STRTTIME(4),STRTTIME                                             
         XC    DIFFTMIN,DIFFTMIN                                                
         XC    DIFFTHRS,DIFFTHRS                                                
         B     STENX                                                            
*                                                                               
STEN10   LR    R2,R3                                                            
         GOTO1 TIMVAL,DMCB,(5(R2),8(R2)),TMPTIME                                
         CLI   DMCB,X'FF'          ERROR ON RETURN                              
         BE    FLDINV                                                           
         MVC   STRTTIME,TMPTIME                                                 
*                                                                               
         LR    R2,R4                                                            
         GOTO1 TIMVAL,DMCB,(5(R2),8(R2)),TMPTIME                                
         CLI   DMCB,X'FF'          ERROR ON RETURN                              
         BE    FLDINV                                                           
         MVC   ENDTIME,TMPTIME                                                  
*                                                                               
         BRAS  RE,TIMDIFF                                                       
STENX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              SHOW TIME ON THE SCREEN                                          
*                 P1 = ADDRESS OF TIME, START AND END                           
*                 P2 = ADDRESS OF START TIME FIELD HEADER                       
*                 P3 = ADDRESS OF END TIME FIELD HEADER                         
*                 P4 = ADDRESS OF TIME DIFF FIELD HEADER                        
*--------------------------------------------------------------------*          
TIMDISP  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            TIME                                         
         MVC   STRTTIME,0(R2)                                                   
         MVC   ENDTIME,2(R2)                                                    
         L     R3,4(R1)            START TIME ON SCREEN                         
         L     R4,8(R1)            END TIME ON SCREEN                           
         L     R2,12(R1)           TIME DIFFERENCE ON SCREEN                    
*                                                                               
         MVC   8(L'STMWTST,R2),SPACES                                           
         MVC   8(L'STMWTNT,R3),SPACES                                           
         MVC   8(L'STMWTNT,R4),SPACES                                           
*                                                                               
         OC    STRTTIME,STRTTIME   ANY WORK TIME?                               
         BZ    TDISPX                                                           
         LA    R5,STRTTIME                                                      
         SR    R6,R6                                                            
TDISP10  ZICM  RF,0(R5),2          RF=MILITARY TIME                             
         MVI   BYTE,C'A'                                                        
         CHI   RF,100              AM, IF TIME < 100                            
         BNL   TDISP15                                                          
         AHI   RF,1200             ADD 1200 TO THE TIME                         
         B     TDISP20                                                          
TDISP15  CHI   RF,1200             AM, IF TIME < 1200                           
         BL    TDISP20                                                          
         MVI   BYTE,C'N'           NOON, IF TIME = 1200                         
         BE    TDISP20                                                          
*                                                                               
         CHI   RF,1300                                                          
         BL    *+8                                                              
         SHI   RF,1200             SUBTRACT 1200 FROM TIME ABOVE 1300           
         MVI   BYTE,C'P'           PM, IF TIME > 1200                           
         CHI   RF,1200                                                          
         BNE   TDISP20                                                          
         MVI   BYTE,C'M'           MIDNIGHT, IF TIME = 2400-1200=1200           
TDISP20  EDIT  (RF),(4,8(R3)),ALIGN=LEFT                                        
         LA    RF,11(R3)           IF TIME IS 3 CHARS, PUT A,P,N,M HERE         
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         LA    RF,1(RF)            IF TIME IS 4 CHARS, PUT A,P,N,M HERE         
         MVC   0(1,RF),BYTE        AM, PM, NOON, OR MIDNIGHT                    
         OI    6(R3),X'80'                                                      
         LTR   R6,R6                                                            
         BNZ   TDISP90                                                          
         LA    R5,ENDTIME          NOW, DISPLAY END TIME                        
         LA    R6,1                SET FLAG FOR 2ND RUN                         
         LR    R3,R4               ADDRESS OF END TIME ON SCREEN                
         B     TDISP10                                                          
*                                                                               
TDISP90  BRAS  RE,TIMDIFF                                                       
*                                                                               
         LA    RE,STMM1THH                                                      
         CR    R2,RE               MEAL 1?                                      
         BE    TDISP95                                                          
         LA    RE,STMM2THH                                                      
         CR    R2,RE               MEAL 2?                                      
         BE    TDISP95                                                          
         LA    RE,STMM3THH                                                      
         CR    R2,RE               MEAL 3?                                      
         BNE   TDISP99                                                          
TDISP95  CLC   DIFFTHRS,=H'100'    MEALS CANNOT EXCEED 1 HOUR                   
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'                                                 
*                                                                               
TDISP99  EDIT  DIFFTHRS,(L'STMWTTH,8(R2)),2                                     
*                                                                               
TDISPX   OI    6(R3),X'80'         TRANSMIT                                     
         OI    6(R4),X'80'                                                      
         OI    6(R2),X'80'                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              DIFFERENCE BETWEEN TWO TIMES, STRTTIME AND ENDTIME               
*                 RESULT STORED IN DIFFTMIN AND DIFFTHRS                        
*--------------------------------------------------------------------*          
TIMDIFF  NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,STRTTIME                                                    
         D     R0,=F'100'          TRY TO SEPARATE MINUTES                      
         LR    RE,R0               NUMBER OF MINUTES IN RF                      
         MHI   R1,60               CONVERT HOURS TO MINUTES                     
         AR    RE,R1                                                            
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,ENDTIME                                                     
         CLC   ENDTIME,STRTTIME    IF ENDTIME < STRTTIME                        
         BNL   *+8                                                              
         AHI   R1,2400             ADD 24 HOURS TO ENDTIME                      
*                                                                               
         D     R0,=F'100'          TRY TO SEPARATE MINUTES                      
         LR    RF,R0               NUMBER OF MINUTES IN RF                      
         MHI   R1,60               CONVERT HOURS TO MINUES                      
         AR    RF,R1                                                            
*                                                                               
         SR    RF,RE                                                            
         STH   RF,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
*                                  GET DIFFERENCE IN HOURS                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CONVERT MINUTES TO HOURS, DIFFTMIN --> DIFFTHRS                  
*--------------------------------------------------------------------*          
MIN2HRS  NTR1  BASE=*,LABEL=*                                                   
         LH    RF,DIFFTMIN                                                      
         SR    RE,RE                                                            
         D     RE,=F'60'                                                        
         MHI   RF,100                                                           
         AR    RF,RE                                                            
         STH   RF,DIFFTHRS                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CONVERT HOURS/MINUTES TO DECIMAL HOURS/MINUTES        *          
*              EXAMPLE: 430 (4 HRS 30 MINS) = 450 (4.5 HRS)          *          
*              INPUT=DIFFTHRS, OUTPUT=R1                             *          
*--------------------------------------------------------------------*          
HRS2DEC  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,HRS2MIN            CONVERT DIFFTHRS TO MINS                   
         LH    R1,DIFFTMIN           CONVERT TO DECIMAL HOURS                   
         MHI   R1,100                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'             R1 = QUOTIENT                              
         CHI   R0,30                 IF THERE IS A REMAINDER >= 30,             
         BL    *+8                                                              
         AHI   R1,1                  ROUND UP                                   
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CONVERT DECIMAL HOURS/MINUTES TO HOURS/MINUTES        *          
*              EXAMPLE: 450 (4.5 HRS) = 430 (4 HRS 30 MINS)          *          
*              INPUT=DIFFTHRS, OUTPUT=DIFFTHRS                       *          
*--------------------------------------------------------------------*          
DEC2HRS  NTR1  BASE=*,LABEL=*                                                   
         LH    R1,DIFFTHRS           CONVERT TO DECIMAL HOURS                   
         MHI   R1,60                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'            R1 = QUOTIENT                              
         CHI   R0,50                 IF THERE IS A REMAINDER >= 50,             
         BL    *+8                                                              
         AHI   R1,1                  ROUND UP                                   
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS            CONVERT DIFFTMIN TO HOURS                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CONVERT HOURS TO MINUTES, DIFFTHRS --> DIFFTMIN                  
*--------------------------------------------------------------------*          
HRS2MIN  NTR1  BASE=*,LABEL=*                                                   
         LH    RF,DIFFTHRS                                                      
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         LR    R1,RE               NUMBER OF MINUTES                            
         MHI   RF,60                                                            
         AR    RF,R1                                                            
         STH   RF,DIFFTMIN                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ADD HRS.MINS TOGETHER                                            
*              INPUT = HRMIN1 AND HRMIN2                                        
*              OUTPUT = DIFFTHRS = SUM OF INPUT                                 
*--------------------------------------------------------------------*          
ADDHRS   NTR1  BASE=*,LABEL=*                                                   
         MVC   DIFFTHRS,HRMIN1                                                  
         BRAS  RE,HRS2MIN          CONVERT TO MINUTES                           
         LH    R1,DIFFTMIN         R1 = # OF MINUTES OF 1ST                     
         MVC   DIFFTHRS,HRMIN2                                                  
         BRAS  RE,HRS2MIN          CONVERT TO MINUTES                           
         AH    R1,DIFFTMIN         ADD 1ST AND 2ND # OF MINS TOGETHER           
         STH   R1,DIFFTMIN         DIFFTMIN = SUM                               
         BRAS  RE,MIN2HRS          CONVERT BACK TO HRS.MINS                     
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE MEAL TIMES                                              
*              R4--> ELEMENT                                                    
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
VALMEAL  NTR1  BASE=*,LABEL=*                                                   
         XC    MEALHRS,MEALHRS                                                  
         XC    MEAL2HRS,MEAL2HRS                                                
         XC    MEAL3HRS,MEAL3HRS                                                
*                                                                               
         OC    STMM1ST,STMM1ST     IS THERE A MEAL 1?                           
         BNZ   VMEAL05                                                          
         OC    STMM1NT,STMM1NT                                                  
         BNZ   VMEAL05                                                          
         LA    R2,STMM2STH         IF NO MEAL 1, MEAL 2 IS INVALID              
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         LA    R2,STMM2NTH                                                      
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         LA    R2,STMM3STH         IF NO MEAL 1, MEAL 3 IS INVALID              
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         LA    R2,STMM3NTH                                                      
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         B     VMEALX                                                           
*                                                                               
VMEAL05  BRAS  RE,VALNDML          VALIDATE NON DEDUCTIBLE MEAL                 
*                                                                               
         GOTOR STENTIME,DMCB,STMM1STH,STMM1NTH     MEAL 1                       
         MVC   TATMM1ST,STRTTIME                   START                        
         MVC   TATMM1NT,ENDTIME                    END                          
         LA    R2,STMM1NTH                                                      
         MVI   WBERRFLD,D#TMM1E                                                 
         OC    DIFFTHRS,DIFFTHRS   MEAL IS 0 HOURS LONG?                        
         BZ    FLDINV              INVALID END TIME                             
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL NON-DEDUCTIBLE?                      
         BNZ   VMEAL30             IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST   START OF MEAL                                
         MVC   MEALEND,TATMM1NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   VMEAL30             IF NOT, IGNORE MEAL                          
VMEAL10  MVC   MEALHRS,DIFFTHRS    LENGTH OF MEALS                              
*                                                                               
VMEAL30  OC    STMM2ST,STMM2ST     NO MEAL 2? SKIP                              
         BNZ   VMEAL32                                                          
         OC    STMM2NT,STMM2NT                                                  
         BNZ   VMEAL32                                                          
         LA    R2,STMM3STH         IF NO MEAL 2, MEAL 3 IS INVALID              
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         LA    R2,STMM3NTH                                                      
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         B     VMEALX                                                           
*                                                                               
VMEAL32  GOTOR STENTIME,DMCB,STMM2STH,STMM2NTH       MEAL 2                     
         MVC   TATMM2ST,STRTTIME                     START                      
         MVC   TATMM2NT,ENDTIME                      END                        
         LA    R2,STMM2NTH                                                      
         MVI   WBERRFLD,D#TMM2E                                                 
         OC    DIFFTHRS,DIFFTHRS   MEAL 2 IS 0 HOURS LONG?                      
         BZ    FLDINV              INVALID END TIME                             
*                                                                               
         LA    R2,STMM2STH                                                      
         MVI   WBERRFLD,D#TMM2S                                                 
         CLC   TATMM1NT,=H'1200'   IF MEAL 1 ENDS IN PM                         
         BL    VMEAL35                                                          
         CLC   TATMM2ST,=H'1200'   AND MEAL 2 STARTS IN AM, OKAY                
         BNH   *+14                                                             
VMEAL35  CLC   TATMM1NT,TATMM2ST   MEAL 2 MUST START AFTER MEAL 1 ENDS          
         BH    FLDINV                                                           
         MVC   MEALSTRT,TATMM2ST   START OF MEAL                                
         MVC   MEALEND,TATMM2NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   VMEAL50             IF NOT, IGNORE MEAL                          
VMEAL40  MVC   MEAL2HRS,DIFFTHRS   LENGTH OF MEAL 2                             
*                                                                               
         MVC   HRMIN1,MEALHRS      MEAL1 HOURS                                  
         MVC   HRMIN2,MEAL2HRS     MEAL 2 HOURS                                 
         BRAS  RE,ADDHRS           ADD MEALHRS + MEAL2HRS                       
         MVC   MEALHRS,DIFFTHRS    TOTAL MEAL HOURS                             
*                                                                               
VMEAL50  OC    STMM3ST,STMM3ST     NO MEAL 3? SKIP                              
         BNZ   VMEAL52                                                          
         OC    STMM3NT,STMM3NT                                                  
         BZ    VMEALX                                                           
*                                                                               
VMEAL52  GOTOR STENTIME,DMCB,STMM3STH,STMM3NTH       MEAL 3                     
         MVC   TATMM3ST,STRTTIME                     START                      
         MVC   TATMM3NT,ENDTIME                      END                        
         LA    R2,STMM3NTH                                                      
         MVI   WBERRFLD,D#TMM3E                                                 
         OC    DIFFTHRS,DIFFTHRS   MEAL 3 IS 0 HOURS LONG?                      
         BZ    FLDINV              INVALID END TIME                             
*                                                                               
         LA    R2,STMM3STH                                                      
         MVI   WBERRFLD,D#TMM3S                                                 
         CLC   TATMM2NT,=H'1200'   IF MEAL 2 ENDS IN PM                         
         BL    VMEAL55                                                          
         CLC   TATMM3ST,=H'1200'   AND MEAL 3 STARTS IN AM, OKAY                
         BNH   *+14                                                             
VMEAL55  CLC   TATMM2NT,TATMM3ST   MEAL 3 MUST START AFTER MEAL 2 ENDS          
         BH    FLDINV                                                           
         MVC   MEALSTRT,TATMM3ST   START OF MEAL                                
         MVC   MEALEND,TATMM3NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   VMEALX              IF NOT, IGNORE MEAL                          
         MVC   MEAL3HRS,DIFFTHRS   LENGTH OF MEAL 3                             
*                                                                               
         MVC   HRMIN1,MEALHRS      MEAL1 + MEAL2 HOURS                          
         MVC   HRMIN2,MEAL3HRS     MEAL 3 HOURS                                 
         BRAS  RE,ADDHRS           ADD MEALHRS + MEAL3HRS                       
         MVC   MEALHRS,DIFFTHRS    TOTAL MEAL HOURS                             
*                                                                               
VMEALX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CHECK IF MEAL FALLS DURING WORK TIME                             
*              CHANGES MEALEND, IF MEAL IS > 1 HOUR                             
*              CHANGES MEALSTART IF WORK START > WORK END                       
*              SETS DIFFTHRS WITH LENGTH OF MEAL DURING WORK TIME               
*              R4--> ELEMENT                                                    
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CHKMLWK  NTR1  BASE=*,LABEL=*                                                   
*                                  SET START AND END OF DAY                     
         MVC   SVSTRT,TATMTTDP                                                  
         OC    TATMTTDP,TATMTTDP   TRAVEL TO - DEPART                           
         BNZ   *+10                                                             
         MVC   SVSTRT,TATMWTST     WORK START                                   
         MVC   SVEND,TATMTFAR                                                   
         OC    TATMTFAR,TATMTFAR   TRAVEL FROM ARRIVE                           
         BNZ   *+10                                                             
         MVC   SVEND,TATMWTNT      WORK END                                     
*                                                                               
         CLI   STM16YN,C'Y'        USING 16 HOUR RULE?                          
         BNE   CMLWK10                                                          
         ZICM  R0,SVSTRT,2                                                      
         AHI   R0,1600                                                          
         CHI   R0,2400             IF NUMBER IS TOO HIGH,                       
         BNH   *+8                                                              
         AHI   R0,-2400            SUBTRACT 2400                                
         STCM  R0,3,SVEND          END TIME IS START + 16 HOURS                 
*                                                                               
CMLWK10  MVC   STRTTIME,MEALSTRT   MEAL START                                   
         MVC   ENDTIME,MEALEND     MEAL END                                     
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL                     
         MVC   TEMPHRS,DIFFTHRS                                                 
         CLC   DIFFTHRS,=H'30'     IF MEAL < 30 MINUTES                         
         BL    CMLWKNO             DOES NOT COUNT AS A MEAL                     
         CLC   DIFFTHRS,=H'100'    IF MEAL > 1 HOUR                             
         BNH   CMLWK30                                                          
         ZICM  RE,MEALSTRT,2       SET MEAL END AS MEAL + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,MEALEND        AFTER 1 HOUR OF MEAL = WORK TIME             
         MVC   TEMPHRS,=H'100'     SET MEAL LENGTH TO 1 HOUR                    
*                                                                               
CMLWK30  MVC   STRTTIME,SVSTRT                                                  
         MVC   ENDTIME,SVEND                                                    
         BRAS  RE,TIMDIFF          LENGTH OF WORK DAY                           
         LH    R1,DIFFTHRS                                                      
         MVC   STRTTIME,SVSTRT     TIME FROM WORK START TO MEAL START           
         MVC   ENDTIME,MEALSTRT                                                 
         CLC   STRTTIME,ENDTIME    IF MEAL STARTS WHEN WORK STARTS              
         BE    CMLWKYES            OKAY                                         
         BRAS  RE,TIMDIFF                                                       
         CH    R1,DIFFTHRS         IF MEAL STARTS AFTER WORK START AND          
         BNH   CMLWK40             BEFORE WORK END, OKAY                        
         MVC   STRTTIME,SVSTRT                                                  
         MVC   ENDTIME,MEALEND                                                  
         BRAS  RE,TIMDIFF          TIME FROM WORK START TO MEAL END             
         CH    R1,DIFFTHRS         IF MEAL ENDS BEFORE WORK END, OKAY           
         BNL   CMLWKYES                                                         
         MVC   STRTTIME,MEALSTRT                                                
         MVC   ENDTIME,SVEND                                                    
         BRAS  RE,TIMDIFF                                                       
         CLC   DIFFTHRS,=H'100'                                                 
         BNH   CMLWKYS2                                                         
         MVC   DIFFTHRS,=H'100'                                                 
         B     CMLWKYS2                                                         
*                                                                               
CMLWK40  MVC   STRTTIME,SVSTRT     WORK START                                   
         MVC   ENDTIME,MEALEND     MEAL END                                     
         BRAS  RE,TIMDIFF                                                       
         CLC   DIFFTHRS,=H'100'    IS PART OF MEAL DURING WORK TIME?            
         BH    CMLWKNO                                                          
         B     CMLWKYS2                                                         
*                                                                               
CMLWKYES MVC   DIFFTHRS,TEMPHRS    RESTORE LENGTH OF MEAL                       
CMLWKYS2 XR    RC,RC                                                            
CMLWKNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE END OF PREMIUM TIME FOR EXTRAS              *          
*              IF USING THE 16HR RULE, ALWAYS 6AM                    *          
*              ALWAYS 6AM FOR PRINCIPALS                             *          
*              R4 --> ELEM                                           *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
PREMEND  NTR1  BASE=*,LABEL=*                                                   
         MVC   PRMEND,=H'600'      DEFAULT = 6AM                                
*                                                                               
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   PRMNDX                                                           
*                                                                               
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE, USE DEFAULT              
         BO    PRMNDX                                                           
*                                                                               
         MVC   STRTTIME,TATMWTST   START OF WORK                                
         MVC   ENDTIME,TATMWTNT    END OF WORK                                  
         OC    STRTTIME,STRTTIME                                                
         BZ    PRMNDX                                                           
         ZICM  R1,STRTTIME,2                                                    
         ZICM  R2,ENDTIME,2                                                     
         LR    RF,R2               HOLD ORIGINAL ENDTIME                        
         CLC   ENDTIME,STRTTIME    IF ENDTIME < STRTTIME                        
         BNL   *+8                                                              
         AHI   R2,2400             ADD 24 HOURS TO ENDTIME                      
*                                                                               
         CHI   R1,600              IF STARTING ON OR AFTER 6AM                  
         BL    PRMND30                                                          
         CHI   R1,2000             BUT STARTS BEFORE 8PM                        
         BNL   PRMND30                                                          
         CHI   R2,2000             AND ENDS BEFORE 8PM                          
         BNH   PRMNDX              THEN NO NIGHT PREMIUM                        
*                                                                               
PRMND30  CHI   R1,600              STARTS AFTER 6AM-8PM?                        
         BL    PRMND35                                                          
         CHI   R1,2000                                                          
         BH    *+8                                                              
         LHI   R1,2000             ASSUME START AT 8PM                          
*                                                                               
PRMND35  CHI   R1,100              STARTS BETWEEN 1AM AND 2AM?                  
         BL    PRMND40                                                          
         CHI   R1,200                                                           
         BL    PRMND50                                                          
PRMND40  CHI   R1,2000             (NOT BEFORE 8PM)                             
         BL    PRMNDX              NP 20% STOPS AT REAL END TIME                
*                                                                               
PRMND50  CHI   RF,600              IF END TIME IS < 6AM, USE DEFAULT            
         BNH   PRMNDX                                                           
         CHI   RF,2000             OR > 8PM, USE DEFAULT                        
         BNL   PRMND60                                                          
         STH   RF,PRMEND           REAL END TIME                                
*                                                                               
PRMND60  CHI   R1,200              IF START TIME IS BETWEEN 1AM AND 2AM         
         BNL   PRMNDX                                                           
         CHI   R1,100              AND END TIME > 8PM                           
         BL    PRMNDX                                                           
         STH   RF,PRMEND           USE REAL END TIME                            
*                                                                               
PRMNDX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE ANY PREMIUM MEAL TIME                                  
*              PREMIUM END TIME = 6AM FOR PRINCIPALS                            
*              INPUT = MEALSTRT AND MEALEND                                     
*              OUTPUT = TEMP10 AND TEMP20                                       
*              NPSTART= 8PM SAG OR 11PM ACTRA                                   
*              AS OF FEB2010 MEALS SHOULD NOT BE ROUNDED                        
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
PRMMEAL  NTR1  BASE=*,LABEL=*                                                   
         XC    TEMP10,TEMP10                                                    
         XC    TEMP20,TEMP20                                                    
*                                                                               
         TM    TGCSORT,X'20'       IF CAST MEMBER IS A PRINCIPAL                
         BO    PRMML05                                                          
         CLI   SVACTRA,CCTY04A     2404A?                                       
         BE    *+12                                                             
         CLI   ACTRULES,C'Y'       ACTRA RULES, (EXCEPT 2404A) SKIP             
         BE    PRMML05                                                          
         CLC   TATMWTST,=H'500'    AND WORK START IS 5AM TO 6AM                 
         BL    PRMML05                                                          
         CLC   TATMWTST,=H'600'                                                 
         BH    PRMML05                                                          
         CLC   MEALEND,=H'500'     IF MEAL IS BETWEEN 5AM AND 6AM,              
         BL    PRMML05                                                          
         CLC   MEALSTRT,=H'600'    MEAL IS NOT DURING PREMIUM TIME              
         BNH   PRMMLX                                                           
*                                                                               
PRMML05  CLC   MEALSTRT,PRMEND     BETWEEN 8PM - PRMEND = PREMIUM               
         BL    PRMML10                                                          
         CLC   MEALSTRT,NPSTART    BETWEEN PRMEND - 8PM = NOT PREMIUM           
         BL    PRMML40                                                          
         B     PRMML20                                                          
PRMML10  TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   PRMML20                                                          
         CLI   ACTRULES,C'Y'       ACTRA RULES?                                 
         BE    PRMML20                                                          
         CLC   MEALSTRT,=H'100'    BETWEEN 1AM-6AM = 20% PREMIUM                
         BL    PRMML20                                                          
*NO-OP   BRAS  RE,RNDQRTR          ROUND TO THE NEAREST QUARTER                 
         MVC   TEMP20,DIFFTHRS     MEAL IS DURING 20% PREMIUM TIME              
         CLC   MEALEND,PRMEND      IS MEAL END BEFORE PRMEND?                   
         BNH   PRMMLX                                                           
         MVC   STRTTIME,MEALSTRT   CALCULATE TIME BETWEEN START OF              
         MVC   ENDTIME,PRMEND      MEAL TO PRMEND                               
         BRAS  RE,TIMDIFF                                                       
*NO-OP   BRAS  RE,RNDQRTR                                                       
         MVC   TEMP20,DIFFTHRS                                                  
         B     PRMMLX                                                           
*                                                                               
*NO-OP0  BRAS  RE,RNDQRTR          ROUND TO THE NEAREST QUARTER                 
PRMML20  MVC   TEMP10,DIFFTHRS     MEAL IS DURING 10% PREMIUM TIME              
         CLI   ACTRULES,C'Y'       ACTRA RULES?                                 
         BE    PRMML23                                                          
         TM    TGCSORT,X'20'       IF CAST MEMBER IS NOT AN EXTRA,              
         BO    PRMML25                                                          
PRMML23  CLC   MEALEND,NPSTART     OKAY IF MEAL ENDS AFTER 8PM                  
         BH    PRMMLX                                                           
         CLC   MEALEND,=H'600'     OKAY IF MEAL ENDS BEFORE 6AM                 
         BNH   PRMMLX                                                           
         MVC   STRTTIME,MEALSTRT   CALCULATE TIME BETWEEN START AND 6AM         
         MVC   ENDTIME,=H'600'     STOP AT 6AM                                  
         BRAS  RE,TIMDIFF                                                       
*NO-OP   BRAS  RE,RNDQRTR                                                       
         MVC   TEMP10,DIFFTHRS     RECALCULATED 10% PREMIUM TIME                
         B     PRMMLX                                                           
*                                                                               
*                                  IF CAST MEMBER IS AN EXTRA,                  
PRMML25  CLC   MEALEND,=H'100'     IS MEAL END BEFORE 1AM?                      
         BNH   PRMMLX                                                           
         CLC   MEALEND,NPSTART     IS MEAL BETWEEN 1AM AND 8PM?                 
         BL    PRMML30                                                          
**no-op  CLC   MEALEND,=H'2400'    OKAY IF BTWN 8PM AND 1AM                     
         CLC   MEALEND,=H'2500'    OKAY IF BTWN 8PM AND 1AM                     
         BNH   PRMMLX                                                           
PRMML30  MVC   STRTTIME,=H'100'    CALCULATE TIME BETWEEN 1AM AND               
         MVC   ENDTIME,MEALEND     MEAL END                                     
         CLC   ENDTIME,PRMEND      IF MEAL END IS > PRMEND,                     
         BNH   *+10                STOP AT PRMEND                               
         MVC   ENDTIME,PRMEND                                                   
         BRAS  RE,TIMDIFF                                                       
*NO-OP   BRAS  RE,RNDQRTR                                                       
         MVC   TEMP20,DIFFTHRS                                                  
         MVC   STRTTIME,MEALSTRT   CALCULATE TIME BETWEEN MEAL START            
         MVC   ENDTIME,=H'100'     AND 1AM                                      
         BRAS  RE,TIMDIFF                                                       
*NO-OP   BRAS  RE,RNDQRTR                                                       
         MVC   TEMP10,DIFFTHRS     RECALCULATED 10% PREMIUM TIME                
         B     PRMMLX                                                           
*                                                                               
*                                  IF NO PREMIUM MEAL HOURS SO FAR,             
PRMML40  CLC   MEALEND,NPSTART     IS MEAL END BEFORE 8PM?                      
         BNH   PRMMLX                                                           
         MVC   STRTTIME,NPSTART    CALCULATE TIME FROM 8PM TO MEAL END          
         MVC   ENDTIME,MEALEND     MEAL CAN BE UP TO 1 HOUR LONG,               
PRMML50  BRAS  RE,TIMDIFF          SO ANY MEAL THAT STARTS BEFORE 8             
*NO-OP   BRAS  RE,RNDQRTR          CAN HAVE 10% PREMIUM TIME ONLY               
         MVC   TEMP10,DIFFTHRS     10% PREMIUM TIME                             
PRMMLX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE ANY NON PREMIUM MEAL TIME                              
*              THAT CAME BEFORE PREMIUM TIME                                    
*              INPUT = MEALSTRT AND MEALEND                                     
*              STORES AMOUNT OF NON-PREM TIME IN MEALNPH                        
*              NPSTART = 8PM SAG OR 11PM ACTRA                                  
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
NONPRMML NTR1  BASE=*,LABEL=*                                                   
         TM    TGCSORT,X'20'       IF CAST MEMBER IS A PRINCIPAL                
         BO    NPRMML02                                                         
         CLC   TATMWTST,=H'500'    AND WORK START IS 5AM TO 6AM                 
         BL    NPRMML02                                                         
         CLC   TATMWTST,=H'600'                                                 
         BH    NPRMML02                                                         
         CLC   MEALEND,=H'500'     IF MEAL IS BETWEEN 5AM AND 6AM,              
         BL    NPRMML02                                                         
         CLC   MEALSTRT,=H'600'    MEAL IS NOT DURING PREMIUM TIME              
         BNH   NPRMML05                                                         
*                                                                               
NPRMML02 CLC   TATMWTST,=H'600'    IF WORK START IS BETWEEN                     
         BL    NPRMMLX             8PM AND 6AM, MEAL IS EITHER                  
         CLC   TATMWTST,NPSTART    DURING PREM TIME OR AFTER                    
         BH    NPRMMLX                                                          
*                                                                               
         CLC   MEALSTRT,NPSTART    IF MEAL START IS BEFORE 8PM                  
         BH    NPRMMLX                                                          
         CLC   MEALSTRT,=H'600'    AND AFTER 6AM,                               
         BNH   NPRMMLX             THEN IT IS NOT DURING PREM TIME              
*                                                                               
         CLC   MEALSTRT,TATMWTNT   IF MEAL START IS BEFORE WORK END             
         BNL   NPRMML05                                                         
         CLC   MEALEND,TATMWTNT    AND, MEAL END IS AFTER WORK END              
         BNL   NPRMMLX             THEN MEAL IS NOT DURING PREM TIME            
         CLC   MEALEND,NPSTART     AND MEAL END IS BEFORE 8PM                   
         BNH   NPRMML10                                                         
         CLC   MEALSTRT,PRMEND     OR MEAL START IS AFTER PREMIUM END           
         BNL   NPRMMLX             THEN MEAL IS AFTER PREMIUM TIME              
*                                                                               
NPRMML05 CLC   MEALEND,NPSTART     DOES MEAL END BEFORE 8PM?                    
         BNH   NPRMML10                                                         
         MVC   STRTTIME,MEALSTRT   CALCULATE TIME BETWEEN MEAL START            
         MVC   ENDTIME,NPSTART     AND 8PM                                      
         BRAS  RE,TIMDIFF                                                       
*NO-OP   BRAS  RE,RNDQRTR          CHANGES DIFFTHRS                             
NPRMML10 BRAS  RE,HRS2DEC          CONVERT DIFFTHRS TO DECIMAL                  
         LH    RE,MEALNPH          ADD TO MEALNPH                               
         AR    RE,R1                                                            
         STH   RE,MEALNPH          HRS OF MEALS BEFORE NIGHT PREMIUM            
*                                                                               
NPRMMLX  XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE ANY PREMIUM TRAVEL TIME FOR EXTRAS                     
*              TRAVEL FROM ONLY                                                 
*              R4--> ELEMENT                                                    
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
PREMTRV  NTR1  BASE=*,LABEL=*                                                   
         XC    TRVLPH10,TRVLPH10                                                
         XC    TRVLPH20,TRVLPH20                                                
*                                                                               
         CLC   TATMTFDP,PRMEND     BETWEEN 8PM - PRMEND = PREMIUM               
         BL    PRMTRV10                                                         
         CLC   TATMTFDP,=H'2000'   BETWEEN PRMEND - 8PM = NOT PREMIUM           
         BL    PRMTRV40                                                         
         B     PRMTRV20            BETWEEN 8PM-1AM = 10% PREMIUM                
PRMTRV10 CLC   TATMTFDP,=H'100'    BETWEEN 1AM-PRMEND = 20% PREMIUM             
         BL    PRMTRV20                                                         
*                                                                               
         BRAS  RE,RNDQRTR          ROUND TO THE NEAREST QUARTER                 
         MVC   TRVLPH20,DIFFTHRS   TRVL IS DURING 20% PREMIUM TIME              
         CLC   TATMTFAR,PRMEND     IS TRVL FROM ARRIVE BEFORE PRMEND?           
         BNH   PRMTRVX                                                          
         MVC   STRTTIME,TATMTFDP   CALCULATE TIME BETWEEN START OF              
         MVC   ENDTIME,PRMEND      TRAVEL TO PRMEND                             
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR                                                       
         MVC   TRVLPH20,DIFFTHRS                                                
         B     PRMTRVX                                                          
*                                                                               
PRMTRV20 BRAS  RE,RNDQRTR          ROUND TO THE NEAREST QUARTER                 
         MVC   TRVLPH10,DIFFTHRS   TRVL IS DURING 10% PREMIUM TIME              
         CLC   TATMTFAR,=H'100'    IS TRVL FROM ARRIVE BEFORE 1AM?              
         BNH   PRMTRVX                                                          
         CLC   TATMTFAR,=H'2000'   IS TRVL BETWEEN 1AM AND 8PM?                 
         BL    PRMTRV30                                                         
         CLC   TATMTFAR,=H'2400'   OKAY IF BTWN 8PM AND 1AM                     
         BNH   PRMTRVX                                                          
PRMTRV30 MVC   STRTTIME,=H'100'    CALCULATE TIME BETWEEN 1AM AND               
         MVC   ENDTIME,TATMTFAR    TRAVEL FROM ARRIVE                           
         CLC   ENDTIME,PRMEND      IF TRVL FROM ARRIVE IS > PRMEND,             
         BNH   *+10                STOP AT PRMEND                               
         MVC   ENDTIME,PRMEND                                                   
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR                                                       
         MVC   TRVLPH20,DIFFTHRS                                                
         MVC   STRTTIME,TATMTFDP   CALCULATE TIME BETWEEN TRVL                  
         MVC   ENDTIME,=H'100'     FROM DEPART AND 1AM                          
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR                                                       
         MVC   TRVLPH10,DIFFTHRS   RECALCULATED                                 
         B     PRMTRVX                                                          
*                                                                               
*                                  IF NO PREMIUM TRAVEL HOURS SO FAR,           
PRMTRV40 CLC   TATMTFAR,=H'2000'   IS TRVL FROM ARRIVE BEFORE 8PM?              
         BNH   PRMTRVX                                                          
         MVC   STRTTIME,=H'2000'   CALCULATE TIME FROM 8 TILL TRVL              
         MVC   ENDTIME,TATMTFAR    FROM ARRIVE. IF ARRIVAL IS                   
         CLC   ENDTIME,PRMEND      BEFORE 1AM, ALL TIME = 10%                   
         BH    PRMTRV80                                                         
         CLC   ENDTIME,=H'100'                                                  
         BNH   PRMTRV80                                                         
         MVC   ENDTIME,=H'100'                                                  
PRMTRV80 BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR                                                       
         MVC   TRVLPH10,DIFFTHRS                                                
         CLC   ENDTIME,=H'100'     IF ARRIVAL IS AFTER 1AM,                     
         BNE   PRMTRVX             CALCULATE 20% PREM. TRVL TIME                
         MVC   STRTTIME,=H'100'    UP TILL PREMIUM END TIME                     
         MVC   ENDTIME,TATMTFAR                                                 
         CLC   ENDTIME,PRMEND                                                   
         BNH   *+10                                                             
         MVC   ENDTIME,PRMEND                                                   
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR                                                       
         MVC   TRVLPH20,DIFFTHRS                                                
*                                                                               
PRMTRVX  XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE ANY MEALS ON SATURDAY                                  
*              FOR SESSIONS THAT START ON FRIDAY                                
*              EXTRAS ONLY                                                      
*              STORES AMOUNT OF SAT MEAL TIME IN MEALSAT                        
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
GETSTML  NTR1  BASE=*,LABEL=*                                                   
         XC    MEALSAT,MEALSAT                                                  
         OC    TATMM1ST,TATMM1ST   IS MEAL 1 ON SATURDAY?                       
         BZ    GSTML20                                                          
         TM    TATMSTA2,TATMS2NM   IS MEAL 1 NON-DEDUCTIBLE?                    
         BNZ   GSTML20                                                          
         CLC   TATMM1ST,TATMWTNT   IF MEAL START > WORK END                     
         BNH   GSTML10             MEAL STARTS ON FRIDAY                        
         CLC   TATMM1NT,TATMWTNT   IF MEAL END > WORK END                       
         BH    GSTML20             MEAL ENDS ON FRIDAY                          
         MVC   STRTTIME,=H'2400'   START AT 12 MIDNIGHT                         
         B     *+10                                                             
GSTML10  MVC   STRTTIME,TATMM1ST   MEAL1 START                                  
         MVC   ENDTIME,TATMM1NT    MEAL1 END                                    
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE,                          
         BNO   GSTML15                                                          
         CLC   TATMM1ST,SVEND      IF MEAL STARTS AFTER 1ST 16 HOURS,           
         BH    GSTML20             SKIP IT                                      
         CLC   TATMM1NT,SVEND      IF MEAL ENDS AFTER 1ST 16 HOURS,             
         BNH   GSTML15             ONLY COUNT MEAL BEFORE 1ST 16 HRS            
         MVC   ENDTIME,SVEND                                                    
GSTML15  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
***      BRAS  RE,RNDQRTR          ROUND DIFFTHRS BY QUARTER HOUR               
         MVC   MEALSAT,DIFFTHRS    HRS OF MEALS ON SATURDAY                     
*                                                                               
GSTML20  OC    TATMM2ST,TATMM2ST   IS MEAL 2 ON SATURDAY?                       
         BZ    GSTML50                                                          
         CLC   TATMM2ST,TATMWTNT   IF MEAL START > WORK END                     
         BNH   GSTML30             MEAL STARTS ON FRIDAY                        
         CLC   TATMM2NT,TATMWTNT   IF MEAL END > WORK END                       
         BH    GSTML50             MEAL ENDS ON FRIDAY                          
         MVC   STRTTIME,=H'2400'   START AT 12 MIDNIGHT                         
         B     *+10                                                             
GSTML30  MVC   STRTTIME,TATMM2ST   MEAL2 START                                  
         MVC   ENDTIME,TATMM2NT    MEAL2 END                                    
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE,                          
         BNO   GSTML35                                                          
         CLC   TATMM2ST,SVEND      IF MEAL STARTS AFTER 1ST 16 HOURS,           
         BH    GSTML50             SKIP IT                                      
         CLC   TATMM2NT,SVEND      IF MEAL ENDS AFTER 1ST 16 HOURS,             
         BNH   GSTML35             ONLY COUNT MEAL BEFORE 1ST 16 HRS            
         MVC   ENDTIME,SVEND                                                    
GSTML35  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
****     BRAS  RE,RNDQRTR          ROUND DIFFTHRS BY QUARTER HOUR               
         BRAS  RE,HRS2MIN          CONVERT DIFFTHRS TO MINUTES                  
         OC    MEALSAT,MEALSAT                                                  
         BZ    GSTML40                                                          
         LH    R3,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALSAT    CONVERT MEAL1 SAT HRS TO MINS                
         BRAS  RE,HRS2MIN                                                       
         AH    R3,DIFFTMIN         MEAL1 SAT MINS + MEAL 2 SAT MINS             
         STH   R3,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT TO HRS                               
GSTML40  MVC   MEALSAT,DIFFTHRS    HRS OF MEALS ON SATURDAY                     
*                                                                               
GSTML50  OC    TATMM3ST,TATMM3ST   IS MEAL 3 ON SATURDAY?                       
         BZ    GSTMLX                                                           
         CLC   TATMM3ST,TATMWTNT   IF MEAL START > WORK END                     
         BNH   GSTML60             MEAL STARTS ON FRIDAY                        
         CLC   TATMM3NT,TATMWTNT   IF MEAL END > WORK END                       
         BH    GSTMLX              MEAL ENDS ON FRIDAY                          
         MVC   STRTTIME,=H'2400'   START AT 12 MIDNIGHT                         
         B     *+10                                                             
GSTML60  MVC   STRTTIME,TATMM3ST   MEAL3 START                                  
         MVC   ENDTIME,TATMM3NT    MEAL3 END                                    
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE,                          
         BNO   GSTML65                                                          
         CLC   TATMM3ST,SVEND      IF MEAL STARTS AFTER 1ST 16 HOURS,           
         BH    GSTMLX              SKIP IT                                      
         CLC   TATMM3NT,SVEND      IF MEAL ENDS AFTER 1ST 16 HOURS,             
         BNH   GSTML65             ONLY COUNT MEAL BEFORE 1ST 16 HRS            
         MVC   ENDTIME,SVEND                                                    
GSTML65  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL3 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
****     BRAS  RE,RNDQRTR          ROUND DIFFTHRS BY QUARTER HOUR               
         BRAS  RE,HRS2MIN          CONVERT DIFFTHRS TO MINUTES                  
         OC    MEALSAT,MEALSAT                                                  
         BZ    GSTML70                                                          
         LH    R3,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALSAT    CONVERT MEAL1+MEAL2 SAT HRS TO MINS          
         BRAS  RE,HRS2MIN                                                       
         AH    R3,DIFFTMIN         MEAL1+2 SAT MINS + MEAL 3 SAT MINS           
         STH   R3,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT TO HRS                               
GSTML70  MVC   MEALSAT,DIFFTHRS    HRS OF MEALS ON SATURDAY                     
*                                                                               
*                                                                               
GSTMLX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              DEDUCTS SATURDAY MEALS FROM MEALHRS TO GET FRI MEALS             
*              FOR SESSIONS THAT START ON FRIDAY                                
*              EXTRAS ONLY                                                      
*              STORES AMOUNT OF FRI MEAL TIME IN MEALFRI                        
*--------------------------------------------------------------------*          
GETFRML  NTR1  BASE=*,LABEL=*                                                   
         XC    MEALFRI,MEALFRI                                                  
         OC    MEALSAT,MEALSAT   IF NO SATURDAY MEALS,                          
         BNZ   GFRML10                                                          
         MVC   MEALFRI,MEALHRS   ALL MEAL HOURS ARE ON FRIDAY                   
         B     GFRMLX                                                           
GFRML10  MVC   DIFFTHRS,MEALHRS  TOTAL MEAL HOURS FOR SESSION                   
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALSAT  SATURDAY MEAL HOURS                            
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN       SUBTRACT SAT MEAL FROM TOTAL MEALS             
         BNP   GFRMLX                                                           
GFRML30  STH   R1,MEALFRI                                                       
GFRMLX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE NIGHT PREMIUM, BETWEEN STRTTIME AND ENDTIME            
*              SAG TIMESHEETS ONLY                                              
*              !! WARNING, ROUTINE CHANGES STRTTIME AND ENDTIME                 
*              R4---> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
NGHTPREM NTR1  BASE=*,LABEL=*                                                   
         MVC   STMNP10,SPACES                                                   
         MVC   STMNP20,SPACES                                                   
         XC    SVNPREM,SVNPREM                                                  
         XC    PREMHRS,PREMHRS                                                  
         XC    EX10NP,EX10NP                                                    
         XC    EX20NP,EX20NP                                                    
         MVI   NP6AMFLG,0          FORCED 6AM END FLAG                          
         MVI   NPEXFLG,0           SUBTRACTION FLAG                             
         MVI   NPFRIFLG,0          FRIDAY AFTER MIDNIGHT FLAG                   
*                                                                               
         TM    TATMSTA2,TATMS2NC   IF NON-CONSECUTIVE DAY,                      
         BO    NGHTPX              NO NIGHT PREMIUM                             
*                                                                               
         TM    TGCSORT,X'20'       EXTRAS ONLY - START WITH TRAVEL TIME         
         BNO   NGHTP10                                                          
         MVC   STRTTIME,TATMTTDP   TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP                                                
         BNZ   *+10                                                             
NGHTP10  MVC   STRTTIME,TATMWTST   START OF WORK                                
         MVC   ENDTIME,TATMWTNT    END OF WORK                                  
         TM    TGCSORT,X'20'       EXTRAS ONLY - IF TRAVEL DAY ONLY             
         BNO   NGHTP15             END WITH TRAVEL TIME                         
         OC    TATMWTNT,TATMWTNT                                                
         BNZ   *+10                                                             
         MVC   ENDTIME,TATMTTAR    TRAVEL TO -ARRIVE                            
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE?                             
         BNO   NGHTP15             NO NIGHT PREMIUM AFTER 16 HOURS              
         ZICM  R3,STRTTIME,2                                                    
         AHI   R3,1600             ADD 16 HOURS TO START TIME                   
         STCM  R3,3,ENDTIME                                                     
         ZICM  R0,TATMTIDP,2                                                    
         OC    TATMTIDP,TATMTIDP   ANY TRAVEL INTERVENING?                      
         BNZ   NGHTP12                                                          
         ZICM  R0,TATMTFDP,2                                                    
         OC    TATMTFDP,TATMTFDP   ANY TRAVEL FROM?                             
         BZ    NGHTP15                                                          
NGHTP12  CHI   R0,1200             IF TRVL < 1200,                              
         BNL   *+8                                                              
         AHI   R0,2400             ADD 2400 TO NUMBER                           
         CR    R0,R3               IF TRAVEL INT/FROM STARTS                    
         BNL   NGHTP15             BEFORE 16 HOURS ENDS,                        
         STCM  R0,3,ENDTIME        NP ENDTIME = TRVL INT/FROM START             
*                                                                               
NGHTP15  OC    STRTTIME,STRTTIME                                                
         BZ    NGHTPX                                                           
         ZICM  R1,STRTTIME,2                                                    
         ZICM  R2,ENDTIME,2                                                     
         LR    RF,R2               HOLD ORIGINAL ENDTIME                        
         CLC   ENDTIME,STRTTIME    IF ENDTIME < STRTTIME                        
         BNL   *+8                                                              
         AHI   R2,2400             ADD 24 HOURS TO ENDTIME                      
*                                                                               
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA,                  
         BNO   NGHTP20                                                          
         CHI   R1,600              IF STARTING ON OR AFTER 6AM                  
         BL    NGHTP35                                                          
NGHTP20  CHI   R1,500              IF STARTING ON OR AFTER 5AM                  
         BL    NGHTP30                                                          
         CHI   R1,2000             BUT STARTS BEFORE 8PM                        
         BNL   NGHTP30                                                          
         CHI   R2,2000             AND ENDS BEFORE 8PM                          
         BNH   NGHTP190            THEN NO NIGHT PREMIUM                        
*                                                                               
NGHTP30  CHI   R1,500              STARTS AFTER 5AM-8PM?                        
         BL    NGHTP35                                                          
         CHI   R1,2000                                                          
         BH    *+8                                                              
         LHI   R1,2000             ASSUME START AT 8PM                          
*                                                                               
NGHTP35  TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA                   
         BNO   NGHTP40                                                          
         MVI   NP6AMFLG,C'N'                                                    
         CHI   R1,200              STARTS BEFORE 2AM?                           
         BL    NGHTP50                                                          
         CHI   R1,2000             (NOT BEFORE 8PM)                             
         BNL   NGHTP50             NP 20% STOPS AT REAL END TIME                
*                                                                               
NGHTP40  CHI   RF,600              ENDTIME IN 6A-8PM?                           
         BL    NGHTP50                                                          
         CHI   RF,2000                                                          
         BH    *+12                                                             
         LHI   RF,600              ASSUME END AT 6AM                            
         MVI   NP6AMFLG,C'Y'       SET FORCED 6AM END FLAG                      
*                                                                               
NGHTP50  CLC   ENDTIME,STRTTIME                                                 
         BNL   *+8                                                              
         AHI   RF,2400                                                          
*                                                                               
         CHI   R1,600              IF HOURS INCLUDE 6A-8P                       
         BH    NGHTP70                                                          
         CHI   R2,2000                                                          
         BH    NGHTP90                                                          
NGHTP70  CHI   R1,3000             IF HOURS INCLUDE 6A-8P                       
         BNL   NGHTP100                                                         
         CHI   R2,4400                                                          
         BNH   NGHTP100                                                         
NGHTP90  MVI   NPEXFLG,C'Y'        SET EXTRA NP FLAG                            
         TM    TGCSORT,X'20'       IF CAST MEMBER IS NOT AN EXTRA,              
         BO    NGHTP110                                                         
*NO-OP   SHI   RF,1400             SUBTRACT 14 HOURS                            
         LHI   RF,600              ASSUME END AT 6AM                            
         MVI   NP6AMFLG,C'Y'       SET FORCED 6AM END FLAG                      
*                                                                               
NGHTP100 TM    TGCSORT,X'20'       IF CAST MEMBER IS NOT AN EXTRA               
         BO    NGHTP110                                                         
         BRAS  RE,ADJMPH           ADJUST PREMIUM MEAL HOURS                    
         STCM  R1,3,STRTTIME                                                    
         STCM  RF,3,ENDTIME                                                     
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR          SETS DIFFTHRS FOR CALCPREM                   
         MVC   MEALPHRS,MEALPH10   10% PREM HRS ONLY FOR NON-EXTRAS             
         MVC   MLNPHTOT,MEALNPH    MEAL NON PREMIUM HOURS TOTAL                 
         BRAS  RE,CALCPREM         CALCULATE AMOUNT OF PREM TIME HRS            
         MVC   TATMNP10,SVNPREM    # OF PREMIUM HOURS                           
         MVC   SVNP10,PREMHRS      ADJUSTED # PREMIUM HOURS                     
*                                                                               
         CLI   NPEXFLG,C'Y'        EXTRA NP?                                    
         BNE   *+8                                                              
         BRAS  RE,ADJPREM          ADJUST NIGHT PREMIUM HOURS                   
*                                                                               
         B     NGHTP190                                                         
*                                                                               
*        EXTRAS GET PAID 10% OF HOURLY RATE FROM 8PM-1AM                        
*        AND 20% OF HOURLY RATE FROM 1AM-6AM                                    
NGHTP110 CLC   ENDTIME,STRTTIME    IF ENDTIME < STRTTIME,                       
         BNL   NGHTP111                                                         
         CLI   NP6AMFLG,C'N'       IF NOT FORCING 6AM END TIME,                 
         BNE   *+8                                                              
         MVI   NPEXFLG,C'N'        DO NOT ADD EXTRA NP                          
*                                                                               
NGHTP111 CLI   NPEXFLG,C'Y'        EXTRA NP?                                    
         BNE   *+12                                                             
         BRAS  RE,SETTIME          CALCULATE START AND END HOURS                
         BE    NGHTP130                                                         
         CHI   R1,2500             IF START TIME IS BEFORE 1AM                  
         BNL   NGHTP180                                                         
         CHI   R1,600              CHECK NOT BETWEEN 1 AND 6 AM                 
         BNL   NGHTP112                                                         
         CHI   R1,100                                                           
         BNL   NGHTP180                                                         
NGHTP112 CHI   RF,2500             AND END TIME IS AFTER 1AM                    
         BNH   *+10                                                             
         MVC   ENDTIME,=H'2500'    SET END TIME AS 1AM AND CALC 10%             
         TM    DAYTYPE,DAYFRI      FRIDAY?                                      
         BNO   NGHTP120                                                         
         CHI   RF,2400             AND END TIME IS AFTER 12 MIDNIGHT            
         BNH   NGHTP120                                                         
         MVC   ENDTIME,=H'2400'    SET END TIME AS 12M AND CALC 10%             
NGHTP120 CHI   R1,100              IF START TIME IS BETWEEN 12M AND 1AM         
         BNL   NGHTP125                                                         
         MVC   ENDTIME,=H'100'     SET END TIME AS 1AM AND CALC 10%             
NGHTP125 STCM  R1,3,STRTTIME                                                    
NGHTP130 BRAS  RE,TIMDIFF                                                       
         TM    DAYTYPE,DAYFRI      IF FRIDAY                                    
         BNO   NGHTP131                                                         
         CLI   NPEXFLG,C'Y'                                                     
         BE    NGHTP131                                                         
         OC    SVML10,SVML10       AND MEALS 12M-1A, DON'T ROUND                
         BZ    NGHTP131                                                         
         MVI   NPFRIFLG,C'Y'       SET FLAG                                     
         B     *+8                                                              
NGHTP131 BRAS  RE,RNDQRTR          SETS DIFFTHRS FOR CALCPREM                   
         MVC   MEALPHRS,MEALPH10   USE 10% PREMIUM HOURS MEAL                   
         MVC   MLNPHTOT,MEALNPH    MEAL NON PREMIUM HOURS TOTAL                 
         BRAS  RE,CALCPREM         CALCULATE # OF 10% PREMIUM TIME HRS          
         MVC   TATMNP10,SVNPREM    # OF 10% PREMIUM HOURS                       
         MVC   SVNP10,PREMHRS      ADJUSTED # OF 10% PREMIUM HOURS              
*                                                                               
         TM    TATMSTA2,TATMS2DL   DISTANT LOCATION?                            
         BNZ   NGHTP133            TREAT AS WEEKDAY                             
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF NOT HOLIDAY,SAT/SUN,           
         BNZ   NGHTP135                                                         
NGHTP133 OC    EX10NP,EX10NP       ANY EXTRA NP10%?                             
         BZ    NGHTP135                                                         
         MVC   DIFFTHRS,EX10NP     EXTRA NP10% IS ALWAYS DOUBLETIME             
         BRAS  RE,HRS2DEC                                                       
         ZICM  R0,SVNP10,2                                                      
         AR    R0,R1               ADD EXTRA NP10% AGAIN TO SVNP10              
         STCM  R0,3,SVNP10                                                      
*                                                                               
NGHTP135 XC    SVNPREM,SVNPREM                                                  
         XC    PREMHRS,PREMHRS                                                  
*                                                                               
NGHTP140 TM    DAYTYPE,DAYFRI      FRIDAY? ALL NP10% FROM 12M - 1AM AND         
         BNO   NGHTP145            ALL NP20% IS DOUBLED                         
         BRAS  RE,ADJNPFR          ADJUST NGHT PREM FOR EXTRAS ON FRI           
         BNE   NGHTP145            IF ALL ON FRIDAY - USE REG RULES             
         BRAS  RE,ADJNPFR2         ADJUST NGHT PREM FOR EXTRAS ON FRI           
         B     NGHTP190                                                         
*                                                                               
NGHTP145 CLI   NPEXFLG,C'Y'                                                     
         BNE   NGHTP170                                                         
         OC    TATMTTDP,TATMTTDP   ANY TRAVEL TO DEPART?                        
         BZ    NGHTP148                                                         
         CLC   TATMTTDP,=H'600'    TRAVEL TO DURING 20%NP (1-6AM)?              
         BNL   NGHTP150                                                         
         CLC   TATMTTDP,=H'100'                                                 
         BNL   NGHTP160                                                         
         B     NGHTP150                                                         
*                                                                               
NGHTP148 CLC   TATMWTST,=H'600'    WORK START DURING 20%NP (1-6AM)?             
         BNL   NGHTP150                                                         
         CLC   TATMWTST,=H'100'                                                 
         BNL   NGHTP160                                                         
*                                                                               
NGHTP150 MVC   STRTTIME,=H'2500'   START TIME IS 1AM                            
         LHI   RF,3000             END TIME IS 6AM                              
         CLI   NP6AMFLG,C'Y'       FORCED 6AM END?                              
         BE    NGHTP180                                                         
         LR    RF,R2               END TIME IS REAL END TIME                    
         B     NGHTP180                                                         
*                                                                               
NGHTP160 STCM  R1,3,STRTTIME       START TIME IS BEGINNING OF DAY               
         MVC   HRMIN1,=H'600'                                                   
         MVC   HRMIN2,EX20NP       ADD ON ANY EXTRA 20% NP                      
         BRAS  RE,ADDHRS                                                        
         LH    RF,DIFFTHRS                                                      
         B     NGHTP180                                                         
*                                                                               
NGHTP170 CHI   RF,2500             IF END TIME IS BEFORE 1AM, NO 20%            
         BH    NGHTP175                                                         
         CHI   R1,100              UNLESS START TIME IS BETWEEN 12-1AM          
         BH    NGHTP190                                                         
         CLC   TATMWTNT,=H'600'    IF END TIME IS BETWEEN 1AM - 6AM             
         BH    NGHTP173                                                         
         CLC   TATMWTNT,=H'100'                                                 
         BNH   NGHTP190                                                         
         ZICM  RF,TATMWTNT,2       USE END TIME                                 
         B     NGHTP175                                                         
NGHTP173 CLI   NP6AMFLG,C'N'       IF NOT FORCING 6AM, USE REAL ENDTIME         
         BE    NGHTP175                                                         
         LHI   RF,3000             IF AFTER 6AM, SET END TIME AS 6AM            
NGHTP175 MVC   STRTTIME,=H'2500'   SET START TIME AS 1AM AND CALC 20%           
         OC    TATMTTDP,TATMTTDP   ANY TRAVEL TO DEPART?                        
         BZ    NGHTP178                                                         
         CLC   TATMTTDP,=H'100'    IF TRAVEL TO DEPART                          
         BH    NGHTP180                                                         
         B     NGHTP179                                                         
NGHTP178 CLC   TATMWTST,=H'100'    OR WORK START < = 100                        
         BH    NGHTP180                                                         
NGHTP179 MVC   STRTTIME,=H'100'    START NP20% AT 100 INSTEAD OF 2500           
NGHTP180 BRAS  RE,SETMEAL          SET NON PREM MEAL HOURS FOR 20% CALC         
         STCM  RF,3,ENDTIME                                                     
*                                                                               
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE AND                       
         BNO   NGHTP185                                                         
         CLC   ENDTIME,=H'2400'                                                 
         BH    NGHTP183                                                         
         CLC   ENDTIME,=H'600'     IF FORCED ENDTIME IS AFTER 6AM               
         BNH   NGHTP185                                                         
         MVC   ENDTIME,=H'600'     SET ENDTIME TO 6AM                           
         B     NGHTP185                                                         
NGHTP183 CLC   ENDTIME,=H'3000'    IF FORCED ENDTIME IS AFTER 6AM               
         BNH   NGHTP185                                                         
         MVC   ENDTIME,=H'3000'    SET ENDTIME TO 6AM                           
*                                                                               
NGHTP185 CLC   STRTTIME,ENDTIME    IF START TIME > ENDTIME,                     
         BNH   NGHTP186                                                         
         ZICM  RE,ENDTIME,2                                                     
         AHI   RE,2400             ADD 2400 TO ENDTIME                          
         STCM  RE,3,ENDTIME                                                     
NGHTP186 BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR          SETS DIFFTHRS FOR CALCPREM                   
         MVC   MEALPHRS,MEALPH20   USE 20% PREMIUM HOURS MEAL                   
         BRAS  RE,CALCPREM         CALCULATE # OF 20% PREMIUM TIME HRS          
         MVC   TATMNP20,SVNPREM    # OF 20% PREMIUM HOURS                       
         MVC   SVNP20,PREMHRS      ADJUSTED # OF 20% PREMIUM HOURS              
*                                                                               
         TM    TATMSTA2,TATMS2DL   DISTANT LOCATION?                            
         BNZ   NGHTP187            TREAT AS WEEKDAY                             
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF NOT HOLIDAY,SAT/SUN,           
         BNZ   NGHTP190                                                         
NGHTP187 OC    EX20NP,EX20NP       ANY EXTRA NP20%?                             
         BZ    NGHTP190                                                         
         MVC   DIFFTHRS,EX20NP     EXTRA NP20% IS ALWAYS DOUBLETIME             
         BRAS  RE,HRS2DEC                                                       
         ZICM  R0,SVNP20,2                                                      
         AR    R0,R1               ADD EXTRA NP20% AGAIN TO SVNP20              
         STCM  R0,3,SVNP20                                                      
*                                                                               
NGHTP190 DS    0H                                                               
*                                                                               
****     BRAS  RE,ADDTRVL          ADD TRAVEL PREMIUM HOURS IF ANY              
*                                                                               
         OC    SVNP10,SVNP10       DO WE HAVE NIGHT PREMIUM HOURS?              
         BNZ   NGHTP200                                                         
         OC    SVNP20,SVNP20                                                    
         BZ    NGHTPX                                                           
*                                                                               
NGHTP200 BRAS  RE,NGHTPYN          NIGHT PREMIUM YES/NO                         
*                                                                               
NGHTPX   OI    STMNP10H+6,X'80'                                                 
         OI    STMNP20H+6,X'80'                                                 
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE AMOUNT OF PREMIUM TIME HOURS                *          
*              R4 ---> ELEM                                          *          
*              INPUT = DIFFTHRS --> NUMBER OF PREM TIME W/MEALS      *          
*              R3 KEEPS TRACK OF REMAINING PREMIUM TIME              *          
*              TIME AND A HALF HOURS ARE MULTIPLIED BY 1.5           *          
*              DOUBLE TIME HOURS ARE MULTIPLIED BY 2                 *          
*              PREMHRS RETURNED W/ NUMBER OF HRS TO X BY HOURLY RATE *          
*              SVNPREM RETURNED W/ NUMBER OF PRINTABLE HOURS         *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CALCPREM NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,HRS2MIN            CONVERT DIFFTHRS TO MINUTES                
         LH    R0,DIFFTMIN           R0 = # OF MINS OF PREM. HRS.               
         MVC   DIFFTHRS,MEALPHRS     # OF PREMIUM MEAL HRS                      
         BRAS  RE,HRS2MIN            CONVERT TO MINUTES                         
         SH    R0,DIFFTMIN           SUBTRACT PREMIUM MEAL HOURS                
         STH   R0,DIFFTMIN           DIFFERENCE IN MINS                         
         BRAS  RE,MIN2HRS            CONVERT BACK TO HOURS                      
*                                                                               
         CLI   NPFRIFLG,C'Y'         IF EXTRA FRI TO SAT, DON'T ROUND           
         BE    *+8                                                              
         BRAS  RE,RNDQRTR            ROUND TO NEAREST QUARTER HOUR              
         MVC   SVNPREM,DIFFTHRS      SAVE # OF PREMIUM HOURS                    
         BRAS  RE,HRS2DEC            CONVERT TO DECIMAL NUMBER                  
         LR    R3,R1                 R3=# OF PREMIUM HOURS (DECIMAL)            
*                                                                               
         SR    R1,R1                                                            
         TM    TGCSORT,X'20'         EXTRAS - START WITH TRAVEL TIME            
         BNO   CLPRM10                                                          
         ZICM  RF,TATMTTDP,2         TRAVEL TO - DEPART                         
         OC    TATMTTDP,TATMTTDP                                                
         BNZ   CLPRM11                                                          
CLPRM10  ZICM  RF,TATMWTST,2         WORK START                                 
CLPRM11  AHI   RF,800                ADD 8 HRS TO WORK START                    
         STH   RF,DIFFTHRS                                                      
         BRAS  RE,HRS2DEC                                                       
         LR    RF,R1                                                            
         AH    RF,MLNPHTOT           ADD ANY MEALS BEFORE PREM HOURS            
         MVC   DIFFTHRS,STRTTIME                                                
         BRAS  RE,HRS2DEC                                                       
         SR    RF,R1                 RF= # OF STRAIGHT TIME HRS                 
         CHI   RF,0                  SEE IF WE HAVE STRAIGHT TIME HRS           
         BNL   *+6                                                              
         SR    RF,RF                                                            
         LR    R5,RF                 R5 = KEEP TRACK OF ADJUSTED HRS            
*                                                                               
         CR    R3,RF                 IF # PREM HRS < # OF STRAIGHT TIME         
         BNL   *+10                  HRS, THEN # PREM HRS = # ADJ. HRS          
         LR    R5,R3                                                            
         B     CLPRM50                                                          
*                                                                               
         SR    R3,RF                 PREM TIME MINUS STRAIGHT TIME              
         LR    R2,RF                 R2 --> STRAIGHT TIME IN DECIMAL            
         TM    TGCSORT,X'20'         EXTRAS - START WITH TRAVEL TIME            
         BNO   CLPRM15                                                          
         ZICM  RF,TATMTTDP,2         TRAVEL TO - DEPART                         
         OC    TATMTTDP,TATMTTDP                                                
         BNZ   CLPRM16                                                          
CLPRM15  ZICM  RF,TATMWTST,2         WORK START                                 
CLPRM16  AHI   RF,1000               ADD 10 HRS TO WORK START                   
         STH   RF,DIFFTHRS                                                      
         BRAS  RE,HRS2DEC                                                       
         LR    RF,R1                                                            
         AH    RF,MLNPHTOT           ADD ANY MEALS BEFORE PREM HOURS            
         MVC   DIFFTHRS,STRTTIME                                                
         BRAS  RE,HRS2DEC                                                       
         SR    RF,R1                 SUBTRACT DECIMAL START TIME                
         SR    RF,R2                 AND STRAIGHT HRS. RF=# OF 1.5 HRS          
         CHI   RF,0                  SEE IF WE HAVE TIME AND A HALF HRS         
         BNH   CLPRM40                                                          
         CR    RF,R3                 IF RF < REMAINING PREMIUM TIME,            
         BNH   CLPRM20               REMAIN PREM TIME = # OF 1.5 HRS            
         LR    R1,R3                 R1 = TIME AND A HALF IN DECIMAL            
         B     *+6                                                              
CLPRM20  LR    R1,RF                 R1 = TIME AND A HALF IN DECIMAL            
*                                                                               
         SR    R3,R1                 REMAINING PREMIUM TIME                     
         TM    TATMSTA2,TATMS2DL     DISTANT LOCATION?                          
         BNZ   CLPRM25               TREAT AS WEEKDAY                           
         TM    TGCSORT,X'08'         IF CAST MEMBER IS OFF CAMERA,              
         BO    CLPRM30               ALL HOURS ARE PAID STRAIGHT                
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF HOLIDAY,SAT/SUN,               
         BNZ   CLPRM30               ALL HOURS GET SAME RATE                    
CLPRM25  LR    RE,R1                 CALCULATE TIME AND A HALF                  
         SR    R0,R0                                                            
         D     R0,=F'2'                                                         
         CHI   R0,1                  IF REMAINDER,                              
         BL    *+8                                                              
         AHI   R1,1                  ROUND UP                                   
         AR    R1,RE                 R1 = HRS*1.5  (TIME AND A HALF)            
CLPRM30  AR    R5,R1                 R2=# OF STRAIGHT + 1.5 HOURS               
*                                                                               
CLPRM40  TM    TATMSTA2,TATMS2DL     DISTANT LOCATION?                          
         BNZ   CLPRM43               TREAT AS WEEKDAY                           
         TM    TGCSORT,X'08'         IF CAST MEMBER IS OFF CAMERA,              
         BO    CLPRM45               ALL HOURS ARE PAID STRAIGHT                
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF HOLIDAY/SAT/SUN,               
         BNZ   CLPRM45               ALL HOURS GET SAME RATE                    
CLPRM43  MHI   R3,2                  REMAINING PREM TIME IS DOUBLE TIME         
CLPRM45  AR    R5,R3                 ADD THIS TO TOTAL PREM HRS                 
CLPRM50  STCM  R5,3,PREMHRS          PREMIUM TIME HOURS (ADJUSTED)              
         XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE NIGHT PREMIUM, BETWEEN STRTTIME AND ENDTIME            
*              ACTRA TIMESHEETS ONLY                                            
*              !! WARNING, ROUTINE CHANGES STRTTIME AND ENDTIME                 
*              R4---> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
NGPRMACT NTR1  BASE=*,LABEL=*                                                   
         MVC   STMNP10,SPACES                                                   
         MVC   STMNP20,SPACES                                                   
         XC    SVNPREM,SVNPREM                                                  
         XC    SVNPSAG,SVNPSAG                                                  
         XC    PREMHRS,PREMHRS                                                  
         XC    EX10NP,EX10NP                                                    
         XC    EX20NP,EX20NP                                                    
         MVI   NP6AMFLG,0          FORCED 6AM END FLAG                          
         MVI   NPEXFLG,0           SUBTRACTION FLAG                             
*                                                                               
         MVC   STRTTIME,TATMWTST   START OF WORK                                
         MVC   ENDTIME,TATMWTNT    END OF WORK                                  
*                                                                               
         OC    STRTTIME,STRTTIME                                                
         BZ    NGPRAX                                                           
         ZICM  R1,STRTTIME,2                                                    
         ZICM  R2,ENDTIME,2                                                     
         LR    RF,R2               HOLD ORIGINAL ENDTIME                        
         CLC   ENDTIME,STRTTIME    IF ENDTIME < STRTTIME                        
         BNL   *+8                                                              
         AHI   R2,2400             ADD 24 HOURS TO ENDTIME                      
*                                                                               
         CLI   SVACTRA,CCTY04A     2404A ONLY                                   
         BNE   NGPRA33                                                          
         CHI   R1,500              IF STARTING ON OR AFTER 5AM                  
         BL    NGPRA30                                                          
         CHI   R1,2300             BUT STARTS BEFORE 11PM                       
         BNL   NGPRA30                                                          
         CHI   R2,2300             AND ENDS BEFORE 11PM                         
         BNH   NGPRA190            THEN NO NIGHT PREMIUM                        
                                                                                
NGPRA30  CHI   R1,500              STARTS AFTER 5AM-11PM?                       
         BL    NGPRA40                                                          
         CHI   R1,2300                                                          
         BH    *+8                                                              
         LHI   R1,2300             ASSUME START AT 11PM                         
         B     NGPRA40                                                          
*                                  ACTRA EXCEPT 2404A                           
NGPRA33  CHI   R1,600              IF STARTING ON OR AFTER 6AM                  
         BL    NGPRA35                                                          
         CHI   R1,2300             BUT STARTS BEFORE 11PM                       
         BNL   NGPRA35                                                          
         CHI   R2,2300             AND ENDS BEFORE 11PM                         
         BNH   NGPRA190            THEN NO NIGHT PREMIUM                        
*                                                                               
NGPRA35  CHI   R1,600              STARTS AFTER 6AM-11PM?                       
         BL    NGPRA40                                                          
         CHI   R1,2300                                                          
         BH    *+8                                                              
         LHI   R1,2300             ASSUME START AT 11PM                         
*                                                                               
NGPRA40  CHI   RF,600              ENDTIME IN 6A-11PM?                          
         BL    NGPRA50                                                          
         CHI   RF,2300                                                          
         BH    *+12                                                             
         LHI   RF,600              ASSUME END AT 6AM                            
         MVI   NP6AMFLG,C'Y'       SET FORCED 6AM END FLAG                      
*                                                                               
NGPRA50  CLC   ENDTIME,STRTTIME                                                 
         BNL   *+8                                                              
         AHI   RF,2400                                                          
*                                                                               
         CHI   R1,600              IF HOURS INCLUDE 6A-11P                      
         BH    NGPRA70                                                          
         CHI   R2,2300                                                          
         BNH   NGPRA70                                                          
         B     NGPRA90                                                          
NGPRA70  CHI   R1,3000             IF HOURS INCLUDE 6A-11P                      
         BNL   NGPRA100                                                         
         CHI   R2,4700                                                          
         BNH   NGPRA100                                                         
NGPRA90  MVI   NPEXFLG,C'Y'        SET EXTRA NP FLAG                            
         LHI   RF,600              ASSUME END AT 6AM                            
         MVI   NP6AMFLG,C'Y'       SET FORCED 6AM END FLAG                      
*                                                                               
NGPRA100 BRAS  RE,ADJMPH           ADJUST PREMIUM MEAL HOURS                    
         STCM  R1,3,STRTTIME                                                    
         STCM  RF,3,ENDTIME                                                     
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,RNDQRTR          SETS DIFFTHRS FOR CALCPREM                   
         MVC   MEALPHRS,MEALPH10   10% PREM HRS ONLY FOR NON-EXTRAS             
         MVC   MLNPHTOT,MEALNPH    MEAL NON PREMIUM HOURS TOTAL                 
         BRAS  RE,CLPRACT          CALC AMOUNT OF PREM HRS FOR ACTRA            
         CLI   SVACTRA,CCTY04A                                                  
         BE    NGPRA110                                                         
         MVC   DIFFTHRS,SVNPREM    # OF PREMIUM ACTRA HOURS                     
         BRAS  RE,RNDHR2           ROUND UP TO THE NEAREST HOUR                 
         MVC   TATMNPAC,DIFFTHRS   # OF PREMIUM ACTRA HOURS                     
         B     NGPRA120                                                         
*                                                                               
NGPRA110 CLI   FIRSTDY,C'Y'        2404A - NOT 1ST DAY OF SESSION?              
         BE    NGPRA115                                                         
         MVC   TATMNPSG,SVNPREM    # OF PREMIUM SAG HOURS                       
         B     NGPRA120                                                         
*                                                                               
NGPRA115 MVC   DIFFTHRS,SVNPST     DECIMAL NP ACTRA HRS (STRAIGHT ONLY)         
         BRAS  RE,DEC2HRS          CONVERT NP ACTRA HRS TO HRS.MIN              
         BRAS  RE,RNDHR2           ROUND UP TO THE NEAREST HOUR                 
         MVC   TATMNPAC,DIFFTHRS   # OF PREMIUM ACTRA HOURS                     
         MVC   DIFFTHRS,SVNPSAG    DECIMAL NP SAG HRS                           
         BRAS  RE,DEC2HRS          CONVERT NP SAG HRS TO HRS.MIN                
         MVC   TATMNPSG,DIFFTHRS   # OF PREMIUM SAG HOURS                       
*                                                                               
NGPRA120 CLI   NPEXFLG,C'Y'        EXTRA NP?                                    
         BNE   *+8                                                              
         BRAS  RE,ADJPREM          ADJUST NIGHT PREMIUM HOURS                   
*                                                                               
NGPRA190 OC    SVNP25A,SVNP25A     DO WE HAVE NIGHT PREMIUM HOURS?              
         BNZ   NGPRA200                                                         
         OC    SVNP25S,SVNP25S                                                  
         BNZ   NGPRA200                                                         
         OC    SVNPDT,SVNPDT                                                    
         BZ    NGPRAX                                                           
*                                                                               
NGPRA200 BRAS  RE,NGHTPYN          NIGHT PREMIUM YES/NO                         
*                                                                               
NGPRAX   OI    STMNP10H+6,X'80'                                                 
         OI    STMNP20H+6,X'80'                                                 
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE AMOUNT OF PREMIUM TIME HOURS (ACTRA)        *          
*              R4 ---> ELEM                                          *          
*              INPUT = DIFFTHRS --> NUMBER OF PREM TIME W/MEALS      *          
*              R3 KEEPS TRACK OF REMAINING PREMIUM TIME              *          
*              TIME AND A HALF HOURS ARE MULTIPLIED BY 1.5           *          
*              DOUBLE TIME HOURS ARE MULTIPLIED BY 2                 *          
*              SVNPREM RETURNED W/ NUMBER OF PRINTABLE HOURS         *          
*              STRAIGHT, OT, AND DT HOURS ARE STORED SEPARATELY IN   *          
*              SVNPST, SVNPOT, AND SVNPDT                            *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CLPRACT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,HRS2MIN            CONVERT DIFFTHRS TO MINUTES                
         LH    R0,DIFFTMIN           R0 = # OF MINS OF PREM. HRS.               
         MVC   DIFFTHRS,MEALPHRS     # OF PREMIUM MEAL HRS                      
         BRAS  RE,HRS2MIN            CONVERT TO MINUTES                         
         SH    R0,DIFFTMIN           SUBTRACT PREMIUM MEAL HOURS                
         STH   R0,DIFFTMIN           DIFFERENCE IN MINS                         
         BRAS  RE,MIN2HRS            CONVERT BACK TO HOURS                      
         MVC   SVNPREM,DIFFTHRS      SAVE # OF PREMIUM HOURS                    
         BRAS  RE,HRS2DEC            CONVERT TO DECIMAL NUMBER                  
         LR    R3,R1                 R3=# OF PREMIUM HOURS (DECIMAL)            
*                                                                               
         SR    R1,R1                                                            
         ZICM  RF,TATMWTST,2         WORK START                                 
*                                                                               
*                               ***ALL SESSIONS ARE 8 HOURS DEC/08***           
*&&DO                                                                           
         TM    SVDOBST,MINOR16       MINOR UNDER 16?                            
         BO    CLPRA05                                                          
         CLI   SVACTRA,CCTY04A       2404A?                                     
         BNE   CLPRA10                                                          
         CLI   FIRSTDY,C'Y'          NOT 1ST DAY OF SESSION?                    
         BE    CLPRA10                                                          
*&&                                                                             
CLPRA05  AHI   RF,800                ADD 8 HRS TO WORK START                    
***      B     *+8                                                              
***RA10  AHI   RF,900                ADD 9 HRS TO WORK START                    
*                                                                               
         STH   RF,DIFFTHRS                                                      
         BRAS  RE,HRS2DEC                                                       
         LR    RF,R1                                                            
         AH    RF,MLNPHTOT           ADD ANY MEALS BEFORE PREM HOURS            
         MVC   DIFFTHRS,STRTTIME                                                
         BRAS  RE,HRS2DEC                                                       
         SR    RF,R1                 RF= # OF STRAIGHT TIME HRS                 
         CHI   RF,0                  SEE IF WE HAVE STRAIGHT TIME HRS           
         BNL   *+6                                                              
         SR    RF,RF                                                            
         LR    R5,RF                 R5 = KEEP TRACK OF ADJUSTED HRS            
CLPRA14  STCM  R5,3,SVNPST           SAVE ACTRA NP STRAIGHT TIME HOURS          
*                                                                               
CLPRA15  CR    R3,RF                 IF # PREM HRS < # OF STRAIGHT TIME         
         BNL   CLPRA16               HRS, THEN # PREM HRS = # ADJ. HRS          
         LR    R5,R3                                                            
         STCM  R5,3,SVNPST           RESET ACTRA NP STRAIGHT TIME HOURS         
         B     CLPRAX                                                           
*                                                                               
CLPRA16  SR    R3,RF                 PREM TIME MINUS STRAIGHT TIME              
         CLI   FIRSTDY,C'Y'          2404A, 1ST DAY OF SESSION?                 
         BNE   CLPRA18                                                          
         STCM  R3,3,SVNPSAG          SAG NP HOURS (OT + DT)                     
*                                                                               
CLPRA18  LR    R2,RF                 R2 --> STRAIGHT TIME IN DECIMAL            
         ZICM  RF,TATMWTST,2         WORK START                                 
         AHI   RF,1000               ADD 10 HRS TO WORK START                   
         STH   RF,DIFFTHRS                                                      
         BRAS  RE,HRS2DEC                                                       
         LR    RF,R1                                                            
         AH    RF,MLNPHTOT           ADD ANY MEALS BEFORE PREM HOURS            
         MVC   DIFFTHRS,STRTTIME                                                
         BRAS  RE,HRS2DEC                                                       
         SR    RF,R1                 SUBTRACT DECIMAL START TIME                
         SR    RF,R2                 AND STRAIGHT HRS. RF=# OF 1.5 HRS          
         CHI   RF,0                  SEE IF WE HAVE TIME AND A HALF HRS         
         BNH   CLPRA30                                                          
         CR    RF,R3                 IF RF < REMAINING PREMIUM TIME,            
         BNH   CLPRA20               REMAIN PREM TIME = # OF 1.5 HRS            
         LR    R1,R3                 R1 = TIME AND A HALF IN DECIMAL            
         B     *+6                                                              
CLPRA20  LR    R1,RF                 R1 = TIME AND A HALF IN DECIMAL            
         STCM  R1,3,SVNPOT           SAVE ACTRA NP OVERTIME HOURS               
*                                                                               
         SR    R3,R1                 REMAINING PREMIUM TIME                     
CLPRA30  STCM  R3,3,SVNPDT           SAVE ACTRA NP DOUBLETIME HOURS             
*                                                                               
CLPRAX   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*--------------------------------------------------------------------*          
*              CALCULATE AMOUNT OF PREMIUM TIME HOURS (ACTRA)        *          
*              R4 ---> ELEM                                          *          
*              INPUT = DIFFTHRS --> NUMBER OF PREM TIME W/MEALS      *          
*              R3 KEEPS TRACK OF REMAINING PREMIUM TIME              *          
*              TIME AND A HALF HOURS ARE MULTIPLIED BY 1.5           *          
*              DOUBLE TIME HOURS ARE MULTIPLIED BY 2                 *          
*              PREMHRS RETURNED W/ NUMBER OF HRS TO X BY HOURLY RATE *          
*              SVNPREM RETURNED W/ NUMBER OF PRINTABLE HOURS         *          
*              IF ACTRA (NOT 2404A) STRAIGHT, OT, AND DT HOURS       *          
*              ARE STORED SEPARATELY IN SVNPST, SVNPOT, AND SVNPDT   *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CLPRACT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,HRS2MIN            CONVERT DIFFTHRS TO MINUTES                
         LH    R0,DIFFTMIN           R0 = # OF MINS OF PREM. HRS.               
         MVC   DIFFTHRS,MEALPHRS     # OF PREMIUM MEAL HRS                      
         BRAS  RE,HRS2MIN            CONVERT TO MINUTES                         
         SH    R0,DIFFTMIN           SUBTRACT PREMIUM MEAL HOURS                
         STH   R0,DIFFTMIN           DIFFERENCE IN MINS                         
         BRAS  RE,MIN2HRS            CONVERT BACK TO HOURS                      
         MVC   SVNPREM,DIFFTHRS      SAVE # OF PREMIUM HOURS                    
         BRAS  RE,HRS2DEC            CONVERT TO DECIMAL NUMBER                  
         LR    R3,R1                 R3=# OF PREMIUM HOURS (DECIMAL)            
*                                                                               
         SR    R1,R1                                                            
         ZICM  RF,TATMWTST,2         WORK START                                 
         TM    SVDOBST,MINOR16       MINOR UNDER 16?                            
         BO    CLPRA05                                                          
         CLI   SVACTRA,CCTY04A       2404A?                                     
         BNE   CLPRA10                                                          
         CLI   FIRSTDY,C'Y'          NOT 1ST DAY OF SESSION?                    
         BE    CLPRA10                                                          
CLPRA05  AHI   RF,800                ADD 8 HRS TO WORK START                    
         B     *+8                                                              
CLPRA10  AHI   RF,900                ADD 9 HRS TO WORK START                    
         STH   RF,DIFFTHRS                                                      
         BRAS  RE,HRS2DEC                                                       
         LR    RF,R1                                                            
         AH    RF,MLNPHTOT           ADD ANY MEALS BEFORE PREM HOURS            
         MVC   DIFFTHRS,STRTTIME                                                
         BRAS  RE,HRS2DEC                                                       
         SR    RF,R1                 RF= # OF STRAIGHT TIME HRS                 
         CHI   RF,0                  SEE IF WE HAVE STRAIGHT TIME HRS           
         BNL   *+6                                                              
         SR    RF,RF                                                            
         LR    R5,RF                 R5 = KEEP TRACK OF ADJUSTED HRS            
         CLI   FIRSTDY,C'Y'          2404A, 1ST DAY OF SESSION?                 
         BNE   CLPRA14                                                          
         STCM  R5,3,PREMACT          ACTRA NP HOURS ADJUSTED                    
         STCM  R5,3,SVNPACT          ACTRA NP HOURS                             
*                                                                               
CLPRA14  CLI   SVACTRA,CCTY04A       IF NOT 2404A,                              
         BE    CLPRA15                                                          
         STCM  R5,3,SVNPST           SAVE ACTRA NP STRAIGHT TIME HOURS          
*                                                                               
CLPRA15  CR    R3,RF                 IF # PREM HRS < # OF STRAIGHT TIME         
         BNL   CLPRA16               HRS, THEN # PREM HRS = # ADJ. HRS          
         LR    R5,R3                                                            
         CLI   FIRSTDY,C'Y'          2404A, 1ST DAY OF SESSION?                 
         BNE   CLPRA15A                                                         
         STCM  R5,3,PREMACT          RESET ACTRA NP HOURS ADJUSTED              
         STCM  R5,3,SVNPACT          RESET ACTRA NP HOURS                       
         B     CLPRA50                                                          
CLPRA15A CLI   SVACTRA,CCTY04A       IF NOT 2404A,                              
         BE    CLPRA50                                                          
         STCM  R5,3,SVNPST           RESET ACTRA NP STRAIGHT TIME HOURS         
         B     CLPRA50                                                          
*                                                                               
CLPRA16  SR    R3,RF                 PREM TIME MINUS STRAIGHT TIME              
         CLI   FIRSTDY,C'Y'          2404A, 1ST DAY OF SESSION?                 
         BNE   CLPRA18                                                          
         STCM  R3,3,SVNPSAG          SAG NP HOURS (OT + DT)                     
         SR    R5,R5                 CLEAR NP ADJ HOURS COUNTER                 
*                                                                               
CLPRA18  LR    R2,RF                 R2 --> STRAIGHT TIME IN DECIMAL            
         ZICM  RF,TATMWTST,2         WORK START                                 
         AHI   RF,1000               ADD 10 HRS TO WORK START                   
         STH   RF,DIFFTHRS                                                      
         BRAS  RE,HRS2DEC                                                       
         LR    RF,R1                                                            
         AH    RF,MLNPHTOT           ADD ANY MEALS BEFORE PREM HOURS            
         MVC   DIFFTHRS,STRTTIME                                                
         BRAS  RE,HRS2DEC                                                       
         SR    RF,R1                 SUBTRACT DECIMAL START TIME                
         SR    RF,R2                 AND STRAIGHT HRS. RF=# OF 1.5 HRS          
         CHI   RF,0                  SEE IF WE HAVE TIME AND A HALF HRS         
         BNH   CLPRA40                                                          
         CR    RF,R3                 IF RF < REMAINING PREMIUM TIME,            
         BNH   CLPRA20               REMAIN PREM TIME = # OF 1.5 HRS            
         LR    R1,R3                 R1 = TIME AND A HALF IN DECIMAL            
         B     *+6                                                              
CLPRA20  LR    R1,RF                 R1 = TIME AND A HALF IN DECIMAL            
*                                                                               
         CLI   SVACTRA,CCTY04A       IF NOT 2404A,                              
         BE    CLPRA30                                                          
         STCM  R1,3,SVNPOT           SAVE ACTRA NP OVERTIME HOURS               
*                                                                               
CLPRA30  SR    R3,R1                 REMAINING PREMIUM TIME                     
         CLI   SVACTRA,CCTY04A       IF NOT 2404A,                              
         BE    CLPRA35                                                          
         STCM  R3,3,SVNPDT           SAVE ACTRA NP DOUBLETIME HOURS             
*                                                                               
CLPRA35  LR    RE,R1                 CALCULATE TIME AND A HALF                  
         SR    R0,R0                                                            
         D     R0,=F'2'                                                         
         CHI   R0,1                  IF REMAINDER,                              
         BL    *+8                                                              
         AHI   R1,1                  ROUND UP                                   
         AR    R1,RE                 R1 = HRS*1.5  (TIME AND A HALF)            
         AR    R5,R1                 R5=# OF STRAIGHT + 1.5 HOURS               
*                                                                               
CLPRA40  MHI   R3,2                  REMAINING PREM TIME IS DOUBLE TIME         
         AR    R5,R3                 ADD THIS TO TOTAL PREM HRS                 
         CLI   FIRSTDY,C'Y'          2404A, 1ST DAY OF SESSION?                 
         BNE   CLPRA50                                                          
         STCM  R5,3,PREMSAG          SAG NP HOURS ADJUSTED (OT + DT)            
         B     CLPRAX                                                           
CLPRA50  STCM  R5,3,PREMHRS          PREMIUM TIME HOURS (ADJUSTED)              
CLPRAX   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*--------------------------------------------------------------------*          
*        SETS START AND END TIME FOR CALCULATION OF 10% NIGHT PREMIUM*          
*        FOR DAYS THAT INCLUDE HOURS OF 6AM - 8PM                               
*        SAVES AMOUNT OF 20% PREMIUM TIME TO ADD LATER                          
* -------------------------------------------------------------------*          
         USING TATMD,R4                                                         
SETTIME  NTR1  BASE=*,LABEL=*                                                   
         OC    TATMTTDP,TATMTTDP                                                
         BZ    STIME10                                                          
         CLC   TATMTTDP,=H'200'    IF TRAVEL TO DEPART IS BETWEEN               
         BNL   STIME07             1AM AND 2AM, ALL DAY IS 20% NP               
         CLC   TATMTTDP,=H'100'                                                 
         BNL   STIMENO                                                          
*                                                                               
STIME07  CLC   TATMTTDP,=H'600'    IS TRAVEL TO DEPART DURING 20NP?             
         BNL   STIME30             (1-6AM)?                                     
         CLC   TATMTTDP,=H'100'                                                 
         BNL   STIME40                                                          
         B     STIME30                                                          
*                                                                               
STIME10  CLC   TATMWTST,=H'200'    IF WORK START IS BETWEEN 1 - 2AM,            
         BNL   STIME15             ALL DAY IS 20% NP                            
         CLC   TATMWTST,=H'100'                                                 
         BNL   STIMENO                                                          
*                                                                               
STIME15  CLC   TATMWTST,=H'600'    IS WORK START DURING 20NP (1-6AM)?           
         BNL   STIME30                                                          
         CLC   TATMWTST,=H'100'                                                 
         BNL   STIME40                                                          
*                                  WORK START DURING 10%NP                      
STIME30  MVC   STRTTIME,=H'2000'   START AT 8PM                                 
         MVC   ENDTIME,TATMWTNT    END AT WORK END                              
         BRAS  RE,TIMDIFF                                                       
         MVC   EX10NP,DIFFTHRS     TIME DIFFERENCE = EXTRA 10%NP                
*                                                                               
         STCM  R1,3,STRTTIME       START TIME IS BEGINNING OF DAY               
         MVC   HRMIN1,=H'2500'     END AT 1AM                                   
         TM    DAYTYPE,DAYFRI      FRIDAY?                                      
         BNO   STIME35                                                          
         MVC   HRMIN1,=H'2400'     END AT 12M                                   
*                                                                               
         OC    TATMTTDP,TATMTTDP                                                
         BZ    STIME33                                                          
         CLC   TATMTTDP,=H'100'    IF TRAVEL TO DEPART                          
         BNL   STIME35                                                          
         B     STIME34                                                          
STIME33  CLC   TATMWTST,=H'100'    OR WORK START                                
         BNL   STIME35                                                          
STIME34  MVC   HRMIN1,=H'2500'    IS BETWEEN 12M AND 1AM, END AT 1AM            
*                                                                               
STIME35  MVC   HRMIN2,EX10NP       ADD ON ANY EXTRA 10% NP                      
         BRAS  RE,ADDHRS                                                        
         MVC   ENDTIME,DIFFTHRS                                                 
*                                                                               
         CLC   STRTTIME,=H'100'    IF START TIME IS BETWEEN 12M AND 1AM         
         BNL   STIMEYES                                                         
         ZICM  R1,ENDTIME,2        SUBTRACT 2400 FROM END TIME                  
         SHI   R1,2400                                                          
         STCM  R1,3,ENDTIME                                                     
         B     STIMEYES                                                         
*                                  WORK START DURING 20%NP                      
STIME40  MVC   STRTTIME,=H'2000'   START AT 8PM                                 
         MVC   ENDTIME,=H'2500'    END AT 1AM THE LATEST                        
*                                                                               
         TM    DAYTYPE,DAYFRI      FRIDAY?                                      
         BNO   STIME50                                                          
         MVC   ENDTIME,=H'2400'    SET END TIME AS 12M                          
         CHI   RF,2400             IF END TIME IS EARLIER THAN 12M,             
         BNL   STIME70                                                          
         STCM  RF,3,ENDTIME        END AT THE REAL END TIME                     
*                                                                               
         OC    TATMTTDP,TATMTTDP                                                
         BZ    STIME43                                                          
         CLC   TATMTTDP,=H'100'    IF TRAVEL TO DEPART                          
         BNL   STIME60                                                          
         B     STIME45                                                          
STIME43  CLC   TATMWTST,=H'100'    OR WORK START                                
         BNL   STIME60                                                          
STIME45  MVC   ENDTIME,=H'2500'    IS BETWEEN 12M AND 1AM, USE 1AM              
         B     STIME60                                                          
*                                  NON-FRIDAY                                   
STIME50  CHI   RF,2500             IF END TIME IS EARLIER THAN 1AM,             
         BNL   STIME70                                                          
         STCM  RF,3,ENDTIME        END AT THE REAL END TIME                     
*                                                                               
STIME60  CLI   NP6AMFLG,C'Y'       IF FORCED 6AM END TIME                       
         BNE   STIMEYES                                                         
         MVC   ENDTIME,=H'2500'    SET END TIME AS 1AM AND CALC 10%             
         TM    DAYTYPE,DAYFRI      FRIDAY?                                      
         BNO   STIMEYES                                                         
         MVC   ENDTIME,=H'2400'    SET END TIME AS 12M AND CALC 10%             
         B     STIMEYES                                                         
*                                                                               
STIME70  TM    DAYTYPE,DAYFRI      FRIDAY?                                      
         BO    STIME80                                                          
         SHI   RF,2500             NO, END TIME - 1AM                           
         STH   RF,EX20NP           AMT OF 20% PREMIUM TIME AFTER 1AM            
         B     STIMEYES                                                         
STIME80  SHI   RF,2400             YES, END TIME - 12M                          
         STH   RF,EX20NP           AMT OF 20% PREMIUM TIME AFTER 12M            
*                                                                               
STIMEYES XR    RC,RC                                                            
STIMENO  LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              SETS TOTAL MEAL NON PREMIUM HOURS FOR 20% NGHT PREM   *          
*              ADDS 10% PREM MEAL HOURS + NON PREM MEAL HOURS        *          
*              OUTPUT = MLNPHTOT                                     *          
*--------------------------------------------------------------------*          
SETMEAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   DIFFTHRS,MEALPH10   10% PREMIUM MEAL HOURS                       
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         AH    R1,MEALNPH          NON PREMIUM MEAL HOURS                       
         STH   R1,MLNPHTOT         MEAL NON PREMIUM HOURS TOTAL                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ADJUST NIGHT PREMIUM IF NPEXFLG = Y.  ON WEEKDAYS ONLY,                
*        ALL EXTRA NP HOURS SHOULD GET PAID AS DOUBLETIME NP                    
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJPREM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   STRTTIME,NPSTART    # OF HOURS BETWEEN 8PM AND WORK END          
         MVC   ENDTIME,TATMWTNT                                                 
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
*                                                                               
         MVC   DIFFTHRS,SVML10                                                  
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN         DEDUCT MEALS DURING EXTRA NP                 
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT TIME W/O MEALS TO HOURS              
         CLI   SVACTRA,CCTY04A     ACTRA TYPE 2404A?                            
         BE    ADJPR10                                                          
         MVC   HRMIN1,TATMNP10                                                  
         MVC   HRMIN2,DIFFTHRS     ADD TO PRINTABLE NP10 HRS.MINS               
         BRAS  RE,ADDHRS                                                        
         BRAS  RE,RNDQRTR          ROUND TO QUARTER HOUR                        
         MVC   TATMNP10,DIFFTHRS                                                
         B     ADJPR20                                                          
*                                                                               
ADJPR10  MVC   HRMIN1,TATMNP20     2404A - ALL DOUBLETIME = SAG HRS             
         MVC   HRMIN2,DIFFTHRS     ADD TO PRINTABLE NP20 HRS.MINS               
         BRAS  RE,ADDHRS                                                        
         BRAS  RE,RNDQRTR          ROUND TO QUARTER HOUR                        
         MVC   TATMNP20,DIFFTHRS                                                
*                                                                               
ADJPR20  CLI   SVACTRA,CCTY04B     IF 2404B                                     
         BE    ADJPR25                                                          
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    ADJPR25                                                          
         CLI   SVACTRA,0           OR NON-ACTRA,                                
         BNE   ADJPR40                                                          
ADJPR25  TM    TATMSTA2,TATMS2DL   DISTANT LOCATION?                            
         BNZ   ADJPR28             TREAT AS WEEKDAY                             
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN   IF NOT HOLIDAY/SAT/SUN           
         BNZ   *+8                                                              
ADJPR28  MHI   R1,2                DOUBLETIME ADJUSTMENT - MINUTES              
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,HRS2DEC          GET EXTRA NP 10%                             
         AH    R1,SVNP10           ADD EXTRA NP 10% TO NP 10%                   
         STH   R1,HALF                                                          
         BRAS  RE,RNDDEC           ROUND DECIMAL BY QUARTER HOUR                
         MVC   SVNP10,HALF         ADJUSTED NIGHT PREMIUM 10%                   
         B     ADJPRX                                                           
*                                                                               
*                                  ACTRA AND 2404A                              
ADJPR40  STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,HRS2DEC          GET EXTRA NP 10%                             
         AH    R1,SVNPDT           ADD EXTRA DOUBLETIME NP TO DT NP             
         STH   R1,HALF                                                          
         BRAS  RE,RNDDEC           ROUND DECIMAL BY QUARTER HOUR                
         MVC   SVNPDT,HALF         DOUBLETIME ACTRA NIGHT PREMIUM               
                                                                                
*                                                                               
ADJPRX   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ADJUST NIGHT PREMIUM MEALS IF NPEXFLG = Y.                             
*        CHANGES MEALPH10                                                       
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJMPH   NTR1  BASE=*,LABEL=*                                                   
         CLI   NPEXFLG,C'Y'                                                     
         BNE   ADJMPHX                                                          
         BRAS  RE,ADJMLNP          ANY MEALS DURING EXTRA NP?                   
*                                                                               
         OC    SVML10,SVML10                                                    
         BZ    ADJMPHX                                                          
         BRAS  RE,HRS2MIN          CONVERT EXTRA NP MEAL TO MINS                
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALPH10   CONVERT ALL NP MEALS TO MINS                 
         BRAS  RE,HRS2MIN                                                       
         LH    R2,DIFFTMIN                                                      
         SR    R2,R1               SUBTRACT EXTRA FROM TOTAL NP MEALS           
         STCM  R2,3,DIFFTMIN                                                    
         BRAS  RE,MIN2HRS                                                       
         MVC   MEALPH10,DIFFTHRS                                                
*                                                                               
ADJMPHX  XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ARE ANY MEALS DURING EXTRA 10% NIGHT PREMIUM?                    
*              WEEKDAYS ONLY, DAYS INCLUDE 6AM-8PM                              
*              SETS SVML10                                                      
*              NPSTART = 8PM SAG OR 11PM ACTRA                                  
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJMLNP  NTR1  BASE=*,LABEL=*                                                   
         XC    SVML10,SVML10                                                    
         XC    DIFFTHRS,DIFFTHRS                                                
*                                                                               
ANP1005  CLC   TATMWTST,=H'1200'   IS WORK START IN PM?                         
         BL    ANP1040                                                          
         CLC   TATMWTST,=H'2400'                                                
         BNL   ANP1040                                                          
*                                                                               
         OC    TATMM1ST,TATMM1ST   IS MEAL 1 8PM - END?                         
         BZ    ANP1020                                                          
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL NON-DEDUCTIBLE?                      
         BNZ   ANP1020             IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST   START OF MEAL                                
         MVC   MEALEND,TATMM1NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   ANP1020                                                          
*                                                                               
         MVC   STRTTIME,TATMM1ST   MEAL1 START                                  
         MVC   ENDTIME,TATMM1NT    MEAL1 END                                    
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1                    
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   ANP1010                                                          
         ZICM  RE,TATMM1ST,2       ENDTIME = START TIME + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,ENDTIME                                                     
*                                                                               
ANP1010  CLC   ENDTIME,TATMWTNT    IS MEAL END > WORK END?                      
         BNL   ANP1020                                                          
         CLC   STRTTIME,TATMWTST   IS MEAL START > WORK START?                  
         BNL   ANP1020                                                          
         CLC   ENDTIME,NPSTART     DOES MEAL END AFTER 8PM?                     
         BNH   ANP1020                                                          
         CLC   TATMM1ST,NPSTART    DOES MEAL START AFTER 8PM?                   
         BNL   *+10                                                             
         MVC   STRTTIME,NPSTART    NO, START COUNTING MEAL AT 8PM               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1                    
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM AFTER 8PM            
*                                                                               
ANP1020  OC    TATMM2ST,TATMM2ST   IS MEAL 2 8PM - END?                         
         BZ    ANP1033                                                          
*                                                                               
         MVC   MEALSTRT,TATMM2ST   START OF MEAL                                
         MVC   MEALEND,TATMM2NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   ANP1033                                                          
*                                                                               
         MVC   STRTTIME,TATMM2ST   MEAL2 START                                  
         MVC   ENDTIME,TATMM2NT    MEAL2 END                                    
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2                    
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   ANP1030                                                          
         ZICM  RE,TATMM2ST,2       ENDTIME = START TIME + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,ENDTIME                                                     
*                                                                               
ANP1030  CLC   ENDTIME,NPSTART     DOES MEAL END AFTER 8PM?                     
         BNH   ANP1033                                                          
         CLC   TATMM2ST,NPSTART    DOES MEAL START AFTER 8PM?                   
         BNL   *+10                                                             
         MVC   STRTTIME,NPSTART    NO, START COUNTING MEAL AT 8PM               
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2                    
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML10                                                    
         BRAS  RE,ADDHRS                                                        
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM AFTER 8PM            
*                                                                               
ANP1033  OC    TATMM2ST,TATMM2ST   IS MEAL 3 8PM - END?                         
         BZ    ANP10X                                                           
*                                                                               
         MVC   MEALSTRT,TATMM3ST   START OF MEAL                                
         MVC   MEALEND,TATMM3NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   ANP10X                                                           
*                                                                               
         MVC   STRTTIME,TATMM3ST   MEAL3 START                                  
         MVC   ENDTIME,TATMM3NT    MEAL3 END                                    
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2                    
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   ANP1035                                                          
         ZICM  RE,TATMM3ST,2       ENDTIME = START TIME + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,ENDTIME                                                     
*                                                                               
ANP1035  CLC   ENDTIME,NPSTART     DOES MEAL END AFTER 8PM?                     
         BNH   ANP10X                                                           
         CLC   TATMM3ST,NPSTART    DOES MEAL START AFTER 8PM?                   
         BNL   *+10                                                             
         MVC   STRTTIME,NPSTART    NO, START COUNTING MEAL AT 8PM               
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2                    
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML10       ADD MEAL1 + MEAL2                            
         BRAS  RE,ADDHRS                                                        
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM AFTER 8PM            
         B     ANP10X                                                           
*                                                                               
*                                  IF WORK START IN AM                          
ANP1040  OC    TATMM1ST,TATMM1ST   IS MEAL 1 DURING 8PM - END?                  
         BZ    ANP1070                                                          
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL NON-DEDUCTIBLE?                      
         BNZ   ANP1070             IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST   START OF MEAL                                
         MVC   MEALEND,TATMM1NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   ANP1070                                                          
*                                                                               
         MVC   STRTTIME,TATMM1ST   MEAL1 START                                  
         MVC   ENDTIME,TATMM1NT    MEAL1 END                                    
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1                    
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   ANP1050                                                          
         ZICM  RE,TATMM1ST,2       ENDTIME = START TIME + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,ENDTIME                                                     
*                                                                               
ANP1050  CLC   STRTTIME,TATMWTST   IF MEAL START < WORK START                   
         BL    ANP1060             MEAL IS AFTER 8PM NP                         
         BE    ANP1070                                                          
         CLC   ENDTIME,NPSTART     DOES MEAL END AFTER 8PM?                     
         BNH   ANP1070                                                          
         CLC   STRTTIME,NPSTART    YES, DOES MEAL START AFTER 8PM?              
         BNL   ANP1060                                                          
         MVC   STRTTIME,NPSTART    NO, START COUNTING MEAL AT 8PM               
ANP1060  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1                    
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM AFTER 8PM            
*                                                                               
ANP1070  OC    TATMM2ST,TATMM2ST   IS MEAL 2 DURING 8PM - END?                  
         BZ    ANP10100                                                         
*                                                                               
         MVC   MEALSTRT,TATMM2ST   START OF MEAL                                
         MVC   MEALEND,TATMM2NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   ANP10100                                                         
*                                                                               
         MVC   STRTTIME,TATMM2ST   MEAL2 START                                  
         MVC   ENDTIME,TATMM2NT    MEAL2 END                                    
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2                    
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   ANP1080                                                          
         ZICM  RE,TATMM2ST,2       ENDTIME = START TIME + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,ENDTIME                                                     
*                                                                               
ANP1080  CLC   STRTTIME,TATMWTST   IF MEAL START < WORK START                   
         BL    ANP1090             MEAL IS AFTER 8PM NP                         
         BE    ANP10100                                                         
         CLC   ENDTIME,NPSTART     DOES MEAL END AFTER 8PM?                     
         BNH   ANP10100                                                         
         CLC   STRTTIME,NPSTART    YES, DOES MEAL START AFTER 8PM?              
         BNL   ANP1090                                                          
         MVC   STRTTIME,NPSTART    NO, START COUNTING MEAL AT 8PM               
ANP1090  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2                    
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML10                                                    
         BRAS  RE,ADDHRS                                                        
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM AFTER 8PM            
*                                                                               
ANP10100 OC    TATMM3ST,TATMM3ST   IS MEAL 3 DURING 8PM - END?                  
         BZ    ANP10X                                                           
*                                                                               
         MVC   MEALSTRT,TATMM3ST   START OF MEAL                                
         MVC   MEALEND,TATMM3NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   ANP10X                                                           
*                                                                               
         MVC   STRTTIME,TATMM3ST   MEAL3 START                                  
         MVC   ENDTIME,TATMM3NT    MEAL3 END                                    
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL3                    
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   ANP10110                                                         
         ZICM  RE,TATMM3ST,2       ENDTIME = START TIME + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,ENDTIME                                                     
*                                                                               
ANP10110 CLC   STRTTIME,TATMWTST   IF MEAL START < WORK START                   
         BL    ANP10120            MEAL IS AFTER 8PM NP                         
         BE    ANP10X                                                           
         CLC   ENDTIME,NPSTART     DOES MEAL END AFTER 8PM?                     
         BNH   ANP10X                                                           
         CLC   STRTTIME,NPSTART    YES, DOES MEAL START AFTER 8PM?              
         BNL   ANP10120                                                         
         MVC   STRTTIME,NPSTART    NO, START COUNTING MEAL AT 8PM               
ANP10120 BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL3                    
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML10       ADD MEAL1 + MEAL2                            
         BRAS  RE,ADDHRS                                                        
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM AFTER 8PM            
ANP10X   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ADJUST NIGHT PREMIUM FOR EXTRAS THAT WORK FRIDAY NIGHTS     *          
*        ALL HOURS AFTER MIDNIGHT SHOULD GET PAID AS DOUBLETIME      *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJNPFR  NTR1  BASE=*,LABEL=*                                                   
         XC    SMKNP10,SMKNP10                                                  
         XC    SMKNP20,SMKNP20                                                  
*                                                                               
         CLC   TATMWTNT,TATMWTST   IF WORK END > WORK START,                    
         BNL   ADJNPNO             WORK IS ALL ON FRIDAY                        
         BRAS  RE,ADJML20          ANY MEALS ON SAT AFTER 1AM?                  
*                                                                               
         MVC   STRTTIME,=H'2400'   12 MIDNIGHT                                  
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE                           
         BNO   ADJNP10                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600             ADD 16 HOURS TO WORK START                   
         CHI   R0,2400             IF NOT AFTER MIDNIGHT,                       
         BNH   ADJNPYES            NO EXTRA 10% NP                              
         CHI   R0,2500             IF 16 HOURS + WORK START IS                  
         BNL   ADJNP10             BEFORE 1AM, USE THIS AS ENDTIME              
         STCM  R0,3,ENDTIME                                                     
         B     ADJNP20                                                          
*                                                                               
ADJNP10  MVC   ENDTIME,=H'100'     1 AM                                         
         CLC   TATMWTNT,=H'100'    IS ENDTIME BEFORE 1AM?                       
         BNL   *+10                                                             
         MVC   ENDTIME,TATMWTNT                                                 
ADJNP20  BRAS  RE,TIMDIFF                                                       
         BRAS  RE,HRS2MIN          CONVERT TO MINUTES                           
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,SVML10     CONVERT 10% NP TO MINUTES                    
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN         DEDUCT MEALS BETWEEN 12 AND 1AM              
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT TIME W/O MEALS TO HOURS              
         MVC   HRMIN1,TATMNP10                                                  
         MVC   HRMIN2,DIFFTHRS     ADD TO PRINTABLE NP10 HRS.MINS               
         BRAS  RE,ADDHRS                                                        
         BRAS  RE,RNDQRTR          ROUND TO QUARTER HOUR                        
         MVC   TATMNP10,DIFFTHRS                                                
*                                                                               
         LR    R0,R1               SAVE R1                                      
         STH   R1,DIFFTMIN         ADJUST NP 10% FOR SMOKE PAY                  
         BRAS  RE,MIN2HRS          DO NOT DOUBLE TIME FROM 12-1                 
         BRAS  RE,HRS2DEC          GET EXTRA NP 10%                             
         STH   R1,FULL             SAVE FOR LOWER DOWN                          
         AH    R1,SVNP10           ADD EXTRA NP 10% TO NP 10%                   
         STH   R1,HALF                                                          
         BRAS  RE,RNDDEC           ROUND DECIMAL BY QUARTER HOUR                
         MVC   SMKNP10,HALF        SMOKE PAY ADJUSTED NIGHT PREMIUM 10%         
*                                                                               
*&&DO                                                                           
         LR    R1,R0               RESTORE R1                                   
         MHI   R1,2                DOUBLETIME ADJUSTMENT - MINUTES              
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,HRS2DEC          GET EXTRA NP 10%                             
         AH    R1,SVNP10           ADD EXTRA NP 10% TO NP 10%                   
         STH   R1,HALF                                                          
*****    BRAS  RE,RNDDEC           ROUND DECIMAL BY QUARTER HOUR                
         MVC   SVNP10,HALF         ADJUSTED NIGHT PREMIUM 10%                   
*&&                                                                             
*                                                                               
***      LR    R1,R0               RESTORE R1                                   
         ZICM  R1,FULL,2           NP 10% 12M-1A                                
         AH    R1,SVNP10           ADD TO NP 10% FROM 8P-12M                    
         STH   R1,HALF                                                          
         BRAS  RE,RNDDEC           ROUND BY QUARTER HOUR                        
         LH    RE,HALF                                                          
         SR    RE,R1               GET DIFFERENCE - WHAT WE ROUNDED UP          
         ZICM  R0,FULL,2                                                        
         AR    RE,R0               ADD TO NP 10% FROM 12M-1A                    
         MHI   RE,2                DOUBLETIME ADJUSTMENT - MINUTES              
***      STH   R1,DIFFTMIN                                                      
***      BRAS  RE,MIN2HRS                                                       
***      BRAS  RE,HRS2DEC          GET EXTRA NP 10%                             
         AH    RE,SVNP10           ADD EXTRA NP 10% TO NP 10%                   
         STH   RE,HALF                                                          
*****    BRAS  RE,RNDDEC           ROUND DECIMAL BY QUARTER HOUR                
         MVC   SVNP10,HALF         ADJUSTED NIGHT PREMIUM 10%                   
*                                                                               
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE                           
         BNO   ADJNP30                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600             ADD 16 HOURS TO WORK START                   
         CHI   R0,2500             IF NOT AFTER 1AM                             
         BNH   ADJNPYES            NO EXTRA 20% NP                              
         CHI   R0,3000             IS 16 HRS + WORK START BEFORE                
         BH    ADJNP25             OR AFTER 6AM?                                
         CHI   R0,2400             IF 16 HRS + WORK START IS BEFORE 6AM         
         BNH   *+8                                                              
         AHI   R0,-2400            IF TOO BIG, DEDUCT 2400                      
         STCM  R0,3,ENDTIME                                                     
         MVC   STRTTIME,=H'100'    SET START TIME TO 1AM                        
         B     ADJNP35                                                          
ADJNP25  MVC   ENDTIME,=H'600'     IF AFTER 6AM, SET ENDTIME TO 6AM             
         MVC   STRTTIME,=H'100'    SET START TIME TO 1AM                        
         B     ADJNP35                                                          
*                                                                               
ADJNP30  MVC   STRTTIME,=H'100'                                                 
         CLC   TATMWTNT,=H'100'    IF ENDTIME IS BEFORE 1AM, NO 20%             
         BNH   ADJNPYES                                                         
         MVC   ENDTIME,TATMWTNT    END OF WORK TIME                             
ADJNP35  BRAS  RE,TIMDIFF                                                       
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,SVML20                                                  
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN         DEDUCT MEALS AFTER 1AM                       
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT TIME W/O MEALS TO HOURS              
         BRAS  RE,RNDQRTR          ROUND TO QUARTER HOUR                        
         MVC   TATMNP20,DIFFTHRS   PRINTABLE 20% NP HOURS                       
         LR    R0,R1               SAVE R1                                      
         BRAS  RE,HRS2DEC                                                       
         STH   R1,SMKNP20          UNADJUSTED 20% NP HRS FOR SMOKE PAY          
         LR    R1,R0               RESTORE R1                                   
*                                                                               
*&&DO                                                                           
         MHI   R1,2                DOUBLETIME ADJUSTMENT - MINUTES              
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          GET EXTRA NP 20%                             
         STH   R1,SVNP20           ADJUSTED NIGHT PREMIUM 20%                   
*&&                                                                             
*                                                                               
         BRAS  RE,HRS2DEC          ROUNDED HOURS                                
         MHI   R1,2                DOUBLETIME ADJUSTMENT                        
         STH   R1,SVNP20                                                        
*                                                                               
ADJNPYES SR    RC,RC                                                            
ADJNPNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ADJUST NIGHT PREMIUM FOR EXTRAS THAT WORK FRIDAY MORNINGS   *          
*        IF WORK START IS 1AM TO 6AM, THIS IS ADDITIONAL 20% NP      *          
*        THAT IS PAID STRAIGHT TIME                                  *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJNPFR2 NTR1  BASE=*,LABEL=*                                                   
         CLC   TATMWTST,=H'600'    IF WORK START IS BETWEEN 12M AND 6AM         
         BNL   ADJNP2X                                                          
         BRAS  RE,ADJML20F         ANY MEALS ON FRI BETWEEN 1AM AND 6AM         
*                                                                               
         MVC   STRTTIME,TATMTTDP   TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         MVC   STRTTIME,TATMWTST   WORK START                                   
         CLC   STRTTIME,=H'100'    IS WORK START BETWEEN 12M AND 1AM?           
         BNL   *+10                                                             
         MVC   STRTTIME,=H'100'    YES, SET TO 1AM                              
*                                                                               
         MVC   ENDTIME,=H'600'     6 AM                                         
         CLC   TATMWTNT,=H'600'    IS ENDTIME BETWEEN 1AM AND 6AM?              
         BNL   ADJNP2A                                                          
         CLC   TATMWTNT,=H'100'                                                 
         BNH   ADJNP2A                                                          
         CLC   TATMWTST,TATMWTNT   IS WORK START > WORK END                     
         BH    *+10                                                             
         MVC   ENDTIME,TATMWTNT    NO, USE REAL ENDTIME                         
ADJNP2A  BRAS  RE,TIMDIFF                                                       
         BRAS  RE,HRS2MIN          CONVERT TO MINUTES                           
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,SVML20F    CONVERT 20% NP TO MINUTES                    
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN         DEDUCT MEALS BETWEEN 1 AND 6AM               
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT TIME W/O MEALS TO HOURS              
         MVC   HRMIN1,TATMNP20                                                  
         MVC   HRMIN2,DIFFTHRS     ADD TO PRINTABLE NP20 HRS.MINS               
         BRAS  RE,ADDHRS                                                        
         BRAS  RE,RNDQRTR          ROUND TO QUARTER HOUR                        
         MVC   TATMNP20,DIFFTHRS                                                
*                                                                               
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,HRS2DEC          GET EXTRA NP 20%                             
         AH    R1,SVNP20           ADD EXTRA NP 20% TO NP 20%                   
         STH   R1,HALF                                                          
*****    BRAS  RE,RNDDEC           ROUND DECIMAL BY QUARTER HOUR                
         MVC   SVNP20,HALF         ADJUSTED NIGHT PREMIUM 20%                   
*                                                                               
ADJNP2X  XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*--------------------------------------------------------------------*          
*        ADJUST NIGHT PREMIUM FOR EXTRAS THAT WORK FRIDAY MORNINGS   *          
*        IF WORK START IS 1201AM TO 1:59AM THIS IS ADDITIONAL 10% NP *          
*        THAT IS PAID DOUBLETIME BECAUSE HOURS > 10                  *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJNPFR3 NTR1  BASE=*,LABEL=*                                                   
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE? YES, NO EXTRA NP            
         BO    ADJNP3X                                                          
*                                                                               
         OC    TATMTTDP,TATMTTDP   ANY TRAVEL TO DEPART TIME?                   
         BZ    ADJNP3A                                                          
         CLC   TATMTTDP,=H'100'    IF TRAVEL TO DEPART IS BETWEEN 12M           
         BNL   ADJNP3X             AND 1AM,                                     
         B     ADJNP3B                                                          
*                                                                               
ADJNP3A  CLC   TATMWTST,=H'100'    IF WORK START IS BETWEEN 12M AND 1AM         
         BNL   ADJNP3X                                                          
ADJNP3B  CLC   TATMWTNT,=H'2000'   AND WORK END IS AFTER 8PM                    
         BNH   ADJNP3X                                                          
*                                                                               
***      BRAS  RE,ADJML10F         ANY MEALS ON FRI BETWEEN 8PM AND 12M         
*                                                                               
         MVC   STRTTIME,=H'2000'   START CALCULATING 10% NP AT 8PM              
         MVC   ENDTIME,=H'2400'    12M                                          
         CLC   TATMWTNT,=H'2400'   IS ENDTIME EARLIER THAN 12M?                 
         BNL   *+10                                                             
         MVC   ENDTIME,TATMWTNT    YES, USE WORK END TIME                       
*                                                                               
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,HRS2MIN          CONVERT TO MINUTES                           
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,SVML10F    CONVERT 10% NP MEALS TO MINUTES              
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN         DEDUCT MEALS BETWEEN 8PM AND 12M             
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT TIME W/O MEALS TO HOURS              
         MVC   HRMIN1,TATMNP10                                                  
         MVC   HRMIN2,DIFFTHRS     ADD TO PRINTABLE NP10 HRS.MINS               
         BRAS  RE,ADDHRS                                                        
         BRAS  RE,RNDQRTR          ROUND TO QUARTER HOUR                        
         MVC   TATMNP10,DIFFTHRS                                                
*                                                                               
         MHI   R1,2                DOUBLETIME ADJUSTMENT - MINUTES              
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,HRS2DEC          GET EXTRA NP 10%                             
         AH    R1,SVNP10           ADD EXTRA NP 10% TO NP 10%                   
         STH   R1,HALF                                                          
         BRAS  RE,RNDDEC           ROUND DECIMAL BY QUARTER HOUR                
         MVC   SVNP10,HALF         ADJUSTED NIGHT PREMIUM 10%                   
*                                                                               
ADJNP3X  XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*--------------------------------------------------------------------*          
*              EXTRAS ON FRIDAYS ONLY                                *          
*              ARE ANY MEALS DURING 12 TO 1AM? 10% PREMIUM TIME      *          
*              DEDUCTS THIS AMOUNT FROM MEALPH10                     *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJML10  NTR1  BASE=*,LABEL=*                                                   
         XC    SVML10,SVML10                                                    
*                                                                               
         TM    TGCSORT,X'20'       EXTRAS ONLY                                  
         BNO   AML10XX                                                          
         TM    DAYTYPE,DAYFRI      FRIDAYS ONLY                                 
         BNO   AML10XX                                                          
         CLC   TATMWTNT,TATMWTST   IF WORK END > WORK START,                    
         BNL   AML10XX             ALL WORK IS ON FRIDAY                        
         OC    TATMM1ST,TATMM1ST                                                
         BZ    AML1030                                                          
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL NON-DEDUCTIBLE?                      
         BNZ   AML1030             IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST                                                
         MVC   MEALEND,TATMM1NT                                                 
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AML1030                                                          
*                                                                               
         CLC   TATMM1ST,=H'100'                                                 
         BL    AML1010                                                          
         CLC   TATMM1ST,=H'2400'                                                
         BE    AML1010                                                          
         CLC   TATMM1NT,=H'100'   IF MEAL ENDS BEFORE 1AM,                      
         BH    AML1030            BUT STARTS BEFORE MIDNIGHT,                   
         MVC   STRTTIME,=H'2400'  START AT MIDNIGHT                             
         B     *+10                                                             
AML1010  MVC   STRTTIME,TATMM1ST   MEAL START                                   
         MVC   ENDTIME,TATMM1NT    MEAL END                                     
         CLC   TATMM1NT,=H'100'    DOES MEAL END AFTER 1AM?                     
         BNH   *+10                                                             
         MVC   ENDTIME,=H'100'     END AT 1AM                                   
*                                                                               
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE?                             
         BNO   AML1020                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600                                                          
         CHI   R0,2400             DO 16 HOURS END AFTER MIDNIGHT?              
         BNH   AML10XX                                                          
         CHI   R0,2500             DO 16 HOURS END BEFORE 1AM?                  
         BNL   AML1020                                                          
         STCM  R0,3,ENDTIME        YES, USE THIS TIME AS ENDTIME                
*                                                                               
AML1020  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL DURING 12-1         
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM                      
*                                                                               
AML1030  OC    TATMM2ST,TATMM2ST                                                
         BZ    AML10X                                                           
         MVC   MEALSTRT,TATMM2ST                                                
         MVC   MEALEND,TATMM2NT                                                 
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AML1060                                                          
*                                                                               
         CLC   TATMM2ST,=H'100'                                                 
         BL    AML1040                                                          
         CLC   TATMM2ST,=H'2400'                                                
         BE    AML1040                                                          
         CLC   TATMM2NT,=H'100'    IF MEAL ENDS BEFORE 1AM,                     
         BH    AML1060             BUT STARTS BEFORE MIDNIGHT,                  
         MVC   STRTTIME,=H'2400'   START AT MIDNIGHT                            
         B     *+10                                                             
AML1040  MVC   STRTTIME,TATMM2ST   START TIME                                   
         MVC   ENDTIME,TATMM2NT    END TIME                                     
         CLC   TATMM2NT,=H'100'    DOES MEAL END AFTER 1AM?                     
         BNH   *+10                                                             
         MVC   ENDTIME,=H'100'     END AT 1AM                                   
*                                                                               
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE?                             
         BNO   AML1050                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600                                                          
         CHI   R0,2400             DO 16 HOURS END AFTER MIDNIGHT?              
         BNH   AML10XX                                                          
         CHI   R0,2500             DO 16 HOURS END BEFORE 1AM?                  
         BNL   AML1020                                                          
         STCM  R0,3,ENDTIME        YES, USE THIS TIME AS ENDTIME                
*                                                                               
AML1050  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL DURING 12-1         
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML10                                                    
         BRAS  RE,ADDHRS                                                        
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM                      
*                                                                               
AML1060  OC    TATMM3ST,TATMM3ST   MEAL 3                                       
         BZ    AML10X                                                           
         MVC   MEALSTRT,TATMM3ST                                                
         MVC   MEALEND,TATMM3NT                                                 
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AML10X                                                           
*                                                                               
         CLC   TATMM3ST,=H'100'                                                 
         BL    AML1070                                                          
         CLC   TATMM3ST,=H'2400'                                                
         BE    AML1070                                                          
         CLC   TATMM3NT,=H'100'    IF MEAL ENDS BEFORE 1AM,                     
         BH    AML10X              BUT STARTS BEFORE MIDNIGHT,                  
         MVC   STRTTIME,=H'2400'   START AT MIDNIGHT                            
         B     *+10                                                             
AML1070  MVC   STRTTIME,TATMM3ST   START TIME                                   
         MVC   ENDTIME,TATMM3NT    END TIME                                     
         CLC   TATMM3NT,=H'100'    DOES MEAL END AFTER 1AM?                     
         BNH   *+10                                                             
         MVC   ENDTIME,=H'100'     END AT 1AM                                   
*                                                                               
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE?                             
         BNO   AML1080                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600                                                          
         CHI   R0,2400             DO 16 HOURS END AFTER MIDNIGHT?              
         BNH   AML10XX                                                          
         CHI   R0,2500             DO 16 HOURS END BEFORE 1AM?                  
         BNL   AML1020                                                          
         STCM  R0,3,ENDTIME        YES, USE THIS TIME AS ENDTIME                
*                                                                               
AML1080  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL DURING 12-1         
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML10                                                    
         BRAS  RE,ADDHRS                                                        
         MVC   SVML10,DIFFTHRS     MEAL DURING 10% PREMIUM                      
*                                                                               
AML10X   OC    SVML10,SVML10                                                    
         BZ    AML10XX                                                          
         MVC   DIFFTHRS,MEALPH10                                                
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,SVML10                                                  
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN         DEDUCT 12-1 MEAL FROM 10% NP MEAL            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         MVC   MEALPH10,DIFFTHRS   10% NP MEAL FROM 8PM-12M                     
AML10XX  XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ARE ANY MEALS ON SATURDAY AFTER 1AM? 20% PREMIUM TIME *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJML20  NTR1  BASE=*,LABEL=*                                                   
         XC    SVML20,SVML20                                                    
*                                                                               
         OC    TATMM1ST,TATMM1ST   IS MEAL 1 ON SATURDAY?                       
         BZ    AML2020                                                          
         CLC   TATMM1ST,TATMWTNT   IF MEAL START > WORK END                     
         BH    AML2020             MEAL STARTS ON FRIDAY- ENDS BEFORE 1         
         CLC   TATMM1NT,TATMWTNT   IF MEAL END > WORK END                       
         BH    AML2020             MEAL ENDS ON FRIDAY                          
         CLC   TATMM1NT,=H'100'    MEAL ENDS BY 1AM?                            
         BNH   AML2020                                                          
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL NON-DEDUCTIBLE?                      
         BNZ   AML2020             IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST   START OF MEAL                                
         MVC   MEALEND,TATMM1NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AML2020                                                          
*                                                                               
         CLC   TATMM1ST,=H'100'    DOES MEAL START BEFORE 1AM?                  
         BNL   AML2010                                                          
         MVC   STRTTIME,=H'100'    START COUNTING MEAL AT 1AM                   
         B     *+10                                                             
AML2010  MVC   STRTTIME,TATMM1ST   MEAL1 START                                  
         MVC   ENDTIME,TATMM1NT    MEAL1 END                                    
*                                                                               
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE?                             
         BNO   AML2015                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600                                                          
         CHI   R0,2500             DO 16 HOURS END BEFORE 1AM?                  
         BNH   AML20X                                                           
         CHI   R0,2400             IF NUMBER IS TOO HIGH,                       
         BNH   *+8                                                              
         AHI   R0,-2400            SUBTRACT 2400                                
         STCM  R0,3,ENDTIME        NO, USE THIS TIME AS ENDTIME                 
         CLC   STRTTIME,=H'600'    IS START TIME AFTER 6AM?                     
         BNL   AML2020                                                          
         CLC   ENDTIME,=H'600'     IS ENDTIME AFTER 6AM?                        
         BNH   AML2013                                                          
         MVC   ENDTIME,=H'600'     YES, USE 6AM AS ENDTIME                      
AML2013  CLC   ENDTIME,TATMM1NT    IF MEAL END TIME IS BEFORE 16 HOURS,         
         BNH   AML2015                                                          
         MVC   ENDTIME,TATMM1NT    USE MEAL END TIME                            
*                                                                               
AML2015  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   SVML20,DIFFTHRS     MEAL DURING 20% PREMIUM                      
*                                                                               
AML2020  OC    TATMM2ST,TATMM2ST   IS MEAL 2 ON SATURDAY?                       
         BZ    AML2050                                                          
         CLC   TATMM2ST,TATMWTNT   IF MEAL START > WORK END                     
         BH    AML2050             MEAL STARTS ON FRIDAY- ENDS BEFORE 1         
         CLC   TATMM2NT,TATMWTNT   IF MEAL END > WORK END                       
         BH    AML2050             MEAL ENDS ON FRIDAY                          
         CLC   TATMM2NT,=H'100'    MEAL ENDS BY 1AM?                            
         BNH   AML2050                                                          
*                                                                               
         MVC   MEALSTRT,TATMM2ST   START OF MEAL                                
         MVC   MEALEND,TATMM2NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AML2050                                                          
*                                                                               
         CLC   TATMM2ST,=H'100'    DOES MEAL START BEFORE 1AM?                  
         BNL   AML2030                                                          
         MVC   STRTTIME,=H'100'    START COUNTING MEAL AT 1AM                   
         B     *+10                                                             
AML2030  MVC   STRTTIME,TATMM2ST   MEAL2 START                                  
         MVC   ENDTIME,TATMM2NT    MEAL2 END                                    
*                                                                               
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE?                             
         BNO   AML2040                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600                                                          
         CHI   R0,2500             DO 16 HOURS END BEFORE 1AM?                  
         BNH   AML20X                                                           
         CHI   R0,2400             IF NUMBER IS TOO HIGH,                       
         BNH   *+8                                                              
         AHI   R0,-2400            SUBTRACT 2400                                
         STCM  R0,3,ENDTIME        NO, USE THIS TIME AS ENDTIME                 
         CLC   STRTTIME,=H'600'    IS START TIME AFTER 6AM?                     
         BNL   AML2050                                                          
         CLC   ENDTIME,=H'600'     IS ENDTIME AFTER 6AM?                        
         BNH   AML2035                                                          
         MVC   ENDTIME,=H'600'     YES, USE 6AM AS ENDTIME                      
AML2035  CLC   ENDTIME,TATMM2NT    IF MEAL END TIME IS BEFORE 16 HOURS,         
         BNH   AML2040                                                          
         MVC   ENDTIME,TATMM2NT    USE MEAL END TIME                            
*                                                                               
AML2040  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML20                                                    
         BRAS  RE,ADDHRS                                                        
         MVC   SVML20,DIFFTHRS     MEAL DURING 20% PREMIUM                      
*                                                                               
AML2050  OC    TATMM3ST,TATMM3ST   IS MEAL 3 ON SATURDAY?                       
         BZ    AML20X                                                           
         CLC   TATMM3ST,TATMWTNT   IF MEAL START > WORK END                     
         BH    AML20X              MEAL STARTS ON FRIDAY- ENDS BEFORE 1         
         CLC   TATMM3NT,TATMWTNT   IF MEAL END > WORK END                       
         BH    AML20X              MEAL ENDS ON FRIDAY                          
         CLC   TATMM3NT,=H'100'    MEAL ENDS BY 1AM?                            
         BNH   AML20X                                                           
*                                                                               
         MVC   MEALSTRT,TATMM3ST   START OF MEAL                                
         MVC   MEALEND,TATMM3NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AML20X                                                           
*                                                                               
         CLC   TATMM3ST,=H'100'    DOES MEAL START BEFORE 1AM?                  
         BNL   AML2060                                                          
         MVC   STRTTIME,=H'100'    START COUNTING MEAL AT 1AM                   
         B     *+10                                                             
AML2060  MVC   STRTTIME,TATMM3ST   MEAL3 START                                  
         MVC   ENDTIME,TATMM3NT    MEAL3 END                                    
*                                                                               
         TM    TATMSTAT,TATMS16Y   USING 16HR RULE?                             
         BNO   AML2070                                                          
         ZICM  R0,TATMTTDP,2       TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP   OR                                           
         BNZ   *+10                                                             
         ZICM  R0,TATMWTST,2       WORK START                                   
         AHI   R0,1600                                                          
         CHI   R0,2500             DO 16 HOURS END BEFORE 1AM?                  
         BNH   AML20X                                                           
         CHI   R0,2400             IF NUMBER IS TOO HIGH,                       
         BNH   *+8                                                              
         AHI   R0,-2400            SUBTRACT 2400                                
         STCM  R0,3,ENDTIME        NO, USE THIS TIME AS ENDTIME                 
         CLC   STRTTIME,=H'600'    IS START TIME AFTER 6AM?                     
         BNL   AML20X                                                           
         CLC   ENDTIME,=H'600'     IS ENDTIME AFTER 6AM?                        
         BNH   AML2065                                                          
         MVC   ENDTIME,=H'600'     YES, USE 6AM AS ENDTIME                      
AML2065  CLC   ENDTIME,TATMM3NT    IF MEAL END TIME IS BEFORE 16 HOURS,         
         BNH   AML2070                                                          
         MVC   ENDTIME,TATMM3NT    USE MEAL END TIME                            
*                                                                               
AML2070  BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL3 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML20       ADD MEALS 1 + 2                              
         BRAS  RE,ADDHRS                                                        
         MVC   SVML20,DIFFTHRS     MEAL DURING 20% PREMIUM                      
AML20X   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*--------------------------------------------------------------------*          
*              ARE ANY MEALS ON FRIDAY BETWEEN 8PM-12M - PREMIUM TIME*          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJML10F NTR1  BASE=*,LABEL=*                                                   
         XC    SVML10F,SVML10F                                                  
*                                                                               
         OC    TATMM1ST,TATMM1ST   IS MEAL 1 ON FRIDAY 8PM - 12M?               
         BZ    AMF1020                                                          
         CLC   TATMM1NT,=H'2000'   IF MEAL ENDS AFTER 8PM                       
         BNH   AMF1020                                                          
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL NON-DEDUCTIBLE?                      
         BNZ   AMF1020             IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST   START OF MEAL                                
         MVC   MEALEND,TATMM1NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AMF1020                                                          
*                                                                               
         MVC   STRTTIME,TATMM1ST   MEAL1 START                                  
         CLC   TATMM1ST,=H'2000'   DOES MEAL START BEFORE 8PM?                  
         BNL   *+10                                                             
         MVC   STRTTIME,=H'2000'   START COUNTING MEAL AT 8PM                   
         MVC   ENDTIME,TATMM1NT    MEAL1 END                                    
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   SVML10F,DIFFTHRS    MEAL DURING 10% PREMIUM (FRIDAY)             
*                                                                               
AMF1020  OC    TATMM2ST,TATMM2ST   IS MEAL 2 ON FRIDAY 8PM - 12M?               
         BZ    AMF10X                                                           
         CLC   TATMM2NT,=H'2000'   IF MEAL ENDS AFTER 8PM                       
         BNH   AMF10X                                                           
*                                                                               
         MVC   MEALSTRT,TATMM2ST   START OF MEAL                                
         MVC   MEALEND,TATMM2NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AMF10X                                                           
*                                                                               
         MVC   STRTTIME,TATMM2ST   MEAL2 START                                  
         CLC   TATMM2ST,=H'2000'   DOES MEAL START BEFORE 8PM?                  
         BNL   *+10                                                             
         MVC   STRTTIME,=H'2000'   START COUNTING MEAL AT 8PM                   
         MVC   ENDTIME,TATMM2NT    MEAL2 END                                    
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML10F                                                   
         BRAS  RE,ADDHRS                                                        
         MVC   SVML10F,DIFFTHRS    MEAL DURING 10% PREMIUM FRIDAY               
AMF10X   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*--------------------------------------------------------------------*          
*              ARE ANY MEALS ON FRIDAY BETWEEN 1AM-6AM - PREMIUM TIME*          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJML20F NTR1  BASE=*,LABEL=*                                                   
         XC    SVML20F,SVML20F                                                  
*                                                                               
         OC    TATMM1ST,TATMM1ST   IS MEAL 1 ON FRIDAY 1AM - 6AM?               
         BZ    AMF2020                                                          
         CLC   TATMM1ST,=H'600'    IF MEAL START IS BEFORE 6AM                  
         BH    AMF2020                                                          
         CLC   TATMM1NT,=H'100'    AND MEAL ENDS IS AFTER 1AM?                  
         BNH   AMF2020                                                          
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL NON-DEDUCTIBLE?                      
         BNZ   AMF2020             IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST   START OF MEAL                                
         MVC   MEALEND,TATMM1NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AMF2020                                                          
*                                                                               
         CLC   TATMM1ST,=H'100'    DOES MEAL START BEFORE 1AM?                  
         BNL   AMF2010                                                          
         MVC   STRTTIME,=H'100'    START COUNTING MEAL AT 1AM                   
         B     *+10                                                             
AMF2010  MVC   STRTTIME,TATMM1ST   MEAL1 START                                  
*                                                                               
         CLC   TATMM1NT,=H'600'    DOES MEAL END AFTER 6AM?                     
         BNH   AMF2015                                                          
         MVC   ENDTIME,=H'600'     END MEAL AT 6AM                              
         B     *+10                                                             
AMF2015  MVC   ENDTIME,TATMM1NT    MEAL1 END                                    
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   SVML20F,DIFFTHRS    MEAL DURING 20% PREMIUM (FRIDAY)             
*                                                                               
AMF2020  OC    TATMM2ST,TATMM2ST   IS MEAL 2 ON FRIDAY 1AM - 6AM?               
         BZ    AMF2050                                                          
         CLC   TATMM2ST,=H'600'    IF MEAL START IS BEFORE 6AM                  
         BH    AMF2050                                                          
         CLC   TATMM2NT,=H'100'    AND MEAL ENDS IS AFTER 1AM?                  
         BNH   AMF2050                                                          
*                                                                               
         MVC   MEALSTRT,TATMM2ST   START OF MEAL                                
         MVC   MEALEND,TATMM2NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AMF2050                                                          
*                                                                               
         CLC   TATMM2ST,=H'100'    DOES MEAL START BEFORE 1AM?                  
         BNL   AMF2030                                                          
         MVC   STRTTIME,=H'100'    START COUNTING MEAL AT 1AM                   
         B     *+10                                                             
AMF2030  MVC   STRTTIME,TATMM2ST   MEAL2 START                                  
*                                                                               
         CLC   TATMM2NT,=H'600'    DOES MEAL END AFTER 6AM?                     
         BNH   AMF2040                                                          
         MVC   ENDTIME,=H'600'      END MEAL AT 6AM                             
         B     *+10                                                             
AMF2040  MVC   ENDTIME,TATMM2NT    MEAL2 END                                    
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL2 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML20F                                                   
         BRAS  RE,ADDHRS                                                        
         MVC   SVML20F,DIFFTHRS    MEAL DURING 20% PREMIUM FRIDAY               
*                                                                               
AMF2050  OC    TATMM3ST,TATMM3ST   IS MEAL 3 ON FRIDAY 1AM - 6AM?               
         BZ    AMF20X                                                           
         CLC   TATMM3ST,=H'600'    IF MEAL START IS BEFORE 6AM                  
         BH    AMF20X                                                           
         CLC   TATMM3NT,=H'100'    AND MEAL ENDS IS AFTER 1AM?                  
         BNH   AMF20X                                                           
*                                                                               
         MVC   MEALSTRT,TATMM3ST   START OF MEAL                                
         MVC   MEALEND,TATMM3NT    END OF MEAL                                  
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   AMF20X                                                           
*                                                                               
         CLC   TATMM3ST,=H'100'    DOES MEAL START BEFORE 1AM?                  
         BNL   AMF2060                                                          
         MVC   STRTTIME,=H'100'    START COUNTING MEAL AT 1AM                   
         B     *+10                                                             
AMF2060  MVC   STRTTIME,TATMM3ST   MEAL3 START                                  
*                                                                               
         CLC   TATMM3NT,=H'600'    DOES MEAL END AFTER 6AM?                     
         BNH   AMF2070                                                          
         MVC   ENDTIME,=H'600'     END MEAL AT 6AM                              
         B     *+10                                                             
AMF2070  MVC   ENDTIME,TATMM3NT    MEAL3 END                                    
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL3 ON SAT             
         CLC   DIFFTHRS,=H'100'    IF MEAL IS LONGER THAN 1 HOUR                
         BNH   *+10                                                             
         MVC   DIFFTHRS,=H'100'    JUST COUNT 1 HOUR                            
         MVC   HRMIN1,DIFFTHRS                                                  
         MVC   HRMIN2,SVML20F      ADD TO MEAL1 + MEAL2                         
         BRAS  RE,ADDHRS                                                        
         MVC   SVML20F,DIFFTHRS    MEAL DURING 20% PREMIUM FRIDAY               
AMF20X   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ADD TRAVEL PREMIUM HOURS (PAY STRAIGHT TIME)          *          
*              SVNPREM RETURNED W/ NUMBER OF PRINTABLE HOURS         *          
*              PREMHRS RETURNED W/ NUMBER OF HRS TO X BY HOURLY RATE *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADDTRVL  NTR1  BASE=*,LABEL=*                                                   
         OC    TRVLPH10,TRVLPH10                                                
         BZ    ADTRVL10                                                         
         MVC   HRMIN1,TRVLPH10     ADD PREM TRVL HRS                            
         MVC   HRMIN2,TATMNP10                                                  
         BRAS  RE,ADDHRS                                                        
         MVC   TATMNP10,DIFFTHRS                                                
*                                                                               
         ZICM  R0,SVNP10,2         ADD PREM TRVL HRS                            
         MVC   DIFFTHRS,TRVLPH10   TO ADJUSTED PREMIUM HOURS                    
         BRAS  RE,HRS2DEC                                                       
         AR    R1,R0                                                            
         STCM  R1,3,SVNP10                                                      
*                                                                               
ADTRVL10 OC    TRVLPH20,TRVLPH20                                                
         BZ    ADTRVLX                                                          
         MVC   HRMIN1,TRVLPH20     ADD PREM TRVL HRS                            
         MVC   HRMIN2,TATMNP20                                                  
         BRAS  RE,ADDHRS                                                        
         MVC   TATMNP20,DIFFTHRS                                                
*                                                                               
         ZICM  R0,SVNP20,2         ADD PREM TRVL HRS                            
         MVC   DIFFTHRS,TRVLPH20   TO ADJUSTED PREMIUM HOURS                    
         BRAS  RE,HRS2DEC                                                       
         AR    R1,R0                                                            
         STCM  R1,3,SVNP20                                                      
ADTRVLX  XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*        ADJUST TRAVEL FOR PRINCIPALS AFTER MIDNIGHT ON WEEKNIGHTS   *          
*        ALL TRVL HRS AFTER MIDNIGHT SHOULD GET PAID TIME AND A HALF *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJTRMD  NTR1  BASE=*,LABEL=*                                                   
         XC    TRAVDED2,TRAVDED2                                                
*                                                                               
         TM    TGCSORT,X'20'        PRINCIPALS ONLY,                            
         BO    ADJTRMDX                                                         
         TM    DAYTYPE,DAYSAT+DAYSUN+DAYHLDY   IF WEEKDAY,                      
         BNZ   ADJTRMDX                                                         
         CLC   TATMTFAR,TATMWTST   IF TRAVEL END > WORK START,                  
         BNL   ADJTRMDX            WORK IS ALL BEFORE MIDNIGHT                  
*                                                                               
         OC    TRAVDED,TRAVDED     ANY TRAVEL DEDUCTIONS?                       
         BZ    ATRMD30                                                          
         OC    TATMTTDP,TATMTTDP   ANY TRAVEL TO?                               
         BNZ   ATRMD20                                                          
         MVC   TRAVDED2,TRAVDED    TRAVEL DEDUCTION AFTER MIDNIGHT              
         B     ATRMD30                                                          
ATRMD20  MVC   STRTTIME,TATMTTDP   GET TRAVEL TO MINUTES                        
         MVC   ENDTIME,TATMTTAR                                                 
         BRAS  RE,TIMDIFF          RETURNS NUMBER OF HOURS START TO END         
         LH    R1,DIFFTMIN                                                      
         LH    R0,TRAVDED                                                       
         CR    R0,R1               IF TRAVDED > TRAVEL TO TIME                  
         BNH   ATRMD30                                                          
         SR    R0,R1               DEDUCT TRAVEL TO FROM TRAVDED                
         STH   R0,TRAVDED2         TRAVEL DEDUCTION AFTER MIDNIGHT              
*                                                                               
ATRMD30  MVC   STRTTIME,TATMTIDP                                                
         OC    TATMTIDP,TATMTIDP   ANY INTERVENING TRAVEL?                      
         BNZ   ATRMD40                                                          
         MVC   STRTTIME,TATMTFDP                                                
         OC    TATMTFDP,TATMTFDP   ANY TRAVEL FROM?                             
         BZ    ADJTRMDX                                                         
ATRMD40  CLC   STRTTIME,=H'1200'   DOES TRAVEL START AFTER MIDNIGHT?            
         BNH   ATRMD50                                                          
         CLC   TATMWTST,TATMWTNT   IF WORK START < WORK END                     
         BH    ATRMD50             WORK GOES PAST MIDNIGHT                      
         OC    TRAVDED2,TRAVDED2                                                
         BZ    ATRMD45                                                          
         MVC   ENDTIME,=H'2400'                                                 
         BRAS  RE,TIMDIFF          TRAVEL FROM BEFORE MIDNIGHT                  
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         LH    R0,TRAVDED2         IF TRAVDED IS <=TRVL BEFORE MIDNIGHT         
         CR    R0,R1                                                            
         BH    ATRMD43                                                          
         XC    TRAVDED2,TRAVDED2   NO TRAVDED AFTER MIDNIGHT                    
         B     ATRMD45                                                          
ATRMD43  SR    R0,R1               DEDUCT TRAVEL BEFORE MIDNIGHT                
         STH   R0,TRAVDED2         NEW TRAVDED AFTER MIDNIGHT                   
ATRMD45  MVC   STRTTIME,=H'0000'   NO, START AT MIDNIGHT                        
ATRMD50  MVC   ENDTIME,TATMTFAR    TRAVEL FROM ARRIVE                           
         BRAS  RE,TIMDIFF                                                       
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         OC    TRAVDED2,TRAVDED2                                                
         BZ    ATRMD60                                                          
         LH    R0,TRAVDED2         IF TRAVDED > TRAV AFTER MIDNIGHT             
         CR    R0,R1                                                            
         BNL   ADJTRMDX            DON'T ADJUST ANYTHING                        
         SR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
ATRMD60  BRAS  RE,MIN2HRS          CONVERT REMAINING TRVL TIME TO HRS           
         BRAS  RE,RNDQRTR          ROUND TO QUARTER HOUR                        
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         SRL   R1,1                DIVIDE TRAVEL MINUTES BY 2                   
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         MVC   EXTRVL,DIFFTHRS     EXTRA TRAVEL MINUTES                         
ADJTRMDX XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ADJUST TRAVEL FOR EXTRAS USING THE 16 HOUR RULE             *          
*        ALL OR SOME OF TRAVEL FROM WILL GET PAID BY 16 HOUR RULE    *          
*        DO NOT PAY TRAVEL FROM AFTER 16 HOURS AS TRAVEL HOURS       *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
ADJTR16  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         TM    TGCSORT,X'20'       EXTRAS ONLY,                                 
         BNO   ATR16X                                                           
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE                           
         BZ    ATR16X                                                           
*                                                                               
         OC    TATMTFDP,TATMTFDP   ANY TRAVEL FROM HOURS?                       
         BZ    ATR16X                                                           
*                                                                               
         MVC   STRTTIME,TATMTTDP   GET START OF DAY                             
         OC    TATMTTDP,TATMTTDP   ANY TRAVEL TO HOURS?                         
         BNZ   *+10                                                             
         MVC   STRTTIME,TATMWTST   WORK START                                   
         ZICM  R0,STRTTIME,2                                                    
         AHI   R0,1600                                                          
         STCM  R0,3,ENDTIME        ENDTIME IS START + 16 HOURS                  
*                                                                               
         MVC   SVSTRT,TATMTFDP     TRAVEL FROM                                  
         OC    TATMTIDP,TATMTIDP   ANY INTERVENING TRAVEL?                      
         BZ    ATR1610                                                          
         MVC   SVSTRT,TATMTIDP     INTERVENING TRAVEL                           
         CLC   STRTTIME,TATMTIDP   IF START TIME > TRAVEL INTV                  
         BNH   ATR1620                                                          
         ZICM  RE,TATMTIDP,2                                                    
         AHI   RE,2400             ADD 2400 TO TRAVEL FROM TIME                 
         STCM  RE,3,SVSTRT                                                      
         B     ATR1620                                                          
ATR1610  CLC   STRTTIME,TATMTFDP   IF START TIME > TRAVEL FROM                  
         BNH   ATR1620                                                          
         ZICM  RE,TATMTFDP,2                                                    
         AHI   RE,2400             ADD 2400 TO TRAVEL FROM TIME                 
         STCM  RE,3,SVSTRT                                                      
*                                                                               
ATR1620  CLC   SVSTRT,ENDTIME      IF TRAVEL FROM STARTS AFTER 16 HOURS         
         BNH   ATR1630                                                          
         XC    TRVLHRS,TRVLHRS     TRAVEL TIME HOURS ARE NOT PAID TRVL          
         XC    SVTRVL,SVTRVL                                                    
         B     ATR16X                                                           
*                                                                               
ATR1630  MVC   STRTTIME,SVSTRT      CALCULATE TIME BETWEEN TRAVEL FROM          
         BRAS  RE,TIMDIFF           START AND END OF 16 HOURS                   
         MVC   TRVLHRS,DIFFTHRS     AMOUNT OF TRAVEL HOURS NOT ROUNDED          
         BRAS  RE,RNDQRTR           ROUND TO QUARTER HOUR                       
         MVC   SVTRVL,DIFFTHRS      NEW TRAVEL TIME ROUNDED                     
*                                                                               
ATR16X   XIT1                                                                   
         DROP R4                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              SEND MESSAGE TO CALC NIGHT PREM - YES OR NO           *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
NGHTPYN  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMNHTPH         DO WE WANT TO ADD NIGHT PREMIUM?             
         CLI   8(R2),C'Y'                                                       
         BE    NTPYN20                                                          
         CLI   8(R2),C'N'          IF NO, DON'T BOTHER CALCULATING              
         BNE   NTPYN10                                                          
         OI    TATMSTAT,TATMSNPN   DON'T CALCULATE NIGHT PREMIUM                
         XC    SVNP10,SVNP10                                                    
         XC    SVNP20,SVNP20                                                    
         XC    TATMNP10,TATMNP10                                                
         XC    TATMNP20,TATMNP20                                                
         XC    SMKNP10,SMKNP10                                                  
         XC    SMKNP20,SMKNP20                                                  
         CLI   SVACTRA,0           IF ACTRA, CLEAR DOUBLETIME NP                
         BE    NTPYNX                                                           
         XC    SVNPDT,SVNPDT                                                    
         B     NTPYNX                                                           
NTPYN10  CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         TM    TGFASTAT,TGFROMFA   SEND MESSAGE TO ENTER Y OR N                 
         BZ    YNNGHTP                                                          
         GOTOR ADDWERR,DMCB,=AL2(ERNGHPYN),0                                    
         B     NTPYNX                                                           
*                                                                               
NTPYN20  OI    TATMSTAT,TATMSNPY   SET STATUS TO CALCULATE NIGHT PREM           
NTPYNX   XIT1                                                                   
         DROP  R4                                                               
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
YNNGHTP  MVC   MYMSGNO,=Y(ERNGHPYN) ENTER Y/N FOR NIGHT PREMIUM                 
         OI    GENSTAT2,USGETTXT    NEW THEEND FOR TWO BYTE ERROR MSGS          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         GOTO1 EXIT,DMCB,0          (WEB COVERED BY ADDWERR CALL ABOVE)         
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE NON DEDUCTIBLE MEAL FIELD                               
*              R4 --> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
VALNDML  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMM1NDH         NON DEDUCTIBLE MEAL?                         
         CLI   5(R2),0             ANY INPUT? DEFAULT IS NO                     
         BE    VNDMLX                                                           
         CLI   8(R2),C'N'                                                       
         BE    VNDMLX                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TATMSTA2,TATMS2NM  TURN ON NON DEDUCTIBLE MEAL STATUS            
VNDMLX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*              SHOW MEAL PENALTY AMOUNTS                                        
*              R4 ---> ELEM                                                     
*              MAR27/07 - AS PER FRAN, CALCULATE ON ALL TRAVEL TIME             
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
MEALPEN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0                                                            
         XC    MPENTOT1,MPENTOT1                                                
         XC    MPENTOT2,MPENTOT2                                                
         XC    MPENTOT3,MPENTOT3                                                
*                                                                               
         TM    TATMSTA2,TATMS2WC   WEATHER CANCELLATION?                        
         BO    MEALPX              NO MEAL PENALTIES ALLOWED                    
*                                                                               
*                                  SET START TIME                               
**NO-OP  TM    TGCSORT,X'20'       EXTRAS ONLY - START WITH TRAVEL TIME         
**MAR07  BNO   MEALP10                                                          
         OC    TATMWTST,TATMWTST   IS THIS A TRAVEL DAY? (NO WORK)              
         BZ    MEALPX                                                           
         MVC   STRTTIME,TATMTTDP   TRAVEL TO - DEPART                           
         OC    TATMTTDP,TATMTTDP                                                
         BNZ   *+10                                                             
MEALP10  MVC   STRTTIME,TATMWTST   OR WORK TIME - START                         
         MVC   SVSTRT,STRTTIME                                                  
*                                                                               
*                                  SET END TIME                                 
         MVC   ENDTIME,TATMTFAR    TRAVEL FROM - ARRIVE                         
         OC    TATMTFAR,TATMTFAR                                                
         BNZ   *+10                                                             
         MVC   ENDTIME,TATMWTNT    WORK TIME - END                              
         MVC   SVEND,ENDTIME       SAVE END TIME                                
*                                                                               
         BRAS  RE,TIMDIFF          CALCULATE TIME DIFFERENCE                    
         CLI   SVACTRA,0           IF USING ACTRA RULES,                        
         BE    MEALP20                                                          
         CLI   SVACTRA,CCTY04B     GIVE A 15 MINUTE BUFFER                      
         BE    MEALP20                                                          
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    MEALP20                                                          
         CLC   DIFFTHRS,=H'615'    IF DIFF <=6.15 HOURS, NO PENALTY             
         B     *+10                                                             
MEALP20  CLC   DIFFTHRS,=H'600'    IF DIFF <=6 HOURS, NO PENALTY                
         BNH   MEALPX                                                           
*                                                                               
         OC    TATMM1ST,TATMM1ST   MEAL1 START                                  
         BNZ   MEALP30                                                          
         BRAS  RE,CALCPEN          THERE MUST BE A MEAL EVERY 6 HRS             
         A     R0,MPENTOT1                                                      
         ST    R0,MPENTOT1         TOTAL AMOUNT OF MEAL PENALTY 1               
         SR    R0,R0                                                            
         B     MEALP120                                                         
MEALP30  MVI   NDMFLAG,0                                                        
         MVC   MEALSTRT,TATMM1ST   MEAL 1 START                                 
         MVC   MEALEND,TATMM1NT    MEAL 1 END                                   
         TM    TATMSTA2,TATMS2NM   NON-DEDUCTIBLE MEAL?                         
         BNO   *+8                                                              
         MVI   NDMFLAG,C'Y'                                                     
         BRAS  RE,CHKMEAL          CHECK IF MEAL IS WITHIN WORK TIME            
         BNE   MEALP50             IGNORE THIS MEAL                             
         CLI   STRTFLG,C'Y'        IF MEAL START IS BEFORE WORK START           
         BNE   MEALP40                                                          
         MVC   STRTTIME,MEALEND    SET MEAL1 END AS START TIME                  
         B     MEALP50             AND LOOK FOR NEXT MEAL                       
MEALP40  MVC   ENDTIME,TATMM1ST    SET MEAL1 START AS END TIME                  
*                                                                               
         BRAS  RE,TIMDIFF          DIFF FROM DAY START TO MEAL1                 
         CLI   SVACTRA,0           IF USING ACTRA RULES,                        
         BE    MEALP45                                                          
         CLI   SVACTRA,CCTY04B     GIVE A 15 MINUTE BUFFER                      
         BE    MEALP45                                                          
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    MEALP45                                                          
         CLC   DIFFTHRS,=H'615'    IF DIFF <=6.15 HOURS, NO PENALTY             
         B     *+10                                                             
MEALP45  CLC   DIFFTHRS,=H'600'    IF DIFF <=6 HOURS, OKAY                      
         BNH   MEALP48                                                          
         BRAS  RE,CALCPEN          CALCULATE PENALTY                            
         A     R0,MPENTOT1                                                      
         ST    R0,MPENTOT1         TOTAL AMOUNT OF MEAL PENALTY 1               
         SR    R0,R0                                                            
*                                                                               
MEALP48  CLI   ENDFLG,C'Y'         IF END OF MEAL IS AFTER END OF WORK          
         BE    MEALP120            DONE                                         
         MVC   STRTTIME,MEALEND    SET MEAL1 END AS START TIME                  
**NO-OP  TM    TATMSTA2,TATMS2NM   IF MEAL1 IS NON-DEDUCTIBLE,                  
**07/09  BNO   MEALP50                                                          
**NO-OP  MVC   STRTTIME,MEALSTRT   SET MEAL1 START AS START TIME                
*                                                                               
MEALP50  MVC   ENDTIME,TATMM2ST    SET MEAL2 START AS END TIME                  
         OC    TATMM2ST,TATMM2ST   MEAL2 START?                                 
         BZ    MEALP100                                                         
         MVC   MEALSTRT,TATMM2ST   MEAL 2 START                                 
         MVC   MEALEND,TATMM2NT    MEAL 2 END                                   
         MVI   NDMFLAG,0                                                        
         BRAS  RE,CHKMEAL          CHECK IF MEAL IS WITHIN WORK TIME            
         BNE   MEALP70                                                          
         CLI   STRTFLG,C'Y'        IF MEAL START IS BEFORE WORK START           
         BNE   MEALP60                                                          
         MVC   STRTTIME,MEALEND    SET MEAL2 END AS START TIME                  
         B     MEALP70             AND LOOK FOR NEXT MEAL                       
*                                                                               
MEALP60  BRAS  RE,TIMDIFF                                                       
         CLI   SVACTRA,0           IF USING ACTRA RULES,                        
         BE    MEALP65                                                          
         CLI   SVACTRA,CCTY04B     GIVE A 15 MINUTE BUFFER                      
         BE    MEALP65                                                          
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    MEALP65                                                          
         CLC   DIFFTHRS,=H'615'    IF DIFF <=6.15 HOURS, NO PENALTY             
         B     *+10                                                             
MEALP65  CLC   DIFFTHRS,=H'600'    IF DIFF <=6 HOURS, OKAY                      
         BNH   MEALP68                                                          
         BRAS  RE,CALCPEN          CALCULATE PENALTY                            
         A     R0,MPENTOT2                                                      
         ST    R0,MPENTOT2         TOTAL AMOUNT OF MEAL PENALTY 2               
         SR    R0,R0                                                            
*                                                                               
MEALP68  CLI   ENDFLG,C'Y'         IF END OF MEAL IS AFTER END OF WORK          
         BE    MEALP120            DONE                                         
         MVC   STRTTIME,MEALEND    SET MEAL2 END AS START TIME                  
*                                                                               
MEALP70  MVC   ENDTIME,TATMM3ST    SET MEAL3 START AS END TIME                  
         OC    TATMM3ST,TATMM3ST   MEAL3 START?                                 
         BZ    MEALP100                                                         
         MVC   MEALSTRT,TATMM3ST   MEAL 3 START                                 
         MVC   MEALEND,TATMM3NT    MEAL 3 END                                   
         MVI   NDMFLAG,0                                                        
         BRAS  RE,CHKMEAL          CHECK IF MEAL IS WITHIN WORK TIME            
         BNE   MEALP100                                                         
         CLI   STRTFLG,C'Y'        IF MEAL START IS BEFORE WORK START           
         BNE   MEALP80                                                          
         MVC   STRTTIME,MEALEND    SET MEAL3 END AS START TIME                  
         B     MEALP100            AND SET END OF DAY AS END TIME               
*                                                                               
MEALP80  BRAS  RE,TIMDIFF                                                       
         CLI   SVACTRA,0           IF USING ACTRA RULES,                        
         BE    MEALP85                                                          
         CLI   SVACTRA,CCTY04B     GIVE A 15 MINUTE BUFFER                      
         BE    MEALP85                                                          
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    MEALP85                                                          
         CLC   DIFFTHRS,=H'615'    IF DIFF <=6.15 HOURS, NO PENALTY             
         B     *+10                                                             
MEALP85  CLC   DIFFTHRS,=H'600'    IF DIFF <=6 HOURS, OKAY                      
         BNH   MEALP88                                                          
         BRAS  RE,CALCPEN          CALCULATE PENALTY                            
         A     R0,MPENTOT3                                                      
         ST    R0,MPENTOT3         TOTAL AMOUNT OF MEAL PENALTY 3               
         SR    R0,R0                                                            
*                                                                               
MEALP88  CLI   ENDFLG,C'Y'         IF END OF MEAL IS AFTER END OF WORK          
         BE    MEALP120            DONE                                         
         MVC   STRTTIME,MEALEND    SET MEAL3 END AS START TIME                  
*                                                                               
MEALP100 MVC   ENDTIME,SVEND       END OF WORK/TRAVEL                           
         BRAS  RE,TIMDIFF                                                       
         CLI   SVACTRA,0           IF USING ACTRA RULES,                        
         BE    MEALP110                                                         
         CLI   SVACTRA,CCTY04B     GIVE A 15 MINUTE BUFFER                      
         BE    MEALP110                                                         
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    MEALP110                                                         
         CLC   DIFFTHRS,=H'615'    IF DIFF <=6.15 HOURS, NO PENALTY             
         B     *+10                                                             
MEALP110 CLC   DIFFTHRS,=H'600'    IF DIFF <=6 HOURS, OKAY                      
         BNH   *+8                                                              
         BRAS  RE,CALCPEN          CALCULATE PENALTY                            
*                                  LATEST MEAL GETS THE PENALTY                 
         OC    TATMM2ST,TATMM2ST   IS THERE A MEAL2?                            
         BZ    MEALP115                                                         
         A     R0,MPENTOT3                                                      
         ST    R0,MPENTOT3         TOTAL AMOUNT OF MEAL PENALTY 3               
         SR    R0,R0                                                            
         B     MEALP120                                                         
*                                                                               
MEALP115 A     R0,MPENTOT2                                                      
         ST    R0,MPENTOT2         TOTAL AMOUNT OF MEAL PENALTY 2               
         SR    R0,R0                                                            
*                                                                               
MEALP120 OC    MPENTOT1,MPENTOT1   DO WE HAVE MEAL PENALTY 1?                   
         BZ    MEALP150                                                         
*                                                                               
         LA    R2,STMMLPHH                                                      
         CLI   8(R2),C'Y'                                                       
         BE    MEALP140                                                         
         CLI   8(R2),C'N'          IF NO, DON'T BOTHER CALCULATING              
         BNE   MEALP130                                                         
         OI    TATMSTAT,TATMSMPN   TURN ON STATUS FOR NO MEAL PENALTY           
         XC    MPENTOT1,MPENTOT1                                                
         B     MEALP150                                                         
MEALP130 CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         TM    TGFASTAT,TGFROMFA   SEND MESSAGE TO ENTER Y OR N                 
         BZ    YNMPEN                                                           
         GOTOR ADDWERR,DMCB,=AL2(ERMPENYN),D#TMMP1                              
         B     MEALPX                                                           
*                                                                               
MEALP140 OI    TATMSTAT,TATMSMPY   TURN ON MEAL PENALTY 1 STATUS                
*                                                                               
MEALP150 OC    MPENTOT2,MPENTOT2   DO WE HAVE MEAL PENALTY 2?                   
         BZ    MEALP180                                                         
*                                                                               
         LA    R2,STMMLP2H                                                      
         CLI   8(R2),C'Y'                                                       
         BE    MEALP170                                                         
         CLI   8(R2),C'N'          IF NO, DON'T BOTHER CALCULATING              
         BNE   MEALP160                                                         
         OI    TATMSTAT,TATMSM2N   TURN ON STATUS FOR NO MEAL PENALTY           
         XC    MPENTOT2,MPENTOT2                                                
         B     MEALP180                                                         
MEALP160 CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         TM    TGFASTAT,TGFROMFA   SEND MESSAGE TO ENTER Y OR N                 
         BZ    YNMPEN                                                           
         GOTOR ADDWERR,DMCB,=AL2(ERMPENYN),D#TMMP2                              
         B     MEALPX                                                           
*                                                                               
MEALP170 OI    TATMSTAT,TATMSM2Y   TURN ON MEAL PENALTY 2 STATUS                
*                                                                               
MEALP180 OC    MPENTOT3,MPENTOT3   DO WE HAVE MEAL PENALTY 3?                   
         BZ    MEALP200                                                         
*                                                                               
         LA    R2,STMMLP3H                                                      
         CLI   8(R2),C'Y'                                                       
         BE    MEALP195                                                         
         CLI   8(R2),C'N'          IF NO, DON'T BOTHER CALCULATING              
         BNE   MEALP190                                                         
         OI    TATMSTA3,TATMSM3N   TURN ON STATUS FOR NO MEAL PENALTY           
         XC    MPENTOT3,MPENTOT3                                                
         B     MEALP200                                                         
MEALP190 CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         TM    TGFASTAT,TGFROMFA   SEND MESSAGE TO ENTER Y OR N                 
         BZ    YNMPEN                                                           
         GOTOR ADDWERR,DMCB,=AL2(ERMPENYN),D#TMMP3                              
         B     MEALPX                                                           
*                                                                               
MEALP195 OI    TATMSTA3,TATMSM3Y   TURN ON MEAL PENALTY STATUS                  
*                                                                               
MEALP200 ZICM  R1,SVNSPNH,4        UPDATE TOTAL NOT SUBJ TO P&H                 
         TM    TATMSTAT,TATMSMPY   PAYING MEAL PENALTY 1?                       
         BZ    *+8                                                              
         A     R1,MPENTOT1                                                      
         TM    TATMSTAT,TATMSM2Y   PAYING MEAL PENALTY 2?                       
         BZ    *+8                                                              
         A     R1,MPENTOT2                                                      
         TM    TATMSTA3,TATMSM3Y   PAYING MEAL PENALTY 3?                       
         BZ    *+8                                                              
         A     R1,MPENTOT3                                                      
         STCM  R1,15,SVNSPNH                                                    
*                                                                               
MEALPX   XIT1                                                                   
         DROP  R4                                                               
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
YNMPEN   MVC   MYMSGNO,=Y(ERMPENYN) ENTER Y/N FOR MEAL PENALTY                  
         OI    GENSTAT2,USGETTXT    NEW THEEND FOR TWO BYTE ERROR MSGS          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         GOTO1 EXIT,DMCB,0          (WEB COVERED BY ADDWERR CALL ABOVE)         
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CHECK IF MEAL FALLS DURING WORK TIME                             
*              MINIMUM MEAL LENGTH IS 30 MINUTES, < 30 RETURNS CC NEQ           
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CHKMEAL  NTR1  BASE=*,LABEL=*                                                   
         MVI   ENDFLG,0                                                         
         MVI   STRTFLG,0                                                        
*                                                                               
         BRAS  RE,GMLEND           GET MEAL END - CHANGES MEALEND               
         CLI   NDMFLAG,C'Y'        NDM CAN BE < 30 MINUTES                      
         BE    CMEAL05                                                          
         CLC   DIFFTHRS,=H'30'     IS MEAL AT LEAST 30 MINUTES                  
         BL    CMEALNO             IF NOT, IGNORE MEAL                          
*                                                                               
CMEAL05  MVC   SVEND2,SVEND                                                     
         CLC   SVSTRT,SVEND        IF WORK START > WORK END                     
         BNH   CMEAL20             ADD 2400 TO WORK END TIME                    
         ZICM  RE,SVEND,2                                                       
         AHI   RE,2400                                                          
         STCM  RE,3,SVEND2                                                      
         CLC   SVSTRT,MEALSTRT     IF WORK START IS LATER THAN                  
         BNH   CMEAL10             MEAL START                                   
***      CLC   MEALSTRT,=H'1200'   ADD 2400 TO MEALS THAT START OR END          
***      BNL   CMEAL10             BEFORE 1200 NOON                             
         ZICM  RE,MEALSTRT,2                                                    
         AHI   RE,2400                                                          
         STCM  RE,3,MEALSTRT                                                    
CMEAL10  CLC   SVSTRT,MEALEND                                                   
         BNH   CMEAL20                                                          
***      CLC   MEALEND,=H'1200'                                                 
***      BNL   CMEAL20                                                          
         ZICM  RE,MEALEND,2                                                     
         AHI   RE,2400                                                          
         STCM  RE,3,MEALEND                                                     
*                                                                               
CMEAL20  CLC   MEALEND,SVSTRT      IF MEAL END < WORK START,                    
         BNH   CMEALNO             IGNORE                                       
         CLC   MEALSTRT,SVSTRT     IF MEAL START <= WORK START,                 
         BH    CMEAL30                                                          
         MVI   STRTFLG,C'Y'        SET FLAG, IGNORE START OF MEAL               
         B     CMEALYES                                                         
*                                                                               
CMEAL30  CLC   MEALSTRT,SVEND2     IF MEAL START > WORK END,                    
         BNL   CMEALNO             IGNORE                                       
         CLC   MEALEND,SVEND2      IF MEAL END >= WORK END,                     
         BL    CMEALYES                                                         
         MVI   ENDFLG,C'Y'         SET FLAG, IGNORE END OF MEAL                 
         B     CMEALYES                                                         
*                                                                               
CMEALYES XR    RC,RC                                                            
CMEALNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              GET MEAL END - MAXIMUM MEAL LENGTH IS 1 HOUR                     
*              SETS DIFFTHRS WITH LENGTH OF MEAL                                
*              CHANGES MEALEND                                                  
*--------------------------------------------------------------------*          
GMLEND   NTR1  BASE=*,LABEL=*                                                   
         MVC   FULL(2),STRTTIME    SAVE START TIME FOR MEALPEN CALC             
         MVC   FULL+2(2),ENDTIME   SAVE END TIME FOR MEALPEN CALC               
*                                                                               
         MVC   STRTTIME,MEALSTRT   MEAL START                                   
         MVC   ENDTIME,MEALEND     MEAL END                                     
         BRAS  RE,TIMDIFF          CALCULATE LENGTH OF MEAL1                    
         MVC   STRTTIME,MEALEND    SET MEAL END AS START TIME                   
         CLC   DIFFTHRS,=H'100'    IF MEAL > 1 HOUR                             
         BNH   GMLENDX                                                          
         ZICM  RE,MEALSTRT,2       SET MEAL END AS MEAL + 1 HOUR                
         AHI   RE,100                                                           
         STCM  RE,3,MEALEND        AFTER 1 HOUR OF MEAL = WORK TIME             
*                                                                               
GMLENDX  MVC   STRTTIME,FULL       RESTORE START TIME FOR MEALPEN CALC          
         MVC   ENDTIME,FULL+2      RESTORE END TIME FOR MEALPEN CALC            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE MEAL PENALTY AMOUNTS                                   
*--------------------------------------------------------------------*          
CALCPEN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   COADST,0                                                         
         CLI   SVCOTYPE,CTYADD     IF COMM'L TYPE IS ADDENDUM                   
         BNE   CPEN10                                                           
         CLC   SVCOADST,=C'NW'     AND ADD. STATE IS NORTHWEST                  
         BNE   CPEN10                                                           
         OI    COADST,COADSTNW     SET IT AS SUCH                               
*                                                                               
CPEN10   MVC   AIO,AIO1                                                         
                                                                                
         CLI   SVACTRA,0                                                        
         BE    CPEN15                                                           
         CLI   SVACTRA,CCTY04B                                                  
         BE    CPEN15                                                           
         CLI   US2404,C'Y'         2404 US CAST = SAG RATES, SAG RULES          
         BE    CPEN15                                                           
         LH    R0,MPENACT          ACTRA RATE                                   
         CLI   SVACTRA,CCTY04A                                                  
         BNE   CPENX                                                            
         LH    R0,MPEN04A          2404A = SAG RATE, ACTRA RULES                
         B     CPENX                                                            
*                                                                               
CPEN15   SR    R0,R0                                                            
         SR    RE,RE                                                            
         LH    RF,DIFFTMIN         DIFFERENCE IN MINUTES                        
         AHI   RF,-360             SUBTRACT 360 MINS (6HRS) FROM DIFF           
         D     RE,=F'30'           CALCULATE NUMBER OF HALF HOURS               
         LTR   RE,RE               IF THERE IS A REMAINDER, ADD 1               
         BZ    *+8                                                              
         AHI   RF,1                RF = NUM OF HALF HOURS OF PENALTIES          
         CHI   RF,0                EXIT IF ZERO                                 
         BNH   CPENX                                                            
*                                                                               
         SR    R0,R0                                                            
         LA    R3,MPENTAB                                                       
         TM    COADST,COADSTNW     IF NW REGIONAL                               
         BZ    *+8                                                              
         LA    R3,MPENTBNW                                                      
         B     *+8                                                              
                                                                                
CPEN20   LA    R3,L'MPENTAB(R3)                                                 
         CLI   0(R3),X'FF'         IF WE REACH THE END OF TABLE,                
         BNE   CPEN30                                                           
         SHI   R3,L'MPENTAB        GO BACK TO LAST ENTRY IN TABLE               
CPEN30   AH    R0,0(R3)                                                         
         BCT   RF,CPEN20                                                        
*                                                                               
CPENX    XIT1  REGS=(R0)                                                        
*                                                                               
MPENTAB  DS    0H                                                               
         DC    H'2500'             1ST HALF HOUR                                
         DC    H'2500'             2ND HALF HOUR                                
         DC    H'5000'             3RD HALF HOUR                                
         DC    H'5000'             4TH +                                        
         DC    X'FF'                                                            
*                                                                               
MPENTBNW DS    0H                           $50.00 max per meal period          
         DC    H'2500'             1ST HALF HOUR                                
         DC    H'2500'             2ND HALF HOUR                                
         DC    H'0000'                                                          
         DC    X'FF'                                                            
*                                                                               
MPEN04A  DC    H'2500'             2404A = SAG RATE, ACTRA RULES                
*                                                                               
MPENACT  DC    H'8000'             ACTRA RATE                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE SMOKE PAY FIELD                                         
*              R4 --> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
VALSMKP  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMSMPYH         SMOKE PAY?                                   
         CLI   5(R2),0             ANY INPUT? DEFAULT IS NO                     
         BE    VSMKPX                                                           
         CLI   8(R2),C'N'                                                       
         BE    VSMKPX                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         MVI   WBERRFLD,D#TMSMK                                                 
         TM    TGCSORT,X'20'      CAST MEMBER MUST BE AN EXTRA                  
         BNO   FLDINV                                                           
         OI    TATMSTA2,TATMS2SP  TURN ON SMOKE PAY STATUS                      
*                                                                               
VSMKPX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*              CALCULATE SMOKE PAY AMOUNT                                       
*              SMOKE PAY RATE IS ALREADY MULTIPLIED BY 100                      
*              WE DIVIDE BY 100 AT THE END TO CORRECT ROUNDING ERRORS           
*--------------------------------------------------------------------*          
CALCSMKP NTR1  BASE=*,LABEL=*                                                   
         ZICM  RE,SVPYMT,4                                                      
         A     RE,SMOKRATE        SMOKE PAY RATE ON TOP OF SESSION              
         STCM  RE,15,SVPYMT                                                     
*                                                                               
***NO-OP OCT/07  *** DO NOT DOUBLE SMOKE PAY IF SAT/SUN/HOLIDAY ***             
*&&DO                                                                           
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF SAT/SUN/HOLIDAY                
         BZ    CSMKP03                                                          
         L     RE,SVPYMT                                                        
         A     RE,SMOKRATE        PAY DOUBLE SMOKE RATE                         
         ST    RE,SVPYMT                                                        
*&&                                                                             
*                                                                               
CSMKP03  TM    DAYTYPE,DAYFRI     IF FRIDAY, ANY OT OR DT AFTER 12M?            
         BNO   CSMKP04            DON'T DOUBLE OT OR DT SMOKE PAY               
         BRAS  RE,CHKFRI2         DOES TIMESHEET GO INTO SATURDAY?              
         BNE   CSMKP04                                                          
*                                 SAT HOURS DON'T GET DOUBLED                   
         LH    R3,EXSATOT         (SMOKRATE/8) * (OVTIME + DBLTIME HRS)         
         AH    R3,EXSATDT                                                       
*                                                                               
         XR    R0,R0                                                            
         LH    R1,EXSATOT         ADD TOTAL OT AND DT TOGETHER TO SEE           
         AH    R1,EXSATDT         IF WE HAVE A WHOLE NUMBER OR IF WE            
         AH    R1,EXFRIOT         NEED TO ROUND UP                              
         AH    R1,EXFRIDT                                                       
         D     R0,=F'100'          DIVIDE BY 100                                
         LTR   R0,R0                                                            
         BZ    CSMKP03B            IF NOT A WHOLE NUMBER,                       
         LHI   R1,100              DEDUCT REMAINDER FROM 100                    
         SR    R1,R0                                                            
         AR    R3,R1               ROUND UP SAT OT/DT HOURS                     
*                                                                               
CSMKP03B XR    R0,R0                                                            
         L     R1,SMOKRATE                                                      
         D     R0,=F'8'           (SMOKRATE/8) * (OVTIME + DBLTIME HRS)         
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         MR    R0,R3                                                            
         D     R0,=F'100'         DIVIDE BY 100                                 
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
*                                 FRIDAY HOURS GET CALC LIKE WEEKDAYS           
         CLC   EXFRIOT,=H'0'      OVERTIME HOURS?                               
         BE    CSMKP03A                                                         
         LH    R3,EXFRIOT         (1.5)*(SMOKRATE/8) * OVERTIME HRS             
         XR    R0,R0                                                            
         L     R1,SMOKRATE        = 3/2*(SMOKRATE)/8 * OVERTIME HRS             
         MHI   R1,3                                                             
         D     R0,=F'16'                                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         MR    R0,R3                                                            
         D     R0,=F'100'         DIVIDE BY 100                                 
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
*                                                                               
CSMKP03A CLC   EXFRIDT,=H'0'     DOUBLETIME HOURS?                              
         BE    CSMKP20                                                          
         LH    R3,EXFRIDT         = (SMOKRATE/4) * DBLTIME HRS                  
         XR    R0,R0                                                            
         L     R1,SMOKRATE                                                      
         D     R0,=F'4'                                                         
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         MR    R0,R3                                                            
         D     R0,=F'100'         DIVIDE BY 100                                 
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
         B     CSMKP20                                                          
*                                                                               
CSMKP04  TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF SAT/SUN/HOLIDAY                
         BZ    CSMKP05                                                          
         TM    SVSTAT2,TATMS2DL   DISTANT LOCATION?                             
         BNZ   CSMKP05            TREAT AS WEEKDAY                              
         ZIC   R3,SVOTIME         (SMOKRATE/4) * (OVTIME + DBLTIME HRS)         
         ZIC   R4,SVDTIME                                                       
         AR    R3,R4                                                            
         XR    R0,R0                                                            
         L     R1,SMOKRATE                                                      
**NO-OP  D     R0,=F'4'                                                         
         D     R0,=F'8'           (SMOKRATE/8) * (OVTIME + DBLTIME HRS)         
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         MR    R0,R3                                                            
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
         B     CSMKP20                                                          
*                                 REGULAR DAY                                   
CSMKP05  CLI   SVOTIME,0          OVERTIME HOURS?                               
         BE    CSMKP10                                                          
         ZIC   R3,SVOTIME         (1.5)*(SMOKRATE/8) * OVERTIME HRS             
         XR    R0,R0                                                            
         L     R1,SMOKRATE        = 3/2*(SMOKRATE)/8 * OVERTIME HRS             
         MHI   R1,3                                                             
         D     R0,=F'16'                                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         MR    R0,R3                                                            
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
*                                                                               
CSMKP10  CLI   SVDTIME,0          DOUBLETIME HOURS?                             
         BE    CSMKP20                                                          
         ZIC   R3,SVDTIME         = (SMOKRATE/4) * DBLTIME HRS                  
         XR    R0,R0                                                            
         L     R1,SMOKRATE                                                      
         D     R0,=F'4'                                                         
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         MR    R0,R3                                                            
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
*                                                                               
CSMKP20  DS    0H                                                               
*                                                                               
***NO-OP MAY/06  *** DO NOT ADD SMOKE PAY TO TRAVEL FROM HOURS ***              
*&&DO                                                                           
CSMKP20  OC    SVTRVL,SVTRVL                                                    
         BZ    CSMKP30                                                          
         MVC   DIFFTHRS,SVTRVL                                                  
         BRAS  RE,HRS2DEC         CONVERT HRS.MINS TO DECIMAL                   
         LR    R3,R1              TRAVEL TIME RETURNED IN R1                    
         XR    R0,R0                                                            
         L     R1,SMOKRATE        = (SMOKRATE/8) * TRVL TIME                    
         D     R0,=F'8'                                                         
         MR    R0,R3                                                            
         D     R0,=F'100'         DIVIDE BY 100                                 
         LTR   R0,R0              ROUND UP IF ANY REMAINDER                     
         BZ    *+8                                                              
         AHI   R1,1                                                             
         L     RE,SVPYMT                                                        
         AR    RE,R1                                                            
         ST    RE,SVPYMT                                                        
*&&                                                                             
*                                                                               
CSMKP30  OC    SMKNP10,SMKNP10    ANY NP HOURS AFTER MIDNIGHT?                  
         BZ    CSMKP32                                                          
         ZICM  R3,SMKNP10,2                                                     
         XR    R0,R0              = (SMOKRATE * 10%)/8 * NP10 HRS               
         L     R1,SMOKRATE        = (SMOKRATE/80) * NP10 HRS                    
         B     CSMKP35                                                          
*                                                                               
CSMKP32  OC    SVNP10,SVNP10      ADJUSTED NIGHT PREMIUM HOURS                  
         BZ    CSMKP40                                                          
         ZICM  R3,SVNP10,2                                                      
         XR    R0,R0              = (SMOKRATE * 10%)/8 * NP10 HRS               
         L     R1,SMOKRATE        = (SMOKRATE/80) * NP10 HRS                    
*                                                                               
***NO-OP OCT/07  *** DO NOT DOUBLE SMOKE PAY IF SAT/SUN/HOLIDAY ***             
*&&DO                                                                           
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF SAT/SUN/HOLIDAY                
         BZ    CSMKP35             PAY DOUBLE                                   
         D     R0,=F'40'           = (SMOKRATE *10%)/4 * NP10 HRS               
         B     *+8                                                              
*&&                                                                             
*                                                                               
CSMKP35  D     R0,=F'80'                                                        
         MR    R0,R3                                                            
         D     R0,=F'100'         DIVIDE BY 100                                 
         LTR   R0,R0              ROUND UP IF ANY REMAINDER                     
         BZ    *+8                                                              
         AHI   R1,1                                                             
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
*                                                                               
CSMKP40  OC    SMKNP20,SMKNP20    NP AFTER MIDNIGHT IS UNADJUSTED               
         BZ    CSMKP43                                                          
         ZICM  R3,SMKNP20,2                                                     
         XR    R0,R0              = (SMOKRATE * 20%)/8 * NP20 HRS               
         L     R1,SMOKRATE        = (SMOKRATE/160) * NP20 HRS                   
         B     CSMKP45                                                          
*                                                                               
CSMKP43  OC    SVNP20,SVNP20      ADJUST NIGHT PREMIUM HOURS                    
         BZ    CSMKP50                                                          
         ZICM  R3,SVNP20,2                                                      
         XR    R0,R0              = (SMOKRATE * 20%)/8 * NP20 HRS               
         L     R1,SMOKRATE        = (SMOKRATE/40) * NP20 HRS                    
*                                                                               
***NO-OP OCT/07  *** DO NOT DOUBLE SMOKE PAY IF SAT/SUN/HOLIDAY ***             
*&&DO                                                                           
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF SAT/SUN/HOLIDAY                
         BZ    CSMKP45             PAY DOUBLE                                   
         D     R0,=F'80'           = (SMOKRATE *20%)/4 * NP20 HRS               
         B     *+8                                                              
*&&                                                                             
*                                                                               
CSMKP45  D     R0,=F'40'                                                        
         LTR   R0,R0               ROUND UP IF ANY REMAINDER                    
         BZ    *+8                                                              
         AHI   R1,1               ROUND UP IF REMAINDER                         
         MR    R0,R3                                                            
         D     R0,=F'100'         DIVIDE BY 100                                 
         LTR   R0,R0              ROUND UP IF ANY REMAINDER                     
         BZ    *+8                                                              
         AHI   R1,1                                                             
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R1                                                            
         STCM  RE,15,SVPYMT                                                     
*                                                                               
CSMKP50  OC    SMK16HR,SMK16HR    ADJUST 16 HOUR RULE (EXTRAS ONLY)             
         BZ    CSMKPX                                                           
         XR    R2,R2                                                            
         ZIC   R3,SMK16HR         (# OF HOURS OVER 16) X SMOKRATE               
         M     R2,SMOKRATE                                                      
*                                                                               
***NO-OP OCT/07  *** DO NOT DOUBLE SMOKE PAY IF SAT/SUN/HOLIDAY ***             
*&&DO                                                                           
         TM    DAYTYPE,DAYHLDY+DAYSAT+DAYSUN  IF SAT/SUN/HOLIDAY                
         BZ    *+8                                                              
         MHI   R3,2                PAY DOUBLE                                   
*&&                                                                             
         ZICM  RE,SVPYMT,4                                                      
         AR    RE,R3                                                            
         STCM  RE,15,SVPYMT                                                     
*                                                                               
CSMKPX   XR    R0,R0                                                            
         ZICM  R1,SVPYMT,4                                                      
         D     R0,=F'100'         DIVIDE BY 100 (ROUNDING)                      
         LTR   R0,R0              ROUND UP IF ANY REMAINDER                     
         BZ    *+8                                                              
         AHI   R1,1                                                             
         STCM  R1,15,SVPYMT                                                     
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE ADJUSTMENT AND SAVE AMOUNT IN SVADJ                     
*--------------------------------------------------------------------*          
VALADJS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMADJH         ADJUSTMENT                                    
         CLI   5(R2),0                                                          
         BE    VADJSX                                                           
         MVI   WBERRFLD,D#TMADJ                                                 
         CLI   8(R2),C'-'         NEGATIVE NUMBER NOT ALLOWED                   
         BE    FLDINV                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(X'80',(R3))  VALIDATE ADJUSTMENT             
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         MVC   SVADJ,4(R1)                                                      
VADJSX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE 16 HOUR RULE FIELD                                      
*              R4 --> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
VAL16HR  NTR1  BASE=*,LABEL=*                                                   
         XC    SMK16HR,SMK16HR                                                  
         MVI   STM16HR,0                                                        
         OI    STM16HRH+6,X'80'                                                 
*                                                                               
         LA    R2,STM16YNH        16 HOURS?                                     
         MVI   WBERRFLD,D#TM16H                                                 
         CLI   SV16HR,0           IF WE DON'T HAVE MORE THAN 16 HRS             
         BH    V16HR10                                                          
         CLI   5(R2),0            FIELD SHOULD BE BLANK                         
         BE    V16HRX                                                           
         CLI   8(R2),C'N'         OR C'N'                                       
         BNE   FLDINV                                                           
         OI    TATMSTAT,TATMS16N  DON'T CALCULATE 16 HR RULE                    
         B     V16HRX                                                           
*                                                                               
V16HR10  CLI   8(R2),C'N'         IF WE HAVE MORE THAN 16 HOURS,                
         BNE   V16HR15            N = DON'T USE 16 HOUR RULE                    
         MVI   SV16HR,0           GET RID OF EXTRA HOURS                        
         OI    TATMSTAT,TATMS16N  DON'T CALCULATE 16 HOUR RULE                  
         B     V16HRX                                                           
*                                                                               
V16HR15  CLI   8(R2),C'Y'         IF NOT Y,N,OR 0 INVALID                       
         BE    V16HR20                                                          
         CLI   5(R2),0            ANY INPUT?                                    
         BNE   FLDINV                                                           
         TM    TGFASTAT,TGFROMFA  SEND MESSAGE TO ENTER Y OR NN                 
         BZ    YN16HR             YES = USE 16 HOUR RULE                        
         GOTOR ADDWERR,DMCB,=AL2(ER16HRYN),0                                    
         B     V16HRX                                                           
*                                                                               
V16HR20  TM    TGCSORT,X'20'      CAST MEMBER MUST BE AN EXTRA                  
         BNO   FLDINV                                                           
         OI    TATMSTAT,TATMS16Y  CALCULATE 16 HOUR RULE                        
         MVC   TATM16HR,SV16HR    PRINTABLE 16HRS (UNADJUSTED)                  
         MVC   SMK16HR,SV16HR     SAVE UNADJUSTED 16HRS FOR SMOKE PAY           
         BRAS  RE,SAT16HR                                                       
         B     V16HRX                                                           
*                                                                               
V16HRX   XIT1                                                                   
         DROP  R4                                                               
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
YN16HR   MVC   MYMSGNO,=Y(ER16HRYN) ENTER Y/N FOR 16 HR RULE                    
         OI    GENSTAT2,USGETTXT    NEW THEEND FOR TWO BYTE ERROR MSGS          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         GOTO1 EXIT,DMCB,0          (WEB COVERED BY ADDWERR CALL ABOVE)         
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              IF FRIDAY SESSION, CHECK IF ANY HOURS AFTER 16TH HOUR            
*              FALL ON SATURDAY - PAY DOUBLE SESSION FEE                        
*              R4 --> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
SAT16HR  NTR1  BASE=*,LABEL=*                                                   
         TM    DAYTYPE,DAYFRI      FRIDAY?                                      
         BNO   S16HRX                                                           
         CLC   TATMWTNT,TATMWTST   IF WORK END > WORK START,                    
         BNL   S16HRX              WORK IS ALL ON FRIDAY                        
         ZICM  R5,TATMWTNT,2                                                    
         AHI   R5,2400             ADD 2400 TO END TIME                         
*                                                                               
         ZICM  R2,TATMWTST,2       WORK START                                   
         AHI   R2,1600             ADD 16 HOURS TO START TIME                   
         CHI   R2,2400             ARE ALL +16 HRS ON SATURDAY?                 
         BNH   S16HR20                                                          
         ZIC   RE,SV16HR           YES, DOUBLE SV16HR                           
         MHI   RE,2                                                             
         STC   RE,SV16HR                                                        
         B     S16HRX                                                           
*                                                                               
S16HR20  LHI   R1,1440             24 HOURS IN MINUTES (MIDNIGHT)               
         STCM  R2,3,DIFFTHRS       CONVERT START OF 16HRS TO MINS               
         BRAS  RE,HRS2MIN                                                       
         LH    R2,DIFFTMIN         R2 = START OF 16HRS                          
         SR    R1,R2               R1 = # OF +16 HRS ON FRIDAY                  
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS          CONVERT BACK TO HOURS                        
         LH    R1,DIFFTHRS                                                      
         ZIC   R3,SV16HR                                                        
         LHI   R0,0                COUNTER FOR # OF HRS ON FRIDAY               
S16HR30  CHI   R1,100                                                           
         BL    S16HR40                                                          
         SHI   R1,100                                                           
         AHI   R0,1                ADD TO FRIDAY HOURS                          
         SHI   R3,1                DEDUCT FROM SATURDAY HOURS                   
         B     S16HR30                                                          
*                                                                               
S16HR40  MHI   R3,2                DOUBLE 16+ HOURS ON SATURDAY                 
         AR    R3,R0               ADD 16+ HOURS ON FRIDAY                      
         STC   R3,SV16HR                                                        
*                                                                               
S16HRX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CALCULATE PREMIUM MEAL HOURS                                     
*              R4--> ELEMENT                                                    
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CALCPML  NTR1  BASE=*,LABEL=*                                                   
         XC    MEALPH10,MEALPH10                                                
         XC    MEALPH20,MEALPH20                                                
         XC    MEALNPH,MEALNPH                                                  
*                                                                               
         TM    TATMSTA2,TATMS2NM   IS MEAL 1 NON-DEDUCTIBLE?                    
         BNZ   CPML30              IF SO, MEAL COUNTS AS WORK TIME              
         MVC   MEALSTRT,TATMM1ST   START OF MEAL 1                              
         MVC   MEALEND,TATMM1NT    END OF MEAL 1                                
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   CPML30              IF NOT, IGNORE MEAL                          
*                                                                               
         MVC   MEALSTRT,TATMM1ST   RESET START OF MEAL                          
*                                                                               
         MVC   TEMPHRS,DIFFTHRS    STORE DIFFTHRS                               
         BRAS  RE,NONPRMML         CALCULATE NON-PREM MEALS                     
         MVC   DIFFTHRS,TEMPHRS    RESET DIFFTHRS                               
         BRAS  RE,PRMMEAL          CALCULATE PREMIUM MEAL HOURS                 
         MVC   MEALPH10,TEMP10     10% PREMIUM HOURS                            
         MVC   MEALPH20,TEMP20     20% PREMIUM HOURS                            
*                                                                               
CPML30   MVC   MEALSTRT,TATMM2ST   START OF MEAL 2                              
         MVC   MEALEND,TATMM2NT    END OF MEAL 2                                
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   CPML50              IF NOT, IGNORE MEAL                          
*                                                                               
         MVC   MEALSTRT,TATMM2ST   RESET START OF MEAL                          
*                                                                               
         OC    MEALPH10,MEALPH10   IF WE ALREADY HAVE A MEAL DURING             
         BNZ   CPML40              PREMIUM HOURS, DON'T NEED TO                 
         OC    MEALPH20,MEALPH20   LOOK FOR NON-PREM MEALS                      
         BNZ   CPML40                                                           
         MVC   TEMPHRS,DIFFTHRS    STORE DIFFTHRS                               
         BRAS  RE,NONPRMML         CALCULATE NON-PREM MEALS                     
         MVC   DIFFTHRS,TEMPHRS    RESET DIFFTHRS                               
CPML40   BRAS  RE,PRMMEAL          CALCULATE PREMIUM MEAL HOURS                 
*                                                                               
         MVC   HRMIN1,MEALPH10     ADD 10% PREM MEAL1 HOURS                     
         MVC   HRMIN2,TEMP10       + 10% PREM MEAL2 HOURS                       
         BRAS  RE,ADDHRS                                                        
         MVC   MEALPH10,DIFFTHRS   TOTAL 10% PREMIUM MEAL HRS                   
*                                                                               
         MVC   HRMIN1,MEALPH20     ADD 20% PREM MEAL1 HOURS                     
         MVC   HRMIN2,TEMP20       + 20% PREM MEAL2 HOURS                       
         BRAS  RE,ADDHRS                                                        
         MVC   MEALPH20,DIFFTHRS   TOTAL 20% PREMIUM MEAL HRS                   
*                                                                               
CPML50   MVC   MEALSTRT,TATMM3ST   START OF MEAL 3                              
         MVC   MEALEND,TATMM3NT    END OF MEAL 3                                
         BRAS  RE,CHKMLWK          CHECK IF MEAL IS DURING WORK TIME            
         BNE   CPML100             IF NOT, IGNORE MEAL                          
*                                                                               
         MVC   MEALSTRT,TATMM3ST   RESET START OF MEAL                          
*                                                                               
         OC    MEALPH10,MEALPH10   IF WE ALREADY HAVE A MEAL DURING             
         BNZ   CPML60              PREMIUM HOURS, DON'T NEED TO                 
         OC    MEALPH20,MEALPH20   LOOK FOR NON-PREM MEALS                      
         BNZ   CPML60                                                           
         MVC   TEMPHRS,DIFFTHRS    STORE DIFFTHRS                               
         BRAS  RE,NONPRMML         CALCULATE NON-PREM MEALS                     
         MVC   DIFFTHRS,TEMPHRS    RESET DIFFTHRS                               
CPML60   BRAS  RE,PRMMEAL          CALCULATE PREMIUM MEAL HOURS                 
*                                                                               
         MVC   HRMIN1,MEALPH10     ADD 10% PREM MEAL1 + MEAL2 HOURS             
         MVC   HRMIN2,TEMP10       + 10% PREM MEAL3 HOURS                       
         BRAS  RE,ADDHRS                                                        
         MVC   MEALPH10,DIFFTHRS   TOTAL 10% PREMIUM MEAL HRS                   
*                                                                               
         MVC   HRMIN1,MEALPH20     ADD 20% PREM MEAL1 + MEAL2 HOURS             
         MVC   HRMIN2,TEMP20       + 20% PREM MEAL3 HOURS                       
         BRAS  RE,ADDHRS                                                        
         MVC   MEALPH20,DIFFTHRS   TOTAL 20% PREMIUM MEAL HRS                   
*                                                                               
CPML100  BRAS  RE,ADJML10          ADJUST MEALPH10 - UP TO 12M ONLY             
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        CHECK FOR EXTRAS WORKING FROM FRIDAY TO SATURDAY                       
*        ADD 100 FOR ANY SATURDAY STRAIGHT TIME HOURS                           
*        ADD 50 FOR ANY SATURDAY OVERTIME HOURS                                 
*        PAY WILL MULTIPLY THIS NUMBER BY THE HOURLY RATE AND                   
*        ADD IT TO THE TOTAL PAYMENT AMOUNT AND SUBJ TO PNH                     
*        DOUBLETIME HOURS ARE ALREADY PAID DOUBLE                               
*        OUTPUT IS IN SVEXSAT                                                   
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CHKFRI   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVEND,TATMWTNT      USED IN GETSTML ROUTINE                      
*                                                                               
         TM    DAYTYPE,DAYFRI      FRIDAY?                                      
         BNO   CKFRIX                                                           
         OC    TATMTTDP,TATMTTDP   ANY TRAVEL TO TIME?                          
         BZ    CKFRI02                                                          
         CLC   TATMWTNT,TATMTTDP   IF WORK END > TRVL TO START,                 
         BNL   CKFRIX                                                           
         B     CKFRI03                                                          
CKFRI02  CLC   TATMWTNT,TATMWTST   IF WORK END > WORK START,                    
         BNL   CKFRIX              WORK IS ALL ON FRIDAY                        
CKFRI03  BRAS  RE,GETSTML          GET MEAL HRS ON SATURDAY                     
         BRAS  RE,GETFRML          GET FRIDAY MEALS                             
         ZICM  R5,TATMWTNT,2                                                    
         AHI   R5,2400             ADD 2400 TO END TIME                         
         STH   R5,DIFFTHRS                                                      
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALSAT    DEDUCT SAT MEAL FROM END TIME                
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN                                                      
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         LH    R5,DIFFTHRS         END TIME NOT INCLUDING SAT MEAL              
*                                                                               
         ZICM  R2,TATMWTST,2                                                    
         OC    TATMTTDP,TATMTTDP   IS THERE TRAVEL TO DEPART?                   
         BZ    CKFRI05                                                          
         ZICM  R2,TATMTTDP,2       TRAVEL TO DEPART = WORK START                
CKFRI05  OC    MEALHRS,MEALHRS     ANY MEALS?                                   
         BZ    CKFRI10                                                          
         MVC   HRMIN1,TATMWTST     ADD FRIDAY MEAL HRS TO WORK START            
         OC    TATMTTDP,TATMTTDP   IF TRAVEL TO DEPART,                         
         BZ    *+10                                                             
         MVC   HRMIN1,TATMTTDP     TRAVEL TO DEPART = WORK START                
         MVC   HRMIN2,MEALFRI                                                   
         BRAS  RE,ADDHRS                                                        
         LH    R2,DIFFTHRS         WORK START + FRI MEAL TIME                   
CKFRI10  AHI   R2,800              ADD 8 HOURS TO START TIME                    
         LR    R3,R2               SAVE END OF STRAIGHT TIME                    
         CHI   R2,2200             IF STRAIGHT TIME + OVERTIME ARE ON           
         BNH   CKFRIX              FRIDAY, EXIT                                 
         CHI   R2,2400             IS ALL STRAIGHT TIME IS ON FRIDAY?           
         BNH   CKFRI30                                                          
*                                                                               
         CR    R2,R5               DO 8 HRS STRAIGHT EXCEED END TIME?           
         BL    CKFRI20                                                          
         SHI   R5,2400             YES, ENDTIME - 2400 = # OF SAT HRS           
         STH   R5,DIFFTHRS         (ALL STRAIGHT TIME)                          
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT DIFFTHRS TO DECIMAL                  
         STH   R1,SVEXSAT                                                       
         B     CKFRIX                                                           
CKFRI20  SHI   R2,2400             NO, END OF STRAIGHT TIME - 2400              
         STH   R2,DIFFTHRS         = # OF STRAIGHT TIME SAT HRS                 
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT DIFFTHRS TO DECIMAL                  
         STH   R1,SVEXSAT                                                       
*                                                                               
CKFRI30  CHI   R3,2400             DOES OVERTIME START BEFORE MIDNIGHT?         
         BNL   CKFRI40                                                          
         STH   R3,DIFFTHRS                                                      
         BRAS  RE,HRS2MIN                                                       
         LHI   R0,1440             24 HOURS IN MINUTES                          
         SH    R0,DIFFTMIN         AMOUNT OF OVERTIME ON FRIDAY                 
         LHI   R1,120              2 HOURS IN MINUTES                           
         SR    R1,R0               AMOUNT OF OVERTIME ON SATURDAY               
*                                                                               
         LH    R2,DIFFTMIN         END OF STRAIGHT TIME (START OF OT)           
         AHI   R2,120              END OF 2 HRS OF OVERTIME                     
         STH   R2,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         CH    R5,DIFFTHRS         DO 2 HRS OVERTIME EXCEED END TIME?           
         BNH   CKFRI50                                                          
*                                                                               
         SRL   R1,1                DIVIDE OVERTIME MINUTES BY 2                 
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         STH   R1,SVEXSAT                                                       
         B     CKFRIX                                                           
*                                                                               
CKFRI40  LR    R2,R3               END OF STRAIGHT TIME (START OF OT)           
         AHI   R2,200              END OF OVERTIME                              
         CR    R2,R5               DO 2 HRS OVERTIME EXCEED END TIME?           
         BH    CKFRI50                                                          
         LH    RE,SVEXSAT          NO, ADD 100 FOR 2 HRS OF OVERTIME            
         AHI   RE,100              200 * .5 = 100                               
         STH   RE,SVEXSAT                                                       
         B     CKFRIX                                                           
*                                                                               
CKFRI50  STH   R5,DIFFTHRS         YES, OT SAT HRS = ENDTIME - OT START         
         BRAS  RE,HRS2MIN          CONVERT ENDTIME TO MINUTES                   
         LH    R5,DIFFTMIN                                                      
         STH   R3,DIFFTHRS                                                      
         BRAS  RE,HRS2MIN          CONVERT OT START TO MINUTES                  
         SH    R5,DIFFTMIN         OT SAT HRS = ENDTIME - OT START              
         CHI   R3,2400             IF OVERTIME HOURS START ON SATURDAY          
         BNH   CKFRI55                                                          
         STH   R5,DIFFTMIN                                                      
         BRAS  RE,RNDHR3           ROUND UP TO THE NEAREST HOUR                 
         LH    R5,DIFFTMIN                                                      
CKFRI55  SRL   R5,1                DIVIDE BY 2                                  
         CHI   R5,60               MORE THAN 1 OT HOUR?                         
         BNH   *+6                                                              
         DC    H'00'                                                            
         STH   R5,DIFFTHRS                                                      
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         AH    R1,SVEXSAT                                                       
         STH   R1,SVEXSAT                                                       
*                                                                               
CKFRIX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        CHECK FOR EXTRAS WORKING FROM FRIDAY TO SATURDAY            *          
*        SAVE # SATURDAY OVERTIME HOURS AND                          *          
*        SAVE # SATURDAY DOUBLETIME HOURS                            *          
*        OUTPUT IS IN EXSATOT AND EXSATDT BY QUARTER HOUR            *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
CHKFRI2  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVEND,TATMWTNT      SAVE WORK END                                
         TM    TATMSTAT,TATMS16Y   IF USING 16HR RULE,                          
         BNO   CKFR201             GET ENDTIME                                  
         ZICM  R0,TATMTTDP,2       ADD 16 HOURS TO TRAVEL TO DEPART             
         OC    TATMTTDP,TATMTTDP                                                
         BNZ   CKFR20A                                                          
         ZICM  R0,TATMWTST,2       OR WORK START                                
CKFR20A  AHI   R0,1600                                                          
         CHI   R0,2400                                                          
         BNH   *+8                                                              
         SHI   R0,2400            SUBTRACT 2400 IF TOO BIG                      
         STCM  R0,3,SVEND                                                       
CKFR201  OC    TATMTTDP,TATMTTDP                                                
         BZ    CKFR202                                                          
         CLC   SVEND,TATMTTDP      IF WORK END > TRVL TO START,                 
         BNL   CKFR2NO             WORK IS ALL ON FRIDAY                        
         B     CKFR203                                                          
CKFR202  CLC   SVEND,TATMWTST      IF WORK END > WORK START,                    
         BNL   CKFR2NO             WORK IS ALL ON FRIDAY                        
*                                                                               
CKFR203  CLC   SVEND,TATMWTNT      IF END OF 16 HOURS IS AFTER WORK END         
         BL    *+10                                                             
         MVC   SVEND,TATMWTNT      USE WORK END AS ENDTIME                      
*                                                                               
         BRAS  RE,GETSTML          GET MEAL HRS ON SATURDAY                     
         BRAS  RE,GETFRML          GET FRIDAY MEALS                             
         ZICM  R5,SVEND,2                                                       
         AHI   R5,2400             ADD 2400 TO END TIME                         
         STH   R5,DIFFTHRS                                                      
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN                                                      
         MVC   DIFFTHRS,MEALSAT    DEDUCT SAT MEAL FROM END TIME                
         BRAS  RE,HRS2MIN                                                       
         SH    R1,DIFFTMIN                                                      
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         LH    R5,DIFFTHRS         END TIME NOT INCLUDING SAT MEAL              
*                                                                               
         ZICM  R2,TATMWTST,2       WORK START                                   
         OC    TATMTTDP,TATMTTDP   IS THERE TRAVEL TO DEPART?                   
         BZ    CKFR205                                                          
         ZICM  R2,TATMTTDP,2       TRAVEL TO DEPART = WORK START                
CKFR205  OC    MEALHRS,MEALHRS     ANY MEALS?                                   
         BZ    CKFR210                                                          
         MVC   HRMIN1,TATMWTST     ADD FRIDAY MEAL HRS TO WORK START            
         OC    TATMTTDP,TATMTTDP   IF THERE IS TRAVEL TO DEPART                 
         BZ    *+10                                                             
         MVC   HRMIN1,TATMTTDP     TRAVEL TO DEPART = WORK START                
         MVC   HRMIN2,MEALFRI                                                   
         BRAS  RE,ADDHRS                                                        
         LH    R2,DIFFTHRS         WORK START + FRI MEAL TIME                   
CKFR210  AHI   R2,800              ADD 8 HOURS TO START TIME                    
         LR    R3,R2               SAVE END OF STRAIGHT TIME                    
         CHI   R2,2200             IF ALL ST + OT ARE ON FRIDAY,                
         BH    CKFR220                                                          
         ZIC   R0,SVOTIME          SAVE ALL OT IN EXFRIOT,                      
         MHI   R0,100                                                           
         STH   R0,EXFRIOT                                                       
         B     CKFR260             AND FIND DOUBLETIME ON SATURDAY              
CKFR220  CHI   R2,2400             IS ALL STRAIGHT TIME ON FRIDAY?              
         BNH   CKFR230                                                          
         CR    R2,R5               DO 8 HRS STRAIGHT EXCEED END TIME?           
         BNL   CKFR2YES            YES - NO OT OR DT, EXIT                      
*                                                                               
CKFR230  CHI   R3,2400             DOES OVERTIME START BEFORE MIDNIGHT?         
         BNL   CKFR240                                                          
         STH   R3,DIFFTHRS                                                      
         BRAS  RE,HRS2MIN                                                       
         LHI   R0,1440             24 HOURS IN MINUTES                          
         SH    R0,DIFFTMIN         AMOUNT OF OVERTIME ON FRIDAY                 
         BAS   RE,SVFRIOT          SAVE FRIDAY OVERTIME HOURS                   
         LHI   R1,120              2 HOURS IN MINUTES                           
         SR    R1,R0               AMOUNT OF OVERTIME ON SATURDAY               
*                                                                               
         LH    R2,DIFFTMIN         END OF STRAIGHT TIME (START OF OT)           
         AHI   R2,120              END OF 2 HRS OF OVERTIME                     
         STH   R2,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         CH    R5,DIFFTHRS         DO 2 HRS OVERTIME EXCEED END TIME?           
         BNH   CKFR250                                                          
*                                                                               
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         STH   R1,EXSATOT          SAVE EXTRA SATURDAY OVERTIME HOURS           
         B     CKFR260                                                          
*                                  OVERTIME STARTS AFTER MIDNIGHT               
CKFR240  LR    R2,R3               END OF STRAIGHT TIME (START OF OT)           
         AHI   R2,200              END OF OVERTIME                              
         CR    R2,R5               DO 2 HRS OVERTIME EXCEED END TIME?           
         BH    CKFR250                                                          
         LH    RE,EXSATOT          NO, ADD 200 FOR 2 HRS OF OVERTIME            
         AHI   RE,200                                                           
         STH   RE,EXSATOT                                                       
         B     CKFR260             DOUBLETIME                                   
*                                                                               
CKFR250  STH   R5,DIFFTHRS         YES, OT SAT HRS = ENDTIME - OT START         
         BRAS  RE,HRS2MIN          CONVERT ENDTIME TO MINUTES                   
         LH    R5,DIFFTMIN                                                      
         STH   R3,DIFFTHRS                                                      
         BRAS  RE,HRS2MIN          CONVERT OT START TO MINUTES                  
         SH    R5,DIFFTMIN         OT SAT HRS = ENDTIME - OT START              
         CHI   R3,2400             IF OVERTIME HOURS START ON SATURDAY          
         BNH   CKFR255                                                          
         STH   R5,DIFFTMIN                                                      
         BRAS  RE,RNDHR3           ROUND UP TO THE NEAREST HOUR                 
         LH    R5,DIFFTMIN                                                      
CKFR255  CHI   R5,120              MORE THAN 2 OT HOURS?                        
         BNH   *+6                                                              
         DC    H'00'                                                            
         STH   R5,DIFFTHRS                                                      
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         STH   R1,EXSATOT          SATURDAY OVERTIME HOURS                      
         B     CKFR2YES                                                         
*                                                                               
*        DOUBLETIME                                                             
CKFR260  AHI   R3,200              R3 = START OF DOUBLETIME                     
         CHI   R3,2400             DOES DT START BEFORE MIDNIGHT?               
         BNL   CKFR270             YES,                                         
         STH   R5,DIFFTHRS         ENDTIME - MIDNIGHT = SAT DT HRS              
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN         ENDTIME IN MINUTES                           
         LHI   R0,1440             24 HOURS IN MINUTES                          
         SR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         STH   R1,EXSATDT          SAVE EXTRA SATURDAY DBLTIME HOURS            
*                                                                               
         STH   R3,DIFFTHRS         GET FRI DT HOURS                             
         BRAS  RE,HRS2MIN          GET START OF DT IN MINUTES                   
         LHI   R0,1440             24 HOURS IN MINUTES                          
         SH    R0,DIFFTMIN         MIDNIGHT - START OF DT = FRI DT HRS          
         STH   R0,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         STH   R1,EXFRIDT          SAVE EXTRA FRIDAY DBLTIME HOURS              
         B     CKFR2YES                                                         
*                                  DT STARTS AFTER MIDNIGHT                     
CKFR270  STH   R5,DIFFTHRS         ENDTIME - DT START = SAT DT HRS              
         BRAS  RE,HRS2MIN                                                       
         LH    R1,DIFFTMIN         ENDTIME IN MINUTES                           
         STH   R3,DIFFTHRS         START OF DOUBLETIME                          
         BRAS  RE,HRS2MIN          START OF DOUBLETIME IN MINUTES               
         LH    R0,DIFFTMIN                                                      
         SR    R1,R0                                                            
         STH   R1,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         STH   R1,EXSATDT          SAVE EXTRA SATURDAY DBLTIME HOURS            
*                                                                               
CKFR2YES XR    RC,RC                                                            
CKFR2NO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        SAVE EXTRA FRIDAY OVERTIME HOURS                                       
*--------------------------------------------------------------------*          
SVFRIOT  NTR1                                                                   
         STH   R0,DIFFTMIN                                                      
         BRAS  RE,MIN2HRS                                                       
         BRAS  RE,RNDQRTR          ROUND BY QUARTER HOUR                        
         BRAS  RE,HRS2DEC          CONVERT TO DECIMAL                           
         STH   R1,EXFRIOT          SAVE EXTRA FRIDAY OVERTIME HOURS             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE REHEARSAL DAY FIELD                                     
*              MUST BE PRINCIPAL, ON CAMERA, ON A WEEKDAY                       
*              R4 --> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
VALRHSL  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMRHSLH         REHEARSAL?                                   
         CLI   5(R2),0             ANY INPUT? DEFAULT IS NO                     
         BE    VRHSLX                                                           
         CLI   8(R2),C'N'                                                       
         BE    VRHSLX                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
**NO-OP  TM    DAYTYPE,DAYSAT+DAYSUN+DAYHLDY    MUST BE WEEKDAY                 
**11/09  BNZ   FLDINV                                                           
         MVI   WBERRFLD,D#TMREH                                                 
         TM    TGCSORT,X'08'       MUST BE ON CAMERA                            
         BO    FLDINV                                                           
*                                                                               
         LA    R2,STMNCWDH         NON-CONSECUTIVE WORKDAY?                     
         TM    TATMSTA2,TATMS2NC   CANNOT BE A NON-CONSECUTIVE DAY              
         BO    FLDINV                                                           
*                                                                               
         OI    TATMSTA2,TATMS2RD  TURN ON REHEARSAL STATUS                      
VRHSLX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE NON CONSECUTIVE WORKDAY FIELD                           
*              MUST BE AN ON CAMERA PRINCIPAL                                   
*              R4 --> ELEM                                                      
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
VALNCON  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMNCWDH         NON-CONSECUTIVE WORKDAY?                     
         CLI   5(R2),0             ANY INPUT? DEFAULT IS NO                     
         BE    VNCONX                                                           
         CLI   8(R2),C'N'                                                       
         BE    VNCONX                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         MVI   WBERRFLD,D#TMNCD                                                 
         TM    TGCSORT,X'08'       MUST BE ON CAMERA                            
         BO    FLDINV                                                           
         TM    TATMSTA2,TATMS2WC   IF WEATHER CANCELLATION,                     
         BO    FLDINV              CANNOT HAVE NON-CONSECUTIVE DAY              
         OI    TATMSTA2,TATMS2NC   TURN ON NON-CONSECUTIVE STATUS               
         MVI   NCONFLAG,C'Y'       NON-CONSECUTIVE FLAG                         
         MVI   SVOTIME,0           NO OT, DT, OR TRAVEL ALLOWED                 
         MVI   SVDTIME,0                                                        
         XC    TRVLHRS,TRVLHRS                                                  
         XC    SVTRVL,SVTRVL                                                    
         CLC   WORKHRS,=H'0'       IF NO WORK HOURS,                            
         BH    VNCONX                                                           
         MVI   SVDAYS,1            STILL COUNTS AS 1 DAY                        
VNCONX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VALIDATE WEATHER CANCELLATION                         *          
*              IF PRINCIPAL - PAY 1/2 PAYCHECK FOR 4 HOURS OR LESS   *          
*              IF EXTRA - PAY 1/2 PAYCHECK FOR 4 HOURS OR LESS,      *          
*              AND PAY 3/4 PAYCHECK FOR BETWEEN 4 AND 6 HRS          *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
WTHRCXL  NTR1  BASE=*,LABEL=*                                                   
         MVI   SVWTRST,0                                                        
         LA    R2,STMWTCNH         WEATHER CANCELLATION                         
         CLI   5(R2),0                                                          
         BE    WCXLX                                                            
         CLI   8(R2),C'N'                                                       
         BE    WCXLX                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         MVI   WBERRFLD,D#TMWCX                                                 
         TM    TGCSORT,X'20'       IF CAST MEMBER IS A PRINCIPAL                
         BO    WCXL10                                                           
         TM    TGCSORT,X'08'       MUST BE ON CAMERA                            
         BO    FLDINV                                                           
WCXL10   OI    TATMSTA2,TATMS2WC   TURN ON WEATHER CANCELLATION STATUS          
*                                                                               
         CLC   WORKHRS,=H'400'     WORKING 4 HRS OR LESS                        
         BH    WCXL20                                                           
         OI    SVWTRST,SVWTRHF     SET 1/2 PAYCHECK STATUS                      
         CLC   WORKHRS,=H'0'       IF NO WORK HOURS,                            
         BH    WCXL15                                                           
*&&DO                                                                           
         OC    SVTRVL,SVTRVL       AND TRAVEL HOURS,                            
         BZ    WCXL12                                                           
         MVI   SVWTRST,0           PAY AS TRAVEL DAY                            
         B     WCXLX                                                            
*                                                                               
WCXL12   MVI   SVDAYS,1            STILL COUNTS AS 1 DAY                        
**NO-OP  XC    SVTRVL,SVTRVL       SHOULD NOT COUNT AS TRAVEL DAY               
*&&                                                                             
         MVI   SVDAYS,1            STILL COUNTS AS 1 DAY                        
         XC    SVTRVL,SVTRVL       SHOULD NOT COUNT AS TRAVEL DAY               
WCXL15   MVC   HRMIN1,WORKHRS                                                   
         MVC   HRMIN2,WCXLTRVL                                                  
         BRAS  RE,ADDHRS                                                        
         LH    R3,DIFFTHRS                                                      
         CHI   R3,400              DON'T PAY TRVL IF TRVL+WORK <=4 HRS          
         BNH   WCXLX                                                            
         AHI   R3,-400             DEDUCT TRVL + WORK < 4 HRS                   
         BNP   WCXLX                                                            
         STCM  R3,3,DIFFTHRS                                                    
         BRAS  RE,RNDQRTR          ROUND TRVL TO THE NEAREST QUARTER            
         MVC   SVTRVL,DIFFTHRS                                                  
         B     WCXLX                                                            
WCXL20   TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA,                  
         BNO   WCXLX                                                            
         CLC   WORKHRS,=H'600'     EXTRAS WORKING 6 HRS OR LESS                 
         BH    WCXLX               ARE PAID 3/4 PAYCHECK                        
         OI    SVWTRST,SVWTR3Q     SET 3/4 PAYCHECK STATUS                      
         MVC   HRMIN1,WORKHRS                                                   
         MVC   HRMIN2,WCXLTRVL                                                  
         BRAS  RE,ADDHRS                                                        
         LH    R3,DIFFTHRS                                                      
         CHI   R3,600              DON'T PAY TRVL IF TRVL+WORK <=6 HRS          
         BNH   WCXLX                                                            
         AHI   R3,-600             DEDUCT TRVL + WORK < 6 HRS                   
         BNP   WCXLX                                                            
         STCM  R3,3,DIFFTHRS                                                    
         BRAS  RE,RNDQRTR          ROUND TRVL TO THE NEAREST QUARTER            
         MVC   SVTRVL,DIFFTHRS                                                  
         B     WCXLX                                                            
*                                                                               
WCXLX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*--------------------------------------------------------------------*          
*              SEND MESSAGE FOR WEATHER CANCELLATION - YES OR NO     *          
*              IF PRINCIPAL - MUST WORK 4 HRS OR LESS                *          
*              IF EXTRA - MUST WORK 6 HRS OR LESS INCLUDING TRVL TO  *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
WTHRCXL2 NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMWTCNH         WEATHER CANCELLATION                         
         CLC   WORKHRS,=H'600'                                                  
         BH    WCXL05                                                           
         TM    TGCSORT,X'20'       IF CAST MEMBER IS AN EXTRA,                  
         BO    WCXL10              MUST WORK 6 HRS OR LESS                      
         CLC   WORKHRS,=H'400'     PRINCIPALS MUST WORK 4 HRS OR LESS           
         BNH   WCXL10                                                           
WCXL05   CLI   5(R2),0                                                          
         BE    WCXLX                                                            
         CLI   8(R2),C'N'                                                       
         BNE   FLDINV                                                           
         B     WCXLX                                                            
WCXL10   CLI   8(R2),C'Y'                                                       
         BE    WCXL30                                                           
         CLI   8(R2),C'N'          IF NO, DON'T BOTHER CALCULATING              
         BNE   WCXL20                                                           
         OI    TATMSTA3,TATMWTCN   NO WEATHER CANCELLATION                      
         B     WCXLX                                                            
WCXL20   CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         TM    TGFASTAT,TGFROMFA   SEND MESSAGE TO ENTER Y OR N                 
         BZ    YNWTCXL                                                          
         GOTOR ADDWERR,DMCB,=AL2(ERWCXLYN),0                                    
         B     WCXLX                                                            
*                                                                               
WCXL30   OI    TATMSTA3,TATMWTCY   SET STATUS FOR WEATHER CANCELLATION          
WCXLX    XIT1                                                                   
         DROP  R4                                                               
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
YNWTCXL  MVC   MYMSGNO,=Y(ERWCXLYN) ENTER Y/N FOR WEATHER CANCELLATION          
         OI    GENSTAT2,USGETTXT    NEW THEEND FOR TWO BYTE ERROR MSGS          
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         GOTO1 EXIT,DMCB,0          (WEB COVERED BY ADDWERR CALL ABOVE)         
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*--------------------------------------------------------------------*          
*              VALIDATE DISTANT LOCATION                             *          
*              MUST BE ON SATURDAY - WORK IS PAID AS STRAIGHT TIME   *          
*              EXTRAS ONLY                                           *          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
DISTLOC  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMDSLCH         DISTANT LOCATION                             
         CLI   5(R2),0                                                          
         BE    DLOCX                                                            
         CLI   8(R2),C'N'                                                       
         BE    DLOCX                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         MVI   WBERRFLD,D#TMTDL                                                 
         TM    DAYTYPE,DAYSAT      MUST BE A SATURDAY                           
         BNO   FLDINV                                                           
         TM    TGCSORT,X'20'       MUST BE AN EXTRA                             
         BNO   FLDINV                                                           
         TM    TATMSTA2,TATMS2WC   CANNOT HAVE WEATHER CANCELLATIOIN            
         BNO   DLOC10                                                           
         MVI   WBERRFLD,D#TMWCX                                                 
         LA    R2,STMWTCNH         WEATHER CANCELLATION                         
         B     FLDINV                                                           
*                                                                               
DLOC10   OI    TATMSTA2,TATMS2DL   TURN ON DISTANT LOCATION STATUS              
*                                                                               
DLOCX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUND UP DIFFTHRS TO NEAREST QUARTER HOUR                        
*--------------------------------------------------------------------*          
RNDQRTR  NTR1  BASE=*,LABEL=*                                                   
         SR    RE,RE                                                            
         LH    RF,DIFFTHRS                                                      
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LR    R1,RF                                                            
         MHI   R1,100              PUT HOURS BACK                               
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    RNDQRTRX                                                         
         LA    RF,15                                                            
         CHI   RE,15                                                            
         BNH   RNDQRTR5                                                         
         LA    RF,30                                                            
         CHI   RE,30                                                            
         BNH   RNDQRTR5                                                         
         LA    RF,45                                                            
         CHI   RE,45                                                            
         BNH   RNDQRTR5                                                         
         LA    RF,100              ROUND UP AN HOUR                             
RNDQRTR5 AR    R1,RF                                                            
         STH   R1,DIFFTHRS                                                      
RNDQRTRX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUND UP DIFFTHRS TO NEAREST HALF HOUR                           
*--------------------------------------------------------------------*          
RNDHALF  NTR1  BASE=*,LABEL=*                                                   
         SR    RE,RE                                                            
         LH    RF,DIFFTHRS                                                      
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LR    R1,RF                                                            
         MHI   R1,100              PUT HOURS BACK                               
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    RNDHALFX                                                         
         LA    RF,30                                                            
         CHI   RE,30                                                            
         BNH   RNDHALF5                                                         
         LA    RF,100              ROUND UP AN HOUR                             
RNDHALF5 AR    R1,RF                                                            
         STH   R1,DIFFTHRS                                                      
RNDHALFX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUND UP R3 TO NEAREST HOUR                                      
*              RETURN NUMBER OF HOURS IN SV16HR                                 
*--------------------------------------------------------------------*          
RNDHOUR  NTR1  BASE=*,LABEL=*                                                   
         XC    SV16RND,SV16RND                                                  
         SR    RE,RE                                                            
         LR    RF,R3                                                            
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    RNDHRX                                                           
         LHI   R1,60                                                            
         SR    R1,RE               DEDUCT REMAINDER FROM 60                     
         STH   R1,SV16RND          SAVE ROUNDED UP MINUTES                      
         AHI   RF,1                ROUND UP AN HOUR                             
RNDHRX   STC   RF,SV16HR           RETURN NUMBER OF HOURS OF PENALTY            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUND UP DIFFTHRS TO NEAREST HOUR                                
*              RETURN NUMBER OF HOURS IN DIFFTHRS                               
*--------------------------------------------------------------------*          
RNDHR2   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    RE,RE                                                            
         LH    RF,DIFFTHRS                                                      
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    RNDHR2X                                                          
         AHI   RF,1                ROUND UP AN HOUR                             
         MHI   RF,100                                                           
         STH   RF,DIFFTHRS         RETURN NUMBER OF HOURS OF PENALTY            
RNDHR2X  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUND UP DIFFTMIN TO NEAREST HOUR                                
*              RETURN NUMBER OF MINUTES IN DIFFTMIN                             
*--------------------------------------------------------------------*          
RNDHR3   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,MIN2HRS          CONVERT MINS TO HOURS                        
         SR    RE,RE                                                            
         LH    RF,DIFFTHRS                                                      
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    RNDHR3X                                                          
         AHI   RF,1                ROUND UP AN HOUR                             
         MHI   RF,100                                                           
         STH   RF,DIFFTHRS         RETURN NUMBER OF HOURS OF PENALTY            
         BRAS  RE,HRS2MIN                                                       
RNDHR3X  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUND UP DECIMAL IN HALF TO NEAREST QUARTER                      
*              RETURNS ROUNDED DECIMAL IN HALF                                  
*--------------------------------------------------------------------*          
RNDDEC   NTR1  BASE=*,LABEL=*                                                   
         SR    RE,RE                                                            
         LH    RF,HALF                                                          
         D     RE,=F'100'          SEPARATE HUNDREDS AND ONES                   
         LR    R1,RF                                                            
         MHI   R1,100              PUT HUNDREDS BACK                            
         LTR   RE,RE               NO ONES TO ROUND UP                          
         BZ    RNDDECX                                                          
         LA    RF,25                                                            
         CHI   RE,25                                                            
         BNH   RNDDEC5                                                          
         LA    RF,50                                                            
         CHI   RE,50                                                            
         BNH   RNDDEC5                                                          
         LA    RF,75                                                            
         CHI   RE,75                                                            
         BNH   RNDDEC5                                                          
         LA    RF,100              ROUND UP AN HOUR                             
RNDDEC5  AR    R1,RF                                                            
         STH   R1,HALF                                                          
RNDDECX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE DISPLAYS THE DAY OF THE WEEK                                   
*        R4 --> TATMEL                                                          
*--------------------------------------------------------------------*          
         USING TATMD,R4                                                         
DISPDAY  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(1,TATMDATE),(0,TMSEDATE) CONVERT TO EBCDIC          
         GOTO1 GETDAY,DMCB,TMSEDATE,WORK                                        
         LA    RE,DAYTAB                                                        
DDAY10   CLI   0(RE),X'FF'         SHOULD FIND A MATCH                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   0(1,R1),0(RE)                                                    
         BE    DDAY20                                                           
         LA    RE,L'DAYTAB(RE)                                                  
         B     DDAY10                                                           
DDAY20   MVC   STMDAY,1(RE)        DISPLAY DAY OF THE WEEK                      
         OI    STMDAYH+6,X'80'     TRANSMIT                                     
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
DAYTAB   DS    0CL10                                                            
         DC    XL1'1',CL9'Monday'                                               
         DC    XL1'2',CL9'Tuesday'                                              
         DC    XL1'3',CL9'Wednesday'                                            
         DC    XL1'4',CL9'Thursday'                                             
         DC    XL1'5',CL9'Friday'                                               
         DC    XL1'6',CL9'Saturday'                                             
         DC    XL1'7',CL9'Sunday'                                               
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE LOOKS UP TO IF TIMESHEET DATE IS HOLIDAY                       
*--------------------------------------------------------------------*          
LKHLDY   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,HLDYTAB                                                       
LHLDY10  CLI   0(R1),X'FF'                                                      
         BE    LHLDNO                                                           
         CLC   TEMPDATE,0(R1)                                                   
         BE    LHLDYES                                                          
         LA    R1,L'HLDYTAB(R1)                                                 
         B     LHLDY10                                                          
*                                                                               
HLDYTAB  DS    0XL3                                                             
         DC    XL3'A30101'      NEW YEARS DAY                                   
         DC    XL3'A30120'      MARTIN LUTHER KING DAY                          
         DC    XL3'A30217'      PRESIDENT'S DAY                                 
         DC    XL3'A30526'      MEMORIAL DAY                                    
         DC    XL3'A30704'      INDEPENDENCE DAY                                
         DC    XL3'A30901'      LABOR DAY                                       
         DC    XL3'A31127'      THANKSGIVING DAY                                
         DC    XL3'A31225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'A40101'      NEW YEARS DAY                                   
         DC    XL3'A40119'      MARTIN LUTHER KING DAY                          
         DC    XL3'A40216'      PRESIDENT'S DAY                                 
         DC    XL3'A40531'      MEMORIAL DAY                                    
         DC    XL3'A40704'      INDEPENDENCE DAY (SUNDAY)                       
         DC    XL3'A40705'      DAY AFTER INDEPENDENCE DAY                      
         DC    XL3'A40906'      LABOR DAY                                       
         DC    XL3'A41125'      THANKSGIVING DAY                                
         DC    XL3'A41225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'A50101'      NEW YEARS DAY                                   
         DC    XL3'A50117'      MARTIN LUTHER KING DAY                          
         DC    XL3'A50221'      PRESIDENT'S DAY                                 
         DC    XL3'A50530'      MEMORIAL DAY                                    
         DC    XL3'A50704'      INDEPENDENCE DAY                                
         DC    XL3'A50905'      LABOR DAY                                       
         DC    XL3'A51124'      THANKSGIVING DAY                                
         DC    XL3'A51225'      CHRISTMAS DAY (SUNDAY)                          
         DC    XL3'A51226'      DAY AFTER CHRISTMAS                             
*                                                                               
         DC    XL3'A60101'      NEW YEARS DAY (SUNDAY)                          
         DC    XL3'A60102'      DAY AFTER NEW YEARS DAY                         
         DC    XL3'A60116'      MARTIN LUTHER KING DAY                          
         DC    XL3'A60220'      PRESIDENT'S DAY                                 
         DC    XL3'A60529'      MEMORIAL DAY                                    
         DC    XL3'A60704'      INDEPENDENCE DAY                                
         DC    XL3'A60904'      LABOR DAY                                       
         DC    XL3'A61123'      THANKSGIVING DAY                                
         DC    XL3'A61225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'A70101'      NEW YEARS DAY                                   
         DC    XL3'A70115'      MARTIN LUTHER KING DAY                          
         DC    XL3'A70219'      PRESIDENT'S DAY                                 
         DC    XL3'A70528'      MEMORIAL DAY                                    
         DC    XL3'A70704'      INDEPENDENCE DAY                                
         DC    XL3'A70903'      LABOR DAY                                       
         DC    XL3'A71122'      THANKSGIVING DAY                                
         DC    XL3'A71225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'A80101'      NEW YEARS DAY                                   
         DC    XL3'A80121'      MARTIN LUTHER KING DAY                          
         DC    XL3'A80218'      PRESIDENT'S DAY                                 
         DC    XL3'A80526'      MEMORIAL DAY                                    
         DC    XL3'A80704'      INDEPENDENCE DAY                                
         DC    XL3'A80901'      LABOR DAY                                       
         DC    XL3'A81127'      THANKSGIVING DAY                                
         DC    XL3'A81225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'A90101'      NEW YEARS DAY                                   
         DC    XL3'A90119'      MARTIN LUTHER KING DAY                          
         DC    XL3'A90216'      PRESIDENT'S DAY                                 
         DC    XL3'A90525'      MEMORIAL DAY                                    
         DC    XL3'A90704'      INDEPENDENCE DAY                                
         DC    XL3'A90907'      LABOR DAY                                       
         DC    XL3'A91126'      THANKSGIVING DAY                                
         DC    XL3'A91225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B00101'      NEW YEARS DAY                                   
         DC    XL3'B00118'      MARTIN LUTHER KING DAY                          
         DC    XL3'B00215'      PRESIDENT'S DAY                                 
         DC    XL3'B00531'      MEMORIAL DAY                                    
         DC    XL3'B00704'      INDEPENDENCE DAY  (SUNDAY)                      
         DC    XL3'B00705'      DAY AFTER INDEPENDENCE DAY                      
         DC    XL3'B00906'      LABOR DAY                                       
         DC    XL3'B01125'      THANKSGIVING DAY                                
         DC    XL3'B01225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B10101'      NEW YEARS DAY                                   
         DC    XL3'B10117'      MARTIN LUTHER KING DAY                          
         DC    XL3'B10221'      PRESIDENT'S DAY                                 
         DC    XL3'B10530'      MEMORIAL DAY                                    
         DC    XL3'B10704'      INDEPENDENCE DAY                                
         DC    XL3'B10905'      LABOR DAY                                       
         DC    XL3'B11124'      THANKSGIVING DAY                                
         DC    XL3'B11225'      CHRISTMAS DAY (SUNDAY)                          
         DC    XL3'B11226'      DAY AFTER CHRISTMAS DAY                         
*                                                                               
         DC    XL3'B20101'      NEW YEARS DAY (SUNDAY)                          
         DC    XL3'B20102'      DAY AFTER NEW YEARS DAY                         
         DC    XL3'B20116'      MARTIN LUTHER KING DAY                          
         DC    XL3'B20220'      PRESIDENT'S DAY                                 
         DC    XL3'B20528'      MEMORIAL DAY                                    
         DC    XL3'B20704'      INDEPENDENCE DAY                                
         DC    XL3'B20903'      LABOR DAY                                       
         DC    XL3'B21122'      THANKSGIVING DAY                                
         DC    XL3'B21225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B30101'      NEW YEARS DAY                                   
         DC    XL3'B30121'      MARTIN LUTHER KING DAY                          
         DC    XL3'B30218'      PRESIDENT'S DAY                                 
         DC    XL3'B30527'      MEMORIAL DAY                                    
         DC    XL3'B30704'      INDEPENDENCE DAY                                
         DC    XL3'B30902'      LABOR DAY                                       
         DC    XL3'B31128'      THANKSGIVING DAY                                
         DC    XL3'B31225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B40101'      NEW YEARS DAY                                   
         DC    XL3'B40120'      MARTIN LUTHER KING DAY                          
         DC    XL3'B40217'      PRESIDENT'S DAY                                 
         DC    XL3'B40526'      MEMORIAL DAY                                    
         DC    XL3'B40704'      INDEPENDENCE DAY                                
         DC    XL3'B40901'      LABOR DAY                                       
         DC    XL3'B41127'      THANKSGIVING DAY                                
         DC    XL3'B41225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B50101'      NEW YEARS DAY            2015                   
         DC    XL3'B50119'      MARTIN LUTHER KING DAY                          
         DC    XL3'B50216'      PRESIDENT'S DAY                                 
         DC    XL3'B50525'      MEMORIAL DAY                                    
         DC    XL3'B50704'      INDEPENDENCE DAY                                
         DC    XL3'B50907'      LABOR DAY                                       
         DC    XL3'B51126'      THANKSGIVING DAY                                
         DC    XL3'B51225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B60101'      NEW YEARS DAY            2016                   
         DC    XL3'B60118'      MARTIN LUTHER KING DAY                          
         DC    XL3'B60215'      PRESIDENT'S DAY                                 
         DC    XL3'B60530'      MEMORIAL DAY                                    
         DC    XL3'B60704'      INDEPENDENCE DAY                                
         DC    XL3'B60905'      LABOR DAY                                       
         DC    XL3'B61124'      THANKSGIVING DAY                                
         DC    XL3'B61225'      CHRISTMAS DAY (SUNDAY)                          
         DC    XL3'B61226'      DAY AFTER CHRISTMAS DAY                         
*                                                                               
         DC    XL3'B70101'      NEW YEARS DAY (SUNDAY)   2017                   
         DC    XL3'B70102'      DAY AFTER NEW YEARS DAY                         
         DC    XL3'B70116'      MARTIN LUTHER KING DAY                          
         DC    XL3'B70220'      PRESIDENT'S DAY                                 
         DC    XL3'B70529'      MEMORIAL DAY                                    
         DC    XL3'B70704'      INDEPENDENCE DAY                                
         DC    XL3'B70904'      LABOR DAY                                       
         DC    XL3'B71110'      VETERAN'S DAY                                   
         DC    XL3'B71123'      THANKSGIVING DAY                                
         DC    XL3'B71225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B80101'      NEW YEARS DAY            2018                   
         DC    XL3'B80115'      MARTIN LUTHER KING DAY                          
         DC    XL3'B80219'      PRESIDENT'S DAY                                 
         DC    XL3'B80528'      MEMORIAL DAY                                    
         DC    XL3'B80704'      INDEPENDENCE DAY                                
         DC    XL3'B80903'      LABOR DAY                                       
         DC    XL3'B81112'      VETERAN'S DAY                                   
         DC    XL3'B81122'      THANKSGIVING DAY                                
         DC    XL3'B81225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'B90101'      NEW YEARS DAY            2019                   
         DC    XL3'B90121'      MARTIN LUTHER KING DAY                          
         DC    XL3'B90218'      PRESIDENT'S DAY                                 
         DC    XL3'B90527'      MEMORIAL DAY                                    
         DC    XL3'B90704'      INDEPENDENCE DAY                                
         DC    XL3'B90902'      LABOR DAY                                       
         DC    XL3'B91111'      VETERAN'S DAY                                   
         DC    XL3'B91128'      THANKSGIVING DAY                                
         DC    XL3'B91225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'C00101'      NEW YEARS DAY            2020                   
         DC    XL3'C00120'      MARTIN LUTHER KING DAY                          
         DC    XL3'C00217'      PRESIDENT'S DAY                                 
         DC    XL3'C00525'      MEMORIAL DAY                                    
         DC    XL3'C00704'      INDEPENDENCE DAY                                
         DC    XL3'C00907'      LABOR DAY                                       
         DC    XL3'C01111'      VETERAN'S DAY                                   
         DC    XL3'C01126'      THANKSGIVING DAY                                
         DC    XL3'C01225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'C10101'      NEW YEARS DAY            2021                   
         DC    XL3'C10118'      MARTIN LUTHER KING DAY                          
         DC    XL3'C10215'      PRESIDENT'S DAY                                 
         DC    XL3'C10531'      MEMORIAL DAY                                    
         DC    XL3'C10704'      INDEPENDENCE DAY (SUNDAY)                       
         DC    XL3'C10705'      DAY AFTER INDEPENDENCE DAY                      
         DC    XL3'C10906'      LABOR DAY                                       
         DC    XL3'C11111'      VETERAN'S DAY                                   
         DC    XL3'C11125'      THANKSGIVING DAY                                
         DC    XL3'C11225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'C20101'      NEW YEARS DAY            2022                   
         DC    XL3'C20117'      MARTIN LUTHER KING DAY                          
         DC    XL3'C20221'      PRESIDENT'S DAY                                 
         DC    XL3'C20530'      MEMORIAL DAY                                    
         DC    XL3'C20704'      INDEPENDENCE DAY                                
         DC    XL3'C20905'      LABOR DAY                                       
         DC    XL3'C21111'      VETERAN'S DAY                                   
         DC    XL3'C21124'      THANKSGIVING DAY                                
         DC    XL3'C21225'      CHRISTMAS DAY (SUNDAY)                          
         DC    XL3'C21226'      DAY AFTER CHRISTMAS DAY                         
*                                                                               
         DC    XL3'C30101'      NEW YEARS DAY (SUNDAY)   2023                   
         DC    XL3'C30102'      DAY AFTER NEW YEARS DAY                         
         DC    XL3'C30116'      MARTIN LUTHER KING DAY                          
         DC    XL3'C30220'      PRESIDENT'S DAY                                 
         DC    XL3'C30529'      MEMORIAL DAY                                    
         DC    XL3'C30704'      INDEPENDENCE DAY                                
         DC    XL3'C30904'      LABOR DAY                                       
         DC    XL3'C31110'      VETERAN'S DAY                                   
         DC    XL3'C31123'      THANKSGIVING DAY                                
         DC    XL3'C31225'      CHRISTMAS DAY                                   
*                                                                               
         DC    XL3'C40101'      NEW YEARS DAY            2024                   
         DC    XL3'C40115'      MARTIN LUTHER KING DAY                          
         DC    XL3'C40219'      PRESIDENT'S DAY                                 
         DC    XL3'C40527'      MEMORIAL DAY                                    
         DC    XL3'C40704'      INDEPENDENCE DAY                                
         DC    XL3'C40902'      LABOR DAY                                       
         DC    XL3'C41111'      VETERAN'S DAY                                   
         DC    XL3'C41128'      THANKSGIVING DAY                                
         DC    XL3'C41225'      CHRISTMAS DAY                                   
*                                                                               
         DC    X'FF'                                                            
LHLDYES  XR    RC,RC                                                            
LHLDNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE DISPLAYS REIMBURSEMENTS                                        
*        R4 ---> TATM ELEMENT IN AIO                                            
*--------------------------------------------------------------------*          
                                                                                
         USING TATMD,R4                                                         
DISPREIM NTR1  BASE=*,LABEL=*                                                   
         XC    STMWANE,STMWANE                                                  
         XC    STMWAEV,STMWAEV                                                  
         XC    STMOTHR,STMOTHR                                                  
         OI    STMWANEH+6,X'80'                                                 
         OI    STMWAEVH+6,X'80'                                                 
         OI    STMOTHRH+6,X'80'                                                 
         OI    STMOTHRH+6,X'80'                                                 
                                                                                
         LA    R2,STMOTHRH         OTHER MISC. REIMBURSED EXPENSES              
         EDIT  TATMOTHR,(10,8(R2)),2,ALIGN=LEFT,ZERO=BLANK                      
                                                                                
         EDIT  TATMWANE,STMWANE,ALIGN=LEFT,ZERO=BLANK                           
         EDIT  TATMWAEV,STMWAEV,ALIGN=LEFT,ZERO=BLANK                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO DISPLAY A COMMENT                                     
*--------------------------------------------------------------------*          
                                                                                
DISPCOM  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACMELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DCOM10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TACMD,R4                                                         
         CLI   TACMTYPE,TACMTYPG                                                
         JNE   DCOM10                                                           
         CLC   TACMCOMM(L'TMSHDATE),TMSHDATE   MATCH WITH CORRECT DATE          
         JNE   DCOM10                                                           
         LA    R3,TACMCOMM+L'TMSHDATE                                           
         ZIC   R1,TACMLEN                                                       
         SHI   R1,7                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   STMCMMT(0),0(R3)                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD A COMMENT                                   *         
***********************************************************************         
                                                                                
BLDCOM   NTR1  BASE=*,LABEL=*                                                   
         USING TACMD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACMELQ      FIND OLD COMMENT ELEMENT                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BCOM20   BRAS  RE,NEXTEL                                                        
         JNE   BCOM30                                                           
         CLI   TACMTYPE,TACMTYPG                                                
         JNE   BCOM20                                                           
         CLC   TMSHDATE,TACMCOMM  MATCH UP WITH TIMESHEET DATE                  
         JNE   BCOM20                                                           
         MVI   TACMEL,X'FF'       MARK THIS COMMENT ELEM WITH X'FF'             
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM            REMOVE THIS COMMENT ELEMENT                   
         DROP  R4                                                               
*                                                                               
BCOM30   CLI   STMCMMTH+5,0        COMMENTS?                                    
         JE    XIT                                                              
         LA    R2,STMCMMTH                                                      
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         AHI   R1,6                + ELCODE + LENGTH + TYPE + DATE              
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TACMD,R4                                                         
         MVI   TACMEL,TACMELQ                                                   
         STC   R1,TACMLEN          LENGTH                                       
         MVI   TACMTYPE,TACMTYPG   GENERAL TYPE                                 
         LA    R3,TACMCOMM                                                      
         MVC   0(L'TMSHDATE,R3),TMSHDATE   TIMESHEET DATE                       
         LA    R3,L'TMSHDATE(R3)   COMMENT GOES AFTER DATE                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R3),8(R2)       COMMENT                                      
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DELETE THE RECORD                                            *         
***********************************************************************         
                                                                                
DELREC   NTR1  BASE=*,LABEL=*                                                   
         USING TLTMD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVC   TLTMCOM,TGCOM                                                    
         MVC   TLTMINV,TGINV                                                    
         MVC   TLTMSSN,TGSSN                                                    
         MVC   TLTMSORT,TGCSORT                                                 
         MVC   TLTMCAT,TGCAT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLTMKEY),KEYSAVE                                           
         JE    DELREC2                                                          
         MVI   ERROR,NOTFOUND                                                   
         J     THEEND                                                           
                                                                                
DELREC2  CLI   PFAID,20                                                         
         JNE   DELMSG                                                           
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES                                               *         
***********************************************************************         
                                                                                
DELMSG   MVC   MYMSGNO,=H'257'      PRESS PF20 TO CONFIRM DELETE OF             
         OI    GENSTAT2,USGETTXT    ALL DAYS FOR THIS PERF/CAT                  
         J     THEEND                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS ERROR TO WEB ERROR TABLE                        *         
*        ON ENTRY ... P1=A(ERROR NUMBER)                              *         
*        ON ENTRY ... P2=A(FIELD MAP CODE) (OPTIONAL)                 *         
*                        IF X'FF', WBERRFLD ALREADY SET               *         
***********************************************************************         
                                                                                
ADDWERR  NTR1  BASE=*,LABEL=*                                                   
         CLI   4(R1),X'FF'                                                      
         JE    AWE10                                                            
         MVC   WBERRFLD,7(R1)                                                   
                                                                                
AWE10    L     R1,0(R1)                                                         
                                                                                
         USING ERRENTD,RF                                                       
         LA    RF,WEERRS                                                        
AWE20    CLC   EENUMB,0(R1)                                                     
         JE    AWE30                                                            
         ZIC   RE,EELEN                                                         
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   AWE20                                                            
         DC    H'00'                                                            
                                                                                
AWE30    CLI   WBERRFLD,0                                                       
         JE    AWE40                                                            
         ZIC   RE,EELEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   BLOCK(0),ERRENTD                                                 
         DROP  RF                                                               
                                                                                
         USING ERRENTD,RF                                                       
         LA    RF,BLOCK                                                         
         MVC   EEFIELD,WBERRFLD                                                 
                                                                                
AWE40    GOTO1 ADDERROR,DMCB,(RF)                                               
         J     XIT                                                              
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        ERROR MESSAGES                                               *         
***********************************************************************         
                                                                                
WEERRS   DS    0X                                                               
                                                                                
WEINVSPF DC    AL1(WINVSPFX-*),AL2(ERINVSPF),AL1(ERRCATY2),AL1(D#TMSPT)         
         DC    C'No Ftrack record if spots=0'                                   
WINVSPFX EQU   *                                                                
                                                                                
WEOFFCAM DC    AL1(WOFFCAMX-*),AL2(EROFFCAM),AL1(ERRCATY1),AL1(D#TMPDD)         
         DC    C'Not allowed for off camera cast'                               
WOFFCAMX EQU   *                                                                
                                                                                
WENGHPYN DC    AL1(WNGHPYNX-*),AL2(ERNGHPYN),AL1(ERRCATY1),AL1(D#TMNPR)         
         DC    C'Do you want to add night premium?'                             
WNGHPYNX EQU   *                                                                
                                                                                
WEMPENYN DC    AL1(WMPENYNX-*),AL2(ERMPENYN),AL1(ERRCATY1),AL1(D#TMMP1)         
         DC    C'Do you want to add meal penalties?'                            
WMPENYNX EQU   *                                                                
                                                                                
WE16HRYN DC    AL1(W16HRYNX-*),AL2(ER16HRYN),AL1(ERRCATY1),AL1(D#TM16H)         
         DC    C'Do you want to use the 16 hour rule?'                          
W16HRYNX EQU   *                                                                
                                                                                
WEWCXLYN DC    AL1(WWCXLYNX-*),AL2(ERWCXLYN),AL1(ERRCATY1),AL1(D#TMWCX)         
         DC    C'Is this day a Weather Cancellation?'                           
WWCXLYNX EQU   *                                                                
                                                                                
WERPVLYN DC    AL1(WRPVLYNX-*),AL2(ERRPVLYN),AL1(ERRCATY1),AL1(D#TMRPV)         
         DC    C'Does a rest period violation exist?'                           
WRPVLYNX EQU   *                                                                
                                                                                
WEINVTTA DC    AL1(WINVTTAX-*),AL2(ERINVTTA),AL1(ERRCATY1),AL1(D#TMTTA)         
         DC    C'Arrival time must equal Work Start time'                       
WINVTTAX EQU   *                                                                
                                                                                
WEINVTID DC    AL1(WINVTIDX-*),AL2(ERINVTID),AL1(ERRCATY1),AL1(0)               
         DC    C'Departure time must equal Work End time'                       
WINVTIDX EQU   *                                                                
                                                                                
WEINVTFD DC    AL1(WINVTFDX-*),AL2(ERINVTFD),AL1(ERRCATY1),AL1(D#TMTFD)         
         DC    C'Departure time must equal Intervening Travel Arrival '         
         DC    C'time'                                                          
WINVTFDX EQU   *                                                                
                                                                                
WERTMINP DC    AL1(WRTMINPX-*),AL2(ERRTMINP),AL1(ERRCATY3),AL1(D#TMINV)         
         DC    C'Invoice has been paid. No changes allowed.'                    
WRTMINPX EQU   *                                                                
                                                                                
WEINVUSD DC    AL1(WINVUSDX-*),AL2(ERINVUSD),AL1(ERRCATY3),AL1(D#TMINV)         
         DC    C'Invoice has timesheet attached to different '                  
         DC    C'Commercial'                                                    
WINVUSDX EQU   *                                                                
                                                                                
WETMDATE DC    AL1(WTMDATEX-*),AL2(ERTMDATE),AL1(ERRCATY3),AL1(D#TMDAT)         
         DC    C'Timesheet Holiday Table needs update'                          
WTMDATEX EQU   *                                                                
                                                                                
WEINVUNI DC    AL1(WINVUNIX-*),AL2(ERINVUNI),AL1(ERRCATY3),AL1(D#TMSEQ)         
         DC    C'Cannot add timesheet for musicians'                            
WINVUNIX EQU   *                                                                
                                                                                
WENOTFND DC    AL1(WNOTFNDX-*),AL2(NOTFOUND),AL1(ERRCATY3),AL1(0)               
         DC    C'Record not on file'                                            
WNOTFNDX EQU   *                                                                
                                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CALL INITAL ROUTINE                                          *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         LA    RF,PFTAB                                                         
         CLI   RECNUM,TM                                                        
         JE    *+8                                                              
         LA    RF,DPFTAB                                                        
         GOTO1 INITIAL,DMCB,(RF)  INITIALIZE                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'TIME',CL8'LIST'                                       
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,(PF14X-PF14)/KEYLNQ,0)                            
         DC    CL3' ',CL8'TIME ',CL8'DISP'                                      
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGINV-1),AL2(TGINV-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,(PF15X-PF15)/KEYLNQ,0)                            
         DC    CL3' ',CL8'TIME ',CL8'DISP'                                      
PF15     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGINV-1),AL2(TGINV-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF15X    EQU   *                                                                
         DC    AL1(PF20X-*,20,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF20X    EQU   *                                                                
         DC    AL1(PF24X-*,24,PFTRPROG+PFTINT,0,0)                              
         DC    CL3' ',CL8' ',CL8' '                                             
PF24X    EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
                                                                                
DPFTAB   DS    0C                  PF KEYS TABLE                                
         DC    AL1(DPF13X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'DTIME',CL8'LIST'                                      
DPF13X   EQU   *                                                                
         DC    AL1(DPF14X-*,14,(DPF14X-DPF14)/KEYLNQ,0)                         
         DC    CL3' ',CL8'DTIME',CL8'DISP'                                      
DPF14    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGINV-1),AL2(TGINV-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
DPF14X   EQU   *                                                                
         DC    AL1(DPF15X-*,15,(DPF15X-DPF15)/KEYLNQ,0)                         
         DC    CL3' ',CL8'DTIME',CL8'DISP'                                      
DPF15    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGINV-1),AL2(TGINV-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
DPF15X   EQU   *                                                                
         DC    AL1(DPF20X-*,20,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
DPF20X   EQU   *                                                                
         DC    AL1(DPF24X-*,24,PFTRPROG+PFTINT,0,0)                             
         DC    CL3' ',CL8' ',CL8' '                                             
DPF24X   EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        IF RECORD NOT FOUND, USE PFKEY TO RETURN TO LIST             *         
***********************************************************************         
                                                                                
INIT2    NTR1  BASE=*,LABEL=*                                                   
         LA    RF,PFTAB2                                                        
         CLI   RECNUM,TM                                                        
         JE    *+8                                                              
         LA    RF,DPFTAB2                                                       
         GOTO1 INITIAL,DMCB,(RF)  INITIALIZE                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
PFTAB2   DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF213X-*,13,0,0,0)                                           
         DC    CL3' ',CL8'TIME',CL8'LIST'                                       
PF213X   EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
                                                                                
DPFTAB2  DS    0C                  PF KEYS TABLE                                
         DC    AL1(DPF213X-*,13,0,0,0)                                          
         DC    CL3' ',CL8'DTIME',CL8'LIST'                                      
DPF213X  EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DETERMINES IF ANY ERRORS HAVE BEEN ENCOUNTERED       *         
*        THAT SHOULD ABORT A WEB TRANSACTION IMMEDIATELY              *         
***********************************************************************         
                                                                                
WEBERRS  NTR1  BASE=*,LABEL=*                                                   
         USING ERRENTD,RE                                                       
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    NO                                                               
         L     RE,TGAERTAB                                                      
         CLI   0(RE),X'FF'                                                      
         JE    NO                                                               
         CLI   EECATY,ERRCATY3     AND A CATEGORY 3 WAS FOUND                   
         J     XIT                 TERMINATE THIS TRANSACTION                   
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        IF VALIDATING A HYPO TIMESHEET, PERFORM SOME SETUP           *         
***********************************************************************         
                                                                                
SETHYPO  NTR1  BASE=*,LABEL=*                                                   
         NI    PROSTAT,X'FF'-HYPOTIME                                           
                                                                                
         USING WEBREQD,R3                                                       
         TM    TGFASTAT,TGFROMFA   IF TIMESHEET IS COMING FROM                  
         JZ    NO                  WEB                                          
         L     R3,TGAFAREQ                                                      
         OC    WBTMCOM,WBTMCOM     AND IS FOR A HYPO COM/CAST/INV               
         JNZ   NO                                                               
         OI    PROSTAT,HYPOTIME    SET HYPO STATUS                              
                                                                                
         MVC   SVCOTYPE,WBTMHTY    SET COMMERCIAL TYPE                          
         MVC   SVACTRA,WBTMHAT     ACTRA TYPE                                   
         MVC   SVCOADST,WBTMHAT    AND ADDENDUM STATE                           
                                                                                
         MVI   SVTAINST,0          CLEAR INVOICE STATUS                         
                                                                                
         CLC   WBTMONO,=C'OFF'     SET ON/OFF CAMERA INDICATOR                  
         JNE   *+8                                                              
         OI    TGCSORT,TLCASRFQ                                                 
                                                                                
         TM    TGCATYPE,EXTRA      SET EXTRA INDICATOR                          
         JZ    *+8                                                              
         OI    TGCSORT,TLCASREQ                                                 
                                                                                
         OC    WBTMDOB,WBTMDOB     (POSSIBLY) SET DATE OF BIRTH                 
         JZ    YES                                                              
         LA    R2,WBTMDOB                                                       
         GOTO1 DTVAL,DMCB,(X'40',TGDATE)                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR SETMINOR,DMCB,TGDATE                                             
         J     YES                                                              
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS MINOR STATUS BASED ON PROVIDED DATE             *         
*        ON ENTRY ... P1 = A(DATE)                                    *         
***********************************************************************         
                                                                                
SETMINOR NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         GOTO1 DATCON,DMCB,(1,0(R2)),(0,WORK)                                   
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+8,16                                 
         CLC   TGTODAY0,WORK+8      MINOR UNDER 16?                             
         JNL   XIT                                                              
         OI    SVDOBST,MINOR16      SET STATUS FOR MINOR UNDER 16               
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TADDGERR                                                       
         EJECT                                                                  
       ++INCLUDE TAONCAIN                                                       
         EJECT                                                                  
JOBD     DSECT                                                                  
         DS    0D                                                               
WRKIO    DS    CL4000              WORK IOAREA                                  
         EJECT                                                                  
       ++INCLUDE DDGETRETD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE7D                                                       
         SPACE 2                                                                
         ORG  STMWORK                                                           
         DS    0A                                                               
AWRKIO   DS    A                   A(WORKING I/O)                               
ATMPAREA DS    A                   A(TEMPORARY AREA)                            
RELO     DS    A                                                                
TMPLNQ   EQU   12500                                                            
*                                                                               
PROSTAT  DS    XL1                                                              
DISPLAYD EQU   X'80'                                                            
HYPOTIME EQU   X'40'                                                            
*                                                                               
TMPTIME  DS    XL4                                                              
STRTTIME DS    XL2                                                              
ENDTIME  DS    XL2                                                              
SVEND    DS    XL2                                                              
DIFFTMIN DS    H                   IN MINUTES                                   
DIFFTHRS DS    H                   IN HOURS                                     
WORKHRS  DS    H                   WORK HOURS                                   
TRVLHRS  DS    H                   TRAVEL HOURS                                 
TRVLHRS2 DS    H                   TRAVEL HOURS UNROUNDED                       
MEALHRS  DS    H                   MEAL HOURS                                   
MEAL2HRS DS    H                   MEAL 2 HOURS                                 
MEAL3HRS DS    H                   MEAL 3 HOURS                                 
MEALPHRS DS    H                   MEAL DURING PREMIUM HOURS                    
MEALSTRT DS    H                   START OF MEAL                                
MEALEND  DS    H                   END OF MEAL                                  
MEALPH10 DS    H                   MEAL DURING 10% PREM HOURS (8-1PM)           
MEALPH20 DS    H                   MEAL DURING 20% PREM HOURS (1-6AM)           
TRVLPH10 DS    H                   TRVL DURING 10% PREM HOURS (8-1PM)           
TRVLPH20 DS    H                   TRVL DURING 20% PREM HOURS (1-6AM)           
TEMP10   DS    H                   TEMPORARY 10% PREM MEAL HOURS                
TEMP20   DS    H                   TEMPORARY 20% PREM MEAL HOURS                
PREMHRS  DS    H                   PREMIUM HRS                                  
TRAVDED  DS    H                   DEDUCT FROM TRAVEL TIME                      
TRAVDED2 DS    H                   DEDUCT FROM TRAVEL AFTER MIDNIGHT            
HRMIN1   DS    H                   HRS.MINS                                     
HRMIN2   DS    H                   HRS.MINS                                     
WCXLTRVL DS    H                   WEATHER CANCEL. TRAVEL IF NEEDED             
NTHTSHT  DS    XL1                 NTH TIME SHEET                               
DAYTYPE  DS    XL1                 TYPE OF DAY                                  
DAYHLDY  EQU   X'80'               HOLIDAY                                      
DAYSAT   EQU   X'40'               SATURDAY                                     
DAYSUN   EQU   X'20'               SUNDAY                                       
DAYFRI   EQU   X'10'               FRIDAY                                       
PDWDDAY  DS    XL1                 PREVIOUS DAY STATUS                          
OLDPWDAY DS    XL1                 OLD PDW DAY STATUS                           
OLDPDWD  DS    XL2                 OLD PDWD IN DECIMAL                          
OLDPWDT  DS    XL3                 OLD PDW DATE                                 
TEMPDATE DS    XL3                                                              
PDWDDEC  DS    XL2                 NEW PDWD IN DECIMAL                          
SVNPREM  DS    XL2                 SAVED NUMBER OF NIGHT PREMIUM HRS            
MEALNPH  DS    H                   NON-PREMIUM MEALS BEFORE NGHT PREM           
COADST   DS    XL1                 COMMERCIAL ADDENDUM STATUS                   
COADSTNW EQU   X'80'                 NORTHWEST REGIONAL COMMERCIAL              
*                                                                               
SVDATA   DS    0CL(TATTLNQ)                                                     
SVSPOTS  DS    XL1                 NUMBER OF SPOTS                              
SVDAYS   DS    XL1                 NUMBER OF DAYS                               
SVOTIME  DS    XL1                 NUMBER OF OVERTIME HOURS                     
SVDTIME  DS    XL1                 NUMBER OF DOUBLE TIME HOURS                  
SVTRVL   DS    XL2                 NUMBER OF TRAVEL HOURS                       
SVPDWD   DS    XL2                 NUMBER OF PRIOR DAY WARDROBE                 
SVTAG    DS    XL1                 NUMBER OF TAGS                               
SVNP10   DS    XL2                 NIGHT PREMIUM HOURS - 10%                    
SVNP20   DS    XL2                 NIGHT PREMIUM HOURS - 20%                    
         ORG   SVNP10                                                           
SVNP25A  DS    XL2                 NIGHT PREMIUM HOURS - 25% ACTRA              
SVNP25S  DS    XL2                 NIGHT PREMIUM HOURS - 25% SAG                
         ORG   SVNP10                                                           
SVNPST   DS    XL2                 ACTRA NP STRAIGHT HOURS (NOT 2404A)          
SVNPOT   DS    XL2                 ACTRA NP OVERTIME HOURS (NOT 2404A)          
*                                                                               
SV16HR   DS    XL1                 16 HOUR RULE - # OF HOURS OVER (ADJ)         
SVINCL   DS    XL1                 INCLUDE CODE                                 
SVREIM   DS    XL4                 REIMBURSEMENT                                
SVNSPNH  DS    XL4                 AMOUNT NOT SUBJECT TO P&H                    
SVPYMT   DS    XL4                 ADDITION TO PAYMENT AMT (SMOKE PAY)          
SVADJ    DS    XL4                 ADJUSTMENT                                   
SVEXSAT  DS    XL2                 EXTRAS SATURDAY PAY ON FRIDAYS               
         ORG   SVEXSAT                                                          
SVNPDT   DS    XL2                 ACTRA NP DBLTIME HOURS (NOT 2404A)           
SVRPVL   DS    XL1                 REST PERIOD VIOLATIONS                       
SVLENQ   EQU   *-SVDATA                                                         
         DS    XL(TATTLNQ-SVLENQ)                                               
*                                                                               
OLDTTEL  DS    CL(TATTLNQ)         OLD TIMESHEET TOTAL ELEMENT                  
*                                                                               
TMSHDATE DS    XL3                 TIME SHEET DATE                              
TMSEDATE DS    CL6                 TIME SHEET EBCDIC DATE                       
PDWDDATE DS    XL3                 PRIOR DAY WARDROBE DATE                      
INVNO    DS    XL6                 INVOICE NUMBER                               
SVKEY    DS    CL32                                                             
PFKFLAG  DS    CL1                 PFKEY FLAG                                   
ASVPBLK  DS    A                   A(SAVED PTR BLOCK)                           
AADPBLK  DS    A                   A(ADDED PTR BLOCK)                           
MPENTOT1 DS    XL4                 TOTAL AMOUNT FOR MEAL PENALTY 1              
MPENTOT2 DS    XL4                 TOTAL AMOUNT FOR MEAL PENALTY 2              
MPENTOT3 DS    XL4                 TOTAL AMOUNT FOR MEAL PENALTY 3              
MYINCL   DS    XL1                 LOWEST INCLUDE CODE IN T-SHEETS              
TEMPHRS  DS    H                   TEMPORARY HOURS                              
PRMEND   DS    H                   NGHTPREM END TIME (DEFAULT = 6AM)            
SPOTFLG  DS    XL1                 SPOT FLAG                                    
         DS    XL1                 SPARE                                        
MEALSAT  DS    H                                                                
MEALFRI  DS    H                                                                
MLNPHTOT DS    H                   MEAL NON PREMIUM HOURS TOTAL                 
SVML10   DS    H                   EXTRA FRI-SAT, MEAL DURING NP 10%            
SVML20   DS    H                   EXTRA FRI-SAT, MEAL DURING NP 20%            
SVML10F  DS    H                   EXTRA FRIDAY AM, MEAL DURING NP 10%          
SVML20F  DS    H                   EXTRA FRIDAY AM, MEAL DURING NP 20%          
SV16RND  DS    H                   SAVED MINUTES TO ROUND UP 16 HR RULE         
SVSTRT   DS    XL2                 SAVED START TIME                             
ENDFLG   DS    XL1                 END OF MEAL IS AFTER WORK END                
STRTFLG  DS    XL1                 START OF MEAL IS BEFORE WORK START           
SVEND2   DS    XL2                 SAVED END TIME                               
*                                                                               
SVTAINST DS    XL1                 INVOICE STATUS                               
NPEXFLG  DS    XL1                 EXTRA NIGHT PREMIUM FLAG                     
NP6AMFLG DS    XL1                 FORCED 6AM NIGHT PREMIUM END FLAG            
EX10NP   DS    H                   EXTRA 10% NIGHT PREMIUM                      
EX20NP   DS    H                   EXTRA 20% NIGHT PREMIUM                      
NPSTART  DS    H                   NIGHT PREMIUM START TIME                     
SVNPSAG  DS    XL2                 NIGHT PREMIUM SAG HOURS                      
SVDOBST  DS    XL1                 SAVED DATE OF BIRTH STATUS                   
MINOR16  EQU   X'80'               MINOR UNDER 16                               
FIRSTDY  DS    XL1                 FIRST DAY OF 2404A SESSION = C'Y'            
SVCOTYPE DS    CL1                 SAVED COMMERCIAL TYPE                        
SVACTRA  DS    XL1                 SAVED ACTRA TYPE                             
SVCOADST DS    CL2                 SAVED ADDENDUM STATE                         
ACTRULES DS    XL1                 ACTRA RULES FLAG                             
         DS    0F                                                               
EXTRVL   DS    XL2                 EXTRA TRAVEL AFTER MIDNIGHT                  
EXSATOT  DS    XL2                 EXTRA OVERTIME AFTER MIDNIGHT                
EXSATDT  DS    XL2                 EXTRA DOUBLETIME AFTER MIDNIGHT              
EXFRIOT  DS    XL2                 EXTRA OVERTIME BEFORE MIDNIGHT               
EXFRIDT  DS    XL2                 EXTRA DOUBLETIME BEFORE MIDNIGHT             
SMKNP10  DS    XL2                 SMOKE PAY ADJUSTED NP 10%                    
SMKNP20  DS    XL2                 SMOKE PAY UNADJUSTED NP 20%                  
SVSTAT2  DS    XL1                 SAVED STATUS 2 BYTE                          
OLDSTAT  DS    XL1                 OLD TATT STATUS BYTE                         
SVWTRST  DS    XL1                 SAVED WEATHER CXL STATUS                     
SVWTRHF  EQU   X'80'               WEATHER CXL HALF PAYCHECK                    
SVWTR3Q  EQU   X'40'               WEATHER CXL QUARTER PAYCHECK                 
SMK16HR  DS    XL1                 UNADJUSTED 16HR (FOR SMOKE PAY)              
NDMFLAG  DS    XL1                 NON-DEDUCTIBLE MEAL FLAG                     
NCONFLAG DS    XL1                 NON-CONSECUTIVE DAY FLAG                     
NPFRIFLG DS    XL1                 EXTRA FRIDAY AFTER MIDNIGHT NP FLAG          
WBERRFLD DS    XL1                 WEB ERROR FIELD                              
SVCNYR   DS    CL3                 SAVED CONTRACT YEAR                          
US2404   DS    XL1                 SAG RULES FLAG FOR 2404                      
         EJECT                                                                  
*              DSECT TO COVER TEMPORARY STORAGE                                 
         SPACE                                                                  
TEMPD    DSECT                                                                  
         SPACE                                                                  
TEMPLNQ  EQU   *-TEMPD                                                          
         EJECT                                                                  
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         ORG   TWAHOLE+3200                                                     
SVPTRBLK DS    CL((72*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                       
ADPTRBLK DS    CL((72*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS           
         SPACE                                                                  
HOLSPARE DS    CL(L'TWAHOLE-3200-(*-SVPTRBLK)) DEFINE -ERR IF EXCEEDED          
         EJECT                                                                  
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* TAWBDSECT                                                                     
* TAPYS69D                                                                      
* TAPYS78D                                                                      
* TAGENEQUS                                                                     
* TAMAPEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAWBDSECT                                                      
       ++INCLUDE TAPYS69D                                                       
       ++INCLUDE TAPYS78D                                                       
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAMAPEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051TAGENE7   03/02/17'                                      
         END                                                                    
