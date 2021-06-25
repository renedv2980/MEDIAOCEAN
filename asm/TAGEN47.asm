*          DATA SET TAGEN47    AT LEVEL 049 AS OF 09/22/14                      
*PHASE T70247A,*                                                                
         TITLE 'T70247 - HISTORY LIST'                                          
T70247   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 CHTBLNQ,T70247                                                   
         LR    R5,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(TWAHOLE)                                
         USING WORKD,R7                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE                                                                  
         GOTO1 INITIAL,DMCB,PFTAB                                               
         ST    R5,ACVRHLD          SAVE ADDRESS OF COVERED HLDS TABLE           
         AHI   R5,CHTBLNQ                                                       
         ST    R5,ACVRHLDX                                                      
*                                                                               
         BRAS  RE,SUCVRHLD         SETUP COVERED HLDS VARIABLES                 
*&&DO                                                                           
         TM    TRNSTAT2,CTSTEREO   IF STEREO ACTIVE                             
         BZ    HSTL10                                                           
         ZIC   RE,CALLSP           AND IF STACK NOT EMPTY                       
         LTR   RE,RE                                                            
         BZ    HSTL10                                                           
         BCTR  RE,0                                                             
         LA    RE,CALLSTCK(RE)                                                  
         CLI   0(RE),SCR2C         AND CAME FROM ESTIMATING                     
         BNE   HSTL10                                                           
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSUBS   SET OVERRIDING SUB-SCREEN FOR STEREO         
         MVI   TIOBAID,X'01'       SET SUB-SCREEN CODE                          
*&&                                                                             
HSTL10   CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   HSTL20                                                           
         BAS   RE,VKEY             VALIDATE KEY FIELDS                          
         BAS   RE,VALOPTS          VALIDATE OPTIONS                             
         CLI   ALLVAL,C'Y'         IF NOT ALL FIELDS PREV VALIDATED             
         BE    *+8                                                              
         BAS   RE,INIT             INITIALIZE INFO FOR SYSIO                    
         B     XIT                                                              
         SPACE 3                                                                
HSTL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   HSTL30                                                           
         MVI   NLISTS,7                                                         
         OI    GLSTSTAT,RETEXTRA   GET CONTROL BACK AT END OF PAGE              
         MVC   LLIST,=AL2(SINSEL2H-SININVH)    SET LEN OF LIST LINE             
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
HSTL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         XC    TIKEY,TIKEY                                                      
         XC    TOTTAB,TOTTAB       CLEAR TOTAL ACCUMULATOR                      
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,MYHOOK           SET UP HEADHOOK                              
         ST    R2,HEADHOOK                                                      
         LA    R2,P                                                             
         B     LSTREC                                                           
         SPACE 3                                                                
LSTREC   EQU   *                                                                
         BRAS  RE,BDCVRHLD         BUILD COVERED HLDS TABLE                     
         SPACE 1                                                                
         LA    R0,IOHOOK           SET ADDRESS OF HOOK FOR SYSIO                
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 PGCNTL,DMCB,TABLE,TIKEY,TIQSKEY                                  
         SPACE 1                                                                
         XC    TOT1(TOTLNQ),TOT1   CLEAR ACCUMULATORS                           
         OC    TIQSKEY,TIQSKEY     IF AT BEGINING OF LIST                       
         BZ    LST2                SET INTERNAL COMML NUM TO ORIGINAL           
         LA    R3,TIQSKEY          ELSE SET IT TOO CURRENT NUMBER               
         USING TLINPD,R3                                                        
         MVC   TIFCOM,TLINHCOM                                                  
         DROP  R3                                                               
         SPACE 1                                                                
LST2     MVC   TIAREC,AIO                                                       
         BRAS  RE,RDCVRHLD         READ ANY CONTINUATION HLDS                   
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         OC    TISVCOM,TISVCOM                                                  
         BZ    LST3                                                             
         MVC   TIFCOM,TISVCOM                                                   
         SPACE 1                                                                
LST3     XC    TIKEY,TIKEY                                                      
         MVI   TOTTYPE,TOTTYEN     END TOTALS                                   
         BAS   RE,DISPTOT          DISPLAY THE LAST PAGES TOTALS                
         CLI   MODE,PRINTREP       IF NOT PRINTING REPORT                       
         BNE   XIT                                                              
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE                                    
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(15,R1),=C'INVOICE RECORDS'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    XIT                                                              
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              INITIALIZES INFO NECESSARY FOR SYSIO                             
         SPACE                                                                  
VKEY     NTR1                                                                   
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         TM    SINAGYH+4,X'20'     IF AGENCY NOT PREV VALIDATED                 
         BZ    VK2                                                              
         TM    SINCIDH+4,X'20'     OR CID NOT PREV VALIDATED                    
         BZ    VK2                                                              
         TM    SINVERH+4,X'20'     OR VER NOT PREV VALIDATED                    
         BZ    VK2                                                              
         TM    SCRSTAT,RECCHG      OR RECORD TYPE CHANGED                       
         BZ    VK20                                                             
         SPACE                                                                  
VK2      LH    RF,=Y(TIEND-TASYSIOD)  CLEAR SYSIO'S W/S                         
         XCEFL TASYSIOD                                                         
         MVI   ALLVAL,C'N'         SET NOT ALL FIELDS VALID                     
         GOTO1 CHKCLG,DMCB,SINAGYH,SINCIDH  CHECK/HANDLE CLI GRP                
         MVI   BYTE,0              ENFORCE AGENCY LIMIT RESTRICTIONS            
         BE    *+8                                                              
         MVI   BYTE,X'80'          SKIP AGENCY LIMIT RESTRICTIONS               
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'20',SINAGYH)  AGENCY               
         GOTO1 CHAROUT,DMCB,(X'80',TASNELQ),0                                   
         MVC   AGYNAME,TGNAME                                                   
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,(X'30',TLCOICDQ),(X'08',SINCIDH),SINCIDNH            
         SPACE                                                                  
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TIFCOM,TLCOCOM      FILTER ON INTERNAL COMMERCIAL NUM            
         SPACE                                                                  
         CLI   TLCOVER,TLCOV026    IF AIO DOES NOT CONTAIN MAIN COMM'L          
         BE    VK4                 RECORD, GO GET IT                            
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'AC',TIFCOM),SINCIDNH                     
         BE    VK4                                                              
         DC    H'00'                                                            
         SPACE                                                                  
VK4      BAS   RE,PROCVERS         PROCESS MAIN COMML, IF VERSION               
         XC    TISVCOM,TISVCOM     CLEAR SAVED INTERNAL COMML NUMBER            
         XC    TISVAGY,TISVAGY     & AGENCY                                     
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TACMELQ,SINCOMMH,TACMTYPG COMMENT                   
         SPACE                                                                  
         MVI   VERCODE,0                                                        
         LA    R2,SINVERH          IF VERSION INPUT                             
         CLI   5(R2),0                                                          
         BE    VK18                                                             
         GOTO1 ANY                                                              
         SPACE                                                                  
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHARACTER          
         BZ    VK10                VERSION CODES                                
         CLI   5(R2),3             AND INPUT IS 3 CHARACTERS OR LESS            
         BH    VK10                                                             
         TM    4(R2),X'08'         AND VALID NUMERIC                            
         BZ    VK10                                                             
         GOTO1 VALINUM             SAVE HEX VALUE INTO ACTUAL                   
         SPACE                                                                  
         CLI   ACTUAL,26           IF VERSION CODE IS GREATER THAN              
         BNH   VK10                26                                           
         SPACE                                                                  
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
VK6      CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   ACTUAL,VINDUPLM                                                  
         BNH   VK7                                                              
         LA    RE,VINDLNQ(RE)                                                   
         B     VK6                                                              
         SPACE 1                                                                
VK7      XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   FLDINV                                                           
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE                                                                  
VK10     MVI   ELCODE,TAVRELQ      CHECK AGAINST VERSIONS ON COMML              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK15     BAS   RE,NEXTEL                                                        
         BNE   FLDINV                                                           
         USING TAVRD,R4                                                         
         SPACE 1                                                                
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHARACTER          
         BZ    VK15A               VERSION CODES                                
         CLI   5(R2),3             AND INPUT IS 3 CHARACTERS OR LESS            
         BH    VK15B               ASSUME VERSION CODE                          
         CLC   TAVRVERS,ACTUAL     ELSE CID                                     
         B     VK15C                                                            
         SPACE 1                                                                
VK15A    CLI   5(R2),1             IF ONE CHARACTER INPUT                       
         BNE   VK15B                                                            
         CLC   TAVRVERS,WORK       ASSUME VERSION CODE                          
         B     VK15C                                                            
VK15B    CLC   TAVRCID,WORK        ELSE CID                                     
VK15C    BNE   VK15                                                             
         MVC   VERCODE,TAVRVERS                                                 
VK18     OI    4(R2),X'20'                                                      
*                                                                               
VK20     LA    R2,SINSTRH                                                       
         TM    4(R2),X'20'         IF START AT FIELD NOT PREV VALIDATED         
         BO    VK40                                                             
         MVI   ALLVAL,C'N'                                                      
         XC    LASTINV,LASTINV     RESET 'LAST' INV                             
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         CLI   5(R2),6             LENGTH MUST BE 6 FOR INVOICE                 
         BNE   FLDINV                                                           
         MVC   INV,SINSTR                                                       
         GOTO1 TINVCON,DMCB,INV,LASTINV,DATCON INVOICE NUMBER                   
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    LASTINV,=6X'FF'     COMPLEMENT IT                                
         SPACE                                                                  
VK40     OI    4(R2),X'20'         SET PREV VALIDATED                           
         LA    R2,SINUSEH                                                       
         TM    4(R2),X'20'         IF USE FIELD PREV VALIDATED                  
         BO    VK70                                                             
         MVI   ALLVAL,C'N'                                                      
         XC    TIFUSE,TIFUSE                                                    
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         GOTO1 USEVAL,DMCB,(X'40',8(R2))   VALIDATE USE                         
         BNE   FLDINV                                                           
         MVC   TIFUSE,TGUSCDE                                                   
         SPACE                                                                  
VK70     OI    4(R2),X'20'                                                      
         LA    R2,SINPERH          PERIOD                                       
         TM    4(R2),X'20'         IF NOT PREV VALIDATED                        
         BO    VKX                                                              
         MVI   ALLVAL,C'N'                                                      
         XC    TIQPEND,TIQPEND                                                  
         XC    TIQPSTR,TIQPSTR                                                  
         CLI   5(R2),0             CHK IF ENTERED                               
         BE    VKX                 IF NOT, VALIDATE AND EXIT                    
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   TIQPEND,PVALPEND                                                 
         MVC   TIQPSTR,PVALPSTA                                                 
         SPACE                                                                  
VKX      OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
*           ROUTINE VALIDATES USER OPTIONS                                      
*           AND SETS CORRESPONDING FLAGS                                        
         SPACE                                                                  
VALOPTS  NTR1                                                                   
         LA    R2,SINOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VO90                                                             
         NI    TIQFLAGS,X'FF'-TIQFPDUM   INIT DUMMY FLAG OFF                    
         MVI   ALLVAL,C'N'                                                      
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VO10                                                             
         MVI   TIQDTYPE,TIQDCYCS   DEFAULT FILTER BY CYCLE DATES                
         B     VO90                                                             
         SPACE                                                                  
VO10     LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         BZ    FLDINV                                                           
VO20     CLC   =C'PER',SCDATA1                                                  
         BNE   VO40                                                             
         CLI   SINPERH+5,0         ENSURE PERIOD ENTERED                        
         BE    FLDINP              ELSE REPORT ERROR                            
         MVI   TIQDTYPE,TIQDCYCS   FILTER BY CYCLE DATES                        
         CLC   =C'CYC',SCDATA2                                                  
         BE    VO50                                                             
         MVI   TIQDTYPE,TIQDPAY    FILTER BY PAY DATE                           
         CLC   =C'PAY',SCDATA2                                                  
         BE    VO50                                                             
         MVI   TIQDTYPE,TIQDBILL   FILTER BY BILL DATE                          
         CLC   =C'BIL',SCDATA2                                                  
         BE    VO50                                                             
         MVI   TIQDTYPE,TIQDCHK    FILTER BY CHECK DATE                         
         CLC   =C'CHK',SCDATA2                                                  
         BE    VO50                                                             
         MVI   TIQDTYPE,TIQDDUE    FILTER BY DUE DATE                           
         CLC   =C'DUE',SCDATA2                                                  
         BNE   FLDINV                                                           
         B     VO50                                                             
         SPACE                                                                  
VO40     CLC   =CL10'D',SCDATA1    CHK FOR 'SHW DUMMY'                          
         BNE   FLDINV              NO MATCH, MUST BE INVALID                    
         CLI   SCLEN2,0            ENSURE SECOND HALF OF FIELD EMPTY            
         BNE   FLDINV              IF NOT, ERROR                                
         OI    TIQFLAGS,TIQFPDUM   ELSE,TURN ON DUMMY FLAG                      
         SPACE                                                                  
VO50     DS    0H                                                               
         LA    R3,SCANNEXT                                                      
         BCT   R0,VO20                                                          
         SPACE                                                                  
VO90     OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*          ROUTINE INITIALIZES FIELDS FOR SYSIO                                 
         SPACE                                                                  
INIT     NTR1                                                                   
         XC    TIABUFF,TIABUFF     CLEAR SO SYSIO WON'T DIE                     
         XC    TOTTAB,TOTTAB       CLEAR TOTAL ACCUMULATOR                      
         XC    TOT1(TOTLNQ),TOT1   CLEAR ACCUMULATORS                           
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO FIRST TIME IN          
         XC    TISVCOM,TISVCOM                                                  
         XC    TISVAGY,TISVAGY                                                  
         XC    TIQSKEY,TIQSKEY                                                  
         MVI   LISTSW,C'F'         SET LIST TO BEGINING                         
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         OI    TIQFLAG2,TIQFNLIM   SKIP AGY & CLI LIMIT ACCESS CHECK            
         MVI   TIREAD,TLINCDQ      LIST BY INVOICE                              
         B     XIT                                                              
         EJECT                                                                  
*              IF COMML IS A VERSION, SHOWS MAIN COMMERCIAL ID                  
         SPACE 1                                                                
PROCVERS NTR1                                                                   
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         CLC   TACOCID,TGCID       IF INPUT CID DOESN'T MATCH RECORD            
         BE    XIT                                                              
         MVC   SINCID,TACOCID      REPLACE VERSION CID W/MAIN COMM'L'S          
         MVC   TGCID,TACOCID       SET TGCID                                    
         OI    SINCIDH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         CLC   LISTNUM,NLISTS      IF LISTMON RETURNING AN EXTRA TIME           
         BNE   HK00                                                             
         MVI   TOTTYPE,TOTTYRU     RUNNING TOTALS                               
         BAS   RE,DISPTOT          DISPLAY THE TOTALS                           
         B     HK20                AND EXIT                                     
         SPACE 1                                                                
HK00     MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         MVI   HISCOMM,0                                                        
         XC    PDAGY,PDAGY                                                      
         BAS   RE,FILTER           FILTER LIST                                  
         BNE   XIT                                                              
         SPACE                                                                  
         USING TLIND,R3                                                         
         L     R3,TIAREC                                                        
*                                                                               
         CLI   TLINCD,TLINCDQ      HAS TO BE AN INVOICE                         
         BE    *+12                                                             
         CLI   TLINCD,TLHCCDQ      OR HISTORY COMMENT                           
         BNE   XIT                                                              
*                                                                               
         LR    R4,R3                                                            
         CLC   TGAGY,TLINAGY       IF INVOICE PAID UNDER A DIFF. AGENCY         
         BE    *+10                                                             
         MVC   PDAGY,TLINAGY       SAVE IT                                      
         CLI   TLINCD,TLHCCDQ                                                   
         BNE   *+12                                                             
         MVI   HISCOMM,C'Y'        IF HISTORY COMMENT RECORD                    
         B     HK9                 SET FLAG AND BRANCH                          
         SPACE                                                                  
         L     R4,TIAREC                                                        
         USING TAIND,R4                                                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMNT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(1,TAINPDTE),(8,LISPAY) PAY DATE                     
         SPACE                                                                  
         TM    TAINSTAT,TAINSAPR   IF INVOICE NOT APPROVED                      
         BO    *+8                                                              
         MVI   LISCODE+L'LISCODE,C'*'  PUT * AFTER IT                           
         SPACE                                                                  
         TM    TAINSTA2,TAINSPRM   IF PRIMARY INVOICE                           
         BNO   *+8                                                              
         MVI   LISCODE+L'LISCODE,C'%'  PUT % AFTER IT                           
         SPACE                                                                  
         MVI   INVBILL,C'N'        SET FLAG - INVOICE NOT BILLED                
         OC    TAINBDTE,TAINBDTE                                                
         BZ    HK1                                                              
         MVI   INVBILL,C'Y'        INVOICE BILLED                               
         SPACE                                                                  
HK1      L     R4,TIAREC                                                        
         USING TARAD,R4                                                         
         MVI   ELCODE,TARAELQ      GET BOVER ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   *+16                                                             
         CLI   TARASTA2,0                                                       
         BE    *+8                                                              
         MVI   LISCODE+L'LISCODE,C'B'  PUT B AFTER IT                           
         L     R4,TIAREC                                                        
         USING TABDD,R4                                                         
         MVI   ELCODE,TABDELQ      GET BILLING DETATAILS ELEMNT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    BDTOT,BDTOT                                                      
         CLI   INVBILL,C'Y'        IF INVOICE BILLED                            
         BNE   HK2                                                              
         MVC   BDTOT,TABDTOT       GET INVOICE TOTAL                            
         SPACE                                                                  
HK2      L     R1,TABDTAX          CALCULATE BDTNH+                             
         A     R1,TABDHND                                                       
         A     R1,TABDHNDC                                                      
         A     R1,TABDFICR                                                      
         ST    R1,BDTNH                                                         
         SPACE                                                                  
         MVI   VERSTAT,0                                                        
         USING TAVRD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAVRELQ      GET VERSIONS ELEMNT                          
         BAS   RE,GETEL            DISPLAY VERSION IN LIFT FIELD                
         BNE   HK3                                                              
         MVC   VERSTAT,TAVRVERS                                                 
         MVC   LISLFT+1(1),TAVRVERS                                             
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    HK3                                                              
         XC    LISLFT,LISLFT                                                    
         EDIT  TAVRVERS,LISLFT,ALIGN=LEFT                                       
         SPACE                                                                  
         USING TAPDD,R4                                                         
HK3      L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMNT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVPDPST2,TAPDPST2                                                
         SPACE                                                                  
         MVC   PDPNH,TAPDPNH       P & H                                        
         SPACE                                                                  
         TM    TAPDOPT3,TAPDODUM   IF DUMMY INVOICE                             
         BNO   *+8                                                              
         MVI   LISCODE+L'LISCODE,C'D' PUT D AFTER IT                            
         SPACE                                                                  
         L     R1,TAPDPAYI         CALCULATE PDPAY+                             
         A     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         ST    R1,PDPAY                                                         
         SPACE                                                                  
         GOTO1 TINVCON,DMCB,TAPDINV,LISCODE,DATCON INVOICE NUMBER               
         MVC   SVINV,TAPDINV       SAVE THIS INVOICE NUMBER                     
         SPACE                                                                  
         TM    TAPDSTAT,TAPDSLFT   LIFT PAYMENT                                 
         BZ    *+8                                                              
         MVI   LISLFT+1,C'Y'                                                    
         TM    TAPDSTA2,TAPDSLFA   PAYMENT TO ALL ON COMM'L                     
         BZ    *+8                                                              
         MVI   LISLFT+1,C'A'                                                    
         TM    TAPDPST1,TAPDPBNP   BILL-NO-PAYROLL PAYMENT                      
         BZ    *+8                                                              
         MVI   LISBNP,C'Y'                                                      
         TM    TAPDPST1,TAPDPCRD   CREDIT PAYMENT                               
         BZ    *+8                                                              
         MVI   LISCRD,C'Y'                                                      
         SPACE                                                                  
         OC    TAPDCYCS,TAPDCYCS    IF THERE ARE CYCLE DATES                    
         BZ    HK5                                                              
         MVC   SVCYCLE,TAPDCYCS                                                 
         GOTO1 DATCON,DMCB,(1,TAPDCYCS),(8,LISCYCS)  START CYCLE                
         OC    TAPDCYCE,TAPDCYCE   SKIP IF NO CYCLE END DATE                    
         BZ    HK4                                                              
         MVI   LISCYCD,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,TAPDCYCE),(8,LISCYCE) END CYCLE                   
         SPACE                                                                  
HK4      MVI   CYCSTAT,0                                                        
         CLC   SVCYCLE+3(3),=X'A11001'                                          
         BL    HK5                                                              
         MVI   CYCSTAT,C'Y'                                                     
         SPACE                                                                  
HK5      GOTOR STUDETS,DMCB,TIAREC                                              
         MVC   LISUSE,MYBLOCK                                                   
         GOTOR CLAITN                                                           
         SPACE                                                                  
HK7      TM    TAPDPST1,TAPDDFUH                                                
         BZ    HK9                                                              
         SPACE                                                                  
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         SPACE                                                                  
         CLI   TGUSEQU,ULCB                                                     
         BNE   HK7A                                                             
         MVI   LISUSE+7,C'*'                                                    
         B     HK9                                                              
         SPACE                                                                  
HK7A     CLI   TGUSEQU,UCBL                                                     
         BNE   HK7B                                                             
         LA    RE,LISUSE+5                                                      
         TM    TGUSTYST,UPGRADE                                                 
         BZ    *+8                                                              
         LA    RE,LISUSE+13                                                     
         MVI   0(RE),C'*'                                                       
         B     HK9                                                              
         SPACE                                                                  
HK7B     CLI   TGUSEQU,USCB                                                     
         BNE   HK7C                                                             
         MVI   LISUSE+16,C'*'                                                   
         B     HK9                                                              
         SPACE                                                                  
HK7C     CLI   TGUSEQU,USWS                                                     
         BNE   HK7D                                                             
         LA    RE,LISUSE+12                                                     
         TM    TGUSTYST,UPGRADE                                                 
         BZ    *+8                                                              
         LA    RE,LISUSE+16                                                     
         MVI   0(RE),C'*'                                                       
         B     HK9                                                              
         SPACE                                                                  
HK7D     LA    RE,LISUSE+8                                                      
         TM    TGUSTYST,UPGRADE                                                 
         BZ    *+8                                                              
         LA    RE,LISUSE+16                                                     
         MVI   0(RE),C'*'                                                       
         SPACE                                                                  
HK9      MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         SPACE                                                                  
         XC    MYBLOCK,MYBLOCK     SET UP AREA FOR CHAROUT - NEED X'00'         
         MVI   MYBLOCK,L'MYBLOCK-1 AT END FOR START OF NEXT FLD HEADER          
         MVC   AIO,TIAREC          SET AIO FOR CHAROUT                          
         MVC   MYBLOCK,SPACES                                                   
         GOTO1 CHAROUT,DMCB,TACMELQ,MYBLOCK,TACMTYPH  HISTORY COMMENT           
         MVC   AIO,AIO1            RESTORE AIO                                  
         SPACE                                                                  
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   HK9C                                                             
         CLI   HISCOMM,C'Y'        AND NOT HISTORY COMMENT RECORD               
         BE    HK12                                                             
         LA    R2,P2               SET R2 TO 2ND PRINT LINE                     
         B     HK10                                                             
*                                                                               
HK9C     XC    USCANINV,USCANINV                                                
         XC    USCAN,USCAN                                                      
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAUCELQ      GET US/CAN LINKED INVOICE ELEMENT            
         BAS   RE,GETEL                                                         
         BNE   HK9E                                                             
         USING TAUCD,R4                                                         
         MVC   USCAN,=C'U$'                                                     
         MVC   USCANINV,TAUCINU    US INVOICE                                   
         XC    USCANINV,=6X'FF'    UNCOMPLEMENT                                 
         CLC   USCANINV,SVINV                                                   
         BNE   HK9E                                                             
         MVC   USCAN,=C'C$'                                                     
         MVC   USCANINV,TAUCINC    CAN INVOICE                                  
         XC    USCANINV,=6X'FF'    UNCOMPLEMENT                                 
         DROP  R4                                                               
*                                                                               
HK9E     CLI   HISCOMM,C'Y'        IF HISTORY COMMENT RECORD                    
         BNE   HK9G                                                             
*        TM    TRNSTAT2,CTSTEREO   AND STEREO NOT ACTIVE                        
*        BO    HK9G                                                             
         OC    USCANINV,USCANINV   IF CAN/US LINKED INVOICE                     
         BZ    HK9F                                                             
         MVC   LISCOMS,MYBLOCK+8   MOVE SHORT COMMENT TO 1ST LINE               
         MVC   LISUSCN(2),USCAN                                                 
         GOTO1 TINVCON,DMCB,USCANINV,LISUSCN+2,DATCON  US/CN LINKED INV         
         B     HK9G                                                             
                                                                                
HK9F     TM    SVPDPST2,TAPDPEUR   IF PAYMENT MADE IN EUROS                     
         BZ    HK9F10                                                           
         MVC   LISCOMS,MYBLOCK+8   MOVE SHORT COMMENT TO 1ST LINE               
         MVC   LISEURO,EUROPD      AND INDICATE WHETHER INVOICE                 
         L     R4,TIAREC           IS PAID NOT BILLED                           
         MVI   ELCODE,TABDELQ3                                                  
         BAS   RE,GETEL                                                         
         BNE   HK9F10                                                           
         MVC   LISEURO,EUROBD      OR PAID AND BILLED                           
                                                                                
HK9F10   MVC   LISCOMM,MYBLOCK+8   MOVE COMM TO FIRST LINE                      
         SPACE 1                                                                
HK9G     L     R2,ATHISLST         MOVE NEXT LINE TO SCREEN W/O LISTMON         
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               BUMP TO NEXT DETAIL LINE                     
         OI    6(R2),X'80'                                                      
         LA    R2,8(R2)                 BUMP PAST HEADER                        
         SPACE 1                                                                
HK10     XC    LISAGY(L'SININV),LISAGY  CLEAR 2ND DETAIL LINE                   
         CLI   HISCOMM,C'Y'        IF HISTORY COMMENT RECORD                    
         BNE   HK14                                                             
*        TM    TRNSTAT2,CTSTEREO   AND STEREO ACTIVE                            
*        BZ    HK20                                                             
*K12     MVC   LISCOMM,MYBLOCK+8   MOVE COMMENT TO 2ND LINE                     
HK12     DS    0H                                                               
         B     HK20                                                             
HK14     MVC   LISAGY,PDAGY        PAID AGENCY                                  
         OC    USCANINV,USCANINV   IF CAN/US LINKED INVOICE                     
         BZ    HK16                                                             
         MVC   LISCOMS,MYBLOCK+8   MOVE SHORT COMMENT TO 1ST LINE               
         MVC   LISUSCN(2),USCAN                                                 
         GOTO1 TINVCON,DMCB,USCANINV,LISUSCN+2,DATCON  US/CN LINKED INV         
         B     HK19                                                             
HK16     TM    SVPDPST2,TAPDPEUR   IF PAYMENT MADE IN EUROS                     
         BZ    HK17                                                             
         MVC   LISCOMS,MYBLOCK+8   MOVE SHORT COMMENT TO 1ST LINE               
         MVC   LISEURO,EUROPD      AND INDICATE WHETHER INVOICE                 
         L     R4,TIAREC           IS PAID NOT BILLED                           
         MVI   ELCODE,TABDELQ3                                                  
         BAS   RE,GETEL                                                         
         BNE   HK19                                                             
         MVC   LISEURO,EUROBD      OR PAID AND BILLED                           
         B     HK19                                                             
HK17     MVC   LISCOMM,MYBLOCK+8   COMMENT                                      
         SPACE                                                                  
HK19     L     R3,PDPAY            SET AMOUNT TO BE EDITED                      
         LA    R5,LISTPAY                                                       
         BAS   RE,EDIT12                                                        
         CLI   INVBILL,C'Y'        IF INVOICE WAS BILLED                        
         BNE   HK20                                                             
         L     R3,BDTOT            SET AMOUNT TO BE EDITED                      
         LA    R5,LISTOT                                                        
         BAS   RE,EDIT12                                                        
         SPACE                                                                  
HK20     DS    0H                                                               
         CLI   HISCOMM,C'Y'        IF NOT HISTORY COMMENT RECORD                
         BE    *+8                                                              
         BAS   RE,ACCPG            ACCUMULATE THIS PAGE'S TOTALS                
         CLI   MODE,PRINTREP       TEST NOT PRINTING REPORT                     
         BE    HK30                                                             
         GOTO1 LISTMON             ELSE CALL LISTMON                            
         SPACE                                                                  
         CLI   HISCOMM,C'Y'        IF NOT HISTORY COMMENT RECORD                
         BE    XIT                                                              
HK25     BRAS  RE,RDCVRHLD         READ COVERED HLDS TABLE                      
         B     XIT                                                              
         SPACE                                                                  
HK30     GOTO1 CATCHIOS            ENSURE IO OVERUSE FOR REPORTS                
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         SPACE                                                                  
         CLI   HISCOMM,C'Y'        IF NOT HISTORY COMMENT RECORD                
         BE    HK35                                                             
         AP    COUNTER,=P'1'       COUNT INVOICE RECORDS OUTPUT                 
HK35     GOTO1 SPOOL,DMCB,(R8)     SPOOL IT FOR COMMENTS                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ACCUMULATE RECORD'S AMOUNTS IN PAGE ACCUMULATORS                       
*                                                                               
ACCPG    NTR1                                                                   
         L     R1,BDTOT                                                         
         A     R1,TOT1             TOTAL RUNNING TOTAL                          
         ST    R1,TOT1                                                          
         L     R1,BDTNH                                                         
         A     R1,TOT2             T&H RUNNING TOTAL                            
         ST    R1,TOT2                                                          
         SPACE                                                                  
         L     R1,PDPNH                                                         
         A     R1,TOT3             P&H RUNNING TOTAL                            
         ST    R1,TOT3                                                          
         L     R1,PDPAY                                                         
         A     R1,TOT4             PAY RUNNING TOTAL                            
         ST    R1,TOT4                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PUTS OUT SUBTOTAL FOR SCREENS                            
*                                                                               
         SPACE                                                                  
DISPTOT  NTR1                                                                   
         XC    BYTE,BYTE           CLEAR BYTE FOR TOTCNTL                       
         CLI   TOTTYPE,TOTTYRU                                                  
         BE    DT05                                                             
         MVI   BYTE,C'E'           IF END OF TOTALS - SET BYTE                  
         SPACE                                                                  
DT05     GOTO1 TOTCNTL,DMCB,(BYTE,TOT1)                                         
         SPACE                                                                  
         USING LTOTD,R2                                                         
         LA    R2,SINTOTH                                                       
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         LA    R2,P+11                                                          
         SPACE 1                                                                
         MVC   LPNHNM,LTPNH                                                     
         L     R3,TOT3                                                          
         LA    R5,LTOTPNH                                                       
         BAS   RE,EDIT11L          DISPLAY PNH                                  
         MVC   LTNHNM,LTTNH                                                     
         L     R3,TOT2                                                          
         LA    R5,LTOTTNH                                                       
         BAS   RE,EDIT11L          DISPLAY TNH                                  
         SPACE                                                                  
         L     R3,TOT4                                                          
         LA    R5,LTOTPAY                                                       
         BAS   RE,EDIT12           DISPLAY PAY                                  
         L     R3,TOT1                                                          
         LA    R5,LTOTTOT                                                       
         BAS   RE,EDIT12           DISPLAY INVOICE TOTAL                        
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   DTX                                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT OUT                                 
         B     XIT                                                              
         SPACE                                                                  
DTX      OI    6(R2),X'80'         TRANSMIT FIELD AGAIN                         
         B     XIT                                                              
*              ROUTINE TO FILTER INVOICE RECORDS                                
         SPACE 1                                                                
FILTER   NTR1                                                                   
         OC    LASTINV,LASTINV     IF A LAST INV WAS REQUESTED                  
         BZ    FILT10                                                           
         USING TLIND,R3                                                         
         L     R3,TIAREC                                                        
         CLC   LASTINV,TLININV     & THIS IS IT                                 
         BNL   YES                                                              
FILT05   MVC   KEY(L'TLINKEY),TIKEY                                             
         LA    R3,KEY              MAKE SYSIO END IT'S READ                     
         USING TLINPD,R3                                                        
         MVI   TLINHINV,X'FF'                                                   
         GOTO1 HIGH                                                             
         B     NO                                                               
         DROP  R3                                                               
*                                                                               
FILT10   CLI   VERCODE,0           IF VERSION LIFT CODE FILTER                  
         BE    YES                                                              
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TAVRD,R4                                                         
         CLC   VERCODE,TAVRVERS    MATCH ON IT                                  
         B     XIT                                                              
         EJECT                                                                  
*        EDIT AMOUNT IN R3 TO LOCATION IN R5                                    
EDIT12   DS    0H                                                               
         EDIT  (R3),(12,(R5)),2,FLOAT=-                                         
         BR    RE                                                               
         SPACE 2                                                                
*        EDIT AMOUNT IN R3 TO LOCATION IN R5 - ALIGN = LEFT                     
EDIT11L  DS    0H                                                               
         EDIT  (R3),(11,(R5)),2,FLOAT=-,ALIGN=LEFT                              
         BR    RE                                                               
         SPACE 2                                                                
*        HEADLINE HOOK                                                          
         SPACE                                                                  
MYHOOK   NTR1                                                                   
         MVC   HEAD4+8(6),TGAGY     AGENCY CODE                                 
         MVC   HEAD4+16(16),AGYNAME AGENCY NAME                                 
         MVC   HEAD4+70(12),TGCID   COMMERCIAL ID                               
         MVC   HEAD5+55(20),SINCIDN COMMERCIAL TITLE                            
         MVC   HEAD5+87(1),SINVER   VERSION ID                                  
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINP   LA    R2,SINPERH          POINT R2 AT PERIOD HEADER                    
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
OPTERR   MVI   ERROR,ERCOMPT                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SINSELH                                                       
         MVI   TOTTYPE,TOTTYRU     RUNNING TOTALS                               
         BAS   RE,DISPTOT          DISPLAY THE TOTALS                           
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'CH',CL8'CHECK   ',CL8'LIST   '                               
PF13     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LISPFAGY-1),AL2(LISPFAGY-LISTD)                   
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LISCODE-1),AL2(LISCODE-LISTD)                     
         SPACE                                                                  
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'CL',CL8'CLA     ',CL8'DISPLAY'                               
PF14     DC    AL1(KEYTYCUR,L'LISPFAGY-1),AL2(LISPFAGY-LISTD)                   
         DC    AL1(KEYTYCUR,L'LISCODE-1),AL2(LISCODE-LISTD)                     
         SPACE                                                                  
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 3                                                                
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         SPACE 3                                                                
LTPNH    DC    C'P&&H='                                                         
LTTNH    DC    C'T&&H='                                                         
EUROPD   DC    CL(L'LISEURO)'PD EUR/NB'                                         
EUROBD   DC    CL(L'LISEURO)'PD EUR/B'                                          
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'HISTORY LIST'                                            
         SSPEC H2,33,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,1,C'AGENCY'                                                   
         SSPEC H4,56,C'COMMERCIAL ID'                                           
         SPACE 1                                                                
         SSPEC H5,84,C'VER'                                                     
         SPACE 1                                                                
         SSPEC H7,1,C'INV #'                                                    
         SSPEC H7,8,C'CYCLE DATES'                                              
         SSPEC H7,26,C'L'                                                       
         SSPEC H7,28,C'BNP'                                                     
         SSPEC H7,32,C'C'                                                       
         SSPEC H7,34,C'PAY DATE'                                                
         SSPEC H7,43,C'PAYMENT TYPE'                                            
         SSPEC H7,59,C'WAGES'                                                   
         SSPEC H7,69,C'INV TOT'                                                 
         SPACE 1                                                                
         SSPEC H8,1,C'-----'                                                    
         SSPEC H8,8,C'-----------'                                              
         SSPEC H8,26,C'-'                                                       
         SSPEC H8,28,C'---'                                                     
         SSPEC H8,32,C'-'                                                       
         SSPEC H8,34,C'--------'                                                
         SSPEC H8,43,C'------------'                                            
         SSPEC H8,59,C'-----'                                                   
         SSPEC H8,69,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE COUNTS ALL ITN PROGRAMS IN INVOICE'S                     
*              CYCLE AND REPORTS THAT NUMBER                                    
         SPACE                                                                  
         USING LISTD,R2                                                         
CLAITN   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCLA        EXIT IF INVOICE IS NOT                       
         BNE   CITNX               FOR CLASS A USE                              
         SPACE                                                                  
         CLI   CYCSTAT,C'Y'        EXIT IF CYCLE DOES NOT                       
         BNE   CITNX               END LATER THAN 9/30/01                       
         SPACE                                                                  
         XC    ITNCNT,ITNCNT       CLEAR ITN USE COUNTERS                       
         XC    ITNLCNT,ITNLCNT                                                  
         SPACE                                                                  
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
         MVC   MINVOICE,TLININV                                                 
         MVC   FINVOICE,TLININV                                                 
         DROP  R4                                                               
         SPACE                                                                  
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL            GET PAYMENT DETAILS ELEMENT                  
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAPDD,R4                                                         
         MVC   SVSTUS,TAPDSTUS     AND SAVE STARTING USE NUMBER                 
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY              READ ALL PASSIVE INVOICE KEYS WITH           
         USING TLINPD,R4           MATCHING INTERNAL COMMERCIAL NUMBER          
         MVI   TLINPCD,TLINHCDQ                                                 
         MVC   TLINHCOM,TIFCOM                                                  
         MVC   TLINHINV,MINVOICE                                                
         GOTO1 HIGH                                                             
         B     CITN20                                                           
CITN10   GOTO1 SEQ                                                              
CITN20   CLC   KEY(TLINHCOM+L'TLINHCOM-TLINPCD),KEYSAVE                         
         BNE   CITN40                                                           
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
         USING TLIND,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVC   IAGY,TLINAGY        SAVE AGENCY                                  
         SPACE                                                                  
         USING TAPDD,R4                                                         
CITN20A  L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL            R4=A(PAYMENT DETAILS ELEMENT)                
         BNE   CITN10                                                           
         OC    FINVOICE,FINVOICE                                                
         BNZ   CITN21                                                           
         CLC   TAPDUSE,=C'CLA'     HAS TO BE FOR CLASS A USE                    
         BNE   CITN10                                                           
         CLC   TAPDCYCS,SVCYCLE    AND IF CYCLES ARE IDENTICAL                  
         BNE   CITN21                                                           
         CLC   TAPDSTUS,SVSTUS     THEN MUST ALSO HAVE LOWER                    
         BNL   CITN10              STARTING USE                                 
         SPACE                                                                  
CITN21   XC    FINVOICE,FINVOICE                                                
         MVC   SVSTAT,TAPDPST1     SAVE STATUS                                  
         DROP  R4                                                               
         SPACE                                                                  
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      TREAT CANCELLER INVOICES                     
         BRAS  RE,GETEL            LIKE CREDITS                                 
         BNE   CITN10                                                           
         TM    TAINSTAT,TAINSCIN                                                
         BZ    CITN21A                                                          
         OI    SVSTAT,TAPDPCRD                                                  
         DROP  R4                                                               
         SPACE 1                                                                
CITN21A  CLI   VERSTAT,0           IF INVOICE BEING LISTED                      
         BE    CITN25              IS FOR A VERSION                             
         USING TAVRD,R4                                                         
         L     R4,AIO              THEN THIS INVOICE RECORD                     
         MVI   ELCODE,TAVRELQ      MUST MATCH THAT VERSION                      
         BRAS  RE,GETEL                                                         
         BE    CITN21B                                                          
         CLI   VERSTAT,1                                                        
         BNE   CITN10                                                           
         B     CITN25                                                           
CITN21B  CLC   TAVRVERS,VERSTAT                                                 
         BNE   CITN10                                                           
         SPACE                                                                  
CITN25   MVI   ANYPRGS,C'N'                                                     
         SPACE                                                                  
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,GETEL            READ ALL PROGRAM ELEMENTS                    
         B     *+8                                                              
CITN30   BRAS  RE,NEXTEL                                                        
         BNE   CITN35                                                           
         MVI   ANYPRGS,C'Y'                                                     
         SPACE                                                                  
         USING TANPD,R4                                                         
         CLI   TANPNWK,C'I'        IF PROGRAM IS ITN                            
         BNE   CITN30                                                           
         CLC   TANPDATE,SVCYCLE    AND DATE FITS WITHIN CYCLE                   
         BL    CITN30                                                           
         CLC   TANPDATE,SVCYCLE+3                                               
         BH    CITN30                                                           
         LH    RE,ITNCNT                                                        
         TM    SVSTAT,TAPDPCRD     DECREMENT ITN COUNTER                        
         BZ    *+12                IF CREDIT INVOICE                            
         SHI   RE,1                                                             
         B     *+8                                                              
         AHI   RE,1                OTHERWISE                                    
         STH   RE,ITNCNT           INCREMENT ITN COUNTER                        
         SPACE                                                                  
         CLI   VERSTAT,0           ALSO                                         
         BNE   CITN30                                                           
         CLI   TANPLFT,0           IF PROGRAM PAID TO LIFT                      
         BE    CITN30                                                           
         LH    RE,ITNLCNT                                                       
         TM    SVSTAT,TAPDPCRD     DECREMENT ITN LIFT COUNTER                   
         BZ    *+12                IF CREDIT INVOICE                            
         SHI   RE,1                                                             
         B     *+8                                                              
         AHI   RE,1                OTHERWISE                                    
         STH   RE,ITNLCNT          INCREMENT ITN LIFT COUNTER                   
         B     CITN30                                                           
         SPACE                                                                  
CITN35   CLI   ANYPRGS,C'Y'        IF NO PROGRAM ELEMENTS ARE                   
         BE    CITN10              FOUND ON CREDIT INVOICE                      
         TM    SVSTAT,TAPDPCRD                                                  
         BZ    CITN10                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTINV))                                     
         BNE   CITN10                                                           
         L     R4,TGELEM                                                        
         USING TANUD,R4            GET THE NUMBER OF THE                        
         MVC   CINVOICE,TANUMBER   INVOICE THATS BEING CREDITED                 
         XC    CINVOICE,=6X'FF'                                                 
         CLC   CINVOICE,MINVOICE                                                
         BE    CITN10                                                           
         DROP  R4                                                               
         MVC   SVINVKY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY              READ THE INVOICE RECORD THATS                
         USING TLIND,R4            BEING CREDITED                               
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,IAGY                                                     
         MVC   TLININV,CINVOICE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLININV+L'TLININV-TLIND),KEYSAVE                             
         BNE   CITN39                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         L     R4,AIO              R4=A(CREDITED INVOICE RECORD)                
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,GETEL            READ ALL PROGRAM ELEMENTS                    
         B     *+8                                                              
CITN36   BRAS  RE,NEXTEL                                                        
         BNE   CITN39                                                           
         SPACE                                                                  
         USING TANPD,R4                                                         
         CLC   TANPDATE,SVCYCLE    IF DATE FITS WITHIN CYCLE                    
         BL    CITN36                                                           
         CLC   TANPDATE,SVCYCLE+3                                               
         BH    CITN36                                                           
         CLI   TANPNWK,C'I'        AND PROGRAM IS ITN                           
         BNE   CITN36                                                           
         LH    RE,ITNCNT                                                        
         SHI   RE,1                                                             
         STH   RE,ITNCNT           DECREMENT ITN COUNTER                        
         SPACE                                                                  
         CLI   VERSTAT,0           ALSO                                         
         BNE   CITN36                                                           
         CLI   TANPLFT,0           IF PROGRAM PAID TO LIFT                      
         BE    CITN36                                                           
         LH    RE,ITNLCNT                                                       
         SHI   RE,1                                                             
         STH   RE,ITNLCNT          DECREMENT ITN LIFT COUNTER                   
         B     CITN36                                                           
         SPACE                                                                  
CITN39   MVC   KEY,SVINVKY                                                      
         GOTO1 HIGH                                                             
         B     CITN10                                                           
         SPACE                                                                  
CITN40   LH    RE,ITNCNT           IF TOTAL IS A NEGATIVE #                     
         CHI   RE,0                REPORT IT AS ZERO                            
         BNL   CITN41                                                           
         XC    ITNCNT,ITNCNT                                                    
         SPACE                                                                  
CITN41   LH    RE,ITNLCNT          IF TOTAL IS A NEGATIVE #                     
         CHI   RE,0                REPORT IT AS ZERO                            
         BNL   CITN45                                                           
         XC    ITNCNT,ITNLCNT                                                   
         SPACE                                                                  
CITN45   LA    R5,LISUSE           POINT R5 AT FIRST TWO                        
CITN46   CLC   0(2,R5),SPACES      CONTIGUOUS SPACES IN USE FIELD               
         BE    CITN47                                                           
         LA    R5,1(R5)                                                         
         B     CITN46                                                           
         SPACE                                                                  
CITN47   MVC   1(2,R5),=C'I='      PRINT TOT ITN USES IN CYCLE                  
         EDIT  ITNCNT,(3,3(R5)),ZERO=NOBLANK,ALIGN=LEFT                         
         SPACE                                                                  
CITN48   CLI   VERSTAT,0                                                        
         BNE   CITN50                                                           
         CLI   LISLFT+1,C'Y'                                                    
         BNE   CITN50                                                           
         CLC   0(2,R5),SPACES      POINT R5 AT FIRST TWO                        
         BE    CITN49              CONTIGUOUS SPACES IN USE FIELD               
         LA    R5,1(R5)                                                         
         B     CITN48                                                           
         SPACE                                                                  
CITN49   MVC   1(3,R5),=C'IL='     PRINT LIFT ITN USES IN CYCLE                 
         EDIT  ITNLCNT,(3,4(R5)),ZERO=NOBLANK,ALIGN=LEFT                        
         SPACE                                                                  
CITN50   MVC   KEY,TIKEY           RESET KEY                                    
         GOTO1 HIGH                AND READ ORIGINAL RECORD BACK                
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,TIAREC                                                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
CITNX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SAVE COVERED HOLDING FEE VARIABLES                *         
***********************************************************************         
*                                                                               
SUCVRHLD NTR1  BASE=*,LABEL=*                                                   
         CLC   RECNUM,TWALREC      IF FIRST TIME DOING HISTORY/LIST             
         JNE   SVH10               FOR THIS COMMERCIAL                          
         GOTO1 FLDVAL,DMCB,(X'40',SINAGYH),(X'80',SINOPTSH)                     
         JE    SVH20                                                            
SVH10    XC    NCVRHLD,NCVRHLD     CLEAR COVERED HOLDING FEE VARIABLES          
         J     SVH30                                                            
*                                                                               
SVH20    GOTO1 FLDVAL,DMCB,(X'80',SINSELH),(X'80',SINLSTH)                      
         JE    SVH30               IF NOT FIRST TIME AND AN ENTRY               
         MVC   NCVRHLD,FCVRHLD     HAS BEEN SELECTED, ENSURE THAT               
*                                  CURRENT SCREEN WILL BE REDISPLAYED           
SVH30    XC    FCVRHLD,FCVRHLD                                                  
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD COVERED HOLDING FEES TABLE                  *         
***********************************************************************         
*                                                                               
BDCVRHLD NTR1  BASE=*,LABEL=*                                                   
         MVI   TGUSEQU,0           CLEAR LAST READ USE                          
*                                                                               
         USING CHTBLD,R3                                                        
         L     R3,ACVRHLD          R3=A(APPLIED CREDIT HISTORY TABLE)           
         MVI   0(R3),X'FF'         INITIALIZE TABLE                             
*                                                                               
         USING TLCAD,R4                                                         
         LA    R4,KEY              READ ALL OF THE COMMERCIAL'S                 
         XC    KEY,KEY             CAST RECORDS                                 
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TIFCOM                                                   
         GOTO1 HIGH                                                             
         J     BCH20                                                            
BCH10    GOTO1 SEQ                                                              
BCH20    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   XIT                                                              
         GOTO1 GETREC                                                           
         DROP  R4                                                               
*                                                                               
         USING TACRD,R4                                                         
         L     R4,AIO              READ APPLIED CREDIT HISTORY ELEMENTS         
         MVI   ELCODE,TACRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     BCH40                                                            
BCH30    BRAS  RE,NEXTEL                                                        
BCH40    JNE   BCH10                                                            
         TM    TACRSTAT,TACRHDLR   ONLY CONSIDER IF IT WAS ADDED                
         JZ    BCH30               BY A DEALER PAYMENT                          
*                                                                               
         L     R3,ACVRHLD          R3=A(APPLIED CREDIT HISTORY TABLE)           
BCH50    C     R3,ACVRHLDX         END OF TABLE?                                
         JL    *+6                                                              
         DC    H'00'               MAKE TABLE BIGGER                            
         CLI   0(R3),X'FF'         END OF DATA IN TABLE?                        
         JE    BCH70                                                            
         CLC   TACRSTRT,CHSTRT     SAME CYCLE START?                            
         JNE   BCH60                                                            
         CLC   TACREND,CHEND       SAME CYCLE END?                              
         JNE   BCH60                                                            
         CLC   TACRINV,CHINV       SAME INVOICE?                                
         JE    BCH30               DON'T NEED TO ADD AGAIN                      
BCH60    LA    R3,CHLNQ(R3)                                                     
         B     BCH50                                                            
*                                  SAVE A NEW ENTRY IN TABLE                    
BCH70    MVC   CHSTRT,TACRSTRT     CYCLE START                                  
         MVC   CHEND,TACREND       CYCLE END                                    
         MVC   CHINV,TACRINV       INVOICE                                      
         LA    R3,CHLNQ(R3)                                                     
         MVI   0(R3),X'FF'         MARK NEW END OF TABLE                        
         L     R3,ACVRHLD          R3 -> BEGINNING OF TABLE                     
         J     BCH30                                                            
         DROP  R3,R4                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO READ COVERED HOLDING FEES TABLE                   *         
***********************************************************************         
                                                                                
RDCVRHLD NTR1  BASE=*,LABEL=*                                                   
         OC    NCVRHLD,NCVRHLD   IF WE HAVE A CARRYOVER COVERED HOLDING         
         JZ    RCH30             FEE TO DISPLAY ...                             
                                                                                
         USING CHTBLD,R3                                                        
         L     R3,ACVRHLD        FIND IT IN COVERED HOLDING FEE TABLE           
RCH10    CLI   0(R3),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   NCVRHLD,0(R3)                                                    
         JE    RCH20                                                            
         LA    R3,CHLNQ(R3)                                                     
         J     RCH10                                                            
                                                                                
RCH20    MVC   FCVRHLD,NCVRHLD   SAVE THE FACT THAT THIS IS THE FIRST           
         XC    NCVRHLD,NCVRHLD   ENTRY ON SCREEN AND GO PROCESS IT              
         J     RCH70                                                            
                                                                                
***********************************************************************         
                                                                                
RCH30    CLI   TGUSEQU,UDLR      IF WE DO NOT HAVE A CARRYOVER, BUT             
         JNE   XIT               THIS PAYMENT IS FOR A DEALER ...               
         MVC   SVDLRINV,SVINV                                                   
         MVC   SVDLRKEY,TIKEY                                                   
         L     R3,ACVRHLD                                                       
RCH40    CLI   0(R3),X'FF'                                                      
         JNE   RCH50                                                            
         MVC   TIQSKEY,SVDLRKEY                                                 
         MVI   TIQSKEY+TLINHSEQ+1-TLINPD,1                                      
         J     XIT                                                              
*                                                                               
RCH50    CLC   SVDLRINV,CHINV    IF COVERED HOLDING FEE LINKED TO               
         JE    RCH70             THIS DEALER, GO PROCESS IT                     
RCH60    LA    R3,CHLNQ(R3)                                                     
         J     RCH40                                                            
                                                                                
***********************************************************************         
                                                                                
RCH70    CLC   LISTNUM,NLISTS    IF WE HAVE ROOM TO DISPLAY THIS                
         JE    RCH90             COVERED HOLDING FEE ...                        
                                                                                
         XC    KEY,KEY           ENSURE DLR INVOICE IS IN TIAREC                
         MVC   KEY(L'TLINPKEY),SVDLRKEY                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLINPKEY),KEYSAVE                                          
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,TIAREC                                                       
         GOTO1 GETREC                                                           
                                                                                
         USING LISTD,R2                                                         
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         JE    RCH80                                                            
         LA    R2,P                                                             
RCH80    MVC   0(L'LISTAR,R2),SPACES                                            
                                                                                
         GOTO1 TINVCON,DMCB,SVDLRINV,LISCODE,DATCON                             
         GOTO1 DATCON,DMCB,(X'11',CHSTRT),(8,LISCYCS)                           
                                                                                
         USING TAIND,R4                                                         
         L     R4,TIAREC         R4=A(INVOICE RECORD)                           
         MVI   ELCODE,TAINELQ    GET INVOICE STATUS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'             DISPLAY PAY DATE                               
         GOTO1 DATCON,DMCB,(1,TAINPDTE),(8,LISPAY)                              
         DROP  R4                                                               
                                                                                
         MVC   LISUSE(26),=C'DEALER-COVERED HOLDING FEE'                        
                                                                                
         L     R2,ATHISLST       MOVE NEXT LINE TO SCREEN W/O LISTMON           
         ZIC   RE,0(R2)                                                         
         AR    R2,RE             BUMP TO NEXT DETAIL LINE                       
         OI    6(R2),X'80'                                                      
         LA    R2,8(R2)          BUMP PAST HEADER AND CLEAR                     
         XC    LISAGY(L'SININV),LISAGY                                          
                                                                                
         MVC   LISTPAY+8(4),=C'0.00'                                            
         GOTO1 LISTMON                                                          
                                                                                
***********************************************************************         
                                                                                
         CLC   LISTNUM,NLISTS    IF THIS IS THE END OF PAGE                     
         JNE   RCH60                                                            
         MVI   TOTTYPE,TOTTYRU   DISPLAY THE TOTALS                             
         BRAS  RE,DISPTOT2                                                      
         OI    GENSTAT7,GES7BACK                                                
         GOTO1 LISTMON                                                          
                                                                                
         LA    R3,CHLNQ(R3)      BUMP TO NEXT COVERED HOLDING FEE               
                                                                                
RCH90    CLI   0(R3),X'FF'       IF MORE COVERED PAYMENTS EXIST                 
         JE    RCH100            FOR THIS DEALER INVOICE                        
         CLC   SVDLRINV,CHINV                                                   
         JNE   RCH100                                                           
         MVC   NCVRHLD,0(R3)     NEXT LIST SCREEN SHOULD START WITH             
         J     RCH110            THE NEXT COVERED HOLDING FEE                   
         DROP  R3                                                               
                                                                                
         USING TLINPD,R4                                                        
RCH100   LA    R4,KEY            IF NO MORE PAYMENTS EXIST FOR THIS             
         XC    KEY,KEY           COMMERCIAL                                     
         MVC   TLINPKEY,SVDLRKEY START ANEW WITH NEXT HIT OF ENTER              
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   TLINHCOM,SVDLRKEY+TLINHCOM-TLINPD                                
         BE    RCH110                                                           
         NI    SINAGYH+4,X'DF'                                                  
         MVC   MYMSGNO,=H'109'                                                  
         J     RCH120                                                           
         DROP  R4                                                               
                                                                                
RCH110   MVC   TIQSKEY,SVDLRKEY                                                 
         MVI   TIQSKEY+TLINHSEQ+1-TLINPD,1                                      
         MVC   TIKEY,TIQSKEY                                                    
         MVC   MYMSGNO,=H'246'                                                  
RCH120   MVI   MYMTYP,GTMINF                                                    
         MVI   BLOCK,0                                                          
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE PUTS OUT SUBTOTAL FOR SCREENS                            
*                                                                               
         SPACE                                                                  
DISPTOT2 NTR1  BASE=*,LABEL=*                                                   
         XC    BYTE,BYTE           CLEAR BYTE FOR TOTCNTL                       
         CLI   TOTTYPE,TOTTYRU                                                  
         BE    DT205                                                            
         MVI   BYTE,C'E'           IF END OF TOTALS - SET BYTE                  
         SPACE                                                                  
DT205    GOTO1 TOTCNTL,DMCB,(BYTE,TOT1)                                         
         SPACE                                                                  
         USING LTOTD,R2                                                         
         LA    R2,SINTOTH                                                       
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         LA    R2,P+11                                                          
         SPACE 1                                                                
         MVC   LPNHNM,LTPNH2                                                    
         L     R3,TOT3                                                          
         LA    R5,LTOTPNH                                                       
         BAS   RE,EDIT11LB         DISPLAY PNH                                  
         MVC   LTNHNM,LTTNH2                                                    
         L     R3,TOT2                                                          
         LA    R5,LTOTTNH                                                       
         BAS   RE,EDIT11LB         DISPLAY TNH                                  
         SPACE                                                                  
         L     R3,TOT4                                                          
         LA    R5,LTOTPAY                                                       
         BAS   RE,EDIT12B          DISPLAY PAY                                  
         L     R3,TOT1                                                          
         LA    R5,LTOTTOT                                                       
         BAS   RE,EDIT12B          DISPLAY INVOICE TOTAL                        
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   DT2X                                                             
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT OUT                                 
         J     XIT                                                              
         SPACE                                                                  
DT2X     OI    6(R2),X'80'         TRANSMIT FIELD AGAIN                         
         J     XIT                                                              
*                                                                               
LTPNH2   DC    C'P&&H='                                                         
LTTNH2   DC    C'T&&H='                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*        EDIT AMOUNT IN R3 TO LOCATION IN R5 - ALIGN = LEFT                     
EDIT11LB DS    0H                                                               
         EDIT  (R3),(11,(R5)),2,FLOAT=-,ALIGN=LEFT                              
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*        EDIT AMOUNT IN R3 TO LOCATION IN R5                                    
EDIT12B  DS    0H                                                               
         EDIT  (R3),(12,(R5)),2,FLOAT=-                                         
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TASUDETS                                                       
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LISTD    DSECT                                                                  
LISCODE  DS    CL6                                                              
         DS    CL1                                                              
LISCYCS  DS    CL8                                                              
LISCYCD  DS    CL1                                                              
LISCYCE  DS    CL8                                                              
         DS    CL1                                                              
LISLFT   DS    CL3                                                              
         DS    CL1                                                              
LISBNP   DS    CL1                                                              
         DS    CL3                                                              
LISCRD   DS    CL1                                                              
         DS    CL1                                                              
LISPAY   DS    CL8                                                              
         DS    CL1                                                              
LISUSE   DS    CL33                                                             
         DS    CL6                 HEADER FOR SECOND DETAIL LINE                
LISPFAGY DS    CL6                 AGENCY FOR PF TABLE                          
         ORG   LISTD                                                            
LISAGY   DS    CL6                 AGENCY                                       
         DS    CL1                                                              
LISCOMM  DS    CL40                COMMENT                                      
         ORG   LISCOMM                                                          
LISCOMS  DS    CL31                SHORTENED COMMENT                            
         DS    CL1                                                              
LISUSCN  DS    CL8                 US/CAN LINKED INVOICE                        
         ORG   LISUSCN                                                          
LISEURO  DS    CL10                EURO INDICATOR                               
         DS    CL1                                                              
LISTPAY  DS    CL12                WAGES                                        
LISTOT   DS    CL12                INVOICE TOTAL                                
         SPACE                                                                  
LTOTD    DSECT                                                                  
         DS    CL8                                                              
         DS    CL1                                                              
LPNHNM   DS    CL4                                                              
LTOTPNH  DS    CL11                                                             
LTNHNM   DS    CL4                                                              
LTOTTNH  DS    CL11                                                             
LTOTPAY  DS    CL12                                                             
LTOTTOT  DS    CL12                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR47D                                                       
         EJECT                                                                  
WORKD    DSECT                                                                  
ACVRHLD  DS    A                   A(COVERED HOLDING FEES TABLE)                
ACVRHLDX DS    A                   A(COVERED HOLDING FEES TABLE END)            
SVDLRINV DS    XL(L'TGINV)         SAVED DEALER INVOICE NUMBER                  
SVDLRKEY DS    XL(L'KEY)           SAVED DEALER INVOICE KEY                     
*                                                                               
FCVRHLD  DS    XL(CHLNQ)           FIRST COVERED HLD DISPLAYED                  
NCVRHLD  DS    XL(CHLNQ)           NEXT COVERED HLD TO DISPLAY                  
*                                                                               
TABLE    DS    16CL(L'TLRCKEY)     PAGING TABLE                                 
COUNTER  DS    PL4                 COUNTER OF NUM OF RECORDS OUTPUT             
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
INV      DS    CL6                 WORK SPACE FOR INVOICE                       
MYBLOCK  DS    CL(L'SININV+8+1)    USED FOR CHAROUT                             
HISCOMM  DS    CL1                 Y=HISTORY COMMENT RECORD                     
PDAGY    DS    CL(L'TGAGY)         PAID AGENCY IF DIFFERENT FROM GLOBAL         
AGYNAME  DS    CL16                AGENCY NAME                                  
*                                                                               
INVBILL  DS    CL1                 C'Y' - INVOICE WAS BILLED                    
VERCODE  DS    CL1                 VERSION CODE                                 
LASTINV  DS    CL6                 LAST INVOICE TO DISPLAY                      
*                                                                               
TOTTYPE  DS    CL1                                                              
TOTTYRU  EQU   C'R'                RUNNING TOTAL                                
TOTTYEN  EQU   C'E'                END TOTAL                                    
*                                                                               
BDTOT    DS    F                   INVOICE TOTAL                                
BDTNH    DS    F                           BDTNH+                               
PDPNH    DS    F                           P & H                                
PDPAY    DS    F                           PDPAY+                               
*                                                                               
TOT1     DS    F                   INVOICE TOTAL                                
TOT2     DS    F                   PNH TOTAL                                    
TOT3     DS    F                   PAY+ TOTAL                                   
TOT4     DS    F                   TNH TOTAL                                    
TOTLNQ   EQU   *-TOT1                                                           
*                                                                               
SVCYCLE  DS    XL6                                                              
SVSTUS   DS    H                                                                
ITNCNT   DS    H                                                                
ITNLCNT  DS    H                                                                
SVSTAT   DS    XL1                                                              
VERSTAT  DS    XL1                                                              
CYCSTAT  DS    XL1                                                              
IAGY     DS    CL6                                                              
ANYPRGS  DS    XL1                                                              
CINVOICE DS    XL6                                                              
MINVOICE DS    XL6                                                              
FINVOICE DS    XL6                                                              
SVINVKY  DS    XL32                                                             
SVINV    DS    XL6                                                              
USCAN    DS    CL2                                                              
USCANINV DS    XL6                                                              
FIRSTINV DS    CL6                                                              
SVPDPST2 DS    XL(L'TAPDPST2)                                                   
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
         SPACE 3                                                                
* TASYSDSECT                                                                    
* DDPERVALD                                                                     
* FATIOB                                                                        
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
***********************************************************************         
*        DSECT FOR APPLIED CREDIT HISTORY TABLE                       *         
***********************************************************************         
*                                                                               
CHTBLD   DSECT                                                                  
CHSTRT   DS      XL3             CYCLE START DATE                               
CHEND    DS      XL3             CYCLE END DATE                                 
CHINV    DS      XL6             INVOICE NUMBER                                 
CHLNQ    EQU     *-CHTBLD                                                       
CHTBLNQ  EQU     (CHLNQ*100)+1                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049TAGEN47   09/22/14'                                      
         END                                                                    
