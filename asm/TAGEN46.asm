*          DATA SET TAGEN46    AT LEVEL 032 AS OF 05/30/12                      
*PHASE T70246A,*                                                                
         TITLE 'T70246 - INVOICE LIST'                                          
T70246   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70246                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE                                                                  
         GOTO1 INITIAL,DMCB,(X'20',PFTAB)                                       
         CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   INVL20                                                           
         BAS   RE,INIT                                                          
         B     XIT                                                              
         SPACE 3                                                                
INVL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   INVL30                                                           
         MVI   NLISTS,16           GET CONTROL BACK AT END OF PAGE              
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
INVL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LSTREC                                                           
         EJECT                                                                  
LSTREC   LA    R3,KEY              R3=A(KEY)                                    
         USING TLINPD,R3                                                        
         GOTO1 SETLSTK,DMCB,('TLINBCDQ',KEY)                                    
         SPACE                                                                  
         OC    KEY,KEY             IF FIRST TIME IN                             
         BNZ   LSTR10                                                           
         MVI   TLINPCD,TLINBCDQ    BUILD KEY                                    
         CLI   SINAGYH+5,0         OPTIONAL START AGENCY                        
         BE    *+10                                                             
         MVC   TLINBAGY,SINAGY                                                  
         CLI   SININVH+5,0         OPTIONAL START INVOICE                       
         BE    *+10                                                             
         MVC   TLINBINV,STRTINV                                                 
         SPACE                                                                  
LSTR10   GOTO1 HIGH                                                             
         B     LSTR30                                                           
         SPACE                                                                  
LSTR20   GOTO1 SEQ                 GET NEXT RECORD                              
         SPACE                                                                  
LSTR30   CLC   TLINPKEY(1),KEYSAVE  TEST STILL HAVE GOOD KEY                    
         BNE   LSTR40                                                           
*                                                                               
         CLI   RECNUM,DI           IF REGULAR INVOICE/LIST                      
         BE    LSTR34              DO NOT LIST DELETES                          
         TM    KEY+TLDRSTAT-TLDRD,TLINSDEL                                      
         BO    LSTR20                                                           
         B     LSTR35                                                           
*                                                                               
LSTR34   TM    KEY+TLDRSTAT-TLDRD,TLINSDEL IF DINVOICE/LIST                     
         BZ    LSTR20                      ONLY LIST DELETES                    
*                                                                               
LSTR35   BAS   RE,FILTOFF          FILTER BY OFFICE IF NEEDED                   
         BNE   LSTR10                                                           
*                                                                               
         BAS   RE,LIMITCHK         CHECK LIMIT ACCESS                           
         BNE   LSTR20                                                           
         BAS   RE,CHKSTAT          FILTER ON INVOICE STATUS                     
         BNE   LSTR20                                                           
         SPACE                                                                  
         GOTO1 GETREC              GET RECORD                                   
         BAS   RE,CHKSTAT2         CHECK THAT CHECKS NOT WRITTEN YET            
         BNE   LSTR20                                                           
         BAS   RE,CHKDUE           CHECK DUE DATE                               
         BNE   LSTR20                                                           
         BAS   RE,CHKTAPD          CHECK THINGS IN TAPD EL.                     
         BNE   LSTR20                                                           
         BAS   RE,DISPLAY                                                       
         B     LSTR20                                                           
         SPACE                                                                  
LSTR40   MVI   NLISTS,15           RESET AT END OF LIST                         
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   XIT                                                              
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
*              FILTER BY OFFICE                                                 
*                                                                               
FILTOFF  NTR1                                                                   
         CLI   SINOFF,C' '         OFFICE FILTER <> C' '                        
         BNH   YES                                                              
         CLC   LASTAGY,TLINBAGY    READ AGENCY RECORD FOR THIS ALREADY          
         BE    YES                                                              
         MVC   LASTAGY,TLINBAGY                                                 
         MVC   SAVKEY,KEY          SAVE KEY TO READ AGENCY RECORD               
         LA    R3,SAVKEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING TLAYD,R2                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,TLINBAGY                                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLI   TLAYCD,TLAYCDQ      STILL AGENCY RECORD                          
         BNE   FILTOFFN                                                         
         CLC   TLAYAGY,TLINBAGY    AND AGENCY MATCHES FILTER                    
         BNE   FILTOFFN                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ      AGENCY ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   FILTOFFN                                                         
         CLC   TAAYTPOF,SINOFF     COMPARE OFFICES                              
         BNE   FILTOFFN                                                         
*                                                                               
FILTOFFY MVC   KEY,SAVKEY          RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     YES                                                              
FILTOFFN XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVC   KEY(TLINBINV-TLINPD),SAVKEY                                      
         SR    R1,R1                                                            
         IC    R1,TLINBAGY+5                                                    
         AHI   R1,1                                                             
         STC   R1,TLINBAGY+5                                                    
         B     NO                                                               
         EJECT                                                                  
*                                                                               
*              PROCESS RECORD IN AIO                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
         USING TLIND,R3                                                         
DISPLAY  NTR1                                                                   
         L     R3,AIO                                                           
         MVC   LISAGY,TLINAGY      AGENCY                                       
         SPACE 1                                                                
         MVC   DUB,TLININV                                                      
         XC    DUB(6),=6X'FF'      UNCOMPLEMENT FOR DISPLAY                     
         GOTO1 TINVCON,DMCB,DUB,LISINV,DATCON CONVERT INVOICE NUMBER            
         SPACE                                                                  
         LR    R4,R3                                                            
         MVI   ELCODE,TARAELQ      GET BOVER INDICATOR FROM                     
         BAS   RE,GETEL            TARAELQ                                      
         BNE   DISP4                                                            
         USING TARAD,R4                                                         
         OC    TARASTA2,TARASTA2                                                
         BZ    *+8                                                              
         MVI   LISBOVER,C'B'                                                    
         OC    TARASTA3,TARASTA3                                                
         BZ    *+8                                                              
         MVI   LISBOVER,C'B'                                                    
*                                                                               
DISP4    LR    R4,R3                                                            
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMNT                   
         BAS   RE,GETEL                                                         
         BNE   DISP5                                                            
         SPACE                                                                  
         USING TAPDD,R4                                                         
         TM    TAPDSTAT,TAPDSLFT   LIFT PAYMENT                                 
         BZ    *+8                                                              
         MVI   LISLFT,C'Y'                                                      
         TM    TAPDSTA2,TAPDSLFA   PAY ALL ON COMM'L                            
         BZ    *+8                                                              
         MVI   LISLFT,C'A'                                                      
         TM    TAPDPST1,TAPDPBNP   BILL-NO-PAYROLL PAYMENT                      
         BZ    *+8                                                              
         MVI   LISBNP,C'Y'                                                      
         TM    TAPDPST1,TAPDPCRD   CREDIT PAYMENT                               
         BZ    *+8                                                              
         MVI   LISCR,C'Y'                                                       
         SPACE                                                                  
DISP5    MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         LR    R4,R3                                                            
         BAS   RE,GETEL                                                         
         BNE   DISP7               NO COMMERCIAL ELEMENT                        
*                                                                               
         USING TACOD,R4                                                         
         MVC   LISCID,TACOCID      COMMERCIAL ID                                
         B     DISP7                                                            
DISP6    CLI   SINTIME,C'Y'        IF T-SHEETS=Y                                
         BNE   DISP7                                                            
         BAS   RE,DISPTMCO                                                      
         SPACE                                                                  
DISP7    LR    R4,R3                                                            
         MVI   ELCODE,TAINELQ      POINT TO INVOICE STATUS ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAIND,R4                                                         
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(8,LISBILL) BILL DATE                   
         SPACE                                                                  
         MVC   LISPAYR,TAINPST     PAYER STAFF                                  
         TM    TAINSTAT,TAINSAPR   IF APPROVED                                  
         BZ    *+10                                                             
         MVC   LISAPPR,TAINQST     SHOW APPROVER                                
         CLI   TAINTERR,0                                                       
         BE    DISP8                                                            
         EDIT  (1,TAINTERR),(2,LISERR),ALIGN=LEFT                               
         SPACE                                                                  
DISP8    CLI   RECNUM,DI                                                        
         BE    DISP9                                                            
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTTTL COMMERCIAL TITLE                 
         MVC   LISTITLE,TGNAME                                                  
         SPACE                                                                  
         TM    TAINSTAT,TAINSPAY   IF NOT PAID                                  
         BO    DISP9B                                                           
         MVC   LISIST,TAINIST      SHOW ASSIGNER                                
         GOTO1 DATCON,DMCB,(1,TAINIDTE),(8,LISIDTE)  AND ASSIGN DATE            
         B     DISP9B                                                           
         SPACE                                                                  
         USING TAACD,R4                                                         
DISP9    LR    R4,R3               IF DELETED INVOICE                           
         MVI   ELCODE,TAACELQ      FIND ACTIVITY ELEMENT                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP9A   BAS   RE,NEXTEL                                                        
         BNE   DISP9B                                                           
         CLI   TAACSCR,X'1F'       FOR THE DELETE                               
         BNE   DISP9A                                                           
         MVC   LISIST,TAACSTAF     SHOW DELETER AND DELETE DATE                 
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,LISIDTE)                             
         DROP  R4                                                               
         SPACE 1                                                                
DISP9B   CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   DISP10                                                           
         GOTO1 CATCHIOS            ENSURE IO OVERUSE FOR REPORTS                
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT LINES OUTPUT                           
         B     XIT                                                              
         SPACE                                                                  
         USING TLDRD,R3                                                         
DISP10   LA    R3,KEY                                                           
         MVC   DMDSKADD,TLDRDA     PASS DISK ADDRESS TO LISTMON                 
         CLI   LISTNUM,15                                                       
         BE    ENDPAGE             GET OUT IF ALREADY FILLED PAGE               
         GOTO1 LISTMON             CALL LISTMON                                 
DISPX    MVC   LISTAR,SPACES                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND SETS UP KEY                     
         SPACE                                                                  
INIT     NTR1                                                                   
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         LA    R2,SINAGYH                                                       
         TM    4(R2),X'20'         IF AGENCY NOT PREV VALIDATED                 
         BO    INIT3                                                            
         MVI   ALLVAL,C'N'                                                      
         NI    SININVH+4,X'DF'     MUST VALIDATE START INVOICE FIELD            
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   SININVH+5,0         IF INVOICE INPUT, REQUIRE AGY                
         BNE   FLDMISS                                                          
         B     INIT3                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2) VALIDATE AGENCY                         
         MVC   SINAGY,TGAGY        MOVE PADDED AGY BACK TO FLD                  
         SPACE                                                                  
INIT3    LA    R2,SININVH                                                       
         TM    4(R2),X'20'         IF START AT FIELD NOT PREV VALIDATED         
         BO    INIT7                                                            
         MVI   ALLVAL,C'N'                                                      
         CLI   5(R2),0             IF THERE'S START INPUT                       
         BE    INIT6                                                            
         GOTO1 TINVCON,DMCB,SININV,STRTINV,DATCON CONVERT FOR KEY               
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    STRTINV,=6X'FF'     COMPLEMENT FOR INVOICE RECORD                
INIT6    OI    4(R2),X'20'         SET PREV VALIDATED                           
*                                                                               
INIT7    TM    SINOFFH+4,X'20'     OFFICE FILTER PREV VALIDATED?                
         BO    INIT8                                                            
         MVI   ALLVAL,C'N'                                                      
         OI    SINOFFH+4,X'20'                                                  
*                                                                               
INIT8    BAS   RE,VALYN            VALIDATE YES,NO FIELDS                       
         CLI   ALLVAL,C'Y'         IF NOT ALL FIELDS PREV VALIDATED             
         BE    XIT                                                              
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO FIRST TIME IN          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES 9 YES OR NO FIELDS STARTING AT SINPDON         
         SPACE                                                                  
VALYN    NTR1                                                                   
         LA    R2,SINPDONH                                                      
         LA    R0,9                                                             
         SR    R1,R1                                                            
         LA    RF,SINDUEH          ALLOW 'U' IN DUE TODAY                       
         LA    RE,SINHLDH          ALLOW 'R' IN HOLD                            
         SPACE                                                                  
VALYN5   TM    4(R2),X'20'         IF FIELD NOT PREV VALIDATED                  
         BO    VALYN10                                                          
         MVI   ALLVAL,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    VALYN9                                                           
         CLI   8(R2),C'Y'                                                       
         BE    VALYN9                                                           
         CLI   8(R2),C'N'                                                       
         BE    VALYN9                                                           
         CLI   8(R2),C'U'                                                       
         BNE   *+14                                                             
         CR    R2,RF                                                            
         BNE   FLDINV                                                           
         B     VALYN9                                                           
         CLI   8(R2),C'R'                                                       
         BNE   FLDINV                                                           
         CR    R2,RE                                                            
         BNE   FLDINV                                                           
VALYN9   OI    4(R2),X'20'         SET PREV VALIDATED                           
VALYN10  IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R0,VALYN5                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE RETURNS CC EQUAL IF THE INVOICE STATUS MATCHES           
*              THE INPUTS, ELSE RETURNS CC NOT EQUAL TO SKIP RECORD             
*              R3=A(KEY)                                                        
         SPACE                                                                  
         USING TLINPD,R3                                                        
CHKSTAT  NTR1                                                                   
         CLI   SINPDON,C'Y'        IF PAID ONLINE? = Y                          
         BNE   CHKST2                                                           
         TM    TLINBST2,TAINSPAY                                                
         BZ    NO                  SKIP IF NOT PAID ONLINE                      
         B     CHKST4                                                           
CHKST2   CLI   SINPDON,C'N'        ELSE IF PAID ONLINE? = N                     
         BNE   CHKST4                                                           
         TM    TLINBST2,TAINSPAY                                                
         BO    NO                  SKIP IF PAID ONLINE                          
         SPACE                                                                  
CHKST4   CLI   SINAPP,C'Y'         IF APPROVED? = Y                             
         BNE   CHKST6                                                           
         TM    TLINBST2,TAINSAPR                                                
         BZ    NO                  SKIP IF NOT APPROVED                         
         B     CHKST8                                                           
CHKST6   CLI   SINAPP,C'N'         ELSE IF APPROVED = N                         
         BNE   CHKST8                                                           
         TM    TLINBST2,TAINSAPR                                                
         BO    NO                  SKIP IF APPROVED                             
         SPACE                                                                  
CHKST8   CLI   SINBILD,C'Y'        IF BILLED? = Y                               
         BNE   CHKST14                                                          
         TM    TLINBST2,TAINSBIL                                                
         BZ    NO                  SKIP IF NOT BILLED                           
         B     CHKST16                                                          
CHKST14  CLI   SINBILD,C'N'        ELSE IF BILLED? = N                          
         BNE   CHKST16                                                          
         TM    TLINBST2,TAINSBIL                                                
         BO    NO                  SKIP IF BILLED                               
         SPACE                                                                  
CHKST16  CLI   SINHLD,C'Y'         IF HOLD? = Y                                 
         BNE   CHKST18                                                          
         TM    TLINBST2,TAINSHLD                                                
         BZ    NO                  SKIP IF NOT HOLD                             
         B     CHKST20                                                          
CHKST18  CLI   SINHLD,C'N'         ELSE IF HOLD? = N                            
         BNE   CHKST19                                                          
         TM    TLINBST2,TAINSHLD                                                
         BO    NO                  SKIP IF HOLD                                 
         B     CHKST20                                                          
CHKST19  CLI   SINHLD,C'R'         ELSE IF HOLD? = R(ELEASED)                   
         BNE   CHKST20                                                          
         TM    TLINBST2,TAINSPAY                                                
         BZ    NO                  SKIP IF NOT PAID                             
         TM    TLINBST2,TAINSHLD+TAINSBIL                                       
         BNZ   NO                  SKIP IF STILL ON HOLD OR IF BILLED           
         SPACE                                                                  
CHKST20  CLI   SINCANC,C'Y'        IF CANCELLED? = Y                            
         BNE   CHKST22                                                          
         TM    TLINBST2,TAINSCAN+TAINSCIN                                       
         BZ    NO                  SKIP IF NOT CANCELLED                        
         B     CHKST24                                                          
CHKST22  CLI   SINCANC,C'N'        ELSE IF CANCELLED? = N                       
         BNE   CHKST24                                                          
         TM    TLINBST2,TAINSCAN+TAINSCIN                                       
         BNZ   NO                  SKIP IF CANCELLED                            
         SPACE                                                                  
CHKST24  CLI   SINERR,C'Y'         IF IN ERROR? = Y                             
         BNE   CHKST26                                                          
         TM    TLINBST2,TAINSERR                                                
         BZ    NO                  SKIP IF NOT IN ERROR                         
         B     YES                                                              
CHKST26  CLI   SINERR,C'N'         ELSE IF IN ERROR? = N                        
         BNE   YES                                                              
         TM    TLINBST2,TAINSERR                                                
         BO    NO                  SKIP IF IN ERROR                             
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TESTS SECONDARY STATUS BITS                              
*              AND ALSO TESTS IF INVOICE HAS TIMESHEETS                         
         SPACE                                                                  
         USING TAIND,R4                                                         
CHKSTAT2 NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         CLI   SINURG,C'Y'         IF URGENT? = Y                               
         BNE   CHKST2A                                                          
         TM    TAINSTA2,TAINSURG                                                
         BZ    NO                  SKIP IF NOT URGENT                           
CHKST2A  CLI   SINURG,C'N'         ELSE IF NOT URGENT? = N                      
         BNE   CHKST2B                                                          
         TM    TAINSTA2,TAINSURG                                                
         BO    NO                  SKIP IF IN ERROR                             
         SPACE                                                                  
CHKST2B  TM    TAINSTAT,TAINSCHK+TAINSBIL                                       
         BO    NO                  SET CC NE IF CHECKS WRITTEN & BILLED         
         SPACE 1                                                                
         CLI   SINHLD,C'R'         IF HOLD? = R(ELEASED)                        
         BNE   CHKST2C                                                          
         TM    TAINSTA2,TAINSHLR                                                
         BZ    NO                  SET CC NE IF INVOICE NOT RELEASED            
         SPACE                                                                  
CHKST2C  CLI   SINTIME,C'Y'        IF T-SHEETS? = YES                           
         BNE   CHK2STX                                                          
         OC    TAINTMCO,TAINTMCO                                                
         BZ    NO                                                               
*                                                                               
CHK2STX  B     YES                                                              
         SPACE 3                                                                
*              ROUTINE RETURNS CC EQUAL IF TODAY'S DATE                         
*              CORRESPONDS TO DUE TODAY INPUT                                   
         SPACE                                                                  
         USING TADDD,R4                                                         
CHKDUE   NTR1                                                                   
         CLI   SINDUEH+5,0         IF THERE'S INPUT IN DUE TODAY?               
         BE    YES                                                              
         L     R4,AIO                                                           
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         SPACE 1                                                                
         CLI   SINDUE,C'Y'         IF DUE TODAY? = Y                            
         BNE   CHKD4                                                            
         CLC   TADDDATE,TGNXTBUS   DUE MUST BE LE NEXT BUSINESS DAY             
         BH    NO                                                               
         B     YES                                                              
         SPACE 1                                                                
CHKD4    CLI   SINDUE,C'U'         IF DUE TODAY = U(RGENT)                      
         BNE   CHKD10                                                           
         CLC   TADDDATE,TGTODAY1   DUE MUST BE LE TODAY                         
         BH    NO                                                               
         B     YES                                                              
         SPACE 1                                                                
CHKD10   CLC   TADDDATE,TGNXTBUS   ELSE IF DUE TODAY? = N                       
         BNH   NO                  DUE MUST BE GT NEXT BUSINESS DAY             
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE RETURNS CC EQUAL IF NOT A SUBSIDIARY INVOICE             
*              AND NOT A DUMMY INVOICE                                          
         SPACE                                                                  
         USING TAPDD,R4                                                         
CHKTAPD  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
         TM    TAPDSTA2,TAPDSSUB                                                
         BO    NO                  SKIP IF SUBSIDIARY INVOICE                   
         TM    TAPDOPT3,TAPDODUM                                                
         BO    NO                  SKIP IF DUMMY PAYMENT                        
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS TIMESHEET COMMERCIAL ID IF T-SHEETS=Y           
*              AND INVOICE HASN'T BEEN PAID YET                                 
DISPTMCO NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         LR    R4,R3                                                            
         MVI   ELCODE,TAINELQ      INVOICE STATUS ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAIND,R4                                                         
         OC    TAINTMCO,TAINTMCO   GET TIMESHEET INTERNAL COMM ID               
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TAINTMCO)                            
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      COMMERCIAL DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DTMCO10                                                          
         USING TACOD,R4                                                         
         MVC   LISCID,TACOCID      DISPLAY COMMERCIAL ID                        
DTMCO10  MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY           RESTORE INVOICE RECORD                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLINKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE CHECKS LIMIT ACCESS RESTRICTIONS FOR AGENCY              
*              AND RETURNS CC EQUAL IF OK                                       
*              R3=A(KEY)                                                        
         SPACE                                                                  
         USING TLINPD,R3                                                        
LIMITCHK NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         JE    YES                                                              
                                                                                
         LHI   R2,1                                                             
                                                                                
         USING FAWSSVRD,R1                                                      
LIMCHK10 LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   NO                                                               
         DROP  R1                                                               
                                                                                
         AHI   R2,1                                                             
                                                                                
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
                                                                                
LIMCHK20 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    LIMCHK10                                                         
                                                                                
         CLC   TLINBAGY,TAVAAGY    IF AGENCY IS FOUND IN STAFF LIMITS           
         BE    YES                 ACCESS IS GRANTED                            
                                                                                
         ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     LIMCHK20                                                         
         DROP  R3                                                               
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SINSELH                                                       
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
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
         DC    CL3'HI',CL8'HISTORY ',CL8'DISPLAY'                               
PF13     DC    AL1(KEYTYCUR,L'LISAGY-1),AL2(LISAGY-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISINV-1),AL2(LISINV-LISTD)                       
         SPACE                                                                  
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'CH',CL8'CHECK   ',CL8'LIST   '                               
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LISAGY-1),AL2(LISAGY-LISTD)                       
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LISINV-1),AL2(LISINV-LISTD)                       
         SPACE                                                                  
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF21X-*,21,PFTINT+PFTCPROG,(PF21X-PF21)/KEYLNQ,0)            
         DC    CL3'A',CL8'INVOICE ',CL8'APPROVE'                                
PF21     DC    AL1(KEYTYCUR,L'LISAGY-1),AL2(LISAGY-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISINV-1),AL2(LISINV-LISTD)                       
         SPACE                                                                  
PF21X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF22X-*,22,PFTINT+PFTCPROG,(PF22X-PF22)/KEYLNQ,0)            
         DC    CL3'D ',CL8'INVOICE ',CL8'DELETE  '                              
PF22     DC    AL1(KEYTYCUR,L'LISAGY-1),AL2(LISAGY-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISINV-1),AL2(LISINV-LISTD)                       
         SPACE                                                                  
PF22X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF23X-*,23,PFTINT+PFTCPROG,(PF23X-PF23)/KEYLNQ,0)            
         DC    CL3'DE',CL8'INVOICE ',CL8'DELETE  '                              
PF23     DC    AL1(KEYTYCUR,L'LISAGY-1),AL2(LISAGY-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISINV-1),AL2(LISINV-LISTD)                       
         SPACE                                                                  
PF23X    EQU   *                                                                
         DC    AL1(PF24X-*,24,PFTINT,(PF24X-PF24)/KEYLNQ,0)                     
         DC    CL3'T',CL8'TIME    ',CL8'LIST   '                                
PF24     DC    AL1(KEYTYCUR,L'LISAGY-1),AL2(LISAGY-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISINV-1),AL2(LISINV-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISCID-1),AL2(LISCID-LISTD)                       
         SPACE                                                                  
PF24X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'INVOICE LIST'                                            
         SSPEC H2,33,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,1,C'AGENCY'                                                   
         SSPEC H4,8,C'INV #'                                                    
         SSPEC H4,15,C'BILL DTE'                                                
         SSPEC H4,24,C'COMML ID'                                                
         SSPEC H4,37,C'PAYR'                                                    
         SSPEC H4,42,C'APPR'                                                    
         SSPEC H4,47,C'L'                                                       
         SSPEC H4,49,C'CR'                                                      
         SSPEC H4,52,C'BNP'                                                     
         SSPEC H4,56,C'ER'                                                      
         SSPEC H4,59,C'TITLE'                                                   
         SPACE 1                                                                
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,8,C'-----'                                                    
         SSPEC H5,15,C'--------'                                                
         SSPEC H5,24,C'--------'                                                
         SSPEC H5,37,C'----'                                                    
         SSPEC H5,42,C'----'                                                    
         SSPEC H5,47,C'-'                                                       
         SSPEC H5,49,C'--'                                                      
         SSPEC H5,52,C'---'                                                     
         SSPEC H5,56,C'--'                                                      
         SSPEC H5,59,C'-----'                                                   
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE 1                                                                
LISTD    DSECT                                                                  
LISAGY   DS    CL6                                                              
         DS    CL1                                                              
LISINV   DS    CL6                                                              
LISBOVER DS    CL1                                                              
LISBILL  DS    CL8                                                              
         DS    CL1                                                              
LISCID   DS    CL12                                                             
         DS    CL1                                                              
LISPAYR  DS    CL4                                                              
         DS    CL1                                                              
LISAPPR  DS    CL4                                                              
         DS    CL1                                                              
LISLFT   DS    CL1                                                              
         DS    CL2                                                              
LISCR    DS    CL1                                                              
         DS    CL2                                                              
LISBNP   DS    CL1                                                              
         DS    CL2                                                              
LISERR   DS    CL2                                                              
         DS    CL1                                                              
LISTITLE DS    CL17                                                             
         ORG   LISTITLE                                                         
LISIST   DS    CL8                                                              
         DS    CL1                                                              
LISIDTE  DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR46D                                                       
         SPACE                                                                  
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
STRTINV  DS    PL6                 START INVOICE                                
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
SVKEY    DS    CL32                                                             
SAVKEY   DS    CL(L'KEY)           TEMPORARY KEY                                
LASTAGY  DS    CL6                 LAST AGENCY                                  
LIMBLK   DS    XL100               AGENCY/CLIENT LIMIT WORK BLOCK               
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032TAGEN46   05/30/12'                                      
         END                                                                    
