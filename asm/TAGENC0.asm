*          DATA SET TAGENC0    AT LEVEL 075 AS OF 10/06/14                      
*PHASE T702C0A                                                                  
*INCLUDE TWABLD                                                                 
         TITLE 'T702C0 - PAYMENT LIST'                                          
T702C0   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C0,R6,R5,RR=R2                                             
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(LOCAL SAVED STORAGE)                    
         USING LOCALD,R7                                                        
         ST    R2,RELO                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         OI    GLSTSTAT,NOSELFLD   NEED TO SET NO SEL FLD FOR TESTSEL           
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE PROGRAM - HANDLE PFKEYS           
*                                                                               
         CLI   MODE,SETFILE        SET ALTERNATE FILE                           
         BNE   *+12                                                             
         BAS   RE,SETCHK                                                        
         B     PAX                                                              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BNE   *+16                                                             
         BAS   RE,VKEY                                                          
         BAS   RE,SETCHK                                                        
         B     PAX                                                              
*                                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BNE   *+12                                                             
         BAS   RE,LVREC                                                         
         B     PAX                                                              
*                                                                               
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BNE   PA20                                                             
         GOTO1 AADDPTRS,DMCB,PTRBLK  UPDATE POINTERS                            
         BAS   RE,FTRACK           UPDATE FTRACK RECORD IF NECESSARY            
         BAS   RE,DUECOMP          UPDATE DUE COMPANY RECORD IF NEC.            
         L     R0,TIAREC                                                        
         MVC   TIAREC,AIO          SET A(UPDATED REC FOR DISPLAY RTN)           
         BAS   RE,DISPLAY          RE-DISPLAY THE RECORD                        
         ST    R0,TIAREC                                                        
         BAS   RE,PUTSCRN          WRITE NEW SCREEN RECORD                      
         BAS   RE,INVOICE          UPDATE INVOICE RECORD                        
         B     PAX                                                              
*                                                                               
PA20     CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PA30                                                             
         LA    R0,LRHOOK           SET A(SYSIO HOOK)                            
         BAS   RE,LREC                                                          
         B     PAX                                                              
*                                                                               
PA30     CLI   MODE,PRINTREP                                                    
         BNE   PAX                                                              
         XC    KEY,KEY             START REPORT FROM BEGINNING                  
         ZAP   COUNTER,=P'0'       LINE COUNTER                                 
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,MYSPECS          SET A(SPECS)                                 
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R0,HEADHOOK                                                      
         LA    R0,PRHOOK           SET A(SYSIO HOOK)                            
         BAS   RE,LREC                                                          
*                                                                               
PAX      B     XIT                                                              
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BE    *+8                                                              
         NI    SPYAGYH+4,X'DF'     SET TO RE-VALIDATE                           
*                                                                               
         LA    R2,SPYAGYH          AGENCY                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SPYINVH+4,X'DF'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),0                               
         MVC   TIFAGY,TGAGY                                                     
         MVC   AGYNAME,TGNAME                                                   
*                                                                               
VK10     LA    R2,SPYINVH          INVOICE NUMBER                               
         TM    4(R2),X'20'                                                      
         BO    VK80                                                             
         NI    SPYOTHH+4,X'DF'                                                  
         CLI   5(R2),0             IF NOTHING INPUT                             
         BNE   VK20                                                             
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)  TRY TO USE GLOBAL                 
         BNE   THEEND                                                           
         MVC   TIFINV,TGINV        MOVE INTO SYSIO FILTER                       
         XC    TIFINV,HEXFFS       UN-COMPLEMENT                                
         GOTO1 TINVCON,DMCB,TIFINV,8(R2),DATCON  CONVERT FOR DISPLAY            
         OI    6(R2),X'80'                                                      
         B     VK25                                                             
*                                                                               
VK20     GOTO1 TINVCON,DMCB,8(R2),TIFINV,DATCON  CONVERT FOR ACTIVE KEY         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         MVC   TGINV,TIFINV        MOVE INTO GLOBAL                             
         XC    TGINV,HEXFFS        COMPLEMENT FOR INVOICE RECORD                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)  VALIDATE RECORD EXISTS            
         BNE   THEEND                                                           
*                                                                               
VK25     MVI   ELCODE,TANUELQ      NOT ALLOWED FOR RETROS                       
         GOTO1 GETL,DMCB,(1,=AL1(TANURT4I))                                     
         BE    FLDINV                                                           
*                                                                               
         USING TAPDD,R4                                                         
VK30     L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      NOT ALLOWED FOR EVENT PAYMENTS               
         BAS   RE,GETEL                                                         
         BNE   VK35                                                             
         CLC   TAPDUSE,=C'EVE'                                                  
         BE    NOPYML                                                           
         DROP  R4                                                               
*                                                                               
VK35     L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            GET INVOICE STATUS ELEMENT                   
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         TM    TAINSTAT,TAINSPAY   INSURE INVOICE HAS BEEN PAID                 
         BZ    NOTPAID                                                          
*                                                                               
         BRAS  RE,EXTINV           EXTRACT INVOICE DATA                         
         OI    4(R2),X'20'         SET PREV. VALIDATED                          
*                                                                               
VK80     LA    R2,SPYOTHH          DISPLAY OTHERS OPTION                        
         TM    4(R2),X'20'                                                      
         BO    VK100                                                            
         XC    DISPDISP,DISPDISP   INSURE WE RE-BUILD SCREEN                    
         NI    OPTS,ALL-OPTOTH                                                  
         CLI   5(R2),0                                                          
         BE    VK90                                                             
         CLI   8(R2),C'N'                                                       
         BE    VK90                                                             
         CLI   8(R2),C'Y'          IF SET TO 'Y'ES                              
         BNE   FLDINV                                                           
         OI    OPTS,OPTOTH         TURN ON CORRES. OPTIONS BIT                  
VK90     OI    4(R2),X'20'         SET PREV. VALIDATED                          
*                                                                               
         BAS   RE,BLDSCRN          BUILD THE SCREEN                             
         XC    KEY,KEY             RESTART LIST - CLEAR KEY                     
*                                                                               
VK100    DS    0H                  SET I DISPLAY/RETURN EXTRA/NO SELECT         
         OI    GLSTSTAT,APPLCDSP+RETEXTRA+NOSELFLD                              
*                                                                               
         TM    INVSTAT,TAINSBIL+TAINSCHK  TEST NOT BILLED AND CHECKED           
         BNZ   VK105                                                            
         TM    PAYOPT5,TAPDONHW                                                 
         JO    VK103                                                            
*        TM    TGUSXUNI,AFM               TEST NOT AFM PAYMENT                  
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BZ    VK105                                                            
VK103    TM    INVSTAT2,TAINSRTH          IF NOT RETRO PAYMENT                  
         BO    VK104                                                            
         TM    INVSTAT,TAINSAPR           TEST NOT APPROVED                     
         BNZ   *+8                                                              
VK104    OI    GLSTSTAT,CHNGLIST          ELSE ALLOW CHANGES                    
*                                                                               
VK105    MVC   LLIST,LLINE         SET L'LINE                                   
         MVI   NLISTS,NLINES       N'LINES                                      
         LH    R1,DFRSTDAT                                                      
         AR    R1,RA                                                            
         ST    R1,AFRSTREC         A(FIRST DATA FIELD)                          
         LH    R1,DLASTDAT                                                      
         AR    R1,RA                                                            
         ST    R1,ALASTDAT         A(LAST DATA FIELD)                           
*                                                                               
         XC    TCARATES,TCARATES   CLEAR FOR SYSCALC EVERY HIT OF ENTER         
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD SCREEN BASED ON USE TYPE                        
         SPACE 1                                                                
BLDSCRN  NTR1                                                                   
         XC    WORK,WORK           BUILD TWABLD PARAMETER BLOCK                 
         LA    R3,WORK                                                          
         USING TWAPARMD,R3                                                      
         MVC   TWAPATWA,ATWA       A(TWA)                                       
*                                                                               
         BAS   RE,SETSCRN          SET A(SCREEN TABLE FOR BLDTWA)               
         BE    BLDSX               SCREEN ALREADY LOADED                        
*                                                                               
*                                  RETURNS A(BEG OF TABLE) IN FULL              
         LA    R4,BLOCK            RETURNS HEADINGS ELEMENT IN BLOCK            
         ST    R4,TWAPAFST         SET A(HEADINGS ELEMENT) FOR TWABLD           
         LA    R1,SPYTAGH                                                       
         ST    R1,TWAPAOUT         A(HEADINGS FIELD)                            
         BAS   RE,GOTWABLD         GO TO TWABLD                                 
*                                                                               
         L     R2,TWAPANXT         SAVE DISP. TO FIRST DATA FIELD               
         SR    R2,RA                                                            
         STH   R2,DFRSTDAT                                                      
*                                                                               
         LA    R0,NLINES           LOOP FOR N'LINES                             
*                                                                               
BLDS20   L     R4,FULL             BUMP PAST HEADING                            
         USING TWAELEMD,R4                                                      
         ZIC   R2,TWAELLN                                                       
         LA    R4,1(R2,R4)                                                      
         ST    R4,TWAPAFST         A(FIRST ELEMENT)                             
*                                                                               
         L     R2,TWAPANXT         A(NEXT FIELD)                                
         ST    R2,TWAPAOUT         IS NOW A(FIRST FIELD)                        
         BAS   RE,GOTWABLD         GO TO TWABLD                                 
*                                                                               
         MVI   ELCODE,0                                                         
         BAS   RE,NEXTEL           BUMP TO END OF BLOCK OF ELS.                 
         BE    *-4                                                              
         LA    R4,1(R4)            SKIP TO NEXT ELEMENT                         
         TM    OPTS,OPTOTH         IF DISPLAYING 'OTHER' FIELDS                 
         BZ    *+8                                                              
         LA    R4,OTHDAT+OTHDATQ   SWITCH TO 'OTHER' TABLE                      
*                                                                               
         ST    R4,TWAPAFST         SET A(FIRST ELEMENT)                         
         MVC   TWAPAOUT,TWAPANXT   SET A(FIRST FIELD) = A(NEXT FIELD)           
         BAS   RE,GOTWABLD         GO TO TWABLD                                 
         BCT   R0,BLDS20           LOOP                                         
*                                                                               
         L     R4,TWAPANXT         SAVE L'LIST LINE                             
         SR    R4,R2                                                            
         STH   R4,LLINE                                                         
         L     R4,TWAPANXT         SAVE DISP. TO LAST DATA FIELD                
         SR    R4,RA                                                            
         STH   R4,DLASTDAT                                                      
*                                                                               
         LA    R4,BOTTAB           NOW HANDLE BOTTOM OF SCREEN                  
         ST    R4,TWAPAFST         A(FIRST ELEMENT)                             
         MVC   TWAPAOUT,TWAPANXT   A(FIRST FIELD)                               
         BAS   RE,GOTWABLD         GO TO TWABLD                                 
*                                                                               
         CLI   TGCTSTTY,TASTTYPP   IF THIS IS PROGRAMMER                        
         BNE   BLDSX                                                            
         GOTO1 HEXOUT,DMCB,TWAPTLEN,SPYLTWA,2,0  DISPLAY L'TWA                  
         OI    SPYLTWAH+6,X'80'                                                 
*                                                                               
BLDSX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE PASSES CONTROL TO TWABLD ROUTINE TO BUILD SCREEN         
         SPACE 1                                                                
GOTWABLD NTR1                                                                   
         GOTO1 =V(TWABLD),TWAPARMD,RR=RELO                                      
         CLI   TWAPERRS,0                                                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*              ROUTINE SETS A(SCREEN TABLE FOR BLDTWA) BASED ON USE             
         SPACE 1                                                                
SETSCRN  NTR1                                                                   
         LA    R4,BSSTAB                                                        
         LA    R3,DISPBSS                                                       
         TM    TGUSSTA2,BSSTYPE    TV SESSION                                   
         BO    SETS50                                                           
         CLI   TGUSEQU,UBSC        BSC USES BSS SCREEN                          
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,BSMTAB           MUSIC SESSION                                
         LA    R3,DISPBSM                                                       
         CLI   TGUSEQU,UBSM                                                     
         BE    SETS50                                                           
         CLI   TGUSEQU,UIMS                                                     
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,BSRTAB           RADIO SESSION                                
         LA    R3,DISPBSR                                                       
         TM    TGUSSTAT,SESSION                                                 
         BZ    *+12                                                             
         CLI   TGMEEQU,RADIO                                                    
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,SOPTAB           SOAP                                         
         LA    R3,DISPSOP                                                       
         CLI   TGUSEQU,USOP                                                     
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,PRTTAB           PRINT                                        
         LA    R3,DISPPRT                                                       
         CLI   TGUSEQU,UPRS                                                     
         BE    SETS50                                                           
         CLI   TGUSEQU,UPRT                                                     
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,IVRTAB           INTERACTIVE VOICE RERECORD                   
         LA    R3,DISPIVR                                                       
         CLI   TGUSEQU,UIVR                                                     
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,DIOTAB           AUDIO RECORDING SESSION                      
         LA    R3,DISPDIO                                                       
         CLI   TGUSEQU,UDIO                                                     
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,RTKTAB           RETAKE                                       
         LA    R3,DISPRTK                                                       
         CLI   TGUSEQU,URTK                                                     
         BE    SETS50                                                           
         SPACE 1                                                                
         LA    R4,DEFTAB           DEFAULT                                      
         LA    R3,DISPDEF                                                       
         SPACE 1                                                                
SETS50   ST    R4,FULL             RETURN A(TABLE) IN FULL                      
         SPACE 1                                                                
         USING TWAELEMD,R4         MOVE 1ST EL (HEADINGS) TO BLOCK              
         ZIC   R1,TWAELLN          R1=L'ELEMENT                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),TWAELEMD   MOVE ELEMENT TO W/S (INCL. X'00')            
         SPACE 1                                                                
         TM    OPTS,OPTOTH         IF OTHER OPTION REQUESTED                    
         BZ    SETSX                                                            
         SH    R1,=Y(OTHDATQ)      R1=DISP TO BEG OF OVERRIDE HEADS             
         LA    R1,BLOCK(R1)                                                     
         MVC   0(OTHDATQ,R1),OTHDAT   MOVE IN 'OTHER' HEADINGS                  
         SPACE 1                                                                
SETSX    LA    R1,DISPTABS         CALC. DISPLACEMENT TO DISPLAY TABLE          
         SR    R3,R1                                                            
         CH    R3,DISPDISP         IF THIS SCREEN ALREADY LOADED                
         BE    YES                 RETURN CC EQ                                 
         STH   R3,DISPDISP         ELSE SAVE DISPLACEMNT TO DISPLAY TBL         
         B     NO                  AND RETURN CC NE                             
         EJECT                                                                  
*              ROUTINE TO CONTROL LISTING RECORDS                               
         SPACE 1                                                                
LREC     NTR1                                                                   
         ST    R0,TIHOOK           SET HOOK TO SYSIO                            
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIUSERID,TWAORIG    SET UP NECESSARY DATA FROM                   
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         MVC   TIKHOOK,SETLSTK     A(KEY SETTING ROUTINE)                       
*                                                                               
*              CLEAR, SET VERIFIED, MAKE NORMAL INTENSITY ALL LIST FLDS         
         L     R3,ALASTDAT                                                      
         BCTR  R3,0                                                             
         GOTO1 ,DMCB,(X'23',AFRSTREC),(X'10',(R3))                              
         TM    GLSTSTAT,CHNGLIST   IF CHANGES ALLOWED                           
         BZ    *+12                                                             
         OI    0(R1),X'04'         SET TO UN-PROTECT FLDS W/ EXTEND HDR         
         B     *+16                                                             
         OI    0(R1),X'08'         ELSE PROTECT ALL FIELDS                      
         LA    R2,SPYINVH                                                       
         ST    R2,ACURFORC         AND POSITION CURSOR AT INVOICE FIELD         
         GOTO1 FLDVAL                                                           
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         MVI   BYTE,C'E'           SET END OF LIST                              
         BAS   RE,SETPAGE          SET PAGE NUMBER                              
*                                                                               
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BE    LRX                 RETURN                                       
*                                                                               
         CP    COUNTER,=P'0'       ELSE THIS MUST BE PRINTREP                   
         BE    LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  COUNTER,(8,P),ALIGN=LEFT                                         
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(13,R1),=C'CHECK RECORDS'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS ON SCREEN                                  
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
         L     R4,TIAREC           R4=A(CHECK RECORD)                           
         USING TLCKD,R4                                                         
         MVC   TGCSORT,TLCKSORT    SAVE CAST SORT KEY                           
         SPACE 1                                                                
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   LRH10                                                            
         USING TACDD,R4                                                         
         TM    TACDSTAT,TACDSLIN   IF CHECK GENERATED BY A LIEN                 
         BO    LRHX                SKIP IT                                      
         TM    TACDSTAT,TACDSTRS   IF CHECK GENERATED BY W4 TRUSTEE             
         BO    LRHX                SKIP IT                                      
*                                                                               
LRH10    CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BNE   LRH20                                                            
         MVI   BYTE,0              SET THIS IS NOT END OF LIST                  
         BAS   RE,SETPAGE          SET PAGE NUMBER                              
         B     LRH60               GO BACK TO LISTMON                           
*                                                                               
LRH20    BAS   RE,DISPLAY          GO DISPLAY IT                                
*                                                                               
         MVC   DMDSKADD,TIDSKADD   SET D/A FOR LISTMON                          
*                                                                               
         CLI   TGCTSTTY,TASTTYPP   IF THIS IS PROGRAMMER                        
         BNE   LRH60                                                            
         L     R2,ALASTDAT               AND IF H&W MSG NOT DISPLAYED           
         CLC   HNWDSP+8(L'HNWMSG,R2),HNWMSG                                     
         BE    LRH60                                                            
         ZIC   R3,LISTNUM                                                       
         LA    R4,1(R3)                                                         
         MH    R3,=H'13'                                                        
         AR    R3,R2                                                            
         EDIT  (R4),(1,8(R3)),ZERO=NOBLANK                                      
         MVC   9(2,R3),=C'->'                                                   
         GOTO1 HEXOUT,DMCB,TIDSKADD,11(R3),4,0  DISPLAY DISK ADDRESS            
*                                                                               
LRH60    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD IN TIAREC ON SCREEN                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         L     R2,ATHISLST         R2=A(FIRST FIELD)                            
         SPACE 1                                                                
         LH    R3,DISPDISP         R3=A(DISPLAY TABLE)                          
         LA    R3,DISPTABS(R3)                                                  
         USING DISPD,R3                                                         
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
DIS20    LH    RF,DISPDIS          RF=A(DISPLAY RTN. FOR CURRENT FIELD)         
         LTR   RF,RF                                                            
         BZ    DIS35                                                            
         AR    RF,RB                                                            
         LA    RE,DIS30                                                         
         NTR1  ,                                                                
         BR    RF                  DISPLAY THE FIELD                            
         SPACE 1                                                                
DIS30    ST    R2,ATHISEND         SAVE A(LAST FIELD DISPLAYED)                 
         BAS   RE,BUMP             BUMP TO NEXT DISPLAY FIELD                   
DIS35    LA    R3,DISPNEXT         BUMP TO NEXT ENTRY IN DISPLAY TABLE          
         SPACE 1                                                                
         CLI   0(R3),X'FE'         TEST TIME TO TEST FOR OTHERS OPT             
         BNE   DIS40                                                            
         LA    R3,1(R3)            BUMP PAST DUMMY BYTE IN TABLE                
         TM    OPTS,OPTOTH         IF DISPLAYING OTHER FIELDS                   
         BZ    DIS40                                                            
         LA    R3,DISPOTH          POINT TO NEW TABLE NOW                       
         SPACE 1                                                                
DIS40    CLI   0(R3),X'FF'         IF NOT END OF TABLE                          
         BNE   DIS20               LOOP                                         
         SPACE 1                                                                
         MVC   AIO,TIAREC                                                       
         GOTO1 FLDSOUT,DMCB,ATHISLST,ATHISEND  HIGHLIGHT CHANGED FIELDS         
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES                                      
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
DISAPPL  DS    0H                                                               
         CLI   TGUSEQU,UPNH        IF PNH ONLY USE                              
         BNE   DISAPPL5                                                         
         OI    1(R2),X'20'         PROTECT CODE FIELD                           
         BAS   RE,BUMP             BUMP TO AMOUNT FIELD                         
         OI    1(R2),X'20'         PROTECT FIELD AND XIT WITH NEW R2            
         B     XITR2                                                            
DISAPPL5 MVC   8(1,R2),TAPDACDE    APPLIED CODE                                 
         CLI   TGUSEQU,UGRT        IF GRT USE                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT CODE FIELD                           
         BAS   RE,BUMP             BUMP TO AMOUNT FIELD                         
         ICM   R3,15,TAPDAPPL      APPLIED AMOUNT                               
         BNZ   *+8                                                              
         L     R3,TAPDGUAR         OR GUARANTEE APPLIED                         
         LCR   R3,R3                                                            
         BAS   RE,EDITAMT          DISPLAY AMOUNT                               
         CLI   TGUSEQU,UGRT        IF GRT USE                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT AMT FIELD                            
         B     XITR2               RETURN NEW R2                                
         SPACE 2                                                                
DISREXP  DS    0H                                                               
         CLI   TGUSEQU,UPNH        IF PNH ONLY USE                              
         BNE   DISREXP5                                                         
         OI    1(R2),X'20'         PROTECT CODE FIELD                           
         BAS   RE,BUMP             BUMP TO AMOUNT FIELD                         
         OI    1(R2),X'20'         PROTECT FIELD AND XIT WITH NEW R2            
         B     XITR2                                                            
DISREXP5 MVC   8(1,R2),TAPDICDE    REIMBURSED EXPENSES INCLUDE CODE             
         BAS   RE,BUMP             BUMP TO AMOUNT FIELD                         
         L     R3,TAPDREXP         REIMBURSED EXPENSES                          
         BAS   RE,EDITAMT                                                       
         B     XITR2               RETURN NEW R2                                
         EJECT                                                                  
DISPAY   DS    0H                                                               
         CLI   TGUSEQU,UPNH        IF PNH ONLY USE                              
         BNE   DISPAY5                                                          
         OI    1(R2),X'20'         PROTECT FIELD AND XIT                        
         B     XIT                                                              
DISPAY5  L     R3,TAPDPAYI         PAYMENT AMOUNT                               
         A     R3,TAPDPAYC                                                      
         BAS   RE,EDITAMTF                                                      
         CLI   TGUSEQU,UGRT        IF GRT USE                                   
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD                                
         B     XIT                                                              
         SPACE 2                                                                
DISSPNH  DS    0H                                                               
         L     R3,TAPDSPNH         SUBJECT TO P&H                               
         BAS   RE,EDITAMTF                                                      
         B     XIT                                                              
         SPACE 2                                                                
DISMDED  DS    0H                                                               
         CLI   TGUSEQU,UPNH        IF PNH ONLY USE                              
         BNE   *+12                                                             
         OI    1(R2),X'20'         PROTECT FIELD AND XIT                        
         B     XIT                                                              
         L     R3,TAPDMDED         MISC. DEDUCTION                              
         A     R3,TAPDDUES                                                      
         BAS   RE,EDITAMT                                                       
         B     XIT                                                              
         SPACE 2                                                                
DISDUES  DS    0H                                                               
         CLI   TGUSEQU,UPNH        IF PNH ONLY USE                              
         BNE   *+12                                                             
         OI    1(R2),X'20'         PROTECT FIELD AND XIT                        
         B     XIT                                                              
         L     R3,TAPDDUES         UNION DUES                                   
         BAS   RE,EDITAMT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES (CONT'D)                             
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
DISPNH   DS    0H                                                               
         L     R3,TAPDPNH          PENSION AND HEALTH                           
         BAS   RE,EDITAMT                                                       
         CLC   TGCSORT,HEXFFS      IF THIS IS UNION CHECK                       
         BE    *+12                                                             
         TM    TGCTSTLV,X'04'      OR IF NOT HIGH-LEVEL STAFF TYPE              
         BO    *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD                                
         B     XIT                                                              
         SPACE 2                                                                
DISHNW   DS    0H                                                               
         CLI   TGUSEQU,UPNH        IF PNH ONLY USE                              
         BNE   *+12                                                             
         OI    1(R2),X'20'         PROTECT FIELD AND XIT                        
         B     XIT                                                              
         L     R3,TAPDHNW          HEALTH AND WELFARE                           
         BAS   RE,EDITAMT                                                       
         CLC   TGCSORT,HEXFFS      IF THIS IS UNION CHECK                       
         BE    *+12                                                             
         TM    TGCTSTLV,X'04'      OR IF NOT HIGH-LEVEL STAFF TYPE              
         BO    *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD                                
         B     XIT                                                              
         SPACE 2                                                                
DISAGT   DS    0H                                                               
         MVC   8(4,R2),=C'NONE'    AGENT                                        
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMNT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         OC    TACANCDE,TACANCDE                                                
         BZ    DAGTX                                                            
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),8(R2)                              
DAGTX    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES (CONT'D)                             
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         USING CASTD,R2            R2=A(FIELD)                                  
DISCAST  DS    0H                                                               
         CLI   MODE,XRECPUT        IF RE-DISPLAYING                             
         BNE   *+12                                                             
         BAS   RE,BUMP             BUMP TO LOCAL FIELD                          
         B     DCASTX              AND DON'T BOTHER WITH REST                   
         SPACE 1                                                                
         MVC   TGSSN,TISSN         MOVE SSN INTO GLOBAL FOR XNAME               
         BAS   RE,SETTAL           SET TALENT FILES                             
         MVC   AIO,AIO2                                                         
         GOTO1 XNAME,DMCB,(X'80',TLW4CDQ),CASTNAME,TIKEY  GET W4 NAME           
         MVC   AIO,AIO1                                                         
         BAS   RE,SETCHK           SET CHECK FILES                              
         SPACE 1                                                                
         MVC   CASTCAT,TICAT       CATEGORY                                     
         SPACE 1                                                                
         OC    TIONOF,TIONOF                                                    
         BZ    *+8                                                              
         MVI   CASTONOF,C'Y'                                                    
         CLC   TIONOF,=C'OFF'      ON/OFF CAMERA                                
         BNE   *+8                                                              
         MVI   CASTONOF,C'N'                                                    
         MVC   CASTYR,TIYEAR       CONTRACT YEAR                                
         SPACE 1                                                                
         BAS   RE,DISOPT           DISPLAY OPTIONAL INFORMATION                 
         MVC   CASTOPTS,BLOCK      RETURNS INFO IN BLOCK                        
         SPACE 1                                                                
         MVC   CASTCTAG,LTCMNT     COMMENT TAG                                  
         SPACE 1                                                                
         BAS   RE,BUMP             BUMP TO LOCAL FIELD                          
         L     R4,AIO2                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL            GET W4 DETAILS ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         MVC   8(3,R2),TAW4LOCL    SAVE AFM LOCAL                               
         SPACE 1                                                                
DCASTX   B     XITR2               RETURN NEW R2                                
         SPACE 2                                                                
DISCMT   DS    0H                                                               
         MVC   AIO,TIAREC                       SET AIO FOR CHAROUT             
         GOTO1 CHAROUT,DMCB,TACMELQ,0,TACMTYPC  GET CHECK COMMENT               
         MVC   AIO,AIO1                         RESTORE AIO                     
         MVC   8(30,R2),TGNAME                  DISPLAY CHECK COMMENT           
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY OPTIONAL PAY INFO                                        
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
DISOPT   NTR1                                                                   
         MVC   BLOCK(40),SPACES    DISPLAY FIRST IN BLOCK                       
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
         CLI   TAPDW4TY,TAW4TYIN   IF THIS ISN'T AN INDIVIDUAL                  
         BE    DOPT2                                                            
         MVC   0(1,R3),TAPDW4TY    DISPLAY W4 TYPE                              
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
DOPT2    XC    TGDUB,TGDUB                                                      
         MVC   TGDUB(4),TAPDOV1    SAVE 1ST OVERSCALE RATE                      
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMNT                      
         BAS   RE,GETEL                                                         
         BNE   DOPT8                                                            
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         SPACE 1                                                                
         OC    TACAGUA,TACAGUA     IF ON A GUARANTEE                            
         BZ    DOPT4                                                            
         MVC   0(3,R3),TACAGUA+1   DISPLAY LAST 3 CHARS OF CODE                 
         MVI   3(R3),C','                                                       
         LA    R3,4(R3)                                                         
         SPACE 1                                                                
DOPT4    CLI   TACADBL,C' '        IF DOUBLES PRESENT                           
         BNH   DOPT6                                                            
         MVC   0(1,R3),TACADBL     DISPLAY THEM                                 
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
DOPT6    MVC   TGDUB+4(4),TACAOV2  SAVE 2ND OVERSCALE RATE                      
         SPACE 1                                                                
DOPT8    OC    TGDUB,TGDUB         IF THERE'S ANY OVERSCALE                     
         BZ    DOPTX                                                            
         MVC   0(2,R3),=C'O='      DISPLAY TAG                                  
         LA    R3,2(R3)                                                         
         L     R1,TGDUB            1ST OVERSCALE RATE                           
         BAS   RE,EDITOV                                                        
         ICM   R1,15,TGDUB+4       2ND OVERSCALE RATE IF PRESENT                
         BZ    *+8                                                              
         BAS   RE,EDITOV                                                        
         SPACE 1                                                                
DOPTX    BCTR  R3,0                SHUFFLE BACK ONE                             
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '          ERASE TRAILING COMMA                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES (CONT'D)                             
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
*                                  R2=A(FIELD)                                  
DISBSS   DS    0H                                                               
         LR    R3,R2               R3=A(TV SESSION DETAIL FIELDS)               
         USING LBSSD,R3                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   DBSSX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         GOTO1 EDIT1,DMCB,TASDSP,LBSSSPT    SPOTS                               
         GOTO1 (RF),(R1),TASDDAY,LBSSDAY    DAYS                                
         GOTO1 (RF),(R1),TASDOT,LBSSOT      OVERTIME                            
         GOTO1 (RF),(R1),TASDDT,LBSSDT      DOUBLETIME                          
         GOTO1 EDIT2,DMCB,TASDTRV,LBSSTRV   TRAVEL TIME                         
         GOTO1 (RF),(R1),TASDPDW,LBSSPDW    PRIOR-DAY WARDROBE                  
         GOTO1 EDIT1,DMCB,TASDTAG,LBSSTAG   TAGS                                
         SPACE 1                                                                
DBSSX    LA    R2,LBSSTAGH         POINT R2 TO LAST FIELD                       
         B     XITR2               RETURN NEW R2                                
         SPACE 2                                                                
         USING LBSRD,R2            R2=A(FIELD)                                  
DISBSR   DS    0H                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   DBSRX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         GOTO1 EDIT1,DMCB,TASDRSP,LBSRSPT   SPOTS                               
         GOTO1 EDIT2,DMCB,TASDRHM,LBSRHM    HOURS/MINUTES                       
         GOTO1 EDIT1,DMCB,TASDRTG,LBSRTAG   TAGS                                
         SPACE 1                                                                
DBSRX    LA    R2,LBSRTAGH         POINT R2 TO LAST FIELD                       
         B     XITR2               RETURN NEW R2                                
         SPACE 2                                                                
         USING LBSMD,R2            R2=A(FIELD)                                  
DISBSM   DS    0H                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   DBSMX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         GOTO1 EDIT1,DMCB,TASDMSP,LBSMSPT   SPOTS                               
         GOTO1 EDIT2,DMCB,TASDMHM,LBSMHM    HOURS/MINUTES                       
         SPACE 1                                                                
DBSMX    LA    R2,LBSMHMH          POINT R2 TO LAST FIELD                       
         B     XITR2               RETURN NEW R2                                
         SPACE 2                                                                
         USING LIVRD,R2            R2=A(FIELD)                                  
DISIVR   DS    0H                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   DIVRX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         GOTO1 EDIT2,DMCB,TASDIVH,LIVRHM    HOURS                               
         GOTO1 (RF),(R1),TASDIVT,LIVRTRV    TRAVEL TIME                         
         SPACE 1                                                                
DIVRX    LA    R2,LIVRTRVH         POINT R2 TO LAST FIELD                       
         B     XITR2               RETURN NEW R2                                
         SPACE 2                                                                
         USING LDIOD,R2            R2=A(FIELD)                                  
DISDIO   DS    0H                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   DDIOX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         GOTO1 EDIT1,DMCB,TASDIRP,LDIOPRO   PROGRAMS                            
         GOTO1 EDIT2,DMCB,TASDIRH,LDIOHM    HOURS                               
         GOTO1 (RF),(R1),TASDIRR,LDIORHM    RETAKE HOURS                        
         SPACE 1                                                                
DDIOX    LA    R2,LDIORHMH         POINT R2 TO LAST FIELD                       
         B     XITR2               RETURN NEW R2                                
         SPACE 2                                                                
         USING LRTKD,R2            R2=A(FIELD)                                  
DISRTK   DS    0H                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   DRTKX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         ZICM  R1,TASDIHM,2                                                     
         ZIC   RE,TASDIOT                                                       
         ZIC   RF,TASDIDT                                                       
         AR    RE,RF                                                            
         MHI   RE,100                                                           
         AR    R1,RE                                                            
         STCM  R1,3,HALF                                                        
         GOTO1 EDIT2,DMCB,HALF,LRTKHM    HOURS                                  
         GOTO1 (RF),(R1),TASDITR,LRTKTRV    TRAVEL TIME                         
         SPACE 1                                                                
         TM    TASDIST,TASDIENQ                                                 
         BZ    *+8                                                              
         MVI   LRTKENT,C'Y'        ENTIRE SCRIPT                                
         SPACE 1                                                                
         TM    TASDIST,TASDIPTQ                                                 
         BZ    *+8                                                              
         MVI   LRTKPRT,C'Y'        PARTIAL SCRIPT                               
         SPACE 1                                                                
         TM    TASDIST,TASDI60Q                                                 
         BZ    *+8                                                              
         MVI   LRTK60,C'Y'         >60 DAYS                                     
         SPACE 1                                                                
DRTKX    LA    R2,LRTK60H          POINT R2 TO LAST FIELD                       
         B     XITR2               RETURN NEW R2                                
         EJECT                                                                  
*              ROUTINE TO EDIT OVERSCALE RATES                                  
         SPACE 1                                                                
*                                  R1=RATE   R3=A(OUTPUT AREA)                  
EDITOV   DS    0H                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          TRUNCATE FRACTION                            
         EDIT  (R1),(4,(R3)),ALIGN=LEFT                                         
         AR    R3,R0               BUMP PAST AMOUNT                             
         MVI   0(R3),C','          ADD COMMA                                    
         LA    R3,1(R3)                                                         
         BR    RE                  RETURN R3=A(NEXT SLOT)                       
         SPACE 3                                                                
*              AMOUNT EDIT ROUTINES                                             
         SPACE 1                                                                
EDITAMT  LTR   R3,R3               ENTRY POINT IF PRINT ZERO AS BLANKS          
         BNZ   EDITAMTF                                                         
         XC    8(10,R2),8(R2)                                                   
         BR    RE                                                               
EDITAMTF TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    *+6                                                              
         LCR   R3,R3               COMPLEMENT AMOUNT FOR DISPLAY                
         CLI   0(R2),8+8+9                                                      
         BE    EDITAMT9                                                         
         BH    *+6                                                              
         DC    H'0'                FIELD IS TOO SMALL FOR EDIT                  
         EDIT  (R3),(10,8(R2)),2,ZERO=NOBLANK,FLOAT=-                           
         BR    RE                                                               
EDITAMT9 EDIT  (R3),(9,8(R2)),2,ZERO=NOBLANK,FLOAT=-                            
         BR    RE                                                               
         SPACE 3                                                                
EDIT1    NTR1  ,                   EDIT 1 BYTE TO 2 BYTES                       
         LM    RE,RF,0(R1)         RE=A(AMOUNT)  RF=A(OUTPUT AREA)              
         EDIT  (1,0(RE)),(2,(RF)),ZERO=BLANK,ALIGN=LEFT                         
         B     XIT                                                              
         SPACE 2                                                                
EDIT2    NTR1  ,                   EDIT 2 BYTES TO 5 BYTES W/ 2 DEC.            
         LM    RE,RF,0(R1)         RE=A(AMOUNT)  RF=A(OUTPUT AREA)              
         EDIT  (2,0(RE)),(5,(RF)),2,ZERO=BLANK,ALIGN=LEFT                       
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE BUMPS R2 TO NEXT FIELD                                   
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE SETS PAGE NUMBER                                         
         SPACE 1                                                                
*                                  BYTE=FLAG FOR HOB OF TOTCNTL CALL            
SETPAGE  NTR1                                                                   
         XC    TOTS,TOTS           CLEAR DUMMY AREA                             
         MVI   THISPAGE,1          SET TO ADD 1 TO PAGE NUMBER                  
         SPACE 1                                                                
         GOTO1 TOTCNTL,DMCB,(BYTE,TOTS)  HANDLE PAGE TOTALS                     
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPP   IF THIS IS PROGRAMMER                        
         BNE   SETPX                                                            
         LA    RE,SPYLTWA+5                                                     
         EDIT  (1,THISPAGE),(3,(RE)),ZERO=NOBLANK  DISPLAY PAGE                 
         OI    SPYLTWAH+6,X'80'                                                 
         SPACE 1                                                                
SETPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES A LISTED RECORD                                
         SPACE 1                                                                
LVREC    NTR1                                                                   
         TM    TGUSSTAT,CANUSE     IF CANADIAN USE                              
         BO    ERRLOCK             NOT ALLOWED TO CHANGE RECORDS                
         MVC   SVKEY,KEY           SAVE KEY                                     
         GOTO1 ASAVPTRS,DMCB,PTRBLK  SAVE POINTERS                              
         SPACE 1                                                                
         BRAS  RE,CKCAN            IF I&R ACCOUNT OR UNION IS ACTRA             
         BE    ERRLOCK             NOT ALLOWED TO CHANGE RECORDS                
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         MVC   SVELEM,0(R4)                                                     
         OI    TAPDPST1,TAPDPCHG   SET PAYMENT HAS CHANGED                      
         SPACE 1                                                                
         BAS   RE,LVINIT           INITIALIZE FOR VALIDATION                    
         SPACE 1                                                                
         L     R2,ATHISLST         R2=A(THIS DATA LINE)                         
         SPACE 1                                                                
         LH    R3,DISPDISP         R3=A(DISPLAY TABLE)                          
         LA    R3,DISPTABS(R3)                                                  
         USING DISPD,R3                                                         
         SPACE 1                                                                
LVR20    LH    RF,DISPVAL          RF=A(VALIDATION RTN FOR CURRENT FLD)         
         LTR   RF,RF                                                            
         BZ    LVR30               FIELD CANNOT BE CHANGED                      
         AR    RF,RB                                                            
         LA    RE,LVR30                                                         
         NTR1  ,                                                                
         BR    RF                  VALIDATE THE FIELD                           
         SPACE 1                                                                
LVR30    OC    DISPDIS,DISPDIS     IF FIELD WASN'T DISPLAYED, SKIP              
         BZ    LVR34                                                            
         CLI   1(R2),X'FF'         IGNORE NOP FIELDS                            
         BE    LVR32                                                            
         TM    4(R2),X'20'         ELSE IF FIELD CHANGED                        
         BO    LVR32                                                            
         TM    1(R2),X'02'         AND IF FIELD HAS EXTENDED HEADER             
         BZ    LVR32                                                            
         ZIC   RE,0(R2)            GET A(FIELD NUMBER)                          
         SH    RE,=H'8'                                                         
         AR    RE,R2                                                            
         CLI   0(RE),#AGT          IF THIS IS NOT AGENT                         
         BE    LVR32                                                            
         CLI   0(RE),#CMT          OR COMMENT                                   
         BE    LVR32                                                            
         OI    VALSTAT,NONMEMO     THEN SET A NON-MEMO ITEM CHANGED             
         SPACE 1                                                                
LVR32    ST    R2,ATHISEND         SAVE A(LAST FIELD VALIDATED)                 
         BAS   RE,BUMP             BUMP TO NEXT DISPLAY FIELD                   
         SPACE 1                                                                
LVR34    LA    R3,DISPNEXT         BUMP TO NEXT ENTRY IN DISPLAY TABLE          
         SPACE 1                                                                
         CLI   0(R3),X'FE'         TEST TIME TO TEST FOR OTHERS OPT             
         BNE   LVR40                                                            
         LA    R3,1(R3)            BUMP PAST DUMMY BYTE IN TABLE                
         TM    OPTS,OPTOTH         IF DISPLAYING OTHER FIELDS                   
         BZ    LVR40                                                            
         LA    R3,DISPOTH          POINT TO NEW TABLE NOW                       
         SPACE 1                                                                
LVR40    CLI   0(R3),X'FF'         IF NOT END OF TABLE                          
         BNE   LVR20               LOOP                                         
         SPACE 1                                                                
         TM    VALSTAT,NONMEMO     IF NO NON-MEMO ITEM CHANGED                  
         BZ    LVR60               DON'T BOTHER FOOLING WITH AMOUNTS            
         SPACE 1                                                                
         BAS   RE,CASTREC          GET CAST RECORD FOR SYSCALC                  
         SPACE 1                                                                
         BAS   RE,GETRATE          GET NEW RATES FROM SYSCALC                   
         SPACE 1                                                                
         TM    TCRTRN,TCRTCAST     IF SYSCALC CHANGED CAST RECORD               
         BZ    LVR50                                                            
         OC    TCACAST,TCACAST     AND WE HAVE ITS ADDRESS                      
         BZ    LVR50                                                            
         MVC   AIO,TCACAST         THEN WRITE IT BACK                           
         BAS   RE,SETTAL                                                        
         GOTO1 PUTREC                                                           
         BAS   RE,SETCHK                                                        
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
LVR50    BAS   RE,CHECK            UPDATE CHECK RECORD APPROPRIATELY            
         SPACE 1                                                                
LVR60    GOTO1 ACTVIN,DMCB,0       UPDATE LAST CHANGED                          
         SPACE 1                                                                
         GOTO1 FLDSIN,DMCB,ATHISLST,ATHISEND  UPDATE FIELDS CHANGED EL.         
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'22',ATHISLST),ATHISEND  SET ALL FLDS VER.         
         SPACE 1                                                                
         OC    DIFFAMTS+TAPDHNW-TAPDAMTS(4),DIFFAMTS+TAPDHNW-TAPDAMTS           
         BZ    LVR80                                                            
         L     R1,ALASTDAT         IF H&W CHANGED DISPLAY SPECIAL MSG           
         MVC   HNWDSP+8(L'HNWMSG,R1),HNWMSG                                     
         OI    6(R1),X'80'                                                      
         SPACE 1                                                                
LVR80    MVC   KEY,SVKEY           RESTORE CHECK RECORD KEY                     
         MVC   AIO,AIO3            SET ALT. I/O AREA                            
         GOTO1 GETREC              GET THE RECORD AGAIN FOR PUTREC              
         MVC   AIO,AIO1            (DID GETREC TO GET CAST RECORD)              
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION INITIALIZATION ROUTINES                               
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
LVINIT   NTR1                                                                   
         XC    DIFFAMTS,DIFFAMTS   CLEAR AMOUNT DIFFERENCES FOR INVOICE         
         XC    DIFFNTNW,DIFFNTNW                                                
         XC    MYAMTS,MYAMTS             LOCAL AMOUNT OVERRIDES                 
         MVI   VALSTAT,0                 VALIDATION STATUS BYTE                 
         XC    TCUDETS(TCUDTLNQ),TCUDETS SYSCALC USE LEVEL INFO                 
         XC    TCCAST(TCCSTLNQ),TCCAST   SYSCALC CAST LEVEL INFO                
         XC    TCCSTBRK,TCCSTBRK                                                
         XC    MYTACREL,MYTACREL         SAVED APPLIED CREDITS ELEMENT          
         XC    AFLDS,AFLDS               A(SELECTED FIELDS)                     
         SPACE 1                                                                
         MVC   TCPCYC,TAPDCYCS     SET CYCLE DATES FROM PAYMENT DTLS EL         
         MVC   TCW4TYPE,TAPDW4TY       W4 TYPE                                  
         MVI   TCUSETAB+3,C' '                                                  
         TM    TAPDSTAT,TAPDSLFT   PAYMENT MADE TO LIFT                         
         BZ    *+8                                                              
         OI    TCUSETAB+3,TCLFTPRO                                              
* NO-OP  MVC   TCOV1,TAPDOV1       OVERSCALE RATE 1 (PICK UP CURRENT)           
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    *+8                                                              
         OI    TCPAYST,TCCREDIT    SET CORRES. SYSCALC BIT                      
         TM    PAYOPT1,TAPDOPHR    IF P&H RATE OVERRIDDEN                       
         BZ    *+14                                                             
         MVC   TCPNHR,PNHRATE      PASS IT TO SYSCALC                           
         OI    TCOPTS,TCOPNHR      LET SYSCALC KNOW IT'S THERE                  
         SPACE 1                                                                
         LH    R1,TAPDSTUS         STARTING USE NUMBER                          
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                LESS ONE                                     
         STH   R1,TCNUSES          IS N'USES PREV. PAID                         
         MVC   TCTUSES,TAPDUSES    N'USES                                       
         MVC   TCUNITS,TAPDUNIT    N'UNITS                                      
         MVC   TCMAJORS,TAPDMAJ    MAJORS                                       
         MVC   TCINSRTS,TAPDINS    INSERTS                                      
         SPACE 1                                                                
         MVC   APPLCODE,TAPDACDE   SAVE ORIGINAL APPLIED CODE                   
         MVC   APPLAMNT,TAPDAPPL   AND APPLIED AMOUNT                           
         SPACE 1                                                                
         BAS   RE,BLDTAUH          BUILD USAGE HISTORY DETAILS EL.              
         SPACE 1                                                                
         XC    TCATASD,TCATASD                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         ST    R4,TCATASD          IF FOUND, PASS ITS ADDRESS                   
         SPACE 1                                                                
         LA    R4,ELTACO           PASS A(COMMERCIAL DETAILS ELEMENT)           
         ST    R4,TCATACO                                                       
         SPACE 1                                                                
         L     R4,AIO              R4=A(CHECK RECORD)                           
         USING TLCKD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCKCAT  SET CATEGORY DETAILS                        
         SPACE 1                                                                
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         SPACE 1                                                                
         GOTO1 UNIVAL,DMCB,TACAUN   SET UNION DETAILS                           
         GOTO1 YRVAL,DMCB,TACAYEAR  AND YEAR DETAILS                            
         SPACE 1                                                                
         MVC   TGLCL,TACALOCL      LOCAL                                        
         MVC   TCCADBL,TACADBL     N'DOUBLES                                    
         MVC   TCOV2,TACAOV2       2ND OVERSCALE RATE                           
         MVC   TCCASTAT,TACASTAT   STATUS BYTE                                  
         MVC   TCCASTA2,TACASTA2   STATUS BYTE 2                                
         MVC   TCCASTA3,TACASTA3   STATUS BYTE 3                                
         MVC   TCCAONOF,TACAONOF   ON/OFF CAMERA                                
         MVC   TCCAFRST,TACAFRST   1ST SERVICES DATE                            
         MVC   TCCAFCYC,TACAFCYC   FIRST FIXED CYCLE DATE                       
         MVC   TGGUA,TACAGUA       GUARANTEE CODE                               
         OI    TCOPTS,TCNOCKDL+TCRESCRS  DON'T CHECK FOR DLR CYCLES             
*                                        AND RESOLVE CREDIT AMOUNTS             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS USAGE HISTORY ELEMENT FOR SYSCALC                 
         SPACE 1                                                                
BLDTAUH  NTR1                                                                   
         XC    TCTAUHEL,TCTAUHEL   CLEAR USAGE HISTORY DETAILS                  
         SPACE 1                                                                
         OC    ELTAUP,ELTAUP       IF THERE ARE NO UPGRADE DETAILS              
         BZ    BUHX                THEN DON'T BOTHER                            
         SPACE 1                                                                
         LA    R3,TCTAUHEL         SET TO BUILD USAGE HISTORY DETAILS           
         USING TAUHD,R3                                                         
         LA    R4,ELTAUP           R4=A(UPGRADE DETAILS EL.)                    
         USING TAUPD,R4                                                         
         MVI   TAUHEL,TAUHELQ                                                   
         MVI   TAUHLEN,TAUHLNQ                                                  
         MVC   TAUHTYPE,TGUSTYP    USE TYPE                                     
         MVC   TAUHSTRT(6),TCPCYC  CYCLE DATES                                  
         SPACE 1                                                                
         CLI   TGUSEQU,UWSP        SPECIAL FOR WILDSPOT                         
         BNE   BUH10                                                            
         MVC   TAUHMAJ,TAUPIMAJ    MAJORS                                       
         MVC   TAUHUNT,TAUPIUNT    UNITS                                        
         B     BUHX                                                             
         SPACE 1                                                                
BUH10    DS    0H                                                               
*UH10    CLI   TGUSEQU,UIFB        SPECIAL FOR INSERTS FOR BOOKENDS             
*        BNE   *+10                                                             
*        MVC   TAUHINS,TAUPIINS    INSERTS                                      
         SPACE 1                                                                
BUHX     B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES                                   
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
VALAPPL  DS    0H                                                               
         LR    R3,R2               SAVE A(APPLIED CODE FIELD)                   
         BAS   RE,BUMP             BUMP TO APPLIED AMOUNT                       
         BAS   RE,AMTVAL           VALIDATE AMOUNT (RETURNED IN FULL)           
         BNE   VAPP10                                                           
         OC    FULL,FULL           IF AMOUNT=0,                                 
         BZ    VAPP20              DON'T BOTHER WITH APPLIED CODE               
         SPACE 1                                                                
VAPP10   GOTO1 CODEVAL,DMCB,(R3)   APPLIED CODE                                 
         MVC   TCAPPLCD,BYTE       SET NEW CODE (RETURNED BY CODEVAL)           
         SPACE 1                                                                
VAPP20   TM    4(R2),X'20'         IF AMOUNT FIELD NOT PREV. VALIDATED          
         BO    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RECALC PAY AND P&H               
         SPACE 1                                                                
         BAS   RE,AMTVAL           VALIDATE AMOUNT                              
         BNE   VAPPX               NO INPUT - DON'T SET OVERRIDE                
         MVC   TCAPPLCR,FULL       SET APPLIED CREDITS                          
         OI    TCINPUT,TCINAPPL    SET APPLIED CREDITS INPUT                    
         ST    R2,AAPPLFLD         SAVE A(FIELD)                                
VAPPX    B     XITR2                                                            
         SPACE 2                                                                
VALPAY   DS    0H                  PAYMENT AMOUNT                               
         ST    R2,APAYFLD          SAVE A(FIELD)                                
         CLI   TGUSEQU,UPNH        IF PNH ONLY USE                              
         BNE   VALPAY5                                                          
         OI    TCINPUT,TCINPAY     SET PAYMENT AMOUNT INPUT FOR SYSCALC         
         B     XIT                                                              
VALPAY5  TM    4(R2),X'20'         IF FIELD DIDN'T CHANGE                       
         BZ    *+12                                                             
         TM    VALSTAT,CALCPAY     AND WE'RE RECALCULATING                      
         BO    VPAYX               THEN EXIT                                    
         SPACE 1                                                                
         TM    VALSTAT,CALCPAY     AND WE'RE RECALCULATING                      
         BAS   RE,AMTVAL           VALIDATE AMOUNT                              
         BNE   VPAYX               NO INPUT - DON'T SET OVERRIDE                
         MVC   TCPAY,FULL          SET PAYMENT AMOUNT                           
         OI    TCINPUT,TCINPAY     SET PAYMENT AMOUNT INPUT                     
VPAYX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES (CONT'D)                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
VALREXP  DS    0H                                                               
         CLI   TGUSEQU,USOP        FOR SOAP PAYMENTS                            
         BNE   *+16                                                             
         TM    4(R2),X'20'         IF FIELD NOT PREV. VALIDATED                 
         BO    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RECALC                           
         SPACE 1                                                                
         GOTO1 CODEVAL,DMCB,(R2)   REIMB. EXPENSES INCLUDE CODE                 
         MVC   TCEXPICD,BYTE                                                    
         BAS   RE,BUMP                                                          
         BAS   RE,AMTVAL                                                        
         MVC   TCEXP,FULL                                                       
         B     XITR2                                                            
         SPACE 2                                                                
SETREXP  DS    0H                  SET REIMB. EXPENSES WHEN NOT DISP.           
         L     R1,TAPDREXP                                                      
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    *+6                                                              
         LCR   R1,R1               COMPLEMENT AMOUNT                            
         ST    R1,TCEXP                                                         
         MVC   TCEXPICD,TAPDICDE                                                
         B     XIT                                                              
         SPACE 2                                                                
SETMDED  DS    0H                  SET MISC. DEDUCTION WHEN NOT DISP.           
         L     R1,TAPDMDED                                                      
         A     R1,TAPDDUES                                                      
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    *+6                                                              
         LCR   R1,R1               COMPLEMENT AMOUNT                            
         ST    R1,TCMDED                                                        
         B     XIT                                                              
         SPACE 2                                                                
VALSPNH  DS    0H                  SUBJECT TO P&H                               
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         OI    VALSTAT,CALCPNH     SET WE'RE RECALCULATING P&H                  
         B     *+12                                                             
         TM    VALSTAT,CALCPNH     ELSE IF WE WERE ALREADY RECALC'ING           
         BO    VSPNHX              THEN EXIT                                    
         SPACE 1                                                                
         BAS   RE,AMTVAL           VALIDATE AMOUNT                              
         BNE   *+14                NO INPUT - DON'T SET OVERRIDE                
         MVC   TCSUBPNH,FULL       ELSE PASS NEW AMOUNT TO SYSCALC              
         OI    TCINPUT,TCINPNH     SET SUBJ. TO P&H OVERRIDDEN                  
VSPNHX   B     XIT                                                              
         EJECT                                                                  
VALMDED  DS    0H                                                               
         ICM   R2,8,=X'80'         ALLOW NEGATIVE                               
         BAS   RE,AMTVAL           MISCELLANEOUS DEDUCTIONS                     
         BNE   *+14                NO INPUT - DON'T SET OVERRIDE                
         MVC   TCMDED,FULL         PASS NEW AMOUNT TO SYSCALC                   
         OI    TCINPUT2,TCINMDED   SET MISC. DED OVERRIDDEN                     
         B     XIT                                                              
         SPACE 2                                                                
VALDUES  DS    0H                                                               
         ICM   R2,8,=X'80'         ALLOW NEGATIVE                               
         BAS   RE,AMTVAL           UNION DUES                                   
         BNE   *+14                NO INPUT - DON'T SET OVERRIDE                
         MVC   TCDUES,FULL         PASS NEW AMOUNT TO SYSCALC                   
         OI    TCINPUT2,TCINDUES   SET UNION DUES OVERRIDE                      
         B     XIT                                                              
         SPACE 2                                                                
SETDUES  DS    0H                  SET UNION DUES WHEN NOT DISP.                
         L     R1,TAPDDUES                                                      
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    *+6                                                              
         LCR   R1,R1               COMPLEMENT AMOUNT                            
         ST    R1,TCDUES                                                        
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES (CONT'D)                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
VALPNH   DS    0H                  PENSION AND HEALTH                           
         TM    4(R2),X'20'         IF FIELD DIDN'T CHANGE                       
         BZ    *+12                                                             
         TM    VALSTAT,CALCPNH     AND WE'RE RECALCULATING                      
         BO    VPNHX               THEN EXIT                                    
         SPACE 1                                                                
         BAS   RE,AMTVAL           VALIDATE AMOUNT                              
         BNE   *+14                NO INPUT - DON'T SET OVERRIDE                
         MVC   MYPNH,FULL          SAVE AMOUNT IN LOCAL W/S                     
         OI    VALSTAT,INPNH       SET P&H AMOUNT OVERRIDDEN                    
VPNHX    B     XIT                                                              
         SPACE 2                                                                
VALHNW   DS    0H                                                               
         TM    4(R2),X'20'         IF FIELD DIDN'T CHANGE                       
         BZ    *+12                                                             
         TM    VALSTAT,CALCPNH     AND WE'RE RECALCULATING                      
         BO    VHNWX               THEN EXIT                                    
         SPACE 1                                                                
         BAS   RE,AMTVAL           HEALTH AND WELFARE                           
         BNE   VHNWX               NO INPUT - DON'T SET OVERRIDE                
*        TM    TGUNEQU,AFM         INPUT ALLOWED FOR AFM ONLY                   
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    NOINPUT                                                          
         MVC   MYHNW,FULL          SAVE AMOUNT IN LOCAL W/S                     
         OI    VALSTAT,INHNW       SET H&W AMOUNT OVERRIDDEN                    
VHNWX    B     XIT                                                              
         SPACE 2                                                                
SETHNW   DS    0H                  SET H&W AMOUNT WHEN NOT DISP.                
         TM    VALSTAT,CALCPNH     IF WE WERE ALREADY RECALC'ING                
         BO    SHNWX               THEN EXIT                                    
         L     R1,TAPDHNW                                                       
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    *+6                                                              
         LCR   R1,R1               COMPLEMENT AMOUNT                            
         ST    R1,MYHNW            SAVE AMOUNT IN LOCAL W/S                     
         OI    VALSTAT,INHNW       SET H&W AMOUNT OVERRIDDEN                    
SHNWX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES (CONT'D)                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
VALAGT   DS    0H                                                               
         TM    4(R2),X'20'         TEST FIELD HAS CHANGED                       
         BO    VAGTX               NO                                           
         XC    HALF,HALF                                                        
         CLI   5(R2),0             AGENT CODE (OPTIONAL)                        
         BE    VAGT10                                                           
         BAS   RE,SETTAL                                                        
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'02',(R2))  VALIDATE IT                    
         BAS   RE,SETCHK                                                        
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),HALF                                  
VAGT10   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         MVC   TACANCDE,HALF       SAVE BINARY AGENT CODE IN ELEMENT            
         OI    TACASTAT,TACASTAO   SET AGENT OVERRIDDEN STATUS                  
VAGTX    B     XIT                                                              
         SPACE 2                                                                
VALCMT   DS    0H                                                               
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPC  CHECK COMMENT          
         B     XIT                                                              
         SPACE 2                                                                
SETCAST  DS    0H                                                               
         BAS   RE,BUMP             BUMP TO AFM LOCAL FIELD                      
*        TM    TGUNEQU,AFM         IF UNION IS AFM                              
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    SETCX                                                            
         CLC   8(3,R2),SPACES      AND IT'S DEFINED                             
         BNH   SETCX                                                            
         MVC   TGLCL,8(R2)         SAVE W4 LOCAL                                
SETCX    B     XITR2                                                            
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES (CONT'D)                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
VALBSS   DS    0H                                                               
         LR    R3,R2               R3=A(TV SESSION DETAIL FIELDS)               
         USING LBSSD,R3                                                         
         GOTO1 FLDVAL,DMCB,(X'40',LBSSSPTH),LBSSTAGH  IF ANY FIELD CHGD         
         BE    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RE-CALCULATE                     
         SPACE 1                                                                
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   VBSSX                                                            
         MVC   ELEMENT,0(R4)                                                    
         SPACE 1                                                                
         LA    R2,LBSSSPTH         N'SPOTS                                      
         BAS   RE,NUMVAL                                                        
         MVC   TASDSP,ACTUAL                                                    
         SPACE 1                                                                
         LA    R2,LBSSDAYH         N'DAYS                                       
         BAS   RE,NUMVAL                                                        
         MVC   TASDDAY,ACTUAL                                                   
         SPACE 1                                                                
         LA    R2,LBSSOTH          N'OVERTIME HOURS                             
         BAS   RE,NUMVAL                                                        
         MVC   TASDOT,ACTUAL                                                    
         SPACE 1                                                                
         LA    R2,LBSSDTH          N'DOUBLE-TIME HOURS                          
         BAS   RE,NUMVAL                                                        
         MVC   TASDDT,ACTUAL                                                    
         SPACE 1                                                                
         LA    R2,LBSSTRVH         N'TRAVEL TIME HOURS/MINUTES                  
         MVC   TGFULL,=F'15'       VALID INCREMENTS ARE 15 MINUTES              
         OI    TGFULL,X'A0'        SET NO 1 HR MINIMUM AND 99 HOUR MAX          
         BAS   RE,VALHRMN                                                       
         MVC   TASDTRV,HALF                                                     
         SPACE 1                                                                
         LA    R2,LBSSPDWH         N'PRIOR-DAY WARDROBE HOURS/MINUTES           
         MVC   TGFULL,=F'15'       VALID INCREMENTS ARE 15 MINUTES              
         BAS   RE,VALHRMN                                                       
         MVC   TASDPDW,HALF                                                     
         SPACE 1                                                                
         LA    R2,LBSSTAGH         N'TAGS                                       
         BAS   RE,NUMVAL                                                        
         MVC   TASDTAG,ACTUAL                                                   
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAPSELQ      IF RECORD DOES NOT ALREADY HAVE              
         BAS   RE,GETEL            PRE-PYMT/LIST SESSION DETAILS ELEM           
         BE    VBSSX                                                            
         MVI   ELEMENT,TAPSELQ     SAVE PRE-PYMT/LIST SESSION DETAILS           
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
VBSSX    LA    R2,LBSSTAGH         POINT R2 TO LAST FIELD                       
         B     XITR2                                                            
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES (CONT'D)                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
VALBSR   DS    0H                                                               
         LR    R3,R2               R3=A(RADIO SESSION DETAIL FIELDS)            
         USING LBSRD,R3                                                         
         GOTO1 FLDVAL,DMCB,(X'40',LBSRSPTH),LBSRTAGH  IF ANY FIELD CHGD         
         BE    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RE-CALCULATE                     
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   VBSRX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         LA    R2,LBSRSPTH         N'SPOTS                                      
         BAS   RE,NUMVAL                                                        
         MVC   TASDRSP,ACTUAL                                                   
         SPACE 1                                                                
         LA    R2,LBSRHMH          HOURS/MINUTES                                
         MVC   TGFULL,=F'1'        ANY INCREMENT ALLOWED                        
         BAS   RE,VALHRMN                                                       
         MVC   TASDRHM,HALF                                                     
         SPACE 1                                                                
         LA    R2,LBSRTAGH         N'TAGS                                       
         BAS   RE,NUMVAL                                                        
         MVC   TASDRTG,ACTUAL                                                   
         SPACE 1                                                                
VBSRX    LA    R2,LBSRTAGH         POINT R2 TO LAST FIELD                       
         B     XITR2                                                            
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES (CONT'D)                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
VALBSM   DS    0H                                                               
         LR    R3,R2               R3=A(MUSIC SESSION DETAIL FIELDS)            
         DROP  R2                                                               
         USING LBSMD,R3                                                         
         GOTO1 FLDVAL,DMCB,(X'40',LBSMSPTH),LBSMHMH  IF ANY FIELD CHGD          
         BE    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RE-CALCULATE                     
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   VBSMX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         LA    R2,LBSMSPTH         N'SPOTS                                      
         BAS   RE,NUMVAL                                                        
         MVC   TASDMSP,ACTUAL                                                   
         SPACE 1                                                                
         LA    R2,LBSMHMH          HOURS/MINUTES                                
         MVC   TGFULL,=F'20'       VALID INCREMENTS ARE 20 MINUTES              
         CLI   TGUSEQU,UIMS                                                     
         BNE   *+10                                                             
         MVC   TGFULL,=F'15'       VALID INCREMENTS ARE 15 MINUTES              
         BAS   RE,VALHRMN                                                       
         MVC   TASDMHM,HALF                                                     
         SPACE 1                                                                
VBSMX    LA    R2,LBSMHMH          POINT R2 TO LAST FIELD                       
         B     XITR2                                                            
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES (CONT'D)                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
VALIVR   DS    0H                                                               
         LR    R3,R2               R3=A(IVR DETAIL FIELDS)                      
         USING LIVRD,R3                                                         
         GOTO1 FLDVAL,DMCB,(X'40',LIVRHMH),LIVRTRVH IF ANY FIELD CHGD           
         BE    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RE-CALCULATE                     
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   VIVRX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         LA    R2,LIVRHMH          HOURS/MINUTES                                
         MVC   TGFULL,=F'1'        ANY INCREMENT ALLOWED                        
         BAS   RE,VALHRMN                                                       
         MVC   TASDIVH,HALF                                                     
         SPACE 1                                                                
         LA    R2,LIVRTRVH         HOURS/MINUTES                                
         MVC   TGFULL,=F'1'        ANY INCREMENT ALLOWED                        
         BAS   RE,VALHRMN                                                       
         MVC   TASDIVT,HALF                                                     
         SPACE 1                                                                
VIVRX    LA    R2,LIVRTRVH         POINT R2 TO LAST FIELD                       
         B     XITR2                                                            
*                                  R2=A(FIELD)                                  
VALDIO   DS    0H                                                               
         LR    R3,R2               R3=A(DIO DETAIL FIELDS)                      
         USING LDIOD,R3                                                         
         GOTO1 FLDVAL,DMCB,(X'40',LDIOPROH),LDIORHMH IF ANY FIELD CHGD          
         BE    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RE-CALCULATE                     
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   VDIOX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         LA    R2,LDIOPROH         PROGRAMS                                     
         BAS   RE,NUMVAL                                                        
         MVC   TASDIRP,ACTUAL                                                   
         SPACE 1                                                                
         LA    R2,LDIOHMH          HOURS/MINUTES                                
         MVC   TGFULL,=F'1'        ANY INCREMENT ALLOWED                        
         BAS   RE,VALHRMN                                                       
         MVC   TASDIRH,HALF                                                     
         SPACE 1                                                                
         LA    R2,LDIORHMH         HOURS/MINUTES                                
         MVC   TGFULL,=F'1'        ANY INCREMENT ALLOWED                        
         BAS   RE,VALHRMN                                                       
         MVC   TASDIRR,HALF                                                     
         SPACE 1                                                                
VDIOX    LA    R2,LDIORHMH         POINT R2 TO LAST FIELD                       
         B     XITR2                                                            
*                                  R2=A(FIELD)                                  
VALRTK   DS    0H                                                               
         LR    R3,R2               R3=A(RTK DETAIL FIELDS)                      
         USING LRTKD,R3                                                         
         GOTO1 FLDVAL,DMCB,(X'40',LRTKHMH),LRTK60H IF ANY FIELD CHGD            
         BE    *+8                                                              
         OI    VALSTAT,CALCPAY+CALCPNH  SET TO RE-CALCULATE                     
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   VRTKX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         LA    R2,LRTKHMH          HOURS/MINUTES                                
         MVC   TGFULL,=F'1'        ANY INCREMENT ALLOWED                        
         BAS   RE,VALHRMN                                                       
         MVC   TASDIHM,HALF                                                     
         SPACE 1                                                                
         LA    R2,LRTKTRVH         TRAVEL TIME                                  
         MVC   TGFULL,=F'1'        ANY INCREMENT ALLOWED                        
         BAS   RE,VALHRMN                                                       
         MVC   TASDITR,HALF                                                     
         SPACE 1                                                                
         MVI   TASDIST,0                                                        
         SPACE                                                                  
         CLI   LRTKENT,C'Y'        ENTIRE SCRIPT                                
         BNE   *+8                                                              
         OI    TASDIST,TASDIENQ                                                 
         SPACE 1                                                                
         CLI   LRTKPRT,C'Y'        PARTIAL SCRIPT                               
         BNE   *+8                                                              
         OI    TASDIST,TASDIPTQ                                                 
         SPACE 1                                                                
         CLI   LRTK60,C'Y'         >60 DAYS                                     
         BNE   *+8                                                              
         OI    TASDIST,TASDI60Q                                                 
         SPACE 1                                                                
VRTKX    LA    R2,LRTK60H          POINT R2 TO LAST FIELD                       
         B     XITR2                                                            
         EJECT                                                                  
*              ROUTINE VALIDATES CODE FIELDS                                    
         SPACE 1                                                                
*                                  P1=A(CODE FIELD)                             
CODEVAL  NTR1                                                                   
         L     R2,0(R1)                                                         
         LR    R3,R2               SAVE ITS ADDRESS                             
         BAS   RE,BUMP             ASSUME CORRESPONDING AMOUNT IS NEXT          
         SPACE 1                                                                
         TM    1(R3),X'20'         IF CODE FIELD IS PROTECTED                   
         BZ    *+10                                                             
         MVC   5(1,R3),7(R3)       SET INPUT LENGTH WITH OUTPUT LENGTH          
         SPACE 1                                                                
         TM    1(R2),X'20'         DO THE SAME FOR AMOUNT FIELD                 
         BZ    *+10                                                             
         MVC   5(1,R2),7(R2)                                                    
         SPACE 1                                                                
         MVI   BYTE,0                                                           
         CLI   5(R3),0             TEST INPUT IN CODE FIELD                     
         BNE   CVAL2                                                            
         CLI   5(R2),0             NOTHING IN CODE - IF THERE'S AN AMNT         
         BE    CVALX                                                            
         LR    R2,R3               REQUIRE CODE                                 
         B     FLDMISS                                                          
         SPACE 1                                                                
CVAL2    CLI   5(R2),0             HAVE CODE, SO AMOUNT IS REQ'D                
         BE    FLDMISS                                                          
         LR    R2,R3                                                            
         GOTO1 VALINUM             VALIDATE NUMERIC FIELD                       
CVALX    MVC   BYTE,8(R2)          RETURN EBCDIC CODE                           
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES NUMERIC INPUT                                  
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
NUMVAL   NTR1                                                                   
         MVI   ACTUAL,0                                                         
         CLI   5(R2),0             INPUT ALWAYS OPTIONAL                        
         BE    NUMVX                                                            
         GOTO1 VALINUM                                                          
NUMVX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES AMOUNT FIELDS                                  
         SPACE 1                                                                
*                                  R2=A(FIELD HEADER)                           
AMTVAL   NTR1                                                                   
         XC    FULL,FULL                                                        
         ZIC   RF,5(R2)            RF=INPUT LENGTH                              
         TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         IC    RF,7(R2)            USE OUTPUT LENGTH IF PROTECTED               
         LTR   RF,RF               RETURN CC NE IF NOT INPUT                    
         BZ    NO                                                               
         GOTO1 CASHVAL,DMCB,8(R2),(RF)  VALIDATE FOR 2 DEC. PLACES              
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         CLM   R2,8,=X'80'         TEST ALLOWING NEGATIVES                      
         BE    *+12                                                             
         TM    4(R1),X'80'         DON'T ALLOW NEGATIVE                         
         BO    AMTINV                                                           
         MVC   FULL,4(R1)          SAVE AMOUNT IN FULL                          
         B     YES                 RETURN CC EQUAL                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE AN HOURS/MINUTES FIELD                       
         SPACE 1                                                                
*                                  TGFULL BYTE 0 X'80'=NO MINIMUM               
*                                                X'20'=99 HR MAXIMUM            
*                                            1-3 = VALID INCREMENTS             
*                                  R2=A(FIELD)                                  
VALHRMN  NTR1                                                                   
         XC    HALF,HALF           PRE-CLEAR RETURN FIELD                       
         BAS   RE,AMTVAL           VALIDATE AMOUNT                              
         BNE   VALHRX                                                           
         ICM   RF,15,FULL          MOVE TO RF / TEST FOR ZERO                   
         BZ    VALHRX                                                           
         STH   RF,HALF             SAVE VALUE IN HALF                           
         SPACE 1                                                                
         TM    TGFULL,X'80'        UNLESS SPECIFIED                             
         BO    *+12                                                             
         C     RF,=F'100'          MINIMUM IS 1 HOUR                            
         BL    AMTINV                                                           
         TM    TGFULL,X'20'        99 HOURS MAXIMUM?                            
         BNO   VALHR20                                                          
         C     RF,=F'9900'                                                      
         BH    AMTINV                                                           
         B     *+12                                                             
VALHR20  C     RF,=F'2400'         MAXIMUM IS 24 HOURS                          
         BH    AMTINV                                                           
         XR    RE,RE                                                            
         D     RE,=F'100'          RF=N'HOURS                                   
         LR    R1,RE               R1=N'MINUTES                                 
         CH    R1,=H'60'           MUST BE LESS THAN SIXTY MINUTES              
         BNL   AMTINV                                                           
         SPACE                                                                  
         MVI   TGFULL,0            CLEAR FLAG                                   
         CLC   TGFULL,=F'1'        DON'T BOTHER IF TGFULL=1                     
         BE    VALHRX                                                           
         MH    RF,=H'60'             N'HOURS * 60 MINUTES                       
         AR    R1,RF               + N'MINUTES = TOTAL N'MINUTES                
         XR    R0,R0                                                            
         D     R0,TGFULL           INSURE VALID INTERVAL                        
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BNZ   AMTINV                                                           
VALHRX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS CAST RECORD FOR SYSCALC                             
         SPACE 1                                                                
CASTREC  NTR1                                                                   
         MVC   AIO,AIO2            USE ALTERNATE I/O AREA                       
         BAS   RE,SETTAL           SET TALENT FILES                             
         XC    TCACAST,TCACAST     PRE-CLEAR A(CAST RECORD)                     
         SPACE 1                                                                
         L     R4,AIO1             R4=A(CHECK RECORD)                           
         USING TLCKD,R4                                                         
         MVC   TGSSN,TLCKSSN       SET GLOBAL SSN                               
         MVC   TGCAT,TLCKCAT                  CATEGORY                          
         MVC   TGCSORT,TLCKSORT               CAST SORT KEY                     
         SPACE 1                                                                
         CLC   TGCSORT,HEXFFS      IF THIS IS UNION CHECK                       
         BNE   CAST20                                                           
         XC    TCSUBPNH,TCSUBPNH   INSURE SUBJ. TO P&H IS ZERO                  
         OI    TCINPUT,TCINPNH                                                  
         B     CASTX               SKIP THE REST                                
         SPACE 1                                                                
CAST20   GOTO1 RECVAL,DMCB,TLCACCDQ,(X'30',0)  READ CAST REC FOR UPDATE         
         BE    CAST24                                                           
         TM    INVSTAT2,TAINSRTH   NOT FOUND - OK IF RETRO PAYMENT              
         BO    CASTX                                                            
         B     ERRCAST             ELSE ERROR - MUST REOPOEN INVOICE            
         SPACE 1                                                                
CAST24   MVC   TCACAST,AIO         SET A(CAST RECORD)                           
         SPACE 1                                                                
         GOTO1 GETOV1,DMCB,TGUSCDE,TCOV1  GET 1ST OVERSCALE RATE                
         CLI   0(R1),X'FF'         IF IT'S AN AMOUNT                            
         BNE   CAST30                                                           
         MVC   TCOVAMT,TCOV1       SAVE IT IN CAST OVERRIDE FIELD               
         OI    TCCASTST,TCCAOVAM   SET CORRESPONDING STATUS                     
         TM    TCINPUT,TCINPAY     IF DON'T ALREADY HAVE PAY OVERRIDE           
         BO    *+14                                                             
         MVC   TCPAY,TCOV1         SAVE AS PAYMENT                              
         OI    TCINPUT,TCINPAY     SET WE HAVE OVERRIDE                         
         XC    TCOV1,TCOV1         CLEAR RATE                                   
         SPACE 1                                                                
*AST30   TM    TGUNEQU,AFM         SKIP FOR MUSICIANS                           
CAST30   GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BO    CASTX                                                            
         BAS   RE,GETTACR          GET APPROPRIATE APPLIED CREDITS EL.          
         BNE   CASTX                                                            
         L     R4,TGELEM           R4=A(TACR ELEMENT)                           
         USING TACRD,R4                                                         
         MVC   MYTACREL,TACREL     SAVE ENTIRE ELEMENT                          
         SPACE 1                                                                
         TM    TGUSSTA2,APPREUSE   IF CANDIDATE FOR APPLIED CREDITS             
         BZ    CAST50                                                           
         L     R1,TACRBAL          INSURE BALANCE >= AMT TO BE APPLIED          
         C     R1,TACRAPPL                                                      
         BL    ERRCRCRS                                                         
         MVI   TACREL,X'FF'        DELETE EL - SYSCALC WILL ADD NEW ONE         
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         B     CASTX                                                            
         SPACE 1                                                                
CAST50   CLI   APPLCODE,APPLSESS   ELSE IF SESSION OR                           
         BE    *+12                                                             
         CLI   APPLCODE,APPLHLD    OR HOLDING FEE CREDITS TAKEN                 
         BNE   CASTX                                                            
         TM    TACRBAL,X'80'       INSURE BALANCE NOT NEGATIVE                  
         BO    ERRCRNEG                                                         
         L     R1,TACRBAL          BALANCE                                      
         S     R1,APPLAMNT         + APPLIED AMOUNT = NEW BALANCE               
         C     R1,TACRAPPL         INSURE DOESN'T EXCEED TOTAL AMOUNT           
         BH    ERRCRAPP                                  TO BE APPLIED          
         ST    R1,TACRBAL          SAVE NEW BALANCE                             
         SPACE 1                                                                
CASTX    BAS   RE,SETCHK           SET CHECK FILES                              
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
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
*              GET RATE FOR THIS PERFORMER                                      
         SPACE 1                                                                
GETRATE  NTR1                                                                   
         GOTO1 TASYSCLC,DMCB,(RC),TCD,SYSCOMM  CALCULATE RATES                  
         SPACE 1                                                                
         L     R1,TCPAY                                                         
         S     R1,TCMDED           IF PAYMENT AMOUNT < MISC DED                 
         BNM   GETR5                                                            
         L     R2,APAYFLD          GIVE ERROR AT PAYMENT FIELD                  
         TM    TCINPUT,TCINPAY     IF PAYMENT AMOUNT OVERRIDDEN                 
         BO    GETR4                                                            
         TM    TCINPUT,TCINAPPL    ELSE IF APPLIED AMOUNT OVERRIDDEN            
         BZ    GETR4                                                            
         OC    AAPPLFLD,AAPPLFLD   THEN IF WE HAVE A(APPLIED AMOUNT)            
         BZ    GETR4                                                            
         L     R2,AAPPLFLD         SET CURSOR THERE INSTEAD                     
GETR4    B     AMTINV                                                           
         SPACE 1                                                                
GETR5    TM    VALSTAT,INPNH       IF P&H AMOUNT OVERRIDDEN                     
         BZ    *+10                                                             
         MVC   TCPNH,MYPNH         SET IT IN SYSCALC W/S                        
         SPACE 1                                                                
         TM    VALSTAT,INHNW       IF H&W AMOUNT OVERRIDDEN                     
         BZ    *+10                                                             
         MVC   TCHNW,MYHNW         SET IT IN SYSCALC W/S                        
         SPACE 1                                                                
         TM    TCPAY,X'80'         IF PAYMENT AMOUNT IS NEGATIVE                
         BZ    GETRX                                                            
         OC    TCAPPLCR,TCAPPLCR   AND THERE ARE APPLIED CREDITS                
         BZ    *+12                                                             
         ICM   R2,15,AAPPLFLD      THEN IF WE HAVE A(APPLIED AMOUNT)            
         BNZ   FLDINV              THEN IT'S BAD                                
         L     R2,APAYFLD          ELSE GIVE ERROR AT PAYMENT FIELD             
         B     FLDINV                                                           
         SPACE 1                                                                
GETRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE UPDATES CHECK RECORD                                     
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
CHECK    NTR1                                                                   
         LA    R2,AMTTAB           R2=A(AMOUNTS TABLE)                          
         USING AMTD,R2                                                          
CHK10    CLI   AMTD,X'FF'                                                       
         BE    CHKX                                                             
         ZIC   RE,AMTCALC                                                       
         LA    R1,TCTOTS(RE)                                                    
         L     R1,0(R1)            R1=AMOUNT FROM SYSCALC W/S                   
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    *+6                                                              
         LCR   R1,R1               COMPLEMENT AMOUNT                            
         SPACE 1                                                                
         IC    RE,AMTTAPD                                                       
         LA    RF,TAPDAMTS(RE)     RF=A(FIELD IN PAYMENT DETAILS EL.)           
         LA    RE,DIFFAMTS(RE)     RE=A(FIELD IN DIFFAMTS FOR INVOICE)          
         SPACE 1                                                                
         XR    R0,R0                                                            
         CLI   AMTCALC,TCAPPLCR-TCTOTS  IF THIS IS APPLIED AMOUNT               
         BNE   CHK30                                                            
         CLI   TAPDACDE,APPLWSPU   THEN IF APPLIED AMT WAS WSP UPGRADE          
         BE    CHK40                                                            
         CLI   TAPDACDE,APPLCAB    OR CABLE UPG THEN DIDN'T AFFECT INV          
         BE    CHK40                                                            
CHK30    L     R0,0(RF)            EXISTING AMOUNT                              
         LCR   R0,R0               (COMPLEMENTED)                               
         SPACE 1                                                                
CHK40    CLI   AMTCALC,TCAPPLCR-TCTOTS  IF THIS IS APPLIED AMOUNT               
         BNE   CHK50                                                            
         CLI   TCAPPLCD,APPLWSPU   THEN IF APPLIED AMT IS WSP UPGRADE           
         BE    CHK60                                                            
         CLI   TCAPPLCD,APPLCAB    OR CABLE UPG THEN DOESN'T AFFECT INV         
         BE    CHK60                                                            
CHK50    AR    R0,R1               + NEW AMOUNT                                 
         SPACE 1                                                                
CHK60    ST    R0,0(RE)            = DIFFERENCE TO ADD TO INVOICE               
         SPACE 1                                                                
         ST    R1,0(RF)            MOVE NEW AMOUNT TO PAYMENT DTLS. EL.         
         SPACE 1                                                                
         LA    R2,AMTNEXT          BUMP TO NEXT ENTRY IN TABLE                  
         B     CHK10               LOOP                                         
         SPACE 1                                                                
CHKX     MVC   TAPDACDE,TCAPPLCD   SET NEW APPLIED CODE                         
         MVC   TAPDICDE,TCEXPICD   AND NEW INCLUDE CODE                         
         SPACE 1                                                                
         BAS   RE,UPDTADW          UPDATE DUECOMP WITHHOLDING EL IF NEC         
         SPACE 1                                                                
         MVC   ELEMENT,SVELEM                                                   
         MVI   ELEMENT,TAPPELQ     SAVE PRE-PYMT/LIST PAYMENT DETAILS           
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE UPDATES DUE COMPANY WITHHOLDING EL ON CHECK              
         SPACE 1                                                                
UPDTADW  NTR1                                                                   
         BAS   RE,UPDDUC           IF WILL UPDATE DUE COMPANY RECORD            
         BNE   UPDWX                                                            
*                                  RETURNS R2=DIFF FOR DUE COMPANY              
         SPACE 1                                                                
         MVI   ELCODE,TADWELQ      GET DUE COMPANY WITHHOLDING EL.              
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   UPDWX               NOT FOUND - GET OUT                          
         USING TADWD,R4                                                         
         L     R1,TADWREC          UPDATE AMOUNT RECEIVED                       
         AR    R1,R2                                                            
         ST    R1,TADWREC                                                       
         LCR   R2,R2               AND UPDATE BALANCE                           
         A     R2,TADWBAL                                                       
         ST    R2,TADWBAL                                                       
UPDWX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE DETERMINES IF DUE COMPANY RECORD NEEDS UPDATE            
         SPACE 1                                                                
UPDDUC   DS    0H                                                               
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         BZ    UPDDNO                                                           
         TM    PAYPST1,TAPDPBNP    AND NOT BNP                                  
         BO    UPDDNO                                                           
         L     R2,DIFFAMTS+TAPDPAYI-TAPDAMTS AND PAYMENT AMOUNT CHANGED         
         A     R2,DIFFAMTS+TAPDPAYC-TAPDAMTS                                    
         A     R2,DIFFAMTS+TAPDREXP-TAPDAMTS                                    
         BZ    UPDDNO                                                           
         CR    RE,RE               RETURN CC EQ - UPDATE DUE COMPANY            
         BR    RE                  RETURN R2 = PAYMENT DIFFERENCE               
         SPACE 1                                                                
UPDDNO   LTR   RE,RE               RETURN CC NE - NO UPDATE NECESSARY           
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO UPDATE/ADD FIELDS CHANGED ELEMENT                     
         SPACE 1                                                                
*                                  P1=A(FIRST FIELD)                            
*                                  P2=A(LAST FIELD) OR 0 FOR EOS                
FLDSIN   NTR1                                                                   
         LM    R2,R3,0(R1)         LOAD PARAMETERS                              
         SPACE 1                                                                
         MVI   ELCODE,TAFCELQ      SET TO DELETE EXISTING ELEMENT               
         GOTO1 REMELEM                                                          
         LA    R4,ELEMENT          REMELEM SAVES EL. HERE IF IT EXISTED         
         USING TAFCD,R4                                                         
         CLI   TAFCEL,TAFCELQ      IF HAVE ELEMENT                              
         BE    FLDI10              THEN SKIP                                    
         SPACE 1                                                                
         MVI   TAFCEL,TAFCELQ      ELSE BUILD NEW ELEMENT                       
         MVI   TAFCLEN,TAFCLNQ                                                  
         SPACE 1                                                                
FLDI10   TM    4(R2),X'20'         IF THIS FIELD CHANGED                        
         BO    FLDI40                                                           
         CLI   1(R2),X'FF'         IGNORE NOP FIELDS                            
         BE    FLDI40                                                           
         TM    1(R2),X'02'         MUST HAVE EXTENDED HEADER                    
         BZ    FLDI40                                                           
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         AR    RF,R2               RF=A(FIELD NUMBER)                           
         SPACE 1                                                                
         LA    R1,TAFCFLDS         LOOP THROUGH ELEMENT                         
FLDI20   CLI   0(R1),0             END OF ELEMENT - NEED TO ADD NEW FLD         
         BE    FLDI30                                                           
         CLC   0(1,R1),0(RF)       IF FIELD NUMBER HERE ALREADY                 
         BE    FLDI40              THEN NOTHING TO DO                           
         LA    R1,L'TAFCFLDS(R1)   ELSE KEEP ON LOOKING                         
         B     FLDI20                                                           
         SPACE 1                                                                
FLDI30   MVC   0(1,R1),0(RF)       ADD NEW FIELD NUMBER TO ELEMENT              
         ZIC   RE,TAFCLEN                                                       
         LA    RE,1(RE)            AND BUMP L'ELEMENT                           
         STC   RE,TAFCLEN                                                       
         SPACE 1                                                                
FLDI40   BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         LTR   R3,R3               IF NO END FIELD DEFINED                      
         BZ    FLDI10              THEN CONTINUE                                
         CR    R2,R3               ELSE TEST PAST END                           
         BNH   FLDI10              NO                                           
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HIGHLIGHT FIELDS BASED ON FIELD CHANGED EL.           
         SPACE 1                                                                
*                                  P1=A(FIRST FIELD)                            
*                                  P2=A(LAST FIELD) OR 0 FOR EOS                
FLDSOUT  NTR1                                                                   
         LM    R2,R3,0(R1)         LOAD PARAMETERS                              
         SPACE 1                                                                
         MVI   ELCODE,TAFCELQ      LOOK FOR ELEMENT                             
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   FLDOX               NOT FOUND - GET OUT                          
         USING TAFCD,R4                                                         
         SPACE 1                                                                
FLDO10   TM    1(R2),X'02'         LOOK AT FIELDS WITH EXTENDED HEADERS         
         BZ    FLDO40                                                           
         CLI   1(R2),X'FF'         IGNORE NOP FIELDS                            
         BE    FLDO40                                                           
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         AR    RF,R2               RF=A(FIELD NUMBER)                           
         SPACE 1                                                                
         LA    R1,TAFCFLDS         LOOP THROUGH ELEMENT                         
         ZIC   R0,TAFCLEN                                                       
         SH    R0,=Y(TAFCLNQ)      R0=N'FIELDS IN ELEMENT                       
FLDO20   CLC   0(1,R1),0(RF)       MATCH ON FIELD NUMBER                        
         BE    FLDO30                                                           
         LA    R1,L'TAFCFLDS(R1)   ELSE KEEP ON LOOKING                         
         BCT   R0,FLDO20                                                        
         B     FLDO40              NOT IN ELEMENT - SKIP TO NEXT FIELD          
         SPACE 1                                                                
FLDO30   OI    1(R2),X'08'         FOUND MATCH - HIGHLIGHT FIELD                
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
FLDO40   BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         LTR   R3,R3               IF NO END FIELD DEFINED                      
         BZ    FLDO10              THEN CONTINUE                                
         CR    R2,R3               ELSE TEST PAST END                           
         BNH   FLDO10              NO                                           
         SPACE 1                                                                
FLDOX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE UPDATES FTRACK TRACKING RECORDS IF NECESSARY             
         SPACE 1                                                                
FTRACK   NTR1                                                                   
         CLC   TGCSORT,HEXFFS      SKIP IF THIS IS UNION CHECK                  
         BE    FTRX                                                             
         TM    TCRTRN,TCRTTACR     IF SYSCALC CHANGED TACR ELEMENT              
         BZ    FTRX                                                             
         CLC   MYTACREL,TCTACREL   AND IT ACTUALLY CHANGED                      
         BNE   *+12                                                             
         TM    TCINPUT,TCINAPPL    OR IF APPLIED CREDITS INPUT                  
         BZ    FTRX                                                             
         BAS   RE,SETTAL           SET TALENT FILES                             
         MVC   AIO,AIO2            USE ALTERNATE I/O AREA                       
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY              INITIALIZE KEY                               
         USING TLFTD,R3                                                         
         MVI   TLFTCD,TLFTCDQ      RECORD CODE                                  
         MVC   TLFTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLFTCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLFTCAST,TGCSORT+4  CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT(6),APPLPD  FIXED CYCLES DATES                           
         XC    TLFTSTRT(6),HEXFFS               (COMPLEMENTED)                  
         SPACE 1                                                                
         GOTO1 HIGH                GET FIRST DIRECTORY REC FOR CYCLE            
         SPACE 1                                                                
         CLC   TLFTKEY(TLFTTRK-TLFTD),KEYSAVE  TEST FOUND A GOOD ONE            
         BNE   NOFTRACK                                                         
         CLC   TLFTINV,TIFINV      IF FIRST RECORD ISN'T FOR THIS INV.          
         BNE   NOFTRACK            GET OUT (CAN'T DELETE IN MIDDLE)             
         SPACE 1                                                                
***********************************************************************         
* THIS CODE IS TO HANDLE CONDITION WHERE THERE WERE NO CREDITS TAKEN            
* ORIGINALLY BUT NOW THEY ARE AND SO I HAVE TO ADD FTRACK TRACKING REC          
***********************************************************************         
*******  MVC   KEY,KEYSAVE         RESTORE KEY                                  
*******  MVC   TGINV,TIFINV                                                     
*******  GOTO1 BLDTRK,DMCB,TLFTTRK-TLFTD,AIO  BUILD TRACKING RECORD             
*******  XC    TGINV,HEXFFS                                                     
*******  L     R3,AIO                                                           
*******  MVC   TLFTINV,TIFINV                                                   
*******  BAS   RE,MYADDREC                                                      
***********************************************************************         
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'       SET TO READ FOR UPDATE                       
         GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAGTELQ      GET TRACKING ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGTD,R4                                                         
         MVC   TAGTPAY,TCPAY       SET UPDATED PAYMENT                          
         MVC   TAGTPNH,TCPNH                   P&H                              
         L     R1,TCAPPLIC         AMOUNT TO BE APPLIED                         
         A     R1,TCAPPLCR         - APPLIED AMOUNT                             
         ST    R1,TAGTCRD          = NEW CREDIT AMOUNT                          
         MVC   TAGTBAL,TCTACREL+TACRBAL-TACRD  BALANCE                          
         MVC   TAGTAPPL,TCAPPLCR               APPLIED CREDITS                  
         MVC   TAGTPST1,PAYPST1                PAYMENT STATUS                   
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK UPDATED RECORD                    
         SPACE 1                                                                
         BAS   RE,SETCHK           SET CHECK FILES                              
         MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
FTRX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO UPDATE DUE COMPANY RECORDS FOR CREDIT PYMTS           
         SPACE 1                                                                
DUECOMP  NTR1                                                                   
         BAS   RE,UPDDUC           TEST WHETHER NEED TO UPDATE DUECOMP          
         BNE   DUEX                                                             
         SPACE 1                                                                
         BAS   RE,SETTAL           SET TALENT FILES                             
         MVC   AIO,AIO2            USE ALTERNATE I/O AREA                       
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY              INITIALIZE KEY                               
         USING TLDUD,R3                                                         
         MVI   TLDUCD,TLDUCDQ      RECORD CODE                                  
         MVC   TLDUSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         L     R4,AIO1                                                          
         USING TATID,R4                                                         
         MVI   ELCODE,TATIELQ      IF A CORP ID NUMBER IS PRESENT               
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   TLDUSSN,TATIID      USE IT                                       
         GOTO1 HIGH                                                             
         B     DUE20                                                            
         SPACE 1                                                                
DUE10    GOTO1 SEQ                 TRY NEXT                                     
         SPACE 1                                                                
DUE20    CLC   TLDUKEY(TLDUDUC-TLDUD),KEYSAVE  TEST FOUND A GOOD REC.           
         BNE   DUEX                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TADUELQ      GET DUE COMPANY DETAILS ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R4                                                         
         TM    TADUSTAT,TADUSAUT   TEST THIS RECORD WAS ADDED BY PAY            
         BZ    DUE10                                                            
         CLC   TADUCINV,TIFINV     AND INVOICE NUMBER MATCHES                   
         BNE   DUE10                                                            
         LCR   R2,R2               COMPLEMENT PAYMENT DIFFERENCE                
         A     R2,TADUDUE          AND UPDATE AMOUNT DUE                        
         ST    R2,TADUDUE                                                       
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK UPDATED RECORD                    
         SPACE 1                                                                
DUEX     BAS   RE,SETCHK           SET CHECK FILES                              
         MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE WRITING SCREEN RECORDS                         
         SPACE 1                                                                
PUTSCRN  NTR1                                                                   
         BAS   RE,SETTAL           SET TALENT FILES                             
         MVC   AIO,AIO2            USE ALTERNATE I/O AREA                       
         SPACE 1                                                                
         L     R3,AIO              R3=A(SCREEN RECORD)                          
         USING TLSCD,R3                                                         
         XC    TLSCKEY(50),TLSCKEY BUILD NEW RECORD                             
         SPACE 1                                                                
         MVI   TLSCCD,TLSCCDQ      SCREEN RECORD CODE                           
         MVC   TLSCAGY,TGAGY       AGENCY                                       
         MVC   TLSCINV,TIFINV      INVOICE NUMBER                               
         SPACE 1                                                                
         TM    INVSTAT2,TAINSSCR   IF INVOICE WAS PAID BY CLIENT                
         BZ    *+16                                                             
         MVC   TLSCINV(3),INVIDTE  KEY USES ASSIGN DATE                         
         MVC   TLSCINV+3(3),INVITIM            AND TIME                         
         SPACE 1                                                                
         ZIC   R1,LSTPAYPG         LAST PAGE NUMBER FROM PAY                    
         ZIC   R0,THISPAGE         PLUS LOCAL PAGE NUMBER                       
         AR    R1,R0                                                            
         STC   R1,TLSCPG           IS PAGE NUMBER OF THIS RECORD                
         STC   R1,SCRNPAGE         SAVE FOR INVOICE RECORD UPDATE               
         SPACE 1                                                                
         MVC   TLSCSCR,TWASCR      SCREEN NUMBER                                
         MVC   TLSCLEN,DATADISP    INIT. RECORD LENGTH                          
         SPACE 1                                                                
         L     R2,ALASTDAT         R2=A(MESSAGE AREA)                           
         NI    1(R2),X'DF'         INSURE MESSAGE AREA UNPROT FOR WRITE         
         SPACE 1                                                                
         BAS   RE,SCRNELS          ADD SCREEN ELEMENTS                          
         SPACE 1                                                                
         BAS   RE,MYADDREC         ADD RECORD                                   
         SPACE 1                                                                
         OI    1(R2),X'20'         RESTORE MESSAGE AREA TO PROTECTED            
         SPACE 1                                                                
         BAS   RE,SETCHK           SET CHECK FILES                              
         MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD SCREEN ELEMENTS TO SCREEN RECORD                  
         SPACE 1                                                                
*        USING TLSCD,R3            R3=A(SCREEN RECORD)                          
SCRNELS  NTR1                                                                   
         LA    R2,CONTAGH          R2=START ADDRESS FOR SCREEN SAVING           
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          R4=A(SCREEN ELEMENT)                         
         USING TASCD,R4                                                         
         MVI   TASCEL,TASCELQ      ELEMENT CODE                                 
         SPACE 1                                                                
SCRN4    LR    RF,R2               RF=A(1ST FIELD TO SAVE), R2=A(NEXT)          
         XR    R0,R0                                                            
         XR    R1,R1               R1=L'ALL FIELDS TO SAVE                      
SCRN6    CLI   0(R2),0             TEST FOR SCREEN END                          
         BE    SCRN8                                                            
         IC    R0,0(R2)            R0=L'FIELD                                   
         AR    R1,R0                                                            
         CH    R1,=H'210'          INSURE ELEMENT DOESN'T GET TOO BIG           
         BH    *+10                                                             
         AR    R2,R0               BUMP TO NEXT FIELD                           
         B     SCRN6                                                            
         SR    R1,R0               REDUCT TOTAL BY L'CURRENT FIELD              
         SPACE 1                                                                
SCRN8    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TASCDATA,0(RF)      MOVE BLOCK OF FIELDS TO EL.                  
         SPACE 1                                                                
         SR    RF,RA               SET DISPLACEMENT TO 1ST FIELD IN EL.         
         STH   RF,TASCDISP                                                      
         LA    RE,TASCLNQ+1(R1)    SET L'ELEMENT                                
         STC   RE,TASCLEN                                                       
         AH    RE,TLSCLEN          ADD L'THIS ELEMENT TO L'REC SO FAR           
         CH    RE,=H'2000'                                                      
         BL    SCRN12              OK TO ADD THIS ELEMENT                       
         SPACE 1                                                                
         OI    TLSCSTAT,TLSCSCON   IT'S TOO LONG - SET CONTINUATION BIT         
         BAS   RE,MYADDREC         AND ADD WHAT WE HAVE SO FAR                  
         ZIC   RE,TLSCSEQ                                                       
         AH    RE,=H'1'            BUMP SEQUENCE NUMBER                         
         STC   RE,TLSCSEQ                                                       
         MVC   TLSCLEN,DATADISP    RESET RECORD LENGTH                          
         MVI   TLSCSTAT,0          CLEAR STATUS                                 
         MVI   TLSCELEM,0                                                       
         SPACE 1                                                                
SCRN12   GOTO1 ADDL                ADD ELEMENT TO RECORD                        
         SPACE 1                                                                
         CLI   0(R2),0             IF WE HAVEN'T REACHED E-O-S                  
         BNE   SCRN4               BUILD ANOTHER ELEMENT                        
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ROUTINE TO ADD/WRITE A SCREEN RECORD                       
         SPACE 1                                                                
MYADDREC NTR1                                                                   
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
         BNE   MYADR4                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         BO    *+14                                                             
         CLI   TLDRCD,TLSCCDQ      EXCEPT FOR SCREEN RECORDS                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDRKEY,KEYSAVE     RESTORE ORIGINAL KEY                         
         MVC   TLDRSTAT,KEYSAVE+TLDRSTAT-TLDRD  AND STATUS                      
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
         MVC   AIO,AIO3            NOW SET TO GET THE RECORD                    
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R4,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         B     MYADRX                                                           
         SPACE 1                                                                
MYADR4   MVC   KEY,KEYSAVE         RECORD NOT FOUND - RESET KEY                 
         GOTO1 ADDREC              AND ADD IT                                   
         SPACE 1                                                                
MYADRX   NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE UPDATES INVOICE RECORD                                   
         SPACE 1                                                                
INVOICE  NTR1                                                                   
         BAS   RE,SETTAL           SET TALENT FILES                             
         MVC   AIO,AIO3            USE ALTERNATE I/O AREA                       
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'30',0)  READ INVOICE FOR UPDATE           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            GET INVOICE DETAILS EL.                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         CLC   TAINLPG,SCRNPAGE    TEST WE HAVE NEW LAST PAGE NUMBER            
         BNL   INV20                                                            
         TM    PAYPST1,TAPDPCHG    IF PAYMENT HASN'T BEEN CHANGED PREV.         
         BO    *+10                                                             
         MVC   TAINLPGP,TAINLPG    THEN SAVE LAST PAGE NUMBER FROM PAY          
         MVC   TAINLPG,SCRNPAGE    SET NEW LAST PAGE NUMBER                     
         SPACE 1                                                                
INV20    L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL            GET PAYMENT DETAILS EL.                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         OI    TAPDPST1,TAPDPCHG   SET PAYMENT HAS CHANGED                      
         MVC   PAYPST1,TAPDPST1    SAVE NEW PAYMENT STATUS BYTE                 
         SPACE 1                                                                
         LA    RE,TAPDAMTS         RE=A(AMOUNTS IN INVOICE RECORD)              
         LA    RF,DIFFAMTS         RF=A(DIFFERENCES FROM CHECK RECORD)          
         LA    R0,TAPDAMTL/4       R0=N'AMOUNTS                                 
         SPACE 1                                                                
INV30    L     R1,0(RE)            OLD AMOUNT                                   
         A     R1,0(RF)            + DIFFERENCE                                 
         ST    R1,0(RE)            = NEW AMOUNT                                 
         LA    RE,4(RE)            BUMP TO NEXT AMOUNT                          
         LA    RF,4(RF)             AND TO NEXT DIFFERENCE                      
         BCT   R0,INV30            LOOP                                         
                                                                                
         ICM   R1,15,TAPDNTNW                                                   
         ICM   RF,15,DIFFNTNW                                                   
         AR    R1,RF                                                            
         STCM  R1,15,TAPDNTNW                                                   
                                                                                
         GOTO1 PUTREC              WRITE BACK INVOICE RECORD                    
         SPACE 1                                                                
         BAS   RE,SETCHK           SET CHECK FILES                              
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS WHEN PRINTING                              
         SPACE 1                                                                
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   PRHX                                                             
         MVC   ATHISLST,AFRSTREC   SET A(CURRENT LINE TO 1ST LINE)              
         BAS   RE,DISPLAY          DISPLAY RECORD TO FIRST SCREEN LINE          
         SPACE 1                                                                
         MVI   BYTE,C'P'           PRINT THE LINE                               
         GOTO1 PRTSCRN,DMCB,ATHISLST,ATHISEND,P-1                               
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'01',ATHISLST),ATHISEND  CLEAR THE LINE            
         SPACE 1                                                                
         AP    COUNTER,=P'1'       ADD TO CAST COUNTER                          
         SPACE 1                                                                
PRHX     B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK FOR REPORT PRINTING                                
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         MVC   HEAD4+9(6),SPYAGY   AGENCY CODE                                  
         MVC   HEAD4+17(16),AGYNAME       NAME                                  
         SPACE 1                                                                
         MVC   HEAD5+9(6),SPYINV   INVOICE NUMBER                               
         SPACE 1                                                                
         MVC   HEAD4+65(12),SPYCID COMMERCIAL ID                                
         MVC   HEAD5+55(24),SPYCIDN           NAME                              
         SPACE 1                                                                
         BAS   RE,SETSCRN          SET A(SCREEN TABLE FOR BLDTWA)               
         LA    R4,BLOCK            RETURNS HEADINGS ELEMENT IN BLOCK            
         USING TWAELEMD,R4                                                      
         ZIC   R1,TWAEFLN          R4=L'HEADINGS                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD7(0),TWAEDTA    MOVE IN HEADINGS                             
         B     XIT                                                              
         EJECT                                                                  
*              FILE SETTING ROUTINES                                            
         SPACE 3                                                                
SETCHK   DS    0H                  SET CHECK FILES                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         SPACE 3                                                                
SETTAL   DS    0H                  SET TALENT FILES                             
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
         BR    RE                                                               
         EJECT                                                                  
*              ERRORS, EXITS                                                    
*                                                                               
NOTPAID  MVI   ERROR,ERNOTPD       INVOICE NOT PAID                             
         B     THEEND                                                           
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
*                                                                               
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
*                                                                               
ERRLOCK  MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     ERREND                                                           
*                                                                               
ERRCAST  MVI   ERROR,ERCASTRC      CAN'T FIND CAST RECORD - MUST REOPEN         
         B     ERREND                                                           
*                                                                               
ERRCRCRS MVI   ERROR,ERCRCRS       CREDITS TAKEN ALREADY - MUST REOPEN          
         B     ERREND                                                           
*                                                                               
ERRCRNEG MVI   ERROR,ERCRNEG       FTRACK BALANCE IS NEG. - MUST REOPEN         
         B     ERREND                                                           
*                                                                               
ERRCRAPP MVI   ERROR,ERCRAPP       FTRACK BAL EXCEEDS TOT - MUST REOPEN         
         B     ERREND                                                           
*                                                                               
NOFTRACK MVI   ERROR,ERNOFTRK      FTRACK TRACKING RECORD NOT FIRST             
         OI    TRNSTAT,TRNABEND    SET TO ABEND TO UNWIND TRANSACTION           
         B     ERREND                                                           
*                                                                               
NOPYML   MVC   MYMSGNO,=Y(ERNOPYML)                                             
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
*                                                                               
ERREND   L     R2,ATHISLST         POINT TO 1ST LIST FIELD                      
         B     THEEND                                                           
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
XITR2    XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3'   ',CL8'HISTORY ',CL8'DISPLAY'                              
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
HNWMSG   DC    C'*** H&&W changed - adjust union check if necessary ***X        
               '                                                                
HNWDSP   EQU   11                                                               
RELO     DS    F                                                                
NLINES   EQU   6                                                                
LTCMNT   DC    C'^Cmnt'                                                         
HEXFFS   DC    6X'FF'                                                           
*                                                                               
#ACDE    EQU   43                  FIELD ID NUMBERS                             
#PRO     EQU   49                                                               
#APPL    EQU   45                                                               
#RHM     EQU   53                                                               
#ENT     EQU   55                                                               
#PRT     EQU   57                                                               
#60      EQU   59                                                               
#ICDE    EQU   63                                                               
#REXP    EQU   65                                                               
#PAY     EQU   67                                                               
#SPNH    EQU   69                                                               
#MDED    EQU   71                                                               
#DUES    EQU   85                                                               
#AGT     EQU   73                                                               
#CMT     EQU   79                                                               
#SPT     EQU   51                                                               
#DAY     EQU   53                                                               
#OT      EQU   55                                                               
#DT      EQU   57                                                               
#TAG     EQU   77                                                               
#TRV     EQU   59                                                               
#PDW     EQU   61                                                               
#HRM     EQU   75                                                               
#PNH     EQU   81                                                               
#HNW     EQU   83                                                               
*                                                                               
*              TABLE MATCHES SYSCALC W/S TO PAYMENT DETAILS ELEMENT             
         SPACE 1                                                                
AMTTAB   DS    0XL2                                                             
         DC    AL1(TAPDGRS-TAPDAMTS,TCGROSS-TCTOTS)                             
         DC    AL1(TAPDAPPL-TAPDAMTS,TCAPPLCR-TCTOTS)                           
         DC    AL1(TAPDGUAR-TAPDAMTS,TCGUAR-TCTOTS)                             
         DC    AL1(TAPDPAYI-TAPDAMTS,TCPAYI-TCTOTS)                             
         DC    AL1(TAPDPAYC-TAPDAMTS,TCPAYC-TCTOTS)                             
         DC    AL1(TAPDREXP-TAPDAMTS,TCEXP-TCTOTS)                              
         DC    AL1(TAPDSPNH-TAPDAMTS,TCSUBPNH-TCTOTS)                           
         DC    AL1(TAPDMDED-TAPDAMTS,TCMDED-TCTOTS)                             
         DC    AL1(TAPDDUES-TAPDAMTS,TCDUES-TCTOTS)                             
         DC    AL1(TAPDPNH-TAPDAMTS,TCPNH-TCTOTS)                               
         DC    AL1(TAPDHNW-TAPDAMTS,TCHNW-TCTOTS)                               
         DC    AL1(TAPDINR-TAPDAMTS,TCINR-TCTOTS)                               
         DC    AL1(TAPDNTNW-TAPDAMTS,TCEXP-TCTOTS)                              
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DEFAULT DISPLAY TABLE                                            
         SPACE 2                                                                
DISPTABS DC    X'00'               START OF DISPLAY TABLES                      
DISPSOP  DS    0C                                                               
DISPPRT  DS    0C                                                               
DISPDEF  DS    0C                                                               
         DC    AL2(DISAPPL-T702C0,VALAPPL-T702C0)                               
         DC    X'FE'                                                            
         DC    AL2(DISREXP-T702C0,VALREXP-T702C0)                               
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISMDED-T702C0,VALMDED-T702C0)                               
         DC    AL2(DISAGT-T702C0,VALAGT-T702C0)                                 
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETHNW-T702C0)   NEED TO SET H&W IF NOT DISPLAYED          
         DC    AL2(0,SETDUES-T702C0)  NEED TO SET DUES IF NOT DISPLAYED         
         DC    X'FF'                                                            
         SPACE 3                                                                
*              DISPLAY TABLE WHEN USING OTH OPTION                              
         SPACE 1                                                                
DISPOTH  DS    0C                                                               
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISPNH-T702C0,VALPNH-T702C0)                                 
         DC    AL2(DISHNW-T702C0,VALHNW-T702C0)                                 
         DC    AL2(DISDUES-T702C0,VALDUES-T702C0)                               
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETREXP-T702C0)  NEED TO SET REXP IF NOT DISPLAYED         
         DC    AL2(0,SETMDED-T702C0)  NEED TO SET MDED IF NOT DISPLAYED         
         DC    X'FF'                                                            
         SPACE 3                                                                
*              TV SESSION DISPLAY TABLE                                         
         SPACE 1                                                                
DISPBSS  DS    0C                                                               
         DC    AL2(DISBSS-T702C0,VALBSS-T702C0)                                 
         DC    X'FE'                                                            
         DC    AL2(DISREXP-T702C0,VALREXP-T702C0)                               
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISMDED-T702C0,VALMDED-T702C0)                               
         DC    AL2(DISAGT-T702C0,VALAGT-T702C0)                                 
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETHNW-T702C0)   NEED TO SET H&W IF NOT DISPLAYED          
         DC    AL2(0,SETDUES-T702C0)  NEED TO SET DUES IF NOT DISPLAYED         
         DC    X'FF'                                                            
         EJECT                                                                  
*              RADIO SESSION DISPLAY TABLE                                      
         SPACE 1                                                                
DISPBSR  DS    0C                                                               
         DC    AL2(DISBSR-T702C0,VALBSR-T702C0)                                 
         DC    AL2(DISAPPL-T702C0,VALAPPL-T702C0)                               
         DC    X'FE'                                                            
         DC    AL2(DISREXP-T702C0,VALREXP-T702C0)                               
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISMDED-T702C0,VALMDED-T702C0)                               
         DC    AL2(DISAGT-T702C0,VALAGT-T702C0)                                 
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETHNW-T702C0)   NEED TO SET H&W IF NOT DISPLAYED          
         DC    AL2(0,SETDUES-T702C0)  NEED TO SET DUES IF NOT DISPLAYED         
         DC    X'FF'                                                            
         SPACE 3                                                                
*              MUSIC SESSION DISPLAY TABLE                                      
         SPACE 1                                                                
DISPBSM  DS    0C                                                               
         DC    AL2(DISBSM-T702C0,VALBSM-T702C0)                                 
         DC    AL2(DISAPPL-T702C0,VALAPPL-T702C0)                               
         DC    X'FE'                                                            
         DC    AL2(DISREXP-T702C0,VALREXP-T702C0)                               
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISMDED-T702C0,VALMDED-T702C0)                               
         DC    AL2(DISAGT-T702C0,VALAGT-T702C0)                                 
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETHNW-T702C0)   NEED TO SET H&W IF NOT DISPLAYED          
         DC    AL2(0,SETDUES-T702C0)  NEED TO SET DUES IF NOT DISPLAYED         
         DC    X'FF'                                                            
         EJECT                                                                  
*              INTERACTIVE VOICE RECORDING DISPLAY TABLE                        
         SPACE 1                                                                
DISPIVR  DS    0C                                                               
         DC    AL2(DISIVR-T702C0,VALIVR-T702C0)                                 
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISMDED-T702C0,VALMDED-T702C0)                               
         DC    AL2(DISAGT-T702C0,VALAGT-T702C0)                                 
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETHNW-T702C0)   NEED TO SET H&W IF NOT DISPLAYED          
         DC    AL2(0,SETDUES-T702C0)  NEED TO SET DUES IF NOT DISPLAYED         
         DC    X'FF'                                                            
         EJECT                                                                  
*              INDUSTRIAL RADIO SESSION DISPLAY TABLE                           
         SPACE 1                                                                
DISPDIO  DS    0C                                                               
         DC    AL2(DISDIO-T702C0,VALDIO-T702C0)                                 
         DC    X'FE'                                                            
         DC    AL2(DISREXP-T702C0,VALREXP-T702C0)                               
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISMDED-T702C0,VALMDED-T702C0)                               
         DC    AL2(DISAGT-T702C0,VALAGT-T702C0)                                 
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETHNW-T702C0)   NEED TO SET H&W IF NOT DISPLAYED          
         DC    AL2(0,SETDUES-T702C0)  NEED TO SET DUES IF NOT DISPLAYED         
         DC    X'FF'                                                            
         EJECT                                                                  
*              RETAKE DISPLAY TABLE                                             
         SPACE 1                                                                
DISPRTK  DS    0C                                                               
         DC    AL2(DISRTK-T702C0,VALRTK-T702C0)                                 
         DC    X'FE'                                                            
         DC    AL2(DISREXP-T702C0,VALREXP-T702C0)                               
         DC    AL2(DISPAY-T702C0,VALPAY-T702C0)                                 
         DC    AL2(DISSPNH-T702C0,VALSPNH-T702C0)                               
         DC    AL2(DISMDED-T702C0,VALMDED-T702C0)                               
         DC    AL2(DISAGT-T702C0,VALAGT-T702C0)                                 
         DC    AL2(DISCAST-T702C0,SETCAST-T702C0)                               
         DC    AL2(DISCMT-T702C0,VALCMT-T702C0)                                 
         DC    AL2(0,SETHNW-T702C0)   NEED TO SET H&W IF NOT DISPLAYED          
         DC    AL2(0,SETDUES-T702C0)  NEED TO SET DUES IF NOT DISPLAYED         
         DC    X'FF'                                                            
         EJECT                                                                  
*              DEFAULT TWABLD ELEMENT TABLE                                     
         SPACE 2                                                                
SOPTAB   DS    0C                                                               
PRTTAB   DS    0C                                                               
DEFTAB   DC    AL1(1,TWAELLNQ+DEFDATQ,2,2,DEFDATQ),X'28',AL1(0)                 
DEFDAT   EQU   *                                                                
         DC    C'            ^A^  ^Applied^ '                                   
         DC    C'I^ Reimb Exp^  ^Payment^ ^Subj-P&&H^ ^Misc Ded^^Agnt'          
DEFDATQ  EQU   *-DEFDAT                                                         
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,15,01),X'00',AL1(#ACDE) APPLIED CODE            
         DC    AL1(1,TWAELLNQ,0,17,10),X'00',AL1(#APPL) APPLIED AMOUNT          
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,29,01),X'00',AL1(#ICDE) INCLUDE CODE            
         DC    AL1(1,TWAELLNQ,0,31,10),X'00',AL1(#REXP) REIMB. EXPENSES         
         DC    AL1(1,TWAELLNQ,0,42,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,53,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,64,10),X'00',AL1(#MDED) MISC DEDUCTIONS         
         DC    AL1(1,TWAELLNQ,0,76,04),X'00',AL1(#AGT)  AGENT                   
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         SPACE 2                                                                
OTHDAT   EQU   *                                                                
         DC    C'  ^Payment^ ^Subj-P&&H^     ^P&&H^     ^H&&W^    ^D'           
         DC    C'ues'                                                           
OTHDATQ  EQU   *-OTHDAT                                                         
         DC    AL1(1,TWAELLNQ,0,29,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,40,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,51,09),X'00',AL1(#PNH)  P&H                     
         DC    AL1(1,TWAELLNQ,0,61,09),X'00',AL1(#HNW)  H&W                     
         DC    AL1(1,TWAELLNQ,0,71,09),X'00',AL1(#DUES) UNION DUES              
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         SPACE 2                                                                
BOTTAB   DC    AL1(1,TWAELLNQ,0,04,76),X'28',AL1(0)  DUMMY FLD FOR LAST         
         DC    AL1(1,TWAELLNQ+BOTDATQ,X'80'+24,02,BOTDATQ),X'28',AL1(0)         
BOTDAT   EQU   *                                                                
         DC    C'PF13=History'                                                  
BOTDATQ  EQU   *-BOTDAT                                                         
         DC    X'00'                                                            
         EJECT                                                                  
*              TWABLD ELEMENT TABLE FOR TV SESSIONS                             
         SPACE 1                                                                
BSSTAB   DC    AL1(1,TWAELLNQ+BSSDATQ,2,2,BSSDATQ),X'28',AL1(0)                 
BSSDAT   EQU   *                                                                
         DC    C'Sp^Dy^Ot^Dt^Travl^Pd-wd^Tg^'                                   
         DC    C'I^^Reimb Exp^  ^Payment^ ^Subj-P&&H^ ^Misc Ded^^Agnt'          
BSSDATQ  EQU   *-BSSDAT                                                         
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,02,02),X'00',AL1(#SPT)  SPOTS                   
         DC    AL1(1,TWAELLNQ,0,05,02),X'00',AL1(#DAY)  DAYS                    
         DC    AL1(1,TWAELLNQ,0,08,02),X'00',AL1(#OT)   OVERTIME                
         DC    AL1(1,TWAELLNQ,0,11,02),X'00',AL1(#DT)   DOUBLETIME              
         DC    AL1(1,TWAELLNQ,0,14,05),X'00',AL1(#TRV)  TRAVEL                  
         DC    AL1(1,TWAELLNQ,0,20,05),X'00',AL1(#PDW)  PD-WARDROBE             
         DC    AL1(1,TWAELLNQ,0,26,02),X'00',AL1(#TAG)  TAGS                    
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,29,01),X'00',AL1(#ICDE) INCLUDE CODE            
         DC    AL1(1,TWAELLNQ,0,31,10),X'00',AL1(#REXP) REIMB. EXPENSES         
         DC    AL1(1,TWAELLNQ,0,42,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,53,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,64,10),X'00',AL1(#MDED) MISC DEDUCTIONS         
         DC    AL1(1,TWAELLNQ,0,76,04),X'00',AL1(#AGT)  AGENT                   
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         SPACE 3                                                                
*              TWABLD ELEMENT TABLE FOR RADIO SESSIONS                          
         SPACE 1                                                                
BSRTAB   DC    AL1(1,TWAELLNQ+BSRDATQ,2,2,BSRDATQ),X'28',AL1(0)                 
BSRDAT   EQU   *                                                                
         DC    C'Sp^Hr.Mn^Tg^^A^  ^Applied^^I^^Reimb Exp^  ^Payment'            
         DC    C'^ ^Subj-P&&H^ ^Misc Ded^^Agnt'                                 
BSRDATQ  EQU   *-BSRDAT                                                         
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,02,02),X'00',AL1(#SPT)  SPOTS                   
         DC    AL1(1,TWAELLNQ,0,05,05),X'00',AL1(#HRM)  HOURS/MINUTES           
         DC    AL1(1,TWAELLNQ,0,11,02),X'00',AL1(#TAG)  TAGS                    
         DC    AL1(1,TWAELLNQ,0,15,01),X'00',AL1(#ACDE) APPLIED CODE            
         DC    AL1(1,TWAELLNQ,0,17,10),X'00',AL1(#APPL) APPLIED AMOUNT          
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,29,01),X'00',AL1(#ICDE) INCLUDE CODE            
         DC    AL1(1,TWAELLNQ,0,31,10),X'00',AL1(#REXP) REIMB. EXPENSES         
         DC    AL1(1,TWAELLNQ,0,42,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,53,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,64,10),X'00',AL1(#MDED) MISC DEDUCTIONS         
         DC    AL1(1,TWAELLNQ,0,76,04),X'00',AL1(#AGT)  AGENT                   
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         SPACE 3                                                                
*              TWABLD ELEMENT TABLE FOR INTERACTIVE VOICE RERECORD              
         SPACE 1                                                                
IVRTAB   DC    AL1(1,TWAELLNQ+IVRDATQ,2,2,IVRDATQ),X'28',AL1(0)                 
IVRDAT   EQU   *                                                                
         DC    C'Hr.Mn  Travl               '                                   
         DC    C'               ^Payment^ ^Subj-P&&H^ ^Misc Ded^^Agnt'          
IVRDATQ  EQU   *-IVRDAT                                                         
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,02,05),X'00',AL1(#HRM)  HR.MN                   
         DC    AL1(1,TWAELLNQ,0,09,05),X'00',AL1(#TRV)  TRAVEL                  
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,42,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,53,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,64,10),X'00',AL1(#MDED) MISC DEDUCTIONS         
         DC    AL1(1,TWAELLNQ,0,76,04),X'00',AL1(#AGT)  AGENT                   
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         SPACE 3                                                                
*              TWABLD ELEMENT TABLE FOR INDUSTRIAL RADIO SESSION                
         SPACE 1                                                                
DIOTAB   DC    AL1(1,TWAELLNQ+DIODATQ,2,2,DIODATQ),X'28',AL1(0)                 
DIODAT   EQU   *                                                                
         DC    C'#Prog  Hr.Mn  Rtk/Hr.Mn    '                                   
         DC    C'I  Reimb Exp    Payment^ ^Subj-P&&H^ ^Misc Ded^^Agnt'          
DIODATQ  EQU   *-DIODAT                                                         
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,03,02),X'00',AL1(#PRO)  PROGRAMS                
         DC    AL1(1,TWAELLNQ,0,09,05),X'00',AL1(#HRM)  HR.MN                   
         DC    AL1(1,TWAELLNQ,0,18,05),X'00',AL1(#RHM)  RTK HR.MIN              
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,29,01),X'00',AL1(#ICDE) INCLUDE CODE            
         DC    AL1(1,TWAELLNQ,0,31,10),X'00',AL1(#REXP) REIMB. EXPENSES         
         DC    AL1(1,TWAELLNQ,0,42,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,53,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,64,10),X'00',AL1(#MDED) MISC DEDUCTIONS         
         DC    AL1(1,TWAELLNQ,0,76,04),X'00',AL1(#AGT)  AGENT                   
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         SPACE 3                                                                
*              TWABLD ELEMENT TABLE FOR RETAKE                                  
         SPACE 1                                                                
RTKTAB   DC    AL1(1,TWAELLNQ+RTKDATQ,2,2,RTKDATQ),X'28',AL1(0)                 
RTKDAT   EQU   *                                                                
         DC    C'Hr.Mn Travl  Ent Part >60  '                                   
         DC    C'I  Reimb Exp    Payment^ ^Subj-P&&H^ ^Misc Ded^^Agnt'          
RTKDATQ  EQU   *-RTKDAT                                                         
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,02,05),X'00',AL1(#HRM)  HR.MN                   
         DC    AL1(1,TWAELLNQ,0,08,05),X'00',AL1(#TRV)  TRAVEL                  
         DC    AL1(1,TWAELLNQ,0,16,01),X'00',AL1(#ENT)  ENTIRE                  
         DC    AL1(1,TWAELLNQ,0,20,01),X'00',AL1(#PRT)  PARTIAL                 
         DC    AL1(1,TWAELLNQ,0,25,01),X'00',AL1(#60)   >60                     
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,29,01),X'00',AL1(#ICDE) INCLUDE CODE            
         DC    AL1(1,TWAELLNQ,0,31,10),X'00',AL1(#REXP) REIMB. EXPENSES         
         DC    AL1(1,TWAELLNQ,0,42,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,53,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,64,10),X'00',AL1(#MDED) MISC DEDUCTIONS         
         DC    AL1(1,TWAELLNQ,0,76,04),X'00',AL1(#AGT)  AGENT                   
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         EJECT                                                                  
*              TWABLD ELEMENT TABLE FOR MUSIC SESSIONS                          
         SPACE 2                                                                
BSMTAB   DC    AL1(1,TWAELLNQ+BSMDATQ,2,2,BSMDATQ),X'28',AL1(0)                 
BSMDAT   EQU   *                                                                
         DC    C'Sp^Hr.Mn^   ^A^  ^Applied^^I^^Reimb Exp^  ^Payment'            
         DC    C'^ ^Subj-P&&H^ ^Misc Ded^^Agnt'                                 
BSMDATQ  EQU   *-BSMDAT                                                         
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,02,02),X'00',AL1(#SPT)  SPOTS                   
         DC    AL1(1,TWAELLNQ,0,05,05),X'00',AL1(#HRM)  HOURS/MINUTES           
         DC    AL1(1,TWAELLNQ,0,15,01),X'00',AL1(#ACDE) APPLIED CODE            
         DC    AL1(1,TWAELLNQ,0,17,10),X'00',AL1(#APPL) APPLIED AMOUNT          
         DC    X'00'                                                            
         DC    AL1(1,TWAELLNQ,0,29,01),X'00',AL1(#ICDE) INCLUDE CODE            
         DC    AL1(1,TWAELLNQ,0,31,10),X'00',AL1(#REXP) REIMB. EXPENSES         
         DC    AL1(1,TWAELLNQ,0,42,10),X'00',AL1(#PAY)  PAYMENT                 
         DC    AL1(1,TWAELLNQ,0,53,10),X'00',AL1(#SPNH) SUBJECT TO P&H          
         DC    AL1(1,TWAELLNQ,0,64,10),X'00',AL1(#MDED) MISC DEDUCTIONS         
         DC    AL1(1,TWAELLNQ,0,76,04),X'00',AL1(#AGT)  AGENT                   
         DC    AL1(1,TWAELLNQ,0,02,47),X'20',AL1(0)     CAST INFO               
         DC    AL1(1,TWAELLNQ,0,00,03),X'FF',AL1(0)     AFM LOCAL               
         DC    AL1(1,TWAELLNQ,0,50,30),X'00',AL1(#CMT)  CHECK COMMENT           
         DC    X'00'                                                            
         EJECT                                                                  
*              SPECS FOR REPORT                                                 
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,34,C'Payment List'                                            
         SSPEC H2,34,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,1,C'Agency'                                                   
         SSPEC H5,1,C'Invoice'                                                  
         SSPEC H4,56,C'Comml ID'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE DETERMINES IF CANDIAN I&R ACCOUNT OR UNION ACT           
         SPACE 1                                                                
CKCAN    NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         CLC   TACAUN,=CL3'ACT'    IF UNION IS ACTRA,                           
         JE    YES                 RETURN CC EQ                                 
         J     NO                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE EXTRACTS RELEVANT INFORMATION FROM INVOICE REC           
         SPACE 1                                                                
         USING TAIND,R4            R4=A(INVOICE STATUS ELEMENT)                 
EXTINV   NTR1  BASE=*,LABEL=*                                                   
         MVC   INVSTAT,TAINSTAT    SAVE INVOICE STATUS BYTE                     
         MVC   INVSTAT2,TAINSTA2        INVOICE STATUS BYTE 2                   
         MVC   INVIDTE,TAINIDTE    SAVE ASSIGN DATE AND TIME                    
         MVC   INVITIM,TAINITIM         IN CASE INV. PAID BY CLIENT             
         ST    R4,AINVEL           SAVE A(INVOICE ELEMENT)                      
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            GET COMML DETAILS EL.                        
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   SPYCID,TACOCID      DISPLAY COMMERCIAL ID                        
         OI    SPYCIDH+6,X'80'                                                  
         MVC   TGCID,TACOCID       MOVE TO GLOBAL                               
         GOTO1 MEDVAL,DMCB,TACOMED SET GLOBAL MEDIA VALUES                      
         MVC   ELTACO,TACOEL       SAVE ENTIRE ELEMENT FOR SYSCALC              
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SPYCIDNH,TAFNTTTL  COMMERCIAL NAME          
*                                                                               
         XC    SPYLID,SPYLID       DISPLAY LIFT ID IF PRESENT                   
         OI    SPYLIDH+6,X'80'                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ                                                   
         BRAS  RE,GETEL            GET LIFT DETAILS EL.                         
         JNE   *+10                                                             
         USING TALFD,R4                                                         
         MVC   SPYLID,TALFLID      DISPLAY LIFT ID                              
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL            GET PAYMENT DETAILS EL.                      
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         XC    SPYCYC,SPYCYC       DISPLAY CYCLE DATES IF PRESENT               
         OI    SPYCYCH+6,X'80'                                                  
         OC    TAPDCYCS,TAPDCYCS                                                
         JZ    EXT30                                                            
         GOTO1 DATCON,DMCB,(1,TAPDCYCS),(8,SPYCYC)                              
         OC    TAPDCYCE,TAPDCYCE                                                
         JZ    EXT30                                                            
         MVI   SPYCYC+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(1,TAPDCYCE),(8,SPYCYC+9)                            
*                                                                               
EXT30    MVC   PAYPST1,TAPDPST1    SAVE PAYMENT STATUS                          
         OI    SPYCRDH+1,X'0C'                                                  
         OI    SPYCRDH+6,X'80'                                                  
         TM    PAYPST1,TAPDPCRD    IF THIS IS CREDIT PAYMENT                    
         JZ    *+8                                                              
         NI    SPYCRDH+1,X'FB'     MAKE CREDIT INDICATOR HIGH INTENSITY         
*                                                                               
         MVC   TGCOM,TAPDCOM       INTERNAL COMML NUMBER TO GLOBAL              
*                                                                               
         GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE SET GLOBAL USE VALS         
*                                                                               
         MVC   SPYUSE(3),TGUSCDE   DISPLAY USE CODE AND NAME                    
         MVI   SPYUSE+4,C'-'                                                    
         MVC   SPYUSE+6(16),TGUSNAME                                            
         OI    SPYUSEH+6,X'80'                                                  
*                                                                               
         TM    TGUSTYST,UPGRADE              IF THIS IS AN UPGRADE              
         JZ    EXT50                                                            
         GOTO1 UPGRVAL,DMCB,TGUSCDE,TGUSTYP  SET GLOBAL VALUES                  
*                                                                               
EXT50    MVC   PAYOPT1,TAPDOPT1    SAVE PAY OPTION BYTE                         
         MVC   PAYOPT5,TAPDOPT5                                                 
*                                                                               
         XC    PNHRATE,PNHRATE                                                  
         TM    PAYOPT1,TAPDOPHR    IF P&H RATE WAS OVERRIDDEN                   
         JZ    EXT60                                                            
         L     R4,AIO              LOOK FOR APPROPRIATE PAY OPTIONS EL.         
         MVI   ELCODE,TAPAELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
EXT52    BRAS  RE,NEXTEL                                                        
         JNE   EXT55                                                            
         USING TAPAD,R4                                                         
         CLI   TAPATYPE,TAPATPHR   IF THIS IS P&H RATE OPTION                   
         JNE   EXT52                                                            
         MVC   PNHRATE,TAPADATA    SAVE THE RATE                                
         J     EXT60                                                            
*                                                                               
EXT55    L     R4,AIO              LOOK FOR PAY DETAILS EL.                     
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL            MUST BE THERE CAUSE GOT IT BEFORE            
         USING TAPDD,R4                                                         
         OC    TAPDSPNH,TAPDSPNH   DON'T BOTHER IF SUBJ TO P&H=0                
         JZ    EXT60                                                            
         OC    TAPDPNH,TAPDPNH     OR IF P&H AMOUNT=0                           
         JZ    EXT60                                                            
         SR    R0,R0               ELSE CALCULATE P&H RATE                      
         L     R1,TAPDPNH          P&H AMOUNT                                   
         M     R0,=F'100000'       X 100000                                     
         D     R0,TAPDSPNH         / SUBJECT TO P&H                             
         XR    R0,R0                                                            
         D     R0,=F'5'            ROUND UP IF NECESSARY                        
         LTR   R1,R1                                                            
         JM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,PNHRATE                                                       
*                                                                               
EXT60    L     R4,AINVEL           RESTORE A(INVOICE STATUS ELEMENT)            
         USING TAIND,R4                                                         
         MVC   LSTPAYPG,TAINLPG    SET LAST PAGE NUMBER                         
         TM    PAYPST1,TAPDPCHG    IF PAYMENT HAS BEEN CHANGED PREV.            
         JZ    *+10                                                             
         MVC   LSTPAYPG,TAINLPGP   THEN USE LAST PAGE FROM PAY                  
*                                                                               
         XC    ELTAUP,ELTAUP       CLEAR SAVED UPGRADE DETAILS EL.              
         L     R4,AIO                                                           
         MVI   ELCODE,TAUPELQ                                                   
         BRAS  RE,GETEL            GET UPGRADE DETAILS EL.                      
         JNE   *+10                                                             
         USING TAUPD,R4                                                         
         MVC   ELTAUP,TAUPEL       SAVE ENTIRE ELEMENT                          
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DISPLAY LINE DSECT FOR TV SESSIONS                               
         SPACE 1                                                                
LBSSD    DSECT                                                                  
LBSSSPTH DS    CL8                                                              
LBSSSPT  DS    CL2                 N'SPOTS                                      
         DS    CL8                                                              
LBSSDAYH DS    CL8                                                              
LBSSDAY  DS    CL2                 N'DAYS                                       
         DS    CL8                                                              
LBSSOTH  DS    CL8                                                              
LBSSOT   DS    CL2                 N'OVERTIME HOURS                             
         DS    CL8                                                              
LBSSDTH  DS    CL8                                                              
LBSSDT   DS    CL2                 N'DOUBLE-TIME HOURS                          
         DS    CL8                                                              
LBSSTRVH DS    CL8                                                              
LBSSTRV  DS    CL5                 N'TRAVEL TIME HOURS                          
         DS    CL8                                                              
LBSSPDWH DS    CL8                                                              
LBSSPDW  DS    CL5                 N'PRIOR-DAY WARDROBE HOURS                   
         DS    CL8                                                              
LBSSTAGH DS    CL8                                                              
LBSSTAG  DS    CL2                 N'TAGS                                       
         DS    CL8                                                              
         SPACE 2                                                                
*              DISPLAY LINE DSECT FOR RADIO SESSIONS                            
         SPACE 1                                                                
LBSRD    DSECT                                                                  
LBSRSPTH DS    CL8                                                              
LBSRSPT  DS    CL2                 N'SPOTS                                      
         DS    CL8                                                              
LBSRHMH  DS    CL8                                                              
LBSRHM   DS    CL5                 HOURS/MINUTES                                
         DS    CL8                                                              
LBSRTAGH DS    CL8                                                              
LBSRTAG  DS    CL2                 N'TAGS                                       
         DS    CL8                                                              
         SPACE 2                                                                
*              DISPLAY LINE DSECT FOR MUSIC SESSIONS                            
         SPACE 1                                                                
LBSMD    DSECT                                                                  
LBSMSPTH DS    CL8                                                              
LBSMSPT  DS    CL2                 N'SPOTS                                      
         DS    CL8                                                              
LBSMHMH  DS    CL8                                                              
LBSMHM   DS    CL5                 HOURS/MINUTES                                
         DS    CL8                                                              
         SPACE 2                                                                
*              DISPLAY LINE DSECT FOR INTERACTIVE VOICE RECORD                  
         SPACE 1                                                                
LIVRD    DSECT                                                                  
LIVRHMH  DS    CL8                                                              
LIVRHM   DS    CL5                 N'HOURS                                      
         DS    CL8                                                              
LIVRTRVH DS    CL8                                                              
LIVRTRV  DS    CL5                 N'TRAVEL TIME HOURS                          
         SPACE 2                                                                
*              DISPLAY LINE DSECT FOR INDUSTRIAL RADIO SESSION                  
         SPACE 1                                                                
LDIOD    DSECT                                                                  
LDIOPROH DS    CL8                                                              
LDIOPRO  DS    CL2                 PROGRAMS                                     
         DS    CL8                                                              
LDIOHMH  DS    CL8                                                              
LDIOHM   DS    CL5                 N'HOURS                                      
         DS    CL8                                                              
LDIORHMH DS    CL8                                                              
LDIORHM  DS    CL5                 N'RETAKE HOURS                               
         SPACE 2                                                                
*              DISPLAY LINE DSECT FOR RETAKE                                    
         SPACE 1                                                                
LRTKD    DSECT                                                                  
LRTKHMH  DS    CL8                                                              
LRTKHM   DS    CL5                 H/HOURS                                      
         DS    CL8                                                              
LRTKTRVH DS    CL8                                                              
LRTKTRV  DS    CL5                 N'TRAVEL TIME HOURS                          
         DS    CL8                                                              
LRTKENTH DS    CL8                                                              
LRTKENT  DS    CL1                 ENTIRE SCRIPT                                
         DS    CL8                                                              
LRTKPRTH DS    CL8                                                              
LRTKPRT  DS    CL1                 PARTIAL                                      
         DS    CL8                                                              
LRTK60H  DS    CL8                                                              
LRTK60   DS    CL1                 >60                                          
         EJECT                                                                  
*              DSECT TO COVER CAST DETAILS FIELD                                
         SPACE 1                                                                
CASTD    DSECT                                                                  
CASTDH   DS    CL8                 FIELD HEADER                                 
CASTNAME DS    CL16                NAME                                         
         DS    CL1                                                              
CASTCAT  DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
CASTONOF DS    CL1                 Y=ON, N=OFF CAMERA                           
         DS    CL1                                                              
CASTYR   DS    CL3                 CONTRACT YEAR                                
CASTOPTS DS    CL16                OPTIONS                                      
CASTCTAG DS    CL5                 COMMENT TAG                                  
         SPACE 3                                                                
*              DSECT TO COVER DISPLAY TABLE                                     
         SPACE 1                                                                
DISPD    DSECT                                                                  
DISPDIS  DS    H                   DISPLACEMENT TO DISPLAY ROUTINE              
DISPVAL  DS    H                   DISPLACEMENT TO VALIDATION ROUTINE           
DISPNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER AMOUNT TABLE                                      
         SPACE 1                                                                
AMTD     DSECT                                                                  
AMTTAPD  DS    AL1                 DISP. TO FIELD IN PAYMENT DETAILS EL         
AMTCALC  DS    AL1                 DISP. TO FIELD IN SYSCALC W/S                
AMTNEXT  EQU   *                                                                
         EJECT                                                                  
*              LOCAL SAVED STORAGE                                              
         SPACE 1                                                                
LOCALD   DSECT                                                                  
AGYNAME  DS    CL16                AGENCY NAME FOR PRINTING                     
INVSTAT  DS    XL1                 INVOICE STATUS BYTE                          
INVSTAT2 DS    XL1                 INVOICE STATUS BYTE 2                        
INVITIM  DS    XL3                 INVOICE ASSIGN DATE                          
INVIDTE  DS    XL3                 INVOICE ASSIGN TIME                          
PAYPST1  DS    XL1                 PAYMENT STATUS BYTE                          
PAYOPT1  DS    XL1                 PAYMENT OPTION BYTE                          
PNHRATE  DS    H                   P&H RATE OVERRIDE                            
AINVEL   DS    A                   A(INVOICE STATUS ELEMENT)                    
DISPDISP DS    H                   DISP. TO DISPLAY TABLE                       
DFRSTDAT DS    H                   DISP. TO FIRST DATA FIELD                    
DLASTDAT DS    H                   DISP. TO LAST DATA FIELD                     
LLINE    DS    H                   L'LIST LINE                                  
ALASTDAT DS    A                   A(LAST DATA FIELD)                           
ATHISEND DS    A                   A(LAST FIELD ON THIS LINE)                   
COUNTER  DS    PL4                 LINE COUNTER                                 
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
*                                                                               
OPTS     DS    XL1                 LOCAL OPTIONS                                
OPTOTH   EQU   X'80'               DISPLAY 'OTHER' FIELDS                       
*                                                                               
VALSTAT  DS    XL1                 VALIDATION STATUS                            
CALCPAY  EQU   X'80'               RE-CALC PAYMENT AMOUNT                       
CALCPNH  EQU   X'40'               RE-CALC PENSION & HEALTH                     
INPNH    EQU   X'20'               P&H AMOUNT OVERRIDDEN                        
INHNW    EQU   X'10'               H&W AMOUNT OVERRIDDEN                        
NONMEMO  EQU   X'08'               NON-MEMO ITEM CHANGED                        
*                                                                               
MYTACREL DS    CL(TACRLNQ)         SAVED TACR ELEMENT                           
         ORG   MYTACREL+TACRSTRT-TACRD                                          
APPLPD   DS    XL6                 PWOS FTRACK PERIOD                           
         ORG                                                                    
APPLCODE DS    CL1                 APPLIED CODE FROM PAY DETAILS EL.            
APPLAMNT DS    F                   APPLIED AMOUNT ''  ''    ''   ''             
*                                                                               
         DS    0F                  LOCAL AMOUNT OVERRIDES                       
MYAMTS   DS    0CL8                                                             
MYPNH    DS    F                                                                
MYHNW    DS    F                                                                
*                                                                               
         DS    0F                  ACCUMULATED DIFFERENCE OF EACH AMT           
DIFFAMTS DS    CL(TAPDAMTL)        CORRESPONDING TO AMTS IN TAPDEL              
         DS    CL(TAPDNTNW-TAPDINS)                                             
DIFFNTNW DS    CL(L'TAPDNTNW)                                                   
         DS    CL4                 SPARE                                        
*                                                                               
ELTACO   DS    CL(TACOLNQ)         COMML DETAILS ELEMENT FOR SYSCALC            
ELTAUP   DS    CL(TAUPLNQ)         UPGRADE DETAILS ELEMENT FROM INV.            
*                                                                               
         DS    0A                                                               
AFLDS    DS    0CL8                                                             
APAYFLD  DS    A                   A(CURRENT PAYMENT AMOUNT FIELD)              
AAPPLFLD DS    A                   A(CURRENT APPLIED AMOUNT FIELD)              
*                                                                               
TOTS     DS    4F                  DUMMY AREA FOR PAGE NUMBER CONTROL           
         ORG   TOTS+3                                                           
THISPAGE DS    XL1                 THIS IS WHERE THE PAGE NUMBER IS             
         ORG                                                                    
LSTPAYPG DS    XL1                 LAST PAGE NUMBER FROM PAY                    
SCRNPAGE DS    XL1                 PAGE NUMBER OF CURRENT SCREEN RECORD         
*                                                                               
SVELEM   DS    XL(L'ELEMENT)       SAVED ELEMENT AREA                           
*                                                                               
PTRBLK   DS    CL(10*L'TLDRREC+1)   BLOCK FOR POINTER MAINTENANCE               
*                                                                               
PAYOPT5  DS    XL1                 PAYMENT OPTION BYTE 5                        
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
       ++INCLUDE TASYSCALCD                                                     
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC0D                                                       
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075TAGENC0   10/06/14'                                      
         END                                                                    
