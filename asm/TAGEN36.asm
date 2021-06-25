*          DATA SET TAGEN36    AT LEVEL 072 AS OF 09/03/08                      
*PHASE T70236A                                                                  
         TITLE 'T70236 - PRODUCT LIST'                                          
T70236   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70236                                                         
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
         GOTO1 INITIAL,DMCB,PFTAB                                               
         CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   PRDL20                                                           
         BAS   RE,INIT             INITIALIZE INFO FOR SYSIO                    
         B     XIT                                                              
         SPACE 3                                                                
PRDL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   PRDL30                                                           
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
PRDL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LSTREC                                                           
         SPACE 3                                                                
LSTREC   EQU   *                                                                
         LA    R0,HOOK             SET ADDRESS OF HOOK FOR SYSIO                
         ST    R0,TIHOOK                                                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIACOMFC,ACOMFACS                                                
         MVI   NLISTS,16           GET CONTROL BACK AT END OF PAGE              
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15           RESET AT END OF LIST                         
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE                                    
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(15,R1),=C'PRODUCT RECORDS'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    XIT                                                              
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              PROCESS SYSIO RECORDS                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
HOOK     NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESS KEYS                                 
         BNE   HK40                                                             
         CLI   BRANDOPT,C'A'       LISTING BRANDS AND NON-BRANDS?               
         BE    YES                                                              
         CLI   BRANDOPT,C'Y'       ONLY LISTING BRANDS?                         
         BNE   HK10                                                             
         TM    TIKEYST,TLPRSBRD    BRAND PRODUCT STATUS MUST BE ON              
         BO    YES                                                              
         B     NO                                                               
HK10     TM    TIKEYST,TLPRSBRD    OTHERWISE ONLY LISTING BRANDS                
         BO    NO                  BRAND PRODUCT STATUS MUST BE OFF             
         B     YES                                                              
*                                                                               
HK40     CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         BAS   RE,FILTREC                                                       
         BNE   NO                                                               
         SPACE                                                                  
         CLI   AGYOVR,C'N'         IF SUPPRESSING AGENCY OVERRIDE               
         BNE   *+14                                                             
         OC    TIAGY,TIAGY         ONLY WANT RECORDS WITH NO AGENCY             
         BNZ   XIT                                                              
         SPACE                                                                  
         MVC   LISCODE,TIPRD       PRODUCT CODE                                 
         MVC   LISAGY,TIAGY        AGENCY                                       
         MVC   LISCLI,TICLI        CLIENT                                       
         MVC   LISNAME,TINAME      DEFAULT TO TRUNCATED LONG NAME               
         CLC   TISHORT,SPACES      IF THERE'S A SHORT NAME                      
         BE    *+10                                                             
         MVC   LISNAME,TISHORT     USE IT                                       
         MVC   LISPRG,TIPRG        PRODUCT GROUP                                
         SPACE                                                                  
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   HK50                                                             
         GOTO1 CATCHIOS            ENSURE IO OVERUSE FOR REPORTS                
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT LINES OUTPUT                           
         B     XIT                                                              
         SPACE                                                                  
HK50     CLI   LISTNUM,15                                                       
         BE    ENDPAGE             GET OUT IF ALREADY AT END OF PAGE            
         GOTO1 LISTMON             ELSE CALL LISTMON                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILTER PRODUCT RECORDS                                
         SPACE                                                                  
FILTREC  NTR1                                                                   
         USING TAPID,R4                                                         
         OC    FLTPTY,FLTPTY        IF PRODUCT TYPE FILTER NOT DEFINED          
         BZ    FREC10               THEN LIST ALL PRODUCTS                      
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPIELQ       IF IT IS DEFINED AND NO PRODUCT             
         BAS   RE,GETEL             INFORMATION ELEMENT OF NEW LENGTH           
         BNE   NO                   REJECT IT                                   
         CLI   TAPILEN,TAPILNQ                                                  
         BE    NO                                                               
         CLC   TAPIPTYP,FLTPTY                                                  
         BNE   NO                                                               
*                                                                               
FREC10   LA    R1,0                                                             
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPIELQ       MAKE SURE WE HAVE PRODUCT INF ELEM          
         BAS   RE,GETEL                                                         
         BNE   FREC20                                                           
         TM    TAPISTAT,TAPISLCK                                                
         BZ    FREC20                                                           
         LA    R1,1                 PRODUCT LOCKED                              
*                                                                               
FREC20   LTR   R1,R1                IF PRODUCT LOCKED,                          
         BZ    *+16                                                             
         CLI   OPTLOCK,C'Y'         REJECT IF NOT DISPLAYING THEM               
         BNE   NO                                                               
         B     *+12                                                             
         CLI   OPTLOCK,C'Y'         ELSE REJECT IF DISPLAYING THEM              
         BE    NO                                                               
*                                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              INITIALIZES INFO NECESSARY FOR SYSIO                             
         SPACE                                                                  
INIT     NTR1                                                                   
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         TM    SPRAGYH+4,X'20'     IF AGENCY NOT PREV VALIDATED                 
         BO    INIT0                                                            
         MVI   ALLVAL,C'N'         SET ALLVAL FLAG                              
         NI    SPRCLIH+4,X'DF'     MUST VALIDATE CLIENT                         
         SPACE                                                                  
INIT0    XC    TIFAGY,TIFAGY       CLEAR FILTER-MUST REVALIDATE AGENCY          
         CLI   SPRAGYH+5,0         IN CASE AGENCY OVERRIDE CHANGED              
         BE    INIT1                                                            
         LA    R2,SPRCLIH          IF HAVE AGENCY INPUT                         
         CLI   5(R2),0                                                          
         BE    FLDMISS             ERROR IF NO CLIENT                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,SPRAGYH VALIDATE AGENCY                      
         MVC   TIFAGY,SPRAGY       FILTER ON AGENCY                             
         OC    TIFAGY,SPACES                                                    
INIT1    OI    SPRAGYH+4,X'20'     SET VALIDATED FOR LENGTH=0                   
         SPACE                                                                  
         TM    SPRCLIH+4,X'20'     IF CLIENT NOT PREV VALIDATED                 
         BO    INIT4                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIFCLI,TIFCLI                                                    
         NI    SPRTYPEH+4,X'DF'                                                 
         CLI   SPRCLIH+5,0         IF THERE'S CLIENT INPUT                      
         BE    INIT2G                                                           
         LA    R2,SPRAGYH                                                       
         CLI   5(R2),0                                                          
         BE    FLDMISS             ERROR IF THERE'S NO AGENCY INPUT             
INIT2E   GOTO1 RECVAL,DMCB,TLCLCDQ,SPRCLIH VALIDATE CLIENT                      
         MVC   TIFCLI,SPRCLI       FILTER ON CLIENT                             
         OC    TIFCLI,SPACES                                                    
INIT2G   OI    SPRCLIH+4,X'20'     SET VALIDATED FOR LENGTH=0                   
         SPACE 1                                                                
INIT4    TM    SPRTYPEH+4,X'20'                                                 
         BO    INIT5                                                            
         XC    FLTPTY,FLTPTY                                                    
         CLI   SPRTYPEH+5,0                                                     
         BE    INIT5                                                            
         GOTO1 RECVAL,DMCB,TLPTCDQ,SPRTYPEH                                     
         MVC   FLTPTY,SPRTYPE                                                   
         OC    FLTPTY,SPACES                                                    
INIT5    OI    SPRTYPEH+4,X'20'                                                 
         SPACE 1                                                                
         TM    SPRPRGH+4,X'20'     IF PRODUCT GROUP NOT PREV VALIDATED          
         BO    INIT6                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIFPRG,TIFPRG                                                    
         CLI   SPRPRGH+5,0         INPUT IS OPTIONAL                            
         BE    INIT6                                                            
         GOTO1 RECVAL,DMCB,TLPGCDQ,SPRPRGH VALIDATE PRODUCT GROUP               
         MVC   TIFPRG,TGPRG        FILTER ON PRODUCT GROUP                      
         SPACE 1                                                                
INIT6    TM    SPRSTRH+4,X'20'     IF START AT FIELD NOT PREV VALIDATED         
         BO    INIT7                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIQSTART,TIQSTART   RESET START AT                               
         ZIC   R3,SPRSTRH+5                                                     
         LTR   R3,R3                                                            
         BZ    INIT6A                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),SPRSTR                                               
INIT6A   OI    SPRSTRH+4,X'20'     SET PREV VALIDATED                           
         SPACE                                                                  
INIT7    LA    R2,SPRFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'         IF NOT PREV VALIDATED                        
         BO    INIT9                                                            
         MVI   ALLVAL,C'N'                                                      
         MVI   TIREAD,TLPRNCDQ     DEFAULT TO LIST BY PRODUCT NAME              
         CLI   5(R2),0                                                          
         BE    INIT8                                                            
         CLI   8(R2),C'A'          IF INPUT IS A FOR NAME SEQUENCE              
         BNE   *+16                                                             
         CLI   SPRPRGH+5,0                                                      
         BNE   OPTERR              ERROR IF THERE'S PROD. GROUP                 
         B     INIT8                                                            
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BNE   FLDINV                                                           
         MVI   TIREAD,TLPRCDQ      LIST BY PRODUCT CODE                         
INIT8    OI    4(R2),X'20'         SET PREV VALIDATED                           
         SPACE                                                                  
INIT9    BAS   RE,VALOPTS                                                       
         SPACE                                                                  
INIT20   LA    R2,SPRBRNDH         VALIDATE BRAND                               
         TM    4(R2),X'20'         IF NOT PREV VALIDATED                        
         BO    INIT30                                                           
         MVI   ALLVAL,C'N'                                                      
         MVI   BRANDOPT,C'A'                                                    
         CLI   8(R2),C'A'          LIST BRANDS AND NON-BRANDS                   
         BE    INIT30                                                           
         CLI   5(R2),0             DEFAULT TO LIST BOTH                         
         BE    INIT30                                                           
         MVI   BRANDOPT,0                                                       
         CLI   8(R2),C'N'          LIST NON-BRANDS ONLY                         
         BE    INIT30                                                           
         MVI   BRANDOPT,C'Y'                                                    
         CLI   8(R2),C'Y'          LIST BRANDS ONLY                             
         BNE   FLDINV                                                           
INIT30   CLI   ALLVAL,C'Y'         IF NOT ALL FIELDS PREV VALIDATED             
         BE    XIT                                                              
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO FIRST TIME IN          
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         MVI   TIQFLAGS,TIQFDIR    RETURNS IN MODE PROCDIR                      
         CLI   AGYOVR,C'N'         IF SUPPRESSING AGENCY OVERRIDE               
         BNE   *+10                                                             
         XC    TIFAGY,TIFAGY       CLEAR AGENCY FILTER                          
         CLI   SPRPRGH+5,0         IF THERE'S PRODUCT GROUP INPUT               
         BE    XIT                                                              
         MVI   TIREAD,TLPRCDQ      LIST BY PRODUCT CODE                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE                                                                  
VALOPTS  NTR1                                                                   
         LA    R2,SPROPTSH         OPTIONS                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    VOPTX                                                            
         MVI   ALLVAL,C'N'                                                      
         MVI   AGYOVR,C'Y'         DEFAULT TO USE AGENCY OVERRIDES              
         MVI   OPTLOCK,0                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VOPT20   MVC   ERRDISP,SCDISP1                                                  
         CLI   SCLEN1,0            LHS IS REQUIRED                              
         BE    FLDINV                                                           
         LA    R4,OPTTAB           LOOK UP IN OPTIONS TABLE                     
         USING OPTD,R4                                                          
         ZIC   RF,SCLEN1                                                        
         BCTR  RF,0                                                             
*                                                                               
VOPT30   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),OPTLHS   MATCH ON LHS                                 
         BE    VOPT35                                                           
         LA    R4,OPTNEXT                                                       
         CLI   0(R4),X'FF'                                                      
         BE    FLDINV              END OF TABLE                                 
         B     VOPT30                                                           
*                                                                               
VOPT35   LH    RF,OPTDISP          DISP TO VAL. ROUTINE                         
         AR    RF,RB                                                            
         MVC   ERRDISP,SCDISP2                                                  
         BASR  RE,RF               GO VALIDATE IT                               
         LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
*                                                                               
VOPTX    OI    4(R2),X'20'         SET PREV VALIDATED                           
         MVI   ERRDISP,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES                            
*                                                                               
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
         SPACE 2                                                                
VALLOCK  DS    0H                  LOCKED CLIENTS                               
         CLI   SCDATA2,C'Y'        IF SHOWING LOCKED ONLY                       
         BNE   FLDINV                                                           
         MVC   OPTLOCK,SCDATA2                                                  
         BR    RE                                                               
         SPACE 2                                                                
VALAGY   DS    0H                                                               
         CLI   SCDATA2,C'N'        SUPPRESS AGENCY OVERRIDES                    
         BNE   FLDINV                                                           
         MVC   AGYOVR,SCDATA2                                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         SPACE                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
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
         LA    R2,SPRSELH                                                       
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
PFTAB    DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'CO',CL8'COMMERCL',CL8'LIST'                                  
PF13     DC    AL1(KEYTYCUR,L'LISAGY-1),AL2(LISAGY-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISCLI-1),AL2(LISCLI-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISCODE-1),AL2(LISCODE-LISTD)                     
PF13X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
OPTTAB   DS    0H                  OPTIONS TABLE                                
         DC    CL10'LOCKED    ',AL2(VALLOCK-T70236)                             
         DC    CL10'AGENCY    ',AL2(VALAGY-T70236)                              
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'PRODUCT LIST'                                            
         SSPEC H2,33,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,1,C'PRODUCT'                                                  
         SSPEC H4,15,C'AGENCY'                                                  
         SSPEC H4,23,C'CLIENT'                                                  
         SSPEC H4,33,C'PRODUCT NAME'                                            
         SSPEC H4,53,C'PRD GRP'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'-------'                                                  
         SSPEC H5,15,C'------'                                                  
         SSPEC H5,23,C'------'                                                  
         SSPEC H5,33,C'------------'                                            
         SSPEC H5,53,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LISTD    DSECT                                                                  
LISCODE  DS    CL6                                                              
         DS    CL8                                                              
LISAGY   DS    CL6                                                              
         DS    CL2                                                              
LISCLI   DS    CL6                                                              
         DS    CL4                                                              
LISNAME  DS    CL16                                                             
         DS    CL4                                                              
LISPRG   DS    CL6                                                              
         EJECT                                                                  
*              DSECT TO COVER OPTIONS TABLE                                     
*                                                                               
OPTD     DSECT                                                                  
OPTLHS   DS    CL10                                                             
OPTDISP  DS    AL2                                                              
OPTNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR36D                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
AGYOVR   DS    CL1                 AGENCY OVERRIDES                             
OPTLOCK  DS    CL1                 Y=SHOW LOCKED PRODUCTS                       
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
BRANDOPT DS    CL1                 Y=BRANDS ONLY, A=BRANDS + NONBRANDS          
FLTPTY   DS    CL6                 PRODUCT TYPE FILTER                          
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072TAGEN36   09/03/08'                                      
         END                                                                    
