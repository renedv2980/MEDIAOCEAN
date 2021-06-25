*          DATA SET TAGEN34    AT LEVEL 031 AS OF 08/18/05                      
*PHASE T70234A,*                                                                
         TITLE 'T70234 - CLIENT LIST'                                           
T70234   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70234                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
CLT10    GOTO1 INITIAL,DMCB,PFTABLE                                             
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
CLT20    CLI   MODE,LISTRECS       LIST SCREEN                                  
         BNE   CLT30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
CLT30    CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         BAS   RE,INIT             RE-INITIALIZE FOR REPORT                     
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
         TM    SCLSTRH+4,X'20'     IF START,                                    
         BZ    VK5                                                              
         TM    SCLFMTH+4,X'20'     FORMAT,                                      
         BZ    VK5                                                              
         TM    SCLOPTSH+4,X'20'    OPTIONS,                                     
         BZ    VK5                                                              
         TM    SCLCGRH+4,X'20'     OR CLIENT GROUP CHANGED                      
         BO    VK6                                                              
*                                                                               
VK5      NI    SCLAGYH+4,X'DF'     FORCE VALIDATION OF WHOLE SCREEN             
*                                                                               
VK6      OC    TGCLGACC,TGCLGACC   IF CLIENT GROUP ACCESS IS DEFINED            
         BZ    VK6A                                                             
         CLI   SCLCGRH+5,0         AND NOT INPUTTED                             
         BNE   VK6A                                                             
         MVC   SCLCGR,TGCLGACC     AUTOMATICALLY INPUT                          
         MVI   SCLCGRH+5,L'TGCLGACC                                             
         OI    SCLCGRH+6,X'80'                                                  
*                                                                               
VK6A     LA    R2,SCLAGYH          AGENCY                                       
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK20                                                             
         NI    SCLSTRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         MVC   SCLAGYN,SPACES      CLEAR AGENCY NAME                            
         OI    SCLAGYNH+6,X'80'                                                 
*                                                                               
         CLI   SCLCGRH+5,0         IF CLIENT GROUP FILTER                       
         BNE   VK7                    DON'T REQUIRE AGENCY                      
         CLI   SCLOPTSH+5,0        OR IF OPTION A = N                           
         BNE   VK7                    DON'T REQUIRE AGENCY                      
*                                                                               
         CLI   SCLFMTH+5,0         IF FORMAT = T (READ VIA COMM'L)              
         BE    VK15                   THEN REQUIRE AGENCY                       
         CLI   SCLFMTH+8,C'T'                                                   
         BE    VK15                OR USE GLOBAL AGENCY                         
*                                                                               
VK7      CLI   5(R2),0                                                          
         BE    VK20                                                             
*                                                                               
VK10     OC    SCLAGY,SPACES                                                    
         MVC   AIO,AIO2                                                         
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
VK15     GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCLAGYH),SCLAGYNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
VK20     OI    4(R2),X'20'                                                      
         LA    R2,SCLSTRH          START AT?                                    
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK30                                                             
         NI    SCLCGRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART   START TO READ FROM                           
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
*                                                                               
VK30     OI    4(R2),X'20'                                                      
         LA    R2,SCLCGRH          CLIENT GROUP                                 
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK40                                                             
         NI    SCLFMTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCGCDQ,SCLCGRH     VALIDATE CLT GRP                 
         OC    TGCLGACC,TGCLGACC   IF CLIENT GROUP ACCESS IS DEFINED            
         BZ    VK36                                                             
         CLC   TGCLG,TGCLGACC      CLIENT GROUP MUST MATCH IT                   
         BNE   INVERR                                                           
*                                                                               
VK36     CLI   SCLFMTH+5,0         IF NO INPUT IN FORMAT FIELD                  
         BE    VK37                                                             
         CLI   SCLFMT,C'C'         OR IF FORMAT REQUESTED = C                   
         BNE   INVERR                                                           
*                                                                               
VK37     MVI   RDSEQ,C'C'          CONTINUE                                     
         MVI   SCLFMT,C'C'                                                      
         OI    SCLFMTH+4,X'20'     VALID - DON'T REVALIDATE                     
         NI    SCLOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         MVC   AIO,AIO1                                                         
*                                                                               
VK40     OI    4(R2),X'20'                                                      
         LA    R2,SCLFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK100                                                            
         NI    SCLOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         MVI   RDSEQ,C'T'          DEFAULT TO READ COMML'S OF AGY               
         CLI   5(R2),0             SET READING SEQUENCE - CODE/NAME             
         BE    VK100                                                            
         CLI   8(R2),C'T'          READ BY COMMERCIALS                          
         BNE   VK60                                                             
         OC    SCLCGR,SCLCGR       CANNOT READ BY COMM'L IF FILTER              
         BNZ   OPTERR                     ON CLT GRP                            
         B     VK100                                                            
*                                                                               
VK60     CLI   8(R2),C'A'          READ BY NAME                                 
         BNE   VK70                                                             
         OC    SCLCGR,SCLCGR       CANNOT READ BY NAME IF FILTER                
         BNZ   OPTERR                     ON CLT GRP                            
         MVI   RDSEQ,C'A'          READ ALPHA SEQUENCE                          
         B     VK100                                                            
*                                                                               
VK70     MVI   RDSEQ,C'C'                                                       
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BNE   INVERR                                                           
*                                                                               
VK100    DS    0H                                                               
         OI    4(R2),X'20'                                                      
         LA    R2,SCLOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VKX                                                              
         BAS   RE,VALOPTS          VALIDATE THE OPTIONS                         
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE                                                                  
VALOPTS  NTR1                                                                   
         MVI   AGYOVR,C'Y'         DEFAULT TO USE AGENCY OVERRIDES              
         MVI   OPTLOCK,0           CLEAR RELEASED OPTION FIELD                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         BZ    INVERR                                                           
*                                                                               
VOPT20   MVC   ERRDISP,SCDISP1                                                  
         CLI   SCLEN1,0            LHS IS REQUIRED                              
         BE    INVERR                                                           
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
         BE    INVERR              END OF TABLE                                 
         B     VOPT30                                                           
*                                                                               
VOPT35   LH    RF,OPTDISP          DISP TO VAL. ROUTINE                         
         AR    RF,RB                                                            
         MVC   ERRDISP,SCDISP2                                                  
         BASR  RE,RF               GO VALIDATE IT                               
         LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
*                                                                               
VOPTX    OI    4(R2),X'20'                                                      
         MVI   ERRDISP,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES                            
*                                                                               
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
         SPACE 2                                                                
VALLOCK  DS    0H                  LOCKED CLIENTS                               
         CLI   SCDATA2,C'Y'        IF SHOWING LOCKED ONLY                       
         BNE   INVERR                                                           
         CLI   RDSEQ,0             & IF READING VIA COMMERCIALS                 
         BE    *+12                                                             
         CLI   RDSEQ,C'T'                                                       
         BNE   VALLOCKX                                                         
         OC    SCLAGY,SCLAGY       MUST HAVE AGENCY INPUT                       
         BNZ   *+14                                                             
         MVC   ERRDISP,SCDISP1                                                  
         B     AGYERR                                                           
VALLOCKX MVC   OPTLOCK,SCDATA2                                                  
         BR    RE                                                               
         SPACE 2                                                                
VALAGY   DS    0H                                                               
         CLI   SCDATA2,C'N'        SUPPRESS AGENCY OVERRIDES                    
         BNE   INVERR                                                           
         OC    SCLAGY,SCLAGY       CANNOT SUPPRESS IF AGENCY OVERRIDE           
         BZ    *+14                                                             
         MVC   ERRDISP,SCDISP1                                                  
         B     OPTERR                                                           
         MVC   AGYOVR,SCDATA2                                                   
         BR    RE                                                               
         EJECT                                                                  
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         XC    TIQSKEY,TIQSKEY                                                  
         CLI   RDSEQ,C'T'          LIST VIA COMM'L                              
         BE    INITX               XIT - NOT USING SYSIO                        
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA                       
         MVC   TIQSTAFF,TGCTSTAF                                                
         XC    TIFAGY,TIFAGY       CLEAR FILTERS                                
         XC    TIFCLG,TIFCLG                                                    
*                                                                               
         MVI   TIREAD,TLCLNCDQ     LIST BY CLIENT NAME                          
         CLI   RDSEQ,C'A'                                                       
         BE    *+8                                                              
         MVI   TIREAD,TLCLCDQ      LIST BY CLIENT CODE                          
*                                                                               
         CLI   AGYOVR,C'N'         SUPPRESS AGENCY OVERRIDE                     
         BE    INIT50                                                           
         CLI   SCLAGYH+5,0         FILTER ON AGENCY                             
         BE    INIT50                                                           
         MVC   TIFAGY,SCLAGY                                                    
*                                                                               
INIT50   CLI   SCLCGRH+5,0         FILTER ON CLIENT GROUP                       
         BE    INITX                                                            
         MVI   TIREAD,TLCLCDQ      LIST BY CLIENT GROUP                         
         MVC   TIFCLG,SCLCGR                                                    
         OC    TIFCLG,SPACES                                                    
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LIST RECORDS                                          
*                                                                               
LR       CLI   RDSEQ,C'T'          LIST VIA COMM'LS                             
         BNE   LR50                                                             
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING TLCOD,R1                                                         
         OC    TIQSKEY,TIQSKEY     CONTINUE KEY                                 
         BZ    LR5                                                              
         MVC   TLCOKEY,TIQSKEY     LAST COMM'L KEY READ                         
         MVI   TLCOPRD,X'FF'       & GET NEXT ONE                               
         B     LR10                                                             
*                                                                               
LR5      MVI   TLCOCD,TLCOCDQ      COMM'L ACTIVE POINTER                        
         MVC   TLCOAGY,SCLAGY                                                   
         OC    TLCOAGY,SPACES                                                   
         MVC   TLCOCLI,TIQSTART    START AT                                     
*                                                                               
LR10     GOTO1 HIGH                                                             
         MVC   TIQSKEY,KEY                                                      
*                                                                               
LR20     CLC   KEY(TLCOCLI-TLCOD),KEYSAVE    STILL SAME AGENCY                  
         BNE   LR60                                                             
         MVC   TIAGY,TLCOAGY                                                    
         MVC   TICLI,TLCOCLI       SET UP INFO INSTEAD OF SYSIO                 
         MVC   SVKEYCO,KEY                                                      
         MVC   SVKEY,KEY           SAVE KEY FOR NEXT READ                       
         DROP  R1                                                               
*                                                                               
LR21     MVC   AIO,AIO2                                                         
         GOTO1 GETREC              GET COMMERCIAL RECORD                        
         USING TAOCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAOCELQ      CHECK IF 'FROM' COMML                        
         BAS   RE,GETEL            (DOES IT HAVE A 'TO' ELEMENT)                
         B     *+8                                                              
LR22     BAS   RE,NEXTEL                                                        
         BNE   LR24                                                             
*                                                                               
         TM    TAOCSTAT,TAOCSTO                                                 
         BNO   LR22                IF ONE COMML IS NOT OK MAYBE NEXT IS         
         GOTO1 SEQ                                                              
         CLC   KEY(TLCOPRD-TLCOD),SVKEYCO  KEEP CHECKING TIL CLIENT             
         BNE   LR47                        CHANGES                              
         B     LR21                                                             
         DROP  R4                                                               
*                                                                               
LR24     MVC   AIO,AIO1            RESET I/O AREA                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'AC',TICLI),0                              
         BNE   LR47                                                             
*                                                                               
         L     R4,AIO                                                           
         USING TLCLD,R4                                                         
         MVC   TIAGY,TLCLAGY                                                    
         MVC   TICLG,SPACES                                                     
         MVC   TINAME,TGNAME                                                    
*                                                                               
         MVC   TISHORT,SPACES                                                   
         L     R4,AIO                                                           
         USING TASND,R4                                                         
         MVI   ELCODE,TASNELQ      GET SHORT NAME                               
         BAS   RE,GETEL                                                         
         BNE   *+10                R4=A(SHORT NAME ELEMENT)                     
         MVC   TISHORT,TASNAME     SAVE SHORT NAME                              
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ      GET CLIENT INFO                              
         BAS   RE,GETEL                                                         
         BNE   LR45                R4=A(CLIENT INFORMATION ELEMENT)             
         USING TACID,R4                                                         
         OC    TACICLG,TACICLG     IF CLIENT GROUP SPECIFIED                    
         BZ    LR45                                                             
         MVC   TICLG,TACICLG       SET IT                                       
*                                                                               
LR45     BAS   RE,PRRECI           PRINT OUT INFORMATION                        
*                                                                               
LR47     MVC   KEY,SVKEY           RESTORE COMMERCIAL KEY                       
         GOTO1 HIGH                RE-READ COMMERCIAL RECORD                    
         LA    R1,KEY                                                           
         USING TLCOD,R1                                                         
         MVI   TLCOPRD,X'FF'       GET NEXT CLIENT                              
         B     LR10                                                             
         DROP  R4                                                               
*                                                                               
LR50     LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVI   NLISTS,16           IN ORDER TO GET CONTROL BACK                 
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15              AT END OF PAGE                            
*                                                                               
LR60     XC    TIQSKEY,TIQSKEY                                                  
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(14,R1),=C'CLIENT RECORDS'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*        PROCESS RECORD                                                         
*                                                                               
PRRECI   NTR1                      FOR SPECIFIC CALL                            
         L     R4,AIO              R4=A(CLIENT RECORD)                          
         B     PR2                                                              
*                                                                               
PRREC    L     R4,TIAREC           R4=A(CLIENT RECORD)                          
*                                                                               
PR2      CLI   AGYOVR,C'N'         SUPPRESS AGENCY OVERRIDE                     
         BNE   PR5                                                              
         OC    TIAGY,TIAGY         IS THERE AN AGENCY                           
         BNZ   PRRX                                                             
*                                                                               
         USING LISTD,R2                                                         
PR5      MVI   CLCOD,C'N'                                                       
         XR    R1,R1               CLIENT NOT LOCKED                            
         MVI   ELCODE,TACIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PR8                                                              
         USING TACID,R4                                                         
*                                                                               
         TM    TACISTAT,TACISCOD   COD CLIENT?                                  
         BZ    PR7                                                              
         MVI   CLCOD,C'Y'                                                       
*                                                                               
PR7      TM    TACISTAT,TACISLCK                                                
         BZ    PR8                                                              
         LA    R1,1                CLIENT LOCKED                                
*                                                                               
PR8      LTR   R1,R1               IF CLIENT LOCKED                             
         BZ    *+16                                                             
         CLI   OPTLOCK,C'Y'        REJECT IF NOT DISPLAYING THEM                
         BNE   PRRX                                                             
         B     *+12                                                             
         CLI   OPTLOCK,C'Y'        ELSE REJECT IF DISPLAYING THEM               
         BE    PRRX                                                             
*                                                                               
PR10     MVC   CLCODE,TICLI        CLIENT CODE                                  
         MVC   CLAGY,TIAGY         AGENCY                                       
         CLC   TISHORT,SPACES      IS A SHORT NAME PRESENT                      
         BH    PR15                NO - THEN                                    
         MVC   CLNAME,TINAME       MOVE 1ST 16 BYTES OF REG NAME                
         B     PR17                                                             
*                                                                               
PR15     MVC   CLNAME,TISHORT                                                   
*                                                                               
PR17     MVC   CLCGRP,TICLG        CLIENT GROUP                                 
         CLI   RDSEQ,C'T'                                                       
         BE    PR17C                                                            
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
*                                                                               
PR17C    CLI   MODE,PRINTREP       REPORT                                       
         BNE   PR20                                                             
         GOTO1 CATCHIOS            ENSURE IO OVERUSE FOR REPORTS                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'       COUNT LINES OUTPUT                           
         B     PRRX                                                             
*                                                                               
PR20     CLI   LISTNUM,15          AT TRUE END OF PAGE                          
         BNE   PR30                                                             
         MVC   MYMSGNO1,OKNO       HIT ENTER FOR NEXT - MSG                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SCLSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR30     GOTO1 LISTMON             CALL LISTMON                                 
PRRX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
OPTERR   MVI   ERROR,ERCOMPT                                                    
         B     ERRXIT                                                           
*                                                                               
AGYERR   MVI   ERROR,ERMISAGY                                                   
         B     ERRXIT                                                           
*                                                                               
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'PR',CL8'PRODUCT ',CL8'LIST'                                  
PF13     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'CLAGY-1),AL2(CLAGY-LISTD)                         
         DC    AL1(KEYTYCUR,L'CLCODE-1),AL2(CLCODE-LISTD)                       
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'CO',CL8'COMMERCL',CL8'LIST'                                  
PF14     DC    AL1(KEYTYCUR,L'CLAGY-1),AL2(CLAGY-LISTD)                         
         DC    AL1(KEYTYCUR,L'CLCODE-1),AL2(CLCODE-LISTD)                       
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'AY',CL8'AGY   ',CL8'DISPLAY'                                 
PF15     DC    AL1(KEYTYCUR,L'CLAGY-1),AL2(CLAGY-LISTD)                         
PF15X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
OPTTAB   DS    0H                  OPTIONS TABLE                                
         DC    CL10'LOCKED    ',AL2(VALLOCK-T70234)                             
         DC    CL10'AGENCY    ',AL2(VALAGY-T70234)                              
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        SPECS FOR SPOOLING                                                     
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'CLIENT LIST'                                             
         SSPEC H2,33,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,15,C'AGENCY'                                                  
         SSPEC H4,33,C'CLIENT NAME'                                             
         SSPEC H4,58,C'CLT GRP'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,15,C'------'                                                  
         SSPEC H5,33,C'-----------'                                             
         SSPEC H5,58,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
LISTD    DSECT                                                                  
CLCODE   DS    CL6                                                              
         DS    CL8                                                              
CLAGY    DS    CL6                                                              
         DS    CL12                                                             
CLNAME   DS    CL16                                                             
         DS    CL9                                                              
CLCGRP   DS    CL6                                                              
         DS    CL4                                                              
CLCOD    DS    CL1                                                              
*                                                                               
         SPACE 2                                                                
*              DSECT TO COVER OPTIONS TABLE                                     
*                                                                               
OPTD     DSECT                                                                  
OPTLHS   DS    CL10                                                             
OPTDISP  DS    AL2                                                              
OPTNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR34D                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 COUNT NUMBER OF OUTPUT LINES                 
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
INTPRD   DS    CL1                 PRODUCTION INTERFACE RECS                    
AGYOVR   DS    CL1                 AGENCY OVERRIDES                             
OPTLOCK  DS    CL1                 Y=SHOW LOCKED CLIENTS                        
SVKEY    DS    CL48                SAVED KEY                                    
SVKEYCO  DS    CL48                SAVED COMML KEY                              
         SPACE 2                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031TAGEN34   08/18/05'                                      
         END                                                                    
