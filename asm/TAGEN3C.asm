*          DATA SET TAGEN3C    AT LEVEL 068 AS OF 02/11/14                      
*PHASE T7023CA,*                                                                
         TITLE 'T7023C - W4 LIST'                                               
T7023C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7023C                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=TWAHOLE                                   
         USING TWAD,R7                                                          
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
W410     GOTO1 INITIAL,DMCB,PFTABLE                                             
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   SW4LPHD(13),=C'SEL PID NUM  '                                    
         OI    SW4LPHDH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
W420     CLI   MODE,LISTRECS                                                    
         BNE   W430                                                             
         TWAXC SW4SELH,SW4LSTH,PROT=Y   CLEAR SCREEN                            
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR 1ST LINE                               
         B     LR                                                               
*                                                                               
W430     CLI   MODE,PRINTREP                                                    
         BNE   W4X                                                              
         XC    TIKEY,TIKEY         ENSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
W4X      B     XIT                                                              
*                                                                               
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       MVI   KEYCHG,C'N'                                                      
*                                                                               
         TM    SCRSTAT,RECCHG      IF RECORD CHANGED                            
         BZ    VK00                                                             
         MVI   KEYCHG,C'Y'                                                      
         NI    SW4FMTH+4,X'FF'-X'20'     TURN OFF VALIDATED BITS                
         NI    SW4STRH+4,X'FF'-X'20'                                            
         NI    SW4CORPH+4,X'FF'-X'20'                                           
         NI    SW4OPTSH+4,X'FF'-X'20'                                           
*                                                                               
*                                                                               
VK00     LA    R2,SW4STRH          START AT SPECIFIC SS NUM                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK7                                                              
         MVI   KEYCHG,C'Y'                                                      
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK7                                                              
         MVI   SPLIT,0                                                          
         LA    R4,TIQSTART                                                      
         ZIC   R6,5(R2)                                                         
         CLI   SW4FMT,C'C'         IF BY CODE CONTINUE REGULARLY                
         BNE   VK1                                                              
         LR    R3,R6                                                            
         B     VK5                                                              
*                                  CANNOT USE SCANNER - NEED 2X16 BYTES         
VK1      XR    R3,R3               COUNTER FOR MOVE                             
         LA    R1,8(R2)            MOVE ALONG LINE WITH R1                      
*                                                                               
VK3      CLI   0(R1),C'/'          DELIMITER BETWEEN LAST & FIRST NAMES         
         BE    VK5                                                              
         LA    R3,1(R3)            INCREMENT COUNTER                            
         LA    R1,1(R1)            NEXT POSITION                                
         CR    R3,R6               AT END OF INPUT                              
         BL    VK3                                                              
*                                                                               
VK5      ZIC   R5,SPLIT            2 NAMES                                      
         LA    R5,1(R5)                                                         
         STC   R5,SPLIT                                                         
*                                                                               
         CH    R3,=H'0'            ENSURE SOMETHING TO MOVE                     
         BE    VK6                                                              
         CLI   SPLIT,2             IF THIS IS 2ND NAME                          
         BL    VK5A                                                             
         CH    R3,=H'8'            ONLY ALLOW 8 CHARS                           
         BNH   VK5A                                                             
         LA    R3,8                                                             
*                                                                               
VK5A     BCTR  R3,0                SUB 1 FOR MOVE                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)       INTO TIQSTART                                
         CLI   SW4FMT,C'C'         IF BY CODE CONTINUE REGULARLY                
         BE    VK6                                                              
         CLI   0(R1),C'/'          DELIMITER BETWEEN LAST & FIRST NAMES         
         BNE   VK6                 MOVE INTO START OF TIQSTART                  
         LA    R1,1(R1)            SKIP THE DELIMITER                           
         LA    R4,L'TLW4NLST(R4)        POINT TO 2ND HALF OF NAME               
         AR    R2,R3               RESET R2 FOR NEXT EXECUTED MOVE              
         LA    R2,2(R2)            1 = BCTR FOR MOVE & 1 = '/'                  
         XR    R3,R3               RESET THE COUNTER                            
         B     VK3                 MOVE INTO START OF TIQSTART                  
*                                                                               
VK6      OC    TIQSTART,SPACES                                                  
*                                                                               
VK7      OI    SW4STRH+4,X'20'                                                  
         LA    R2,SW4CORPH         CORP FILTER                                  
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK8                                                              
         MVI   KEYCHG,C'Y'                                                      
         XC    SVCORP,SVCORP                                                    
         CLI   5(R2),0                                                          
         BE    VK8                                                              
         CLI   5(R2),9             INPUT MUST BE 9 LONG                         
         BE    VK7A                                                             
         TM    TGSYSTAT,TASYSPID   UNLESS USING PIDS                            
         BZ    INVERR                                                           
         CLI   5(R2),6             WHEN INPUT CAN BE 6 LONG                     
         BNE   INVERR                                                           
         MVC   TGPID,SW4CORP                                                    
         GOTO1 SSNUNPK,DMCB,SW4CORP,TGSSN                                       
         BNE   VK7A                                                             
         MVC   SW4CORP,TGSSN                                                    
         MVI   SW4CORPH+5,9                                                     
         OI    SW4CORPH+6,X'80'                                                 
VK7A     MVI   W4FOUND,C'Y'                                                     
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'2C',(R2)),SW4CNAMH                        
         BE    *+8                                                              
         MVI   W4FOUND,C'N'                                                     
         TM    TGSYSTAT,TASYSPID                                                
         BZ    VK7B                                                             
         XC    SW4CORP,SW4CORP                                                  
         MVC   SW4CORP(L'TGPID),TGPID                                           
         MVI   SW4CORPH+5,6                                                     
VK7B     CLI   W4FOUND,C'Y'                                                     
         BNE   INVERR                                                           
         MVC   SVCORP,TGSSN                                                     
         L     R4,AIO              VALIDATE IT IS A CORPORATION                 
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TAW4TYPE,TAW4TYCO   CORP RECORD                                  
         BE    VK8                                                              
         CLI   TAW4TYPE,TAW4TYTR   TRUSTEE RECORD                               
         BE    VK8                                                              
         MVC   SW4CNAM,SPACES      IF INVALID - ERASE NAME                      
         OI    SW4CNAMH+6,X'80'                                                 
         B     INVERR                                                           
*                                                                               
VK8      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SW4FMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         MVI   KEYCHG,C'Y'                                                      
         MVI   RDSEQ,C'A'          DEFAULT TO ALPHA SEQUENCE                    
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         OC    SW4CORP,SW4CORP     IF A CORP NUM INPUT                          
         BNZ   COMPTERR            THEN CANNOT INPUT FORMAT                     
*                                                                               
         CLI   8(R2),C'A'                                                       
         BE    VK10                                                             
         MVI   RDSEQ,C'C'                                                       
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BNE   INVERR                                                           
*                                                                               
VK10     OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         LA    R2,SW4OPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         BAS   RE,VALOPTS          VALIDATE OPTIONS FIELD                       
*                                                                               
         CLI   KEYCHG,C'Y'                                                      
         BNE   VKX                                                              
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS FIELD                                           
         SPACE 1                                                                
VALOPTS  NTR1                                                                   
         XC    OPTS,OPTS           CLEAR OPTION FIELDS                          
*                                                                               
         MVI   KEYCHG,C'Y'                                                      
         CLI   5(R2),0                                                          
         BE    VOPT60                                                           
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
         BASR  RE,RF               GO VALIDATE                                  
*                                                                               
         LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
*                                                                               
VOPT60   OI    4(R2),X'20'         VALIDATED                                    
         MVI   ERRDISP,0                                                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES                            
         SPACE 1                                                                
         USING SCAND,R3            R3 = A(SCAN BLOCK ENTRY)                     
         SPACE 1                                                                
VALTYPE  DS    0H                                                               
         CLI   SCLEN2,1            LENGTH OF RHS MUST BE 1                      
         BNE   INVERR                                                           
         LA    R4,TYPETAB                                                       
VTYPE10  CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    INVERR                                                           
         CLC   SCDATA2(1),0(R4)    MATCH ON W4 TYPE                             
         BE    VTYPE20                                                          
         LA    R4,1(R4)                                                         
         B     VTYPE10                                                          
VTYPE20  MVC   OPTTYPE,SCDATA2     TYPE TO FILTER RECORDS WITH                  
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         MVI   LISTSW,C'F'         START LIST FROM BEGINING                     
         XC    TIFCORP,TIFCORP     CLEAR FILTERS                                
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVC   TIFCORP,SVCORP                                                   
         MVI   TIREAD,TLW4NCDQ     LIST BY W4 NAME                              
         CLI   SW4CORPH+5,0        IF CORP FILTER                               
         BNE   INIT10              THEN SWITCH TIREAD BACK AND SYSIO            
         CLI   RDSEQ,C'A'               WILL CHANGE IT                          
         BE    INITX               ELSE LIST BY CODE                            
*                                                                               
INIT10   MVI   TIREAD,TLW4CDQ                                                   
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 PGCNTL,DMCB,TABLE,TIKEY,TIQSKEY                                  
         MVI   NLISTS,16           IN ORDER TO GET CONTROL BACK                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15              AFTER 1 FULL PAGE                         
*                                                                               
         CLI   TIERROR,TINOTOLN                                                 
         BE    ONLINERR                                                         
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(10,R1),=C'W4 RECORDS'                                          
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
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    DS    0H                                                               
         BAS   RE,FILTER           FILTER OPTIONS                               
         BNE   PRRX                                                             
         MVC   LISTAR,SPACES       CLEAR PREVIOS LINE                           
         USING LISTD,R2                                                         
         MVC   W4SSN,TISSN         SS NUMBER                                    
         TM    TGSYSTAT,TASYSPID                                                
         BZ    PR02                                                             
         MVC   W4SSN,SPACES                                                     
         GOTO1 SSNPACK,DMCB,TISSN,W4SSN                                         
*                                                                               
PR02     LA    R3,TIKEY            IF AKA PASSIVE POINTER                       
         USING TLW4PD,R3              SHOW AKA NAME                             
         CLI   TLW4PCD,TLW4NCDQ    IF LISTING BY NAME -                         
         BNE   PR04                   CHECK STATUS                              
         TM    TLW4NSTA,TLW4NSAK                                                
         BNO   PR04                                                             
         L     R4,TIAREC                                                        
         USING TAAKD,R4                                                         
         MVI   ELCODE,TAAKELQ      POINT TO AKA ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   W4STAR,C'*'                                                      
         MVC   W4NAME,TAAKNAM2     LAST NAME                                    
         MVC   W4FIRST,TAAKNAM1    FIRST NAME                                   
         B     PR08                                                             
         DROP  R3,R4                                                            
*                                                                               
PR04     L     R4,TIAREC                                                        
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ      POINT TO W4 ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    *+12                                                             
         CLI   TAW4TYPE,TAW4TYTR                                                
         BNE   PR05                                                             
         MVC   W4NAME(L'TAW4CRPN),TAW4CRPN                                      
         B     PR08                                                             
*                                                                               
PR05     MVC   W4NAME,TAW4NAM2     LAST NAME                                    
         MVC   W4FIRST,TAW4NAM1    FIRST NAME                                   
*                                                                               
PR08     L     R4,TIAREC                                                        
         USING TAA2D,R4                                                         
         MVI   ELCODE,TAA2ELQ      POINT TO NEW ADDRESS ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   PR16                                                             
*                                                                               
         MVC   W4ADDR1,TAA2ADD1    MOVE INTO LINE - 1 ADDRESS LINE              
         OC    TAA2CITY,TAA2CITY   IF HAVE CITY                                 
         BZ    PR10                                                             
         XC    WORK(39),WORK                                                    
         MVC   WORK(25),TAA2CITY      SHOW IT                                   
         MVC   WORK+26(2),TAA2ST      WITH STATE                                
         MVC   WORK+29(10),TAA2ZIP    AND ZIP                                   
         GOTO1 SQUASHER,DMCB,WORK,39  SQUASHED                                  
         MVC   W4ADDR2,WORK           ON LAST ADDRESS LINE                      
         B     PR20                                                             
*                                                                               
PR10     MVC   W4ADDR2,TAA2ADD3    SHOW 3RD ADDRESS LINE                        
         OC    TAA2ADD3,TAA2ADD3   IF HAVE 3RD ADDRESS LINE                     
         BNZ   *+10                                                             
         MVC   W4ADDR2,TAA2ADD2    ELSE SHOW 2ND ADDRESS LINE INSTEAD           
         B     PR20                                                             
*                                                                               
PR16     L     R4,TIAREC                                                        
         USING TAADD,R4                                                         
         MVI   ELCODE,TAADELQ      POINT TO ADDRESS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   PR20                                                             
*                                                                               
         ZIC   R1,2(R4)            NUMBER OF LINES IN ELEMENT                   
         BCTR  R1,0                   LESS 1ST LINE                             
         LA    R4,3(R4)            POINT TO 1ST ACTUAL ADDRESS LINE             
         MVC   W4ADDR1,0(R4)       MOVE INTO LINE - 1 ADDRESS LINE              
         CH    R1,=H'0'            IF ONLY 1 LINE OF ADDRESS                    
         BE    PR20                   SKIP LOOP                                 
*                                                                               
PR18     LA    R4,L'TAADADD(R4)    POINT TO NEXT LINE                           
         BCT   R1,PR18                                                          
         MVC   W4ADDR2,0(R4)       MOVE INTO LINE - LAST ADDRESS LINE           
*                                                                               
PR20     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PR40                                                             
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY I/O-S               
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PR40     CLI   LISTNUM,15          IF END OF PAGE                               
         BNE   PR50                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SW4SELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR50     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
*                                                                               
*              ROUTINE TO FILTER ON OPTIONS                                     
*                                                                               
FILTER   NTR1                                                                   
*                                                                               
*                                                                               
         CLI   RECNUM,TD           TD1 ACTION?                                  
         BNE   FILTER10                                                         
         L     R4,TIAREC                                                        
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAD1ELQ      ARE THERE ANY TD1 ELEMENTS?                  
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
*                                                                               
*                                                                               
FILTER10 CLI   OPTTYPE,C' '        TYPE OPTION SET?                             
         BNH   YES                                                              
         L     R4,TIAREC                                                        
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ      POINT TO W4 ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TAW4TYPE,OPTTYPE    ONLY LIST RECORDS OF THIS TYPE               
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
COMPTERR MVI   ERROR,ERCOMPT                                                    
         B     ERRXIT                                                           
*                                                                               
ONLINERR LA    R2,CONRECH                                                       
         MVC   MYMSGNO,=Y(ERENTCNT)                                             
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
         SPACE 2                                                                
*                                                                               
PFTABLE  DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'DUE',CL8'DUE COMP',CL8'LIST    '                             
PF13     DC    AL1(KEYTYCUR,L'TGSSN-1),AL2(W4SSN-LISTD)                         
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'LIE',CL8'LIEN    ',CL8'LIST    '                             
PF14     DC    AL1(KEYTYCUR,L'TGSSN-1),AL2(W4SSN-LISTD)                         
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'CO',CL8'CCOM    ',CL8'LIST    '                              
PF15     DC    AL1(KEYTYCUR,L'TGSSN-1),AL2(W4SSN-LISTD)                         
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3'GRT',CL8'GRT     ',CL8'LIST    '                             
PF16     DC    AL1(KEYTYCUR,L'TGSSN-1),AL2(W4SSN-LISTD)                         
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF17X-*,17,0,(PF17X-PF17)/KEYLNQ,0)                          
         DC    CL3'CHE',CL8'CHECK   ',CL8'LIST    '                             
PF17     DC    AL1(KEYTYCUR,L'TGSSN-1),AL2(W4SSN-LISTD)                         
PF17X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
OPTTAB   DS    0H                                                               
         DC    CL10'TYPE      ',AL2(VALTYPE-T7023C)                             
         DC    X'FF'                                                            
*                                                                               
TYPETAB  DC    C'AICEFT',X'FF'                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'W4 LIST'                                                 
         SSPEC H2,32,C'-------'                                                 
         SPACE 1                                                                
         SSPEC H4,1,C'SS NUMBER'                                                
         SSPEC H4,11,C'LAST NAME'                                               
         SSPEC H4,28,C'FIRST NAME'                                              
         SSPEC H4,45,C'ADDRESS 1'                                               
         SSPEC H4,62,C'ADDRESS 2'                                               
         SPACE 1                                                                
         SSPEC H5,1,C'---------'                                                
         SSPEC H5,11,C'---------'                                               
         SSPEC H5,28,C'----------'                                              
         SSPEC H5,45,C'---------'                                               
         SSPEC H5,62,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
W4SSN    DS    CL9                                                              
W4STAR   DS    CL1                                                              
W4NAME   DS    CL16                                                             
         DS    CL1                                                              
W4FIRST  DS    CL16                                                             
         DS    CL1                                                              
W4ADDR1  DS    CL16                                                             
         DS    CL1                                                              
W4ADDR2  DS    CL16                                                             
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER OPTIONS TABLE                                     
*                                                                               
OPTD     DSECT                                                                  
OPTLHS   DS    CL10                                                             
OPTDISP  DS    AL2                                                              
OPTNEXT  EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR3CD                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
KEYCHG   DS    CL1                 SWITCH / KEY CHANGED OR NOT                  
SPLIT    DS    XL1                 SWITCH / 2 NAMES FOR STARTING FIELD          
W4FOUND  DS    XL1                 W4 FOUND?                                    
SVCORP   DS    XL9                 SAVED CORP FILTER                            
*                                                                               
OPTS     DS    0CL5                OPTIONS                                      
OPTTYPE  DS    CL1                 LIST W4'S WITH SPECIFIC TYPE                 
         DS    CL4                 N/D                                          
*                                                                               
SVKEY    DS    CL48                SAVED KEY                                    
TABLE    DS    16CL(L'TLW4KEY)     TABLE TO PAGE (TO RESET GENCON KEY)          
         SPACE 5                                                                
TWAD     DSECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
         SPACE 3                                                                
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068TAGEN3C   02/11/14'                                      
         END                                                                    
