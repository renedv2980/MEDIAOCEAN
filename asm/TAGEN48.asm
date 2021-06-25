*          DATA SET TAGEN48    AT LEVEL 013 AS OF 07/30/12                      
*PHASE T70248C,*                                                                
         TITLE 'T70248 - CAST LIST BY SS NUM'                                   
T70248   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70248                                                         
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
COM10    GOTO1 INITIAL,DMCB,PFCAST                                              
         TM    TGSYSTAT,TASYSPID                                                
         BZ    COM15                                                            
         MVC   SCOLSSN(7),=C'Pid Num'                                           
         OI    SCOLSSNH+6,X'80'                                                 
*                                                                               
COM15    CLI   MODE,VALKEY             FIRST TIME IN                            
         BE    VK                                                               
*                                                                               
COM20    CLI   MODE,LISTRECS                                                    
         BNE   COM30                                                            
         BAS   RE,STAFF                                                         
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
COM30    CLI   MODE,PRINTREP                                                    
         BNE   COMX                                                             
         BAS   RE,STAFF                                                         
         XC    KEY,KEY             START REPORT FROM BEGINING                   
         ZAP   COUNTER,=P'0'       LINE COUNTER                                 
         LA    R2,CASPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
COMX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VK       LA    R2,SCOSSNH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SCOAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK05                                                             
         CLI   SCOSSNH+5,6                                                      
         BH    VK05                                                             
         MVC   TGPID,SCOSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK05                                                             
         MVC   SCOSSN,TGSSN                                                     
         MVI   SCOSSNH+5,9                                                      
VK05     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SCONAMEH                        
         OI    4(R2),X'20'                                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK10                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCOSSN,SPACES                                                    
         MVC   SCOSSN(L'TGPID),TGPID                                            
         MVI   SCOSSNH+5,6                                                      
         OI    SCOSSNH+6,X'80'                                                  
*                                                                               
VK10     LA    R2,SCOAGYH          AGENCY                                       
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         MVC   SCOAGYN,SPACES                                                   
         OI    SCOAGYNH+6,X'80'    TRANSMIT                                     
         NI    SCOOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),SCOAGYNH    AGENCY              
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK20     OI    4(R2),X'20'         VALIDATE                                     
         LA    R2,SCOOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VKX                                                              
         BAS   RE,VALOPTS                                                       
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO                        
         BAS   RE,INIT             SET UP SYSIO                                 
*        BAS   RE,STAFF            SET STAFF VARIABLES                          
*                                                                               
VKX      B      XIT                                                             
         EJECT                                                                  
*              VALIDATE OPTIONS FIELD                                           
         SPACE 1                                                                
VALOPTS  NTR1                                                                   
         XC    OPTS,OPTS                                                        
*                                                                               
         LA    R2,SCOOPTSH         R2 = A(FIELD)                                
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
         BASR  RE,RF               GO VALIDATE                                  
*                                                                               
VOPT40   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
*                                                                               
VOPTX    OI    4(R2),X'20'                                                      
         MVI   ERRDISP,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES                            
         SPACE 1                                                                
         USING SCAND,R3            R3 = A(SCAN BLOCK ENTRY)                     
         SPACE 1                                                                
VALDATE  DS    0H                                                               
         LR    R5,RE               SAVE ADDRESS OF RTRN POINT                   
         TM    SCVAL2,X'80'        TEST VALID NUMERIC                           
         BZ    VALD10                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SCBIN2+1       YES, MOVE TO RF                              
         LCR   RF,RF               AND COMPLEMENT                               
         GOTO1 ADDAY,DMCB,TGTODAY0,WORK,(RF) GO BACK N'DAYS                     
         B     VALD20                                                           
         SPACE 1                                                                
VALD10   GOTO1 DATVAL,DMCB,SCDATA2,WORK  ELSE MUST BE A DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    DATINV                                                           
         SPACE 1                                                                
VALD20   GOTO1 DATCON,DMCB,(0,WORK),(1,OPTDATE) LAST ACTIVE DATE                
         LR    RE,R5               RESTORE RE                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         XC    TIFAGY,TIFAGY       CLEAR FILTERS                                
         XC    TIFSSN,TIFSSN                                                    
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVC   TIFSSN,TGSSN        SS NUM                                       
         MVI   TIREAD,TLCACCDQ     SET CAST RECORD                              
         CLI   SCOAGYH+5,0         AGENCY FILTER                                
         BE    INITX                                                            
         MVC   TIFAGY,TGAGY        AGENCY                                       
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP STAFF VARIABLES                                                 
*                                                                               
STAFF    NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK+8(2),TWAORIG                                                
         MVC   AIO,AIO3                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   AIO,AIO1                                                         
         BE    *+6                                                              
         DC    H'00'               SET USER ID VARIABLES                        
*                                                                               
         MVC   AIO,AIO2                                                         
         OC    TIQSTAFF,TIQSTAFF                                                
         BNZ   *+10                                                             
         MVC   TIQSTAFF,TGCTSTAF                                                
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A4',TIQSTAFF)                             
         BE    *+6                                                              
         DC    H'00'               GET STAFF RECORD INTO AIO2                   
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,8            SET NUMBER OF LINES FOR LIST                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,7                                                         
*                                                                               
         CLI   TIERROR,0           IF RETURNED WITH ERROR                       
         BE    LR2                                                              
         CLI   OFFLINE,C'Y'        AND ONLINE PROGRAM                           
         BE    LR2                                                              
         B     ONLNERR             GIVE MSG - JOB CANNOT BE RUN ONLINE          
*                                                                               
LR2      CLI   MODE,PRINTREP                                                    
         BE    LR4                                                              
         L     R3,ATHISLST         CLEAR BOT OF SCREEN                          
         LA    R4,SCOLST                                                        
         CR    R3,R4                                                            
         BNL   LRX                                                              
         TWAXC (R3),(R4),PROT=Y                                                 
         B     LRX                                                              
*                                                                               
LR4      GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(12,R1),=C'CAST RECORDS'                                        
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
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         L     R4,TIAREC                                                        
*                                                                               
         USING CASD1,R2                                                         
         USING TLCAD,R4                                                         
         MVC   TGCOM,TLCACOM       INTERNAL COM NUM                             
         GOTO1 XNAME,DMCB,(X'80',TLCOCCDQ),COXNAME,TIKEY                        
         CLC   COXNAME,SPACES      IF COMMERCIAL FOUND                          
         BNH   PRRNO               REESTABLISH READ SEQUENCE                    
         DROP  R4                                                               
*                                                                               
PRR2     BAS   RE,FILTER                                                        
         BNE   PRRNO                                                            
*                                                                               
         USING TLCOD,R4            COMMERCIAL IN AIO FROM XNAME                 
         L     R4,AIO              GET COMML AGY,TITLE, CLIENT & PRD            
         XC    SVCLI,SVCLI         ENSURE RE-READ CLIENT RECORD                 
         MVC   CAAGY,TLCOAGY       AGENCY                                       
         MVC   TGAGY,TLCOAGY       AGENCY CODE                                  
         MVC   TGCLI,TLCOCLI       CLIENT CODE                                  
         MVC   TGPRD,TLCOPRD       PRODUCT CODE                                 
         MVI   ELCODE,TANAELQ      GET NAME                                     
         BAS   RE,GETEL                                                         
         BNE   PRR20                                                            
         GOTO1 CHAROUT,DMCB,(X'80',TASNELQ)                                     
         MVC   CONAME(16),TGNAME    RETURN NAME                                 
         DROP  R4                                                               
*                                                                               
PRR20    MVC   CACID,COXNAME       COMMERCIAL ID                                
         MVC   CACONM,CONAME       TITLE                                        
         MVC   CACAT,TICAT         CATEGORY                                     
         MVC   CACAM,TIONOF        ON/OFF CAMERA                                
         MVC   CAAGENT,TIAGT       AGENT                                        
         MVC   CAUNI,TIUN          UNION                                        
         MVC   CAYR,TIYEAR         YEAR                                         
*                                                                               
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY IO'S                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   PRR30                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRR40                                                            
*                                                                               
PRR30    L     R2,ATHISLST         BUMP TO  NEXT DETAIL LINE                    
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         XC    8(L'SCOCOM,R2),8(R2) CLEAR LINE                                  
         OI    6(R2),X'80'         TRANSMIT LINE                                
         LA    R2,8(R2)            PAST FIELD HEADER                            
*                                                                               
         USING CASD2,R2                                                         
PRR40    MVC   CACLT,SPACES                                                     
         MVC   CACLT,TGCLI         CLIENT                                       
         CLC   TGAGY,SVAGY                                                      
         BNE   PRR50                                                            
         CLC   TGCLI,SVCLI                                                      
         BE    PRR60                                                            
*                                                                               
PRR50    GOTO1 XNAME,DMCB,TLCLCDQ,CLNAME,TIKEY                                  
         XC    SVPRD,SVPRD         ENSURE RE-READ PRD RECORD                    
*                                                                               
PRR60    MVC   CACLNM,CLNAME       CLIENT NAME                                  
         MVC   CAPRD,TGPRD         PRODUCT                                      
         CLC   TGPRD,SPACES                                                     
         BH    PRR62                                                            
         MVC   PRNAME,SPACES                                                    
         B     PRR70                                                            
*                                                                               
PRR62    CLC   TGPRD,SVPRD                                                      
         BE    PRR70                                                            
*                                                                               
PRR65    MVC   PRNAME,SPACES                                                    
         GOTO1 XNAME,DMCB,TLPRCDQ,PRNAME,TIKEY                                  
         MVC   SVPRD,TGPRD                                                      
*                                                                               
PRR70    MVC   SVAGY,TGAGY                                                      
         MVC   SVCLI,TGCLI                                                      
         MVC   CAPRNM,PRNAME       PRODUCT NAME                                 
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS                            
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY IO'S                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   PRR80                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PRR80    CLI   LISTNUM,7           IF END OF PAGE                               
         BNE   PRR90               BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SCOSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PRR90    GOTO1 LISTMON             CALL LISTMON                                 
         L     R2,ATHISLST         UPDATE GENCON'S ATHISLST                     
         ZIC   RE,0(R2)            BUMP PAST PROTECTED SEL FIELD                
         AR    R2,RE                                                            
         ZIC   RE,0(R2)            BUMP PAST DETAIL LINE                        
         AR    R2,RE                                                            
         ST    R2,ATHISLST         RESET ATHISLST FOR LISTMON                   
*                                       GENCON                                  
         B     PRRX                                                             
*                                                                               
PRRNO    GOTO1 HIGH                RE-ESTBALISH READ SEQUENCE                   
PRRX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO FILTER COMMERCIAL RECDS ON REQUESTED OPTIONS          
         SPACE 1                                                                
FILTER   NTR1                                                                   
         USING TLCOD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
*                                                                               
*&&DO                                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    FIL1000                                                          
         LHI   R2,1                                                             
*                                                                               
         USING FAWSSVRD,R1                                                      
FIL01    LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    FIL1000             STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
FIL02    CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    FIL01                                                            
*                                                                               
         CLC   TLCOAGY,TAVAAGY     IF AGENCY IS FOUND IN STAFF LIMITS           
         BNE   FIL04                                                            
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         BE    FIL1000             ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
FIL03    CLC   TLCOCLI,0(RF)       IF CLIENT IS FOUND IN STAFF LIMITS           
         BE    FIL1000             ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   FIL03                                                            
*                                                                               
FIL04    ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     FIL02                                                            
         EJECT                                                                  
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*&&                                                                             
         L     R4,TIAREC           CAST RECORD                                  
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL            R4=A(CAST ELEMENT)                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
         TM    TACASTA3,TACASPPL   IGNORE PAYROLL PLUS EMPLOYEE                 
         BO    NO                                                               
*                                                                               
FIL1000  OC    OPTDATE,OPTDATE     IF ACTIVITY DATE OPTION                      
         BZ    YES                                                              
*                                                                               
         L     R4,AIO              COMMERCIAL IN AIO FROM XNAME                 
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            R4=A(COMMERCIAL ELEMENT)                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         OC    TACOPDTE,TACOPDTE   IF ANY LAST PYMT DATE                        
         BZ    FIL2000                                                          
         CLC   TACOPDTE,OPTDATE    IF LAST PYMT DATE >= CUTOFF DATE             
         BNL   YES                 LIST RECORD                                  
         B     NO                                                               
         DROP  R4                                                               
*                                                                               
FIL2000  MVI   ELCODE,TAACELQ      ELSE, IF NO LAST PYMT DATE                   
         GOTO1 ACTVOUT,DMCB,(X'20',0)                                           
         ICM   R4,15,DMCB          R4=A(ACTIVITY ELEMENT)                       
         BZ    NO                                                               
         USING TAACD,R4                                                         
         CLC   TAACCDTE,OPTDATE    IF LAST PYMT DATE >= CUTOFF DATE             
         BNL   YES                 LIST RECORD                                  
         B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ERROR & EXIT ROUTINES                                            
         SPACE 1                                                                
DATINV   MVI   ERROR,INVDATE       INVALID DATE                                 
         B     ERRXIT                                                           
*                                                                               
ONLNERR  MVI   ERROR,ERNOTOLN      JOB CANNOT BE RUN ONLINE                     
         LA    R2,CONRECH                                                       
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     ERRXIT                                                           
*                                                                               
INFXIT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
         SPACE 1                                                                
PFCAST   DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF10AX-*,10,0,(PF10AX-PF10A)/KEYLNQ,0)                       
         DC    CL3'CA',CL8'CAST    ',CL8'LIST'                                  
PF10A    DC    AL1(KEYTYCUR,L'CAAGY-1),AL2(CAAGY-CASD1)                         
         DC    AL1(KEYTYCUR,L'CACID-1),AL2(CACID-CASD1)                         
PF10AX   EQU   *                                                                
*                                                                               
         DC    AL1(PF11AX-*,11,0,(PF11AX-PF11A)/KEYLNQ,0)                       
         DC    CL3'HI',CL8'HISTORY ',CL8'LIST'                                  
PF11A    DC    AL1(KEYTYCUR,L'CAAGY-1),AL2(CAAGY-CASD1)                         
         DC    AL1(KEYTYCUR,L'CACID-1),AL2(CACID-CASD1)                         
PF11AX   EQU   *                                                                
*                                                                               
         DC    AL1(PF21AX-*,21,PFTINT,(PF21AX-PF21A)/KEYLNQ,PFTUSE)             
         DC    CL3'  ',CL8'        ',CL8'PAY '                                  
PF21A    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'CACID-1),AL2(CACID-CASD1)                         
PF21AX   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
OPTTAB   DS    0H                                                               
         DC    CL10'ACTIVE    ',AL2(VALDATE-T70248)                             
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
CASPECS  SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'CAST RECORDS'                                            
         SSPEC H2,32,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,1,C'AGENCY'                                                   
         SSPEC H4,9,C'COMM ID/CLI'                                              
         SSPEC H4,23,C'TITLE'                                                   
         SSPEC H4,43,C'CAT/PRD'                                                 
         SSPEC H4,52,C'CAM'                                                     
         SSPEC H4,57,C'AGENT'                                                   
         SSPEC H4,64,C'UNI'                                                     
         SSPEC H4,69,C'YEAR'                                                    
         SPACE 1                                                                
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,9,C'-----------'                                              
         SSPEC H5,23,C'-----'                                                   
         SSPEC H5,43,C'-------'                                                 
         SSPEC H5,52,C'---'                                                     
         SSPEC H5,57,C'-----'                                                   
         SSPEC H5,64,C'---'                                                     
         SSPEC H5,69,C'----'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
CASD1    DSECT                                                                  
CAAGY    DS    CL6                                                              
         DS    CL2                                                              
CACID    DS    CL12                                                             
         DS    CL2                                                              
CACONM   DS    CL16                                                             
         DS    CL4                                                              
CACAT    DS    CL3                                                              
         DS    CL6                                                              
CACAM    DS    CL3                                                              
         DS    CL2                                                              
CAAGENT  DS    CL4                                                              
         DS    CL3                                                              
CAUNI    DS    CL3                                                              
         DS    CL3                                                              
CAYR     DS    CL3                                                              
*                                                                               
CASD2    DSECT                                                                  
         DS    CL8                                                              
CACLT    DS    CL6                                                              
         DS    CL8                                                              
CACLNM   DS    CL16                                                             
         DS    CL4                                                              
CAPRD    DS    CL6                                                              
         DS    CL3                                                              
CAPRNM   DS    CL16                                                             
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
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR48D                                                       
         EJECT                                                                  
*                                                                               
OPTS     DS    0CL3                                                             
OPTDATE  DS    XL3                 LAST ACTIVE DATE                             
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
SVAGY    DS    CL6                 SAVED AGENCY                                 
SVCLI    DS    CL6                 SAVED CLIENT                                 
SVPRD    DS    CL6                 SAVED PRODUCT                                
SVCOM    DS    CL4                 SAVED COMMERCIAL                             
SAGY     DS    CL6                 SAVED AGENCY                                 
SCLI     DS    CL6                 SAVED CLIENT                                 
SPRD     DS    CL6                 SAVED PRODUCT                                
SCOM     DS    CL4                 SAVED COMMERCIAL                             
SVDSKADD DS    XL4                 SAVED COMMERCIAL DISK ADDRESS                
CONAME   DS    CL16                COMMERCIAL NAME                              
PRNAME   DS    CL16                PRODUCT NAME                                 
CLNAME   DS    CL16                CLIENT NAME                                  
COXNAME  DS    CL12                COM ID                                       
*                                                                               
LIMBLK   DS    XL100               AGENCY/CLIENT LIMIT WORK BLOCK               
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TAGEN48   07/30/12'                                      
         END                                                                    
