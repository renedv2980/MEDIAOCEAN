*          DATA SET TAGEN00    AT LEVEL 109 AS OF 03/20/15                      
*PHASE T70200C,*                                                                
*INCLUDE TINVCON                                                                
         SPACE 3                                                                
       ++INCLUDE TAGENPHASE                                                     
         TITLE 'T70200 - TALENT GENERAL CONTROLLER'                             
T70200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLNQ,T70200,R7,R6,RR=R2,CLEAR=YES                             
         L     RA,4(R1)            RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         LR    R8,RC               R8=A(SPOOL WORK AREAS)                       
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '         INITIALIZE SPACES                            
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         LA    RC,SPOOLEND                                                      
         USING GEND,RC             RC=A(GENCON COMMON STORAGE)                  
         ST    RB,SYSRB                                                         
         LR    R9,R1                                                            
         ST    R9,SYSPARMS         DERIVE START OF PROGRAM WORK AREAS           
         LA    R9,IO                                                            
         AH    R9,=AL2(IOLNQ)      TAKE 3 4000-BYTE I/O AREAS + LABELS          
         ST    R9,ASYSD                                                         
         USING SYSD,R9             R9=A(PROGRAM WORK AREAS)                     
         LR    R1,R9               BUMP TO END OF SYSD                          
         AH    R1,=AL2(SYSDLNQ)                                                 
         ST    R1,APTRB00          SAVE A(POINTER BLOCK)                        
         ST    R2,RELO00           SAVE 00'S RELOCATION FACTOR                  
         ST    R7,SYSR7                                                         
         ST    R6,SYSR6                                                         
         SPACE 1                                                                
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         MVC   SCONHEAD,CONHEAD    SAVE MESSAGE                                 
         XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREAS                          
         XC    CONHED2(60),CONHED2                                              
         OI    CONHED2H+6,X'80'                                                 
         SPACE 1                                                                
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,RCHANG      THEN SET RCHANG FLAG                         
         SPACE 1                                                                
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,ACHANG      THEN SET ACHANG FLAG                         
         SPACE 1                                                                
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+18                                                             
         CLC   =C'CHA',CONACT      AND IT CHANGED TO 'CHA'                      
         BNE   *+8                                                              
         OI    TRNSTAT,USERCHA     THEN SET USER CAUSED 'CHA' FLAG              
         SPACE 1                                                                
         BAS   RE,SYSINIT          INITIALIZE SYSTEM/PROGRAM DEPENDENT          
         SPACE 1                                                                
         BAS   RE,GOGENCON         GO OFF TO GENCON                             
*                                                                               
         BAS   RE,PCTASP                                                        
         BAS   RE,PROWEBRQ         PROCESS REQUESTS COMING FROM WEB             
         BE    XIT                                                              
         SPACE 1                                                                
         BAS   RE,ENTSCRN          SETUP ENTRY SCREEN                           
         SPACE 1                                                                
         OI    CONRECH+4,X'20'     SET RECORD/ACTION FLDS VALID                 
         OI    CONACTH+4,X'20'                                                  
         SPACE 1                                                                
         TM    CONHEADH+6,X'08'    IF MSG SET TO HIGH INTENSITY                 
         BZ    *+8                                                              
         NI    CONHEADH+6,X'F7'    RESTORE IT TO NORMAL INTENSITY               
         SPACE 1                                                                
         CLI   RECNUM,GX           FOR AUTOMATIC GTX REPORTS. . .               
         BNE   *+16                                                             
         CLI   TWACOMMN,1                                                       
         BNE   *+8                                                              
         NI    SGTLONGH+1,X'FF'-X'20'  . . . UNPROTECT SPECIAL FIELD            
         SPACE 1                                                                
         CLI   RECNUM,OF           AND SAME FOR TDO REPORTS                     
         BNE   MAIN30                                                           
         CLI   ACTNUM,ACTREP                                                    
         BNE   MAIN30                                                           
         CLI   TWACOMMN,1                                                       
         BNE   *+8                                                              
         NI    SGTLONGH+1,X'FF'-X'20'  . . . UNPROTECT SPECIAL FIELD            
         SPACE 1                                                                
MAIN30   BAS   RE,DISPIOS          DISPLAY I/O COUNT                            
         SPACE 1                                                                
         LA    R1,CONKEYH          RESET A(KEY FIELD) FOR CONCLUDE              
         ST    R1,EFHKEY                                                        
         GOTO1 CONCLUDE            FINISHING UP ROUTINES                        
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE PROGRAM DEPENDENT VALUES                              
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         L     R1,SYSPARMS                                                      
         OC    TWAVPRNT,TWAVPRNT   SET OFFLINE SWITCH NOW                       
         BZ    *+12                                                             
         MVI   OFFLINE,C'Y'                                                     
         B     SYS0                                                             
*                                                                               
         L     RE,0(R1)            IF ONLINE                                    
         USING TIOBD,RE                                                         
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         MVC   PFAID,TIOBAID       AND PFKEY FOR WORK BEFORE GENCON             
         NI    TIOBINDS,X'FF'-TIOBASP    TURN OFF PCPAK ALTERNATE SCR           
         DROP  RE                                                               
*                                                                               
SYS0     LM    RE,RF,12(R1)        SET SOME ADDRESSES -                         
         ST    RE,ATIA             FOR WORK DONE BEFORE GOING TO GENCON         
         ST    RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   PERVERT,CPERVERT                                                 
         MVC   GETRET,CGETRET                                                   
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         MVC   LINKIO,CLINKIO                                                   
         MVC   WSSVR,CWSSVR                                                     
         MVC   MQIO,CMQIO                                                       
         MVC   SWITCH,CSWITCH                                                   
         SPACE 1                                                                
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
SYS1     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO00                                                        
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS1                                                          
         SPACE 1                                                                
         LA    R2,CORETAB          PICK UP A(CORE-RESIDENT ROUTINES)            
         LA    R0,NCORES                                                        
         LA    R4,COREFACS                                                      
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   4(3,R1),=X'D9000A'                                               
SYS2     MVC   7(1,R1),0(R2)                                                    
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          PICK UP A(PROGRAM COMMON ROUTINES)           
         SR    R3,R3                                                            
         LA    R4,PRGCOMM                                                       
         LA    R0,NPRGCOMM                                                      
SYS3     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SYS3                                                          
         SPACE 1                                                                
         L     R2,TASYSVAL         PICK UP A(SYSTEM COMMON ROUTINES)            
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R0,NSYSCOMM                                                      
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SYS4                                                          
         SPACE 1                                                                
         LA    R4,SYSCOMM2         2ND SET OF SYSTEM COMMON ROUTINES            
         LA    R0,NSYSCOM2                                                      
SYS5     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SYS5                                                          
*                                  SET SYSTEM DEPENDENT VALUES                  
         MVI   SYSTEM,C'T'         TALENT                                       
         MVI   MAXIOS,NIOS         SET N'IO AREAS                               
         MVC   SIZEIO,=AL4(LIOS)   AND L'IO AREAS                               
         MVC   GETUSER,VALUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=Y(L'TLRCKEY)  DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=Y(L'TLRCSTAT)                                           
         MVC   DATADISP,=Y(TLRCELEM-TLRCD)                                      
         MVC   SYSDIR,=C'TALDIR  '                                              
         MVC   SYSFIL,=C'TALFIL  '                                              
         MVC   REQFILE,=C'TALREQ '                                              
         MVI   ACTELOPT,C'N'       DON'T ADD ACTIVITY ELEMS. TO RECORDS         
         SPACE 1                                                                
* SET APPL CALLOV/ADD DEL REC/APPL CTRL RDUP/APPL SET EFH/MERGE ON FLD#         
         OI    GENSTAT1,APPLIC+OKADDEL+RDUPAPPL+NOSETEFH+EFHKYMRG               
* RE-DISPLAY THIS LIST PG AFTER SELECT                                          
         OI    GENSTAT2,DISTHSPG                                                
* VAL SELECT CODES / SEL-REST X'E00' TWA0 / HAVE MULTIPLE FILES                 
         OI    GENSTAT3,OKVALSEL+RESTXE00+MULTFILS                              
* CONFIRMATION OF DELETE REQUIRED                                               
         OI    GENSTAT4,CONFDEL                                                 
* DISALLOW 'D' FROM LIST(USE DE/DEL)                                            
         OI    GENSTAT5,NODLST                                                  
*                                                                               
         MVI   ALTPROG,X'F2'       SET ALT. PROGRAM FOR SCREENS                 
         MVI   GETMSYS,70          USES GETMSG FOR SYSTEM 70                    
         MVC   LWORK,=AL4(WORKLNQ) WE TOOK XXXXX BYTES IN NMOD                  
         MVC   RCPROG(2),=C'TA'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9070200' PRESET FOR PROGRAM CALLOVS                 
*                                                                               
         MVC   TGTHREE,CONACT                                                   
         OC    TGTHREE,SPACES                                                   
         CLC   =C'P  ',TGTHREE                                                  
         BE    SYS5A                                                            
         CLC   =C'PA',TGTHREE      IF CURRENT ACTION IS PAY                     
         BNE   SYS7                                                             
SYS5A    CLI   TWALACT,ACTPAY      AND PREVIOUS ACTION WAS NOT PAY              
         BE    SYS8                                                             
         CLI   TWASCR,SCR90        AND CURRENT SCREEN IS NOT A PAY              
         BL    SYS6                MENU                                         
         CLI   TWASCR,SCR99        OR CAST SELECT                               
         BNH   SYS8                                                             
SYS6     MVI   TWASCR,0            CLEAR LOADED SCREEN                          
         B     SYS8                                                             
                                                                                
SYS7     CLI   TWALACT,ACTPAY      IF CURRENT ACTION IS NOT PAY                 
         BNE   SYS8                AND PREVIOUS ACTION WAS PAY                  
         MVI   TWASCR,0            CLEAR LOADED SCREEN                          
                                                                                
SYS8     MVC   TGTHREE,CONACT                                                   
         OC    TGTHREE,SPACES                                                   
         CLC   =C'P  ',TGTHREE                                                  
         BE    SYS8A                                                            
         CLC   =C'PA',TGTHREE      IF CURRENT ACTION IS PAY                     
         BNE   SYS10                                                            
SYS8A    CLI   PFAID,0             AND USER HAS HIT ENTER                       
         BE    SYS9                                                             
         CLI   PFAID,13            OR PF13 OR HIGHER                            
         BL    SYS10                                                            
SYS9     MVI   ALTPROG,X'F3'       SET ALT. PROGRAM FOR PAY SCREENS             
*                                                                               
SYS10    CLI   TGTALNUM,0                                                       
         BNE   SYS20                                                            
         L     RF,SWITCH           SEE IF CALLED OFFLINE, SWITCH=0              
         LTR   RF,RF                                                            
         BNZ   SYS13                                                            
                                                                                
         L     R1,TWAMASTC         ADDRESS OF MASTER                            
         USING MASTD,R1                                                         
         L     R1,MCUTL            A(UTL)                                       
         B     SYS15                                                            
                                                                                
SYS13    GOTO1 SWITCH,DMCB,(X'FF',X'FFFFFFFF'),0                                
         L     R1,0(R1)                                                         
SYS15    XR    RF,RF                                                            
         IC    RF,TSYS-UTLD(R1)                                                 
         SRA   RF,4                                                             
         STC   RF,TGTALNUM         SAVE TALENT SYSTEM NUMBER (TAL?)             
                                                                                
SYS20    L     R2,TASYSTBL         A(SYSTEM TABLES)                             
         LH    R1,=Y(TGAGRACT-TGTABLES)                                         
                                                                                
         AR    R1,R2                                                            
         A     R2,0(R1)            A(RECACT TABLE)                              
         USING RACTD,R2                                                         
         LA    R1,RACTTBL                                                       
         ST    R1,ARECACT                                                       
         AH    R1,RACTCMSK         A(CAST MASK BITS)                            
         MVC   CASTCMSK,0(R1)                                                   
         MVC   LRECACT,RACTLTBL    LENGTH OF TABLE ENTRY                        
*                                                                               
         LA    R1,STARTSV          SET A(SAVED STORAGE)                         
         ST    R1,ASTARTSV                                                      
         SPACE 1                                                                
         BAS   RE,SETEFH           SET EHF TAGS                                 
         MVC   LSVTWA0,=AL2(TWA018K) L'STORAGE TO SAVE IN TWA0                  
         MVI   NTWA,0                DON'T SAVE ANY EXTRA PAGES                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROCESSES REQUEST COMING FROM THE WEB                *         
***********************************************************************         
                                                                                
PROWEBRQ NTR1                                                                   
         OC    LINKIO,LINKIO       DO NOT EXECUTE ROUTINE IF COMING             
         JZ    NO                  FROM EASYEST                                 
                                                                                
         USING LIOB,R3                                                          
         L     R3,AIO1             R3=A(LINKIO CONTROL BLOCK)                   
                                                                                
         LR    R0,R3               CLEAR CONTROL BLOCK                          
         LHI   R1,LIOBX-LIOB                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
******** L     R0,TGAS2ACC                                                      
******** AHI   R0,STF2LNQ                                                       
         L     R0,APTRB00          REQUEST'S FIRST 14K BYTES                    
         ST    R0,LIOBABUF         SET ADDRESS OF AREA TO STORE                 
                                                                                
         AHI   R0,14*1024          SET ADDRESS OF AREA TO STORE                 
         ST    R0,LIOBAREC         REQUEST'S FINAL CHUNK                        
                                                                                
         MVC   LIOBACOM,ACOMFACS   SET ADDRESS OF COMFACS                       
                                                                                
         GOTO1 LINKIO,DMCB,('LIOAINI',LIOB)                                     
         JNE   NO                  IF LINKIO REQUEST IS FOUND ...               
         OI    TGFASTAT,TGFROMFA   SET REQUEST MADE BY FALINK                   
         MVI   OVERLAY,X'FA'       AND CALL TAGENFA FOR FURTHER                 
         GOTO1 LOADSOPH,DMCB,0     PROCESSING                                   
         GOTO1 (R3),DMCB,(RC)                                                   
         J     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE HANDLES PASSING OF CONTROL TO AND FROM GENCON            
         SPACE 1                                                                
GOGENCON NTR1                                                                   
         BAS   RE,SETRD            SET RD SO GENCON ALWAYS RETURNS              
         SPACE 1                                                                
GOG2     MVI   GOAGAIN,C'N'        INITIALIZE RETURN SWITCH                     
         OI    TRNSTAT,FRSTMODE    ALLOWS APPL TO DETECT FIRST MODE             
         MVI   GLSTSTAT,0          INSURE LIST STATUS INITIALIZED               
         SPACE 1                                                                
GOG4     GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON - PASS A(W/S)                  
         SPACE 1                                                                
         TM    TRNSTAT,TRNABEND    TEST PROGRAM WANTS TO ABEND                  
         BZ    GOG5                                                             
         DC    H'0',C'$ABEND'      ABEND, BUT RETURN CONTROL TO USER            
         SPACE 1                                                                
GOG5     TM    TRNSTAT,PYINPROG    IF PAY UPDATE IN PROGRESS                    
         BZ    *+6                                                              
         DC    H'0'                THEN DIE - PAY ALWAYS TURNS OFF THIS         
*                                  BIT WHEN IT FINISHES WRITING TO FILE         
         SPACE 1                                                                
         TM    TRNSTAT2,RESTSARD   IF TSAR BUFFER WAS RESTORED                  
         BZ    GOG5B                                                            
         LA    R2,TSARBLK          R2=A(PARAM BLOCK=TSARBLK)                    
         USING TSARD,R2                                                         
         MVI   TSACTN,TSASAV       SAVE BUFFER TO DISK BEFORE LEAVING           
         BRAS  RE,CALLTSAR                                                      
         SPACE 1                                                                
GOG5B    CLI   GOAGAIN,C'Y'        REQUEST BY APPLIC. TO GO BACK                
         BE    GOG2                                                             
         ICM   R1,15,AFRSTKEY      IF CURSOR NOT AT FIRST KEY FIELD             
         BZ    GOGX                                                             
         TM    6(R1),X'40'                                                      
         BO    GOG5X                                                            
         CLI   TWASCR,SCR40        AND ESTIMATE REPORT SCREEN                   
         BNE   GOGX                                                             
         LA    R1,ERPAGYH          CHECK CURSOR AT 2ND(REAL) KEY FIELD          
         TM    6(R1),X'40'                                                      
         BZ    GOGX                                                             
         SPACE 1                                                                
GOG5X    CLI   OKNO,2              AND GENCON IS ASKING FOR MORE INPUT          
         BNE   GOG8                                                             
         CLI   GOAGAIN,C'K'        AND WE DIDN'T TRY THIS ALREADY               
         BE    GOG8                                                             
         CLI   ACTNUM,ACTDEL       AND IF ACTION IS NOT DELETE                  
         BE    GOGX                                                             
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         BE    GOGX                                                             
GOG6     MVI   CONKEY,C','         MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   CONKEYH+5,1         APPLICATION GETS A CHANCE TO FILL            
         MVI   GOAGAIN,C'K'        IN KEY FIELDS                                
         B     GOG4                GO BACK                                      
         SPACE 1                                                                
GOG8     CLI   5(R1),0             IF NOTHING IS IN FIRST KEY FIELD             
         BNE   *+12                                                             
         CLI   ERROR,MISSING       AND MISSING INPUT FIELD ERROR                
         BE    PLSENTER            SWITCH TO PLEASE ENTER FIELDS ...            
         SPACE 1                                                                
GOGX     CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BNE   GOGXX                                                            
         TM    GLSTSTAT,NOSELFLD   AND SELECT FIELD EXISTS                      
         BO    GOGXX                                                            
         CLI   OKNO,16             IF GENCON MSG IS "END OF LIST - HIT          
         BE    SELFIRST            ENTER...", CHANGE TO SELECT OR HIT..         
         CLI   OKNO,15             IF MSG IS "LIST DISPLAYED - HIT              
         BE    SELNEXT             ENTER...", CHANGE TO SELECT OR HIT..         
GOGXX    B     XIT                 ALL THROUGH                                  
         SPACE 3                                                                
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO WE GET CONTROL BACK                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY I/O COUNTS                                    
         SPACE 1                                                                
DISPIOS  NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPP   ONLY FOR PROGRAMMERS                         
         BNE   DIOX                                                             
         GOTO1 GETFACT,DMCB,0      GET COUNT FROM GETFACT                       
         L     R3,DMCB                                                          
         USING FACTSD,R3                                                        
         LA    R2,CONHED2          FIND FIRST AVAILABLE SLOT                    
         LA    R0,L'CONHED2-10                                                  
         XR    R4,R4                                                            
         SPACE 1                                                                
DIO2     CLC   0(5,R2),=C'(I/O='   OR PREV. LITERAL                             
         BNE   *+10                                                             
         MVC   0(11,R2),SPACES                                                  
         SPACE 1                                                                
         CLI   0(R2),C' '          SCAN FOR FIRST OPEN SPOT                     
         BH    *+12                                                             
         LTR   R4,R4               IF NOT SAVED YET                             
         BNZ   *+6                                                              
         LR    R4,R2               SAVE IT NOW                                  
         SPACE 1                                                                
         LA    R2,1(R2)            TRY NEXT                                     
         BCT   R0,DIO2                                                          
         SPACE 1                                                                
         MVC   0(5,R4),=C'(I/O='              START LITERAL                     
         EDIT  FATIOCNT,(5,5(R4)),ALIGN=LEFT  DISPLAY I/O COUNT                 
         AR    R4,R0                                                            
         MVI   5(R4),C')'                     END LITERAL                       
         SPACE 1                                                                
DIOX     B     XIT                                                              
         EJECT                                                                  
*              SET A(CONTROLLER FIELDS) FOR GENCON                              
         SPACE 1                                                                
SETEFH   NTR1                                                                   
         LA    R1,CONRECH          SET EFH TAGS                                 
         ST    R1,EFHREC                                                        
         LA    R1,CONACTH                                                       
         ST    R1,EFHACT                                                        
         LA    R1,CONKEYH                                                       
         ST    R1,EFHKEY                                                        
         LA    R1,CONWHENH                                                      
         ST    R1,EFHWHEN                                                       
         LA    R1,CONTAGH                                                       
         ST    R1,EFHTAG                                                        
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    EFH30                                                            
         TM    TRNSTAT,RACHANG     AND REC AND ACT HAVE NOT CHANGED             
         BNZ   EFH20                                                            
         CLI   TWASCR,X'FF'        AND REQUEST DISPLAY ACTIVE                   
         BE    EFH10                                                            
         CLI   TWALACT,ACTREP      OR ACTION IS REPORT                          
         BE    EFH10                                                            
         CLI   TWALACT,ACTLIST     OR ACTION IS LIST                            
         BE    EFH10                                                            
         CLI   TWASCR,SCR07        OR PRESENT SCREEN IS MREISSUE SCREEN         
         BE    EFH10                                                            
         CLI   TWASCR,SCRAD        OR GTRACK REBUILD                            
         BE    EFH10                                                            
         CLI   TWASCR,SCR2F        OR CAGENT NEW                                
         BE    EFH10                                                            
         CLI   TWASCR,SCRDE        OR OFFICE REPORT                             
         BE    EFH10                                                            
         CLI   TWASCR,SCR8E        OR CLIENT COPY                               
         BNE   EFHX                                                             
         SPACE 1                                                                
EFH10    CLI   CONKEYH+5,3         & INPUT IN KEY FIELD IS 3 CHARS              
         BNE   EFH20                                                            
         CLC   =C'REQ',CONKEY      & REQUESTING REQUESTS                        
         BNE   EFH20                                                            
         LA    R1,CONKEYH          SET A(OTHERS FIELD) TO A(KEY FIELD)          
         ST    R1,EFHOTH                                                        
         LA    R1,TMPKEYH                                                       
         ST    R1,EFHKEY           & SET A(KEY) TO TEMP FIELD                   
         SPACE 1                                                                
EFH20    TM    TRNSTAT,RCHANG      IF RECORD TYPE DIDN'T CHANGE                 
         BO    EFHX                                                             
         CLI   TWASCR,SCRC1        AND SCREEN IS FOR ETV/ERADIO                 
         BNE   EFH25                                                            
         ZIC   R1,CONRECH+5                                                     
         BCTR  R1,0                                                             
         B     EFH40               MAY NEED TO SET A(DESTINATION FIELD)         
         SPACE 1                                                                
EFH25    TM    TRNSTAT,RACHANG     IF RECORD/ACTION DIDN'T CHANGE               
         BNZ   EFHX                                                             
         CLI   TWASCR,SCR40        AND SCREEN IS ESTIMATE REPORT                
         BNE   EFHX                                                             
         SPACE 1                                                                
EFH30    ZIC   R1,CONRECH+5                                                     
         BCTR  R1,0                                                             
         LA    RF,=C'ESTIMATE'     IF RECORD IS ESTIMATE                        
         EX    R1,EXCLCREC                                                      
         BNE   EFH38                                                            
         ZIC   R1,CONACTH+5                                                     
         BCTR  R1,0                                                             
         LA    RF,=CL8'REPORT'     AND ACTION IS REPORT                         
         EX    R1,EXCLCACT                                                      
         BNE   EFHX                                                             
EFH35    LA    R1,ERPDESTH         SET A(DESTINATION FIELD)                     
         ST    R1,EFHDEST                                                       
         B     EFHX                                                             
         SPACE 1                                                                
EFH38    CLI   OFFLINE,C'Y'        IF ONLINE, RECORD MUST MATCH SCREEN          
         BNE   EFHX                                                             
         SPACE 1                                                                
EFH40    LA    RF,=CL8'ETV'        IF RECORD IS ETV                             
         EX    R1,EXCLCREC                                                      
         BE    EFH45                                                            
         LA    RF,=CL8'ERADIO'     OR ERADIO                                    
         EX    R1,EXCLCREC                                                      
         BNE   EFHX                                                             
EFH45    ZIC   R1,CONACTH+5                                                     
         BCTR  R1,0                                                             
         LA    RF,=CL8'REPORT'     AND ACTION IS REPORT                         
         EX    R1,EXCLCACT                                                      
         BNE   EFHX                                                             
         LA    R1,SSSDESTH         SET A(DESTINATION FIELD)                     
         ST    R1,EFHDEST                                                       
         SPACE 1                                                                
EFHX     B     XIT                                                              
         SPACE 3                                                                
EXCLCREC CLC   CONREC(0),0(RF)     COMPARE RECORD FIELD TO 0(RF)                
         SPACE 3                                                                
EXCLCACT CLC   CONACT(0),0(RF)     COMPARE ACTION FIELD TO 0(RF)                
         EJECT                                                                  
*              PROGRAM COMMON ROUTINES                                          
         SPACE 2                                                                
         DS    0D                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         L     R8,ASPOOLD                                                       
         L     R7,SYSR7                                                         
         L     R6,SYSR6                                                         
         SPACE 1                                                                
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VVALUSER            X'00'                                        
         B     VINITIAL            X'04'                                        
         B     VEXIT               X'08'                                        
         B     VADDPTRS            X'0C'                                        
         B     VSAVPTRS            X'10'                                        
         B     VGETTWA             X'14'                                        
         B     VTOTCNTL            X'18'                                        
         B     VREADACC            X'1C'                                        
         B     VSETLSTK            X'20'                                        
         B     VFLDVAL             X'24'                                        
         B     VLOADPAY            X'28'                                        
         B     VTSARCTL            X'2C'                                        
         B     VREVERSE            X'30'                                        
         B     VPGCNTL             X'34'                                        
         B     VCHKCLG             X'38'                                        
         B     VGQEXT              X'3C'                                        
         B     VNTFYVIT            X'40'                                        
         B     VELIMCHR            X'44'                                        
         B     VRAPPLSA            X'48'                                        
         SPACE 1                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              FIRST ROUTINE CALLED BY GENCON EACH TIME IN                      
         SPACE 1                                                                
VVALUSER DS    0H                                                               
         L     RE,TASYSTBL         PICK UP A(GLOBAL TABLES)                     
         LA    RF,TGTABLES                                                      
         LA    R0,NTABLS                                                        
         XC    DMCB,DMCB                                                        
VUSR1    L     R1,TASYSTBL         RELOCATE TABLE ENTRIES                       
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,VUSR1                                                         
                                                                                
         BAS   RE,PRGINIT          INITIALIZE PROGRAM                           
                                                                                
         L     RE,APTRB00                                                       
         AHI   RE,PTRB00L                                                       
         ST    RE,TGAS2ACC         SAVE A(STAFF LIMIT BLOCK)                    
         SPACE 1                                                                
         MVC   TGSYSTAB,TASYSTBL                                                
         GOTO1 SYSVINIT,DMCB,CONSTAFH,CONPASSH  SYSTEM INITIALIZATION           
         BE    VUSR1X                                                           
         TM    PRGSTAT,TESTSYS     DON'T CARE ABOUT PASSWORDS IN TST            
         BO    VUSR1X                                                           
         B     VUSR1X              *** NO-OP UNTIL TP IS READY ***              
*                                                                               
         OI    PRGSTAT,STAYPCHG      PASSWORD EXPIRED                           
         MVC   CONREC,=CL8'PASSWORD' SET RECORD TYPE FOR USER                   
         MVI   CONRECH+5,8                                                      
         OI    CONRECH+6,X'80'                                                  
         MVC   CONACT,=CL8'CHANGE'   AND ACTION                                 
         MVI   CONACTH+5,8                                                      
         OI    CONACTH+6,X'80'                                                  
         MVC   CONKEY(8),TGDUB       AND OLD PASSWORD IN KEY                    
         MVI   CONKEYH+5,8                                                      
         SPACE 1                                                                
VUSR1X   CLI   OFFLINE,C'Y'        ********* IF NOT OFFLINE ***********         
         BE    *+16                ****                            ****         
         OC    TGCTSTAF,TGCTSTAF   ****** TEMP CODE TO TRAP BUG *******         
         BNZ   *+6                 ***                              ***         
         DC    H'0'                *** GLOBAL STORAGE IS VANISHING! ***         
         SPACE 1                                                                
         CLI   TWAFIRST,0          IF FIRST TIME IN                             
         BNE   VUSR2                                                            
         CLI   OFFLINE,C'Y'        AND IF OFFLINE THEN GENCON WILL SET          
         BE    *+8                                                              
         MVI   TWAFIRST,1          ELSE SET NO LONGER FIRST TIME                
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPO   IF THIS IS AN OPERATOR                       
         BNE   VUSR2                                                            
         TM    PRGSTAT,STAYPCHG    IF FORCING TO PASSWORD/CHANGE                
         BO    VUSR2               MUST CHANGE PASSWORD FIRST                   
         MVC   CONREC,=CL8'SCHECK'   SET RECORD TYPE FOR HIM                    
         MVI   CONRECH+5,8                                                      
         OI    CONRECH+6,X'80'                                                  
         MVC   CONACT,=CL8'DISPLAY'  AND ACTION                                 
         MVI   CONACTH+5,8                                                      
         OI    CONACTH+6,X'80'                                                  
         SPACE 1                                                                
VUSR2    TM    CONRECH+1,X'20'     IF REC/USE FLD WAS PROTECTED BY PAY          
         BZ    VUSR3                                                            
         NI    CONRECH+1,X'DF'     UNPROTECT REC/USE AND ACTION                 
         OI    CONRECH+6,X'80'                                                  
         NI    CONACTH+1,X'DF'                                                  
         OI    CONACTH+6,X'80'                                                  
         OI    TRNSTAT2,UNPROTRA   SET STATUS THAT WE DID THIS                  
         SPACE 1                                                                
VUSR3    MVC   USERNAME(66),SVUSER GET USER ID INFO FROM SAVED STORAGE          
         SPACE 1                                                                
         MVC   RECNUM,TWALREC     MOVE SAVED REC/ACT TO CURRENT IN CASE         
         MVC   ACTNUM,TWALACT     WE TAKE ERROR BEFORE GENCON SETS THEM         
*                                                                               
         BRAS  RE,LKUPRACT         LOOKUP REC/ACT FOR TALx <> TAL1              
*                                                                               
         SPACE 1                                                                
*&&DO                                                                           
         CLI   TWASCR,SCR18        TEST CURRENT SCREEN IS COMM PAGE 2           
         BNE   VUSR3A                                                           
         CLC   =C'AFM',SC2AFM#                                                  
         BNE   VUSR3A                                                           
         CLI   CONKEYH+5,0         IF THERE IS SOMETHING IN KEY FIELD,          
         BE    VUSR3A                                                           
         CLI   CONKEYH+5,1                                                      
         BNE   *+12                                                             
         CLI   CONKEY,C','         EXCEPT IF JUST A COMMA                       
         BE    *+8                                                              
         OI    TRNSTAT,RCHANG      PRETEND RECORD FIELD HAS CHANGED             
*&&                                                                             
         SPACE 1                                                                
VUSR3A   CLI   PFAID,2             IF 'CHANGE' PFKEY PRESSED                    
         BE    VUSR4                                                            
         CLI   CONACT,C'L'         OR ACTION IS NOT LIST                        
         BE    VUSR5                                                            
         CLI   CONACT,C'R'         OR REPORT                                    
         BE    VUSR5                                                            
         CLC   CONACT(2),=C'CO'    OR COPY                                      
         BE    VUSR5                                                            
         CLC   CONACT(2),=C'MO'    OR MOVE                                      
         BE    VUSR5                                                            
         CLC   CONACT(2),=C'MR'    OR MULTIPLE REISSUE                          
         BE    VUSR5                                                            
         CLC   CONREC(6),=C'CAGENT' IF CAGENT/NEW                               
         BNE   *+14                                                             
         CLC   CONACT(2),=C'NE'                                                 
         BE    VUSR5                                                            
         CLC   CONREC(3),=C'ETV'   IF ETV/DOWN OR ERADIO/DOWN                   
         BE    *+14                                                             
         CLC   CONREC(6),=C'ERADIO'                                             
         BNE   *+14                                                             
         CLC   CONACT(2),=C'DO'    ACTION DOWNLOAD                              
         BE    VUSR5                                                            
         CLI   CONWHENH+5,0        AND SOMETHING'S IN PRINT FIELD               
         BE    VUSR5                                                            
VUSR4    XC    CONWHEN,CONWHEN     THEN CLEAR IT                                
         MVI   CONWHENH+5,0                                                     
         OI    CONWHENH+6,X'80'                                                 
         SPACE 1                                                                
VUSR5    TM    TRNSTAT,RACHANG     IF RECORD OR ACTION FLD HAS CHANGED          
         BZ    VUSR6                                                            
         BAS   RE,CLRSTACK         THEN CLEAR CALLPROG STACK                    
         SPACE 1                                                                
         TM    TRNSTAT,ACHANG      IF NOT ACTION (ONLY RECORD) CHANGED          
         BO    VUSR6                                                            
*&&DO                                                                           
         CLI   TWASCR,SCR18        IF CURRENT SCREEN IS COMM PAGE 2             
         BNE   VUSR6                                                            
         CLC   =C'AFM',SC2AFM#                                                  
         BNE   VUSR6                                                            
         MVI   TWASCR,0            INSURE GENCON LOADS NEW SCREEN               
*&&                                                                             
VUSR6    CLI   TWALACT,ACTVER      IF LAST ACTION WAS VERIFY TWASCR MAY         
         BNE   VUSR8                  NOT MATCH SCREEN ACTUALLY LOADED          
                                                                                
         CLI   ALTPROG,X'F3'                                                    
         BE    VUSR8                                                            
         CLI   TWASCR,SCR0A        TEST CURRENT SCREEN IS CAST/LIST             
         BE    *+8                                                              
         CLI   TWASCR,SCR6A        TEST CURRENT SCREEN IS EMPLOYEE/LIST         
         BNE   VUSR8                                                            
         CLI   PFAID,10            IF USER HIT PFKEY FOR CAST/LIST              
         BE    *+12                                                             
         TM    TRNSTAT,RACHANG     OR RECORD/ACTION FIELD HAS CHANGED           
         BZ    *+8                    (MAY HAVE CHANGED TO CAST/LIST)           
         MVI   TWASCR,0            INSURE GENCON LOADS NEW SCREEN               
                                                                                
VUSR8    TM    PRGSTAT,STAYPCHG    IF FORCING TO PASSWORD/CHANGE                
         BO    VUSR9               SKIP PFVAL(MUST CHANGE PASSWORD)             
         GOTO1 PFVAL,DMCB,(12,PFTAB) HANDLE GLOBAL PFKEY PRESENCE               
         SPACE 1                                                                
VUSR9    TM    TRNSTAT,RACHANG+USERCHA  IF USER HASN'T CHANGED REC/ACT          
         BNZ   VUSR10                                                           
         OC    SVACTION,SVACTION   AND SAVED ACTION EXISTS                      
         BZ    VUSR10                                                           
         MVC   CONACT,SVACTION     THEN RESTORE IT                              
         SPACE 1                                                                
VUSR10   CLI   TWASCR,SCR90        IF CURRENT SCREEN IS A PAY MENU              
         BL    VUSR12                                                           
         CLI   TWASCR,SCR99        OR CAST SELECT                               
         BH    VUSR12                                                           
         CLC   CONACT(3),=C'PAY'   IF NOT PAY, CONTINUE                         
         BNE   VUSR12                                                           
         GOTO1 LOADPAY             LOAD PAY COMMON ROUTINES                     
         MVI   ACTNUM,ACTPAY       SET ACTION=PAY                               
         SPACE 1                                                                
         GOTO1 APAYINIT,DMCB,(X'80',0) AND GO TO PAY INITIALIZATION             
         SPACE 1                                                                
VUSR12   DS    0H                                                               
         SPACE 1                                                                
VUSRX    B     XIT                                                              
         EJECT                                                                  
*              PROGRAM INITIALIZATION                                           
         SPACE 1                                                                
PRGINIT  NTR1                                                                   
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         L     R3,DMCB                                                          
         USING FACTSD,R3                                                        
*&&DO                                                                           
         TM    FATSTAT6,TST6FLSH   IF CONNECTED VIA STEREO                      
         BZ    *+8                                                              
         OI    TRNSTAT2,CTSTEREO   SET STEREO ACTIVE                            
*&&                                                                             
         TM    FATSTAT6,TST6STFU   IF CONNECTED VIA FULL STEREO MODE            
         BZ    *+8                                                              
         OI    TRNSTAT2,CTFLSTER   SET FULL MODE STEREO ACTIVE                  
*                                                                               
         CLI   OFFLINE,C'Y'        ALWAYS PROCESS REST IF OFFLINE               
         BE    *+12                                                             
         CLI   TWAFIRST,0          ELSE IF FIRST TIME IN                        
         BNE   PRGINITX                                                         
         SPACE 1                                                                
         CLI   FASYSID,1           CHECK FOR TEST SYSTEM                        
         BNE   *+8                                                              
         OI    PRGSTAT,TESTSYS                                                  
         CLI   FASYSID,11          CHECK FOR CSC SYSTEM                         
         BNE   *+8                                                              
         OI    PRGSTAT,CSCSYS                                                   
         CLI   FASYSID,15          CHECK FOR FQA SYSTEM                         
         BNE   *+8                                                              
         OI    PRGSTAT,FQASYS                                                   
         SPACE 1                                                                
         MVC   TGLINEID(8),FALINE  SAVE LINE ID                                 
         SPACE 1                                                                
         XC    KEY,KEY             GET DDS PID FROM PASSWORD RECORD             
         LA    R2,KEY                                                           
         USING SA0REC,R2                                                        
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,FATAGYSC    SAVE AGENCY CODE FOR SECURITY                
         MVC   SA0KNUM,FAPASSWD    AND PASSWORD ID NUMBER                       
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         GOTO1 HIGH                                                             
         MVI   USEIO,C'N'                                                       
         CLC   KEY(L'SA0KEY),KEYSAVE                                            
         BNE   PINIT30                                                          
         SPACE 1                                                                
         L     R2,AIO                                                           
         LA    R2,SA0DATA                                                       
         MVI   ELCODE,SAPALELQ                                                  
         USING SAPALD,R2                                                        
PINIT10  CLI   0(R2),0                                                          
         BE    PINIT30                                                          
         CLI   0(R2),SAPALELQ                                                   
         BE    PINIT20                                                          
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     PINIT10                                                          
PINIT20  MVC   TGCTSTAF,SAPALPID   SAVE PID AS CONNECT STAFF ID                 
         SPACE 1                                                                
PINIT30  XC    KEY,KEY             GET AGENCY NAME & ADDR FROM ID REC.          
         LA    R2,KEY                                                           
         USING CTIKEY,R2                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                                                             
         MVI   USEIO,C'N'                                                       
         XC    FILENAME,FILENAME                                                
         SPACE 1                                                                
         L     R2,AIO                                                           
         LA    R2,CTIDATA                                                       
         MVI   ELCODE,X'30'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   *+10                                                             
         USING CTDSTD,R2                                                        
         MVC   SVUSER,CTDSTNAM     SAVE AGENCY NAME AND ADDRESS                 
         SPACE 1                                                                
PRGINITX B     XIT                                                              
         EJECT                                                                  
*              SETUP SYSTEM'S ENTRY SCREEN                                      
         SPACE 1                                                                
ENTSCRN  NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         TM    TRNSTAT2,CTSTEREO+CTFLSTER                                       
         BNZ   XIT                                                              
*                                                                               
         CLI   TWASCR,00           IF ON MENU SCREEN                            
         BNE   XIT                                                              
         CLI   TWAAUTH+1,X'0F'     AND CONNECTING VIA NEW SECURITY              
         BE    ESCR10                                                           
         CLI   TWAAUTH+1,X'FF'     HIDE STAFF AND PASSWORD FIELDS               
         BE    ESCR10                                                           
         GOTO1 FLDVAL,DMCB,(X'0A',CONTAGH),(X'08',CONPASSH)                     
*                                                                               
ESCR10   GOTO1 FLDVAL,DMCB,(X'80',CONRECH),(X'80',CONSTAFH)                     
         BNE   XIT                                                              
         CLI   PFAID,0             IF RECORD/ACTION OR PFKEY                    
         BNE   XIT                 HAVE NOT BEEN ENTERED                        
         CLI   ERROR,ERINVPSW                                                   
         BE    XIT                                                              
         NI    CONSTAFH+6,X'BF'    GIVE MESSAGE                                 
         MVC   MYMSGNO,=H'271'                                                  
         MVI   MYMTYP,GTMINF                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
         EJECT                                                                  
*              OVERLAY INITIALIZATION                                           
         SPACE 1                                                                
*                                  P1=A(PFKEY VAL. TABLE) OR ZEROS              
*                                  P1, BYTE 0 X'80'=FORCE CLEAR W/S             
*                                             X'40'=FORCE NO CLEAR W/S          
VINITIAL DS    0H                                                               
         MVC   TGBYTE,0(R1)        SAVE STATUS BYTE                             
         ZICM  R3,1(R1),3                                                       
                                                                                
         CLI   TGTALNUM,0                                                       
         BNE   INIT030                                                          
         L     RF,SWITCH           SEE IF CALLED OFFLINE, SWITCH=0              
         LTR   RF,RF                                                            
         BNZ   INIT020                                                          
                                                                                
         L     R1,TWAMASTC         ADDRESS OF MASTER                            
         USING MASTD,R1                                                         
         L     R1,MCUTL            A(UTL)                                       
         B     INIT025                                                          
                                                                                
INIT020  GOTO1 SWITCH,DMCB,(X'FF',X'FFFFFFFF'),0                                
         L     R1,0(R1)                                                         
INIT025  XR    RF,RF                                                            
         IC    RF,TSYS-UTLD(R1)                                                 
         SRA   RF,4                                                             
         STC   RF,TGTALNUM         SAVE TALENT SYSTEM NUMBER (TAL?)             
                                                                                
         SPACE 1                                                                
INIT030  LTR   R3,R3               IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT040                                                          
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
         GOTO1 PFVAL,DMCB,(24,(R3)) HANDLE LOCAL PFKEY PRESENCE                 
         BE    DUMMYERR            TAKE DUMMY ERROR EXIT FOR GOAGAIN            
         SPACE 1                                                                
INIT040  MVI   SCRSTAT,0           CLEAR SCREEN STATUS BYTE                     
         SPACE 1                                                                
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
         SPACE 1                                                                
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
         SPACE 1                                                                
         MVC   BYTE,ACTNUM         MOVE CURRENT ACTION TO TEMP. W/S             
         CLI   BYTE,ACTCHA         IF CURRENT ACTION IS CHANGE                  
         BNE   *+16                                                             
         CLI   SVACT,ACTSEL        AND SAVED ACTION WAS SELECT                  
         BNE   *+8                                                              
         MVI   BYTE,ACTSEL         PRETEND CURRENT ACTION IS SELECT             
         SPACE 1                                                                
         CLC   BYTE,SVACT          TEST ACTION CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,ACTCHG                                                   
         SPACE 1                                                                
         TM    TGBYTE,X'80'        FORCE TO CLEAR APPLIC. STORAGE               
         BNZ   INIT050                                                          
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    INIT050                                                          
         TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
         BZ    INIT100                                                          
         CLI   BYTE,ACTREP         ALWAYS CLEAR IF ACTION IS NOW REPORT         
         BE    INIT050                                                          
         CLI   TWASCR,SCRA0        ALWAYS CLEAR IF THIS SCREEN INV/REL          
         BE    INIT050                                                          
         CLI   SVACT,ACTSEL        IF LAST ACTION NOT SELECT                    
         BE    INIT100                                                          
         CLI   BYTE,ACTSEL         AND THIS ACTION NOT SELECT                   
         BE    INIT100                                                          
         CLI   TWASCR,SCR80        CLEAR IF THIS SCREEN CAST/COPY               
         BE    INIT050                                                          
         CLI   SVSCR,SCR80         OR LAST SCREEN NOT CAST/COPY                 
         BE    INIT050                                                          
         CLI   TWASCR,SCR1A        DON'T IF THIS SCREEN CAST                    
         BE    INIT100                                                          
         CLI   SVSCR,SCR1A         OR LAST SCREEN CAST                          
         BE    INIT100                                                          
         CLI   TWASCR,SCRA3        DON'T IF THIS SCREEN RCAST                   
         BE    INIT100                                                          
         CLI   SVSCR,SCRA3         OR LAST SCREEN RCAST                         
         BE    INIT100                                                          
         CLI   TWASCR,SCRCA        DON'T IF THIS SCREEN SOAP CAST               
         BE    INIT100                                                          
         CLI   SVSCR,SCRCA         OR LAST SCREEN SOAP CAST                     
         BE    INIT100                                                          
         SPACE                                                                  
INIT050  TM    TGBYTE,X'40'        FORCE TO NOT CLEAR STORAGE                   
         BO    INIT100                                                          
         LA    RE,TWAHOLE          CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'TWAHOLE)                                               
         XCEFL                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         XR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    *+10                                                             
         AR    RE,RF                                                            
         B     *-10                                                             
         LA    RE,3(RE)            BUMP PAST CONTROL BYTES                      
         LR    RF,RE                                                            
         SR    RF,RA                                                            
         SH    RF,=AL2(3520+64)    L'AVAIL TWA0 AS DEFINED IN DDGENTWA          
         LCR   RF,RF                                                            
         XCEFL ,                   CLEAR AREA AFTER SCREEN END                  
         SPACE 1                                                                
INIT100  MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM                     RECORD                          
         MVC   SVACT,BYTE                       ACTION                          
         SPACE 1                                                                
         CLI   MODE,DISPKEY        TEST MODE DISPLAY KEY (SELECT)               
         BNE   INIT120                                                          
         GOTO1 EXTRACT             EXTRACT GLOBAL STORAGE FROM AIO              
         SPACE 1                                                                
INIT120  CLI   OFFLINE,C'Y'        IF WE'RE OFFLINE                             
         BNE   INIT128                                                          
         CLI   MODE,VALKEY         AND WE'RE ABOUT TO VALIDATE KEY              
         BE    *+12                                                             
         CLI   MODE,VALREC         (TEST THIS MODE IN CASE PROG REC.)           
         BNE   INIT125                                                          
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         L     R1,MCVREMOT                                                      
         USING REMOTED,R1                                                       
         OC    REMOTKEY,REMOTKEY   AND GOING TO PQ                              
         BZ    INIT125                                                          
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTKEY(8),CONREC  SET REPORT NAME                              
         OC    REMOTKEY(8),SPACES                                               
         TM    WHEN,X'20'                 SOON?                                 
         BZ    INIT125                                                          
         CLC   REMOTKEY(8),=CL8'ADVICE'   AND ADVICE / RECEIVE                  
         BNE   INIT125                                                          
         CLI   ACTNUM,ACTRECV                                                   
         BNE   INIT125                                                          
         MVC   REMOTKEY(8),=CL8'TVR'      CHANGE REPORT NAME TO TVR             
INIT125  B     INIT130                                                          
         DROP  R1                                                               
         SPACE 1                                                                
INIT128  CLI   MODE,VALKEY         WE'RE ONLINE-TEST MODE VALIDATE KEY          
         BNE   INIT130                                                          
         TM    WHEN,X'38'          IF THIS IS FOR AN OFFLINE REQUEST            
         BZ    INIT130                                                          
         OC    TGCTSTAF,TGCTSTAF   ****TEMP CODE TO TRAP BUG ****               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CONKEY(8),TGCTSTAF  MOVE STAFF CODE TO KEY FIELD                 
         MVI   CONKEYH+5,8                                                      
         SPACE 1                                                                
INIT130  CLI   TGCTSTTY,TASTTYPP   TEST FOR PROGRAMMERS                         
         BNE   INIT200                                                          
         CLI   ACTNUM,ACTSEL       IF THIS IS SELECT                            
         BNE   *+16                                                             
         CLI   MODE,DISPKEY        AND MODE IS DISPLAY KEY                      
         BNE   INIT200                                                          
         B     *+12                                                             
         CLI   MODE,DISPREC        ELSE IF MODE IS DISPLAY RECORD               
         BNE   INIT140                                                          
         MVC   CONHED2(5),=C'(D/A=' DISPLAY D/A OF RECORD                       
         GOTO1 HEXOUT,DMCB,DMDSKADD,CONHED2+5,4,0                               
         MVC   CONHED2+13(4),=C')(L=' DISPLAY LENGTH OF RECORD                  
         L     RE,AIO                                                           
         USING TLRCD,RE                                                         
         EDIT  (2,TLRCLEN),(4,CONHED2+17),ALIGN=LEFT                            
         DROP  RE                                                               
         LR    RE,R0                                                            
         LA    RE,CONHED2+17(RE)                                                
         MVI   0(RE),C')'                                                       
         MVC   TGDSKADD,CONHED2+5  SAVE D/A IN GLOBAL STORAGE                   
         B     *+10                                                             
INIT140  XC    CONHED2(21),CONHED2 PRE-CLEAR D/A DISPLAY AREA                   
         SPACE 1                                                                
INIT200  BAS   RE,LOCKCHK          TEST URGENT CHECK RUN LOCKOUT STATUS         
         SPACE 1                                                                
INITX    B     XIT                                                              
         EJECT                                                                  
*              LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                           
         SPACE 1                                                                
*                                  P1, BYTE 0 = MAXIMUM PFKEY TO CHECK          
*                                  P1  BYTES 1-3 = A(PFKEY VAL. TABLE)          
PFVAL    NTR1                                                                   
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         TM    TIOBINDS,TIOBSUBS   IF OVERRODE SUB-SCREEN FOR STEREO            
         BO    NO                  SKIP PFVAL (TIOBAID OVERRIDED)               
         SPACE 1                                                                
PFV1     CLC   =C'Review complete - hit pf13 to approve',SCONHEAD               
         BNE   PFV1A                                                            
         CLI   PFAID,13                                                         
         BE    PFV1A                                                            
         CLI   PFAID,24                                                         
         BNE   NO                                                               
         SPACE 1                                                                
PFV1A    CLI   PFAID,1             TEST FOR INVALID HELP REQUEST                
         BE    HELPERR                                                          
         BL    NO                  USER HIT ENTER                               
         SPACE 1                                                                
         CLC   PFAID,0(R1)         INSURE WITHIN BOUNDARY                       
         BH    NO                                                               
         L     RF,0(R1)            RF=A(PFKEY TABLE)                            
         USING PFTABD,RF           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(RF),X'FF'                                                      
         BNE   *+16                                                             
         CLI   PFAID,12            GLOBAL PF KEYS ALWAYS OK                     
         BNH   NO                                                               
         B     PFERR                                                            
         SPACE 1                                                                
         TM    PFTSTAT,PFTGPFK     IF LOAD GLOBAL PFKEY SCREEN                  
         BZ    *+12                                                             
         CLI   TGCTSTTY,TASTTYPP   SKIP IF PROGRAMMER                           
         BE    PFV2D                                                            
         TM    PFTSTAT,PFTINT      TEST INTERNAL PFKEY                          
         BZ    *+12                                                             
         TM    TRNSTAT,OKINTPFK    TEST IT'S OK TO RECOGNIZE THEM               
         BZ    *+14                                                             
         CLC   PFAID,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
PFV2D    ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     PFV2                                                             
         SPACE 1                                                                
PFV3     TM    PFTSTAT,PFTMAIN     IF THIS IS A MAINT. ACTION CHANGE            
         BZ    *+12                                                             
         CLI   TWALACT,ACTPAY      LAST ACTION CAN'T HAVE BEEN PAY              
         BE    PFERR                                                            
         SPACE 1                                                                
         TM    PFTSTAT2,PFTVCAST   IF GOING TO VCAST SCREEN                     
         BZ    *+8                                                              
         MVI   THISLSEL,C'V'       SET SELECT FIELD                             
         SPACE 1                                                                
         TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    NO                                                               
         TM    PFTSTAT,PFTLIST     IF THIS IS A LIST ACTION CHANGE              
         BZ    PFV4                                                             
         CLI   TWASCR,SCR0A        TEST CAST LIST - WILL HANDLE ITSELF          
         BE    NO                                                               
         CLI   TWASCR,SCR6A        TEST EMPL LIST - WILL HANDLE ITSELF          
         BE    NO                                                               
         CLI   TWASCR,SCR01        AS WILL SOAP CAST                            
         BE    NO                                                               
         CLI   TWASCR,SCR03        AS WILL RCAST                                
         BE    NO                                                               
         CLI   TWASCR,SCR2C        AS WILL ESTIMATE MAINTENANCE                 
         BE    NO                                                               
         CLI   TWALACT,ACTPAY      AS WILL PAY                                  
         BE    NO                                                               
         CLI   TWASCR,SCR88        AS WILL GUARANTEE TRACKING                   
         BE    NO                                                               
         CLI   TWASCR,SCR8A        AS WILL FIXED CYCLE TRACKING                 
         BE    NO                                                               
         CLI   TWASCR,SCRA0        AS WILL INVOICE RELEASE                      
         BE    NO                                                               
         CLI   TWALACT,ACTLIST     LAST ACTION MUST HAVE BEEN LIST              
         BE    PFV4                                                             
         CLI   TWALACT,ACTSEL      OR SELECT                                    
         BNE   PFERR                                                            
         SPACE 1                                                                
PFV4     CLI   TWASCR,SCR1A        IF SCREEN IS CAST EXTENSION SCREEN           
         BE    PFV4D                                                            
         CLI   TWASCR,SCRA3        OR RCAST EXTENSION SCREEN                    
         BE    PFV4D                                                            
         CLI   TWASCR,SCRCA        OR SOAP CAST EXTENSION SCREEN                
         BE    PFV4D                                                            
         CLI   TWASCR,SCR69        OR EMPLOYEE EXTENSION SCREEN                 
         BE    PFV4D                                                            
         CLI   TWASCR,SCR6B        OR EVTIME EXTENSION SCREEN                   
         BNE   PFV5                                                             
PFV4D    CLI   PFTAID,2            AND USER HIT KEY TO CHANGE ACTION            
         BNE   PFV5                                                             
         OI    TRNSTAT,USERCHA     AND SET USER CAUSED 'CHA' FLAG               
         B     PFV6                                                             
         SPACE 1                                                                
PFV5     CLI   TWASCR,SCR18       IF CURRENT SCREEN IS COMMERCIAL               
         BE    *+12                                                             
         CLI   TWASCR,SCRF8       OR COMMERCIAL2                                
         BNE   PFV5A                                                            
         CLI   PFAID,19           AND FLIPPING BETWEEN THEM                     
         BE    PFV10              DO NOT CLEAR STACK                            
         SPACE 1                                                                
PFV5A    TM    PFTSTAT,PFTCPROG+PFTRPROG ELSE IF NOT CPROG/RPROG ACTION         
         BNZ   PFV6                                                             
         CLI   PFAID,7             AND NOT PF7 OR PF8                           
         BE    PFV6                (IN CASE PUSHED TO HIST LIST)                
         CLI   PFAID,8                                                          
         BE    PFV6                                                             
         BAS   RE,CLRSTACK         THEN CLEAR CALLPROG STACK                    
         SPACE 1                                                                
PFV6     CLI   PFAID,10            IF CAST/LIST                                 
         BE    *+12                                                             
         CLI   PFAID,11            OR HISTORY/LIST PFKEY PRESSED                
         BNE   PFV8                                                             
         CLI   TWASCR,SCR38        TEST COMML LIST - WILL HANDLE ITSELF         
         BE    PFV7                                                             
         CLI   TWASCR,SCR48             CCOM  LIST                              
         BE    PFV7                                                             
         CLI   TWASCR,SCR3A             GCAST LIST                              
         BE    PFV7                                                             
         CLI   TWASCR,SCRB6             PCOM  LIST                              
         BE    PFV7                                                             
         CLI   TWASCR,SCRCE             AFMCON LIST                             
         BNE   PFV8                                                             
PFV7     CLI   PFTNKEYS,0          TEST KEY FIELDS NOT PRESENT                  
         BE    NO                  MUST BE GLOBAL CHECK - GET OUT               
         SPACE 1                                                                
PFV8     CLI   PFAID,3             IF W4/DIS PFKEY PRESSED                      
         BNE   PFV10                                                            
         CLI   TWASCR,SCR0A        AND SCREEN IS CAST/LIST                      
         BE    PFV9                                                             
         CLI   TWASCR,SCR6A        AND SCREEN IS EMPLOYEE/LIST                  
         BE    PFV9                                                             
         CLI   TWASCR,SCR03        OR RCAST/LIST                                
         BE    PFV9                                                             
         CLI   TWASCR,SCRBC        OR PCAST/LIST                                
         BE    PFV9                                                             
         CLI   TWASCR,SCR01        OR SOAP CAST/LIST                            
         BNE   PFV10                                                            
PFV9     CLI   PFTNKEYS,0          AND KEY FIELD ENTRIES NOT PRESENT            
         BE    NO                  THEN LET CAST LIST HANDLE IT                 
         SPACE 1                                                                
PFV10    BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES                 IF RETURNS, RETURN CC EQUAL                  
         EJECT                                                                  
*              ROUTINE TO PROCESS PFKEY REQUEST                                 
         SPACE 1                                                                
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         ST    RF,APFTENT          SAVE A(PFTAB ENTRY)                          
         MVI   PFAID,0             CLEAR PFKEY FOR NEXT SCREEN                  
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         MVI   TIOBAID,0           CLEAR PF KEY HERE AS WELL                    
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPP   IF PROGRAMMER                                
         BNE   PFI1                                                             
         CLI   PFTAID,6            AND PF6 PRESSED                              
         BNE   PFI1                                                             
         CLI   TWASCR,SCRD1        THEN IF RECORD/DISPLAY NOT ACTIVE            
         BE    *+14                                                             
         MVC   SVRECORD,CONREC     SAVE CURRENT RECORD FIELD                    
         B     PFI1                AND CONTINUE                                 
         MVC   CONREC,SVRECORD     ELSE RESTORE RECORD FIELD                    
         OC    CONREC,SPACES                                                    
         MVI   CONRECH+5,L'CONREC                                               
         B     XIT                 AND RETURN                                   
         SPACE 1                                                                
PFI1     TM    PFTSTAT,PFTPUSH     IF PFKEY FOR PUSH                            
         BZ    *+8                                                              
         BAS   RE,VALRCRD          FIRST VALIDATE RECORD TO PUSH TO             
         SPACE 1                                                                
         TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
         XC    TGFSTREC,TGFSTREC                                                
         SPACE                                                                  
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
         SPACE 1                                                                
         TM    PFTSTAT,PFTPUSH     TEST PFKEY FOR PUSH                          
         BZ    *+12                                                             
         BAS   RE,GETRCRD          GET RECORD FOR DISPLAY                       
         B     DUMMYERR            TAKE DUMMY ERROR EXIT FOR GOAGAIN            
         SPACE 1                                                                
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    PFI2                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         TM    PRGSTAT,PAYINITD    IF RESTORED SCREEN IS NOT FROM PAY           
         BZ    DUMMYERR            TAKE DUMMY ERROR EXIT FOR GOAGAIN            
         B     DUMMYXIT            ELSE SIMPLY EXIT                             
         SPACE 1                                                                
PFI2     TM    PFTSTAT,PFTGPFK     LOAD GLOBAL PFKEY SCREEN                     
         BZ    PFI3                                                             
         TM    TGCTSTST,TGCTSCLI   IF THIS ISN'T A CLIENT                       
         BZ    PFI3                IGNORE BIT AND GO CHANGE REC/ACT             
         MVI   OVERLAY,SCR0F       ELSE SET SCREEN NUMBER                       
         BRAS  RE,LOADSCRN         LOAD IT                                      
         B     PFLOADED            AND EXIT TO USER                             
         SPACE 1                                                                
PFI3     TM    PFTSTAT,PFTPAY      LOAD PAY SCREEN                              
         BZ    PFI4                                                             
         MVI   TWASCR,0                                                         
         MVI   OVERLAY,SCR90       SET SCREEN NUMBER                            
         MVI   ALTPROG,X'F3'       SET ALT. PROGRAM FOR PAY SCREENS             
         BRAS  RE,LOADSCRN                                                      
         MVI   ALTPROG,X'F2'       RESET ALT PROGRAM FOR SCREENS                
         MVC   CONACT(3),=C'PAY'   PRE-SET ACTION                               
         B     PYLOADED            EXIT TO USER                                 
         DROP  RE                                                               
*                                                                               
PFI4     CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BNE   PFI6                GO MOVE IT OUT                               
         TM    PFTSTAT2,PFTUSE     OR IF WE HAVE USE TYPE                       
         BZ    PFI8                                                             
         MVC   CONREC,SPACES                                                    
         MVC   CONREC(3),TGUSCDE   MOVE IT OUT                                  
         MVI   ALTPROG,X'F3'                                                    
         B     PFI7                                                             
         SPACE 1                                                                
PFI6     MVC   CONREC,PFTREC       MOVE OUT NEW RECORD TYPE                     
         SPACE 1                                                                
         CLI   PFTAID,10           IF REQUESTING CAST LIST                      
         BNE   PFI6D                                                            
         CLI   TGCTEQU,CTYSOAP     AND CURRENT COMML TYPE IS SOAP               
         BNE   PFI6D                                                            
         MVI   CONREC,C'S'         THEN INSERT 'S' BEFORE RECORD TYPE           
         MVC   CONREC+1(L'CONREC-1),PFTREC                                      
         SPACE 1                                                                
PFI6D    CLI   PFTAID,9            IF REQUESTING COMMERCIAL DISPLAY             
         BE    *+12                                                             
         CLI   PFTAID,10           OR CAST LIST                                 
         BNE   PFI7                                                             
         MVI   TWASCR,0                                                         
         CLI   TWASCR,SCRB6        AND COMING FROM PCOM                         
         BE    PFI6P                                                            
         CLI   ACTNUM,ACTLIST      OR NOT COMING FROM LIST                      
         BE    PFI7                                                             
         TM    TGMEEQU,PRINT       AND CURRENT COMMERCIAL IS PRINT              
         BZ    PFI7                                                             
PFI6P    MVI   CONREC,C'P'         THEN INSERT 'P' BEFORE RECORD TYPE           
         MVC   CONREC+1(L'CONREC-1),PFTREC                                      
         SPACE 1                                                                
PFI7     OI    CONRECH+6,X'80'     TRANSMIT NEW RECORD TYPE                     
         MVI   CONRECH+5,8         SET L'I/P                                    
         SPACE 1                                                                
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
         SPACE 1                                                                
PFI8     CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BE    PFI10                                                            
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
         SPACE 1                                                                
         MVC   TGTHREE,CONACT                                                   
         OC    TGTHREE,SPACES                                                   
         CLC   =C'P  ',TGTHREE                                                  
         BE    PFI9                                                             
         CLC   =C'PA',TGTHREE      IF ACTION IS PAY                             
         BNE   PFI10                                                            
PFI9     MVI   ALTPROG,X'F3'       SET ALT. PROGRAM FOR PAY SCREENS             
         SPACE 1                                                                
PFI10    CLI   CONWHENH+5,0        DON'T BOTHER IF INPUT PRESENT                
         BNE   PFIX                                                             
         TM    PFTSTAT2,PFTSETPN   TEST PRESET OF PRINT FIELD TO NOW            
         BZ    PFI10D                                                           
         MVC   CONWHEN(4),=C'NOW,'    PRESET PRINT FIELD FOR NOW                
         MVC   CONWHEN+4(2),TGCTSTAF                                            
         MVI   CONWHENH+5,6                                                     
         B     PFI10X                                                           
         SPACE 1                                                                
PFI10D   TM    PFTSTAT2,PFTSETPR   TEST PRESET OF PRINT FIELD TO SOON           
         BZ    PFIX                                                             
         MVC   CONWHEN(5),=C'SOON,'   PRESET PRINT FIELD FOR SOON               
         MVC   CONWHEN+5(2),TGCTSTAF                                            
         MVI   CONWHENH+5,7                                                     
PFI10X   OI    CONWHENH+6,X'80'    TRANSMIT NEW PRINT FIELD                     
         SPACE 1                                                                
PFIX     BAS   RE,PCTASP           HANDLE PCTALENT ALTERNATE SCREEN             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
PCTASP   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         L     R1,SYSPARMS         HANDLE PCTALENT ALTERNATE SCREEN             
         L     RE,0(R1)            IF ONLINE                                    
         USING TIOBD,RE                                                         
         NI    TIOBINDS,X'FF'-TIOBASP                                           
*                                                                               
         CLI   ALTPROG,X'F3'       PAY SCREEN, TURN ON BIT                      
         BE    PCTASP9                                                          
         CLI   ALTPROG,X'F2'                                                    
         BNE   PCTASPX                                                          
*                                                                               
         LA    RF,F2SCRTAB         POINT TO NON-PAY SCREEN TABLE                
PCTASP3  CLI   0(RF),X'FF'         EOT?                                         
         BE    PCTASPX                                                          
         CLC   TWASCR,0(RF)        FOUND MATCH?                                 
         BE    PCTASP9             TURN BIT ON AND LEAVE                        
         AHI   RF,1                                                             
         B     PCTASP3             LOOP                                         
*                                                                               
PCTASP9  OI    TIOBINDS,TIOBASP                                                 
PCTASPX  B     XIT                                                              
*                                                                               
F2SCRTAB DC    0X                  NON-PAY SCREEN THAT NEED BIT ON              
         DC    X'44'               INVOICE APPROVE/REOPEN                       
         DC    X'50'               INTERNET/NEW MEDIA DISPLAY                   
         DC    X'51'               INTERNET/NEW MEDIA LIST                      
         DC    X'52'               NMR DISPLAY                                  
         DC    X'53'               ADVICES - INTERNET/NEW MEDIA                 
         DC    X'54'               GRT DISPLAY                                  
         DC    X'55'               GRT MAINTENANCE - LARGE OVERSCALE            
         DC    X'56'               GRT MAINTENANCE - PERCYCLE GRT               
         DC    X'57'               GRTCMT                                       
         DC    X'58'               VCAST LIST                                   
         DC    X'59'               ISU DISPLAY                                  
         DC    X'85'               OPTIONS DISPLAY                              
         DC    X'AA'               CLA DISPLAY                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
         SPACE 1                                                                
*                                  R3=A(PFKEY TABLE)                            
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST OR DELETED LIST SCREEN               
         BE    TSEL1                                                            
         CLI   ACTNUM,ACTLDEL                                                   
         BE    TSEL1                                                            
         CLI   ACTNUM,ACTCOMP                                                   
         BE    TSELX                                                            
         OC    TGFSTREC,TGFSTREC                                                
         BZ    TSELX                                                            
TSEL1    TM    GLSTSTAT,NOSELFLD   & SELECT FIELD EXISTS (TEST REQUIRES         
         BO    TSELX               BIT SET BEFORE INITIAL CALL IN PGM!)         
         SPACE 1                                                                
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         OC    TGFSTREC,TGFSTREC                                                
         BZ    *+8                                                              
         ICM   R2,15,TGFSTREC                                                   
         ZIC   R4,0(R2)                                                         
         SH    R4,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R4,=H'8'            R4 = LENGTH OF FIELD - 1                     
         SPACE 1                                                                
         BAS   RE,SETROW           SET ROW (RETURNS R1=ROW NUMBER-1             
*                                                   R0=COL NUMBER-1)            
         SPACE 1                                                                
TSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
         SPACE 1                                                                
         CH    R0,=H'1'            SELECT FIELD MUST BE ON/BEFORE COL 2         
         BH    TSEL6                                                            
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
         EX    R4,*+8              YES, SO PAD WITH SPACES                      
         B     *+10                                                             
         OC    8(0,R2),SPACES                                                   
         SPACE 1                                                                
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   PFTSEL(0),8(R2)     MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
         SPACE 1                                                                
         TM    PFTSTAT2,PFTUSE     OR IF EXPLICITLY ALLOWED                     
         BZ    TSEL5                                                            
         LR    R0,RF                                                            
         GOTO1 USEVAL,DMCB,(X'40',8(R2))  TEST FOR VALID USE CODE               
         LR    RF,R0                                                            
         BE    TSEL8                                                            
         SPACE 1                                                                
TSEL5    ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     TSEL4                                                            
         SPACE 1                                                                
TSEL6    BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         BE    TSELX               (E-O-S)                                      
         TM    1(R2),X'20'         IGNORE PROTECTED FIELDS                      
         BO    TSEL6                                                            
         BAS   RE,SETROW           SET ROW (RETURNS R1=ROW NUMBER-1)            
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL2               SELECT FIELD                                 
         B     TSEL6                                                            
         SPACE 1                                                                
TSEL8    EX    R4,*+8              FOUND A MATCH - CLEAR SELECT FIELD           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LR    R0,R2               SAVE A(FIELD)                                
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         BAS   RE,ANYFLD           TEST THERE'S SOMETHING TO SELECT             
         BNE   TSEL6               (NO, SO IGNORE)                              
         SPACE 1                                                                
         ST    R0,ROWADDR          SAVE ADDR OF SELECT FIELD FOR CAST           
         SPACE 1                                                                
         MVC   PFAID,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    R0,RA                                                            
         STH   R0,CURDISP          SAVE DISP. TO FIELD                          
         OI    TRNSTAT,OKINTPFK    SET OK TO RECOGNIZE INTERNAL PFKEY           
TSELX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE RECORD TO DISPLAY ON A PUSH              
         SPACE 1                                                                
VALRCRD  NTR1                                                                   
         LA    RF,RCRDTAB          RF=A(RCRDTAB ENTRY)                          
         LR    R2,RA                                                            
         AH    R2,CURDISP          R2=A(FIELD HEADER)                           
         SPACE                                                                  
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BO    VALR3                                                            
         BAS   RE,TESTEST          IF NONE, TEST IF ESTIMATING                  
         BNE   FLDPFERR            ERROR IF NOT, ELSE                           
         MVI   BYTE,64             SET DUMMY EXTENDED HEADER FOR COMM'L         
         LA    R3,BYTE             POINT R3 TO IT                               
         B     VALR5                                                            
         SPACE                                                                  
VALR3    ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'                                                         
         LA    R3,0(R1,R2)         R3=A(EXTENDED HEADER)                        
         SPACE                                                                  
VALR5    CLI   0(RF),X'FF'                                                      
         BE    FLDPFERR            ERROR IF NOT FOUND IN RCRDTAB                
         CLC   0(1,RF),0(R3)                                                    
         BE    VALR15                                                           
         LA    RF,L'RCRDTAB(RF)    BUMP TO NEXT RCRD TABLE ENTRY                
         B     VALR5                                                            
         SPACE                                                                  
VALR15   L     RE,ARECACT          RE=A(RECACT ENTRY)                           
VALR17   CLI   0(RE),X'01'                                                      
         BNE   FLDPFERR            ERROR IF RECORD TYPE NOT FOUND               
         CLC   9(1,RE),1(RF)       MATCH ON RECORD TYPE                         
         BE    VALR30                                                           
         ZIC   R1,LRECACT                                                       
         AR    RE,R1                                                            
         B     VALR17                                                           
         SPACE                                                                  
VALR30   CLI   8(R2),X'40'         TEST PRESENCE OF CHAR IN 1ST POS.            
         BNH   PFLDMISS            (ALLOWS PFKEY IF FIELD PROTECTED)            
         SPACE                                                                  
         ST    RE,RECADDR          SAVE ADDRESS OF RECACT ENTRY                 
         SPACE                                                                  
         ZIC   R4,2(RF)            R4=DISP. TO FIELD IN GLOBAL STORAGE          
         LA    R4,TGD(R4)                                                       
         ZIC   R1,3(RF)            R1=L'FIELD-1                                 
         EX    R1,MVCTOGLB         MOVE TO GLOBAL STORAGE                       
         EX    R1,OCSPACES         INSURE PADDED WITH SPACES                    
         SPACE                                                                  
         CLI   0(RF),TLGUCDQ       IF THIS IS GUARANTEE                         
         BNE   VALR40                                                           
         XC    0(L'TGGUA,R4),HEXFFS   COMPLEMENT CODE                           
         SPACE                                                                  
         CLI   ALTPROG,X'F3'                                                    
         BE    VALR40                                                           
         CLI   TWASCR,SCR0A        IF WE'RE ON CAST LIST SCREEN                 
         BE    *+8                                                              
         CLI   TWASCR,SCR6A        OR EMPLOYEE LIST SCREEN                      
         BNE   *+8                                                              
         BAS   RE,NEEDSSN          NEED CORRESPONDING S/S NUMBER                
         SPACE                                                                  
VALR40   CLI   0(RF),TLLOCDQ       IF THIS IS LOCAL                             
         BNE   VALRX                                                            
         CLI   SVREC,CA            AND WE'RE ON CAST SCREEN                     
         BE    VALR44                                                           
         CLI   SVREC,SO            OR SOAP CAST SCREEN                          
         BE    VALR44                                                           
         CLI   SVREC,EC            OR ECAST SCREEN                              
         BE    VALR44                                                           
         CLI   SVREC,RS            OR RCAST SCREEN                              
         BNE   VALRX                                                            
VALR44   MVC   TGLCL,8+3+1(R2)     GET LOCAL FROM 1 PAST UNION                  
         OC    TGLCL,SPACES                                                     
         SPACE                                                                  
VALRX    B     XIT                                                              
         SPACE 3                                                                
MVCTOGLB MVC   0(0,R4),8(R2)       MOVE FIELD DATA TO GLOBAL STORAGE            
OCSPACES OC    0(0,R4),SPACES      PAD WITH SPACES                              
         SPACE 3                                                                
*              ROUTINE TO TEST IF CURRENT SCREEN IS ESTIMATING                  
*              R2=A(CURRENT FIELD HEADER)                                       
         SPACE                                                                  
TESTEST  NTR1                                                                   
TESTES5  BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         BE    NO                                                               
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    TESTES5                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         SH    R2,=H'4'            BUMP R2 TO SCREEN NUMBER                     
         CLI   0(R2),X'2C'         TEST FOR ESTIMATE SCREEN                     
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE DERIVES SSN ON CAST/LIST SCREEN FOR GUAR PUSH            
         SPACE 1                                                                
*                                  R2=A(GUARANTEE FIELD)                        
NEEDSSN  NTR1                                                                   
         LA    RF,CONTAGH          RF=A(FIRST FIELD ON SCREEN)                  
         SPACE 1                                                                
NSSN2    TM    1(RF),X'02'         FIELD MUST HAVE EXTENDED HEADER              
         BZ    NSSN4                                                            
         ZIC   R1,0(RF)                                                         
         SH    R1,=H'8'                                                         
         AR    R1,RF               R1=A(EXTENDED HEADER)                        
         SPACE 1                                                                
         CLI   0(R1),TLW4CDQ       REQUIRE W4 RECORD CODE IN FIELD NO.          
         BNE   NSSN4                                                            
         MVC   TGSSN,8(RF)         THIS IS S/S NUMBER FIELD - SAVE IT           
         SPACE 1                                                                
         LR    R0,RF                                                            
         MVC   TGPID,8(RF)                                                      
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         LR    RF,R0                                                            
         SPACE 1                                                                
NSSN4    ZIC   R1,0(RF)                                                         
         AR    RF,R1               BUMP TO NEXT FIELD                           
         SPACE 1                                                                
         CR    RF,R2               IF WE'VE REACHED GUARANTEE FIELD             
         BE    XIT                 THEN WE'RE DONE                              
         B     NSSN2               ELSE KEEP ON TRYING                          
         EJECT                                                                  
* THIS ROUTINE SAVES THE CURRENT TWA IN ONE OF FOUR HALVES OF TEMPSTR           
* RECORD NUMBERS 2 AND 3.  IT THEN SAVES THE SCREEN NUMBER FOUND IN             
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH             
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO            
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE            
* WORKING ON.  THE FOUR HALVES OF TEMPSTR RECORDS 2 AND 3 ALLOWS FOR            
* FOUR LEVELS OF SCREEN CALLING AND RETURNING.  WHEN THE USER WANTS TO          
* RETURN TO A SCREEN, RETPROG WILL BE CALLED TO RESTORE THE SCREEN.             
*                                                                               
CPROG    NTR1                                                                   
         OC    TGFSTREC,TGFSTREC                                                
         BZ    *+8                                                              
         MVI   CALLSP,0                                                         
*                                                                               
         CLI   CALLSP,CALLSMAX     IF ALREADY HAVE MAX NEST LEVELS              
         BNL   CANTPUSH            ERROR FOR STACK OVERFLOW                     
*                                                                               
         ZIC   R3,CALLSP           SAVE SCREEN NUMBER ON STACK                  
         LA    RF,CALLSTCK(R3)                                                  
         MVC   0(1,RF),TWASCR                                                   
*                                                                               
         LA    R3,1(R3)            INCREMENT STACK POINTER                      
         STC   R3,CALLSP                                                        
*                                                                               
         BAS   RE,REPROT           REPROTECT RECORD/ACTION FLDS IF NEC.         
*                                                                               
         L     RE,ATIA             SAVE FROM CONRECH FOR X'D00' INTO            
         LA    RF,X'D00'               TWA RECORD #2                            
         LA    R0,CONRECH                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATIA             SAVE CURRENT MESSAGE AFTER TWA               
         LA    RE,X'D00'(RE)                                                    
         MVC   0(L'SCONHEAD,RE),SCONHEAD                                        
         MVC   L'SCONHEAD(2,RE),CONSERV+6 SAVE SUB-SCREEN AFTER MSG             
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),STARTSV)                                       
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
REPROT   DS    0H                                                               
         TM    TRNSTAT2,UNPROTRA   IF UNPROTECTED REC/ACT DURING PAY            
         BZR   RE                                                               
         OI    CONRECH+1,X'20'     TURN PROTECTED BIT BACK ON                   
         NI    CONRECH+6,X'7F'     AND TURN OFF TRANSMIT BIT                    
         OI    CONACTH+1,X'20'                                                  
         NI    CONACTH+6,X'7F'                                                  
         BR    RE                                                               
         EJECT                                                                  
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON             
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG.         
*                                                                               
RPROG    NTR1                                                                   
         CLI   CALLSP,CALLSOFF     ERROR IF DISABLED                            
         BE    PFERR                                                            
         CLI   CALLSP,0            ERROR IF STACK IS EMPTY                      
         BE    PFERR                                                            
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE CONRECH FOR X'D00' FROM              
         LA    RF,X'D00'               TWA RECORD #2                            
         LA    R0,CONRECH                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ATIA             RESTORE SAVED MESSAGE IN CASE WE             
         LA    R2,X'D00'(R2)       DON'T RETURN TO GENCON                       
         MVC   CONHEAD,0(R2)                                                    
         MVC   SCONHEAD,CONHEAD    SAVE MESSAGE                                 
*                                  READ TWA RECORD #3 FOR MAX L'STORAGE         
         LH    R0,LSVTWA0          RESTORE SAVED STORAGE                        
         GOTO1 GETTWA,DMCB,(X'23',STARTSV),(R0)                                 
*                                                                               
         ZIC   R3,CALLSP           DECREMENT STACK POINTER                      
         BCTR  R3,0                                                             
         STC   R3,CALLSP                                                        
*                                                                               
         LA    RF,CALLSTCK(R3)     EXTRACT TWASCR                               
         MVC   TWASCR,0(RF)                                                     
*&&DO                                                                           
         TM    TRNSTAT2,CTSTEREO   IF STEREO ACTIVE                             
         BZ    RPROG10                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSCRN   SET OVERRIDING SCREEN FOR STEREO             
         MVC   TIOBCNT(1),TWASCR   RESTORE ORIGINAL SCREEN CODE                 
         OI    TIOBINDS,TIOBSUBS   SET OVERRIDING SUB-SCREEN FOR STEREO         
         LR    R0,RE               SAVE RE                                      
         GOTO1 HEXIN,DMCB,L'SCONHEAD(R2),BYTE,2                                 
         LR    RE,R0               RESTORE RE                                   
         ZIC   R1,BYTE             CHANGE X0 TO 0X                              
         SRL   R1,4                                                             
         STC   R1,TIOBAID          RESTORE ORIGINAL SUB-SCREEN CODE             
*&&                                                                             
RPROG10  LA    R2,CONHEADH         MUST SET INDICTOR TO XMIT ALL FIELDS         
         BAS   RE,BUMP                 OR SCREEN WILL BE MESSED UP              
         BNE   *-4                 FIND END OF TWA                              
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         BAS   RE,POPDISP          CHANGE POP SCREEN DISPLAY                    
         OI    TRNSTAT,RETURNED    SET THAT RETPROG HAS BEEN CALLED             
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE UPDATES POP SCREEN DISPLAY IN CONHED2                    
         SPACE                                                                  
POPDISP  NTR1                                                                   
         LA    RE,CONHED2+L'CONHED2-1 RE=A(1ST POP SCREEN FROM CURRENT)         
         LA    R0,L'CONHED2-1                                                   
         SR    R1,R1               R1=L'(1ST POP SCREEN FROM CURRENT)           
POPD5    CLI   0(RE),C','                                                       
         BE    POPD10                                                           
         CLI   0(RE),C'='                                                       
         BE    POPD9               B IF ONLY 1 POP SCREEN                       
         LA    R1,1(R1)            INCREMENT LENGTH COUNT                       
         BCTR  RE,0                                                             
         BCT   R0,POPD5            KEEP LOOKING BACK TILL FIND ','              
         SPACE                                                                  
POPD9    XC    CONHED2,CONHED2     CLEAR CURRENT DISPLAY                        
         B     XIT                                                              
         SPACE                                                                  
POPD10   XC    WORK,WORK                                                        
         MVC   WORK(5),=C'(PF5='   BUILD DISPLAY FOR CONHED2                    
         LA    RF,WORK+5           RF=A(NEXT SPOT FOR DISPLAY)                  
         SPACE                                                                  
         LA    R2,5(R1)            R2=LENGTH OF NEW DISPLAY                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+5(0),1(RE)     MOVE FROM CURRENT TO NEW DISPLAY             
         SPACE                                                                  
         XC    CONHED2,CONHED2     CLEAR CURRENT DISPLAY                        
         LA    RE,CONHED2+L'CONHED2 RIGHT ADJUST NEW DISPLAY TO CONHED2         
         SR    RE,R2                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD                   
         SPACE 1                                                                
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R2,WORK             R2=A(WORK)                                   
         ZIC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
*                                                                               
EXP10    XR    R1,R1                                                            
         TM    KEYTYPE,KEYTYXCM    TEST EXCLUDING TRAILING COMMA                
         BZ    EXP12                                                            
         LA    R1,1                TURN ON FLAG                                 
         NI    KEYTYPE,X'FF'-KEYTYXCM                                           
*                                                                               
EXP12    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         CLI   KEYTYPE,KEYTYGLB    GLOBAL STORAGE                               
         BE    EXP15                                                            
         LA    RF,TWAHOLE                                                       
         CLI   KEYTYPE,KEYTYWS     W/S (TWAHOLE)                                
         BE    EXP15                                                            
         CLI   KEYTYPE,KEYTYCUR    CURSOR LOCATION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RF,CURDISP          ASSUME THIS IS A SELECT FIELD                
         AR    RF,RA               RF=A(FLD WHERE CURSOR IS)                    
         BAS   RE,BMPTOROW         BUMP TO FIRST FIELD FOR THIS ROW             
         BNE   PFERR                                                            
         L     RF,FULL             RETURNS ADDRESS IN FULL                      
*                                                                               
EXP15    AH    RF,KEYDISP          RF=A(DATA)                                   
         ZIC   RE,KEYLEN           RE=L'DATA-1                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE TO WORK                                 
         AR    R2,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R2),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
         LA    R2,1(R2)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CH    R3,=H'1'            TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    LTR   R1,R1               TEST EXCLUDING COMMA                         
         BNZ   EXP30                                                            
         MVI   0(R2),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R2,1(R2)            BUMP PAST COMMA TO NEXT POSITION             
*                                                                               
EXP30    LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R2,R3               R2=L'TMPKEY FIELD                            
         CLM   R2,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
         SPACE 1                                                                
         STC   R2,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     XIT                                                              
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
         EJECT                                                                  
*              SCREEN/FIELD MANIPULATION ROUTINES                               
         SPACE 2                                                                
BUMP     DS    0H                  BUMP TO NEXT SCREEN FIELD                    
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0             RETURN CC EQ IF E-O-S                        
         BR    RE                                                               
         SPACE 2                                                                
SETROW   DS    0H                  DETERMINE ROW (R2=A(FLD HEADER))             
         LH    R1,2(R2)                                                         
         XR    R0,R0                                                            
         D     R0,=F'80'           R1 NOW HAS ROW NUMBER - 1                    
         BR    RE                                                               
         SPACE 2                                                                
BMPTOROW NTR1                      BUMP TO FIRST FIELD IN ROW                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         BAS   RE,SETROW           SET ROW NUMBER-1 (RETURNS IN R1)             
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    BAS   RE,SETROW           SET ROW (RETURNS R1=ROW NUMBER-1)            
         CLM   R1,1,BYTE                                                        
         BE    *+16                                                             
         BAS   RE,BUMP             TRY NEXT FIELD                               
         BNE   BMPT2                                                            
         B     NO                  RETURN CC NE IF REACHED E-O-S                
*                                                                               
         L     RF,APFTENT          RF=A(PFTAB ENTRY)                            
         USING PFTABD,RF                                                        
         TM    PFTSTAT2,PFTNOSEL   UNLESS SPECIFIED OTHERWISE                   
         BO    *+8                                                              
         BAS   RE,BUMP             ASSMUME SELECT FIELD - BUMP PAST             
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FIELD) IN FULL                
         B     YES                                                              
         SPACE 2                                                                
ANYFLD   NTR1  ,                   TEST IF ANYTHING IN FIELD                    
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BZ    NO                                                               
         B     YES                                                              
         SPACE 2                                                                
CLRSTACK MVI   CALLSP,0            CLEAR CALLPROG STACK                         
         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
         XC    SVACTION,SVACTION   CLEAR SAVED ACTION FOR CAST                  
         NI    PRGSTAT,ALL-PAYINITD  TURN OFF PAY INITIALIZED BIT               
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE GETS THE RECORD TO PUSH TO AND DISPLAYS                  
*              THE STACK OF POP SCREENS IN CONHED2                              
         SPACE                                                                  
GETRCRD  NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(5),=C'(PF5='   BUILD DISPLAY FOR CONHED2                    
         LA    RF,WORK+5           RF=A(NEXT SPOT FOR DISPLAY)                  
         LA    R2,5                R2=LENGTH OF NEW DISPLAY                     
         MVC   0(L'CONREC,RF),CONREC PUT THIS SCRN REC FIRST TO DISPLAY         
         LA    R2,L'CONREC(R2)     UPDATE LENGTH COUNT                          
         LA    RF,L'CONREC-1(RF)   FIND LAST CHARACTER                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  R2,0                DECREMENT LENGTH COUNT                       
         BCT   RF,*-10                                                          
         SPACE                                                                  
         MVI   1(RF),C','          PUT ',' BEFORE NEXT POP SCREEN               
         SPACE                                                                  
         LA    RE,CONHED2+L'CONHED2-1                                           
GETR12   CLI   0(RE),C'='          FIND CURRENT POP SCRN AFTER '(PF5='          
         BE    GETR15                                                           
         CLI   0(RE),0                                                          
         BE    GETR14              IF NO OTHER POP SCREEN                       
         BCT   RE,GETR12                                                        
         SPACE                                                                  
GETR14   MVI   1(RF),C')'          END DISPLAY WITH ')'                         
GETR15   SR    RE,R2               PUT NEW POP SCRN BEFORE ANY CURRENT          
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         SPACE                                                                  
         L     RE,RECADDR          RE=A(RECACT ENTRY)                           
         MVC   CONREC,1(RE)        MOVE OUT RECORD                              
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
         SPACE 1                                                                
         CLC   CONREC,=C'COMMERCL'   IF PUSHING TO COMM'L                       
         BNE   GETR20                                                           
         MVC   CONREC,=CL8'HISTORY'  SET RECORD TO HISTORY INSTEAD              
         MVC   CONACT,=CL8'LIST'     AND SET ACTION TO LIST                     
         B     GETR30                                                           
*&&DO                                                                           
         TM    TRNSTAT2,CTSTEREO   IF STEREO ACTIVE                             
         BZ    GETR30                                                           
         CLI   RECNUM,ES           AND IF COMING FROM ESTIMATING                
         BNE   GETR30                                                           
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSUBS   SET OVERRIDING SUB-SCREEN FOR STEREO         
         MVI   TIOBAID,X'01'       SET SUB-SCREEN CODE                          
         B     GETR30                                                           
*&&                                                                             
GETR20   MVC   CONACT,=CL8'DISPLAY' SET ACTION DISPLAY                          
GETR30   OI    CONACTH+6,X'80'      TRANSMIT                                    
         MVI   CONACTH+5,8          SET L'I/P                                   
GETRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TESTS IF URGENT CHECK RUN LOCKOUT IN EFFECT              
         SPACE 1                                                                
LOCKCHK  NTR1                                                                   
*                                                                               
         LA    R1,LOCKRECS         R1=A(ELIGIBLE RECORDS)                       
LOCK10   CLC   RECNUM,0(R1)                                                     
         BE    LOCK20                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   LOCK10                                                           
         CLI   RECNUM,AN           SPECIAL FOR AGENT                            
         BE    LOCK14                                                           
         CLI   RECNUM,CO           OR COMMERCIAL                                
         BE    LOCK14                                                           
         CLI   RECNUM,MT           OR CNET/TMKT/RMKT/CSYS                       
         BE    LOCK14                                                           
         CLI   RECNUM,EV           OR EVENT                                     
         BE    LOCK14                                                           
         CLI   RECNUM,PCO          OR PRINT COMMERCIAL                          
         BNE   LOCKX                                                            
LOCK14   CLI   ACTNUM,ACTCOPY      MAY NEED TO LOCK OUT COPY                    
         BE    LOCK40                                                           
         CLI   ACTNUM,ACTADD       OR MAY NEED TO LOCK OUT ADD                  
         BE    LOCK40                                                           
         B     LOCKX                                                            
*                                                                               
LOCK20   LA    R1,LOCKACTS         R1=A(ELIGIBLE ACTIONS)                       
LOCK30   CLC   ACTNUM,0(R1)                                                     
         BE    LOCK40                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   LOCK30                                                           
         CLI   RECNUM,DR           DLETTER MAY ALSO NEED TO LCK OUT REP         
         BNE   *+16                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    LOCK40                                                           
         B     LOCKX                                                            
         CLI   RECNUM,GT           GTRACK MAY ALSO NEED TO LOCK OUT ADD         
         BE    LOCK14                                                           
         B     LOCKX                                                            
         SPACE 1                                                                
LOCK40   CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   LOCK50                                                           
         CLI   THISLSEL,C'C'       SELECT CODE MUST BE FOR CHANGE               
         BNE   LOCKX                                                            
*                                                                               
LOCK50   GOTO1 TSTLCKT,DMCB,=C'TAL_CHECKS'       TEST LOCK SET                  
         BE    CHKLOCK             THEN LOCKOUT ACCESS                          
         GOTO1 TSTLCKT,DMCB,=C'TAL_PRCHKS'       TEST LOCK SET                  
         BE    CHKLOCK             THEN LOCKOUT ACCESS                          
         GOTO1 TSTLCKT,DMCB,=C'TAL_P+CHKS'       TEST LOCK SET                  
         BE    CHKLOCK             THEN LOCKOUT ACCESS                          
LOCKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LOAD PAY COMMON ROUTINES                              
         SPACE 2                                                                
VLOADPAY DS    0H                                                               
         OC    APAYINIT,APAYINIT   TEST ALREADY LOADED                          
         BNZ   XIT                                                              
         MVI   OVERLAY,X'50'       SET PHASE NUMBER                             
         GOTO1 LOADSOPH,DMCB,0                                                  
         ST    R3,APAYINIT                                                      
         MVI   APAYINIT,0          FIRST ROUTINE INITIALIZES                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE PASSIVE POINTERS                                 
         SPACE 1                                                                
VSAVPTRS GOTO1 ASAVPTRS                                                         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO ADD/CHANGE PASSIVE POINTERS                           
         SPACE 1                                                                
VADDPTRS GOTO1 AADDPTRS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO READ AN ACCOUNT FILE RECORD                           
*                                                                               
*                                  P1=A(NAME FIELD) OR ZERO                     
*                                  P1, BYTE 0  X'80'=GET PROD. HEXCOMP          
VREADACC DS    0H                                                               
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         TM    0(R1),X'80'         TEST PRODUCTION-SIDE HEXCOMP REQ'D           
         BZ    VRA6                                                             
*                                                                               
         MVC   BLOCK(L'KEY),KEY    SAVE CALLER'S KEY                            
*                                                                               
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'A4',TGAGY) GET INTERFACE REC.             
         MVI   ERROR,ERINTER       INTERFACE RECORD NOT FOUND                   
         BNE   NO                                                               
         MVI   ELCODE,TAIFELQ      GET INTERFACE ELEMENT                        
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TAIFD,R3                                                         
         GOTO1 USERVAL,DMCB,(X'80',TAIFAGY) READ AGENCY USER ID                 
*                                                                               
         MVC   KEY,BLOCK           RESTORE CALLER'S KEY                         
         CLI   KEY,X'40'           NOW HAVE HEXCOMP - MOVE TO KEY               
         BNE   *+14                                                             
         MVC   KEY(1),TGACCHX                                                   
         B     *+10                                                             
         MVC   KEY+1(1),TGACCHX                                                 
*                                                                               
VRA6     MVC   DMCB(1),TGACCSE     SWITCH TO APPROPRIATE ACC SYSTEM             
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         MVI   ERROR,ERSWACC       CAN'T SWITCH TO ACC SYSTEM                   
         BNE   VRA20                                                            
*                                                                               
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=CL8'ACCFIL'                                            
         GOTO1 HIGH                READ RECORD                                  
         MVI   USEIO,C'N'                                                       
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   DMCB(1),SVSYS       SWITCH BACK TO ORIGINAL SYSTEM               
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LTR   R2,R2               IF NAME FIELD GIVEN                          
         BZ    VRA10                                                            
*                                                                               
         ZIC   R4,0(R2)            THEN PRE CLEAR NAME FIELD                    
         SH    R4,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R4,=H'8'            R4 = LENGTH OF FIELD - 1                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
VRA10    CLC   KEY(42),KEYSAVE     IF RECORD NOT FOUND THEN RETURN              
         MVI   ERROR,NOTFOUND      ACC RECORD NOT FOUND                         
         BNE   NO                      CC NEQ                                   
*                                                                               
         LTR   R2,R2               IF NAME FIELD NOT GIVEN THEN RETURN          
         BZ    YES                                                              
*                                                                               
         L     R3,AIO              ELSE POINT R3 TO NAME ELEMENT                
         LA    R3,49(R3)                                                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   YES                                                              
*                                                                               
         ZIC   R1,1(R3)            R1 = LENGTH OF NAME - 1                      
         SH    R1,=H'3'                                                         
*                                                                               
         CR    R1,R4               R1 = LESSER OF L'NAME AND L'FIELD            
         BNH   *+6                                                              
         LR    R1,R4                                                            
*                                                                               
         EX    R1,*+8              MOVE NAME TO FIELD                           
         B     *+10                                                             
         MVC   8(0,R2),2(R3)                                                    
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         B     YES                                                              
*                                                                               
VRA20    MVC   DMCB(1),SVSYS       SWITCH BACK TO ORIGINAL SYSTEM               
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         BE    NO                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*              ROUTINE SETS KEY FOR ONLINE LISTS                                
         SPACE 1                                                                
*                                  P1, BYTE 0    = RECORD EQUATE                
*                                  P1, BYTES 1-3 = A(KEY TO SET)                
VSETLSTK DS    0H                                                               
         MVC   TGBYTE,0(R1)        SAVE RECORD EQUATE                           
         MVC   TGFULL,0(R1)        SAVE A(KEY TO SET)                           
         SPACE 1                                                                
         OC    KEY,KEY             DON'T BOTHER IF NOTHING IN KEY               
         BZ    SETLX                                                            
         L     R3,APTRB00                                                       
         GOTO1 AGENPTRS,DMCB,(R3)  BUILD POINTER BLOCK                          
         SPACE 1                                                                
         USING TLDRD,R3            LOOP THROUGH BLOCK MATCHING ON EQU           
SETL1A   CLC   TLDRCD,TGBYTE                                                    
         BE    SETL1C                                                           
SETL1B   LA    R3,L'TLDRREC(R3)                                                 
         B     SETL1A                                                           
         SPACE 1                                                                
         USING TLCOPD,R3                                                        
SETL1C   CLI   TGBYTE,TLCOVRDQ     IF LISTING COMMERCIAL VERSIONS               
         BNE   SETL1D                                                           
         CLC   TLCOVCOM,VRLSTCOM   IF SAME COMMERCIAL AS LAST                   
         BNE   SETL1D                                                           
         CLC   TLCOVVER,VRLSTLET   GET KEY WITH HIGHER VERSION                  
         BL    SETL1B              LETTER THAN LAST                             
         SPACE 1                                                                
         USING TLDRD,R3            LOOP THROUGH BLOCK MATCHING ON EQU           
SETL1D   XC    KEY,KEY                                                          
         BAS   RE,SETLKEY          SET RF=L'KEY FOR EXECUTED MOVE               
         LR    R4,RF               (SAVE IT)                                    
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),TLDRKEY      MOVE POINTER TO KEY                          
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         LA    R3,KEY                                                           
SETL2    CLC   TLDRDA,DMDSKADD     MATCH D/A OF ACTIVE POINTER                  
         BE    SETL4                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   TLDRKEY(0),KEYSAVE  AS LONG AS RECORD HASN'T CHANGED             
         BNE   SETL6                                                            
         GOTO1 SEQ                 TRY NEXT RECORD                              
         B     SETL2                                                            
         SPACE 1                                                                
SETL4    CLC   TLDRDA,VERYFRST     IF WE'RE NOT AT THE TOP OF THE LIST          
         BE    SETL6                                                            
         GOTO1 SEQ                 START LIST WITH NEXT                         
         SPACE 1                                                                
SETL6    CLC   TLDRCD,KEYSAVE      IF NO LONGER SAME RECORD TYPE                
         BE    *+10                                                             
         XC    KEY,KEY             CLEAR KEY                                    
         SPACE 1                                                                
SETLX    L     R1,TGFULL           SET KEY IN CALLER'S AREA                     
         MVC   0(L'TLDRKEY,R1),KEY                                              
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ROUTINE TO SET L'KEY FOR COMPARES/MOVES                    
         SPACE 1                                                                
         USING TLDRD,R3            R3=A(KEY)                                    
SETLKEY  DS    0H                                                               
         CLI   TLDRCD,TLINBCDQ     SPECIAL FOR INVOICE STATUS PTRS.             
         BNE   SETLKEY2                                                         
         LA    RF,TLDRSTAT+L'TLDRSTAT-TLDRD-1  INCL. STATUS BYTES               
         BR    RE                                                               
         SPACE 1                                                                
SETLKEY2 DS    0H                                                               
         LA    RF,L'TLDRKEY-1      ELSE SET TO FULL KEY                         
         SPACE 1                                                                
         BR    RE                  RETURN RF=LENGTH FOR EXEC. COMPARE           
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TESTS MULTIPLE FIELDS FOR ATTRIBUTES                     
         SPACE 1                                                                
*                                  P1 = A(FIRST FIELD)                          
*                                  P1 BYTE 0 = ATTRIBUTE TYPES                  
*                                     X'80' = TEST FOR EMPTY FLDS               
*                                     X'40' = TEST FOR VALIDITY BITS            
*                                     X'20' = MAKE ALL FLDS VALID               
*                                     X'10' = MAKE ALL FLDS INVALID             
*                                     X'08' = MAKE ALL FLDS PROTECTED           
*                                     X'04' = MAKE ALL FLDS UNPROTECTED         
*                                     X'02' = MAKE ALL FLDS TRANSMITTED         
*                                     X'01' = MAKE ALL FLDS CLEARED             
*                                  P2 = A(LAST FLD) OR #FLDS IF < 1000          
*                                  P2 BYTE 0 = MORE ATTRIBUTE TYPES             
*                                     X'80' = SKIP PROTECTED FLDS               
*                                     X'40' = SKIP UNPROTECTED FLDS             
*                                     X'20' = MAKE ALL FLDS HIGH INT            
*                                     X'10' = MAKE ALL FLDS NORM INT            
*                                     X'08' = MAKE ALL FLDS LOW INT             
*                                     X'04' = MAKE ALL FLDS UN-CURSORED         
*                                     X'02' = MAKE HIGH INT IF NOT              
*                                             VALID & NOT EMPTY,                
*                                             RETURN CC EQ AND                  
*                                             P3=A(1ST NON-VALID FLD)           
*                                     X'01' = P3 BEING USED ALSO                
*                                  P3 = RETURN FOR X'02' BIT IN P2              
*                                  P3 BYTE 0 = MORE ATTRIBUTE TYPES             
*                                              (SEE X'01' BIT IN P2)            
*                                     X'80' = TEST FOR NORMAL INT FLDS          
*                                     X'40' = TEST FOR EMPTY USING OC           
VFLDVAL  DS    0H                                                               
         LR    R4,R1               SAVE A(PLIST) IN R4                          
         LM    R2,R3,0(R1)                                                      
         SLL   R2,8                R2 = A(FIRST FIELD)                          
         SRL   R2,8                                                             
         SLL   R3,8                R3 = A(LAST FIELD) OR NUMBER OF FLDS         
         SRL   R3,8                                                             
         XC    9(3,R4),9(R4)       CLEAR A(FAILED FIELD)                        
*                                                                               
         TM    0(R1),X'08'+X'04'+X'01' IF MAKE PROT/UNPROT/CLEARED              
         BNZ   *+12                                                             
         TM    4(R1),X'20'+X'10'+X'08'+X'02'  OR MAKE HIGH/NORM/LOW INT         
         BZ    FV20                                                             
         OI    0(R1),X'02'         THEN MAKE ALL TRANSMITTED                    
*                                                                               
FV20     CLI   1(R2),X'FF'         IF NOP FIELD THEN SKIP                       
         BE    FV90                                                             
         TM    4(R1),X'80'         IF REQUEST IS SKIP PROTECTED FLDS            
         BZ    FV30                                                             
         TM    1(R2),X'20'         THEN SKIP PROTECTED FLDS                     
         BO    FV90                                                             
*                                                                               
FV30     TM    4(R1),X'40'         IF REQUEST IS SKIP UNPROTECTED FLDS          
         BZ    FV40                                                             
         TM    1(R2),X'20'         THEN SKIP UNPROTECTED FLDS                   
         BZ    FV90                                                             
*                                                                               
FV40     TM    0(R1),X'80'         IF REQUEST IS TEST FOR EMPTY                 
         BZ    *+12                                                             
         CLI   5(R2),0             THEN TEST USING INPUT LENGTH                 
         BNE   FVNO                EXIT IF NOT EMPTY                            
*                                                                               
         TM    4(R1),X'01'         TEST P3 BEING USED ALSO                      
         BZ    FV45                                                             
         TM    8(R1),X'40'         IF REQUEST IS TEST FOR EMPTY                 
         BZ    *+12                                                             
         BAS   RE,ANYFLD           THEN IF ANYTHING IN FIELD USING OC           
         BE    FVNO                EXIT IF NOT EMPTY                            
*                                                                               
         TM    8(R1),X'80'         IF REQUEST IS TEST FOR NORMAL INT            
         BZ    *+12                                                             
         TM    1(R2),X'0C'         THEN TEST FOR NORMAL INT                     
         BNZ   FVNO                EXIT IF HIGH OR LOW INT                      
*                                                                               
FV45     TM    0(R1),X'40'         IF REQUEST IS TEST FOR VALID                 
         BZ    FV47                                                             
         TM    4(R2),X'20'         THEN TEST FOR VALID                          
         BZ    FVNO                EXIT IF NOT VALID                            
         CLI   TWASCR,SCR55                                                     
         BE    FV46                                                             
         CLI   TWASCR,SCR5F                                                     
         BE    FV46                                                             
         CLI   TWASCR,SCR66                                                     
         BNE   FV47                                                             
FV46     TM    4(R2),X'80'                                                      
         BO    FVNO                                                             
*                                                                               
FV47     TM    0(R1),X'20'         IF REQUEST IS MAKE ALL VALID                 
         BZ    *+8                                                              
         OI    4(R2),X'20'         THEN MAKE VALID                              
*                                                                               
         TM    0(R1),X'10'         IF REQUEST IS MAKE ALL INVALID               
         BZ    *+8                                                              
         NI    4(R2),X'DF'         THEN MAKE INVALID                            
*                                                                               
         TM    0(R1),X'08'         IF REQUEST IS MAKE ALL PROTECTED             
         BZ    *+8                                                              
         OI    1(R2),X'20'         THEN MAKE PROTECTED                          
*                                                                               
         TM    0(R1),X'04'         IF REQUEST IS MAKE ALL UNPROTECTED           
         BZ    FV48                                                             
         CLI   ALTPROG,X'F3'       IF THESE ARE PAY SCREENS, SKIP               
         BE    FV47A                                                            
         CLI   TWASCR,SCR0A        AND NOT CAST LIST                            
         BE    FV47U                                                            
         CLI   TWASCR,SCR6A        AND NOT EMPLOYEE LIST                        
         BE    FV47U                                                            
         CLI   TWASCR,SCR44        OR INVOICE APPROVE                           
         BE    FV47U                                                            
         CLI   TWASCR,SCRF8        OR COM2 DISPLAY                              
         BE    FV47U                                                            
FV47A    TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
FV47U    NI    1(R2),X'DF'         THEN MAKE UNPROTECTED                        
*                                                                               
FV48     TM    0(R1),X'02'         IF REQUEST IS MAKE ALL TRANSMITTED           
         BZ    *+8                                                              
         OI    6(R2),X'80'         THEN MAKE TRANSMITTED                        
*                                                                               
         TM    0(R1),X'01'         IF REQUEST IS MAKE ALL CLEARED               
         BZ    FV50                                                             
         ZIC   RF,0(R2)            THEN MAKE ALL CLEARED                        
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
*                                                                               
FV50     TM    4(R1),X'20'         IF REQUEST IS MAKE ALL HIGH INT              
         BZ    *+12                                                             
         OI    1(R2),X'08'         THEN MAKE HIGH INTENSITY                     
         NI    1(R2),X'FB'                                                      
*                                                                               
         TM    4(R1),X'02'         IF REQ IS MAKE HIGH INT IF NOT VALID         
         BZ    FV60                AND NOT EMPTY                                
         TM    4(R2),X'20'         THEN TEST FOR NOT VALID                      
         BO    FV60                                                             
         OC    9(3,R4),9(R4)       IF NO NON-VALID FIELDS YET                   
         BNZ   *+8                                                              
         STCM  R2,7,9(R4)          SAVE A(FIRST NON-VALID FIELD)                
         CLI   5(R2),0             ALSO TEST FOR NOT EMPTY                      
         BE    FV60                                                             
         OI    1(R2),X'08'         MAKE HIGH INTENSITY                          
         NI    1(R2),X'FB'                                                      
*                                                                               
FV60     TM    4(R1),X'10'         IF REQUEST IS MAKE ALL NORM INT              
         BZ    *+8                                                              
         NI    1(R2),X'F3'         THEN MAKE NORMAL INTENSITY                   
*                                                                               
         TM    4(R1),X'08'         IF REQUEST IS MAKE ALL LOW INT               
         BZ    *+8                                                              
         OI    1(R2),X'0C'         THEN MAKE LOW INTENSITY                      
*                                                                               
         TM    4(R1),X'04'         IF REQUEST IS MAKE ALL UN-CURSORED           
         BZ    *+8                                                              
         NI    6(R2),X'BF'         THEN TURN OF INSERT CURSOR HERE BIT          
*                                                                               
FV90     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R2),0             IF END OF SCREEN THEN DONE                   
         BE    FVYES                                                            
*                                                                               
         C     R3,=F'1000'         IF R3 < 1000                                 
         BNL   FV100                                                            
         BCT   R3,FV20             THEN DECREMENT R3 AND LOOP BACK              
*                                                                               
         B     FVYES               ELSE DONE                                    
*                                                                               
FV100    CR    R2,R3               ELSE IF R2 <= R3                             
         BNH   FV20                THEN LOOP BACK                               
*                                                                               
FVYES    TM    4(R1),X'02'         IF REQ IS MAKE HIGH INT IF NOT VALID         
         BZ    YES                                                              
         OC    9(3,R4),9(R4)                                                    
         BZ    NO                  NO NON-VALID FIELDS                          
         B     YES                                                              
*                                                                               
FVNO     ST    R2,8(R4)            SOME FIELD FAILED CONDITION                  
         B     NO                  RETURN CC NE, A(FIELD HEADER)                
         EJECT                                                                  
*              ROUTINE TO READ/WRITE TEMPSTR PAGES                              
         SPACE 2                                                                
*                                  P1, BYTE  0=BIT SETTINGS/PAGE NUMBER         
*                                  P1, BYTES 1-3=READ/WRITE ADDRESS             
VGETTWA  DS    0H                                                               
         MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         LM    R2,R3,0(R1)         READ/WRITE ADDRESS                           
         SPACE 1                                                                
         XC    DMCB+20(4),DMCB+20  MAKE SURE NOTHING LEFTOVER                   
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA2                                                            
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS                    
         BNZ   GTWA2                                                            
         MVC   COMMAND(6),=C'DMREAD'                                            
         TM    BYTE,X'20'                                                       
         BZ    GTWA1                                                            
         MVC   DMCB+20(2),=C'L='   X'20'=1 IS USER DEFINED LENGTH               
         STCM  R3,3,DMCB+22                                                     
         B     GTWA2                                                            
GTWA1    TM    BYTE,X'10'          X'10'=0 IS 6144 BYTE TWAS                    
         BZ    GTWA2                                                            
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=AL2(CHKPTTWL)  X'10'=1 IS NEW MAX AT 18432           
         SPACE 1                                                                
GTWA2    NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
         SPACE 1                                                                
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
         SPACE 1                                                                
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*              ROUTINE REVERSES ALL AMOUNTS IN INVOICE OR CHECK RECORD          
         SPACE 1                                                                
VREVERSE DS    0H                                                               
         L     R3,AIO              LOOP THROUGH RECORD                          
                                                                                
       ++INCLUDE TAREVERSE                                                      
                                                                                
REVX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS PAGING INSTEAD OF GENCON                        
*              P1       A(TABLE OF KEYS FOR LIST - 16 X L'TLRCKEY)              
*              P1 BYTE0=OVERRIDE N'ENTRIES IN TABLE                             
*              P2       A(ANTICIPATED 1ST KEY FOR THIS PAGE)                    
*              P3       A(ACTUAL 1ST KEY OF THIS PAGE - SET BY PGCNTL)          
         SPACE 1                                                                
VPGCNTL  DS    0H                                                               
         LM    R2,R4,0(R1)                                                      
         ZIC   RF,0(R1)            SET N'TABLE ENTRIES                          
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,16                                                            
         MH    RF,=AL2(L'TLRCKEY)                                               
         STH   RF,TGFULL           TGFULL = SIZE OF TABLE                       
         SPACE 1                                                                
         MVC   0(L'TLRCKEY,R2),0(R3)   SET PAGING TABLE W/ANTICIPATED           
*                                        1ST KEY FOR THIS PAGE                  
         SPACE 1                                                                
         CLI   LISTSW,C'T'         IF WE JUST RETURNED FROM SELECT              
         BNE   PGC10               SET TO RE-DISPLAY SAME PAGE                  
         MVC   0(L'TLRCKEY,R4),L'TLRCKEY(R2)                                    
         B     PGC40                                                            
         SPACE 1                                                                
PGC10    CLI   LISTSW,C'F'         IF THIS IS THE BEGINING OF THE LIST          
         BNE   PGC20                                                            
         LA    RE,BLOCK            CLEAR TEMPORARY TABLE                        
         LH    RF,TGFULL                                                        
         XCEFL                                                                  
         LR    RE,R2               CLEAR PERMANENT TABLE                        
         LH    RF,TGFULL                                                        
         XCEFL                                                                  
         XC    0(L'TLRCKEY,R4),0(R4)  CLEAR CONTINUE KEY                        
         B     PGCX                                                             
         SPACE 1                                                                
PGC20    CLI   LISTSW,C'L'         IF NOT PAGING BACKWARD (FORWARD)             
         BE    PGC30                                                            
         MVC   KEY,0(R3)           THEN RESET KEY WITH LAST KEY READ            
         B     PGC50                                                            
         SPACE 1                                                                
PGC30    L     R1,FULL             SET CONTINUE KEY                             
         MVC   0(L'TLRCKEY,R4),2*L'TLRCKEY(R2)                                  
         SPACE 1                                                                
         BAS   RE,POP              POP TABLE                                    
         BAS   RE,TMPTOTAB         COPY TEMP TABLE TO PAGING TABLE              
         SPACE 1                                                                
PGC40    BAS   RE,POP              POP TABLE                                    
         BAS   RE,TMPTOTAB                                                      
         MVC   KEY(L'TLRCKEY),0(R2)   SET KEY WITH CURRENT TABLE ENTRY          
         SPACE 1                                                                
PGC50    OC    KEY(L'TLRCKEY),KEY  IF NOTHING IN KEY                            
         BZ    PGC60                  THEN LET USER SET KEY                     
         GOTO1 HIGH                ELSE REREAD TO RESET KEY                     
         SPACE 1                                                                
         CLC   KEY(1),KEYSAVE      IF END OF LIST - RESET KEY                   
         BE    PGC60                  (DIFFERENT RECORD EQUATE)                 
         XC    KEY,KEY             AND LET USER HANDLE IT                       
         SPACE 1                                                                
PGC60    DS    0H                  SET CONTINUE KEY                             
         MVC   0(L'TLRCKEY,R4),KEY                                              
         XC    BLOCK(L'TLRCKEY),BLOCK  CLEAR TOP OF TEMP TABLE                  
         LR    RE,R2                   A(TABLE)                                 
         LH    RF,TGFULL                                                        
         SH    RF,=AL2(L'TLRCKEY)      COPY MAX - 1 ENTRIES                     
         LA    R0,BLOCK+L'TLRCKEY      PUSH TABLE                               
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         SPACE 1                                                                
         BAS   RE,TMPTOTAB         COPY TEMP TABLE TO PAGING TABLE              
         SPACE 1                                                                
PGCX     B     XIT                                                              
         EJECT                                                                  
*        POP TABLE FOR PAGING                                                   
*            R2 = A(PAGING TABLE)                                               
         SPACE 1                                                                
POP      NTR1                                                                   
         LA    R0,BLOCK            POP TEMP TABLE 1 X                           
         LH    R1,TGFULL           TEMP TABLE <= TABLE                          
         LA    R2,L'TLRCKEY(R2)                                                 
         LH    R3,TGFULL                                                        
         SH    R3,=AL2(L'TLRCKEY)  COPY MAX - 1 ENTRIES                         
         MVCL  R0,R2                                                            
         B     XIT                                                              
         SPACE 2                                                                
*        COPY TEMP TABLE TO PAGING TABLE                                        
*            R2 = A(PAGING TABLE)                                               
*            R4 = TEMP TABLE                                                    
         SPACE 1                                                                
TMPTOTAB NTR1                                                                   
         LH    R3,TGFULL           MOVE TO PAGING TABLE                         
         LA    R0,BLOCK            FROM TEMP TABLE                              
         LR    R1,R3                                                            
         MVCL  R2,R0                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE KEEPS RUNNING TOTALS                                     
*              P1       =A(TOTALS OF THIS PAGE)                                 
*              P1 BYTE 0=IF LAST PAGE - C'E'                                    
*                        ELSE         - X'0'                                    
         SPACE 1                                                                
VTOTCNTL DS    0H                                                               
         L     R2,0(R1)            A(THIS PAGES TOTALS)                         
         CLI   LISTSW,C'F'         IF STARTING FROM THE BEGIINING               
         BNE   CT10                                                             
         XC    TOTTAB,TOTTAB       THEN CLEAR TABLE                             
         B     CT30                AND GO ACCUMULATE TOTALS                     
         SPACE                                                                  
CT10     CLI   LISTSW,C'T'         IF WE JUST RETURNED FROM SELECT              
         BNE   CT20                                                             
         BAS   RE,POPTOT           POP TOTAL TABLE 1 X                          
         B     CT30                AND GO ACCUMULATE TOTALS                     
         SPACE                                                                  
CT20     CLI   LISTSW,C'L'         IF PAGING BACKWARD                           
         BNE   CT30                                                             
         BAS   RE,POPTOT           POP TOTAL TABLE 2 X                          
         BAS   RE,POPTOT                                                        
         SPACE                                                                  
CT30     BAS   RE,TACCUM           ACCUMULATE TOTALS                            
         SPACE                                                                  
CT40     XC    BLOCK(256),BLOCK    CLEAR TEMP TABLE                             
         MVC   BLOCK+16(240),TOTTAB                                             
         MVC   TOTTAB,BLOCK        RESTORE 'PUSHED' TABLE                       
         CLI   0(R1),C'E'          IF THIS IS THE LAST PAGE OF LIST             
         BE    CTEXIT              INSERT 0 IN TABLE                            
         MVC   TOTTAB(16),0(R2)    & SET THIS RUNNING TOTAL IN TABLE            
         SPACE                                                                  
CTEXIT   B     XIT                                                              
         SPACE 1                                                                
*              POP TOTAL TABLE 1 X                                              
*                                                                               
POPTOT   NTR1                                                                   
         MVC   BLOCK(256),TOTTAB   COPY TABLE TO TEMP TABLE                     
         XC    TOTTAB,TOTTAB       CLEAR ACTUAL TABLE                           
         MVC   TOTTAB(240),BLOCK+16   AND MOVE BACK TABLE                       
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE ACCUMULATES TOTALS (4 FULL WORDS)                        
*              R2    SINGLE PAGES TOTALS                                        
*                                                                               
TACCUM   NTR1                                                                   
         LA    R1,4                N'ENTRIES IN TABLE                           
         LA    R4,TOTTAB           TABLE OF TOTALS                              
         SPACE                                                                  
TA10     L     R0,0(R2)            TOTAL FROM THIS PAGE                         
         A     R0,0(R4)            + TOTAL FROM PREVIOUS PAGE                   
         ST    R0,0(R2)            = ACCUMULATED TOTAL                          
         LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R1,TA10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS ACCESS TO AGENCY AND COMMERCIAL BY                
*              READING COMMERCIAL CLIENT GROUP PASSIVE POINTER.                 
*              AGENCY INPUT OF CLI GRP OR AGENCY IN CLI GRP ALLOWED             
*                                  XIT - REAL AGENCY DSPLY'D IN AGY FLD         
*                                      & CC SET, <> MEANS SKIP AGY LIM          
         SPACE 1                                                                
VCHKCLG  DS    0H                                                               
         LM    R2,R3,0(R1)         R2=A(AGENCY FIELD),R3=A(COMML FIELD)         
         XR    R4,R4               SET TO ENFORCE AGENCY LIMIT RESTRICT         
         OC    TGCLGACC,TGCLGACC   IF CLIENT GROUP ACCESS DEFINED               
         BZ    CHKCLGX                                                          
         LA    R4,1                SET TO SKIP AGENCY LIMIT RESTRICT            
         CLI   5(R2),0             IF AGENCY INPUT                              
         BE    CHKCLGX                                                          
         OC    8(L'TGAGY,R2),SPACES                                             
         CLC   8(L'TGAGY,R2),TGCLGACC AND IT IS THE CLIENT GROUP                
         BNE   CHKCLGX                                                          
         MVC   TGCLG,TGCLGACC                                                   
         NI    4(R3),X'DF'         TURN OFF PREV. VALIDATED COMML FLD           
         GOTO1 RECVAL,DMCB,TLCOGCDQ,(X'02',(R3)) READ COMML CLI GRP             
         BNE   THEEND                                                           
         LA    R3,KEY              DISLAY REAL AGENCY TO SCRN                   
         MVC   8(L'TGAGY,R2),TLCOGAGY-TLCOPD(R3)                                
         MVI   5(R2),L'TGAGY                                                    
         NI    4(R2),X'DF'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
CHKCLGX  LTR   R4,R4               RETURN CONDITION CODE                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT GUARANTEE TRACKING REQUESTS                   
         SPACE                                                                  
*                                  P1 BYTE 0 = N'SUB ELEMENTS IN TAGQEL         
*                                  P1 BYTES 1-3 = A(FIRST SUB-ELEMENT)          
         SPACE                                                                  
         USING REQD,R3                                                          
         USING TAGQSBEL,R4                                                      
VGQEXT   DS    0H                                                               
         ZIC   R2,0(R1)            R2=N'SUB ELEMENTS                            
         L     R4,0(R1)            R4=A(FIRST SUB-ELEMENT)                      
         LA    R3,BLOCK            R3=A(REQUEST RECORD)                         
         SPACE 1                                                                
GQ2      XC    REQUEST,REQUEST     CLEAR REQUEST CARD                           
         SPACE 1                                                                
         MVC   TGOFF,TAGQOFF       SET TP OFFICE                                
         SPACE 1                                                                
         L     R1,TAGQSSN          CVT SSN TO EBCDIC                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQUEST+56(9),DUB+3(5)                                           
         SPACE 1                                                                
         MVI   REQUEST+32,16+8     PRETEND THERE'S A FIELD HEADER HERE          
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',REQUEST+56),REQUEST+32                
         BE    *+6                                                              
         DC    H'0'                NO W4 RECORD                                 
         SPACE 1                                                                
         MVC   REQUEST(40),GREQCARD  MOVE IN BEGINNING OF REQUEST CARD          
         MVI   REQUEST+77,C'*'     END OF CARD MARKER                           
         SPACE 1                                                                
         LH    R1,TAGQGUA          CVT GUAR CODE TO EBCDIC                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQUEST+65(4),DUB+6(2)                                           
         SPACE 1                                                                
         OC    TAGQSTRT,TAGQSTRT   IF CYCLE START DEFINED                       
         BZ    GQ5                                                              
         GOTO1 DATCON,DMCB,(1,TAGQSTRT),(8,REQUEST+69) PYMT CYCLE START         
         SPACE 1                                                                
GQ5      BAS   RE,ADDREQ           ADD REQUEST TO REQUEST FILE                  
         SPACE 1                                                                
         LA    R4,L'TAGQSBEL(R4)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R2,GQ2                                                           
         SPACE 1                                                                
GQX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD A REQUEST TO REQUEST FILE                         
         USING REQD,R3                R3=A(REQUEST RECORD)                      
         SPACE 1                                                                
ADDREQ   NTR1                                                                   
         XC    REQHDR,REQHDR          CLEAR HEADER                              
*                                     BUILD SORT AREA AT BEG. OF CARD           
         MVC   REQUEST+2(2),TGCTALPH  CONNECT ALPHA USER ID                     
         MVC   REQUEST+4(1),TGOFF     TP OFFICE                                 
         MVC   REQUEST+5(6),TGAGY     AGENCY                                    
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMADD '),REQFILE,(R3),(R3)                    
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*        ROUTINE TO CONTROL CALLS TO TSAR, RETURNS CONDITION CODE               
*              MYTSACTN=ACTION, MYTSPAGL=LOW TEMPSTR/TEMPEST PAGE #             
*              (TSAR DEFAULTS TO 1 DURING INIT), MYTSRNUM=RECORD NUMBER         
*              MYTSPAGN=# OF PAGES(DEFAULT=1), MYTSRECI=REC INDICATOR           
*              MYTSKEYL=KEY LENGTH, MYTSRECL=REC LEN OR MAX IF VARIABLE         
*              P1 = A(RECORD) OR 0 TO INDICATE END OF TSAR USE                  
         SPACE                                                                  
         USING TSARD,R2                                                         
VTSARCTL DS    0H                                                               
         L     R3,0(R1)              R3=A(RECORD)                               
         LTR   R3,R3                 IF 0, THEN WE'RE DONE WITH TSAR            
         BNZ   TSARC2                                                           
         MVI   CALLSP,0              ENABLE PUSH/POP                            
         NI    PRGSTAT,ALL-INITSARD  SET TSAR NO LONGER INITIALIZED             
         B     YES                                                              
         SPACE                                                                  
TSARC2   MVI   CALLSP,CALLSOFF     ENSURE PUSH/POP IS DISABLED                  
*                                  (TSAR USES TEMPSTR PAGES 1-3)                
         LA    R2,TSARBLK          R2=A(PARAM BLOCK=TSARBLK)                    
         CLI   MYTSACTN,TSAINI     IF ACTION NOT INITIALIZE                     
         BE    TSARC7                                                           
         TM    PRGSTAT,INITSARD    AND TSAR NOT INITIALIZED YET                 
         BO    TSARC3                                                           
         MVI   TSACTN,TSAINI       THEN INIT FIRST                              
         BRAS  RE,CALLTSAR                                                      
         B     TSARC7                                                           
         SPACE                                                                  
TSARC3   CLI   MYTSACTN,TSARES     IF ACTION NOT RESTORE                        
         BE    TSARC7                                                           
         TM    TRNSTAT2,RESTSARD   AND TSAR BUFFER NOT RESTORED YET             
         BO    TSARC7                                                           
         MVI   TSACTN,TSARES       THEN RESTORE FIRST                           
         BRAS  RE,CALLTSAR                                                      
*                                                                               
TSARC7   MVC   TSACTN,MYTSACTN                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         TM    TSERRS,TSEDUP       DIE IF DUPLICATE KEY ON ADD                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSACTN,TSAWRT       IF ACTION WAS WRITE                          
         BE    *+12                                                             
         CLI   TSACTN,TSADEL       OR DELETE A RECORD                           
         BNE   TSARC10                                                          
         TM    TSERRS,TSERNF       DIE IF RECORD NOT FOUND                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
TSARC10  CLI   TSERRS,0            TEST IF ANY ERRORS AND SET COND CODE         
         B     XIT                 BEFORE LEAVING                               
         EJECT                                                                  
*              GLOBAL EXIT ROUTINES                                             
         SPACE 1                                                                
*                                  P1, BYTE 0 - X'80'=RETURN ERRORS             
*                                  R2=A(FIELD HEADER)                           
VEXIT    DS    0H                                                               
         TM    GENSTAT2,USGETTXT   CALL FOR GETTXT                              
         BZ    EXT2                                                             
         XC    GETTXTCB,GETTXTCB   DEFINE CONTROL BLOCK                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         CLI   MYMTYP,0            IF NOT DEFINED                               
         BNE   *+8                                                              
         MVI   MYMTYP,GTMINF       SET INFORMATION TYPE                         
         MVC   GTMTYP,MYMTYP       SET MESSAGE TYPE                             
         MVC   GTMSGNO,MYMSGNO     AND MESSAGE NUMBER                           
         MVC   GTMSYS,GETMSYS      AND MESSAGE SYSTEM                           
         CLI   MYMSYS,0            IF OVERRIDE SYSTEM AROUND                    
         BE    *+10                                                             
         MVC   GTMSYS,MYMSYS       USE IT                                       
         LA    RE,BLOCK            IN CASE IT'S DEFINED                         
         STCM  RE,7,GTASUBST       SET A(SUBSTITUTION BLOCK)                    
*                                                                               
EXT2     LR    RE,R2               COPY R2                                      
         N     RE,=X'00FFFFFF'     STRIP OFF HIGH-ORDER BYTE                    
         CR    RE,RA               IF R2 IS NOT POINTING PAST TWA               
         BL    EXT3                                                             
         LR    RF,RA                                                            
         AHI   RF,TWA018K                                                       
         CR    RE,RF                                                            
         BL    *+8                                                              
*                                                                               
EXT3     LA    R2,CONRECH          RESET IT TO RECORD FIELD                     
*                                                                               
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         CLI   ERRDISP,0           DO I NEED TO OVERRIDE CURSOR POS.            
         BE    EXT4                                                             
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP    DISPLACMENT INTO FIELD                       
         SPACE 1                                                                
EXT4     TM    0(R1),X'80'         CALLER WANTS CONTROL BACK                    
         BO    XIT                                                              
         TM    GENSTAT2,USGETTXT   CALL FOR GETTXT                              
         BO    EXT5                                                             
         CLI   ERROR,X'FE'         APPLIC. DISPLAYED ERRORS                     
         BNE   EXT5                                                             
         GOTO1 ERREX2              PUT OUT MY ERROR/MESSAGE                     
         SPACE 1                                                                
EXT5     GOTO1 ERREX               PUT OUT SYSTEM MESSAGE                       
         EJECT                                                                  
***********************************************************************         
*        ROUTINE NOTIFIES VITA                                        *         
*        ON ENTRY ... P1 BYTE 0 = X'80' NOTIFY ONE ENVIRONMENT        *         
*                     P1        = A(MQ MESSAGE)                       *         
*                     P2        = L'MQ MESSAGE                        *         
***********************************************************************         
                                                                                
VNTFYVIT DS    0H                                                               
         BRAS  RE,NVITA                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ELIMINATE XML-UNFRIENDLY CHARACTERS                          *         
***********************************************************************         
                                                                                
VELIMCHR DS    0H                                                               
         L     R2,0(R1)            R2=A(FIELD)                                  
         ZIC   R3,0(R1)            R3=L'FIELD                                   
                                                                                
         LR    RE,R3               PAD WITH SPACES                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         OC    0(0,R2),SPACES                                                   
                                                                                
EC10     CLI   0(R2),C'"'          REPLACE DOUBLE QUOTES                        
         BNE   EC20                WITH SINGLE QUOTES                           
         MVI   0(R2),X'7D'                                                      
                                                                                
EC20     LA    R2,1(R2)                                                         
         BCT   R3,EC10                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE REC/ACT FOR PAYROLL PLUS AGENCIES                   *         
*        *** NOTE: MAKE SURE TLAY RECORD TGAYSTA7 IS SET ***          *         
*              P1 = ZERO, CHECK TABLE 1                               *         
*                 = NOT ZERO, CHECK TABLE 2                           *         
*              CC = EQUAL, REC / ACT IS VALID                         *         
*              CC = NEQ, REC / ACT IS INVALID                         *         
***********************************************************************         
                                                                                
VRAPPLSA DS    0H                                                               
         OC    TGAGY,TGAGY         MUST HAVE AGENCY SET AND READ                
         JZ    YES                                                              
         TM    TGAYSTA7,TAAYSPPL   MUST BE PAYROLL PLUS AGENCY                  
         JZ    YES                                                              
         OC    DMCB,DMCB                                                        
         JNZ   VTMPP200                                                         
                                                                                
         LA    R2,NPP1TAB          REC / ACT INVALID FOR P+                     
VTMPP100 CLI   0(R2),X'FF'         DIDN'T FIND ANY                              
         JE    YES                                                              
         CLC   RECNUM,0(R2)                                                     
         JNE   VTMPP150                                                         
         CLC   ACTNUM,1(R2)                                                     
         JE    NO                                                               
VTMPP150 AHI   R2,2                                                             
         J     VTMPP100                                                         
                                                                                
VTMPP200 LA    R2,NPP2TAB          ELSE CHECK THIS LIST TOO                     
VTMPP210 CLI   0(R2),X'FF'         DIDN'T FIND ANY                              
         JE    YES                 OK FOR P+ AGENCY TO USE                      
         CLC   RECNUM,0(R2)                                                     
         JNE   VTMPP250                                                         
         CLC   ACTNUM,1(R2)                                                     
         JE    NO                                                               
VTMPP250 AHI   R2,2                                                             
         J     VTMPP210                                                         
         EJECT                                                                  
*              LOCAL EXIT/ERROR ROUTINES                                        
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
PFLDMISS MVI   ERROR,MISSING       MISSING INPUT FIELD FOR PUSH                 
         B     PFERRALL                                                         
HELPERR  MVI   ERROR,ERINVHLP      INVALID HELP REQUEST                         
         B     PFERRALL                                                         
CANTPUSH MVI   ERROR,ERNOPUSH      PUSH ERROR - TOO MANY NEST LEVELS            
         B     PFERRALL                                                         
FLDPFERR MVI   ERROR,ERINVPFF      INVALID PK KEY FOR THIS FIELD                
         B     PFERRALL                                                         
PFERR    MVI   ERROR,ERINVPFK      INVALID PF KEY                               
PFERRALL BAS   RE,REPROT           REPROTECT RECORD/ACTION FLDS IF NEC.         
RETCURS  LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         B     THEEND                                                           
CHKLOCK  MVI   ERROR,ERCKLOCK      URGENT CHECK RUN LOCKOUT IN EFFECT           
         L     R2,EFHACT           CURSOR TO ACTION FIELD                       
         B     THEEND                                                           
         SPACE 1                                                                
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER FIELDS AS REQUIRED              
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         L     R2,AFRSTKEY         R2 TO 1ST KEY FIELD                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         NI    TIOBINDS,X'FF'-TIOBALRM TURN OFF BEEP                            
         B     THEEND                                                           
         SPACE 1                                                                
PFLOADED MVI   MYMSGNO1,1          PFK SCREEN LOADED                            
         B     *+8                                                              
PYLOADED MVI   MYMSGNO1,2          ENTER USE CODE OR "?"                        
         OI    GENSTAT2,USGETTXT                                                
         B     RECEND                                                           
         SPACE 1                                                                
DUMMYERR MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
DUMMYXIT MVI   ERROR,X'FE'         SET DUMMY ERROR - WE SET MESSAGE             
         B     RECEND                                                           
         SPACE 1                                                                
SELFIRST MVI   MYMSGNO1,10         SELECT OR HIT ENTER FOR FIRST                
         B     *+8                                                              
SELNEXT  MVI   MYMSGNO1,9          SELECT OF HIT ENTER FOR NEXT                 
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         MVI   OKNO,0              CLEAR OKNO SO WON'T LOOP ENDLESSLY           
         L     R2,AFRSTREC         R2 TO 1ST SEL FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
RECEND   LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
THEEND   GOTO1 EXIT,DMCB,0                                                      
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, TABLES, ETC.                                          
         SPACE 1                                                                
HEXFFS   DC    (L'TGGUA)X'FF'                                                   
GREQCARD DC    CL80'GTXXOAGENCY 0203GTX 0303REP 0503DDS 0637'                   
* REMAINDER OF REQUEST CARD IS:  NNNNNNNNNNNNNNNNSSSSSSSSSGGGGDDDDDDDD*         
         SPACE 3                                                                
LOCKRECS DC    AL1(AC,SY,SC,CR,IN,HI,LN,DU,GT,GU,AD,CH,SS,TA,PBA,DR)            
         DC    AL1(W4,GN,0)                                                     
LOCKACTS DC    AL1(ACTCHA,ACTDEL,ACTAPP,ACTUNAPP,ACTREOP,ACTCAN)                
         DC    AL1(ACTREL,ACTSEL,ACTREBLD,ACTVOID,ACTISS,ACTREIS)               
         DC    AL1(ACTREF,ACTTRAN,ACTPULL,ACTSTOP,0)                            
         SPACE 3                                                                
SYSVCON  DS    0V                                                               
         DC    A(0)                                                             
         DC    V(TINVCON)                                                       
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
CORETAB  DS    0X                  CORE-RESIDENT PHASE NUMBERS                  
         DC    AL1(QGENCON,QTASYSIO,QTASYSVL,QTASYSTB,QTASYSCA)                 
         DC    AL1(QTASYSES,QTSAR,QFALINK,QTRPACK)                              
NCORES   EQU   *-CORETAB                                                        
         SPACE 3                                                                
RCRDTAB  DS    0CL4                RECORD CODE/EQUATE/GLOBAL DATA               
         DC    AL1(TLANCDQ,AN,TGAGT-TGD,L'TGAGT-1)                              
         DC    AL1(TLAGCDQ,AG,TGAGG-TGD,L'TGAGG-1)                              
         DC    AL1(TLAYCDQ,AY,TGAGY-TGD,L'TGAGY-1)                              
         DC    AL1(TLATCDQ,AT,TGATT-TGD,L'TGATT-1)                              
         DC    AL1(TLCGCDQ,CG,TGCLG-TGD,L'TGCLG-1)                              
         DC    AL1(TLCLCDQ,CL,TGCLI-TGD,L'TGCLI-1)                              
         DC    AL1(TLCOCDQ,CO,TGCID-TGD,L'TGCID-1)                              
         DC    AL1(TLDUCDQ,DU,TGDUC-TGD,L'TGDUC-1)                              
         DC    AL1(TLEMCDQ,EM,TGEMP-TGD,L'TGEMP-1)                              
         DC    AL1(TLEPCDQ,EP,TGEPI-TGD,L'TGEPI-1)                              
         DC    AL1(TLINCDQ,HI,TGINV-TGD,L'TGINV-1)                              
         DC    AL1(TLLNCDQ,LN,TGLIN-TGD,L'TGLIN-1)                              
         DC    AL1(TLPGCDQ,PG,TGPRG-TGD,L'TGPRG-1)                              
         DC    AL1(TLPRCDQ,PR,TGPRD-TGD,L'TGPRD-1)                              
         DC    AL1(TLSTCDQ,ST,TGSTAF-TGD,L'TGSTAF-1)                            
         DC    AL1(TLW4CDQ,W4,TGSSN-TGD,L'TGSSN-1)                              
         DC    AL1(TLLOCDQ,UN,TGUNI-TGD,L'TGUNI-1)                              
         DC    AL1(TLGUCDQ,GU,TGGUA-TGD,L'TGGUA-1)                              
         DC    AL1(TLOFCDQ,OF,TGOFF-TGD,L'TGOFF-1)                              
         DC    AL1(TLMUCDQ,MU,TGMUS-TGD,L'TGMUS-1)                              
         DC    AL1(TLARCDQ,AR,TGAREA-TGD,L'TGAREA-1)                            
         DC    AL1(TLUSCDQ,US,TGUSE-TGD,L'TGUSE-1)                              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        REC/ACT NOT FOR PRODUCTION PLUS                              *         
***********************************************************************         
NPP1TAB  DS    0CL2                RECORD CODE/ACTION CODE                      
         DC    AL1(DV,ACTADD)      ADVICE / ADD                                 
******** DC    AL1(DV,ACTCHA)      ADVICE / CHANGE                              
******** DC    AL1(DV,ACTCOMP)     ADVICE / COMPLETE                            
******** DC    AL1(DV,ACTCOPY)     ADVICE / COPY                                
******** DC    AL1(DV,ACTDEL)      ADVICE / DEL                                 
         DC    AL1(DV,ACTDIS)      ADVICE / DISPLAY                             
         DC    AL1(DV,ACTREP)      ADVICE / REPORT                              
******** DC    AL1(DV,ACTVER)      ADVICE / VERIFY                              
         DC    AL1(AF,ACTLIST)     AFMCON / LIST                                
         DC    AL1(AK,ACTADD)      ALIAS / ADD                                  
******** DC    AL1(AK,ACTDEL)      ALIAS / DEL                                  
         DC    AL1(AK,ACTDIS)      ALIAS / DIS                                  
         DC    AL1(AK,ACTLIST)     ALIAS / LIST                                 
******** DC    AL1(CA,ACTCOPY)     CAST / COPY                                  
******** DC    AL1(CA,ACTLDEL)     CAST / LDELETE                               
******** DC    AL1(CA,ACTLIST)     CAST / LIST                                  
******** DC    AL1(CA,ACTVER)      CAST / VERIFY                                
******** DC    AL1(CS,ACTCHA)      CEPISODE / CHANGE                            
         DC    AL1(CS,ACTDIS)      CEPISODE / DISPLAY                           
         DC    AL1(CI,ACTNEW)      CID / NEW                                    
******** DC    AL1(CM,ACTADD)      COMMENT / ADD                                
******** DC    AL1(CM,ACTCHA)      COMMENT / CHANGE                             
******** DC    AL1(CM,ACTDEL)      COMMENT / DEL                                
******** DC    AL1(CM,ACTDIS)      COMMENT / DISPLAY                            
         DC    AL1(CO,ACTADD)      COMMERCL / ADD                               
******** DC    AL1(CO,ACTCHA)      COMMERCL / CHANGE                            
         DC    AL1(CO,ACTCOPY)     COMMERCL / COPY                              
******** DC    AL1(CO,ACTDEL)      COMMERCL / DEL                               
******** DC    AL1(CO,ACTDIS)      COMMERCL / DISPLAY                           
******** DC    AL1(CO,ACTLIST)     COMMERCL / LIST                              
******** DC    AL1(CO,ACTREP)      COMMERCL / REPORT                            
******** DC    AL1(C2,ACTCHA)      COM2 / CHANGE                                
******** DC    AL1(C2,ACTCOPY)     COM2 / COPY                                  
         DC    AL1(C2,ACTDIS)      COM2 / DISPLAY                               
         DC    AL1(ON,ACTADD)      CONTRACT / ADD                               
         DC    AL1(ON,ACTASSGN)    CONTRACT / ASSIGN                            
******** DC    AL1(ON,ACTCHA)      CONTRACT / CHANGE                            
******** DC    AL1(ON,ACTDEL)      CONTRACT / DEL                               
         DC    AL1(ON,ACTDIS)      CONTRACT / DISPLAY                           
         DC    AL1(ON,ACTLIST)     CONTRACT / LIST                              
         DC    AL1(ON,ACTUASGN)    CONTRACT / UNASGN                            
******** DC    AL1(CY,ACTCHA)      CYCLE / CHANGE                               
         DC    AL1(CY,ACTDIS)      CYCLE / DISPLAY                              
         DC    AL1(DR,ACTADD)      DLETTER / ADD                                
******** DC    AL1(DR,ACTCHA)      DLETTER / CHANGE                             
******** DC    AL1(DR,ACTDEL)      DLETTER / DEL                                
         DC    AL1(DR,ACTDIS)      DLETTER / DISPLAY                            
         DC    AL1(DR,ACTREP)      DLETTER / REPORT                             
         DC    AL1(EC,ACTLIST)     ECAST / LIST                                 
         DC    AL1(EP,ACTADD)      EPISODE / ADD                                
******** DC    AL1(EP,ACTCHA)      EPISODE / CHANGE                             
******** DC    AL1(EP,ACTDEL)      EPISODE / DEL                                
         DC    AL1(EP,ACTDIS)      EPISODE / DISPLAY                            
         DC    AL1(EP,ACTLIST)     EPISODE / LIST                               
         DC    AL1(ER,ACTADD)      ERADIO / ADD                                 
******** DC    AL1(ER,ACTCHA)      ERADIO / CHANGE                              
******** DC    AL1(ER,ACTCOPY)     ERADIO / COPY                                
         DC    AL1(ER,ACTDEL)      ERADIO / DEL                                 
******** DC    AL1(ER,ACTDIS)      ERADIO / DISPLAY                             
******** DC    AL1(ER,ACTLIST)     ERADIO / LIST                                
******** DC    AL1(ER,ACTREP)      ERADIO / REPORT                              
         DC    AL1(ES,ACTADD)      ESTIMATE / ADD                               
******** DC    AL1(ES,ACTCHA)      ESTIMATE / CHANGE                            
         DC    AL1(ES,ACTCOPY)     ESTIMATE / COPY                              
******** DC    AL1(ES,ACTDEL)      ESTIMATE / DEL                               
         DC    AL1(ES,ACTDIS)      ESTIMATE / DISPLAY                           
         DC    AL1(ES,ACTLIST)     ESTIMATE / LIST                              
         DC    AL1(ES,ACTREBLD)    ESTIMATE / REBUILD                           
         DC    AL1(ES,ACTREP)      ESTIMATE / REPORT                            
         DC    AL1(ET,ACTADD)      ETV / ADD                                    
******** DC    AL1(ET,ACTCHA)      ETV / CHANGE                                 
         DC    AL1(ET,ACTCOPY)     ETV / COPY                                   
******** DC    AL1(ET,ACTDEL)      ETV / DEL                                    
         DC    AL1(ET,ACTDIS)      ETV / DISPLAY                                
         DC    AL1(ET,ACTLIST)     ETV / LIST                                   
         DC    AL1(ET,ACTREP)      ETV / REPORT                                 
         DC    AL1(HO,ACTCOPY)     HOLD / COPY                                  
******** DC    AL1(HO,ACTDEL)      HOLD / DEL                                   
         DC    AL1(HO,ACTDIS)      HOLD / DISPLAY                               
         DC    AL1(HO,ACTLIST)     HOLD / LIST                                  
         DC    AL1(HO,ACTMTCH)     HOLD / MATCH                                 
         DC    AL1(HO,ACTMOVE)     HOLD / MOVE                                  
         DC    AL1(HO,ACTREP)      HOLD / REPORT                                
         DC    AL1(HO,ACTUMTCH)    HOLD / UNMATCH                               
         DC    AL1(IS,ACTCHA)      ISPLIT / CHANGE                              
         DC    AL1(PCA,ACTLIST)    PCAST / LIST                                 
         DC    AL1(PCO,ACTADD)     PCOMML / ADD                                 
******** DC    AL1(PCO,ACTCHA)     PCOMML / CHANGE                              
******** DC    AL1(PCO,ACTDEL)     PCOMML / DEL                                 
         DC    AL1(PCO,ACTDIS)     PCOMML / DISPLAY                             
         DC    AL1(PCO,ACTLIST)    PCOMML / LIST                                
         DC    AL1(PC,ACTTRAN)     PERCYCLE / TRANSFER                          
         DC    AL1(SO,ACTLIST)     SCAST / LIST                                 
******** DC    AL1(SO,ACTLDEL)     SCAST / LDELETE                              
******** DC    AL1(SO,ACTPDEL)     SCAST / PDELETE                              
******** DC    AL1(SP,ACTCHA)      SEPISODE / CHANGE                            
         DC    AL1(SP,ACTDIS)      SEPISODE / DISPLAY                           
         DC    AL1(SG,ACTADD)      SGRT / ADD                                   
******** DC    AL1(SG,ACTCHA)      SGRT / CHANGE                                
******** DC    AL1(SG,ACTDEL)      SGRT / DEL                                   
         DC    AL1(SG,ACTDIS)      SGRT / DISPLAY                               
         DC    AL1(SG,ACTLIST)     SGRT / LIST                                  
         DC    AL1(SG,ACTREP)      SGRT / REPORT                                
         DC    AL1(EO,ACTTRAN)     STEREO / TRANSFER                            
         DC    AL1(TM,ACTADD)      TIME / ADD                                   
******** DC    AL1(TM,ACTCHA)      TIME / CHANGE                                
******** DC    AL1(TM,ACTDEL)      TIME / DEL                                   
         DC    AL1(TM,ACTDIS)      TIME / DISPLAY                               
         DC    AL1(TM,ACTLIST)     TIME / LIST                                  
******** DC    AL1(VC,ACTCHA)      VCAST / CHANGE                               
         DC    AL1(VC,ACTLIST)     VCAST / LIST                                 
******** DC    AL1(VR,ACTCHA)      VERSION / CHANGE                             
         DC    AL1(VR,ACTDIS)      VERSION / DISPLAY                            
         DC    X'FFFF'                                                          
                                                                                
NPP2TAB  DS    0CL2                RECORD CODE/ACTION CODE                      
         DC    AL1(DV,ACTCOPY)     ADVICE / COPY                                
         DC    AL1(DV,ACTLIST)     ADVICE / LIST                                
         DC    AL1(CN,ACTLIST)     CAGENT / LIST                                
         DC    AL1(CA,ACTCOPY)     CAST / COPY                                  
         DC    AL1(CC,ACTLIST)     CCOMM / LIST                                 
         DC    AL1(CS,ACTLIST)     CEPISODE / LIST                              
         DC    AL1(CO,ACTCOPY)     COMMERCL / COPY                              
         DC    AL1(EH,ACTLIST)     ECHECK / LIST                                
         DC    AL1(GN,ACTADD)      GCON / ADD                                   
******** DC    AL1(GN,ACTCHA)      GCON / CHANGE                                
         DC    AL1(GN,ACTSEL)      GCON / SELECT                                
         DC    AL1(GN,ACTLIST)     GCON / LIST                                  
******** DC    AL1(GU,ACTADD)      GRT / ADD                                    
******** DC    AL1(GU,ACTCHA)      GRT / CHANGE                                 
******** DC    AL1(GU,ACTSEL)      GRT / SELECT                                 
******** DC    AL1(GU,ACTLIST)     GRT / LIST                                   
         DC    AL1(PN,ACTLIST)     PAGENT / LIST                                
         DC    AL1(PC,ACTTRAN)     PERCYCLE / TRANSFER                          
         DC    AL1(MU,ACTADD)      PMUSIC / ALL                                 
******** DC    AL1(MU,ACTCHA)      PMUSIC / CHANGE                              
         DC    AL1(MU,ACTSEL)      PMUSIC / SELECT                              
******** DC    AL1(MU,ACTDEL)      PMUSIC / DELETE                              
         DC    AL1(MU,ACTDIS)      PMUSIC / DISPLAY                             
         DC    AL1(MU,ACTLIST)     PMUSIC / LIST                                
         DC    X'FFFF'                                                          
         LTORG                                                                  
         EJECT                                                                  
*              TABLE TO COVER GLOBAL PFKEYS                                     
         SPACE 1                                                                
PFTAB    DS    0H                                                               
         DC    AL1(PF2X-*,2,PFTMAIN,0,0)                                        
         DC    CL3'   ',CL8'        ',CL8'CHANGE'                               
PF2X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF3X-*,3,0,0,0)                                              
         DC    CL3'   ',CL8'W4      ',CL8'DISPLAY'                              
PF3X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF4X-*,4,PFTCPROG+PFTPUSH,0,0)                               
         DC    CL3'   ',CL8'        ',CL8'       '                              
PF4X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF5X-*,5,PFTRPROG,0,0)                                       
         DC    CL3'   ',CL8'        ',CL8'       '                              
PF5X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF6X-*,6,PFTGPFK,0,0)                                        
         DC    CL3'   ',CL8'INVOICE ',CL8'APPROVE'                              
PF6X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF6PX-*,6,0,(PF6PX-PF6P)/KEYLNQ,0)                           
         DC    CL3'   ',CL8'RECORD  ',CL8'DISPLAY'                              
PF6P     DC    AL1(KEYTYGLB,L'TGDSKADD-1),AL2(TGDSKADD-TGD)                     
PF6PX    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF7X-*,7,PFTLIST,0,0)                                        
         DC    CL3'   ',CL8'        ',CL8'LAST'                                 
PF7X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF8X-*,8,PFTLIST,0,0)                                        
         DC    CL3'   ',CL8'        ',CL8'FIRST'                                
PF8X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF9X-*,9,0,0,PFTCOML)                                        
         DC    CL3'   ',CL8'COMM    ',CL8'DISPLAY'                              
PF9X     EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF10X-*,10,0,0,PFTCOML)                                      
         DC    CL3'   ',CL8'CAST    ',CL8'LIST'                                 
PF10X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF11X-*,11,0,0,PFTCOML)                                      
         DC    CL3'   ',CL8'HISTORY ',CL8'LIST'                                 
PF11X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF12X-*,12,PFTPAY,0,0)                                       
         DC    CL3'   ',CL8'        ',CL8'      '                               
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TALURACT                                                       
*              ROUTINE TO LOAD GLOBAL PFKEY SCREEN                              
         SPACE 1                                                                
LOADSCRN NTR1  BASE=*,LABEL=*                                                   
         CLC   TWASCR,OVERLAY      TEST SCREEN ALREADY LOADED                   
         JE    XIT                                                              
         L     R3,EFHTAG                                                        
         GOTO1 LOADSOPH,DMCB,1     LOAD THE SCREEN                              
         MVC   TWASCR,OVERLAY                                                   
         SPACE 1                                                                
         XC    CONREC,CONREC       CLEAR RECORD                                 
         OI    CONRECH+6,X'80'                                                  
         XC    CONACT,CONACT       AND ACTION FIELDS                            
         OI    CONACTH+6,X'80'                                                  
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALLS TSAR                                               
*              R3=A(RECORD), R2=A(PARAM BLOCK=TSARBLK)                          
         SPACE                                                                  
         USING TSARD,R2                                                         
CALLTSAR NTR1  BASE=*,LABEL=*                                                   
         CLI   TSACTN,TSARES       TEST ACTION IS RESTORE                       
         JE    CALLTS2                                                          
         CLI   TSACTN,TSAINI       IF ACTION IS INITIALIZE                      
         JNE   CALLTS8                                                          
         SPACE                                                                  
CALLTS2  MVC   BYTE,TSACTN         SAVE ACTION IN BYTE                          
         XC    TSARBLK,TSARBLK     CLEAR PARAMETER BLOCK                        
         MVC   TSACTN,BYTE         RESTORE ACTION                               
         MVC   TSACOM,ACOMFACS     SET A(COMFACS)                               
         MVC   TSRECI,MYTSRECI     RECORD INDICATOR                             
         MVC   TSKEYL,MYTSKEYL     KEY LENGTH                                   
         MVC   TSRECL,MYTSRECL     RECORD LENGTH                                
         MVC   TSINDS,MYTSINDS     TSAR INDICATOR                               
         OI    TSINDS,TSIXTTWA     SPECIFY 14K RECORDS                          
         MVC   TSPAGL,MYTSPAGL     LOW TEMPEST/TEMPSTR PAGE #                   
         SPACE                                                                  
         OC    TSPAGL,TSPAGL       IF INITIALIZED ALREADY THIS CONNECT          
         JZ    *+8                                                              
         OI    TSINDS,TSIREUSE     SET TO REUSE PREV ALLOC TEMPEST AREA         
         SPACE                                                                  
         MVC   TSPAGN,MYTSPAGN     NUMBER OF PAGES, DEFAULT TO 1                
         CLI   TSPAGN,0                                                         
         JNE   *+8                                                              
         MVI   TSPAGN,1                                                         
         CLI   TSACTN,TSARES       TEST ACTION                                  
         JE    *+8                                                              
         OI    PRGSTAT,INITSARD    SET INITIALIZED AND/OR                       
         OI    TRNSTAT2,RESTSARD   SET BUFFER HAS BEEN RESTORED                 
         SPACE                                                                  
CALLTS8  ST    R3,TSAREC           SET A(RECORD)                                
         MVC   TSRNUM,MYTSRNUM     SETS RECORD NUMBER OR CLEARS TSRNUM          
*                                                                               
         GOTO1 TSAR,TSARBLK                                                     
         CLI   TSACTN,TSAINI       TEST ACTION INIT                             
         JNE   CALLTS10                                                         
         TM    TSINDS,TSIINIOK                                                  
         JNZ   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
         SPACE                                                                  
CALLTS10 MVC   MYTSRNUM,TSRNUM     SAVE RECORD NUMBER FOUND BY TSAR             
         MVC   MYTSPAGL,TSPAGL     AND LOW TEMPEST PAGE NUMBER                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE NOTIFIES VITA                                        *         
*        ON ENTRY ... P1 BYTE 0 = X'80' NOTIFY ONE ENVIRONMENT        *         
*                     P1        = A(MQ MESSAGE)                       *         
*                     P2        = L'MQ MESSAGE                        *         
***********************************************************************         
                                                                                
NVITA    NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE,0(R1)        TGBYTE=STATUS BYTE                           
         ZICM  R2,1(R1),3          R2=A(MQ MESSAGE)                             
         L     R3,4(R1)            R3=L'MQ MESSAGE                              
                                                                                
***********************************************************************         
*        SET BYTE TO INDICATE THE TALENT FILE                         *         
***********************************************************************         
                                                                                
         MVI   BYTE,TAL2           SET BYTE TO INDICATE TAL2                    
         CLC   TGUSER,=H'2276'     IF USER-ID IS DPS2                           
         JE    NV20                                                             
         CLC   TGUSER,=H'7698'     OR CLITST                                    
         JE    NV20                                                             
                                                                                
         MVI   BYTE,TAL3           SET BYTE TO INDICATE TAL3                    
         CLC   TGUSER,=H'7538'     IF USER-ID IS TALFQA                         
         JE    NV20                                                             
         CLC   TGUSER,=H'7697'     OR CLIFQA                                    
         JE    NV20                                                             
                                                                                
         MVI   BYTE,TAL4           SET BYTE TO INDICATE TAL4                    
         CLC   TGUSER,=H'15778'    IF USER-ID IS TPTPC                          
         JE    NV20                                                             
         CLC   TGUSER,=H'15777'    OR USER-ID IS TPCLI                          
         JE    NV20                                                             
                                                                                
***********************************************************************         
*        IF ON CSC, NOTIFY VITA 4 STG ENVIRONMENT                     *         
***********************************************************************         
                                                                                
         TM    PRGSTAT,CSCSYS                                                   
         JZ    NV10                                                             
         MVC   0(16,R2),=C'CSCVITA1********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ON ADV4, NOTIFY VITA TV AND RADIO PRODUCTION ENVIRONMENTS *         
***********************************************************************         
                                                                                
NV10     MVC   0(16,R2),=C'TALVITA2********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ON TST AND CONNECTING TO TAL2, NOTIFY VITA PAN, PDT, BIA  *         
*        AND DEV1 ENVIRONMENTS                                        *         
***********************************************************************         
                                                                                
NV20     TM    PRGSTAT,TESTSYS                                                  
         JZ    NV40                                                             
         CLI   BYTE,TAL2                                                        
         JNE   NV30                                                             
         MVC   0(16,R2),=C'TSTVITA3********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         MVC   0(16,R2),=C'TSTVITA5********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         TM    TGBYTE,X'80'                                                     
         JO    XIT                                                              
******** MVC   0(16,R2),=C'TSTVITA*********'                                    
******** GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         MVC   0(16,R2),=C'TSTVITA2********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ON TST AND CONNECTING TO TAL3, NOTIFY VITA QADATA AND     *         
*        IRIS ENVIRONMENTS                                            *         
***********************************************************************         
                                                                                
NV30     CLI   BYTE,TAL3                                                        
         JNE   XIT                                                              
         MVC   0(16,R2),=C'TSTVITA6********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         TM    TGBYTE,X'80'                                                     
         JO    XIT                                                              
         MVC   0(16,R2),=C'TSTVITA4********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ON FQA AND CONNECTING TO TAL2, NOTIFY VITA DEV2         , *         
***********************************************************************         
                                                                                
NV40     CLI   BYTE,TAL2                                                        
         JNE   NV50                                                             
         MVC   0(16,R2),=C'FQAVITA6********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ON FQA AND CONNECTING TO TAL3, NOTIFY VITA NOX, QA2/QACI, *         
*        FQA AND EOS ENVIRONMENTS                                     *         
***********************************************************************         
                                                                                
NV50     CLI   BYTE,TAL3                                                        
         JNE   NV60                                                             
         MVC   0(16,R2),=C'FQAVITA3********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         MVC   0(16,R2),=C'FQAVITA5********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         TM    TGBYTE,X'80'                                                     
         JO    XIT                                                              
******** MVC   0(16,R2),=C'FQAVITA*********'                                    
******** GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         MVC   0(16,R2),=C'FQAVITA2********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF ON FQA AND CONNECTING TO TAL4, NOTIFY VITA RADIO TEST     *         
*        AND VITA 4 UAT ENVIRONMENTS                                  *         
***********************************************************************         
                                                                                
NV60     CLI   BYTE,TAL4                                                        
         JNE   XIT                                                              
         MVC   0(16,R2),=C'FQAVITA4********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         MVC   0(16,R2),=C'FQAVITA7********'                                    
         GOTO1 MQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
TAL1     EQU   1                                                                
TAL2     EQU   2                                                                
TAL3     EQU   3                                                                
TAL4     EQU   4                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              EQUATES TO COVER PROGRAM NMOD1 ALLOCATION                        
         SPACE 1                                                                
SPOOLLNQ EQU   SPOOLEND-SPOOLD                                                  
GENDLNQ  EQU   GENDEND-GEND                                                     
SYSDLNQ  EQU   SYSDEND-SYSD                                                     
IOLNQ    EQU   NIOS*(LIOS+8)                                                    
PTRB00L  EQU   (40*(L'TLDRREC)+1)                                               
STF2LNQ  EQU   (2000-(TLRCELEM-TLRCD))                                          
LNKBLK   EQU   27500                                                            
WORKLNQ  EQU   SPOOLLNQ+GENDLNQ+IOLNQ+SYSDLNQ+LNKBLK                            
******** EQU   SPOOLLNQ+GENDLNQ+IOLNQ+SYSDLNQ+PTRB00L+STF2LNQ+LNKBLK            
         SPACE 1                                                                
NIOS     EQU   3                   3 IO AREAS                                   
LIOS     EQU   4000                4000 BYTE IO AREAS                           
         EJECT                                                                  
       ++INCLUDE TAGENEQUS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSEQUS                                                      
         EJECT                                                                  
       ++INCLUDE TAGENWORKD                                                     
         EJECT                                                                  
       ++INCLUDE TASYSDSECT                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
         EJECT                                                                  
       ++INCLUDE TALDCPTRD                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FACHKPT                                                        
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* TASCR07D - THESE SCREENS ARE ORG'D TO CONTAGH                                 
* TASCR88D                                                                      
* TASCR81D                                                                      
* TASCR40D                                                                      
* TASCRC1D                                                                      
* TASCRF8D                                                                      
*                                                                               
* TAGENFILE - THESE DSECTS ARE ALSO INCLUDED HERE                               
* CTGENFILE                                                                     
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
* FAGETTXTD                                                                     
* FAFACTS                                                                       
* DDMASTD                                                                       
* DDREMOTED                                                                     
* SEACSFILE                                                                     
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR07D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR88D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR81D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR40D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRF8D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC1D                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109TAGEN00   03/20/15'                                      
         END                                                                    
