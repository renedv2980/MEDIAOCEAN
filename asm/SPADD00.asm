*          DATA SET SPADD00    AT LEVEL 086 AS OF 05/01/02                      
*PHASE T21200A                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE DPTRD                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE TICTOC                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T21200 - ADDS CONTROLLER'                                       
T21200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T21200,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         L     RA,4(R1)            A(TWA)                                       
         USING T212FFD,RA                                                       
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    R7,BASER7                                                        
         LR    R9,R1                                                            
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         MVC   ATIOB,0(R1)         A(TIOB)                                      
         BAS   RE,SYSINIT          INITIALIZE PROGRAM DEPENDENT VALUES          
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR SAVED MESSAGES                         
         XC    CONHED2,CONHED2                                                  
         OI    CONHED2H+6,X'80'                                                 
*                                                                               
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,RCHANG      THEN SET RCHANG FLAG                         
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,ACHANG      THEN SET ACHANG FLAG                         
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+18                                                             
         CLC   =C'CHA',CONACT      AND IT CHANGED TO 'CHA'                      
         BNE   *+8                                                              
         OI    TRNSTAT,USERCHA     THEN SET USER CAUSED 'CHA' FLAG              
*                                                                               
         BAS   RE,GOGENCON         GO OFF TO GENCON                             
*                                                                               
         OI    CONRECH+4,X'20'     SET RECORD/ACTION FLDS VALID                 
         OI    CONACTH+4,X'20'                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* INITIALIZE PROGRAM DEPENDENT VALUES *                                         
*                                                                               
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS10    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS10                                                         
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,PRGCOMM                                                       
         LA    R0,NPRGCOMM                                                      
*                                                                               
SYS20    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SYS20                                                         
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R4,COREFACS         POINT TO ADDRESS AREA                        
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)           A(COMFACS)                                   
         L     RF,CCALLOV-COMFACSD(R1)                                          
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS30    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SYS30                                                         
*                                                                               
         MVI   SYSTEM,C'S'         SPOT                                         
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   ACTELOPT,C'N'       DON'T ADD ACTIVITY ELEMENT                   
         MVI   GETMSYS,23          USES GETMSG FOR SYSTEM 23                    
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9021200'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,STARTSV          SET SAVED STORAGE START                      
         ST    R1,ASTARTSV                                                      
*                                                                               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         OC    TWAVPRNT,TWAVPRNT   DON'T BOTHER IF OFFLINE                      
         BNZ   SYS40                                                            
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BE    SYS40                                                            
         LA    R1,RECACT1          DON'T ALLOW ALL RECORD TYPES                 
         ST    R1,ARECACT1                                                      
         LA    R1,RECACT3          OR ALL RECORD/ACTION COMBINATIONS            
         ST    R1,ARECACT3                                                      
*                                                                               
SYS40    LA    R1,CONRECH          SET EFH TAGS                                 
         ST    R1,EFHREC                                                        
         LA    R1,CONACTH                                                       
         ST    R1,EFHACT                                                        
         LA    R1,CONKEYH                                                       
         ST    R1,EFHKEY                                                        
         LA    R1,CONWHENH                                                      
         ST    R1,EFHWHEN                                                       
         LA    R1,CONOUTH                                                       
         ST    R1,EFHOUT                                                        
         LA    R1,CONDESTH                                                      
         ST    R1,EFHDEST                                                       
         LA    R1,CONOTHH                                                       
         ST    R1,EFHOTH                                                        
         LA    R1,CONTAGH                                                       
         ST    R1,EFHTAG                                                        
*                                                                               
         LA    R1,TREPTAB                                                       
         ST    R1,ATREPTAB                                                      
         LA    R1,RREPTAB                                                       
         ST    R1,ARREPTAB                                                      
*                                                                               
         OI    GENSTAT1,NOSETEFH                                                
*        OI    GENSTAT2,RETEQSEL                                                
         OI    GENSTAT3,OKVALSEL+RESTXE00                                       
         MVC   LSVTWA0,=AL2(MAXLTWA0)  L'STORAGE TO SAVE IN TWA0                
         MVI   NTWA,0              DON'T SAVE ANY EXTRA PAGES                   
         MVI   LRECACT,L'RECACT    SET L'RECACT TABLE ENTRY                     
*                                                                               
         L     RF,ATIOB            A(TIOB)                                      
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID          PICK UP PFKEY VALUE                          
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         DROP  RF                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE HANDLES PASSING OF CONTROL TO AND FROM GENCON                         
***********************************************************************         
GOGENCON NTR1                                                                   
         BAS   RE,SETRD            SET RD SO GENCON ALWAYS RETURNS              
*                                                                               
GOG10    MVI   GOAGAIN,C'N'        INITIALIZE RETURN SWITCH                     
         OI    TRNSTAT,FRSTMODE    ALLOWS APPL TO DETECT FIRST MODE             
*                                                                               
GOG20    GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON - PASS A(W/S)                  
*                                                                               
         CLI   GOAGAIN,C'Y'        REQUEST BY APPLIC. TO GO BACK                
         BE    GOG10                                                            
         ICM   R1,15,AFRSTKEY      IF CURSOR IS AT FIRST KEY FIELD              
         BZ    GOG40                                                            
         TM    6(R1),X'40'                                                      
         BZ    GOG40                                                            
         CLI   OKNO,2              AND GENCON IS ASKING FOR MORE INPUT          
         BNE   GOG30                                                            
         CLI   GOAGAIN,C'K'        AND WE DIDN'T TRY THIS ALREADY               
         BE    GOG30                                                            
         CLI   ACTNUM,ACTDEL       AND IF ACTION IS NOT DELETE                  
         BE    GOG40                                                            
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         BE    GOG40                                                            
*                                                                               
         MVI   CONKEY,C','         MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   CONKEYH+5,1         APPLICATION GETS A CHANCE TO FILL            
         MVI   GOAGAIN,C'K'        IN KEY FIELDS                                
         B     GOG20               GO BACK                                      
*                                                                               
GOG30    CLI   5(R1),0             IF NOTHING IS IN FIRST KEY FIELD             
         BNE   *+12                                                             
         CLI   ERROR,MISSING       AND MISSING INPUT FIELD ERROR                
         BE    PLSENTER            SWITCH TO PLEASE ENTER FIELDS ...            
*                                                                               
GOG40    CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BNE   GOGX                                                             
         CLI   OKNO,16             IF GENCON MSG IS "END OF LIST - HIT          
         BE    SELFIRST            ENTER...", CHANGE TO SELECT OR HIT..         
         CLI   OKNO,15             IF MSG IS "LIST DISPLAYED - HIT              
         BE    SELNEXT             ENTER...", CHANGE TO SELECT OF HIT..         
*                                                                               
GOGX     B     XIT                 ALL THROUGH                                  
         SPACE 3                                                                
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO WE GET CONTROL BACK                
         B     XIT                                                              
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VUSER                                                            
         B     VMED                                                             
         B     VCLI                                                             
         B     VPROD                                                            
         B     VMKT                                                             
         B     VSTAT                                                            
         B     VSLN                                                             
         B     VMGRPID                                                          
         B     VMGRPNO                                                          
         B     VSOURCE                                                          
         B     VEST                                                             
         B     VREFN                                                            
         B     VBUYER                                                           
         B     VSTNDCOM                                                         
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         B     VCLEARF                                                          
         B     VMYERR                                                           
         B     VREFTOPK                                                         
         B     VPKTOREF                                                         
         B     VGNXTREF                                                         
         B     VINITIAL                                                         
         B     GQPRD                                                            
         B     VGETTWA                                                          
*                                                                               
BASER7   DC    A(0)                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE GETS CALLED BY GENCON ON EVERY TRANSACTION                       
* BEFORE CALLING THE APPLICATION                                                
*                                                                               
VUSER    CLI   OFFLINE,C'Y'        ALWAYS DO THIS OFFLINE                       
         BE    VUSER10                                                          
         CLI   TWAFIRST,0          TEST FIRST TIME                              
         BE    VUSER10             YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER20                                                          
*                                                                               
VUSER10  MVI   TWAFIRST,1          WE'VE BEEN THROUGH HERE                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG FROM TWA                                     
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO                 
*                                                                               
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         DROP  R6                                                               
*                                                                               
VUSER20  MVC   RECNUM,TWALREC     MOVE SAVED REC/ACT TO CURRENT IN CASE         
         MVC   ACTNUM,TWALACT     WE TAKE ERROR BEFORE GENCON SETS THEM         
*                                                                               
         CLI   TWASCR,X'F6'        IS COMMENT MAINT SCREEN LOADED?              
         BNE   *+18                                                             
         CLC   =C'COM',CONREC      RECORD TYPE COMMENT?                         
         BNE   VUSER40                                                          
         B     VUSER30                                                          
*                                                                               
         CLI   TWASCR,X'F4'        IS AVMKT MAINT SCREEN LOADED?                
         BNE   *+18                                                             
         CLC   =C'AVM',CONREC      RECORD TYPE AVMKT?                           
         BNE   VUSER40                                                          
         B     VUSER30                                                          
*                                                                               
         CLI   TWASCR,X'F8'        IS AVMKT MAINT SCREEN LOADED?                
         BNE   VUSER40                                                          
         CLC   =C'AVC',CONREC      RECORD TYPE AVMKT?                           
         BNE   VUSER40                                                          
*                                                                               
VUSER30  CLC   =C'SEL',CONACT      ACTION SELECT?                               
         BNE   VUSER40                                                          
         CLI   THISLSEL,C'C'       'C' ENTERED AS LIST SELECTION?               
         BNE   VUSER40                                                          
         CLI   PFKEY,3             ERASE A LINE?                                
         BE    *+12                                                             
         CLI   PFKEY,4             INSERT A LINE?                               
         BNE   VUSER40                                                          
         MVC   CONACT(3),=C'CHA'   FORCE ACTION TO CHANGE                       
*                                                                               
VUSER40  TM    TRNSTAT,RCHANG      IF RECORD field HAS CHANGED                  
         BZ    VUSERX                                                           
         MVI   CALLSP,0            CLEAR CALLPROG STACK                         
         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
*                                                                               
VUSERX   B     XIT                                                              
         EJECT                                                                  
* OVERLAY INITIALIZATION                                                        
*                                                                               
*                                  P1=A(PFKEY VAL. TABLE) OR ZEROS              
*                                                                               
VINITIAL BAS   RE,CHNGSEL          CHANGE ANY 'S' TO 'C' AS PER MEL             
*                                                                               
         ICM   R3,7,1(R1)          IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT10                                                           
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
         GOTO1 PFVAL,DMCB,(R3)     HANDLE LOCAL PFKEY PRESENCE                  
         BE    DUMMYERR            TAKE DUMMY ERROR EXIT FOR GOAGAIN            
*                                                                               
INIT10   MVI   SCRSTAT,0           CLEAR SCREEN STATUS BYTE                     
*                                                                               
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
*                                                                               
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
*                                                                               
         MVC   BYTE,ACTNUM         MOVE CURRENT ACTION TO TEMP. W/S             
         CLI   BYTE,ACTCHA         IF CURRENT ACTION IS CHANGE                  
         BNE   *+16                                                             
         CLI   SVACT,ACTSEL        AND SAVED ACTION WAS SELECT                  
         BNE   *+8                                                              
         MVI   BYTE,ACTSEL         PRETEND CURRENT ACTION IS SELECT             
*                                                                               
         CLC   BYTE,SVACT          TEST ACTION CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,ACTCHG                                                   
*                                                                               
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    INIT20                                                           
         TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
         BZ    INIT30                                                           
         CLI   BYTE,ACTREP         ALWAYS CLEAR IF ACTION IS NOW REPORT         
         BE    INIT20                                                           
         CLI   SVACT,ACTSEL        IF LAST ACTION NOT SELECT                    
         BE    INIT30                                                           
         CLI   BYTE,ACTSEL         AND THIS ACTION NOT SELECT                   
         BE    INIT30                                                           
*                                                                               
INIT20   LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'SYSSPARE)                                              
         XCEFL                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         SR    RF,RF                                                            
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
*                                                                               
INIT30   MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM                     RECORD                          
         MVC   SVACT,BYTE                       ACTION                          
*                                                                               
         CLI   TWAOFFC,C'*'        IF DDS TERMINAL                              
         BNE   INITX                                                            
         CLI   ACTNUM,ACTSEL       IF THIS IS SELECT                            
         BNE   *+16                                                             
         CLI   MODE,DISPKEY        AND MODE IS DISPLAY KEY                      
         BNE   INITX                                                            
         B     *+12                                                             
         CLI   MODE,DISPREC        ELSE IF MODE IS DISPLAY RECORD               
         BNE   INIT40                                                           
         MVC   CONHED2(5),=C'(D/A=' DISPLAY D/A OF RECORD                       
         GOTO1 HEXOUT,DMCB,DMDSKADD,CONHED2+5,4,0                               
         MVI   CONHED2+13,C')'                                                  
         B     *+10                                                             
INIT40   XC    CONHED2(14),CONHED2 PRE-CLEAR D/A DISPLAY AREA                   
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*              LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                           
*                                                                               
*                                  P1  BYTES 1-3 = A(PFKEY VAL. TABLE)          
PFVAL    NTR1                                                                   
         CLI   PFKEY,0             USER HIT ENTER?                              
         BE    NO                  YES                                          
*                                                                               
         L     RF,0(R1)            RF=A(PFKEY TABLE)                            
         USING PFTABD,RF           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(RF),X'FF'                                                      
         BE    PFERR                                                            
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    NO                                                               
*                                                                               
*        TM    PFTSTAT,PFTCPROG+PFTRPROG ELSE IF NOT CPROG/RPROG ACTION         
*        BNZ   *+14                                                             
*        MVI   CALLSP,0            CLEAR CALLPROG STACK                         
*        XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
*                                                                               
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES                 IF RETURNS, RETURN CC EQUAL                  
         EJECT                                                                  
*              ROUTINE TO PROCESS PFKEY REQUEST                                 
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         MVI   PFKEY,0             CLEAR PFKEY FOR NEXT SCREEN                  
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
         CLI   CALLSP,0            DON'T NEED TO CHECK PREVIOUS SCREEN?         
         BE    PFI4                DON'T NEED TO                                
         ZIC   R3,CALLSP                                                        
         BCTR  R3,0                                                             
         LA    R2,CALLSTCK(R3)     PREVIOUS SCREEN IS THE SAME ONE WE           
         CLC   PFTSCRN,0(R2)           WANT TO GO TO?                           
         BNE   PFI4                NO                                           
         NI    PFTSTAT,X'FF'-PFTCPROG  YES, DON'T PUSH IF WE CAN POP            
         OI    PFTSTAT,PFTRPROG                                                 
*                                                                               
PFI4     TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    PFI5                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         CLI   CALLAGN,C'Y'                                                     
         BE    DUMMYERR            GO AGAIN TO GENCON                           
         B     DUMYERR1            TAKE DUMMY ERROR EXIT (NOT GOAGAIN)          
*                                                                               
PFI5     CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BE    PFI8                                                             
         MVC   CONREC,PFTREC       MOVE IT OUT                                  
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
*                                                                               
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
*                                                                               
PFI8     CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BE    PFIX                                                             
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
*                                                                               
PFIX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE SELECT IN LIST TO CHANGE                                    
***********************************************************************         
CHNGSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   CSELX                                                            
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
CSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    CSEL6                                                            
         OC    8(3,R2),SPACES                                                   
         ZIC   R1,5(R2)            R1=L(SELECT INPUT)                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SEL'     MATCH ON EXACT SELECT CODE                   
         BNE   CSEL6                                                            
         XC    8(3,R2),8(R2)       CHANGE SELECT CODE TO CHANGE                 
         MVI   8(R2),C'C'                                                       
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CSEL6    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    CSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   CSEL2               SELECT FIELD                                 
         B     CSEL6                                                            
*                                                                               
CSELX    B     XIT                                                              
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
*                                                                               
*                                  R3=A(PFKEY TABLE)                            
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
TSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
         OC    8(3,R2),SPACES                                                   
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         CLC   PFTSEL,8(R2)        MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     TSEL4                                                            
*                                                                               
TSEL6    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL2               SELECT FIELD                                 
         B     TSEL6                                                            
*                                                                               
TSEL8    MVC   8(3,R2),SPACES      FOUND A MATCH - CLEAR SELECT FIELD           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R3,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    R3,RA                                                            
         STH   R3,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR               
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN                   
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH             
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO            
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE            
* WORKING ON.  WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL          
* BE CALLED TO RESTORE THE SCREEN.                                              
*                                                                               
CPROG    NTR1                                                                   
         CLI   CALLSP,L'CALLSTCK   IF ALREADY HAVE MAX NEST LEVELS              
         BNL   CANTPUSH                                                         
*                                                                               
         ZIC   R3,CALLSP           SAVE SCREEN NUMBER ON STACK                  
         LA    RF,CALLSTCK(R3)                                                  
         MVC   0(1,RF),TWASCR                                                   
*                                                                               
         LA    R3,1(R3)            INCREMENT STACK POINTER                      
         STC   R3,CALLSP                                                        
*                                                                               
         L     RE,ATIA             SAVE SCREEN IN FIRST HALF OF TWA             
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),STARTSV)                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON             
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG.         
*                                                                               
RPROG    NTR1                                                                   
         CLI   CALLSP,0                                                         
         BE    PFERR               ERROR IF STACK IS EMPTY                      
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE SCREEN FROM 1ST HALF OF TWA          
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   ACTNUM,TWALACT      SPECIAL CODE TO KEEP SELECT GOING            
         MVC   RECNUM,TWALREC                                                   
         MVC   CONHEAD(30),=CL30'Came back from another screen.'                
         MVC   CONHEAD+30(30),=CL30'  Please continue ...'                      
*                                                                               
         LA    R2,3                READ TWA RECORD #3                           
         GOTO1 GETTWA,DMCB,((R2),STARTSV)                                       
*                                                                               
         ZIC   R3,CALLSP           DECREMENT STACK POINTER                      
         BCTR  R3,0                                                             
         STC   R3,CALLSP                                                        
*                                                                               
         LA    RF,CALLSTCK(R3)     EXTRACT TWASCR                               
         MVC   TWASCR,0(RF)                                                     
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         OI    TRNSTAT,RETURNED    SET THAT RETPROG HAS BEEN CALLED             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD                   
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R2,WORK             R2=A(WORK)                                   
         ZIC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
*                                                                               
EXP10    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         LA    RF,SYSSPARE                                                      
         CLI   KEYTYPE,KEYTYWS     W/S (SYSSPARE)                               
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
EXP20    MVI   0(R2),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R2,1(R2)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R2,R3               R2=L'TMPKEY FIELD                            
         CLM   R2,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R2,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     XIT                                                              
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
         SPACE 5                                                                
BMPTOROW NTR1                      BUMP TO FIRST FIELD IN ROW                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE                                                        
         BE    BMPT4                                                            
         ZIC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   BMPT2                                                            
         B     NO                  RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    ZIC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     YES                                                              
         EJECT                                                                  
* VALIDATE MEDIA CODE *                                                         
*                                                                               
VMED     GOTO1 ANY                                                              
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VMED2    BAS   RE,NEXTEL                                                        
         BNE   VMED4                                                            
         CLC   2(1,R6),8(R2)                                                    
         BNE   VMED2                                                            
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,4(R6)         MEDIA NAME                                   
         MVC   MEDCAPT,14(R6)      AND CAPTION                                  
         B     XIT                                                              
*                                                                               
VMED4    MVI   GERROR1,INVMED                                                   
         B     VMYERR                                                           
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE CLIENT - ON EXIT QCLT AND BCLT CONTAIN VALUES                        
*                                                                               
VCLI     GOTO1 ANY                 CLIENT                                       
*                                                                               
         MVI   GERROR1,INVCLI                                                   
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),3                                                          
         BH    VMYERR                                                           
         CLI   5(R2),2                                                          
         BL    VMYERR                                                           
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   VMYERR                                                           
*                                                                               
* READ CLIENT HEADER *                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
* SAVE CLIENT PRODUCT LIST *                                                    
*                                                                               
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
*                                                                               
*****                                                                           
         MVI   GERROR1,SECLOCK                                                  
         OC    TWAACCS(2),TWAACCS  ANY SECURITY LIMIT?                          
         BZ    XIT                                                              
         CLI   TWAACCS,C'*'        OFFICE LOCKOUT?                              
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'+'        MKT LOCKOUT?                                 
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'$'        OFFICE LIST?                                 
         BE    CLT20               YES                                          
*****                                                                           
CLT10    CLC   TWAACCS(2),BCLT                                                  
         BNE   VMYERR                                                           
CLT20    CLI   TWAACCS,C'$'                                                     
         BE    CLT30                                                            
         CLI   TWAACCS,C'*'                                                     
         BNE   *+14                                                             
         CLC   TWAACCS+1(1),COFFICE                                             
         BNE   VMYERR                                                           
         B     XIT                                                              
*****                                                                           
CLT30    CLI   TWAACCS,C'$'        OFFICE LIST?                                 
         BNE   XIT                                                              
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 OFFICER,DMCB,DUB,ACOMFACS                                        
         CLI   0(R1),0                                                          
         BNE   VMYERR                                                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE PRD (-SLN)   - ON EXIT WORK(3)   = EBCDIC PRODUCT                    
*                                 WORK+3(1) = PRODUCT CODE                      
*                                 WORK+4(1) = SPOT LENGTH (IF ENTERED)          
*                                 BPRD      = PRODUCT CODE                      
*                                 QPRD      = EBCDIC PRODUCT                    
*                                 PRDNM     = PRODUCT NAME                      
VPROD    XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
*                                                                               
         MVI   GERROR1,INVPROD                                                  
         CLC   =C'AAA',12(R4)                                                   
         BE    VMYERR                                                           
         CLI   0(R4),2                                                          
         BL    VMYERR                                                           
         CLI   0(R4),3                                                          
         BH    VMYERR                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),12(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   WORK(3),12(R4)      RETURN EBCDIC PRD CODE                       
         MVC   WORK+3(1),PCODE+1      AND BINARY PRD CODE                       
         MVC   BPRD,PCODE+1                                                     
         MVC   QPRD,WORK                                                        
         MVC   PRDNM,PNAME                                                      
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE SPOT LENGTH IF INPUT *                                               
*                                                                               
         MVI   WORK+4,0                                                         
         CLI   1(R4),0             TEST INPUT                                   
         BE    XIT                                                              
         MVC   WORK+4(1),11(R4)    RETURN SLN IN BINARY                         
         MVI   GERROR1,BADSLN                                                   
         CLI   1(R4),3                                                          
         BH    VMYERR                                                           
         TM    3(R4),X'40'         TEST NUMERIC                                 
         BZ    VMYERR                                                           
*                                                                               
VPRD8    LA    R1,SLNTAB                                                        
         LA    R0,15                                                            
         CLC   0(1,R1),WORK+4      COMPARE BINARY VALUES                        
         BE    VPRD10                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     VMYERR                                                           
*                                                                               
VPRD10   MVC   WORK+4(1),11(R4)                                                 
         B     XIT                                                              
*                                                                               
SLNTAB   DC    AL1(10,15,20,30,40,45,50,60,90,120,105,150,75,5)                 
         DC    5AL1(0)                                                          
         EJECT                                                                  
* VALIDATE ESTIMATE - ON EXIT QEST  = EBCDIC ESTIMATE                           
*                             BEST  = BINARY ESTIMATE                           
*                             ESTNM = ESTIMATE NAME                             
*                                                                               
VEST     GOTO1 ANY                                                              
         MVI   GERROR1,INVEST                                                   
         MVC   QEST,WORK                                                        
         TM    4(R2),X'08'         TEST VALID NUMERIC                           
         BZ    VMYERR                                                           
         CLI   5(R2),3                                                          
         BH    VMYERR                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'1'            TEST IN RANGE 1-255                          
         BL    VMYERR                                                           
         CH    RE,=H'255'                                                       
         BH    VMYERR                                                           
         STC   RE,BEST             SET BINARY ESTIMATE                          
         XC    KEY,KEY                                                          
         LA    R6,KEY              READ ESTIMATE HEADER                         
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   ESTNM,EDESC         SET ESTIMATE NAME                            
         MVC   ESTDYMNU,EDAYMENU   SET ESTIMATE DAYPART MENU                    
         MVC   ESTSTRT,ESTART      SET ESTIMATE PERIOD                          
         MVC   ESTEND,EEND                                                      
         MVC   ESTBOOK,EBOOK       SET ESTIMATE RATING BOOK                     
         MVC   ESTDEMOS,EDEMLST    SET ESTIMATE DEMOS                           
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE STATION CALL LETTERS - ON EXIT QSTA QMKT BMKTSTA                     
*                                         AND STAPRINT ARE SET                  
*                                                                               
VSTAT    XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVI   GERROR1,INVSTAT                                                  
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   0(R4),3                                                          
         BL    VMYERR                                                           
         MVC   QSTA(4),12(R4)      SAVE CALL LETTERS                            
*                                                                               
         CLI   QMED,C'R'           TEST MEDIA IS RADIO                          
         BE    VSTA6                                                            
*                                                                               
* MEDIA NOT RADIO *                                                             
*                                                                               
         CLI   1(R4),0             TEST SUB-MEDIA ENTERED                       
         BNE   VSTA4               YES                                          
         MVC   QSTA+4(1),QMED      ELSE SET SUB-MED = MEDIA                     
         B     VSTA10                                                           
VSTA4    MVC   QSTA+4(1),22(R4)    MOVE SUB-MEDIA                               
         CLI   1(R4),1                                                          
         BNE   VMYERR                                                           
         CLC   QSTA+4(1),QMED      IF INPUT, MUST MATCH MEDIA CODE              
         BNE   VMYERR                                                           
         B     VSTA10                                                           
*                                                                               
* MEDIA = RADIO - AM OR FM IS REQUIRED *                                        
*                                                                               
VSTA6    CLI   1(R4),2                                                          
         BH    VMYERR                                                           
         MVC   QSTA+4(1),22(R4)                                                 
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,STAAM                                                         
         BE    VSTA10                                                           
         EX    R5,STAFM                                                         
         BE    VSTA10                                                           
         B     VMYERR                                                           
*                                                                               
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
*                                                                               
* FORMAT STATION FOR PRINTING (EG WABC-FM) *                                    
*                                                                               
VSTA10   MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4      MOVE SUB-MEDIA                               
         MVI   3(RE),C'V'          ASSUME TV                                    
         CLI   QMED,C'T'                                                        
         BE    VSTA12                                                           
         MVI   3(RE),C'M'          ASSUME RADIO                                 
         CLI   QMED,C'R'                                                        
         BE    VSTA12                                                           
         MVI   3(RE),C' '                                                       
*                                                                               
* READ STATION MASTER RECORD *                                                  
*                                                                               
VSTA12   DS    0H                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',KEY,AIO                  
         CLI   8(R1),0                                                          
         BNE   VMYERR                                                           
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         MVC   QMKT,SMKT                                                        
         GOTO1 MSPACK,DMCB,QMKT,QSTA,BMKTSTA                                    
         EJECT                                                                  
* READ MARKET RECORD TO IO1+200                                                 
*                                                                               
         LA    R6,200(R6)                                                       
*                                                                               
VSTA14   ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         MVI   GERROR1,INVMKT                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLC   KEY(MKTKEYLN),0(R6)                                              
         BNE   VMYERR                                                           
*                                                                               
         CLI   TWAACCS,C'+'        MARKET LOCKOUT?                              
         BNE   VSTA20                                                           
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
         CLC   TWAACCS+1(1),0(R1)                                               
         BE    VSTA20                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         MVI   GERROR1,NOMKTACC                                                 
         B     VMYERR                                                           
*                                                                               
VSTA20   MVC   MKTNM,MKTNAME       RETURN MARKET NAME TO USER                   
         MVI   BYTE,C'0'           FIND RATING SERVICE MARKET                   
         CLI   BKVALSRC,C'N'                                                    
         BE    *+8                                                              
         MVI   BYTE,C'1'                                                        
         CLC   MKTRS1,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM1                                                    
         CLC   MKTRS2,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM2                                                    
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE MARKET CODE *                                                        
*                                                                               
VMKT     DS    0H                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   GERROR1,INVMKT                                                   
         B     VMYERR                                                           
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SET FOR USER TOO                             
*                                                                               
         L     R6,AIO1                                                          
         B     VSTA14                                                           
         SPACE 2                                                                
* VALIDATE SPOT LENGTH *                                                        
*                                                                               
VSLN     MVI   GERROR1,BADSLN                                                   
         GOTO1 VALINUM                                                          
         MVC   WORK(1),ACTUAL                                                   
         MVC   WORK+4(1),ACTUAL    MOVE HERE FOR EDIT COMPATABILITY             
         B     VPRD8                                                            
         EJECT                                                                  
* VALIDATE MARKET GROUP ID - ON EXIT MGRPID AND MGRPLEN ARE SET                 
*                                                                               
VMGRPID  GOTO1 ANY                                                              
         MVI   GERROR1,INVMGRP                                                  
         CLI   8(R2),C'A'          TEST MKTGRP ID ALPHA                         
         BL    VMYERR                                                           
         CLI   8(R2),C'Z'                                                       
         BH    VMYERR                                                           
         MVC   MGRPID,8(R2)        MARKET GROUP ID                              
         XC    KEY,KEY             GET MKTGRP X'0000' RECORD                    
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         CLI   MGRPID,C'F'                                                      
         BH    *+10                                                             
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+8(1),MGRPID                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VMYERR                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL            GET MKTGRP BREAK DESCRIPTION                 
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKGEL01,R6                                                       
         MVC   MGRPLEN,MKGBK1LN    MKTGRP BREAK 1 LEN                           
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE MARKET GROUP NO - ON EXIT MGRPNO AND MGRPNM ARE SET                  
*                                                                               
VMGRPNO  MVI   GERROR1,INVMGRP                                                  
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         CLM   R3,1,MGRPLEN        TEST FOR CORRECT BREAK 1 LENGTH              
         BNE   VMYERR                                                           
         XC    KEY,KEY             GET MKTGRP RECORD                            
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT       FIRST TRY WITH CLIENT                        
         MVC   KEY+8(1),MGRPID                                                  
         MVC   WORK(4),=C'0000'                                                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),9(R2)                                                    
         PACK  DUB(3),WORK(5)                                                   
         MVC   MGRPNO,DUB          MARKET GROUP NUMBER                          
         MVC   KEY+9(2),MGRPNO                                                  
*                                                                               
VMGRPN2  MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         UNPK  DUB(5),KEY+9(3)                                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),DUB         TEST RECORD EXISTS                           
         BE    VMGRPN4                                                          
         OC    KEYSAVE+3(2),KEYSAVE+3  NO - TEST FOR CLIENT SPECIFIC            
         BZ    VMYERR                       NO - ERROR                          
         MVC   KEY,KEYSAVE                  YES- TRY WITHOUT CLIENT             
         XC    KEY+3(2),KEY+3                                                   
         B     VMGRPN2                                                          
*                                                                               
VMGRPN4  MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKGEL10,R6                                                       
         MVC   MGRPNM,MKGNAM1      MARKET GROUP NAME                            
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
*  VALIDATE RATING SOURCE                                                       
*                                                                               
VSOURCE  XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY     FOR MARKET SECURITY                          
*                                                                               
         GOTO1 ANY                                                              
         ZIC   RE,5(R2)            GET INPUT LENGTH                             
         BCTR  RE,0                SET FOR EX                                   
*                                                                               
         MVI   GERROR1,INVSRC                                                   
         LA    R4,SRCLIST                                                       
         USING SRCLISTD,R4                                                      
*                                                                               
SRC10    EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SRCINPUT                                                 
         BE    SRC20                                                            
         LA    R4,SRCNEXT                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   SRC10                                                            
         B     VMYERR                                                           
*                                                                               
SRC20    MVC   DBSELSRC,SRCDBSRC                                                
         MVC   DBFILE,SRCDBFIL                                                  
         MVC   8(6,R2),SRCINPUT                                                 
         OI    6(R2),X'80'         TRANSMIT FULL SOURCE NAME                    
         MVC   DBSELMED,SRCSELMD                                                
         MVC   BKVALSRC,SRCBKSRC                                                
*                                                                               
         MVC   BYTE,DBSELMED                                                    
         CLI   BYTE,C'C'                                                        
         BNE   *+8                                                              
         MVI   BYTE,C'T'                                                        
         GOTO1 MEDGET,DMCB,(BYTE,AGENCY),DATAMGR,WORK                           
         MVC   BAGYMD,WORK         SAVE AGENCY/MEDIA BYTE                       
         CLI   8(R1),X'FF'                                                      
         BE    VMYERR                                                           
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
SRCLIST  DS    0CL12                                                            
         DC    C'ARB   ',C'T',C'TP ',C'AA'                                      
         DC    C'BBM   ',C'C',C'TP ',C'AA'                                      
         DC    C'CSI   ',C'C',C'TP ',C'NN'                                      
         DC    C'NSI   ',C'T',C'TP ',C'NN'                                      
         DC    C'RARB  ',C'R',C'TP ',C'AA'                                      
         DC    C'RBIR  ',C'R',C'TP ',C'NN' **SHOULD BE SOURCE B                 
         DC    C'BIRCH ',C'R',C'TP ',C'NN' **                                   
         DC    C'DRARB ',C'R',C'RDP',C'AA'                                      
         DC    C'DRBIR ',C'R',C'RDP',C'NN' **                                   
         DC    X'FF'                                                            
*                                                                               
SRCLISTD DSECT                                                                  
SRCINPUT DS    CL6                 VALID INPUT                                  
SRCSELMD DS    CL1                 DBSELMED VALUE                               
SRCDBFIL DS    CL3                 DBSELFIL VALUE                               
SRCDBSRC DS    CL1                 DBSELSRC VALUE                               
SRCBKSRC DS    CL1                 BOOKVAL SOURCE VALUE                         
SRCNEXT  EQU   *                                                                
*                                                                               
T21200   CSECT                                                                  
         EJECT                                                                  
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS           
*                                                                               
VMYERR   OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
*                                                                               
         MVI   GTMSYS,23           SYSTEM 23 MESSAGES                           
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    *+8                 NO -- USE CONTROL SYSTEM MESSAGES            
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
*                                                                               
         GOTO1 ERREX                                                            
         EJECT                                                                  
***********************************************************************         
* VALIREF:                                                                      
*        THIS ROUTINE VALIDATES THE REFERENCE NUMBER FIELD.                     
*                                                                               
* ON ENTRY:    (R2)                A(FIELD HEADER OF REFERENCE NUMBER)          
*                                                                               
* ON EXIT:     QMED       CL1      EBCDIC OF MEDIA                              
*              BAGYMD     XL1      AGENCY/MEDIA COMBINATION                     
*              MEDNM      CL10     MEDIA NAME                                   
*              MEDCAPT    CL7      MEDIA CAPTION                                
***********************************************************************         
VREFN    DS    0H                                                               
         MVI   GERROR1,INVREFN     ERROR CODE FOR INVALID REFERENCE #           
*                                                                               
         CLI   8(R2),C'T'          IF MEDIA IS NOT TELEVISION OR RADIO          
         BE    *+12                                                             
         CLI   8(R2),C'R'                                                       
         BNE   VMYERR              THEN ERROR                                   
*                                                                               
         CLI   5(R2),1             IF ONLY THE MEDIA                            
         BE    VREF20              THEN VALIDATE THE MEDIA                      
*                                                                               
         ZIC   R1,5(R2)            R1 = # OF DIGITS IN REFERENCE #              
         BCTR  R1,0                                                             
         LA    R3,9(R2)            R3 = A(REFERENCE # W/O MEDIA)                
*                                                                               
VREF10   CLI   0(R3),C'0'          IF CHARACTER NOT A VALID NUMERIC             
         BL    VMYERR                                                           
         CLI   0(R3),C'9'                                                       
         BH    VMYERR              THEN ERROR                                   
*                                                                               
         LA    R3,1(R3)            CHECK NEXT CHARACTER                         
         BCT   R1,VREF10           UNTIL ALL THE DIGITS ARE VALID               
*                                                                               
VREF20   GOTO1 VALIMED                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REFTOPAK                                                                      
*        THIS ROUTINE CONVERTS THE REFERENCE NUMBER TO ITS PACKED WITH          
* SIGN AND UNCOMPLEMENTED EQUIVALENT.                                           
*                                                                               
* ON ENTRY:    P1                  A(REFERENCE # PWOS AND 9'S COMP.)            
*                                                                               
* ON EXIT:     P1                  PACKED EQUIVALENT                            
***********************************************************************         
VREFTOPK DS    0H                                                               
         L     R4,0(R1)                                                         
         ZICM  R1,0(R4),3                                                       
         SLL   R1,4                                                             
         ST    R1,DMCB                                                          
         OI    DMCB+3,X'0F'                                                     
         SP    DMCB(4),=P'999999'  UNCOMPLEMENT REFERENCE NUMBER                
         OI    DMCB+3,X'0F'                                                     
         B     XIT                                                              
***********************************************************************         
* PAKTOREF                                                                      
*        THIS ROUTINE CONVERTS THE REFERENCE NUMBER TO ITS PACKED               
* WITHOUT SIGN AND 9'S COMPLEMENTED EQUIVALENT.                                 
*                                                                               
* ON ENTRY:    P1                  A(REFERENCE NUMBER)                          
*                                                                               
* ON EXIT:     P1                  PWOS AND 9'S COMPLEMENT                      
***********************************************************************         
VPKTOREF DS    0H                                                               
         L     R4,0(R1)                                                         
         L     R1,0(R4)                                                         
         ST    R1,DMCB                                                          
         SP    DMCB(4),=P'999999'  COMPLEMENT IT                                
         OI    DMCB+3,X'0F'                                                     
         L     R1,DMCB                                                          
         SRL   R1,4                                                             
         ST    R1,DMCB                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GNEXTREF                                                                      
*        THIS ROUTINE GETS THE NEXT REFERENCE NUMBER FOR THE                    
* AGENCY/MEDIA.  THE NUMBER WILL BE RETURNED IN PARAMETER ONE IN PACKED         
* DECIMAL FORM.                                                                 
*                                                                               
* CAUTION:     SVKEY GETS CLOBBERED                                             
*                                                                               
* ON EXIT:     P1                   NEXT REFERENCE NUMBER                       
***********************************************************************         
VGNXTREF DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY USED                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   SAVE YEAR DIGIT FOR TODAY            
         MVC   YRDIGIT,DUB+1                                                    
         NI    YRDIGIT,X'0F'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,FULL)  SAVE WEEK IN YEAR FOR TODAY         
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),FULL+2                                                  
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         LA    R2,7                                                             
         DR    R0,R2                                                            
         STC   R1,WKNUMB                                                        
*                                                                               
VGN10    LA    R4,KEY                                                           
         USING AVARECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   AVAKTYP,AVAKTYPQ                                                 
         MVI   AVAKSUB,AVAKSUB2                                                 
         MVC   AVAKAM,BAGYMD                                                    
*                                                                               
         ZIC   R1,YRDIGIT          GET 9'S COMPLEMENT OF YEAR DIGIT             
         LA    R0,9                                                             
         SR    R0,R1                                                            
*                                                                               
         SLL   R0,4                PUT YEAR IN KEY                              
         STC   R0,AVAKREF2                                                      
*                                                                               
         GOTO1 HIGH                NONE FOR THIS AGY/MED?                       
*                                                                               
         ZIC   R1,WKNUMB           MAKE WEEK NUMBER PACKED                      
         CVD   R1,DUB                                                           
*                                                                               
         CLC   KEY(AVAKREF2-AVAKEY),KEYSAVE                                     
         BNE   VGN20               YES, MAKE A NEW ONE                          
*                                                                               
         GOTO1 REFTOPAK,DMCB,AVAKREF2                                           
         CLC   YRDIGIT,DMCB        YEAR IS THE SAME?                            
         BNE   VGN20               NO, MAKE A NEW ONE                           
*                                                                               
         LH    R1,DUB+6            WEEK IS THE SAME?                            
         SRL   R1,4                                                             
         CLM   R1,1,DMCB+1                                                      
         BNE   VGN20                                                            
*                                                                               
         AP    DMCB+2(2),=P'1'                                                  
         B     VGNX                                                             
*                                                                               
VGN20    XC    DMCB,DMCB                                                        
         MVC   DMCB(1),YRDIGIT                                                  
         LH    R1,DUB+6                                                         
         SRL   R1,4                                                             
         STC   R1,DMCB+1                                                        
         MVI   DMCB+3,X'1F'                                                     
*                                                                               
VGNX     MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY BEING USED                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIBUYR                                                                      
*        THIS ROUTINE VALIDATES THE BUYER AND RETURNS THE OFFICE CODE.          
*                                                                               
* CAUTION:     SVKEY GETS CLOBBERED.                                            
*                                                                               
* ON ENTRY:    P1                   A(3 BYTE CHARACTER BUYER)                   
*                                                                               
* ON EXIT:     QBUYER               BUYER'S FULL NAME                           
*              QOFFICE              OFFICE CODE OF THE BUYER                    
*              QPHONE               PHONE NUMBER OF THE BUYER                   
*              QPHONEXT             PHONE EXTENSION                             
***********************************************************************         
VBUYER   DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY USED                                
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         LA    R4,KEY              SET UP THE KEY                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKBYR,0(R2)                                                    
         OC    BYRKBYR,SPACES                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BE    *+14                                                             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY BEING USED                       
         B     NO                  RETURN 'NO' TO CALLER                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BYRDSCD,R6                                                       
         MVC   QBUYER,BYRFNAME     GET BUYER'S FULL NAME                        
         MVC   QOFFICE,BYROFFID        OFFICE ID                                
         MVC   QPHONE,BYRPHONE         PHONE NUMBER                             
         MVC   QPHONEXT,BYRPHEXT       AND PHONE EXTENSION                      
*                                                                               
         XC    KEY,KEY             RESTORE KEY BEING USED                       
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         B     YES                 RETURN 'YES' TO CALLER                       
         EJECT                                                                  
***********************************************************************         
* GETQPRD                                                                       
*        THIS ROUTINE GETS THE EBCDIC EQUIVALENT FOR THE BINARY PRODUCT         
* CODE SET IN BPRD.  BAGYMD AND BCLT MUST BE SET SO THAT THE CLIENT             
* RECORD CAN BE READ.                                                           
*                                                                               
* CAUTION:     SVKEY GETS CLOBBERED.                                            
*                                                                               
* ON ENTRY:    BPRD                 BINARY PRODUCT CODE                         
*              BAGYMD               BINARY AGENCY/MEDIA CODE                    
*              BCLT                 BINARY CLIENT CODE                          
*                                                                               
* ON EXIT:     QPRD                 EBCDIC PRODUCT CODE                         
***********************************************************************         
GQPRD    DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY USED                                
*                                                                               
         LA    R4,KEY              SET UP THE KEY                               
         USING CLTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(L'BAGYMD),BAGYMD                                           
         MVC   KEY+2(L'BCLT),BCLT                                               
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                RETURN 'NO' IF ERROR                         
         BNE   NO                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BNE   NO                                                               
*                                                                               
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         LA    R4,CLIST                                                         
GQPRDLP  CLI   0(R4),0                                                          
         BNE   *+8                                                              
         B     NO                  RETURN 'NO' TO CALLER IF EOT                 
         CLC   BPRD,3(R4)                                                       
         BE    GQPRDYES                                                         
         LA    R4,4(R4)                                                         
         B     GQPRDLP                                                          
*                                                                               
GQPRDYES MVC   QPRD,0(R4)          GOT THE EBCDIC PRODUCT CODE                  
*                                                                               
         XC    KEY,KEY             RESTORE KEY BEING USED                       
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         B     YES                 RETURN 'YES' TO CALLER                       
         EJECT                                                                  
***********************************************************************         
* VALISCOM                                                                      
*        THIS ROUTINE VALIDATES IF THE STANDARD COMMENT EXISTS.                 
*                                                                               
* CAUTION:     SVKEY GETS CLOBBERED.                                            
*                                                                               
* ON ENTRY:    P1                   A(STANDARD COMMENT)                         
***********************************************************************         
VSTNDCOM DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY USED                                
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         LA    R4,KEY              SET UP THE KEY                               
         USING COMRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   COMKTYP,COMKTYPQ                                                 
         MVI   COMKSUB,COMKSUBQ                                                 
         MVC   COMKAM,BAGYMD                                                    
         MVC   COMKCOM,0(R2)                                                    
         OC    COMKCOM,SPACES                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'COMKEY),KEYSAVE                                            
         BE    *+14                                                             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY BEING USED                       
         B     NO                  RETURN 'NO' TO CALLER                        
*                                                                               
         XC    KEY,KEY             RESTORE KEY BEING USED                       
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         B     YES                 RETURN 'YES' TO CALLER                       
         EJECT                                                                  
*              ROUTINE TO READ/WRITE TEMPSTR PAGES                              
         SPACE 2                                                                
*                                  P1, BYTE  0=BIT SETTINGS/PAGE NUMBER         
*                                  P1, BYTES 1-3=READ/WRITE ADDRESS             
*                                                                               
VGETTWA  MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA10                                                           
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA10                                                           
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA10   NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
*                                                                               
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* VCLEARF - CLEAR AND FOUT FIELDS                                               
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
VCLEARF  LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
*                                                                               
VCLEARF2 IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,*+8              BRANCH ACCORDINGLY                           
         B     *+8                                                              
         BC    0,VCLEARF4                                                       
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
*                                                                               
VCLEARF4 LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    VCLEARF2            NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
         SPACE 5                                                                
* SUBROUTINE SETS A COMMA AFTER LAST DATA CHAR *                                
*                                                                               
SETCOMMA CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
*              LOCAL EXIT/ERROR ROUTINES                                        
*                                                                               
CANTPUSH MVI   GERROR1,ERNOPUSH    PUSH ERROR - TOO MANY NEST LEVELS            
         B     RETCURS                                                          
PFERR    MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
RETCURS  LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         GOTO1 MYERR                                                            
*                                                                               
PLSENTER MVI   GERROR1,2           PLEASE ENTER FIELDS AS REQUIRED              
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         L     R2,AFRSTKEY         R2 TO 1ST KEY FIELD                          
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         NI    TIOBINDS-TIOBD(RE),X'FF'-TIOBALRM   TURN OFF BEEP                
         GOTO1 MYERR                                                            
*                                                                               
DUMMYERR MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
DUMYERR1 LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
*                                                                               
SELFIRST MVI   GERROR1,10          SELECT OR HIT ENTER FOR FIRST                
         B     *+8                                                              
SELNEXT  MVI   GERROR1,9           SELECT OR HIT ENTER FOR NEXT                 
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         MVI   OKNO,0              CLEAR OKNO SO WON'T LOOP ENDLESSLY           
         L     R2,AFRSTREC         R2 TO 1ST SEL FIELD                          
         GOTO1 MYERR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
RELO     DS    F                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(CLPACK)                                                        
         DC    V(CLUNPK)                                                        
         DC    V(MEDGET)                                                        
         DC    V(RECUP)                                                         
         DC    V(BINSRCH)                                                       
         DC    V(DPTRD)                                                         
         DC    V(TICTOC)                                                        
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QQSORT)                                                      
         DC    AL1(QMSPACK)                                                     
         DC    AL1(QMSUNPK)                                                     
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QGETIDS)                                                     
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                                                               
RECACT   DS    0CL12                                                            
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                                                               
RECACT1  DC    X'01',C'BUYER   ',AL1(01),X'0000'                                
         DC    X'01',C'MARKET  ',AL1(02),X'0000'                                
         DC    X'01',C'AVAIL   ',AL1(03),X'0000'                                
         DC    X'01',C'AVMKTS  ',AL1(04),X'0000'                                
         DC    X'01',C'PROFILE ',AL1(05),X'0000'                                
         DC    X'01',C'COMMENT ',AL1(06),X'0000'                                
         DC    X'01',C'AUDIT   ',AL1(07),X'0000'                                
         DC    X'01',C'AVCOM   ',AL1(08),X'0000'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,04,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,02,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,05,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,03,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,05,00)                                  
         DC    X'02',C'ASSIGN  ',AL1(07,07,00)                                  
         DC    X'02',C'XFER    ',AL1(08,08,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'SEND    ',AL1(13,13,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                                                               
RECACT3  DC    X'03',AL1(01,01),X'F101000080',C'    '  BUYER    ADD             
         DC    X'03',AL1(01,04),X'F101000080',C'    '  BUYER    CHA             
         DC    X'03',AL1(01,02),X'F101000080',C'    '  BUYER    DIS             
         DC    X'03',AL1(01,05),X'F101000080',C'    '  BUYER    DEL/RES         
         DC    X'03',AL1(01,03),X'F101000080',C'    '  BUYER    SEL             
         DC    X'03',AL1(01,10),X'E101000080',C'    '  BUYER    LIST            
*                                                                               
         DC    X'03',AL1(02,07),X'F202000081',C'    '  MARKET   ASSIGN          
*                                                                               
         DC    X'03',AL1(03,01),X'F303000080',C'    '  AVAIL    ADD             
         DC    X'03',AL1(03,04),X'F303000080',C'    '  AVAIL    CHA             
         DC    X'03',AL1(03,02),X'F303000080',C'    '  AVAIL    DIS             
         DC    X'03',AL1(03,05),X'F303000080',C'    '  AVAIL    DEL/RES         
         DC    X'03',AL1(03,03),X'F303000080',C'    '  AVAIL    SEL             
         DC    X'03',AL1(03,10),X'E303000080',C'    '  AVAIL    LIST            
         DC    X'03',AL1(03,13),X'D3030003C0',C'ASAD'  AVAIL    SEND            
*                                                                               
         DC    X'03',AL1(04,01),X'F404000080',C'    '  AVMKTS   ADD             
         DC    X'03',AL1(04,04),X'F404000080',C'    '  AVMKTS   CHA             
         DC    X'03',AL1(04,02),X'F404000080',C'    '  AVMKTS   DIS             
         DC    X'03',AL1(04,05),X'F404000080',C'    '  AVMKTS   DEL/RES         
         DC    X'03',AL1(04,03),X'F404000080',C'    '  AVMKTS   SEL             
         DC    X'03',AL1(04,08),X'D404000080',C'    '  AVMKTS   XFER            
         DC    X'03',AL1(04,10),X'E404000080',C'    '  AVMKTS   LIST            
*                                                                               
         DC    X'03',AL1(05,01),X'F505000080',C'    '  PROFILE  ADD             
         DC    X'03',AL1(05,04),X'F505000080',C'    '  PROFILE  CHA             
         DC    X'03',AL1(05,02),X'F505000080',C'    '  PROFILE  DIS             
         DC    X'03',AL1(05,05),X'F505000080',C'    '  PROFILE  DEL/RES         
         DC    X'03',AL1(05,03),X'F505000080',C'    '  PROFILE  SEL             
         DC    X'03',AL1(05,10),X'E505000080',C'    '  PROFILE  LIST            
*                                                                               
         DC    X'03',AL1(06,01),X'F606000080',C'    '  COMMENT  ADD             
         DC    X'03',AL1(06,04),X'F606000080',C'    '  COMMENT  CHA             
         DC    X'03',AL1(06,02),X'F606000080',C'    '  COMMENT  DIS             
         DC    X'03',AL1(06,05),X'F606000080',C'    '  COMMENT  DEL/RES         
         DC    X'03',AL1(06,03),X'F606000080',C'    '  COMMENT  SEL             
         DC    X'03',AL1(06,10),X'E606000080',C'    '  COMMENT  LIST            
*                                                                               
         DC    X'03',AL1(07,12),X'D707000018',C'ARAD'  AUDIT    REPORT          
*                                                                               
         DC    X'03',AL1(08,04),X'F808000080',C'    '  AVCOM    CHA             
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPADREPS                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPADDWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPADDSECTS                                                     
         EJECT                                                                  
         PRINT OFF                                                              
SPDEMUPD DSECT                                                                  
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE SPADDFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086SPADD00   05/01/02'                                      
         END                                                                    
