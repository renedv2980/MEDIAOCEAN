*          DATA SET NEPOD00    AT LEVEL 027 AS OF 06/07/17                      
*PHASE T32500E                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE NETUNWK                                                                
*INCLUDE NSIWEEK                                                                
*INCLUDE PNAME                                                                  
*INCLUDE NEPODSCN                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T32500 - RESEARCH WRITER CONTROLLER'                            
T32500   CSECT                                                                  
*        PRINT NOGEN                                                            
         NMOD1 LENWORK,T32500,R7,R6,RR=R2,CLEAR=YES                             
         PRINT NOGEN                                                            
         ST    R2,RELO                                                          
         B     MAIN                                                             
RELO     DS    A                                                                
*                                                                               
MAIN     LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 2000 BYTE I/O AREAS               
         ST    R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         ST    RA,ATWA                                                          
         USING T325FFD,RA                                                       
         LA    RF,SYSD             FIND WORK AREA START                         
         SR    RE,RE                                                            
         LH    RE,=Y(PODBKL-SYSD)  AND OFFSET                                   
         AR    RE,RF               GENERATE REAL ADDRESS                        
         ST    RE,APODBKL          SAVE IT                                      
*                                                                               
         LH    RE,=Y(PODMKTL-SYSD) DO FOR REST                                  
         AR    RE,RF                                                            
         ST    RE,APODMKTL                                                      
*                                                                               
         LH    RE,=Y(PODPRGL-SYSD) DO FOR REST                                  
         AR    RE,RF                                                            
         ST    RE,APODPRGL                                                      
*                                                                               
         LH    RE,=Y(PODINPL-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APODINPL                                                      
*                                                                               
         LH    RE,=Y(PODOBSQ-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APODOBSQ                                                      
*                                                                               
         LH    RE,=Y(PDD2VAL-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDD2VAL                                                      
*                                                                               
         LH    RE,=Y(PDDEML1-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDDEML1                                                      
*                                                                               
         LH    RE,=Y(PDDEML2-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDDEML2                                                      
*                                                                               
         LH    RE,=Y(PDPLGL1-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDPLGL1                                                      
*                                                                               
         LH    RE,=Y(PDPLGL2-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDPLGL2                                                      
*                                                                               
         LH    RE,=Y(PDREQDT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDREQDT         'EXACT' OPTION DATE TABLE                    
*                                                                               
         LH    RE,=Y(PDPDFLT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDPDFLT         'PDF' FILTER PROGRAM/NETWORK TABLE           
*                                                                               
         LH    RE,=Y(PDPRGTT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDPRGTT         'PDF' PROGRAM DATES/TIMES TABLE              
*                                                                               
         LH    RE,=Y(PDAFFLT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDAFFLT         'AFFIL' OPTION TABLE                         
*                                                                               
         LH    RE,=Y(PDUFFLT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDUFFLT         'UAFFIL' OPTION TABLE                        
*                                                                               
         LH    RE,=Y(PDSUBPT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDSUBPT         'SUBPTYPE' FILTER TABLE                      
*                                                                               
         LH    RE,=Y(PDSTSCN-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDSTSCN         STACKED SCREEN BUILD AREA                    
*                                                                               
         LH    RE,=Y(PDSTDEM-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDSTDEM         STACKED DEMO SAVE AREA                       
*                                                                               
         LH    RE,=Y(PDNTIFT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDNTIFT         NTI PROGRAM CODE LIST                        
*                                                                               
         LH    RE,=Y(PDFLBUF-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDFLBUF         FLOWCHART BUFFER                             
*                                                                               
         LH    RE,=Y(PDDMBUF-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,PDADEMTB         DEMO BUFFER                                  
*                                                                               
         LH    RE,=Y(PODNET-SYSD)                                               
         AR    RE,RF                                                            
         ST    RE,APODNET          STATIONS BUFFER                              
*                                                                               
         LH    RE,=Y(PDSYSCD-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDSYSC          SYSCODE BUFFER                               
*                                                                               
         LH    RE,=Y(PVSFILT-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APVSFILT         PVS FILTER TABLE                             
*                                                                               
         LH    RE,=Y(VALBLCK-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,AVALBLCK         VALFILT/VALOPT BLOCK                         
*                                                                               
         LH    RE,=Y(PDNTDMS-SYSD)                                              
         AR    RE,RF                                                            
         ST    RE,APDNTDMS         COMSCORE DEMO NAMES                          
*                                                                               
         OI    GENSTAT1,RDUPAPPL   SET READ FOR UPDATE OFF                      
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         BNE   XIT                                                              
*                                                                               
         TM    REQIND,REQITRN                                                   
         BZ    *+8                                                              
         OI    GENSTAT2,NOREQDET                                                
*                                                                               
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT ACTION FOR GENCON               
         OI    CONSERVH+1,X'01'     SERVICE FIELD IS ALWAYS MODIFIED            
         OI    CONSERVH+6,X'80'                                                 
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*                                                                               
         BAS   RE,CHKTOP           CHECK TOP OF SCREEN                          
*                                                                               
         BAS   RE,GOGENCON         OFF TO GENCON                                
         B     XIT                 THEN WE'RE THROUGH                           
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* INITIALIZE SYSTEM ADDRESSES                                                   
*                                                                               
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
*                                                                               
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(TIOB) A(TIA) A(COMFACS)                    
         L     R2,0(R1)            THIS IS WERE TIOB REALLY IS                  
         ST    R2,ATIOB                                                         
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   DEMOUT,CDEMOUT                                                   
         MVC   GETFACT,CGETFACT                                                 
         MVC   VGETPROF,CGETPROF                                                
         MVC   DEMOMATH,CDEMOMTH                                                
*                                                                               
         LA    R0,GOMSPACK                                                      
         ST    R0,VMSPACK                                                       
         LA    R0,GOMSUNPK                                                      
         ST    R0,VMSUNPK                                                       
*                                                                               
         L     RF,=V(PNAME)                                                     
         A     RF,RELO                                                          
         ST    RF,HISPNAME                                                      
*                                                                               
         L     RF,=V(NSIWEEK)      DATE ROUTINE FOR WTP FILE                    
         A     RF,RELO                                                          
         ST    RF,NSIWEEK                                                       
*                                                                               
         L     RF,=V(NEPODSCN)     SPECIAL SCANNER ROUTINE                      
         A     RF,RELO                                                          
         ST    RF,PODSCAN                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CMASTC-COMFACSD(RF)                                        
         USING MASTD,RF                                                         
         L     RF,MCAEXTRA                                                      
         USING MCEXTRA,RF                                                       
         MVC   PODPASS,MCCSPASS    PASS MODE (1/2 FOR COMSCORE)                 
         DROP  RF                                                               
*                                                                               
*        OC    TWAVPRNT,TWAVPRNT   TEST OFFLINE                                 
*        BNZ   SYS1                YES                                          
*        GOTO1 CSWITCH,DMCB,X'00FFFFFF'  NO-GET PROGRAM NAME                    
*                                                                               
SYS1     LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS4     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS4                                                          
*                                                                               
         L     R2,PODGEN           SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,NSYSCOMM                                                      
*                                                                               
SYS6     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS6                                                          
         EJECT                                                                  
*                                                                               
         MVC   DMCB+4(4),=X'D9000A5A'                                           
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         MVC   VCOMINTR,DMCB       A(COMINTER)                                  
*                                                                               
* OTHER INITIALIZATION                                                          
*                                                                               
*                                  SEED SYSD WITH DUMP COMMENTS                 
         LA    R1,SIDREC           SAVE ADDRESSES IN PODBLOCK                   
         STCM  R1,15,PDSDRCAD        MAY WANT TO USE COVAIL IN 01               
         LA    R1,SIDBUFF            INSTEAD OF THIS.                           
         STCM  R1,15,PDSDBFAD                                                   
         LA    R1,IUNREC                                                        
         STCM  R1,15,PDIUNAD                                                    
         LA    RE,SIDREC                                                        
         LH    RF,=Y(SIDWKLEN)                                                  
         XCEF                                                                   
*                                                                               
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPFCIL,=C'*FACILS*'                                            
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         LH    R1,=Y(BUFF-8-SYSD)                                               
         LA    R1,SYSD(R1)                                                      
         MVC   0(8,R1),=C'**WEEKS*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADBUFF           SAVE START OF BUFF                           
         ST    R1,ADATES                                                        
         ST    R1,AWEEKS                                                        
         LA    R1,NWEEKS*4(R1)                                                  
         MVC   0(8,R1),=C'*MONTHS*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,AMONTHS                                                       
         LA    R1,NMONTHS*4(R1)                                                 
         MVC   0(8,R1),=C'**QTRS**'                                             
         LA    R1,8(R1)                                                         
         ST    R1,AQTRS                                                         
*                                                                               
         L     R1,ADBUFF                                                        
         LA    R1,LDATES(R1)                                                    
         MVC   0(8,R1),=C'**BKLST*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,PDABKLST                                                      
         LA    R1,360(R1)                                                       
         MVC   0(8,R1),=C'**DPG***'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADPGPROG                                                      
         ST    R1,DRSTBUF                                                       
*        LA    R1,3000(R1)                                                      
         LA    R1,4000(R1)                                                      
         ST    R1,DRENDBUF                                                      
         MVC   0(8,R1),=C'**DPGIO*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,DRSPTIO                                                       
*                                                                               
*                                  SET SYSTEM DEPENDENT VALUES                  
         MVC   SYSDUMMY,DUMMY      END OF SYSTEM BASE                           
         MVI   NTWA,1              N'SAVE STORAGE                               
         MVI   SYSTEM,C'F'         FILE MAINT                                   
         MVI   FILTIDNO,2          PROGRAM FILTER FIELD ID NUMBER               
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 1000 BYTES                       
         MVC   GETUSER,VALUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,24          USES GETMSG FOR SYSTEM 24                    
         MVC   LWORK,=AL4(LENWORK)    WE TOOK XXXXX BYTES IN NMOD               
         MVC   LNDEMCOD,=H'3'      SET DEMO CODE ENTRY LENGTH                   
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,DMCB                                                          
         LA    R4,FACTWRK                                                       
         MVC   0(80,R4),0(RF)                                                   
         USING FACTSD,R4                                                        
         CLI   FAOVSYS,2           SPOT SYSTEM?                                 
         BNE   *+18                                                             
         MVI   SYSTEM,C'S'                                                      
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT                            
         B     SYS8                                                             
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   *+18                                                             
         MVI   SYSTEM,C'N'                                                      
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT                            
         B     SYS8                                                             
         CLI   FAOVSYS,8           REP SYSTEM?                                  
         BNE   *+14                                                             
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT                            
         MVI   SYSTEM,C'R'                                                      
*                                                                               
*                                                                               
SYS8     MVC   SYSPHASE,=X'D9032500'    PRESET FOR SYSTEM CALLOVS               
*                                                                               
         CLI   FAOVSYS,0                                                        
         BE    *+10                                                             
         MVC   ALTSYS(1),FAOVSYS   SET FOR PROGRAM RECORDS                      
         DROP  R4                                                               
         LA    R1,RECACTS                                                       
         CLC   AGENCY,=C'WI'       FOR WILA ALLOW TRANSMIT                      
         BNE   *+8                                                              
         LA    R1,RECACTS2                                                      
         ST    R1,ARECACT          RECORD/ACTION DIRECTORY                      
*                                                                               
         LH    R1,=Y(STARTSV-SYSD)                                              
         LA    R1,SYSD(R1)                                                      
         ST    R1,ASTARTSV                                                      
*                                                                               
         MVC   PDPRINT,VPRINT      A(PRINT)                                     
*                                                                               
         GOTO1 INITDRON            INITIALIZE DRONE EARLY                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,CONRECH+5                                                   
         BZ    SYSINTX                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONREC(0),=C'TRANSMIT'   IF TRANSMIT, SET BIT                    
         BNE   *+8                                                              
         OI    REQIND,REQITRN                                                   
*                                                                               
SYSINTX  CR    RB,RB               NORMAL EXIT                                  
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE EXECUTED BEFORE GOING TO GENCON                                       
*                                                                               
CHKTOP   LR    R0,RE                                                            
*                                                                               
         ZIC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BM    CT10                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'LIST'  UNLESS ACTION=LIST,                          
         BE    CT50                                                             
CT10     LA    RE,SPLCODH                                                       
         ST    RE,AFRSTKEY         GIVE GENCON A(FIRST KEY FIELD)               
         LA    RE,SPLNAMH          AND A(FIRST REC FIELD),                      
         ST    RE,AFRSTREC         TO AVOID SPORADIC DUMPS                      
*                                                                               
CT50     ZIC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BM    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'REPORT' IF ACTION IS NOT REPORT                   
         BE    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'LIST'  OR LIST                                      
         BE    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'SELECT'   OR REPORT SELECT                          
         BE    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'DISPLAY'   OR DISPLAY                               
         BE    CTX                                                              
         XC    CONWHEN,CONWHEN     THEN ERASE PRINT OPTION FIELD                
         OI    CONWHENH+6,X'80'                                                 
         MVI   CONWHENH+5,0                                                     
CTX      LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE HANDLES PASSING OF CONTROL TO AND FROM GENCON                         
***********************************************************************         
*                                                                               
GOGENCON NTR1                                                                   
         BAS   RE,SETRD            SET RD SO GENCON ALWAYS RETURNS              
*                                                                               
GOG10    MVI   GOAGAIN,C'N'        INITIALIZE RETURN SWITCH                     
*                                                                               
GOG20    GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON - PASS A(W/S)                  
*                                                                               
         CLI   PDOVSYS,3           NET SYSTEM                                   
         BE    GOG25               DON'T CHANGE SCREEN                          
         CLI   TWASCR,X'E0'                                                     
         BNE   GOG25                                                            
*                                                                               
         MVC   SPLNETH-8(8),=C'Stations'                                        
         OI    SPLNETH-10,X'80'    TRANSMIT                                     
         OI    SPLPSTH-30,X'2C'    PROTECT & HIDE                               
         OI    SPLPSTH-25,X'80'    TRANSMIT                                     
         OI    SPLPSTH+1,X'20'     PROTECT IT                                   
         OI    SPLPSTH+6,X'80'     TRANSMIT IT                                  
         OI    SPLPENH-10,X'2C'                                                 
         OI    SPLPENH-5,X'80'                                                  
         OI    SPLPENH+1,X'20'        "                                         
         OI    SPLPENH+6,X'80'        "                                         
*                                                                               
GOG25    CLI   GOAGAIN,C'Y'        REQUEST BY APPLIC. TO GO BACK                
         BE    GOG10                                                            
         ICM   R1,15,AFRSTKEY      IF CURSOR IS AT FIRST KEY FIELD              
         BZ    GOGX                                                             
         TM    6(R1),X'40'                                                      
         BZ    GOGX                                                             
         CLI   OKNO,2              AND GENCON IS ASKING FOR MORE INPUT          
         BNE   GOG30                                                            
         CLI   GOAGAIN,C'K'        AND WE DIDN'T TRY THIS ALREADY               
         BE    GOG30                                                            
         CLI   ACTNUM,ACTREP       AND IF ACTION IS REPORT                      
         BNE   GOGX                                                             
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
GOGX     B     XIT                 ALL THROUGH                                  
*                                                                               
*                                                                               
*                                                                               
SETRD    NTR1                                                                   
         ST    RD,SYSRD                                                         
         B     XIT                                                              
*                                                                               
*                                                                               
PLSENTER MVI   PERROR+1,2          PLEASE ENTER FIELDS AS REQUIRED              
         MVI   PMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         L     R2,AFRSTKEY         R2 TO 1ST KEY FIELD                          
         L     RE,ATIOB            RE =A(TRANSLATOR I/O BLOCK)                  
         NI    TIOBINDS-TIOBD(RE),X'FF'-TIOBALRM  TURN OFF BEEP                 
         GOTO1 PDSERR                                                           
*                                                                               
SELFIRST MVI   PERROR+1,10          SELECT OR HIT ENTER FOR FIRST               
         B     *+8                                                              
SELNEXT  MVI   PERROR+1,9           SELECT OR HIT ENTER FOR NEXT                
         MVI   PMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         MVI   OKNO,0              CLEAR OKNO SO WON'T LOOP ENDLESSLY           
         L     R2,AFRSTREC         R2 TO 1ST SEL FIELD                          
         GOTO1 PDSERR                                                           
         B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
***************************************************************                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=*,LABEL=*,WORK=(R2,8)                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R2                                                      
         XC    0(32,R2),0(R2)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,PDAPROF+7                                               
         MVI   STAPMED,C'T'        TV                                           
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 STAPACK,(R2)                                                     
         CLI   STAPERR,0                                                        
         BE    GOMSP10                                                          
         MVI   ERROR,INVALID                                                    
         B     GOMSPX                                                           
*                                                                               
GOMSP10  L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
*                                                                               
GOMSPX   XIT1                                                                   
         DROP  R2                                                               
*                                                                               
GOMSUNPK NTR1  BASE=*,LABEL=*,WORK=(R2,8)                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R2                                                      
         XC    0(32,R2),0(R2)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,PDAPROF+7                                               
         MVI   STAPMED,C'T'        TV                                           
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 STAPACK,(R2)                                                     
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QGENCON)                                                     
         DC    AL1(QDRONE)                                                      
         DC    AL1(QDEFINE)                                                     
         DC    AL1(QGETNUN)                                                     
         DC    AL1(QGETHUT)                                                     
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QNETWEEK)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QPDWRIGN)                                                    
         DC    AL1(QDEMAND)                                                     
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QNETUNBK)                                                    
         DC    AL1(QMSPACK)                                                     
         DC    AL1(QMSUNPK)                                                     
         DC    AL1(QRANSID)                                                     
         DC    AL1(QSPDEMUP)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QSPGETIUN)                                                   
         DC    AL1(QREGETIUN)                                                   
         DC    AL1(QGENPRG)                                                     
         DC    AL1(QSTAPACK)                                                    
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
TABLES   DS    0A                                                               
         DC    A(RECACTS)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
SYSVCON  DS    0A                                                               
         DC    V(BINSRCH)                                                       
         DC    V(COVAIL)                                                        
         DC    V(DUMMY)                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLES OF RECORDS ACTIONS AND COMBINATIONS                                    
*                                                                               
RECACTS  DS    0D                                                               
*                                                                               
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
*                                                                               
         DC    X'04',C'RESEARCH',AL1(01),X'0000'    WRITER                      
*                                                                               
*                                                                               
*                                                                               
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
*                                                                               
*                                                                               
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
*                                                                               
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,12),X'E001000138',C'PDPD'  WRITER REPORT            
         DC    X'FF'                                                            
*                                                                               
*              PHASE USED UP BY SYSTEM SO FAR                                   
*                                                                               
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     Y  Y     Y                                                      
*        FX        Y                                         Y                  
         EJECT                                                                  
* TABLES OF RECORDS ACTIONS AND COMBINATIONS #2                                 
*                                                                               
* >>>>>>>>>> FOR TEST AGENCY SRW ONLY <<<<<<<<<<<                               
*            (GETS SOON FOR REPORTS)                                            
*                                                                               
RECACTS2 DS    0D                                                               
         DC    X'04',C'RESEARCH',AL1(01),X'0000'    WRITER                      
         DC    X'04',C'TRANSMIT',AL1(02),X'0000'    TRANSMIT WRITER             
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
*                                                                               
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,12),X'E001000138',C'PDPD'  WRITER REPORT            
         DC    X'03',AL1(02,12),X'E001000138',C'PDPT'  TRANSMIT WRITER          
         DC    X'FF'                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*SIDREC*'                                                      
SIDREC   DC    2000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*SIDBUFF'                                                      
SIDBUFF  DS    5XL256                                                           
SIDBUFFX EQU   *                                                                
*------------------------------------------------------------                   
         DS    0D                                                               
         DC    CL8'*IUNREC*'                                                    
IUNREC   DS    1024C               IUN RECORD BUILT HERE                        
*                                                                               
IUNDEMS  EQU   32                                                               
IUNHMDSP EQU   80                                                               
*                                                                               
**************** I U N   W O R K   A R E A  ***************                     
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'**IUNWK*'                                                    
IUNWK    DS    0C                                                               
*                                                                               
IUNVS    DS    (IUNDEMS)F                                                       
IUNVX    EQU   *                                                                
*                                                                               
IUNOLD   EQU   *                                                                
IRTGOLD  DS    (IUNDEMS)F                                                       
IIMPOLD  DS    (IUNDEMS)F                                                       
IPUTOLD  DS    (IUNDEMS)F                                                       
ITOTOLD  DS    (IUNDEMS)F                                                       
IUNOLDX  EQU   *                                                                
*                                                                               
IUNLEN   EQU   *-IUNWK+12          UNVS/RTGS/IMPS/PUTS/TOTS + VUTS              
*                                                                               
IUNNEW   EQU   *                                                                
IRTGNEW  DS    (IUNDEMS)F                                                       
IIMPNEW  DS    (IUNDEMS)F                                                       
IPUTNEW  DS    (IUNDEMS)F                                                       
ITOTNEW  DS    (IUNDEMS)F                                                       
IUNNEWX  EQU   *                                                                
*                                                                               
IUNXTRA  EQU   *                                                                
IUNVUT   EQU   *                                                                
IUNSHMS  DS    F                                                                
IUNSMETA DS    F                                                                
IUNSMETB DS    F                                                                
*                                                                               
ILUNVS   DC    (IUNDEMS)F'0'                                                    
IUNWKLEN EQU   *-IUNWK                                                          
SIDWKLEN EQU   *-SIDREC                                                         
         EJECT                                                                  
       ++INCLUDE NEPODWORK                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*        INCLUDE DDSPOOLD                                                       
*        INCLUDE DDSPLWORKD                                                     
*        INCLUDE SPWRIFFD                                                       
*        INCLUDE DDGENTWA                                                       
*        INCLUDE SPGENAGY                                                       
*        INCLUDE SPGENCLT                                                       
*        INCLUDE SPGENPRD                                                       
*        INCLUDE SPGENPRG                                                       
*        INCLUDE SPGENEST                                                       
*        INCLUDE SPGENMKG                                                       
*        INCLUDE SPGENMKT                                                       
*        INCLUDE SPGENSTA                                                       
*        INCLUDE CTGENFILE                                                      
*        INCLUDE FAFACTS                                                        
*        INCLUDE FATIOB                                                         
*        INCLUDE DDCOMFACS                                                      
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE DRDICFILE                                                      
*        INCLUDE DDBIGBOX                                                       
*        INCLUDE DDWIDED                                                        
*        INCLUDE DDOFFICED                                                      
*        INCLUDE DRONEBLKHD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE NEPODFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEPODE0D                                                       
       ++INCLUDE DDGENTWA                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKG                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FATCB                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027NEPOD00   06/07/17'                                      
         END                                                                    
