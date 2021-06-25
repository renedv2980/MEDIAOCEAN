*          DATA SET RERMP00    AT LEVEL 002 AS OF 07/21/10                      
*PHASE T81000C                                                                  
*INCLUDE UNBOOK                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE UNUPGR                                                                 
*INCLUDE RETEXT                                                                 
*INCLUDE UNTEXT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE UPOUT                                                                  
*INCLUDE INVDAY                                                                 
*INCLUDE PAVSTA                                                                 
*INCLUDE DEMTIME                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE GETKSRC                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T81000 - RERMP00 - REP RESEARCH FILE MAINTENANCE (RMP)'         
*                                                                               
*******************************************************************             
*                                                                 *             
*        RERMP00 --- REP SUPER FILE MAINT BASE                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
* JUN30/00 (BU ) --- REMOVE REFERENCES TO GLV1GOTO PER MEL H.     *             
*                                                                 *             
* AUG02/01 (BU ) --- REMOVE 'DTRANS' FROM ACCEPTABLE REQUESTS     *             
*                                                                 *             
* AUG29/01 (BU ) --- REMOVE 'LTRANS' FROM ACCEPTABLE REQUESTS     *             
*                                                                 *             
* FEB25/02 (BU ) --- DELETE 'SOON' FOR ODELETE ACTION             *             
*                                                                 *             
* MAR01/02 (BU ) --- RESTORE 'SOON' FOR ODELETE ACTION            *             
*                                                                 *             
* MAR06/02 (SKU) --- DISABLE SWAP ACTION                          *             
*                                                                 *             
* JUL29/02 (BU ) --- CHANGE REPORT CODE FOR TCOPY TO 'TC'         *             
*                                                                 *             
* AUG22/02 (HQ ) --- CHANGE READ TO CTFILE TO BE NON-UPDATIVE     *             
*                    PER ALAN A.                                  *             
*                                                                 *             
* SEP11/02 (BU ) --- SET TCOPY SOON BIT IN TABLE                  *             
*                                                                 *             
* JAN09/03 (BU ) --- DEACTIVATE ETRANS FOR FORSEEABLE FUTURE      *             
*                                                                 *             
* FEB06/03 (BU ) --- REACTIVATE ETRANS FOR ISDN INTERFACE         *             
*                                                                 *             
* OCT21/03 (BU ) --- ESTABLISH  XTRANS FOR UNIX FILE TESTING      *             
*                                                                 *             
* MAY28/04 (BU ) --- STATION VALIDATION: ALLOW NUMERIC CHAR       *             
*                                                                 *             
* JUL31/08 (KUI) --- CHANGE CALL TO LOCKET TO NOT USE R2          *             
*                                                                 *             
* MAR  /09 (BOB) --- NEW INVENTORY RECORD KEY                     *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*HERE**************************************************************             
*                                                                               
T81000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T81000,R7,R5,RR=R2,CLEAR=YES                           
         USING GEND,RC                                                          
         SPACE 1                                                                
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         SPACE 1                                                                
         ST    R2,RELO                                                          
         ST    RD,SAVERD                                                        
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(LENIOAS)      R9=A(SFM SYSTEM WORKING STORAGE)             
*                                  GRABBING 3 2096 BYTE I/O AREAS               
         MVI   GCMODE,0                                                         
         SPACE 1                                                                
         ST    R1,SYSPARMS                                                      
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
         SPACE 1                                                                
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
         SPACE 1                                                                
         L     RA,4(R1)                                                         
         ST    RA,ATWA             RA=A(TWA)                                    
         USING CONHEADH-64,RA                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         SPACE 1                                                                
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    R7,SYSR7            SECOND BASE REGISTER                         
         ST    R5,SYSR5            THIRD BASE REGISTER                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LHI   R2,(CORETAB-T81000)                                              
         A     R2,SYSRB                                                         
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
SFM20    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0         GO TO CALLOV                                 
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SFM20                                                         
*                                                                               
         MVI   DMCB+7,QSTAPACK                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,DMCB        SAVE MODULE ADDRESS                         
*                                                                               
         LA    R0,GOMSPACK                                                      
         ST    R0,MSPACK                                                        
         LA    R0,GOMSUNPK                                                      
         ST    R0,MSUNPK                                                        
*                                                                               
         BAS   RE,CKGLOB                                                        
         SPACE 1                                                                
         BAS   RE,SETPROFS                                                      
         SPACE 1                                                                
         MVI   RETURNED,0          HELPS DETERMINE FROM WHENCE WE CAME          
*                                                                               
*        ANALYZE INPUT TO SEE IF WE NEED TO INTERVENE                           
*        ON SELECT ACTIONS                                                      
*                                                                               
         CLI   TWASCR,X'D1'        LOOKING FOR TEXT MAINTENANCE                 
         BNE   TXRECN                                                           
*                                                                               
         MVC   MYSCRNUM,TWASCR     UPDATE SCREEN NUMBER                         
*                                                                               
         CLI   PFKEY,12            SKIP IF PF12 HIT                             
         BE    TXRECX                                                           
*                                                                               
         CLC   =C'TEXT',CONREC     DOING 'TEXT' MAINTENANCE                     
         BNE   TXRECX                                                           
*                                                                               
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   *+18                                                             
         CLI   THISLSEL,C'D'       SKIP IF LAST ACTION WAS DELETE               
         BE    *+10                                                             
         MVC   CONACT(3),=C'CHA'      CHANGE ACTION                             
*                                                                               
         CLC   =C'COP',CONACT      CHANGE COPY TO 'ADD'                         
         BNE   *+14                                                             
         MVI   ORIGACT,C'C'           SET ORIGINAL ACTION                       
         MVC   CONACT(3),=C'ADD'      CHANGE ACTION                             
*                                                                               
TXRECX   DS    0H                                                               
*                                                                               
         B     GOGENCON                                                         
*                                                                               
TXRECN   DS    0H                                                               
*                                                                               
         B     GOGENCON                                                         
*                                                                               
GOGENCON DS    0H                                                               
*                                                                               
         MVI   RACHANG,C'N'        SET FLAG TO IDICATE IF USER                  
         TM    CONRECH+4,X'20'     CHANGED RECORD/ACTION                        
         BZ    GOGEN1                                                           
         CLC   =C'TEXT',CONREC     SKIP IF TEXT SCREEN                          
         BE    GOGEN2                                                           
         TM    CONACTH+4,X'20'                                                  
         BO    *+8                                                              
GOGEN1   DS    0H                                                               
         MVI   RACHANG,C'Y'                                                     
GOGEN2   DS    0H                                                               
         SPACE 2                                                                
* CALL A ROUTINE THAT CALLS GENCON SO THAT GENCON WILL RETURN TO                
* NEXT INSTRUCTION AFTER THIS                                                   
* DON'T GO AGAIN UNLESS OVERLAY WANTS TO CALL ANOTHER OVERLAY                   
         SPACE 1                                                                
AGAIN    MVI   GOAGAIN,C'N'                                                     
         BAS   RE,CALLGENC                                                      
         SPACE 1                                                                
* IF OVERLAY WISHED TO CALL GENCON WITH A NEW RECORD AND ACTION,                
* THEN THIS FLAG WILL BE SET                                                    
         SPACE 1                                                                
         CLI   GOAGAIN,C'Y'                                                     
         BNE   DONE                                                             
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT HERE                 
         B     AGAIN                                                            
         SPACE 1                                                                
DONE     OI    CONRECH+4,X'20'                                                  
         OI    CONACTH+4,X'20'                                                  
         B     XIT                 ELSE EXIT BACK TO USER                       
         EJECT                                                                  
*THIS ROUTINE CALL GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON               
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.                
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW                    
* RECORD AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION              
* AND A SELECTION WAS MADE.                                                     
         SPACE 2                                                                
CALLGENC NTR1                                                                   
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
         SPACE 1                                                                
         GOTO1 GENCON,DMCB,(R8)    CALL GENCON-PASS A(WORKING STORAGE)          
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
*              INITIALIZE SYSTEM DEPENDENT VALUES                               
         SPACE 3                                                                
SYSINIT  NTR1                                                                   
*---------------------------------------------------------------                
         OI    GENSTAT3,OKVALSEL                                                
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   DON'T BOTHER IF OFFLINE                      
         BNZ   *+8                                                              
         OI    GENSTAT1,APPLIC     TO LOAD T81018 INTO CORE                     
*---------------------------------------------------------------                
         MVI   CTRLMAIN,C'N'       (RE)SET SYSTEM SE# FLAG                      
*              GET TERMINAL VALUES                                              
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
         SPACE 1                                                                
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
SYS20    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS20                                                         
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R1,VCOUNT                                                        
         SPACE 1                                                                
SYS40    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R1,SYS40                                                         
         SPACE 1                                                                
*                                  SET SYSTEM DEPENDENT VALUES                  
         SPACE 1                                                                
         LA    R1,STARTSAV                                                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'81'          SAVE 1 LARGE TWA                             
         MVI   SYSTEM,C'R'         REP                                          
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2096 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETREP      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'27'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         OI    GENSTAT2,DISTHSPG   STAY ON SAME LIST PAGE ON RETURN             
         MVI   GETMSYS,8           USES GETMSG FOR SYSTEM 8                     
         MVC   LWORK,=Y(LENWORK)   SPACE TAKEN IN NMOD                          
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9081000'    PRESET FOR SYSTEM CALLOVS               
         LHI   R1,(RECACT-T81000)  RECORD/ACTION DIRECTORY                      
         A     R1,SYSRB                                                         
         ST    R1,ARECACT                                                       
*                                                                               
*                                                                               
         SPACE 1                                                                
* SET UP CERTAIN ROUTINE ADDRESSES - CAN'T WAIT FOR GENCON                      
         SPACE 1                                                                
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMAND,CDEMAND                                                   
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   DEMOUT,CDEMOUT                                                   
         SPACE                                                                  
         OI    GENSTAT3,USEKEYSV   USE KEYSAVE,KEY (INTEREP)                    
         SPACE 1                                                                
*   IF ONLINE,CHECK AUTHORIZATION VALUES FOR ACTION                             
         OC    TWAVPRNT,TWAVPRNT                                                
         BNZ   XIT                                                              
*                                                                               
* FOR RECORD DR (DIRECT RESPONSE) AND RECORD GOALN,                             
*     ACTIONS LIST AND REPORT ARE VALID FOR ALL                                 
*                                                                               
         CLC   =C'GOALN',CONREC    GOALN?                                       
         BE    INIT0020            YES - CHECK FOR REPORT OR LIST               
         CLC   =C'DR',CONREC       DR?                                          
         BE    INIT0020            YES - CHECK FOR REPORT OR LIST               
         CLC   =C'GOAL',CONREC     GOAL?                                        
         BE    INIT0040            YES - CHECK FOR LIST ONLY                    
         CLC   =C'DRN',CONREC      DRN?                                         
         BE    INIT0040            YES - CHECK FOR LIST ONLY                    
         B     INIT0080            ALL OTHERS - FULL CHECK                      
INIT0020 EQU   *                                                                
         CLC   =C'REP',CONACT                                                   
         BE    XIT                                                              
INIT0040 EQU   *                                                                
         LA    RF,CONACTH          A(ACTION FIELD HEADER)                       
         ZIC   RE,5(RF)            LOAD LENGTH OF INPUT                         
         BCTR  RE,0                                                             
         EX    RE,INIT0060         COMPARE ACTION TO 'LIST'                     
         BE    XIT                 EQUAL:  ACCEPTED                             
         B     INIT0080            NOT EQUAL:  CHECK RESTRICTS                  
INIT0060 CLC   8(0,RF),=C'LIST'                                                 
INIT0080 EQU   *                                                                
         SPACE 1                                                                
         LHI   R1,AUTHTAB-T81000   SEE IF ACTION HAS RESTRICTIONS               
         LA    R1,T81000(R1)                                                    
         LA    R2,CONRECH                                                       
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
INIT0100 EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    INIT0120                                                         
         CLI   0(R1),X'FF'                                                      
         BE    XIT                 NO RESTRICTIONS                              
         LA    R1,L'AUTHTAB(R1)                                                 
         B     INIT0100                                                         
         SPACE 1                                                                
INIT0120 MVC   BYTE,8(R1)          NOW SEE IF USER MEETS RESTRICTIONS           
         NC    BYTE,TWAAUTH                                                     
         BNZ   XIT                                                              
*                                                                               
INIT0140 DS    0H                                                               
         XC    CONHEAD,CONHEAD     NEED TO DO MY OWN ERR MSG                    
         MVC   CONHEAD(L'NOAUTH),NOAUTH                                         
         OI    6(R2),X'40'         POSITION CURSOR                              
         OI    CONHEADH+6,X'80'     TRANSMIT HEADER                             
         L     RD,SAVERD           BACK OUT ALL THE WAY                         
         B     XIT                                                              
         SPACE 1                                                                
NOAUTH   DC    C'* ERROR * SECURITY LOCKOUT'                                    
         EJECT                                                                  
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     RA,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     R7,SYSR7            2ND BASE REG - R7 MUST BE SET FIRST          
         L     R5,SYSR5            3RD BASE REG - R5 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VUSER                                                            
         B     VTXT                                                             
         B     DTXT                                                             
         B     VRPT                REPORT                                       
         B     VSVC                SERVICE                                      
         B     VSTA                STATION                                      
         B     VDPT                DAYPART                                      
         B     VGRP                GROUP                                        
         B     VOFF                OFFICE                                       
         B     VADV                ADVERTISER                                   
         B     VBOOK               BOOKS WITH BOOKTYPE OPTION                   
         B     VBKL                BOOKS WITH LABEL OPTION                      
         B     DBKL                DISPLAY BOOKS WITH LABEL OPTION              
         B     VDEMO               DEMOS                                        
         B     DDEMO               DISPLAY DEMOS                                
         B     VCON                CONTRACT                                     
         B     BDUP                BUILD DEFAULT UPGRADE EXPRESSION             
         B     SWISPT              SWITCH TO SPOT SYSTEM                        
         B     SWIREP              SWITCH TO REP SYSTEM                         
         B     VSID                VALIDATE SCHEME/PERIOD/(YEAR)                
         B     PACK                PACK                                         
         B     DLEN                DISPLAY LENGTH                               
         B     VSOURCE             VALIDATE SOURCE FIELD (I OR S)               
         B     VINV                VALIDATE INVENTORY NUMBER                    
         B     DINV                DISPLAY INVENTORY NUMBER                     
         B     DINVDTT             DISPLAY INVENTORY DAY/TIME/TITLE             
         B     VUPT                VALIDATE UPGRADE EXPRESSION                  
         B     CPROG               SWITCH BETWEEN OVERLAYS                      
         B     RPROG               RETURN TO PREVIOUS OVERLAY                   
         B     DPURDTT             DISPLAY PURE DAY/TIME/TITLE                  
         B     MYERR               CALL GETTXT FOR MESSAGE                      
         B     GETINVN             READ BEST FIT INVENTORY RECORD               
         B     VLOC                                                             
         B     VDPTM               VALIDATE MULTIPLE DAYPARTS                   
         B     VBKDAT              CHECK BOOK DATE <= TODAY'S DATE              
         B     PFPROC              ADDRESS TO PFPROM ROUTINE                    
         B     LTRANS              ISSUE LTRANS REQUEST                         
         B     INMENADD            ADD INV REC TO STATIONS IN MENU              
         B     VAGY                VALIDATE AGENCY AND GET PARENT REP           
         B     GETBKTP             GET 2-CHAR BOOKTYPE                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              USER REP                                                         
         SPACE 3                                                                
* VUSER - GET REP DATA FROM CONTROL FILE USER ID RECORD                         
*                                                                               
VUSER    MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         CLI   TWASCR,X'D1'        LOOKING FOR TEXT MAINTENANCE                 
         BNE   *+10                                                             
         MVC   MYSCRNUM,TWASCR     UPDATE SCREEN NUMBER                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VU10                                                             
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         SPACE 1                                                                
VU10     XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    VUSERX              YES -- THERE CAN'T BE ANY PFKEYS             
         SPACE 1                                                                
         CLI   RACHANG,C'Y'        IF USER CHANGED RECORD/ACTION                
         BNE   *+14                                                             
         MVI   CALLSP,0            THEN RESET STACK POINTER                     
         XC    CALLSTCK,CALLSTCK                                                
         SPACE 1                                                                
         CLI   PFKEY,0             WAS ENTER PRESSED?                           
         BE    VUSERX              YES -- NORMAL EXIT                           
         CLI   PFKEY,12            WAS PF12 PRESSED?                            
         BNE   VU20                NO                                           
         CLI   CALLSP,0            YES -- DO WE HAVE SOMEWHERE TO GO?           
         BE    VUSERX              NO -- NORMAL EXIT                            
         SPACE 1                                                                
*                                                                               
         OC    SVLIST(188),SVLIST                                               
         BZ    *+16                                                             
         MVC   LISTDIR(188),SVLIST                                              
         MVC   LSTONTWA(80),SVLIST+188                                          
*                                                                               
         MVC   RETURNED,PFKEY      INDICATE THAT RETURN IS IN PROGRESS          
         MVI   PFKEY,0             THEN RESET PFKEY                             
         GOTO1 RETPROG             AND RETURN TO OVERLAY                        
         B     VUSERX                                                           
                                                                                
VU20     DS    0H                                                               
*                                                                               
*        XC    SVLIST,SVLIST                                                    
*        CLC   CONACT(3),=CL3'SEL'                                              
*        BNE   *+10                                                             
*        MVC   SVLIST,LISTDIR      SAVE LIST POINTERS                           
*                                                                               
         PRINT GEN                                                              
PFPROC   GOTO1 =A(VPFPROC),DMCB,(RC),RR=Y                                       
         PRINT NOGEN                                                            
         B     VUSERX                                                           
*                                                                               
LTRANS   DS    0H                  ISSUE LTRANS REQUEST                         
         GOTO1 =A(LTRANSR),RR=Y                                                 
         B     VUSERX                                                           
*                                                                               
VUSERX   B     XIT                                                              
         SPACE 3                                                                
*              HANDLE TEXT ELEMENTS                                             
         SPACE 3                                                                
VTXT     CLI   MODE,DISPREC                                                     
         BE    DTXT                                                             
         ZIC   R0,MAX              MAX FIELDS                                   
         LA    R4,1                SEQUENCE NUMBER                              
         GOTO1 REMELEM             TAKE OUT EXISTING X'02' ELEMENTS             
         SPACE 1                                                                
VTXT20   MVC   ELEMENT(2),=X'0289' BUILD NEW X'02' ELEMENTS                     
         MVC   ELEMENT+3(67),SPACES                                             
         CLI   5(R2),0                                                          
         BE    VTXT40                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+3(0),8(R2)  MOVE IN RIGHT HAND SIDE OF LINE              
VTXT40   STC   R4,ELEMENT+2        SEQUENCE NO.                                 
         SPACE 1                                                                
         BAS   RE,BUMP                                                          
         BCTR  R0,0                                                             
         MVC   ELEMENT+70(67),SPACES                                            
         CLI   5(R2),0                                                          
         BE    VTXT60                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+70(0),8(R2)  MOVE IN LEFT HAND SIDE OF LINE              
         SPACE 1                                                                
VTXT60   GOTO1 ADDELEM                                                          
         BAS   RE,BUMP                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,VTXT20                                                        
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        NOW UPDATE X'01' ELEMENT                     
         BAS   RE,GETEL                                                         
         USING ROVRELEM,R6                                                      
         MVC   ROVRDATE,BTODAY     DATE                                         
         BAS   RE,GETTIME                                                       
         MVC   ROVRTIME,WORK       TIME                                         
         DROP  R6                                                               
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 1                                                                
DTXT     ZIC   R0,MAX              DISPLAY UP TO MAX FIELDS                     
         L     R6,AIO              USER SUPPLIED ELCODE                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
DTXT20   BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST HAVE MAX NUMBER OF TXT ELEMS            
         MVC   8(67,R2),3(R6)       PICK OUT DATA                               
         OI    6(R2),X'80'         AND TRANSMIT                                 
         BAS   RE,BUMP                                                          
         BCTR  R0,0                                                             
         MVC   8(67,R2),70(R6)                                                  
         OI    6(R2),X'80'         AND TRANSMIT                                 
         BAS   RE,BUMP                                                          
         BCT   R0,DTXT20                                                        
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*  VALIDATE REPORT                                                              
         SPACE 1                                                                
VRPT     GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),2             MUST BE 2 CHARACTERS                         
         BNE   MYERR                                                            
         LA    R3,RPTLIST                                                       
RPT10    CLI   0(R3),X'FF'                                                      
         BE    MYERR                                                            
         CLC   0(2,R3),8(R2)                                                    
         BE    RPTX                                                             
         LA    R3,2(R3)                                                         
         B     RPT10                                                            
         SPACE 1                                                                
RPTX     B     XIT                                                              
         SPACE 3                                                                
RPTLIST  DS    0CL2                                                             
         DC    CL2'MO'                                                          
         DC    CL2'TU'                                                          
         DC    CL2'WE'                                                          
         DC    CL2'TH'                                                          
         DC    CL2'FR'                                                          
         DC    CL2'SA'                                                          
         DC    CL2'SU'                                                          
         DC    CL2'RA'              ROLLING AVERAGE                             
         DC    CL2'OT'              OTHER                                       
         DC    CL2'SS'              SATURDAY/SUNDAY                             
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*  VALIDATE SERVICE                                                             
*                                                                               
VSVC     DS    0H                                                               
*                                                                               
         MVC   RERROR,=AL2(INVALID)                                             
*                                                                               
         CLI   5(R2),3             MAX 3 CHARACTERS                             
         BH    MYERR                                                            
*                                                                               
         LA    R3,SVCLIST          POINT TO LIST OF VALID SERVICES              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET INPUT LENGTH                             
         BZ    SVCFND              NO ENTRY - USE FIRST IN LIST                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
SVCLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R3),X'FF'         ERROR IF END OF LIST REACHED                 
         BE    MYERR                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),8(R2)                                                    
         BE    SVCFND                                                           
*                                                                               
SVCCONT  DS    0H                                                               
*                                                                               
         LA    R3,3(R3)            BUMP TO NEXT SERVICE IN LIST                 
         B     SVCLOOP                                                          
*                                                                               
SVCFND   DS    0H                                                               
*                                                                               
         MVC   CSOURCE,0(R3)       SAVE SERVICE CODE                            
         MVC   8(3,R2),0(R3)       RE-DISPLAY FOUND SERVICE                     
         OI    6(R2),X'80'         FORCE TRANSMISSION                           
*                                                                               
SVCX     DS    0H                                                               
         B     XIT                                                              
*                                                                               
*        LIST OF VALID SERVICES. DEFAULT IS FIRST IN LIST                       
*                                                                               
SVCLIST  DS    0CL3                                                             
         DC    CL3'NSI'                                                         
         DC    CL3'MFX'                                                         
         DC    CL3'SRC'                                                         
******   DC    CL3'ARB'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* VALIDATE STATION CALL LETTERS                                                 
* ON EXIT, CALL LETTERS ARE IN WORK                                             
*                              WORK+4  - A=AM F=FM C=CM T=BLANK                 
*                              WORK+10 - MARKET NAME                            
*                              WORK+40 - 1 OR 2 IF SATELLITE STATION            
*                              WORK+41 - GROUP/SUBGROUP CODE                    
         SPACE                                                                  
VSTA     DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         MVC   CSTAT,SPACES        INIT SAVE AREAS                              
         MVC   CMARKET,SPACES                                                   
         MVC   CMKTNAM,SPACES                                                   
         MVC   CSTAGRUP,SPACES                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVC   RERROR,=AL2(INVSTA)                                              
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   0(R4),3                                                          
         BL    MYERR                                                            
         CLI   0(R4),4                                                          
         BH    MYERR                                                            
***      TM    2(R4),X'40'         TEST ALPHA                                   
***      BZ    MYERR                                                            
         MVC   WORK(4),12(R4)      SAVE CALL LETTERS                            
         MVC   CSTAT(4),12(R4)     SAVE CALL LETTERS                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)          DEFAULT = TV                                 
         BZ    VS100               YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV            TV LEAVE BLANK                               
         BE    VS100                                                            
         MVI   WORK+4,C'A'         AM = A                                       
         EX    RE,STAAM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'F'         FM = F                                       
         EX    RE,STAFM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'L'         LOW FREQUENCY = L                            
         EX    RE,STALF                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'C'         CM = C                                       
         EX    RE,STACM                                                         
         BE    VS100                                                            
*                                                                               
         MVI   WORK+4,C'H'         NETWORK NHTI FILE?                           
         EX    RE,STANHT                                                        
         BNE   VS45                                                             
         MVC   CSTAT(3),WORK                                                    
         MVC   CSTAT+3(2),=C' H'                                                
         MVC   CMKTNAM,=C'NHTI'                                                 
         B     VS100                                                            
*                                                                               
VS45     MVI   WORK+4,C' '         MAY BE SATELLITE STATION                     
         EX    RE,STA1                                                          
         BNE   VS50                                                             
         MVI   WORK+40,C'1'                                                     
         B     VS100                                                            
VS50     EX    RE,STA2                                                          
         BNE   MYERR                                                            
         MVI   WORK+40,C'2'                                                     
*                                                                               
VS100    DS    0H                                                               
         MVC   CSTAT+4(1),WORK+4   SAVE BROADCAST BAND                          
         CLI   CSTAT+4,C' '        IF BLANK                                     
         BH    *+8                                                              
         MVI   CSTAT+4,C'T'           FORCE TO TV                               
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY     REP                                          
         MVC   RSTAKSTA,WORK       STATION                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         USING RSTAELEM,R6                                                      
         MVC   WORK+10(L'RSTAMKT),RSTAMKT                                       
         MVC   CMKTNAM,RSTAMKT                                                  
         MVC   WORK+41(L'RSTAGRUP),RSTAGRUP                                     
         MVC   CSTAGRUP,RSTAGRUP                                                
         DROP  R4,R6                                                            
*                                                                               
VSEXT    B     XIT                                                              
*                                                                               
STATV    CLC   22(0,R4),=C'TV'                                                  
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACM    CLC   22(0,R4),=C'CM'                                                  
STALF    CLC   22(0,R4),=C'LF'                                                  
STA1     CLC   22(0,R4),=C'1'                                                   
STA2     CLC   22(0,R4),=C'2'                                                   
STANHT   CLC   22(0,R4),=C'H '                                                  
         EJECT                                                                  
* VALIDATE DAYPART - RECORD READ INTO AIO2                                      
* R2 = INPUT HEADER                                                             
VDPT     GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVDPT)                                              
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R4),X'24'                                                      
         MVC   24(2,R4),AGENCY     REP                                          
         MVC   26(1,R4),8(R2)      DAYPART                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    DP120                                                            
         CLI   5(R2),1                                                          
         BNE   MYERR                                                            
         B     DP200                                                            
DP100    BAS   RE,NEXTEL                                                        
         BNE   MYERR                                                            
DP120    CLC   9(1,R2),2(R6)                                                    
         BNE   DP100                                                            
*                                                                               
DP200    MVC   AIO,AIO1                                                         
DPEXT    B     XIT                                                              
         EJECT                                                                  
* VALIDATE MULTIPLE DAYPARTS                                                    
* PARAM 1 = INPUT FIELD IN SCREEN FIELD FORMAT                                  
*           P1+5 = NUMBER OF DAYPARTS                                           
*           P1+8 = DAYPARTS                                                     
VDPTM    L     R3,0(R1)                                                         
         ZIC   R4,5(R2)            NUMBER OF DAYPARTS                           
         LA    R3,8(R3)            POINT TO FIRST DAYPART                       
*                                                                               
VDPTM20  MVC   RERROR,=AL2(INVDPT)                                              
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(RE),X'3C'                                                      
         MVC   24(2,RE),AGENCY     REP                                          
         MVC   26(1,RE),0(R3)      DAYPART                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
* GET NEXT DAYPART                                                              
         LA    R3,1(R3)                                                         
         BCT   R4,VDPTM20                                                       
         B     XIT                                                              
         EJECT                                                                  
* GLOBAL INVENTORY ADD:                                                         
* ADD INVENTORY RECORD TO ALL STATIONS LISTED IN THE MENU RECORD                
* PARAM 1 = A(SET IDENTIFIER)                                                   
* PARAM 2 = A(TABLE TO STORE STATIONS)                                          
*                                                                               
INMENADD DS    0H                                                               
         MVC   RERROR,=AL2(INVIDENT)                                            
         L     R3,0(R1)            A(SET IDENTIFIER)                            
         L     R4,4(R1)            A(TABLE TO STORE STATIONS)                   
*                                                                               
         OC    0(4,R3),SPACES      MOVE IN SPACES FOR BLANKS                    
         XC    MENUCNT,MENUCNT     # OF STATION MEMBERS IN MENU REC             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              BUILD MENU RECORD                            
         USING REMENUD,R6                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   X'38' RECORD TYPE                            
         MVC   RSETKREP,AGENCY     REP CODE                                     
         MVC   RSETKSET,=C'MS'     MARKET STATIONS FOR REPORT ORDER             
         MVC   RSETKID,0(R3)       SET IDENTIFIER                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSETKEY),KEYSAVE    VALID IDENTIFIER?                      
         BNE   MYERR                     NO                                     
*                                                                               
         GOTO1 GETREC                    YES- GET RECORD                        
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVC   DATADISP,=H'34'                                                  
         MVI   ELCODE,X'20'              GET SET MEMBERS ELEMENT                
         BAS   RE,GETEL                                                         
*                                                                               
         USING RSETMEMD,R6         SET MEMBERS ELEMENT                          
         ZIC   R3,RSETMELN         ELEMENT LENGTH                               
         SH    R3,=H'3'            3 BYTES OF OVERHEAD LENGTH                   
         SR    R2,R2                                                            
         D     R2,=F'5'            # OF STATIONS                                
         STC   R3,MENUCNT          COUNTER FOR # OF STATIONS                    
         LA    R6,RSETMEMB         MEMBERS (LENGTH = 5 FOR MS)                  
*                                                                               
INMEN10  DS    0H                                                               
         MVC   0(5,R4),0(R6)       MOVE STA MEMBER INTO STATION TABLE           
         CLI   4(R4),C' '          NO STATELLITE - JUST TV?                     
         BNE   *+8                                                              
         MVI   4(R4),C'T'          TV                                           
*                                                                               
         LA    R6,5(R6)            NEXT MEMBER ENTRY IN ELEMENT                 
         LA    R4,5(R4)            NEXT TABLE ENTRY                             
         BCT   R3,INMEN10                                                       
*                                                                               
INMENUX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE AGENCY AND GET PARENT REP                                            
*                                                                               
VAGY     DS    0H                                                               
         XC    KEY,KEY             NOW LOOK UP REP RECORD                       
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   CPARREP,RREPPAR     SAVE PARENT REP                              
VAGYX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* CHECK BOOK DATE <= TODAY'S DATE                                               
* PARAM 1 = STORAGE FOR TODAY'S DATE                                            
* PARAM 2 = ADDRESS OF BOOK DATE IN INV RECORD                                  
* PARAM 3 = BOOK TYPE                                                           
*                                                                               
VBKDAT   DS    0H                                                               
         L     R3,0(R1)            A(TODAY'S DATE IN BINARY)                    
         L     R4,4(R1)            A(BOOK DATE IN INV RECORD)                   
         L     R6,8(R1)            BOOK TYPE                                    
*                                                                               
         CLI   0(R6),C'P'          PROJECTED BOOK?                              
         BE    VBK10                                                            
         CLI   0(R6),C'S'          SPECIAL BOOK?                                
         BE    VBKX                                                             
         CLI   0(R6),C'T'          TIME PERIOD?                                 
         BE    VBKX                                                             
         CLI   0(R6),C'E'          ESTIMATED BOOK?                              
         BNE   VBK20                                                            
*                                                                               
VBK10    DS    0H                  EST BOOK MUST BE > TODAY'S DATE              
         MVC   RERROR,=AL2(INVEBDAT)                                            
*                                                                               
         XC    DUB,DUB                                                          
*                                                                               
         CLI   1(R4),0             ESTYY BOOK?                                  
         BE    VBKX                                                             
*                                                                               
         MVC   BLOCK+10(2),0(R4)    MOVE IN BOOK YY/MM                          
         MVI   BLOCK+12,X'1E'       YY/MM/DD WHERE DD=30                        
*                                                                               
         XC    BLOCK(3),BLOCK                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,BLOCK),0,0                                  
         LHI   R4,-45              GIVE 45 DAY LEEWAY                           
         GOTO1 ADDAY,DMCB,BLOCK,DUB,(R4)                                        
         GOTO1 DATCON,DMCB,(0,DUB),(3,(R3)),0,0                                 
*                                                                               
         CLC   BLOCK+10(3),0(R3)   TODAY'S DATE <= BOOK DATE?                   
         BL    MYERR               NO                                           
         B     VBKX                                                             
*                                                                               
VBK20    DS    0H                                                               
         MVC   RERROR,=AL2(INVBKDAT)                                            
         GOTO1 DATCON,DMCB,(5,0),(3,(R3)),0,0     GET TODAY'S DATE              
         CLC   0(2,R3),0(R4)       TODAY'S DATE >= BOOK DATE?                   
         BL    MYERR               NO - ERROR                                   
         B     VBKX                                                             
*                                                                               
VBKX     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*  VALIDATE GROUP - R2 POINTS AT SCREEN FIELD ON ENTRY                          
*                 - WORK HAS GROUP CODE, NAME & SUB GROUP NAME ON EXIT          
VGRP     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RGRPKEY,R4                                                       
         MVI   RGRPKTYP,7                                                       
         MVC   RGRPKREP,AGENCY     REP                                          
         MVC   RGRPKGRP,8(R2)                                                   
         OC    RGRPKGRP,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         MVC   WORK(2),RGRPKGRP    GROUP CODE                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK+10(10),RGRPNAME   GROUP NAME                                
         MVC   WORK+20(10),RGRPSBNM   SUB GROUP NAME                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  VALIDATE OFFICE - R2 POINTS AT SCREEN FIELD ON ENTRY                         
*                 - WORK HAS OFFICE CODE ON EXIT                                
*                 - EOFF... HAS ADDRESS INFO ON EXIT                            
         SPACE 1                                                                
VOFF     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ROFFKEY,R4                                                       
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,AGENCY     REP                                          
         MVC   ROFFKOFF,8(R2)                                                   
         OC    ROFFKOFF,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         MVC   WORK(2),ROFFKOFF    OFFICE CODE                                  
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK+2(20),ROFFNAME                                              
         MVC   EOFFADD1,ROFFADD1                                                
         MVC   EOFFADD2,ROFFADD2                                                
         MVC   EOFFSTT,ROFFSTT                                                  
         MVC   EOFFZIP,ROFFZIP                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  VALIDATE ADVERTISER - R2 POINTS AT SCREEN FIELD ON ENTRY                     
*                      - WORK HAS ADVERTISER CODE ON EXIT                       
*                      - WORK+10 HAS ADVERTISER NAMEON EXIT                     
VADV     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RADVKEY,R4                                                       
         MVI   RADVKTYP,8                                                       
         MVC   RADVKREP,AGENCY     REP                                          
         MVC   RADVKADV,8(R2)                                                   
         OC    RADVKADV,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         MVC   WORK(4),RADVKADV    GROUP CODE                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK+10(20),RADVNAME   GROUP NAME                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  VALIDATE BOOKS WITH BOOK TYPE OPTION -  R2 POINTS TO BOOK FIELD              
*                                                                               
         SPACE 2                                                                
VBOOK    XC    CBKTYPE,CBKTYPE                                                  
         GOTO1 ANY                                                              
         GOTO1 BOOKVAL,DMCB,(CSOURCE,(R2)),(MAX,WORK),(C'B',SCANNER),  X        
               CBKTYPE,(C'C',ACOMFACS)                                          
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   RERROR,=AL2(INVBOK)                                              
         B     MYERR                                                            
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         SPACE 1                                                                
         ZIC   R1,ACTUAL                                                        
         LA    R3,CBOOKS                                                        
         LA    R4,CBKTYPE                                                       
         LA    RE,WORK                                                          
BOOK10   MVC   0(3,R3),0(RE)       MOVE IN BOOK                                 
         MVC   3(1,R3),0(R4)       SAVE CBKTYPE IN BOOK+3                       
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    RE,3(RE)                                                         
         BCT   R1,BOOK10                                                        
         MVI   0(R3),X'FF'         END OF LIST MARKER                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 2-CHARACTER BOOKTYPES DISPLAY SUPPORT                                         
* INPUT PARAM 1 = HIGH-ORD BYTE INTERNAL BOOKTYPE                               
* OUTPUT PARM 1 = HIGH-ORD LENGTH OF OUTPUT BOOK, BYTE3-4 BOOKTYPE              
         SPACE 2                                                                
GETBKTP  DS    0H                                                               
         MVC   BYTE,DMCB                                                        
         CLI   BYTE,0                                                           
         BNE   GTBK10                                                           
         XC    DMCB,DMCB                                                        
         B     XIT                                                              
*                                                                               
GTBK10   DS    0H                                                               
         L     R3,ACOMFACS                                                      
         SPACE                                                                  
         L     RF,CDEMTABS-COMFACSD(,R3)                                        
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
GTBK20   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,SPBKTYPN                                                    
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     GTBK20                                                           
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+2(2),SPBKTYPA    BOOK TYPE                                  
         MVI   DMCB,1                                                           
         CLI   SPBKTYPA+1,0                                                     
         BE    XIT                                                              
         CLI   SPBKTYPA+1,C' '                                                  
         BE    XIT                                                              
         MVI   DMCB,2                                                           
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*  VALIDATE BOOKS WITH LABEL OPTION -  R2 POINTS TO BOOK FIELD                  
*                                                                               
         SPACE 2                                                                
VBKL     MVC   CBKLABEL,SPACES                                                  
         GOTO1 ANY                                                              
         GOTO1 BOOKVAL,DMCB,(CSOURCE,(R2)),(MAX,CBOOKS),(C'L',SCANNER),X        
               CBKLABEL,(C'C',ACOMFACS)                                         
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   RERROR,=AL2(INVBOK)                                              
         B     MYERR                                                            
         SPACE 1                                                                
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY BOOKS WITH LABELS                                                     
* PARAMETER 1 = A(BOOK FIELD), '$' IN HIGH-ORD BYTE MEANS USE $, NOT -          
* PARAMETER 2 = A(LABEL ELEMENT (X'0B')), IF THERE IS ONE                       
* PARAMETER 3 = 0                                                               
* PARAMETER 4 = A(FILTERS), '+' IN HIGH-ORDER BYTE (OR ZERO)                    
         SPACE 2                                                                
DBKL     MVC   DMCB+12(4),12(R1)   FILTERS (IF ANY)                             
         L     R3,0(R1)            BOOK FIELD                                   
         MVC   DMCB+8(4),4(R1)     ADDRESS OF LABEL ELEMENT (IF ANY)            
         OC    DMCB+8(4),DMCB+8                                                 
         BZ    *+8                                                              
         MVI   DMCB+8,C'L'         THEN, INDICATE LABEL OPTION                  
         MVC   BYTE,DMCB                                                        
         GOTO1 VUNBOOK,DMCB,(MAX,0(R3)),(BYTE,0(R2))                            
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE DEMO(S)                                                              
         SPACE 1                                                                
VDEMO    GOTO1 ANY                                                              
         MVI   BYTE,0                                                           
         CLI   3(R1),1             1=VALIDATE CPM/CPP/PRIMARY DEMO              
         BNE   *+8                                                              
         MVI   BYTE,C'Y'                                                        
         LA    R3,BLOCK                                                         
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         GOTO1 DEMOVAL,PARAS,(CNDEMFLD,(R2)),(MAX,CDEMOS),(BYTE,(R3))           
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   RERROR,=AL2(INVDEM)                                              
         B     MYERR                                                            
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* DISPLAY DEMOS                                                                 
* PARAMETER 1 POINTS TO LIST OF DEMOS                                           
         SPACE 2                                                                
DDEMO    L     R3,0(R1)                                                         
         ZIC   R4,MAX                                                           
DD10     CLI   1(R3),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R3),C'I'                                                       
         LA    R3,3(R3)                                                         
         BCT   R4,DD10                                                          
         L     R3,0(R1)                                                         
         LA    R4,BLOCK                                                         
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOCON,DMCB,(MAX,0(R3)),(9,8(R2)),(0,DBLOCK)                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE CONTRACT - 1ST PARAMETER POINTS TO CONTRACT FIELD HEADER             
         SPACE 1                                                                
VCON     L     R2,0(R1)                                                         
         GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    MYERR                                                            
         SPACE 1                                                                
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)       CONTRACT NUM IN 9'S COMPLEMENT          
         SPACE 1                                                                
         MVC   CCONNUM,WORK                                                     
         PACK  CCONNUM(1),WORK+3(1)      REVERSE THE COMPLIMENT                 
         PACK  CCONNUM+1(1),WORK+2(1)    FOR AVL/PRO RECORDS LATER              
         PACK  CCONNUM+2(1),WORK+1(1)                                           
         PACK  CCONNUM+3(1),WORK(1)                                             
         SPACE 1                                                                
         MVC   RERROR,=AL2(NOTFOUND)                                            
         MVC   AIO,AIO2            PUT CONTRACT IN IO2                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONPTYP,R6                                                      
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,WORK       CONTRACT NUMBER                              
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCONKEY,R6                                                       
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    CON10                                                            
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    CON10                                                            
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   CON10                                                            
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    CON10               TO ALL OFFICES                               
         CLC   RCONKOFF,TWAACCS+2  ELSE,COMPARE OFFICES                         
         BE    CON10                                                            
         MVC   RERROR,=AL2(SECLOCK) SECURITY LOCKOUT                            
         B     MYERR                                                            
         SPACE 1                                                                
CON10    MVC   CCONKSTA,RCONKSTA   STATION CALL LETTERS                         
         MVC   ESTATION(4),RCONKSTA ALSO SAVE IN PRINTABLE FORMAT               
         CLI   RCONKSTA+4,C' '     TV                                           
         BE    CON15                                                            
         LA    RE,ESTATION+3       FOR RADIO, SHOW -A,-F,-C                     
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),RCONKSTA+4                                               
         SPACE 1                                                                
CON15    MVC   CCONKAGY(6),RCONKAGY  AGENCY CODE                                
         MVC   CCONKADV,RCONKADV   ADVERTISER CODE                              
         MVC   CCONKOFF,RCONKOFF                                                
         DROP  R6                                                               
         SPACE 1                                                                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RCONELEM,R6                                                      
         MVC   CCONSAL,RCONSAL     SALESPERSON CODE                             
         MVC   CCONDAT,RCONDATE    START/END DATES                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,ECONDATE)     CONTRACT               
         MVI   ECONDATE+8,C'-'                           DATES                  
         GOTO1 (RF),(R1),(3,RCONDATE+3),(5,ECONDATE+9)                          
         MVC   CSOURCE,RCONRTGS    RATING SERVICE                               
         MVC   ECONBUYR,RCONBUYR   BUYER NAME                                   
         MVC   CCONWKS,RCONWKS     NUMBER OF WEEKS IN CONTRACT                  
         MVC   HALF,RCONCTGY       CATEGORY                                     
         CLC   RCONPRD,SPACES                                                   
         BE    *+14                                                             
         MVC   CCONPRD,RCONPRD                                                  
         B     CON16                                                            
         DROP  R6                                                               
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        GET PRODUCT NAME                             
         BAS   RE,GETEL                                                         
         USING RCONEXEL,R6                                                      
         MVC   EPRDNAME,RCONEXPR                                                
         DROP  R6                                                               
         SPACE 1                                                                
CON16    L     R6,AIO                                                           
         MVI   ELCODE,X'12'        GET SAR ELEMENT (IF ANY)                     
         BAS   RE,GETEL                 -SAVE BOOKS, DEMOS, LENGTHS             
         BE    CON18                                                            
*********CLC   AGENCY,=C'BL'       BL,B1 AND NB MUST HAVE SAR                   
*********BE    CON17                                                            
*********CLC   AGENCY,=C'B1'                                                    
*********BE    CON17                                                            
*********CLC   AGENCY,=C'NB'                                                    
*********BNE   CON20                                                            
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'R0AV'                                              
         MVC   WORK+20(2),AGENCY                                                
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         CLI   WORK,C'Y'           IS SAR INFO REQUIRED?                        
         BNE   CON20               NO                                           
         SPACE 1                                                                
CON17    CLC   CCONKADV,=CL4'GEN'  IF ADVERTISER IS 'GEN'. . .                  
         BNE   *+14                                                             
         CLC   HALF,=C'ZZ'         . . . AND CATEGORY IS 'ZZ'. . .              
         BE    CON20               . . . THEN SAR IS NOT REQUIRED               
         MVC   RERROR,=AL2(MISSSAR)                                             
         B     MYERR               OTHERWISE, SAR IS REQUIRED                   
         SPACE 1                                                                
         USING RSAREL,R6                                                        
CON18    CLC   RSARBKS(2),=C'DR'   FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    *+16                AND DEMOS                                    
         MVC   CSARBKS,RSARBKS                                                  
         MVC   CSARDEM,RSARDEM                                                  
         MVC   CSARLEN,RSARRFRM                                                 
         DROP  R6                                                               
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'0B'        GET SAR BOOK LABEL ELEMENT (IF ANY)          
         BAS   RE,GETEL                                                         
         BNE   CON20                                                            
         ZIC   RE,1(R6)            LENGTH OF ELEMENT                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CSAR0B(0),0(R6)        SAVE ELEMENT                              
         SPACE 1                                                                
CON20    XC    KEY,KEY             GET MARKET NAME                              
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,CCONKSTA                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EMKTNAME,RSTAMKT    MARKET NAME                                  
         EDIT  (2,RSTACHAN),(4,ESTACHAN),ALIGN=LEFT                             
         MVC   ESTAAFFL,RSTAAFFL         AFFILIATE                              
         DROP  R6                                                               
         SPACE 1                                                                
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   STNX                                                             
*  IF STATION IS USER, CHECK THAT THE STATION IS AUTHORIZED                     
*   TO SEE THIS CONTRACT                                                        
         MVC   RERROR,=AL2(SECLOCK) SECURITY LOCKOUT                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
STN10    BAS   RE,NEXTEL                                                        
         BNE   MYERR                                                            
         USING RSTASOEL,R6         RSTASID IS VALID SIGN-ON FOR THIS            
         CLC   TWAORIG,RSTASID     STATION'S CONTRACTS                          
         BNE   STN10                                                            
         DROP  R6                                                               
STNX     DS    0H                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET SALESPERSON NAME                         
         LA    R6,KEY                                                           
         USING RSALREC,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,CCONSAL                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   ESALNAME,RSALNAME                                                
         MVC   ESALTEL,RSALTEL     TELEPHONE NUMBER                             
         DROP  R6                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET AGENCY NAME                              
         LA    R6,KEY                                                           
         USING RAGYREC,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),CCONKAGY                                             
         MVC   RAGYKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    AGY75                                                            
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    AGY75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
AGY75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EAGYNAM1,RAGYNAM1                                                
         MVC   EAGYNAM2,RAGYNAM2                                                
         DROP  R6                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET ADVERTISER NAME                          
         LA    R6,KEY                                                           
         USING RADVREC,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,CCONKADV                                                
         MVC   RADVKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    ADV75                                                            
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    ADV75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
ADV75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EADVNAME,RADVNAME                                                
         DROP  R6                                                               
         SPACE 1                                                                
         OC    EPRDNAME,EPRDNAME                                                
         BNE   PRDX                                                             
         XC    KEY,KEY             GET PRODUCT NAME                             
         LA    R6,KEY                                                           
         USING RPRDREC,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CCONKADV                                                
         MVC   RPRDKPRD,CCONPRD                                                 
         MVC   RPRDKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),AGENCY                                                 
         BE    PRD75                                                            
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    PRD75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
PRD75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EPRDNAME,RPRDNAME                                                
         DROP  R6                                                               
PRDX     DS    0H                                                               
         SPACE 1                                                                
         XC    ECONNUM,ECONNUM                                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ECONNUM(0),8(R2)    SAVE EBCDIC CONTRACT NUMBER                  
         SPACE 1                                                                
         XC    KEY,KEY             NOW LOOK UP REP RECORD                       
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   CPARREP,RREPPAR     SAVE PARENT REP                              
         DROP  R6                                                               
         SPACE 1                                                                
         MVC   AIO,AIO1            RESET IO AREA                                
         B     XIT                                                              
         EJECT                                                                  
* BUILDS DEFAULT UPGRADE EXPRESSION GIVEN AN INVENTORY KEY                      
* AT ENTRY:                                                                     
*   R6-->INVENTORY KEY                                                          
* OUTPUT:                                                                       
*   CC SET TO HIGH ==> INPUT ERROR                                              
*   CC  "  "  EQAL ==> ELEM   CONTAINS UPGRADE ELEMENT                          
*   CC  "  "  LOW  ==> ERROR INTERNAL TO ROUTINE                                
                                                                                
BDUP     DS    0H                                                               
         USING RINVD,R6                                                         
         LHI   R0,2                ASSUME INPUT ERROR                           
         CLI   RINVKTYP,RINVKTYQ   MAKE SURE IT'S AN INVENTORY KEY              
         BNE   BDUPX                                                            
         CLI   RINVKBK+1,1         MAKE SURE MONTH IS VALID                     
         BL    BDUPX                                                            
         CLI   RINVKBK+1,12        MAKE SURE MONTH IS VALID                     
         BH    BDUPX                                                            
                                                                                
*                                                                               
         DS    0H                  BUILD DUMMY TWA FIELD W/ UPGRADE             
         XC    WORK,WORK                                                        
         LA    R2,WORK+8            R2 = POINTER TO DUMMY TWA FIELD             
*                                                                               
         DS    0H                                                               
         MVC   0(4,R2),=C'PUT/'                                                 
         AHI   R2,4                                                             
*                                                                               
         DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+0(2),RINVKBK    GET (BINARY) YEAR/MONTH FROM KEY            
         MVI   FULL+2,1                                                         
         LHI   R0,2                 ASSUME INPUT ERROR                          
         ZIC   R1,FULL+0            USE BOOK                                    
         BCTR  R1,0                  FROM A YEAR AGO                            
         CHI   R1,79                SHOULDN'T BE BEFORE 1979                    
         BL    BDUPX                                                            
         STC   R1,FULL+0                                                        
                                                                                
         DS    0H                                                               
         GOTO1 DATCON,DMCB,(X'83',FULL),(6,DUB),0                               
         ZIC   R1,DMCB+4                                                        
         MVC   DUB+3(2),DUB+4       REMOVE THE SLASH (/)                        
         SHI   R1,2                 NO MORE SLASH & EX INSTRUCTION              
                                                                                
         EXMVC R1,0(R2),DUB         MOVE IN "MMMYY"                             
         LA    R2,1(R1,R2)                                                      
*                                                                               
         DS    0H                                                               
         LA    R0,WORK+8                                                        
         SR    R2,R0                R2 = L(FORMATTED UPG EXPRESSION)            
         STC   R2,WORK+5                                                        
         AHI   R2,8                                                             
         STC   R2,WORK+0                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         SR    R0,R0                ASSUME INVALID UPGRADE EXPRESSION           
         GOTO1 UPVAL,DMCB,(1,WORK),(C'Y',ELEM),(C'/',ACOMFACS)                  
         CLI   DMCB+0,1             DMCB+0(1) = # OF UPG EXPRESSIONS            
         BNE   BDUPX                                                            
                                                                                
         LHI   R0,1                 UPGRADE EXPRESSION IS VALID                 
         DROP  R6                                                               
                                                                                
*                                                                               
BDUPX    DS    0H                  EXIT WITH CONDITION CODE SET                 
         CHI   R0,1                 MAKE SURE  R0  IS SET                       
         B     XIT                                                              
         EJECT                                                                  
*  SWITCH TO SPOT SYSTEM TO GET AGENCY/MEDIA CODE AND FOR RANSID                
         SPACE 1                                                                
SWISPT   DS    0H                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   SS10                                                             
         L     R3,TWAMASTC         GET A(MASTC)                                 
         L     R3,MCUTL-MASTD(R3)  GET A(UTL)                                   
         MVI   4(R3),X'32'         PUT SPOT SENUM IN UTL                        
         LHI   R0,(SPTFLIST-T81000)                                             
         A     R0,SYSRB                                                         
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',(R0),AIO2                       
         B     SS40                                                             
         SPACE                                                                  
SS10     L     RF,SWITCH                                                        
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'32'          SPOT Q SYSTEM NUMBER                         
         CLC   AGENCY,=C'SJ'       FOR TESTING                                  
         BNE   *+8                                                              
         MVI   DMCB,X'02'          USE SPOT 1 (SYSTEM NUMBER IS 2)              
         GOTO1 (RF),DMCB                                                        
         SPACE 1                                                                
         CLI   4(R1),2             TEST SYSTEM NOT OPERATIONAL                  
         BNE   *+14                                                             
         MVC   RERROR,=AL2(SPOTSTOP)                                            
         B     MYERR                                                            
         SPACE 1                                                                
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*  SET UP INTERNAL VALUES FOR SPOT                                              
SS40     MVI   SYSTEM,C'S'                                                      
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   REQFILE,=C'SPTREQ '                                              
         B     XIT                                                              
         EJECT                                                                  
* SWITCH BACK TO REP                                                            
SWIREP   DS    0H                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   SR20                                                             
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
         L     R3,TWAMASTC         GET A(MASTC)                                 
         L     R3,MCUTL-MASTD(R3)  GET A(UTL)                                   
         CLI   CTRLMAIN,C'Y'       HAS SE# BEEN FOUND ALREADY?                  
         BE    SR10                YES                                          
         MVI   CTRLMAIN,C'Y'       SET SE# FOUND                                
         BAS   RE,CTRLSET          FIND SE#                                     
SR10     EQU   *                                                                
         LHI   R0,(REPFLIST-T81000)                                             
         A     R0,SYSRB                                                         
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',(R0),AIO2                        
*                                                                               
*   OPEN REP FILES *AND* CONTROL FILE UNDER REP SE #                            
*                                                                               
         B     SR40                                                             
*                                                                               
SR20     L     RF,SWITCH                                                        
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*  RESET INTERNAL VALUES FOR REP                                                
SR40     MVI   SYSTEM,C'R'                                                      
         MVC   LKEY,=H'27'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  CTRLSET:  SETS REP SE # BY LOOKING IN CONTROL FILE.  DOES IT                 
*     ONE TIME BASED ON SWITCH CTRLMAIN.                                        
*         R3  =  A(UTL)                                                         
*                                                                               
CTRLSET  NTR1                                                                   
         MVI   4(R3),X'0A'         SET UTL SE TO CTFILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AIO2,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'5'           FIND CONTROL FILE ACCESS RECORD              
         MVC   WORK+23,AGENCY      INSERT POWER CODE                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',WORK,AIO2                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIO2                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         ST    R1,FULL             SAVE A(X'21' ELEMENT)                        
*****>   GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'CONTROL'                              
*                                  CLOSE CONTROL FILE AS X'0A'                  
         L     R1,FULL             RESET A(X'21' ELEMENT)                       
         MVC   4(1,R3),3(R1)       OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
         B     XIT                                                              
         EJECT                                                                  
*  ROUTINE VALIDATES SCHEME/PERIOD FIELD                                        
*  ON ENTRY, R2 POINTS TO PJ OR SCHEME/PERIOD FIELD ON SCREEN                   
*     ENTRY CAN BE SCHEME/PERIOD                                                
*     OR SCHEME/PERIOD-YEAR (VALIDATE THROUGH RANSID IN SPOT)                   
         SPACE 1                                                                
VSID     DS    0H                                                               
         SPACE 1                                                                
         CLI   CAGYMED,0           DO WE ALREADY HAVE AGENCY/MEDIA              
         BNE   *+8                 YES                                          
         BAS   RE,GETAGY           NO, SO GO GET IT                             
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),C',=/='  CHANGE DELIMETER            
         LA    R4,BLOCK                                                         
         CLI   DMCB+4,2                                                         
         BL    SID60               MUST HAVE SCHEME AND PERIOD                  
         CLI   DMCB+4,3            ALSO CAN HAVE YEAR                           
         BH    SID60                                                            
         SPACE 1                                                                
         CLI   0(R4),2             SCHEME IS 2                                  
         BL    SID60                                                            
         CLI   0(R4),3             OR 3 CHARACTERS                              
         BH    SID60                                                            
         OC    CSCHEME,CSCHEME                                                  
         BZ    SID30                                                            
         MVC   CSCHEME2,12(R4)                                                  
         B     *+10                                                             
SID30    MVC   CSCHEME,12(R4)                                                   
         SPACE 1                                                                
         LA    R4,32(R4)                                                        
         CLI   0(R4),4             PERIOD IS UP TO 4 CHARACTERS                 
         BH    SID60                                                            
         OC    CPERIOD,CPERIOD                                                  
         BZ    SID40                                                            
         MVC   CPERIOD2,12(R4)                                                  
         B     *+10                                                             
SID40    MVC   CPERIOD,12(R4)                                                   
         SPACE 1                                                                
         CLI   DMCB+4,2                                                         
         BE    SID70                                                            
         LA    R4,32(R4)                                                        
         CLI   0(R4),2             YEAR IS 2 CHARACTERS                         
         BNE   SID60                                                            
         TM    2(R4),X'80'         MUST BE NUMERIC                              
         BZ    SID60                                                            
         OC    CYEAR,CYEAR                                                      
         BZ    SID50                                                            
         MVC   CYEAR2,7(R4)                                                     
         B     SID70                                                            
SID50    MVC   CYEAR,7(R4)         BINARY VALUE OF YEAR                         
         B     SID70                                                            
         SPACE 1                                                                
SID60    MVC   RERROR,=AL2(BADFMT)                                              
         B     MYERR                                                            
         SPACE 1                                                                
SID70    XC    WORK,WORK           READ SID PROFILE-MUST BE REP SCHEME          
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVI   WORK+22,C'T'                                                     
         OC    CSCHEME2,CSCHEME2                                                
         BZ    SID73                                                            
         CLC   CSCHEME2,=C'ALL'                                                 
         BE    SID75                                                            
         MVC   WORK+23(3),CSCHEME2                                              
         B     SID75                                                            
SID73    CLC   CSCHEME,=C'ALL'                                                  
         BE    *+10                                                             
         MVC   WORK+23(3),CSCHEME                                               
SID75    GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    SID77                                                            
         CLI   WORK+1,C'Y'         ERROR IF NOT A Y                             
         BE    SID79                                                            
SID77    MVC   RERROR,=AL2(REPSCM)                                              
         B     MYERR                                                            
         SPACE 1                                                                
SID79    LA    R4,BUFF             USE BUFF+2000 FOR RANSID BLOCK               
         LA    R4,1000(R4)                                                      
         LA    R4,1000(R4)         DON'T CREAM ITEMTAB                          
         USING SRBLKD,R4                                                        
         LA    RE,SRBLK            CLEAR BLOCK                                  
         LA    RF,SRBLKLN                                                       
         XCEF                                                                   
         SPACE 1                                                                
         MVC   SRASIR,AIO2         USI IO2 FOR NSID RECORDS                     
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,VCLPACK                                                 
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SRAMASTC,TWAMASTC                                                
         SPACE 1                                                                
         MVC   SRSELSCH,CSCHEME                                                 
         MVC   SRSELPER,CPERIOD                                                 
         MVC   SRSELYR,CYEAR                                                    
         OC    CSCHEME2,CSCHEME2                                                
         BZ    SID80                                                            
         MVC   SRSELSCH,CSCHEME2                                                
         MVC   SRSELPER,CPERIOD2                                                
         MVC   SRSELYR,CYEAR2                                                   
SID80    MVC   SRSELAM,CAGYMED                                                  
         MVC   SRSELAGY,AGENCY                                                  
         MVI   SRSELMED,C'T'                                                    
         SPACE 1                                                                
         GOTO1 RANSID,DMCB,(R4)                                                 
         SPACE 1                                                                
         CLI   SRERROR,SRNOERR     TEST FOR ERROR                               
         BE    SID110                                                           
         CLI   SRERROR,SRNOSCH                                                  
         BNE   SID90                                                            
         MVC   RERROR,=AL2(NOSCM)                                               
         B     MYERR                                                            
SID90    CLI   SRERROR,SRNOPER                                                  
         BNE   SID100                                                           
         MVC   RERROR,=AL2(NOPER)                                               
         B     MYERR                                                            
SID100   MVC   RERROR,=AL2(INVALID) SOMETHING ELSE IS WRONG                     
         B     MYERR                                                            
SID110   CLI   SRMODE,SRONEREC                                                  
         BE    XIT                                                              
         MVC   RERROR,=AL2(NOREC)                                               
         B     MYERR                                                            
         DROP  R4                                                               
         EJECT                                                                  
* PACK - R2 HAS ADDRESS OF HEADER                                               
         SPACE 1                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    PACKX               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE LENGTH FIELD                                        
*  PARAMETER 1 HAS AREA WITH 2-BYTE FIELDS,                                     
*                  WHERE BYTE 1 IS CLASS NUMBER IN BINARY                       
*                    AND BYTE 2 IS LENGTH IN SECONDS                            
*       (CLASS NUMBER IS OPTIONAL FOR AVAILS,                                   
*         AND IS ALWAYS ZERO FOR PROPOSALS)                                     
         SPACE 2                                                                
DLEN     L     R6,0(R1)            LENGTH FIELD                                 
         ZIC   R3,MAX              MAXIMUM NUMBER OF LENGTHS                    
         LA    R4,8(R2)                                                         
         OC    0(2,R6),0(R6)                                                    
         BZ    XIT                                                              
         B     DL6                 DON'T NEED COMMA FOR FIRST TIME              
DL2      OC    0(2,R6),0(R6)                                                    
         BZ    XIT                                                              
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
DL6      SR    RE,RE                                                            
         IC    RE,1(R6)            LENGTH                                       
         MH    RE,=H'10'           LEFT ONE POSITION                            
         XR    RF,RF                                                            
         IC    RF,0(R6)            CLASS                                        
         AR    RE,RF                                                            
         EDIT  (RE),(5,0(R4)),1,ALIGN=LEFT                                      
         SH    R0,=H'2'            R0 HAS NUM OF SIGNIF. CHARS                  
         AR    R4,R0               LENGTH OF OUTPUT                             
         CLI   ETYPE,C'P'                                                       
         BNE   DL10                                                             
         CLI   0(R4),C'.'          WIPE OUT CLASS FOR PROPOSALS                 
         BE    DL20                (WHEN COMING FROM SAR)                       
DL10     CLC   0(2,R4),=C'.0'                                                   
         BNE   *+14                                                             
DL20     MVC   0(2,R4),SPACES                                                   
         B     *+8                                                              
         LA    R4,2(R4)                                                         
         LA    R6,2(R6)                                                         
         BCT   R3,DL2                                                           
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE SOURCE FIELD (INVENTORY OR SID)                    
* R2 POINTS TO SOURCE FIELD, AND FIELD IS REQUIRED                              
         SPACE 1                                                                
VSOURCE  GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         MVI   ESOURCE,C'I'                                                     
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'INV'                                                  
         BE    XIT                                                              
         B     MYERR          ***  REMOVE THIS LINE WHEN SID IS VALID           
         SPACE 3                                                                
         MVI   ESOURCE,C'S'                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SID'                                                  
         BNE   MYERR                                                            
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES AN INVENTORY NUMBER AS FAR AS FORMAT, AND ALSO         
* MAKES SURE THAT THE INVENTORY RECORD EXISTS, AND THE INVENTORY                
* DATES FALL WITHIN THE CONTRACT DATE BOUNDARIES                                
*                                                                               
* ON EXIT, CBLOCK(3) HAS INVENTORY NUMBER IN HEX                                
*          CBLOCK+3(3) HAS INVENTORY DATE                                       
*          CBLOCK+6(1) HAS SATELLITE CODE                                       
         SPACE 1                                                                
VINV     DS    0H                                                               
         XC    CBLOCK(40),CBLOCK                                                
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),C',=,-'                              
         SR    R4,R4                                                            
         IC    R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    MYERR                                                            
         SPACE 1                                                                
         LA    R3,BLOCK                                                         
         CLC   12(3,R3),=C'MAN'    IF INVENTORY IS MANUAL                       
         BNE   *+14                                                             
         MVC   CBLOCK(3),=C'MAN'                                                
         B     VINVX                                                            
         CLC   0(2,R3),=X'0100'    1ST HALF OF FIELD LEN=1, NO 2ND HALF         
         BNE   VINV10                                                           
         CLI   12(R3),C'P'              PURE                                    
         BNE   MYERR                                                            
         MVI   BYTE,C'P'                                                        
         LA    R3,32(R3)                                                        
         BCT   R4,VINV10                                                        
         B     MYERR                                                            
         SPACE 1                                                                
VINV10   CLI   0(R3),3                  INVENTORY NUMBER                        
         BL    MYERR                                                            
         CLI   0(R3),4                                                          
         BH    MYERR                                                            
         SPACE 1                                                                
         CLI   12(R3),C'0'                                                      
         BL    MYERR                                                            
         CLI   12(R3),C'9'                                                      
         BH    MYERR                                                            
         CLI   13(R3),C'0'                                                      
         BL    MYERR                                                            
         CLI   13(R3),C'9'                                                      
         BH    MYERR                                                            
         SPACE 1                                                                
         PACK  DUB(8),12(2,R3)     QTR HOUR NUMBER                              
         CVB   R0,DUB                                                           
         STC   R0,CBLOCK                                                        
         SPACE 1                                                                
         CLI   14(R3),C'D'         TYPICAL                                      
         BE    VINV20                                                           
         CLI   14(R3),C'E'         WEEKEND                                      
         BE    VINV20                                                           
         CLI   14(R3),C'0'                                                      
         BL    MYERR                                                            
         CLI   14(R3),C'9'                                                      
         BH    MYERR                                                            
VINV20   SR    R1,R1                                                            
         IC    R1,14(R3)                                                        
         CLI   BYTE,C'P'          PURE                                          
         BNE   VINV50                                                           
         CLI   14(R3),C'D'         FOR TYPICAL                                  
         BE    VINV30                                                           
         CLI   14(R3),C'E'         AND WEEKEND                                  
         BNE   VINV40                                                           
VINV30   AH    R1,=H'9'            CONVERT CHARACTER TO NUMBER                  
VINV40   SLL   R1,28                                                            
         SRL   R1,24                                                            
VINV50   STC   R1,CBLOCK+1         DAY                                          
         SPACE 1                                                                
         CLI   BYTE,C'P'                                                        
         BE    VINV60                                                           
         MVI   CBLOCK+2,C'0'                                                    
         CLI   0(R3),3                                                          
         BE    VINV70                                                           
         MVC   CBLOCK+2(1),15(R3)                                               
         B     VINV70                                                           
         SPACE 1                                                                
VINV60   CLI   0(R3),3                                                          
         BE    VINV70                                                           
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,15(R3)           START WEEK                                   
         SLDL  RE,28               RE 4 HIGH ORDER                              
         SRL   RF,28               RF 4 LOW ORDER                               
         CH    RF,=H'7'                                                         
         BH    MYERR               0-7, A-G                                     
         SLL   RF,1                                                             
         EX    RF,*+8              BITES 4-6                                    
         B     *+8                                                              
         OI    CBLOCK+1,0                                                       
         SPACE 1                                                                
         CH    RE,=H'15'                                                        
         BNE   *+14                NOT NUMERIC                                  
         LTR   RF,RF                                                            
         BZ    *+16                INPUT ZERO                                   
         B     VINV70                                                           
         SPACE 1                                                                
         CH    RE,=H'12'                                                        
         BNE   MYERR               NOT A-G                                      
         OI    CBLOCK+1,1          LOW ORDER ON FOR ZERO AND A-G                
         SPACE 1                                                                
VINV70   MVC   CBLOCK+6(1),22(R3)  SATELLITE                                    
         CLI   CBLOCK+6,X'40'                                                   
         BNE   *+8                                                              
         MVI   CBLOCK+6,0                                                       
         SPACE 1                                                                
         LA    R3,32(R3)           OPTIONAL DATE                                
         CH    R4,=H'1'                                                         
         BE    VINV80                                                           
         GOTO1 DATVAL,DMCB,(0,12(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    MYERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,CBLOCK+3)                                
         SPACE 1                                                                
*  NOW LOOK UP AN INVENTORY OR PURE NUMBER TO VERIFY THAT                       
*   IT EXISTS, AND THAT IT FALLS WITH IN THE DATE PARAMETERS                    
         SPACE 1                                                                
VINV80   GOTO1 DATCON,DMCB,(3,CCONDAT),(2,CBLOCK+20)                            
         GOTO1 DATCON,DMCB,(3,CCONDAT+3),(2,CBLOCK+22)                          
         SPACE 1                                                                
         MVC   AIO,AIO2            USE IO2 FOR PAV OR INV RECORD                
         LA    R3,ELEM                                                          
         SPACE 1                                                                
         CLI   8(R2),C'P'                                                       
         BE    VINV150                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY              SET KEY FOR INV                              
         USING RINVD,R6                                                         
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RINVKSTA,CCONKSTA                                                
         MVC   RINVKSTA+4(1),CBLOCK+6                                           
         OI    RINVKSTA+4,X'40'                                                 
         CLI   RINVKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RINVKSTA+4,C'T'                                                  
         SPACE 1                                                                
         MVC   RINVKINV,CBLOCK                                                  
         MVC   RINVKSTD,CBLOCK+3                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE 1                                                                
VINV90   MVC   RERROR,=AL2(NOTFOUND)                                            
*                                  SAME INVENTORY ITEM                          
         CLC   KEYSAVE(RINVKSTD-RINVKEY),KEY                                    
         BNE   MYERR                                                            
         OC    CBLOCK+3(3),CBLOCK+3 ANY DATE GIVEN                              
         BZ    *+14                                                             
         CLC   KEYSAVE(RINVKSPR-RINVKEY),KEY                                    
         BNE   MYERR                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    *+14                                                             
         CLC   RINVPEFF+2(2),CBLOCK+20  INVENTORY ENDS                          
         BL    VINV100             BEFORE CONTRACT START                        
         SPACE 1                                                                
         CLC   RINVPEFF(2),CBLOCK+22    INVENTORY STARTS                        
         BH    VINV100             AFTER CONTRACT END                           
         MVC   CBLOCK+3(3),RINVKSTD      SAVE DATE, EVEN IF THEY DIDN'T         
         B     VINVX                     SUPPLY IT                              
         SPACE 1                                                                
VINV100  OC    CBLOCK+3(3),CBLOCK+3 ANY DATE GIVEN                              
         BNZ   MYERR                                                            
         LA    R6,KEY                                                           
*                                         FORCE NEXT INV NUMBER                 
         MVI   RINVKRSR,X'FF'                                                   
         MVC   RINVKRSR+1(RINVLEN-RINVKRSR-1),RINVKRSR                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     VINV90                                                           
         DROP  R6                                                               
         SPACE 3                                                                
VINV150  XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU                                                  
         MVC   PRMEDIA,CCONKSTA+4                                               
         CLI   PRMEDIA,C' '                                                     
         BNE   *+8                                                              
         MVI   PRMEDIA,C'T'                                                     
         SPACE 1                                                                
         MVC   PRSRC,CSOURCE                                                    
         MVC   PRSTAT(4),CCONKSTA                                               
         MVC   PRSTAT+4(1),PRMEDIA                                              
         CLI   CBLOCK+6,0                                                       
         BE    *+10                                                             
         MVC   PRSTAT+4(1),CBLOCK+6                                             
         SPACE 1                                                                
         LA    R3,CBOOKS                                                        
         OC    0(3,R3),0(R3)                                                    
         BNZ   INVALP0                                                          
         MVC   RERROR,=AL2(MISSBKS)                                             
         B     MYERR                                                            
INVALP0  LA    R1,6                AVAILS HAVE UP TO 6 BOOKS                    
         CLI   ETYPE,C'P'                                                       
         BNE   *+8                                                              
         LA    R1,1                PROPOSALS HAVE 1 BOOK                        
         SPACE 1                                                                
INVALP1  MVC   PRBOOK,1(R3)                                                     
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         BAS   RE,PAVHIGH                                                       
         L     R4,AIO                                                           
         CLC   PRKEY(PRSTYP-PRKEY),0(R4)                                        
         BNE   INVALP2                                                          
         SPACE 1                                                                
         MVC   PRSTIM(2),CBLOCK   (INVENTORY NUMBER)                            
         BAS   RE,DMFLHI                                                        
         CLC   PRKEY(PRRLEN-PRKEY),0(R4)                                        
         BE    VINVX                                                            
         SPACE 1                                                                
INVALP2  LA    R3,3(R3)                                                         
         OC    0(3,R3),0(R3)                                                    
         BZ    INVALP3                                                          
         XC    PRBOOK(PRFRSTEL-PRBOOK),PRBOOK                                   
         BCT   R1,INVALP1                                                       
         SPACE 1                                                                
INVALP3  MVC   RERROR,=AL2(NOTFOUND)                                            
         B     MYERR                                                            
         DROP  R6                                                               
         SPACE 2                                                                
VINVX    MVC   AIO,AIO1            RESET IO AREA                                
         B     XIT                                                              
         EJECT                                                                  
* THIS CODE (DOWN TO DMCHECK LABEL) WAS COPIED FROM DATA SET REPAVREAD          
* AT LEVEL 010 AS OF 11/23/81 AND AMENDED TO FIT HERE                           
*              COMMUNICATION WITH DATA MANAGER (PAVFL)                          
         SPACE 2                                                                
PAVREAD  MVC   COMMAND,=C'DMREAD'                                               
         B     PAVFILE                                                          
         SPACE 1                                                                
PAVSEQ   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PAVFILE                                                          
         SPACE 1                                                                
PAVHIGH  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
PAVFILE  NTR1                                                                   
         LA    R6,=C'PAVDIR'                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),(R6),KEY,AIO                      
         L     R2,AIO                                                           
         USING DMKEY,R2                                                         
         MVC   SVDXDA,DMNDXDA                                                   
         DROP  R2                                                               
         B     DMCHECK                                                          
         SPACE 1                                                                
DMFLHI   NTR1                                                                   
         LA    R6,=C'PAVFIL'                                                    
         MVC   COMMAND,=C'DMRDHI'                                               
         L     R2,AIO                                                           
FILMINOR USING PRKEY,R2                                                         
         LA    RE,KEY                                                           
DIRMAJOR USING PRKEY,RE                                                         
         MVC   0(PRFRSTEL-PRKEY,R2),KEY                                         
*                                                                               
* MOVE IN STATUS BYTE - SUPPORT PAV SPLIT FILES                                 
         MVC   FILMINOR.PRRSTAT,DIRMAJOR.PRKSTAT                                
         DROP  FILMINOR,DIRMAJOR                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),(R6),SVDXDA,AIO                   
         B     XIT                                                              
         SPACE 2                                                                
DMCHECK  MVI   DMINBTS,X'00'                                                    
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    XIT                                                              
         SPACE                                                                  
         MVC   RERROR,=AL2(NOTFOUND)                                            
         B     MYERR                                                            
         EJECT                                                                  
* THIS ROUTINE WILL DISPLAY AN INVENTORY NUMBER                                 
* R2 POINTS TO INVENTORY NUMBER FIELD                                           
* INVENTORY NUMBER (RAVLDINV OR RPRPDINV) IS IN CBLOCK                          
* INVENTORY DATE (RAVLDATE OR RPRPDATE) IS IN CBLOCK+3                          
* SATELLITE (RAVLSAT OR RPRPDSAT) IS IN CBLOCK+6                                
         SPACE 1                                                                
DINV     DS    0H                                                               
         LA    R3,8(R2)                                                         
         MVC   0(6,R3),=C'MANUAL'                                               
         CLC   CBLOCK(3),=C'MAN'                                                
         BE    DINV50                                                           
         SPACE 1                                                                
         MVC   0(6,R3),SPACES                                                   
         CLI   CBLOCK+2,0                                                       
         BNE   *+14                NOT PURE                                     
         MVC   0(2,R3),=C'P,'      PURE                                         
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         XR    R1,R1                                                            
         IC    R1,CBLOCK                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R3),DUB+1       QTR HOUR                                     
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         IC    R1,CBLOCK+1             DAY CODE                                 
         CLI   8(R2),C'P'          IS IT PURE                                   
         BNE   DINV30                                                           
         SRL   R1,4                                                             
         CLM   R1,1,=X'0D'         FOR TYPICAL OR                               
         BE    DINV10                                                           
         CLM   R1,1,=X'0E'         WEEKEND,                                     
         BNE   DINV20                                                           
DINV10   O     R1,=X'000000C0'     CONVERT NUMBER TO CHARACTER                  
         SH    R1,=H'9'                                                         
         B     DINV30                                                           
         SPACE 1                                                                
DINV20   O     R1,=X'000000F0'                                                  
         SPACE 1                                                                
DINV30   STC   R1,0(R3)                                                         
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
         CLI   8(R2),C'P'                                                       
         BE    DINV40                                                           
         CLI   CBLOCK+2,C'0'                                                    
         BE    DINV50                                                           
         MVC   0(1,R3),CBLOCK+2                                                 
         LA    R3,1(R3)                                                         
         B     DINV50                                                           
         SPACE 1                                                                
DINV40   TM    CBLOCK+1,X'0F'                                                   
         BZ    DINV50                                                           
         IC    R1,CBLOCK+1                                                      
         SLL   R1,28                                                            
         SRL   R1,29                                                            
         STC   R1,0(R3)                                                         
         OI    0(R3),X'C0'                                                      
         CLI   0(R3),X'C0'                                                      
         BE    *+12                                                             
         TM    CBLOCK+1,X'01'                                                   
         BO    *+8                                                              
         OI    0(R3),X'F0'                                                      
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DINV50   CLI   CBLOCK+6,0                                                       
         BE    *+18                                                             
         MVI   0(R3),C'-'                                                       
         MVC   1(1,R3),CBLOCK+6                                                 
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         OC    CBLOCK+3(3),CBLOCK+3 DATE                                        
         BZ    DINVX                                                            
         MVI   0(R3),C','                                                       
         GOTO1 DATCON,DMCB,(3,CBLOCK+3),(5,1(R3))                               
         SPACE 1                                                                
DINVX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
* THIS ROUTINE WILL DISPLAY THE DAY, TIME, AND TITLE FROM THE                   
* INVENTORY RECORD                                                              
*                                                                               
* ON ENTRY, CBLOCK WILL HAVE INVENTORY NUMBER                                   
*           CBLOCK+3 WILL HAVE INVENTORY DATE                                   
*                                                                               
* ON EXIT, CBLOCK+10 WILL HAVE DAY                                              
*          CBLOCK+20 WILL HAVE TIME                                             
*          CBLOCK+40 WILL HAVE 1ST 27 CHARACTERS OF PROGRAM NAME                
         SPACE 2                                                                
DINVDTT  DS    0H                                                               
         XC    CBLOCK+10(57),CBLOCK+10                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RINVD,R6                                                         
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RINVKSTA,CCONKSTA                                                
         MVC   RINVKSTA+4(1),CBLOCK+6                                           
         OI    RINVKSTA+4,X'40'                                                 
         CLI   RINVKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RINVKSTA+4,C'T'                                                  
         SPACE 1                                                                
         MVC   RINVKINV,CBLOCK                                                  
         MVC   RINVKSTD,CBLOCK+3                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   XIT                 INVENTORY WAS DELETED                        
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RINVPEL,R6                                                       
         MVC   CBLOCK+10(30),SPACES          DAY/TIME                           
         OC    RINVPADY,RINVPADY   IF AVAIL DAY OVERRIDE,                       
         BZ    DDTT10                                                           
         GOTO1 UNDAY,DMCB,RINVPADY,CBLOCK+10     USE IT                         
         B     DDTT20                                                           
DDTT10   GOTO1 UNDAY,DMCB,RINVPDAY,CBLOCK+10                                    
         SPACE 1                                                                
DDTT20   OC    RINVPATM,RINVPATM   IF AVAIL TIME OVERRIDE,                      
         BZ    DDTT30                                                           
         GOTO1 UNTIME,DMCB,RINVPATM,CBLOCK+20    USE IT                         
         B     DDTT40                                                           
         SPACE 1                                                                
DDTT30   GOTO1 UNTIME,DMCB,RINVPTIM,CBLOCK+20                                   
         SPACE 1                                                                
DDTT40   ZIC   RE,RINVPLEN                   PROGRAM                            
         SH    RE,=H'40'                                                        
         CH    RE,=H'27'                                                        
         BNH   *+8                                                              
         LA    RE,27                                                            
         SPACE 1                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CBLOCK+40(0),RINVPROG                                            
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*  THIS ROUTINE VALIDATES AN UPGRADE EXPRESSION                     *           
*  VALID UPGRADE EXPRESSIONS ARE UPT=PUT/MMMYY                      *           
*                                UPT=HUT/MMMYY                      *           
*                                UPT=HPT/MMMYY/INDEX                *           
*  ON ENTRY, R3 POINTS TO EITHER UPGRADE AREA 1 OR 2                *           
*-------------------------------------------------------------------*           
         SPACE 1                                                                
VUPT     DS    0H                                                               
         USING UPGD,R3                                                          
*         CLI   ESOURCE,C'S'                                                    
*         BE    VUPT10                                                          
         GOTO1 UPVAL,DMCB,(10,(R2)),WORK,ACOMFACS                               
         CLI   0(R1),0             TEST VALID EXPRESSION                        
         BNE   *+14                ERROR                                        
         MVC   RERROR,=AL2(BADUPGR)                                             
         B     MYERR                                                            
         CLI   0(R1),1             TEST ONLY ONE EXPRESSION                     
         BE    *+14                ERROR                                        
         MVC   RERROR,=AL2(ONEUPGR)                                             
         B     MYERR                                                            
         SPACE 1                                                                
         MVC   ELEM(14),WORK                                                    
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,=C'ADD=CODE'                   
         CLI   DMCB+12,0                                                        
         BE    XIT                 NO ERROR                                     
         MVC   RERROR,=AL2(TOOLONG)                                             
         CLI   DMCB+12,5                                                        
         BE    MYERR                                                            
         DC    H'0'                BAD RETURN CODE                              
         SPACE 1                                                                
*VUPT10   MVC   RERROR,=AL2(INVALID)                                            
*         CLC   8(8,R2),=C'UPT=PUT/'                                            
*         BE    VUPT20                                                          
*         CLC   8(8,R2),=C'UPT=HUT/'                                            
*         BE    VUPT20                                                          
*         CLC   8(8,R2),=C'UPT=HPT/'                                            
*         BNE   MYERR                                                           
*VUPT20   XC    BLOCK(256),BLOCK                                                
*         XC    DMCB+8(4),DMCB+8                                                
*         GOTO1 SCANNER,DMCB,(24,(R2)),BLOCK                                    
*         LA    R4,BLOCK                                                        
* READ INPUT STRING FOR UPGRADE EXPRESSION AND BUILD FLDHDR                     
         SPACE 1                                                                
*         MVC   CUPPRG,22(R4)       MOVE DATA TO SAVE AREA                      
*         XC    ELEM,ELEM                                                       
*         IC    RE,1(R4)            GET EXPRESSION LENGTH                       
*         STC   RE,ELEM+5           SET INPUT STRING LENGTH                     
*         BCTR  RE,0                SET FOR EX                                  
*         EX    RE,*+8                                                          
*         B     *+10                                                            
*         MVC   ELEM+8(0),22(R4)    MOVE DATA                                   
*         LA    RE,9(RE)            ADJUST LEN TO INCLUDE FLDHDR                
*         STC   RE,ELEM             AND SET IN FLDHDR                           
         SPACE 1                                                                
* GET UPVAL ADDRESS                                                             
*         GOTO1 UPVAL,DMCB,ELEM,WORK,(C'/',ACOMFACS)                            
*         CLI   0(R1),0             TEST VALID EXPRESSION                       
*         BNE   *+14                ERROR                                       
*         MVC   RERROR,=AL2(BADUPGR)                                            
*         B     MYERR                                                           
*         MVC   CUPTYPE(8),WORK+4                                               
         SPACE 1                                                                
*         LA    R4,46(R4)           NON-STANDARD SCANNER                        
*         OC    0(2,R4),0(R4)       TEST ANY MORE FIELDS                        
*         BZ    VUPTX                                                           
         SPACE 1                                                                
*         CLC   =C'BK',12(R4)                                                   
*         BE    VUPT30                                                          
         SPACE 1                                                                
*         MVC   RERROR,=AL2(INVALID)                                            
*         B     MYERR                                                           
         SPACE 1                                                                
* EDIT OVERRIDE FOR SHARE BOOK                                                  
         SPACE 1                                                                
*VUPT30   CLI   1(R4),0                                                         
*         BNE   *+14                                                            
*VUPT40   MVC   RERROR,=AL2(BADOBOK)                                            
*         B     MYERR                                                           
*         XC    ELEM,ELEM                                                       
*         ZIC   RE,1(R4)                                                        
*         STC   RE,ELEM+5                                                       
*         BCTR  RE,0                                                            
*         EX    RE,*+8                                                          
*         B     *+10                                                            
*         MVC   ELEM+8(0),22(R4)    **EXECUTED**                                
*         LA    RE,9(RE)                                                        
*         STC   RE,ELEM                                                         
         SPACE 1                                                                
*         GOTO1 BOOKVAL,DMCB,(C'N',ELEM),(1,DUB),SCANNER                        
*         CLI   4(R1),0                                                         
*         BE    VUPT40                                                          
*         TM    DUB,X'BF'           TEST ANY GARBAGE OPTIONS SPECIFIED          
*         BNZ   VUPT40                                                          
*         MVC   CUPFBK,DUB+1                                                    
         SPACE 1                                                                
*VUPTX    B     XIT                                                             
*         DROP  R3                                                              
         EJECT                                                                  
* THIS ROUTINE FACILITATES THE ACTION OF SWITCHING, WITHIN A SINGLE             
* TRANSACTION, BETWEEN ONE OVERLAY AND ANOTHER BY CHANGING THE RECORD,          
* ACTION, PRINT AND KEY FIELDS ON THE SCREEN AND CALLING GENCON AGAIN.          
* FIRST IT OPTIONALLY SAVES THE CURRENT TWA IN TEMPSTR AND PUSHES THE           
* CURRENT OVERLAY NUMBER ONTO A STACK.  THEN IT CHANGES THE RECORD,             
* ACTION, PRINT AND KEY FIELDS TO THE DATA SPECIFIED IN THE PARAMETER           
* LIST.  FINALLY, IT SETS THE FLAG 'GOAGAIN' TO 'Y' AND TAKES AN ERROR          
* EXIT BACK TO GENCON WHICH RETURNS BACK TO THE CONTROLLER.  THE                
* CONTROLLER THEN RECOGNIZES THE FLAG AND CALLS GENCON AGAIN.  KEY              
* PARAMETERS ARE POSITIONAL, ENDING WITH A ZERO.                                
*                                                                               
* (FLAG,0),RECORD,ACTION,PRINT,(L'KEY1,KEY1),(L'KEY2,KEY2),...,0                
*              IF FLAG IS A 'Y', THEN SCREEN IS SAVED                           
*              RECORD, ACTION, AND KEY1 ARE REQUIRED                            
*                                                                               
*  (RA POINTS TO TWA)                                                           
CPROG    LR    R4,R1               SAVE POINTER TO PARMS                        
*                                                                               
         CLI   0(R4),C'Y'          SHOULD WE SAVE THE SCREEN?                   
         BE    *+18                YES                                          
         MVI   CALLSP,0            NO -- CLEAR STACK                            
         XC    CALLSTCK,CALLSTCK                                                
         B     CP3                                                              
*                                                                               
         ZIC   R3,CALLSP           GET STACK POINTER                            
         LR    R2,R3               R2=ORIGINAL STACK POINTER VALUE              
         MH    R3,=H'3'                                                         
         LA    RF,CALLSTCK(R3)     RF=A(NEXT POSITION)                          
         MVC   0(1,RF),MYSCRNUM    SLOT IN SCREEN NUMBER                        
         MVC   1(1,RF),TWALACT                                                  
         MVC   2(1,RF),TWALREC     SLOT IN SCREEN NUMBER                        
         LA    R3,1(R2)                                                         
         STC   R3,CALLSP                                                        
         CLI   CALLSP,4            TEST MORE THAN 4 NEST LEVELS                 
         BNH   *+6                                                              
         DC    H'0'                YES-A REAL PROBLEM                           
*                                                                               
         SRL   R2,1                DIVIDE ORIGINAL LEVEL BY TWO                 
         LA    R2,3(R2)            ADD BACK THREE TWA PAGES                     
         SLL   R2,32-8             MOVE TO HIGH ORDER BYTE                      
         ICM   R2,3,2(RA)                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STC   R3,BYTE             SAVE STACK LEVEL                             
         L     RE,ATIA             RE=DESTINATION                               
         LA    RF,3072             MOVE/CLEAR HALF A TWA                        
         LA    R0,CONRECH          START AT RECORD HEADER                       
         LR    R1,RF               MOVE RECORD HEADER FOR 3072 BYTES            
         TM    BYTE,X'01'          TEST FOR ODD NUMBER                          
         BO    *+6                 YES                                          
         AR    RE,RF               NO-MOVE TO SECOND HALF OF TWA                
         MVCL  RE,R0                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R2),ATIA,0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CP3      LA    R4,4(R4)            BUMP TO SECOND PARM                          
         XC    CONREC,CONREC       SET NEW RECORD TYPE                          
         OI    CONRECH+6,X'80'                                                  
         NI    CONRECH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONRECH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONREC(0),0(RF)                                                  
*                                                                               
         LA    R4,4(R4)            BUMP TO THIRD PARM                           
         XC    CONACT,CONACT       SET NEW ACTION TYPE                          
         OI    CONACTH+6,X'80'                                                  
         NI    CONACTH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONACT(0),0(RF)                                                  
*                                                                               
         LA    R4,4(R4)            BUMP TO FOURTH PARM                          
         XC    CONWHEN,CONWHEN     SET NEW PRINT OPTION                         
         OI    CONWHENH+6,X'80'                                                 
         NI    CONWHENH+4,X'DF'                                                 
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONWHENH+5                                                    
         CLI   CONWHENH+5,0        IS THERE A PRINT OPTION?                     
         BE    CP5                 NO                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONWHEN(0),0(RF)                                                 
*                                                                               
CP5      LA    R4,4(R4)            BUMP TO FIFTH PARM                           
         XC    CONKEY,CONKEY       SET NEW KEY FIELDS                           
         OI    CONKEYH+6,X'80'                                                  
         NI    CONKEYH+4,X'DF'                                                  
         LA    R2,CONKEY           BUILD KEY FIELD FROM FIFTH PARM ON           
         LR    R3,R2                                                            
*                                                                               
CP10     L     RF,0(R4)            ADD PARM TO KEY FIELD                        
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
         LA    R2,0(R2,RE)         BUMP R2 PAST IT                              
*                                                                               
         CLI   0(R2),C' '          BACKUP UNTIL NON-SPACE                       
         BH    *+10                                                             
         BCTR  R2,0                                                             
         B     *-10                                                             
         LA    R2,1(R2)                                                         
*                                                                               
         LA    R4,4(R4)            BUMP TO NEXT PARM                            
         OC    0(4,R4),0(R4)                                                    
         BZ    CPX                 IF PARM IS ZERO THEN NO MORE PARMS           
*                                                                               
         MVI   0(R2),C','          ELSE PUT COMMA BEFORE NEXT PARM              
         LA    R2,1(R2)            BUMP R2 TO WHERE NEXT PARM GOES              
         B     CP10                DISPLAY NEXT PARM                            
*                                                                               
CPX      SR    R2,R3               INSERT FAKE INPUT LEN INTO KEY FIELD         
         STC   R2,CONKEYH+5                                                     
*                                                                               
         MVI   RETURNED,0          CLEAR FLAG INDICATING RETURN                 
*                                                                               
         MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* THIS ROUTINE RESTORES THE TWA AND OVERLAY NUMBER TO THAT WHICH IS             
* ON THE TOP OF THE OVERLAY STACK.  IT THEN SETS THE 'GOAGAIN' FLAG             
* TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON.  WHEN THE CONTROLLER         
* GETS CONTROL BACK IT WILL CALL GENCON AGAIN WITH THE RESTORED SCREEN.         
* (RA POINTS TO TWA)                                                            
*                                                                               
RPROG    ZIC   R3,CALLSP           GET STACK POINTER                            
         BCTR  R3,0                DECREMENT POINTER TO POP STACK               
         STC   R3,CALLSP                                                        
         LR    R2,R3                                                            
         MH    R3,=H'3'                                                         
         LA    RE,CALLSTCK(R3)                                                  
         MVC   MYSCRNUM,0(RE)      EXTRACT OVERLAY NUMBER                       
         MVC   ACTNUM,1(RE)      EXTRACT OVERLAY NUMBER                         
         MVC   RECNUM,2(RE)      EXTRACT OVERLAY NUMBER                         
*^^GYL   XC    0(3,RE),0(RE)     (KEEP TRACE OF CALLSTCK FOR DEBUGGING)         
*                                                                               
         SRL   R2,1                DIVIDE LEVEL BY TWO                          
         LA    R2,3(R2)            START AT TWA PAGE 3                          
         SLL   R2,32-8             MOVE PAGE TO HOB                             
         ICM   R2,3,2(RA)                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CONRECH                                                       
         LA    RF,3072             MOVE RECORD HEADER FOR 3072 BYTES            
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         LA    R3,1(R3)            RESTORE ORIGINAL LEVEL                       
         STC   R3,BYTE                                                          
         TM    BYTE,X'01'          TEST FOR ODD LEVEL                           
         BO    *+6                 YES                                          
         AR    R0,R1               NO-MUST BE IN SECOND HALF OF PAGE            
         MVCL  RE,R0                                                            
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
         MVC   TWASCR,MYSCRNUM     DON'T LET GENCON RELOAD THE SCREEN           
         MVC   OVERLAY,MYSCRNUM    SET OVERLAY TOO                              
*                                                                               
         MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* THIS ROUTINE WILL DISPLAY THE DAY, TIME, AND TITLE FROM THE                   
* PURE RECORD                                                                   
*                                                                               
* ON ENTRY, CBLOCK WILL HAVE PURE NUMBER                                        
*           CBLOCK+6 WILL HAVE SATELLITE                                        
*           CBLOCK+7 WILL HAVE BOOKS                                            
*                                                                               
* ON EXIT, CBLOCK+10 WILL HAVE DAY                                              
*          CBLOCK+20 WILL HAVE TIME                                             
*          CBLOCK+40 WILL HAVE 1ST 27 CHARACTERS OF PROGRAM NAME                
         SPACE 2                                                                
DPURDTT  DS    0H                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRKEY,R6                                                         
         MVI   PRCODE,C'P'                                                      
         MVI   PRMED,C'T'                                                       
         MVC   PRSRC,CSOURCE                                                    
         MVC   PRSTAT,CCONKSTA                                                  
         CLI   PRSTAT+4,C' '                                                    
         BNE   *+8                                                              
         MVI   PRSTAT+4,C'T'                                                    
         CLI   CBLOCK+6,0          SATELLITE OPTION                             
         BE    *+10                                                             
         MVC   PRSTAT+4(1),CBLOCK+6                                             
         MVC   PRBOOK,CBLOCK+7     BOOK                                         
         SPACE 1                                                                
         BAS   RE,PAVHIGH                                                       
         L     R4,AIO                                                           
         CLC   PRKEY(PRSTYP-PRKEY),0(R4)                                        
         BE    *+14                                                             
         MVC   CBLOCK+10(60),SPACES                                             
         B     DPURX                                                            
         SPACE 1                                                                
         MVC   PRSTIM(2),CBLOCK    INVENTORY NUMBER                             
         BAS   RE,DMFLHI                                                        
         CLC   PRKEY(PRRLEN-PRKEY),0(R4)                                        
         BE    *+14                                                             
         MVC   CBLOCK+10(60),SPACES                                             
         B     DPURX                                                            
         SPACE 1                                                                
         L     R4,AIO                                                           
         GOTO1 INVEDIT,DMCB,CBLOCK,WORK                                         
         MVC   CBLOCK+10(3),WORK           DAY                                  
         MVC   CBLOCK+20(6),WORK+3         TIME                                 
         LR    R6,R4                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   DPURX                                                            
         USING PPNELEM,R6                                                       
         ZIC   R1,PPNELN           LENGTH OF PROGRAM NAME                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CBLOCK+40(0),PPNNME                                              
         SPACE 1                                                                
DPURX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL            
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG.  SINCE RERMP             
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE            
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                               
*                                                                               
MYERR    OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,RINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,RERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,RMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,RTXTLEN      LENGTH OF OPTIONAL TEXT                      
         MVC   GTATXT,RTXTADR      A(OPTIONAL TEXT)                             
         CLC   RERROR,=H'60'       IF MESSAGE NUMBER <= 60                      
         BH    *+8                                                              
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
         SPACE 1                                                                
         GOTO1 ERREX                                                            
         B     XIT                 IN CASE WE RETURN                            
         EJECT                                                                  
* THIS ROUTINE GETS THE INVENTORY MASTER THAT BEST FITS THE KEY. IF             
* NO EFFECTIVE DATE IS PASSED THE MOST RECENT IS RETURNED. IF EFFECTIVE         
* DATE DOES NOT EXIST RECORD BEFORE THAT DATE IS PASSED. INPUT TO THIS          
* ROUTINE IS KEY.                                                               
*                                                                               
GETINVN  XC    WORK,WORK                                                        
         MVC   WORK(27),KEY        SAVE INPUT KEY                               
INVKEYD  USING RINVKEY,KEY                                                      
WRKKEYD  USING RINVKEY,WORK                                                     
*                                  GET FIRST RECORD                             
         XC    KEY,KEY                                                          
         MVC   KEY(RINVKSTD-RINVKEY),WORK                                       
         MVC   RERROR,=AL2(NOTFOUND)                                            
         GOTO1 HIGH                                                             
*                                                                               
GTINV10  DS    0H                  COMPARE UP UNTIL STATION                     
         CLC   KEY(RINVKINV-RINVKEY),KEYSAVE                                    
         BNE   GTINVERR                                                         
*                                  WAS INV NUMBER INPUTTED                      
         CLI   WRKKEYD.RINVKINV,0                                               
         BE    GTINVEX             NO, JUST GET 1ST STATION RECORD              
*                                                                               
*                                  COMPARE UP UNTIL EFFECTIVE DATE              
         CLC   KEY(RINVKSTD-RINVKEY),KEYSAVE                                    
         BNE   GTINVERR                                                         
*                                                                               
*                                  WAS EFFECTIVE DATE PASSED                    
         CLI   WRKKEYD.RINVKSTD,0                                               
         BE    GTINV260            NO GET LATEST                                
*                                                                               
         CLC   KEY(27),WORK       DOES EFF DATES MATCH                          
         BE    GTINV500           NOPE DO SKIP READ                             
*                                                                               
*  EFFECTIVE DATE PASSED GET LATEST RECORD PRIOR TO                             
*  THE INPUTTED EFFECTIVE DATE                                                  
*                                                                               
         XC    KEY,KEY                                                          
*                                  RE LOAD ORIGINAL KEY                         
         MVC   KEY(RINVKSTD-RINVKEY),WORK                                       
GTINV40  GOTO1 HIGH                                                             
*                                                                               
GTINV60  DS    0H                  HAVE WE PASSED OUR KEY                       
         CLC   KEY(RINVKSTD-RINVKEY),WORK                                       
         BNE   GTINV100                                                         
*                                  COMPARE EFFECTIVE DATES                      
         CLC   INVKEYD.RINVKSTD,WRKKEYD.RINVKSTD                                
         BH    GTINV100                                                         
         MVC   WORK+30(27),KEY     SAVE THIS KEY                                
*                                  BUMP EFFECTIVE DATE BY 1                     
         ZIC   RE,INVKEYD.RINVKSTD+2                                            
         LA    RE,1(RE)                                                         
         STCM  RE,1,INVKEYD.RINVKSTD+2                                          
*                                  CLEAR REST OF KEY                            
         XC    INVKEYD.RINVKSPR(RINVLEN-RINVKSPR),INVKEYD.RINVKSPR              
         B     GTINV40                                                          
*                                                                               
GTINV100 CLI   WORK+30,0           DID ANY KEYS QUALIFY                         
         BE    GTINVERR            NO ERROR                                     
         B     GTINV400                                                         
*                                                                               
*-- NO EFFECTIVE DATE PASSED GET LATEST INVENTORY RECORD                        
*                                                                               
GTINV200 XC    KEY,KEY                                                          
*                                  RE LOAD ORIGINAL KEY                         
         MVC   KEY(RINVKSTD-RINVKEY),WORK                                       
GTINV240 GOTO1 HIGH                                                             
*                                                                               
GTINV260 DS    0H                  HAVE WE PASSED OUR KEY                       
         CLC   KEY(RINVKSTD-RINVKEY),WORK                                       
         BNE   GTINV300                                                         
                                                                                
*                                  SAVE KEY IF INVENTORY HEADER,                
         OC    INVKEYD.RINVKRTP,INVKEYD.RINVKRTP                                
         BNZ   *+10                 OTHERWISE DON'T                             
         MVC   WORK+30(27),KEY     SAVE THIS KEY                                
                                                                                
*                                  BUMP EFFECTIVE DATE BY 1                     
         ZIC   RE,INVKEYD.RINVKSTD+2                                            
         LA    RE,1(RE)                                                         
         STCM  RE,1,INVKEYD.RINVKSTD+2                                          
*                                  CLEAR REST OF KEY                            
         XC    INVKEYD.RINVKSPR(RINVLEN-RINVKSPR),INVKEYD.RINVKSPR              
         B     GTINV240            DO SKIP READ                                 
*                                                                               
GTINV300 CLI   WORK+30,0           DID ANY KEY QUALIFY                          
         BE    GTINVERR            NO ERROR                                     
         B     GTINV400                                                         
*                                                                               
GTINV400 MVC   KEY(27),WORK+30     GET LAST QUALIFIED RECORD                    
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
GTINV500 MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
GTINVEX  XC    RERROR,RERROR                                                    
         B     XIT                                                              
*                                                                               
GTINVERR CLI   ACTNUM,ACTADD                                                    
         BE    XIT                                                              
         B     MYERR                                                            
         EJECT                                                                  
* COMMON ROUTINE TO LOCK RECS BY TYPE, OR CHECK TO SEE IF LOCKED *              
         SPACE                                                                  
VLOC     LA    R4,WORK                                                          
         USING LKKEYD,R4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)          SAVE LOCK OPTION                             
         BNZ   *+8                                                              
         LA    R0,LKTESTQ             DEFAULT TO TEST FOR LOCK                  
*                                                                               
         XC    WORK,WORK                                                        
         L     R6,ACOMFACS                                                      
         SPACE                                                                  
         L     RF,CGETFACT-COMFACSD(,R6)                                        
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         MVC   LOCKSE,FASYS-FACTSD(RE)                                          
         SPACE                                                                  
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=CL2'RI'                                                 
         MVC   LOCKKEY(5),CSTAT                                                 
         XC    LOCKKEY+5(5),LOCKKEY+5                                           
         SPACE                                                                  
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2               CLEAR INCASE OF LOCKET ERROR                 
         L     RF,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         SPACE                                                                  
         PRINT GEN                                                              
         GOTO1 (RF),(R1),((R0),WORK),(R6)                                       
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VLOCERR                                                          
*                                                                               
VLOCEX   B     XIT                                                              
*--LOCK  ERROR                                                                  
VLOCERR  MVC   RERROR,=AL2(RECLOCKD)                                            
         B     MYERR                                                            
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 1                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
GETTIME  DC    0H'0'                                                            
         ST    RE,FULL             SAVE RE                                      
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         AP    FATIME,=P'80000'                                                 
         MVC   WORK(4),FATIME                                                   
         L     RE,FULL             RESTORE RE                                   
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 2                                                                
* THIS ROUTINE WILL GET THE AGENCY/MEDIA CODE                                   
         SPACE 1                                                                
GETAGY   ST    RE,FULL                                                          
* FIRST GET AGENCY/MEDIA CODE                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVC   CAGYPROF,AGYPROF                                                 
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     GAM20                                                            
GAM10    BAS   RE,NEXTEL                                                        
GAM20    BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING AGYMEDEL,R6                                                      
         CLI   AGYMEDCD,C'T'        WANT TELEVISION                             
         BNE   GAM10                                                            
         MVC   CMED,AGYMEDCD                                                    
         MVC   CAGYMED,AGYMEDBT                                                 
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1                                                         
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        CKGLOB --- CHECK FOR GLOBBER LOADER VARIABLES FROM THE                 
*                   RSC PROGRAM                                                 
*                                                                               
CKGLOB   NTR1                                                                   
*                                                                               
         CLI   TWAMODE,1               1=OFFLINE                                
         BE    CKGLX                                                            
******************************************************************              
**        FOLLOWING BLOCK OF COMMENTED OUT CODE SEEMS TO BE                     
**          VESTIGIAL SFM REMAINS THAT DO NOT BELONG HERE                       
**                                                                              
**       L     R4,ACOMFACS                                                      
**       USING COMFACSD,R4                                                      
**       GOTO1 CGLOBBER,DMCB,=C'GETF',CONKEYH,,GLRCONNO                         
**       CLI   DMCB+8,0                                                         
**       BNE   CKGLX                                                            
**       GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLRCONNO                                
**FULL HAS STATUS WORD                                                          
**       GOTO1 CGLOBBER,DMCB,=C'GETD',PFKEY,L'PFKEY,GLRPFKEY                    
**       GOTO1 CGLOBBER,DMCB,=C'GETD',FULL,L'FULL,GLRSTAT                       
**       CLI   PFKEY,4                                                          
**       BNE   *+10                                                             
**       MVC   CONREC(5),=C'AHEAD'                                              
**       CLI   PFKEY,5                                                          
**       BNE   *+10                                                             
**       MVC   CONREC(5),=C'PHEAD'                                              
**       CLI   PFKEY,6                                                          
**       BNE   *+10                                                             
**       MVC   CONREC(5),=C'*OPEN'                                              
**       OI    CONRECH+6,X'80'     XMIT                                         
**       MVI   CONRECH+4,X'80'     TURN ON FIELD INPUT THIS TIME                
**       MVI   CONRECH+5,5         SET INPUT LENGTH                             
**       MVC   CONACT(3),=C'ADD'                                                
**                                                                              
** CKGLOB20 DS    0H                                                            
**       OI    CONACTH+6,X'80'                                                  
**       MVI   CONACTH+4,X'80'                                                  
**       MVI   CONACTH+5,3                                                      
**       ZIC   R1,CONKEYH+5        GET LENGTH OF KEY                            
**       LA    R2,CONKEY                                                        
**       AR    R2,R1               PT TO NEXT BLANK IN KEY                      
** 07MAY90  LA    R1,1(R1)                                                      
** 07MAY90  STC   R1,CONKEYH+5                                                  
** 07MAY90  MVC   0(2,R2),=C',I'     FIELD DOESN'T EXIST ANYMORE                
**       OI    CONKEYH+6,X'80'                                                  
**       MVI   CONKEYH+4,X'80'                                                  
** 07MAY90  MVI   CONKEYH+5,9                                                   
**       TM    FULL,X'80'          NOTE STATUS IS FULL WORD                     
**       BZ    CKGLX                                                            
**       MVC   0(13,R2),=C',,SAR,SAR,SAR'                                       
**       LA    R1,13(R1)                                                        
**       STC   R1,CONKEYH+5                                                     
**                                                                              
**       CKGLOB EXIT                                                            
**                                                                              
**********************************************************************          
                                                                                
                                                                                
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
                                                                                
         XC    BLOCK(256),BLOCK       CLEAR FOR ELEMENT RETRIEVAL               
         GOTO1 CGLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL   XCTL ELEM?             
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLX                                                            
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
                                                                                
         L     RD,4(RD)              POP BACK TWICE                             
                                                                                
CKGLX    B     XIT                                                              
         SPACE 3                                                                
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        SETPROFS --- GET AND SET SFM/REP PROFILES FROM THE REP REC             
*                                                                               
SETPROFS NTR1                                                                   
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AH    R2,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R2                                                       
*                                                                               
*- IF NOT ALREADY DONE, READ IN REP RECORD FOR PGM PROFILE                      
         CLC   SVPGREP(2),AGENCY                                                
         BNE   SETP005                                                          
         CLI   SVPGP#,RREPQRMP     USE TO BE RSC (SAME EQUATED#)                
         BE    SETP100             IN TWA FROM PRIOR HIT                        
*                                                                               
SETP005  EQU   *                                                                
         XC    SVPGENTY,SVPGENTY                                                
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   SETP010                                                          
         MVC   SVPGREP(2),AGENCY                                                
         MVI   SVPGP#,RREPQSFM                                                  
****>>   MVC   SVPGPBIT,=8X'FF'                                                 
*                                                                               
*   DON'T SET ALL BITS FOR DDS TERMINAL!!                                       
*                                                                               
*        MVI   CSTRT6AM,C'D'       READ REP PROFILE ONLY FOR 6AM PROF           
*        DROP  R8                  WE ARE CHEATING BY USING CSTRT6AM            
*                                  FOR A TEMPORARY FLAG                         
SETP010  EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),AGENCY                                                 
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEYSAVE,KEY,         X        
               0,0                                                              
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                REP RECORD NOT ON FILE?  HOW?                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,BUFF,        X        
               DMWORK,0                                                         
*                                                                               
         LA    R6,BUFF                                                          
         USING RREPREC,R6                                                       
*                                                                               
*        CLI   CSTRT6AM,C'D'       READ REP PROF AND EXIT                       
*        BNE   SETP015                                                          
*        MVC   CSTRT6AM,RREPPROF+4                                              
*        B     SETP100                                                          
*                                                                               
SETP015  DS    0H                                                               
         MVC   CSTRT6AM,RREPPROF+4                                              
*                                                                               
         MVC   SVPGREP(2),AGENCY                                                
         MVI   SVPGP#,RREPQSFM                                                  
*                                                                               
         LA    RE,RREPREC          RECORD IS HERE                               
         ZICM  RF,RREPLEN,2                                                     
         DROP  R6                                                               
         AR    RF,RE                                                            
         MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
SETP020  EQU   *                                                                
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BE    SETP100                                                          
*                                                                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    SETP040                                                          
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     SETP020                                                          
*                                                                               
*- FIND SFM PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                         
SETP040  EQU   *                                                                
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
         DROP  RE                                                               
*                                                                               
SETP050  CLI   0(RE),RREPQRMP      LOOKING FOR RESEARCH                         
         BE    SETP060                                                          
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,SETP050                                                       
         B     SETP100             NO MATCH. USE DEFAULTS                       
*                                                                               
SETP060  MVC   SVPGPBIT(8),2(RE)   SAVE UNIT IN TWA.                            
         B     *+8                          <-- NOOP TO TEST OPTION             
         OI    SVPGPBIT+RMPIMPSB,RMPIMPSA   <-- TESTING IMPS OPTN               
         SPACE                                                                  
SETP100  EQU   *                                                                
         LA    RE,BUFF                                                          
         LA    RF,4000                                                          
         XCEF                                                                   
*&&DO                                                                           
         XC    0(250,RE),0(RE)                                                  
         XC    250(250,R3),250(RE)                                              
         XC    500(250,R3),500(RE)                                              
         XC    750(250,R3),750(RE)                                              
*&&                                                                             
SETPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=SYSRB,WORK=(R4,8),LABEL=*                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,SYSR7                                                         
         L     R5,SYSR5                                                         
         L     R8,ASPOOLD                                                       
         LR    R6,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,CAGYPROF+7                                              
         MVC   STAPMED,CMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R6)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R6)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R6)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GOMSUNPK NTR1  BASE=SYSRB,WORK=(R4,8),LABEL=*                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,SYSR7                                                         
         L     R5,SYSR5                                                         
         L     R8,ASPOOLD                                                       
         LR    R6,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,CAGYPROF+7                                              
         MVC   STAPMED,CMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R6)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R6)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R6)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R6),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SVDXDA   DS    F                   FOR PAV FILE READ                            
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(UNBOOK)                                                        
         DC    V(DUMMY)                                                         
         DC    V(CLPACK)                                                        
         DC    V(UNUPGR)                                                        
         DC    V(RETEXT)                                                        
         DC    V(UNTEXT)                                                        
         DC    V(RECUP)                                                         
         DC    V(UPOUT)                                                         
         DC    V(PAVSTA)                                                        
         DC    V(INVDAY)                                                        
         DC    V(HRTOQH)                                                        
         DC    V(GETKSRC)                                                       
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
*  TABLE OF CORE RESIDENT MODULE ADDRESSES                                      
CORETAB  DS    0X                                                               
         DC    X'30'               GENCON                                       
         DC    X'E0'               DEMOCON                                      
         DC    X'47'               RANSID                                       
         DC    X'26'               DEFINE                                       
         DC    X'21'               SPDEMLK (SPGETDEMO)                          
         DC    X'13'               UPVAL                                        
         DC    X'22'               SPDEMUP                                      
         DC    AL1(QMSPACK)        MSPACK                                       
         DC    AL1(QMSUNPK)        MSUNPK                                       
         DC    AL1(QQSORT)         QSORT                                        
         DC    X'08'               DEMUP                                        
         DC    X'09'               INVEDIT                                      
         DC    AL1(QGETBROD)       GETBROAD                                     
         DC    AL1(QLINUP)         LINUP                                        
         DC    AL1(QREGTIUN)       REGETIUN                                     
         DC    AL1(QREFETCH)       FETCH                                        
*                                                                               
CORES    EQU   (*-CORETAB)                                                      
         SPACE 3                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
*              TABLE OF AUTHORIZATION VALUES FOR VARIOUS ACTIONS                
         SPACE 2                                                                
*   NOTE - OVER HAS NO RESTRICTIONS                                             
*          OVRPC HAS NO RESTRICTIONS                                            
*          INVENTORY TEXT HAS NO RESTRICTIONS                                   
*          SRA HAS STATION RESTRICTIONS - SEE T81806,T81807                     
         DS    0D                                                               
         SPACE 1                                                                
*                                  CL8 EXPANDED RECORD NAME                     
*                                  XL2 AUTHORIZATION VALUES                     
         SPACE 1                                                                
AUTHTAB  DS    0CL10                                                            
*        DC    C'DPT     ',X'800F'                                              
         DC    C'PRGT    ',X'800F'                                              
         DC    C'SDD     ',X'800F'                                              
         DC    C'DR      ',X'900F'                                              
         DC    C'DRN     ',X'900F'                                              
         DC    C'GOALN   ',X'800F'                                              
         DC    C'GOAL    ',X'800F'                                              
         DC    C'COMM    ',X'400F'                                              
         DC    C'SWITCH  ',X'200F'                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 3                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
RECACT1  DC    X'01',C'TEXT    ',AL1(02),X'0000'                                
         DC    X'01',C'MARKET  ',AL1(03),X'0000'                                
*        DC    X'01',C'MPR     ',AL1(04),X'0000'                                
*        DC    X'01',C'M2      ',AL1(05),X'0000'                                
         DC    X'04',C'ODELETE ',AL1(06),X'0000'                                
         DC    X'01',C'INV     ',AL1(07),X'0000'                                
         DC    X'01',C'DINV    ',AL1(27),X'0000'                                
         DC    X'01',C'SINV    ',AL1(08),X'0000'                                
         DC    X'01',C'MINV    ',AL1(09),X'0000'                                
         DC    X'04',C'OTRANS  ',AL1(10),X'0000'                                
         DC    X'04',C'OPROJ   ',AL1(11),X'0000'                                
         DC    X'01',C'MULTI   ',AL1(12),X'0000'                                
         DC    X'01',C'SWAP    ',AL1(13),X'0000'                                
         DC    X'01',C'DPT     ',AL1(14),X'0000'                                
         DC    X'01',C'MENU    ',AL1(15),X'0000'                                
****     DC    X'01',C'DTRANS  ',AL1(16),X'0000'                                
         DC    X'01',C'REPCH   ',AL1(17),X'0000'                                
         DC    X'01',C'DMENU   ',AL1(18),X'0000'                                
         DC    X'01',C'ETRANS  ',AL1(19),X'0000'                                
         DC    X'01',C'LTRANS  ',AL1(20),X'0000'                                
         DC    X'04',C'SCOPY   ',AL1(21),X'0000'                                
         DC    X'04',C'BTRANS  ',AL1(22),X'0000'                                
***      DC    X'04',C'FILEFIX ',AL1(23),X'0000' <== FIXES INVNTRY FILE         
         DC    X'04',C'TCOPY   ',AL1(24),X'0000'                                
         DC    X'01',C'GLOBAL  ',AL1(25),X'0000'                                
******** DC    X'01',C'REPTR   ',AL1(26),X'0000'                                
XTRANS   DC    X'01',C'XTRANS  ',AL1(27),X'0000' <== UNIX FILE TESTING          
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         DC    X'02',C'UPDATE  ',AL1(13,13,00)                                  
         DC    X'02',C'COPY    ',AL1(15,15,00)                                  
         DC    X'02',C'PCOPY   ',AL1(16,16,00)                                  
         DC    X'02',C'TRACKS  ',AL1(17,17,00)                                  
         DC    X'02',C'ROVER   ',AL1(18,18,00)                                  
         DC    X'02',C'DOVER   ',AL1(19,19,00)                                  
         DC    X'02',C'TKDELETE',AL1(20,20,00)                                  
         DC    X'02',C'TKRESTOR',AL1(21,21,00)                                  
         DC    X'02',C'DEMOS   ',AL1(22,22,00)                                  
*        DC    X'02',C'M       ',AL1(23,23,00)                                  
         DC    X'02',C'DTRACKS ',AL1(24,24,00)                                  
                                                                                
* THE FOLLOWING ACTIONS ARE YET TO BE IMPLEMENTED                               
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 3                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
         DC    X'03',AL1(02,01),X'D105000080',C'    '  TEXT     MAINT           
         DC    X'03',AL1(02,10),X'D205000080',C'    '  TEXT     LIST            
         DC    X'03',AL1(02,12),X'D205000058',C'    '  TEXT     REPORT          
         DC    X'03',AL1(03,01),X'D306000080',C'    '  MKT      MAINT           
         DC    X'03',AL1(03,10),X'D406000080',C'    '  MKT      LIST            
         DC    X'03',AL1(03,12),X'D406000058',C'AMAM'  MKT      RPT             
         DC    X'03',AL1(04,12),X'D507000878',C'SBSB'  MPR      RPT             
         DC    X'03',AL1(05,12),X'D507000818',C'S2S2'  MPR - 2 COPIES           
         DC    X'03',AL1(06,12),X'D6090A0B38',C'XDXD'  OV DELTE RPT             
         DC    X'03',AL1(07,01),X'D710000080',C'    '  INV      MAINT           
         DC    X'03',AL1(07,10),X'D810000080',C'    '  INV      LIST            
         DC    X'03',AL1(07,12),X'D810000078',C'VNVN'  INV      LIST            
         DC    X'03',AL1(27,01),X'D731000080',C'    '  DINV     MAINT           
         DC    X'03',AL1(27,10),X'D831000080',C'    '  DINV     LIST            
         DC    X'03',AL1(07,17),X'E817000089',C'    '  INV      TRACKS          
         DC    X'03',AL1(07,24),X'E817000089',C'    '  INV      DTRACKS         
         DC    X'03',AL1(07,18),X'E718000089',C'    '  INV      ROVER           
         DC    X'03',AL1(07,20),X'E718000089',C'    '  INV      TKDELET         
         DC    X'03',AL1(07,21),X'E718000089',C'    '  INV      TKRESTO         
         DC    X'03',AL1(07,22),X'BE41000089',C'    '  INV      DEMOS           
         DC    X'03',AL1(08,01),X'D911000080',C'    '  MTRACK   MAINT           
         DC    X'03',AL1(08,10),X'D811000080',C'    '  MTRACK   LIST            
         DC    X'03',AL1(09,01),X'DB12000080',C'    '  MINV     MAINT           
         DC    X'03',AL1(10,12),X'E121222338',C'XTXT'  OV TRANS RPT             
         DC    X'03',AL1(11,12),X'E224252638',C'XPXP'  OV PROJ  RPT             
         DC    X'03',AL1(12,01),X'DC15000080',C'    '  INV      MAINT           
         DC    X'03',AL1(13,01),X'E316000080',C'    '  SWAP     MAINT           
         DC    X'03',AL1(13,10),X'E416000080',C'    '  SWAP     MAINT           
         DC    X'03',AL1(07,19),X'E619000089',C'    '  INV      DOVER           
         DC    X'03',AL1(14,01),X'FA1A000080',C'    '  DAYPART  MAINT           
         DC    X'03',AL1(14,10),X'EA1A001A80',C'    '  DAYPART  LIST            
         DC    X'03',AL1(14,12),X'EA1A001A78',C'DPDP'  DAYPART  REPORT          
         DC    X'03',AL1(15,01),X'FB1B000080',C'    '  MENU     MAINT           
         DC    X'03',AL1(15,10),X'EB1B001B80',C'    '  MENU     LIST            
         DC    X'03',AL1(15,12),X'EB1B001B78',C'MNMN'  MENU     REPORT          
         DC    X'03',AL1(16,12),X'EC1C001C18',C'DTDT'  DEM TRAN RPT             
         DC    X'03',AL1(17,12),X'EE1E001E18',C'RXRX'  REP CHG RPT              
         DC    X'03',AL1(18,01),X'C11F000080',C'    '  DEMO MNU MAINT           
         DC    X'03',AL1(18,10),X'C21F000080',C'    '  DEMO MNU LIST            
         DC    X'03',AL1(18,12),X'C21F000078',C'DMDP'  DEMO MNU REPORT          
         DC    X'03',AL1(19,12),X'ED20002028',C'ETET'  DEM TRAN RPT(ED)         
         DC    X'03',AL1(20,12),X'EC1C001C18',C'LTLT'  DEM TRAN RPT(OV)         
         DC    X'03',AL1(21,12),X'CF27002718',C'SCSC'  SCOPY    REPORT          
         DC    X'03',AL1(22,01),X'C329000080',C'    '  INV      BOOK TR         
***      DC    X'03',AL1(23,12),X'F00F000F18',C'FXFX'  FILEFIX  REPORT          
         DC    X'03',AL1(24,12),X'CE28000038',C'TCTC'  TCOPY    REPORT          
         DC    X'03',AL1(25,01),X'C404000080',C'    '  GLOBAL   CHANGE          
         DC    X'03',AL1(26,12),X'C832003218',C'RXRX'  REP TRAN REPORT          
         DC    X'03',AL1(27,12),X'ED20002028',C'EXEX'  UNIX TEST (XTR)          
         DC    X'FF'                                                            
                                                                                
* THE FOLLOWING REC/ACT COMBOS ARE YET TO BE IMPLEMENTED                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R7,R5                                                         
*****************************************************************               
* PROCESS PF KEYS                                                               
*****************************************************************               
         DS    0H                                                               
VPFPROC  NMOD1 0,**PFPC**,RR=R2                                                 
         L     RC,0(R1)                                                         
         ST    R2,RELO2                                                         
*                                                                               
         L     RA,ATWA             RA=A(TWA)                                    
         USING CONHEADH-64,RA                                                   
*                                                                               
         LA    RE,PFTABLE          TABLE OF SCREENS AND PF KEYS                 
         MVC   HALF(1),MYSCRNUM    SAVED SCREEN NUMBER                          
         MVC   HALF+1(1),PFKEY     PRESSED PFKEY                                
                                                                                
VPF10    CLC   HALF,0(RE)          MATCH ON SCREEN/PFKEY?                       
         BE    VPF20               YES                                          
         LA    RE,8(RE)            BUMP TO NEXT ENTRY IN TABLE                  
         CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    VPFPROCX            YES                                          
         B     VPF10                                                            
                                                                                
VPF20    DS    0H                                                               
*                                                                               
         MVI   PFKEY,0             SO WE DON'T LOOP                             
*&&DO                                                                           
         MVI   CALLSP,0            CLEAR STACK POINTER                          
*&&                                                                             
VPF30    L     RF,4(RE)            A(CALLPROG CALL)                             
         A     RF,RELO2                                                         
         BR    RF                                                               
*                                                                               
*                                                                               
VPF60    CLC   CONACT(R3),=CL3'SEL'                                             
         BE    VPF65                                                            
         XC    CONACT,CONACT                                                    
         MVC   CONACT(7),=CL7'DISPLAY'                                          
VPF65    GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'SINV',=C'CHANGE',0,          X        
               (6,CCOSCRST),(4,CCONINV),(8,CCONEFF),0                           
*                                                                               
VPF80    CLC   CONACT(R3),=CL3'SEL'                                             
         BE    VPF85                                                            
*                                                                               
         CLI   ACTEQU,24           PF FROM INV DLIST?  !!!!                     
         BE    VPF82               YES                                          
         CLI   ACTNUM,ACTLIST      PF FROM INV LIST?                            
         BE    VPF82               YES                                          
*                                                                               
         XC    CONACT,CONACT                                                    
         MVC   CONACT(7),=CL7'DISPLAY'                                          
         B     VPF85                                                            
*                                                                               
VPF82    DS    0H                                                               
         OC    MINVTAB,MINVTAB     GO TO MINV?                                  
         BNZ   VPF85               YES                                          
         OC    INEFFTAB,INEFFTAB   GO TO ROVER OR TEXT?                         
         BZ    VPF90                                                            
*                                                                               
         CLI   INEFFTAB,C'R'       GO TO INV/ROVER?                             
         BE    VPF87                                                            
         CLI   INEFFTAB,C'O'       GO TO INV/DOVER?                             
         BE    VPF89                                                            
         CLI   INEFFTAB,C'T'       GO TO TEXT ADD?                              
         BE    VPF190                                                           
         B     VPF90                                                            
*                                                                               
VPF85    MVC   DUB,=8X'40'                                                      
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'MINV',=C'CHANGE',0,          X        
               (6,CCOSCRST),0                                                   
         B     VPF90                                                            
*                                                                               
VPF87    MVC   DUB,=8X'40'                                                      
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'INV',=C'ROVER',0,            X        
               (6,CCOSCRST),0                                                   
         B     VPF90                                                            
*                                                                               
VPF89    MVC   DUB,=8X'40'                                                      
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'INV',=C'DOVER',0,            X        
               (6,CCOSCRST),0                                                   
         B     VPF90                                                            
*                                                                               
* --SWAP TO MASTER REPORT                                                       
VPF90    L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
                                                                                
* - MOVE INDICATOR                                                              
         MVC   DUB(6),=CL6'MASTER' SEND INDICATOR                               
         DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',DUB,6,GLRCONNO                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'RMP'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'RSC'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VPFPROCX                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
VPF100   MVC   DUB,=8X'40'                                                      
         GOTO1 CALLPROG,BLOCK,(C'N',0),=C'INV',=C'LIST',0,             X        
               (6,CCOSCRST),0                                                   
*                                                                               
VPF120   LA    RF,BUFF                                                          
         MVC   0(2,RF),=CL2'PF'    SET DATA TRANSFER INDICATOR                  
         MVC   DUB,=8X'40'                                                      
         GOTO1 CALLPROG,BLOCK,(C'N',0),=C'MINV',=C'CHA',0,             X        
               (6,CCOSCRST),0                                                   
*                                                                               
VPF140   DS    0H                                                               
         CLC   CONACT(3),=CL3'SEL'                                              
         BE    VPF145                                                           
         XC    CONACT,CONACT                                                    
         MVC   CONACT(7),=CL7'DISPLAY'                                          
                                                                                
VPF145   DS    0H                                                               
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'INV',=C'TRACKS',0,           +        
               (6,CCOSCRST),(L'ITKINV,CCONINV),(8,CCONEFF),0                    
         B     VPFPROCX                                                         
                                                                                
         DS    0H                  PREVENTS CLOBBERING TWA W/ JUNK              
         DS    0XL(L'CCONINV-L'ITKINV+1)                                        
*                                                                               
VPF150   DS    0H                                                               
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'INV',=C'ROVER',0,            +        
               (6,CCOSCRST),(4,CCONINV),(8,CCONEFF),                   +        
               (L'IROSRC,CCONRSVC),(L'IROBOOK,CCONKBK),0                        
         B     VPFPROCX                                                         
                                                                                
         DS    0H                  PREVENTS CLOBBERING TWA W/ JUNK              
         DS    0XL(L'CCONRSVC-L'IROSRC+1)                                       
         DS    0XL(L'CCONKBK-L'IROBOOK+1)                                       
*                                                                               
VPF160   DS    0H                                                               
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'INV',=C'DOVER',0,            +        
               (6,CCOSCRST),(4,CCONINV),(8,CCONEFF),                   +        
               (L'DOVSRC,CCONRSVC),(L'DOVBOOK,CCONKBK),0                        
         B     VPFPROCX                                                         
                                                                                
         DS    0H                  PREVENTS CLOBBERING TWA W/ JUNK              
         DS    0XL(L'CCONRSVC-L'DOVSRC+1)                                       
         DS    0XL(L'CCONKBK-L'DOVBOOK+1)                                       
                                                                                
*                                                                               
VPF170   DS    0H                  INV TRA --> PF4 --> INV/TKDELETE             
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'INV',=C'TKDELETE',0,         +        
               (6,CCOSCRST),(4,CCONINV),(8,CCONEFF),                   +        
               (L'IROSRC,CCONRSVC),(L'IROBOOK,CCONKBK),0                        
         B     VPFPROCX                                                         
                                                                                
         DS    0H                  PREVENTS CLOBBERING TWA W/ JUNK              
         DS    0XL(L'CCONRSVC-L'IROSRC+1)                                       
         DS    0XL(L'CCONKBK-L'IROBOOK+1)                                       
*                                                                               
VPF190   DS    0H                                                               
         CLC   CONACT(R3),=CL3'SEL'                                             
         BE    VPF195                                                           
         CLI   ACTNUM,ACTLIST      PF FROM INV LIST?                            
         BE    VPF193              YES                                          
*                                                                               
         XC    CONACT,CONACT                                                    
         MVC   CONACT(7),=CL7'DISPLAY'                                          
         B     VPF195                                                           
*                                                                               
VPF193   DS    0H                                                               
         MVC   CCONINV,INEFFTAB+1  INV #                                        
         MVC   CCONEFF,INEFFTAB+5  EFFECTIVE DATE                               
*                                                                               
VPF195   XC    ELEM(20),ELEM                                                    
         MVC   ELEM(4),CCONINV                                                  
         LA    RE,ELEM                                                          
         LA    RF,4                                                             
*                                                                               
VPF200   CLI   0(RE),X'40'                                                      
         BNH   VPF205                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VPF200                                                        
VPF205   MVI   0(RE),C'\'                                                       
         MVC   1(8,RE),CCONEFF                                                  
*                                                                               
         CLI   ACTEQU,17           PF FROM INV/TRACK?                           
         BE    VPF207              YES                                          
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'TEXT',=C'CHANGE',0,          X        
               (6,CCOSCRST),(13,ELEM),0                                         
         B     VPFPROCX                                                         
*                                                                               
VPF207   GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'TEXT',=C'CHANGE',0,          X        
               (6,CCOSCRST),(13,ELEM),0                                         
         B     VPFPROCX                                                         
*                                                                               
*                                                                               
VPF210   DS    0H                  INV TRA --> PF9 --> INV/DEMOS                
         GOTO1 CALLPROG,BLOCK,(C'Y',0),=C'INV',=C'DEMOS',0,            +        
               (6,CCOSCRST),(4,CCONINV),(8,CCONEFF),0                           
         B     VPFPROCX                                                         
                                                                                
         DS    0H                  PREVENTS CLOBBERING TWA W/ JUNK              
         DS    0XL(L'CCONTRKS-L'IDMBKS+1)                                       
*                                                                               
VPFPROCX DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
RELO2    DS    A                                                                
*                                                                               
PFTABLE  DS    0F                                                               
*                                                                               
* BYTE 1:    SAVED SCREEN NUMBER                                                
* BYTE 2:    PFKEY NUMBER                                                       
* BYTE 3-4:  SPARE                                                              
* BYTE 5-8:  A(CALLPROG CALL)                                                   
*                                                                               
         DC    X'D7040000',A(VPF60)    INV MNT --> PF4 --> TRACK TRAN           
         DC    X'D7050000',A(VPF80)    INV MNT --> PF5 --> INV TRAN             
         DC    X'D7060000',A(VPF90)    INV MNT --> PF6 --> MASTRE RPT           
         DC    X'D7070000',A(VPF140)   INV MNT --> PF7 --> INV/TRACKS           
         DC    X'D7080000',A(VPF190)   INV MNT --> PF8 --> TEXT REC             
         DC    X'D8050000',A(VPF80)    INV LST --> PF5 --> MINV CHA             
         DC    X'D8050000',A(VPF80)    INV LST --> PF5 --> INV ROVER            
         DC    X'D8050000',A(VPF80)    INV LST --> PF5 --> INV DOVER            
         DC    X'D8050000',A(VPF80)    INV LST --> PF5 --> TEXT ADD             
         DC    X'DC040000',A(VPF100)   INV MLT --> PF4 --> INV LIST             
         DC    X'DC050000',A(VPF120)   INV MLT --> PF5 --> INV TRAN             
         DC    X'E8020000',A(VPF150)   INV TRA --> PF2 --> INV/ROVER            
         DC    X'E8030000',A(VPF160)   INV TRA --> PF3 --> INV/DOVER            
         DC    X'E8040000',A(VPF170)   INV TRA --> PF4 --> INV/TKDELETE         
         DC    X'E8090000',A(VPF210)   INV TRA --> PF9 --> INV/DEMOS            
         DC    X'E80A0000',A(VPF195)   INV TRA --> PF10--> TEXT/ADD             
         DC    X'DA050000',A(VPF80)    INV DLS --> PF5 --> MINV CHA             
         DC    X'DA050000',A(VPF80)    INV DLS --> PF5 --> INV ROVER            
         DC    X'DA050000',A(VPF80)    INV DLS --> PF5 --> INV DOVER            
         DC    X'DA050000',A(VPF80)    INV DLS --> PF5 --> TEXT ADD             
         DC    X'FFFF'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        ISSUE LTRANS REQUEST IF ON-LINE AND REP WANTS IT                       
*                                                                               
LTRANSR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    LTRNX                                                            
*                                                                               
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AH    R2,=AL2(SFMPROFS-CONHEADH)   POINT TO PROGRAM PROFILE            
         USING SVDSECT,R2                                                       
*                                                                               
         TM    SVPGPBIT+RMPLTRNB,RMPLTRNA    SKIP IF LTRANS NOT NEEDED          
         BNO   LTRNX                                                            
*                                                                               
         XC    IO(26),IO           BUILD REQUEST IN IO                          
         MVC   IO+26(80),SPACES    INIT REQUEST AREA                            
         MVC   IO+106(80),SPACES   INIT REQUEST AREA                            
*                                                                               
         LA    R4,IO+26                                                         
*                                                                               
         MVC   0(2,R4),=C'LT'      SET REQUEST CODE FOR LTRANS                  
         MVC   2(2,R4),AGENCY      SET REPID                                    
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SYSTEM INPUT NUMBER                      
         L     R4,DMCB                                                          
         USING FACTSD,R4                                                        
*                                                                               
         EDIT  (4,FASIN),(6,IO+31),FILL=0                                       
*                                                                               
         ZIC   R4,REQSEQNO         BUMP REQUEST NUMBER                          
         LA    R4,1(R4)                                                         
         STC   R4,REQSEQNO                                                      
*                                                                               
         LA    R4,REQSEQTB(R4)                                                  
         MVC   IO+37(1),0(R4)                                                   
*                                                                               
         LA    R4,IO                                                            
         USING REQOFFC,R4                                                       
*                                                                               
         MVC   REQOUT,TWAOUT                                                    
         MVC   REQDEST,TWADEST                                                  
*                                                                               
*        BUILD REQUEST RECORD                                                   
*                                                                               
         LA    R1,IO+26+12         START OF REQUEST IN CARD                     
*                                                                               
         MVC   0(10,R1),=C'0206LTRANS'   RECORD                                 
         LA    R1,11(R1)           NEXT AREA                                    
*                                                                               
         MVC   0(7,R1),=C'0303REP'  ACTION                                      
         LA    R1,8(R1)            NEXT AREA                                    
*                                                                               
         MVC   0(10,R1),=C'0506OV,***'  PRINT OPTION                            
         LA    R1,11(R1)           NEXT AREA                                    
*                                                                               
         MVC   0(2,R1),=C'11'      STATION FIELD                                
*                                                                               
         MVC   4(4,R1),CSTAT       STATION CALL LETTERS                         
*                                                                               
         LA    RF,7(R1)            POINT TO LAST CALL LETTER                    
*                                                                               
         CLI   0(RF),C' '          BACK UP IF BLANK                             
         BH    *+6                                                              
         BCTR  RF,0                                                             
*                                                                               
         CLI   CSTAT+4,C' '        NO BAND FOR TV                               
         BNH   LTRANS20                                                         
         CLI   CSTAT+4,C'T'                                                     
         BE    LTRANS20                                                         
*                                                                               
         MVI   1(RF),C'-'          PRINT BAND                                   
         MVC   2(1,RF),CSTAT+4                                                  
*                                                                               
         LA    RF,2(RF)                                                         
*                                                                               
LTRANS20 DS    0H                                                               
*                                                                               
         LA    RE,1(RF)                                                         
         SR    RE,R1               FIELD LENGTH                                 
         SH    RE,=H'4'                                                         
*                                                                               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  2(2,R1),DUB         FIELD LENGTH                                 
*                                                                               
         LA    R1,4(RE,R1)         NEXT BYTE AVAILABLE                          
*                                                                               
         MVI   0(R1),C'*'          END OF REQUEST                               
*                                                                               
*                                  ADD REQUEST                                  
         GOTO1 DATAMGR,DMCB,=C'DMADD ',=C'REQUEST',IO,IO                        
*                                                                               
LTRNX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
REQSEQTB DC    C'  ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                        
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
UPGD     DSECT                                                                  
CUPTYPE  DS    XL1                 UPGRADE TYPE                                 
*CUPTRTG EQU   2                   RATING UPGRADE                               
*CUPTHUT EQU   3                   HUT UPGRADE (CUPSTYP NE P)                   
*CUPTPUT EQU   3                   PUT UPGRADE (CUPSTYP EQ P)                   
*                                  CUPFLD1 = OLDHPT SOURCE BOOK                 
*CUPTNDX EQU   4                   INDEX UPGRADE                                
*CUPTHPT EQU   6                   H/P/T UPGRADE                                
CUPSTYP  DS    XL1                 UPGRADE SUB-TYPE (P=PUT UPGRADE)             
*                                  UGRADE BOOK/INDEX VALUES                     
CUPFLD1  DS    XL2                                                              
CUPFLD2  DS    XL2                                                              
CUPFLD3  DS    XL2                                                              
CUPFBK   DS    XL2                 FROM BOOK (SHARES)                           
CUPUDAY  DS    XL1                 DAY CODE                                     
CUPUTIM  DS    XL4                 START AND END TIMES (BINARY)                 
CUPSTA   DS    CL5                 STATION CALL LETTERS                         
         EJECT                                                                  
*************                                                                   
* DDSPOOLD  *                                                                   
*************                                                                   
         SPACE 1                                                                
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
**************                                                                  
* DDSPLWORKD *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
************                                                                    
* RERMPFFD *                                                                    
************                                                                    
         SPACE 1                                                                
       ++INCLUDE RERMPFFD                                                       
         SPACE 1                                                                
**********************************************                                  
* DDGENTWA - DSECT TO COVER GENCON TWA AREAS *                                  
**********************************************                                  
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
**************                                                                  
* RERMPWTWA  *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
**************                                                                  
* RERMPWORKD *  !!!!!!!!!!!!!!!!                                                
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE RERMPWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*=========================== RERMP SCREENS ===========================*         
                                                                                
* THESE ARE SCREENS IN THE $RMP PROGRAM WHICH THIS OVERLAY MAKES                
*  REFERENCES TO.                                                               
                                                                                
T810FFD  DSECT                                                                  
                                                                                
*------------------- INV/TRACKS (RERMPE8<==>T810E8) ------------------*         
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE RERMPE8D                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------- INV/ROVER (RERMPE7<==>T810E7) -------------------*         
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE RERMPE7D                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------- INV/DOVER (RERMPE6<==>T810E6) -------------------*         
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE RERMPE6D                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------- INV/DEMOS (RERMPBE<==>T810BE) -------------------*         
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE RERMPBED                                                       
         PRINT ON                                                               
***********************************************************************         
         EJECT                                                                  
****************************                                                    
* REGENINV (RINVD DSECT)   *                                                    
* REGENOVR                 *                                                    
* REGENALLA                *                                                    
* DEDEMFILE                *                                                    
* CTGENFILE                *                                                    
* DDCOMFACS                *                                                    
* FAFACTS                  *                                                    
* FATIOB                   *                                                    
* DDREPMASTD               *                                                    
* DDCOREQUS                *                                                    
* DEDBLOCK (DBLOCKD DSECT) *                                                    
* SPRANSIDD (SRBLKD DSECT) *                                                    
* REGENSET (MENU DSECT)    *                                                    
****************************                                                    
         SPACE 1                                                                
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENOVR                                                       
       ++INCLUDE REGENALLA                                                      
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE RERMPPROF                                                      
       ++INCLUDE DEDEMTABD                                                      
REMENUD  DSECT                                                                  
       ++INCLUDE REGENSET                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE FALOCKETD                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DMREQHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMREQHDR                                                       
         PRINT ON                                                               
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002RERMP00   07/21/10'                                      
         END                                                                    
