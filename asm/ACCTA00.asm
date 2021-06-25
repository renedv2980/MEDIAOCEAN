*          DATA SET ACCTA00    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T61E00A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T61E00 - CTA - PROGRAM CONTROLLER'                              
T61E00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T61E00,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,4(R1)            A(TWA)                                       
         USING T61EFFD,RA                                                       
         LR    R8,RC               SPOOL AREA                                   
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND         GEND                                         
         USING GEND,RC                                                          
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    R7,BASER7                                                        
         LR    R9,R1                                                            
         ST    R9,SYSPARMS                                                      
*                                                                               
         MVI   SPACES,C' '         INITIALIZE SPACES TO SPACES                  
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         MVC   ATIOB,0(R1)         A(TIOB)                                      
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE PROGRAM DEPENDENT VALUES          
         BAS   RE,CHKGLOB          CHECK IF CALLED WITH GLOBBER INFO            
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR SAVED MESSAGES                         
*                                                                               
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRANSTAT,RCHANG      THEN SET RCHANG FLAG                        
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRANSTAT,ACHANG      THEN SET ACHANG FLAG                        
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+18                                                             
         CLC   LP@CHA,CONACT       AND IT CHANGED TO 'CHA'                      
         BNE   *+8                                                              
         OI    TRANSTAT,USERCHA     THEN SET USER CAUSED 'CHA' FLAG             
*                                                                               
MAIN10   BAS   RE,GOGENCON         GO OFF TO GENCON                             
*                                                                               
         OI    CONRECH+4,X'20'     SET RECORD/ACTION FLDS VALID                 
         OI    CONACTH+4,X'20'                                                  
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        INITIALIZE PROGRAM DEPENDENT VALUES                         *          
**********************************************************************          
*                                                                               
SYSINIT  NTR1                                                                   
*                                                                               
         LR    R1,R9               SAVE A(DISPLAY BLOCK)                        
         AH    R1,=Y(DISPBLK-SYSD)                                              
         ST    R1,ADISPBLK                                                      
*                                                                               
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
         L     R1,8(R1)            A(COMFACS)                                   
*                                                                               
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
         MVI   SYSTEM,C'A'         ACCOUNT                                      
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VALUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=Y(L'ACTKEY)   KEY/STATUS AND DATADISP VALUES               
         MVC   LSTATUS,=Y(L'ACTRSTA)                                            
         MVC   DATADISP,=Y(L'ACTKEY+L'ACTRLEN+L'ACTRSTA+L'ACTRLNK)              
         MVC   SYSFIL,=C'ACCMST  '                                              
         MVC   SYSDIR,=C'ACCDIR  '                                              
         MVI   CURRSYS,C'A'        ACC SYSTEM                                   
         MVI   ACTELOPT,C'Y'       ADD ACTIVITY ELEMENT                         
         MVI   GETMSYS,6           USES GETMSG FOR SYSTEM 6                     
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'AC'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9061E00'    PRESET FOR SYSTEM CALLOVS               
         MVI   USEIO,C'N'                                                       
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
         OI    GENSTAT1,NOSETEFH+OKADDEL+USKYMRG+RDUPAPPL+APPLIC                
         OI    GENSTAT2,DISTHSPG                                                
         OI    GENSTAT3,OKVALSEL+RESTXE00+DIEONERR                              
         OI    GENSTAT4,NODELLST+CONFDEL+NODUPDIE+USEBIGKY                      
         CLI   TWAFIRST,0                                                       
         BNE   *+8                                                              
         OI    GENSTAT4,SVTWA0FF                                                
*                                                                               
         MVC   LSVTWA0,=AL2(MAXLTWA0)  L'STORAGE TO SAVE IN TWA0                
         MVI   NTWA,0                  DON'T SAVE ANY EXTRA PAGES               
         MVI   LRECACT,L'RECACT        SET L'RECACT TABLE ENTRY                 
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   DON'T BOTHER IF OFFLINE                      
         BNZ   SYS100                                                           
*                                                                               
         L     R1,ATIOB            A(TIOB)                                      
         USING TIOBD,R1                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID          PICK UP PFKEY VALUE                          
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
SYS100   B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
*        CHECK IF CALLED WITH GLOBBER TRANSFER ELEM                  *          
**********************************************************************          
*                                                                               
CHKGLOB  NTR1                                                                   
         NI    PRGSTAT,X'FF'-(GLOBCALL+GLOBELEM)                                
         XC    GLOBFRSY,GLOBFRSY                                                
         XC    GLOBFRPR,GLOBFRPR                                                
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    XNO                                                              
         ST    RF,VGLOBBER                                                      
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   XNO                                                              
         USING GLVXFRSY,R2                                                      
         LA    R2,ELEM                                                          
         CLC   GLVXFRSY,=C'ACC'                                                 
         BE    XNO                                                              
         MVC   GLOBFRSY,GLVXFRSY   SAVE FROM SYSTEM                             
         MVC   GLOBFRPR,GLVXFRPR   SAVE FROM PROGRAM                            
         OI    PRGSTAT,GLOBCALL                                                 
         GOTO1 (RF),(R1),=C'DELE'   GET RID OF QUICKLY!                         
         DROP  R2                                                               
*                                                                               
         GOTO1 (RF),(R1),=C'GETF',CONRECH,3,GLVXREC                             
         CLI   8(R1),0                                                          
         BNE   XNO                                                              
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         GOTO1 (RF),(R1),=C'GETF',CONACTH,3,GLVXACT                             
         CLI   8(R1),0                                                          
         BNE   XNO                                                              
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         MVI   CONKEY,C'*'         FAKE INPUT FOR GENCON                        
         MVI   CONKEYH+5,1                                                      
*                                                                               
CGX      B     XYES                                                             
         EJECT                                                                  
**********************************************************************          
*        ROUTINE HANDLES PASSING OF CONTROL TO AND FROM GENCON       *          
**********************************************************************          
*                                                                               
GOGENCON NTR1                                                                   
         BAS   RE,SETRD            SET RD SO GENCON ALWAYS RETURNS              
*                                                                               
GOG10    MVI   GOAGAIN,C'N'        INITIALIZE RETURN SWITCH                     
         OI    TRANSTAT,FRSTMODE    ALLOWS APPL TO DETECT FIRST MODE            
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
GOGX     B     XIT                 ALL THROUGH                                  
*                                                                               
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO WE GET CONTROL BACK                
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY              *          
**********************************************************************          
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VVALUSER                                                         
         B     VINITIAL                                                         
         B     VGETTWA                                                          
         B     VGETLDG             GET LEDGER LEVELS                            
         B     VGTLEVNM            GET LEVEL NAMES                              
         B     VGETNME             GET NAME FROM REC                            
         B     VACCSYS             SWITCH TO ACC SYSTEM                         
         B     VSPTSYS             SWITCH TO SPOT SYSTEM                        
         B     VVALSMED            VALIDATE SPOT MEDIA                          
         B     VVALCON#            VALIDATE/GET SPOT CONTRACT                   
         B     VVSPCNTR            VALIDATE/GET SPOT CONTRACTOR                 
         B     VVALCATG            VALIDATE CATEGORY                            
         B     VVALSUPP            VALIDATE SUPPLIER                            
         B     VGETPO#             GET NEXT ORDER # FROM CONTROL REC            
         B     VUPDPO#             UPDATE ORDER CONTROL REC NUMBER              
         B     VMYERR                                                           
*                                                                               
BASER7   DC    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
*        LTORG                                                       *          
**********************************************************************          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        THIS ROUTINE GETS CALLED BY GENCON ON EVERY TRANSACTION     *          
*        BEFORE CALLING THE APPLICATION                              *          
**********************************************************************          
*                                                                               
VVALUSER DS    0H                                                               
         CLI   MODE,NEWSCR                                                      
         BNE   VUSR00                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
VUSR00   CLI   OFFLINE,C'Y'        ALWAYS IF OFFLINE                            
         BE    VUSR10                                                           
         CLI   TWAFIRST,0          ELSE IF FIRST TIME IN                        
         BNE   VUSR30                                                           
VUSR10   BAS   RE,PRGINT           INITIALIZE PROGRAM                           
         MVI   TWAFIRST,1          ELSE SET NO LONGER FIRST TIME                
*                                                                               
VUSR30   MVC   USERNAME,USRNME    GET USER ID INFO FROM SAVED STORAGE           
         MVC   USERADDR,USRADD                                                  
*                                                                               
         MVC   RECNUM,TWALREC     MOVE SAVED REC/ACT TO CURRENT IN CASE         
         MVC   ACTNUM,TWALACT     WE TAKE ERROR BEFORE GENCON SETS THEM         
*                                                                               
         CLI   CONACT,C'L'         IF ACTION IS NOT LIST                        
         BE    VUSR40                                                           
         CLI   CONACT,C'R'         OR REPORT                                    
         BE    VUSR40                                                           
         CLI   CONWHENH+5,0        AND SOMETHING'S IN PRINT FIELD               
         BE    VUSR40                                                           
         XC    CONWHEN,CONWHEN     THEN CLEAR IT                                
         MVI   CONWHENH+5,0                                                     
         OI    CONWHENH+6,X'80'                                                 
*                                                                               
VUSR40   TM    TRANSTAT,RACHANG    IF RECORD OR ACTION FLD HAS CHANGED          
         BZ    VUSR50                                                           
         MVI   CALLSP,0            CLEAR STACK                                  
*                                                                               
VUSR50   TM    TRANSTAT,RCHANG     IF RECORD CHANGE                             
         BZ    VUSR60                                                           
         MVC   CONOPT,SPACES       CLEAR OPTIONS                                
         MVI   CONOPTH+5,0                                                      
         OI    CONOPTH+6,X'80'                                                  
*                                                                               
VUSR60   TM    TRANSTAT,RACHANG+USERCHA IF USER HASN'T CHANGED REC/ACT          
         BNZ   VUSR70                                                           
         OC    SVACTION,SVACTION   AND SAVED ACTION EXISTS                      
         BZ    VUSR70                                                           
         MVC   CONACT,SVACTION     THEN RESTORE IT                              
*                                                                               
VUSR70   CLI   TWASCR,X'00'        ARE WE ON THE FF SCREEN                      
         BNE   VUSRX                                                            
         LA    R2,PFTABLE                                                       
         GOTO1 INITIAL,DMCB,(X'40',(R2)),CONPFKYH                               
         CLI   CONRECH+5,0                                                      
         BE    PLSENTER            PLEASE ENTER FIELDS ...                      
VUSRX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        PROGRAM INITIALIZATION - PROCESSED ONE TIME ONLY            *          
**********************************************************************          
*                                                                               
PRGINT   NTR1                                                                   
         L     R1,SYSPARMS         COMPANY CODE                                 
         MVC   CMPY,0(R1)                                                       
*                                                                               
         USING CCTIKEY,R3                                                       
         XC    BIGKEY,BIGKEY       GET AGENCY NAME & ADDR FROM ID REC.          
         LA    R3,BIGKEY                                                        
         MVI   CCTIKTYP,C'I'                                                    
         MVC   CCTIKID+8(2),TWAORIG                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',BIGKEY,AIO,0                  
*                                                                               
         SR    R0,R0                                                            
         L     R3,AIO                                                           
         LA    R3,CCTIDATA                                                      
*                                                                               
PRG10    CLI   0(R3),X'06'         AGENCY ALPHA ELEM                            
         BE    PRG30                                                            
         CLI   0(R3),X'30'         AGENCY NAME AND ADDRESS                      
         BE    PRG40                                                            
         CLI   0(R3),0                                                          
         BE    PRG50                                                            
PRG20    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PRG10                                                            
*                                                                               
         USING CCTAGYD,R3                                                       
PRG30    MVC   ALPHA,CCTAGYID      AGENCY ALPHA                                 
         B     PRG20                                                            
*                                                                               
         USING CCTDSTD,R3                                                       
PRG40    MVC   USRNME,CCTDSTNAM    SAVE AGENCY NAME AND ADDRESS                 
         MVC   USRADD,CCTDSTADD    AND ADDRESS                                  
         B     PRG20                                                            
*                                                                               
         USING CCT5REC,R3                                                       
PRG50    XC    BIGKEY,BIGKEY       GET SYSTEM SE NUMBERS                        
         LA    R3,BIGKEY                                                        
         MVI   CCT5KTYP,CCT5KTYPQ  C'5'                                         
         MVC   CCT5KALPH,ALPHA                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',BIGKEY,AIO,0                  
*                                                                               
         SR    R0,R0                                                            
         L     R3,AIO                                                           
         LA    R3,CCT5DATA                                                      
         XC    SELIST,SELIST                                                    
*                                                                               
PRG60    CLI   0(R3),X'21'         SYSTEM AUTH ELEM                             
         BE    PRG70                                                            
         CLI   0(R3),0                                                          
         BE    PRG100                                                           
PRG60NX  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PRG60                                                            
*                                                                               
         USING CCTSYSD,R3                                                       
PRG70    CLI   CCTSYSNUM,X'02'      SPOT SYSTEM                                 
         BNE   *+14                                                             
         MVC   SPOTSE,CCTSYSSE      SPOT SE NUMBER                              
         B     PRG60NX                                                          
         CLI   CCTSYSNUM,X'03'      NET SYSTEM                                  
         BNE   *+14                                                             
         MVC   NETSE,CCTSYSSE                                                   
         B     PRG60NX                                                          
         CLI   CCTSYSNUM,X'04'      PRINT SYSTEM                                
         BNE   *+14                                                             
         MVC   PRNTSE,CCTSYSSE                                                  
         B     PRG60NX                                                          
         CLI   CCTSYSNUM,X'06'      ACC SYSTEM                                  
         BNE   PRG60NX                                                          
         MVC   ACCSE,CCTSYSSE                                                   
         B     PRG60NX                                                          
         DROP  R3                                                               
*                                                                               
PRG100   GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         CLI   FASYSID,1           CHECK FOR TEST SYSTEM                        
         BNE   *+8                                                              
         OI    PRGSTAT,TESTSYS                                                  
         MVC   LINEID(8),FALINE    SAVE LINE ID                                 
         DROP  R3                                                               
*                                                                               
         USING CPYRECD,R4          READ ACC COMPANY REC                         
         LA    R4,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   CPYKCPY,CMPY                                                     
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'CPYKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         USING CPYELD,R4                                                        
         L     R4,AIO                                                           
         MVI   ELCODE,CPYELQ       X'10' COMPANY ELEM                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SUPPUL,CPYSUPP      SUPPLIER UNIT/LEDGER                         
         DROP  R4                                                               
*                                                                               
         LA    R1,SECBLK           SET ADDRESS OF SECRET BLOCK                  
         ST    R1,ASECBLK                                                       
         XC    DMCB(24),DMCB       INIT SECRET                                  
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK)                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         XC    TSARBLK,TSARBLK                                                  
*                                                                               
         MVC   AMASTD,TWAMASTC                                                  
         USING MASTD,R1                                                         
         L     R1,AMASTD           A(MASTER)                                    
         MVC   VUTL,MCUTL          A(UTL)                                       
         MVC   VREMOT,MCVREMOT                                                  
         DROP  R1                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        OVERLAY INITIALIZATION                                      *          
*        P1=A(PFKEY VAL. TABLE) OR ZEROS                             *          
*        P2=A(PFKEY LINE) OR ZEROS                                   *          
**********************************************************************          
*                                                                               
VINITIAL DS    0H                                                               
         MVC   INITBYTE,0(R1)      SAVE STATUS BYTE                             
         SR    R4,R4                                                            
         ICM   R4,7,1(R1)          IF PFKEY VALIDATION TABLE PASSED             
*                                                                               
         ICM   R3,15,4(R1)         IF PFKEY LINE PASSED - VALIDATE LIST         
         BZ    INIT05                                                           
         GOTO1 VPFLINE,DMCB,(R3)   HANDLE LOCAL PFKEY PRESENCE                  
*                                                                               
INIT05   LTR   R3,R4               IF PFKEY VALIDATION TABLE PASSED             
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
         TM    INITBYTE,X'80'      FORCE TO APPL. CLEAR STORAGE                 
         BO    INIT20                                                           
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
INIT20   TM    INITBYTE,X'40'      FORCE NOT TO CLEAR STORAGE                   
         BO    INIT30                                                           
         LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
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
INITX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                      *          
*        P1  BYTES 1-3 = A(PFKEY VAL. TABLE)                         *          
**********************************************************************          
*                                                                               
PFVAL    NTR1                                                                   
         CLI   PFKEY,0             USER HIT ENTER?                              
         BE    XNO                 YES                                          
*                                                                               
         L     R2,0(R1)            R2=A(PFKEY TABLE)                            
         USING PFTABD,R2           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(R2),X'FF'                                                      
         BE    PFERR                                                            
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    R2,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    XNO                                                              
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     XYES                IF RETURNS, RETURN CC EQUAL                  
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO PROCESS PFKEY REQUEST                            *          
*        INPUT                     R2=A(PFTABLE ENTRY)               *          
**********************************************************************          
*                                                                               
         USING PFTABD,R2           R2=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         MVI   PFKEY,0             CLEAR PFKEY FOR NEXT SCREEN                  
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
         TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    PFI5                                                             
         TM    PRGSTAT,GLOBCALL    CALLED WITH GLOBBER                          
         BZ    PFI4                                                             
         CLI   RECNUM,RTCTA        IF ON CTAGS RETURN TO SPOT                   
         BNE   PFI4                                                             
         USING GLVXFRSY,R1                                                      
         LA    R1,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   GLVXFRSY,=C'ACC'                                                 
         MVC   GLVXFRPR,=C'CTA'                                                 
         MVC   GLVXTOSY,GLOBFRSY                                                
         MVC   GLVXTOPR,GLOBFRPR                                                
         OI    GLVXFLG1,GLV1RETN            RETURN CALL                         
         OI    GLVXFLG1,GLV1SEPS            SEPARATE SESSION??                  
         DROP  R1                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,14,GLVXCTL                           
         B     DUMYERR1                                                         
*                                                                               
PFI4     BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
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
         BE    PFI10                                                            
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
*                                                                               
PFI10    CLI   CONWHENH+5,0        DON'T BOTHER IF INPUT PRESENT                
         BNE   PFIX                                                             
         TM    PFTSTAT2,PFTSETPN   TEST PRESET OF PRINT FIELD TO NOW            
         BZ    PFIX                                                             
         MVC   CONWHEN(7),=C'NOW,CTA'                                           
         MVI   CONWHENH+5,7                                                     
         OI    CONWHENH+6,X'80'    TRANSMIT NEW PRINT FIELD                     
*                                                                               
PFIX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS            *          
*        INPUT                     R3=A(PFKEY TABLE)                 *          
**********************************************************************          
*                                                                               
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
         CLI   TWASCR,X'00'        IF ON FF SCREEN                              
         BE    TSELX               THEN NO LIST                                 
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
         TM    1(R2),X'20'         SKIP PROTECTED                               
         BO    TSEL6                                                            
*                                                                               
         XC    WORK,WORK           MOVE FIELD TO WORK                           
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+4                                                           
         MVC   WORK(0),8(R2)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    WORK(0),SPACES                                                   
*                                                                               
         USING PFTABD,R5           R3 =A(START OF TABLE)                        
         LR    R5,R3               RESET TO START OF TABLE                      
TSEL4    CLI   0(R5),X'FF'                                                      
         BE    TSEL6                                                            
         CLI   PFTSEL,C' '                                                      
         BNL   TSEL5                                                            
         CLC   PFTSEL,WORK         MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
TSEL5    ZIC   R1,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    R5,R1                                                            
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
         LR    R6,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            ANYTING TO SELECT                            
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BNH   TSEL6                                                            
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    R6,RA                                                            
         STH   R6,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR    *          
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN        *          
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH  *          
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO *          
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE *          
* WORKING ON. WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL*          
* BE CALLED TO RESTORE THE SCREEN.                                   *          
**********************************************************************          
*                                                                               
CPROG    NTR1                                                                   
         SR    R3,R3               ALWAYS USE SAME SPOT FOR SAVED SCN           
         LA    RF,CALLSTCK(R3)                                                  
         MVC   0(1,RF),TWASCR                                                   
*                                                                               
         MVI   CALLSP,1                  ALWAYS SET TO 1  AND                   
*        NI    PRGSTAT,X'FF'-GLOBCALL    CLEAR CALLED WITH GLOBBER              
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
***********************************************************************         
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON   *         
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG*         
***********************************************************************         
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
         LA    R2,CONHEADH         TRANSMIT SCREEN                              
RP10     CLI   0(R2),0                                                          
         BE    RP20                FIND END OF TWA                              
         OI    6(R2),X'80'                                                      
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     RP10                                                             
*                                                                               
RP20     MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
         OI    TRANSTAT,RETURNED   SET THAT RETPROG HAS BEEN CALLED             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD               *         
***********************************************************************         
*                                                                               
         USING PFTABD,R2           R2=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R5,WORK             R5=A(WORK)                                   
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
         MVC   0(0,R5),0(RF)       MOVE TO WORK                                 
         AR    R5,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R5),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R5,0                                                             
         BCT   RE,*-10                                                          
         LA    R5,1(R5)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CH    R3,=H'1'            TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    MVI   0(R5),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R5,1(R5)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R5,R3               R5=L'TMPKEY FIELD                            
         CLM   R5,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R5,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     XIT                                                              
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
         DROP  R4,R2                                                            
         EJECT                                                                  
***********************************************************************         
*        BUMP TO FIRST FIELD IN ROW                                   *         
***********************************************************************         
*                                                                               
BMPTOROW NTR1                                                                   
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
         B     XNO                 RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    ZIC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO READ/WRITE TEMPSTR PAGES                          *         
*        INPUT                     P1, BYTE  0=BIT SETTING/PAGE NUMBER*         
*        INPUT                     P1, BYTES 1-3=READ/WRITE ADDRESS   *         
***********************************************************************         
*                                                                               
VGETTWA  DS    0H                                                               
         MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA2                                                            
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA2                                                            
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA2    NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
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
*        BUILD PFKEY LIST WITH VALID KEYS FROM TABLE                  *         
***********************************************************************         
*                                                                               
VPFLINE  NTR1                                                                   
         L     R4,0(R1)            R4= A(PFKEY  FIELD)                          
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R0,0(R4)            LENGTH OF FIELD                              
         SH    R0,=H'8'            SUBTRACT FIELD HEADER R0=LEN                 
         TM    1(R4),X'02'                                                      
         BZ    *+8                                                              
         SH    R0,=H'8'                                                         
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),SPACES      CLEAR                                        
         OI    6(R4),X'80'         XMIT                                         
*                                                                               
         LA    R3,SCRTABL          TABLE OF SCREENS                             
VPF10    CLC   =X'FFFF',0(R3)      EOT                                          
         BE    VPFX                                                             
         CLC   TWASCR,0(R3)        MATCH ON SCREEN                              
         BE    VPF20                                                            
         LA    R3,5(R3)                                                         
         B     VPF10                                                            
*                                                                               
         USING PFLISTD,R5                                                       
VPF20    ICM   R5,15,1(R3)         R5=ADDRESS OF PFKEYLINE TABLE                
         A     R5,RELO                                                          
         LA    R4,8(R4)            R4=WHERE TO START LINE                       
         LR    R3,R4                                                            
         AR    R3,R0               R3=WHERE TO STOP LINE                        
         B     VPF25                                                            
*                                                                               
VPF20NX  LA    R5,PFLLENQ(R5)      NEXT LINE IN TABLE                           
VPF25    CLI   PFLREC,X'FF'        EOT                                          
         BE    VPFX                                                             
*                                                                               
         TM    PFLSTAT,PFLSSEC     CHECK FOR SECURITY ON REC/ACT?               
         BNO   VPF30                                                            
         OC    ASECBLK,ASECBLK                                                  
         BZ    VPF30                                                            
         LA    R2,PFLACT                                                        
         ICM   R2,8,PFLREC         R3=(RECORD, A(ACTION))                       
         GOTO1 SECRET,DMCB,('SECPRACT',ASECBLK),(R2)                            
         BNE   VPF20NX             DON'T HAVE THE SECURITY                      
*                                                                               
VPF30    TM    PFLSTAT,PFLSRET     VALIDATE KEY DOESN'T = RETURN KEY            
         BNO   VPF40                                                            
         CLI   CALLSP,0            ANYTHING IN STACK                            
         BE    VPF40                                                            
         CLC   PFLSCR,CALLSTCK     COMPARE SCREEN TO STACKED SCREEN             
         BE    VPF20NX             SKIP IF =                                    
*                                                                               
VPF40    TM    PFLSTAT,PFLSNRET    IS RETURN VALID (ANY STACK?)                 
         BNO   VPF50                                                            
         CLI   CALLSP,0            ANYTHING IN STACK                            
         BNE   VPF50               YES                                          
         TM    PRGSTAT,GLOBCALL    OR GLOBBER SWITCH                            
         BNO   VPF20NX             SKIP IF NOT                                  
*                                                                               
VPF50    TM    PFLSTAT,PFLSNXTS    ANY PENDING SELECTS                          
         BNO   VPF60                                                            
         CLI   ACTEQU,ACTSEL                                                    
         BNE   VPF20NX                                                          
         LA    R1,LISTDIR                                                       
         ZIC   R0,LISTNUM                                                       
VPF55    CLI   0(R1),0                                                          
         BH    VPF100              THERE'S MORE DISP ENTER=NEXT KEY             
         LA    R1,6(R1)                                                         
         BCT   R0,VPF55                                                         
         B     VPF20NX             NOTHING MORE                                 
*                                                                               
VPF60    TM    PFLSTAT,PFLSDDS     DISPLAY FOR DDS TERMINALS                    
         BNO   VPF100                                                           
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   VPF20NX             SKIP IF NOT                                  
*                                                                               
VPF100   DS    0H                  WILL IT FIT                                  
         ZIC   R1,PFLPF#LN         LEN OF "PF#="                                
         ZIC   R0,PFLDLEN          LEN OF PFKEY DESCRIPTION                     
         AR    R1,R0                                                            
         AR    R1,R4               CURRENT POSITION                             
         CR    R3,R1                                                            
         BL    VPFX                NO MORE ROOM                                 
*                                                                               
         ZIC   R1,PFLPF#LN         LEN OF "PF#="                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PFLPF#                                                   
         LA    R4,1(R1,R4)         BUMP                                         
*                                                                               
         ZIC   R1,PFLDLEN          DESC LEN                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PFLDESC                                                  
         LA    R4,2(R1,R4)         1 FOR EX + 1 FOR SPACE                       
         B     VPF20NX                                                          
*                                                                               
VPFX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        GET LEDGER LEVEL LENGTHS AND NAMES                           *         
* INPUT        R1 = A(TWO BYTE UNIT LEDGER VARIABLE)                  *         
* OUTPUT       LDGLN1, LDGNM1, ETC. HAVE LEDGER INFO AS IN ELEMENT    *         
*              ABCDLEN HAS THE INDIVIDUAL LENGTHS OF EACH LEVEL       *         
*              CC = NO IF LEDGER NOT FOUND                            *         
***********************************************************************         
*                                                                               
VGETLDG  DS    0H                                                               
         CLC   LDGC,0(R1)          DO WE ALREADY HAVE LEDGER                    
         BE    GETLEDGX            YES, SO EXIT                                 
         XC    LDGLEL,LDGLEL       CLEAR LEDGER RECORD INFO                     
         LA    R2,BIGKEY                                                        
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CMPY        COMPANY                                      
         MVC   LDGKUNT(2),0(R1)    UNIT/LEDGER                                  
         GOTO1 HIGH                                                             
         CLC   LDGKEY,KEYSAVE                                                   
         BNE   GETLEDGN            NO LEDGER FOUND                              
         MVC   LDGLEL,LDGKUNT      SAVE LEDGER CODE                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,ACLELQ       GET LENGTHS ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO LENGTHS ELEMENT                           
         USING ACLELD,R4                                                        
         MVC   LDGLEL,ACLEL        SAVE THE ELEMENT                             
*                                                                               
         LR    R6,R4                                                            
         ZIC   R1,1(R6)            TOTAL EL LENGTH                              
         SH    R1,=Y(ACLLN1Q)      - CODE AND LENGTH                            
         SR    R0,R0                                                            
         SR    R3,R3                                                            
         LA    R3,L'ACLVALS(R3)    LENGTH OF EACH ACCT DESC AND LENGTH          
         DR    R0,R3                                                            
         STC   R1,NUMLEVS          SAVE NUMBER OF ACCOUNT LEVELS                
*                                                                               
         ZIC   R5,NUMLEVS                                                       
         AH    R6,=Y(ACLLN1Q)      BUMP TO FIRST LENGTH                         
         LA    R3,ABCDLEN          FIELD TO CONTAIN ALL 4 LENGTHS               
         XC    ABCDLEN,ABCDLEN                                                  
         SR    R1,R1               ACUMULATES HOW MUCH TO SUBTRACT              
         ZIC   R2,0(R6)                                                         
LENLP    STC   R2,0(R3)                                                         
         AR    R1,R2               ACCUMULATE LENGTHS                           
         LA    R3,1(R3)                                                         
         LA    R6,L'ACLVALS(R6)    NEXT LENGTH                                  
         ZIC   R2,0(R6)                                                         
         SR    R2,R1                                                            
         BCT   R5,LENLP                                                         
*                                                                               
         B     GETLEDGX                                                         
*                                                                               
GETLEDGN MVC   GERROR,=AL2(ACELEDG)           INVALID LEDGER                    
GETLEDGX B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*                GET LEVEL NAME OF ACCOUNT RECORD                     *         
* INPUT        P1 = A(TWO BYTE UNIT/LEDGER VARIABLE)                  *         
*              P2 = A(12 BYTE ACCOUNT FOR DESIRED LEVEL)              *         
*              P3 = A(SCREEN HEADER TO PUT NAME) - OPTIONAL           *         
* OUTPUT       WORK WILL HAVE THE NAME OF THE LEVEL                   *         
*              CC = NO IF LEDGER NOT FOUND                            *         
***********************************************************************         
*                                                                               
VGTLEVNM DS    0H                                                               
         L     R5,8(R1)            SAVE SCREEN HEADER                           
         L     R4,4(R1)            ACCOUNT                                      
         LA    R2,BIGKEY                                                        
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),0(R1)    UNIT/LEDGER                                  
         MVC   ACTKACT,0(R4)       ACCOUNT                                      
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   GTLEVNMN            NO LEDGER FOUND                              
         GOTO1 GETREC                                                           
         GOTO1 GETNME,DMCB,AIO,(R5)                                             
         B     GTLEVNMX                                                         
*                                                                               
GTLEVNMN MVC   GERROR,=AL2(ACEACCT)           INVALID account                   
         B     XNO                                                              
GTLEVNMX B     XYES                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        GET NAME  FROM X'20' ELEM IN RECORD                          *         
*        INPUT                  P1 = A(RECORD TO GET NAME FROM)       *         
*                               P2 = A(SCREEN FIELD HEADER) - OPTIONAL*         
***********************************************************************         
*                                                                               
VGETNME  LM    R4,R5,0(R1)         LOAD RECORD & SCREEN ADDRESS                 
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,NAMELQ       SET ELEMENT CODE                             
         BAS   RE,GETEL                                                         
         BNE   XNO                 NOT FOUND                                    
         USING NAMELD,R4                                                        
         SR    R1,R1               MOVE NAME TO WORK                            
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q)      MINUS ELCODE AND LENGTH                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),NAMEREC                                                  
*                                                                               
         LTR   R5,R5               TEST FIELD HEADER                            
         BZ    XIT                 NONE, OK TO EXIT                             
         ZIC   R3,0(R5)            LENGTH OF HEADER + DATA                      
         SH    R3,=H'9'            FIELD HEADER LENGTH W/O EXTENSION            
         TM    1(R5),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SH    R3,=H'8'            FIELD HEADER LENGTH WITH EXTENSION           
         OI    6(R5),X'80'         TRANSMIT FIELD                               
         LA    R5,8(R5)            R5 TO DATA FIELD                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPACES                                                   
*                                                                               
         CR    R3,R1               GET SHORTEST IN R3                           
         BNH   *+6                                                              
         LR    R3,R1                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK        NAME TO SCREEN FIELD                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        SWITCH TO ACC SYSTEM                                                   
***********************************************************************         
*                                                                               
VACCSYS  DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VAC10                                                            
         L     R1,VUTL                                                          
         MVC   4(1,R1),ACCSE                                                    
         B     VAC20                                                            
*                                                                               
VAC10    GOTO1 SWITCH,DMCB,=C'ACC',0                                            
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    XNO                                                              
         CLI   4(R1),1                                                          
         BE    XNO                                                              
VAC20    MVI   CURRSYS,C'A'                                                     
         MVC   LKEY,=Y(L'ACTKEY)   KEY/STATUS AND DATADISP VALUES               
         MVC   LSTATUS,=Y(L'ACTRSTA)                                            
         MVC   DATADISP,=Y(L'ACTKEY+L'ACTRLEN+L'ACTRSTA+L'ACTRLNK)              
         MVC   SYSFIL,=C'ACCMST  '                                              
         MVC   SYSDIR,=C'ACCDIR  '                                              
         B     XYES                                                             
***********************************************************************         
*        SWITCH TO SPOT SYSTEM                                                  
***********************************************************************         
*                                                                               
VSPTSYS  DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VSP10                                                            
         L     R1,VUTL                                                          
         MVC   4(1,R1),SPOTSE                                                   
         LA    R2,SPFLIST                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'SPOT',(R2),IO,0                   
         B     VSP20                                                            
*                                                                               
VSP10    GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    XNO                                                              
         CLI   4(R1),1                                                          
         BE    XNO                                                              
VSP20    MVI   CURRSYS,C'S'                                                     
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SPOT MEDIA                                                    
*        INPUT                     QMED                                         
*        OUTPUT                    SBAGYMD,SMEDNM                               
***********************************************************************         
*                                                                               
VVALSMED DS    0H                                                               
         CLI   CURRSYS,C'S'        MUST BE SWITCHED TO SPOT                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SBAGYMD,0                                                        
         MVC   SMEDNM,SPACES                                                    
*                                                                               
         USING AGYKEY,R6                                                        
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       GET AGENCY RECORD                            
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                SPOT READ                                    
         CLC   BIGKEY(L'AGYKEY),KEYSAVE                                         
         BNE   XNO                                                              
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
VSMED10  BAS   RE,NEXTEL                                                        
         BNE   XNO                                                              
         CLC   2(1,R4),QMED                                                     
         BNE   VSMED10                                                          
         MVC   SBAGYMD,3(R4)       SPOT AGENCY/MEDIA                            
         MVC   SMEDNM,4(R4)        SPOT MEDIA NAME                              
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SPOT CONTRACT NUMBER AND GET CONTRACT RECORD                  
*        INPUT                     R2=FIELD HEADER                              
*        OUTPUT                    CON# (CHAR)                                  
*                                  PCON# (PACKED 9'S COMP), REC IN AIO          
***********************************************************************         
*                                                                               
VVALCON# DS    0H                                                               
         CLI   CURRSYS,C'S'        MUST BE SWITCHED TO SPOT                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    CON#,CON#                                                        
         XC    PCON#,PCON#                                                      
         ZAP   GCIAMT,=P'0'                                                     
         ZAP   NCIAMT,=P'0'                                                     
         ZAP   NCIPCT,=P'10000'                                                 
         ZAP   MGBUY,=P'0'                                                      
         ZAP   MGPAID,=P'0'                                                     
         MVI   CONSTAT,0                                                        
*                                                                               
         ZICM  R1,5(R2),1          ANYTHING ENTERED                             
         BZ    XNO                                                              
         TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   XNO                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(4),8(0,R2)                                                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK(4)                                                
         SRP   WORK+4(4),1,0                                                    
*                                                                               
         MVC   PCON#,WORK+4        PACKED 9'S COMPLEMENT                        
         MVC   CON#,8(R2)                                                       
*                                                                               
         USING CTARECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       GET SPOT CONTRACT RECORD                     
         MVI   CTAKTYP,CTAKTYPQ    X'0D'                                        
         MVI   CTAKSUB,CTAKSUBQ    X'7E'                                        
         MVC   CTAKAGMD,SBAGYMD    AGENCY MEDIA                                 
         MVC   CTAKCNUM,PCON#                                                   
         GOTO1 HIGH                SPOT READ                                    
         CLC   BIGKEY(L'CTAKEY),KEYSAVE                                         
         BNE   XNO                                                              
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         TM    CTARCNTL,CTARCLOC   CONTRACT LOCKED                              
         BZ    *+8                                                              
         OI    CONSTAT,CSLOCK                                                   
         TM    CTARCNTL,CTARCCLS   CONTRACT CLOSED                              
         BZ    *+8                                                              
         OI    CONSTAT,CSCLOSE                                                  
*                                                                               
         USING CTAEL,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,X'01'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                 REQ'D                                        
         DC    H'0'                                                             
         MVC   CONTRCTR,CTDSCNTR   CONTRACTOR                                   
*                                                                               
         MVC   MEDGCI,CTDSCGCI     SAVE ORIGINAL MEDIA GCI                      
         ZICM  R1,CTDSCGCI,4                                                    
         CVD   R1,DUB                                                           
         ZAP   GCIAMT,DUB(8)       PACKED GCI AMOUNT                            
*                                                                               
         ZICM  R1,CTDSCNCI,4                                                    
         CVD   R1,DUB                                                           
         ZAP   NCIAMT,DUB(8)       PACKED NCI AMOUNT                            
*                                                                               
         ZICM  R1,CTDSCCOM,4                                                    
         BZ    *+14                                                             
         CVD   R1,DUB                                                           
         ZAP   NCIPCT,DUB(8)       PACKED COMMISSION PCT IF NOT 85%             
*                                                                               
         OC    CTDSCCOM,CTDSCCOM   COMMISSIONABLE                               
         BZ    *+8                                                              
         OI    CONSTAT,CSCOMM                                                   
         TM    CTDSCTYP,X'80'      DOLLARS ARE ALLOCATED TO STATIONS            
         BNO   *+8                 = CHECK IF OUT OF BALANCE ON RETURN          
         OI    CONSTAT,CSDOLL                                                   
         TM    CTDSCTYP,X'04'      PERCENTS ARE ALLOCATED TO STATIONS           
         BNO   *+8                 = RECALC DOLLARS ON SPOT REC                 
         OI    CONSTAT,CSPCT                                                    
*                                                                               
         USING CTAUSELD,R4                                                      
         L     R4,AIO                                                           
         MVI   ELCODE,CTAUSELQ     X'06' USAGE ELEMENT                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VCON20   BAS   RE,NEXTEL                                                        
         BNE   VCON30                                                           
         ZICM  R1,CTAUSOGR,4                                                    
         CVD   R1,DUB                                                           
         AP    MGBUY,DUB(8)        MEDIA GROSS BUY TOTAL                        
         ZICM  R1,CTAUSPGR,4                                                    
         CVD   R1,DUB                                                           
         AP    MGPAID,DUB(8)       MEDIA GROSS PAID TOTAL                       
         B     VCON20                                                           
*                                                                               
         USING CTAXFELD,R4                                                      
VCON30   L     R4,AIO                                                           
         MVI   ELCODE,CTAXFELQ     X'07' TRANSFER ELEM                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VCON35   BAS   RE,NEXTEL                                                        
         BNE   VCONX                                                            
         ZICM  R1,CTAXFOGR,4                                                    
         CVD   R1,DUB                                                           
         TM    CTAXSTAT,CTAXSTPD   DOLLARS ARE PAID                             
         BO    *+10                THEN ADD TO PAID AND BOUGHT                  
         MP    DUB(8),=P'-1'       ELSE MINUS                                   
         AP    MGBUY,DUB(8)        MEDIA GROSS BUY TOTAL                        
         AP    MGPAID,DUB(8)       MEDIA GROSS PAID TOTAL                       
         B     VCON35                                                           
*                                                                               
VCONX    B     XYES                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SPOT CONTRACTOR                                               
*        INPUT                     R2=FIELD HEADER                              
*        OUTPUT                    REC IN AIO                                   
*                                  CONTNAME=NAME                                
***********************************************************************         
*                                                                               
VVSPCNTR DS    0H                                                               
         CLI   CURRSYS,C'S'        MUST BE SWITCHED TO SPOT                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CNTRNAME,SPACES     CONTRACTOR NAME                              
         MVC   CONTNAME,SPACES     CONTRACT NAME                                
         MVC   CNTRADR,SPACES      CONTRACTOR ADDRESS                           
*                                                                               
         USING CXRRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       GET SPOT CONTRACTOR REC                      
         MVI   CXRKTYP,CXRKTYPQ    X'0D'                                        
         MVI   CXRKSUB,CXRKSUBQ    X'7D'                                        
         MVC   CXRKAGMD,SBAGYMD    AGENCY MEDIA                                 
         MVC   CXRKCTA,CONTRCTR                                                 
         GOTO1 HIGH                SPOT READ                                    
         CLC   BIGKEY(L'CXRKEY),KEYSAVE                                         
         BNE   XNO                                                              
         GOTO1 GETREC                                                           
*                                                                               
         USING CXRCTEL,R4                                                       
         L     R4,AIO                                                           
         MVI   ELCODE,CXRCTELQ     X'01' DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                 REQ'D                                        
         DC    H'0'                                                             
*                                                                               
         MVC   CNTRNAME,CXRCTCON   CONTRACTOR NAME                              
         LA    R2,L'CNTRNAME       MAX LENGTH                                   
         LA    R3,CNTRNAME                                                      
         LA    R1,0                                                             
VCNTR10  CLI   0(R3),C' '          CALC NUMBER OF TRAILING SPACES               
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *+8                                                              
         LA    R1,0                                                             
         LA    R3,1(R3)                                                         
         BCT   R2,VCNTR10                                                       
         LA    R2,L'CNTRNAME       MAX LENGTH                                   
         SR    R2,R1                                                            
         STC   R2,CNTRNMLN                                                      
*                                                                               
         MVC   CONTNAME,CON#                                                    
         LA    R1,L'CON#                                                        
         STC   R1,CONTNMLN                                                      
         B     VCNTR30                                                          
*                                                                               
VCNTR15  LA    R2,L'CONTNAME       MAX LENGTH                                   
         LA    R3,CONTNAME                                                      
         LA    R1,0                                                             
VCNTR20  CLI   0(R3),C' '          CALC NUMBER OF TRAILING SPACES               
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *+8                                                              
         LA    R1,0                                                             
         LA    R3,1(R3)                                                         
         BCT   R2,VCNTR20                                                       
         LA    R2,L'CONTNAME       MAX LENGTH                                   
         SR    R2,R1                                                            
         STC   R2,CONTNMLN                                                      
*                                                                               
         USING CXADREL,R4                                                       
VCNTR30  L     R4,AIO                                                           
         MVI   ELCODE,CXADRELQ     X'22' CONTRACTOR ADDRESS                     
         BAS   RE,GETEL                                                         
         BNE   VCNTRX                                                           
         ZIC   R1,CXADRLEN                                                      
         SH    R1,=H'1'                                                         
         BM    VCNTRX                                                           
         EX    R1,*+4                                                           
         MVC   CNTRADR(0),CXADREL  SAVE WHOLE ELEM                              
*                                                                               
VCNTRX   B     XYES                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CATEGORY                                                      
*        INPUT                     R2=FIELD HEADER                              
*        OUTPUT                    CATEGORY = CATEGORY WORK CODE                
***********************************************************************         
*                                                                               
VVALCATG DS    0H                                                               
         XC    CATEGORY,CATEGORY                                                
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BM    XNO                                                              
         USING CATTBLD,R3                                                       
         LA    R3,CATTABLE         TABLE OF VALID CATEGORIES                    
VC10     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CATWORD(0),8(R2)                                                 
         BE    VC20                                                             
         LA    R3,CATTLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   VC10                                                             
         B     XNO                                                              
*                                                                               
VC20     MVC   CATEGORY,CATWRKCD                                                
VCXYES   B     XYES                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SUPPLIER                                                      
*        INPUT                     R2=FIELD HEADER                              
*        OUTPUT                    SUPPACT/SUPPFAX/SUPPSTAT/NAME/ADR            
*                                  SUPPOUL - OVERRIDE U/L                       
***********************************************************************         
*                                                                               
VVALSUPP DS    0H                                                               
         MVC   SUPPACT,SPACES                                                   
         MVI   SUPPSTAT,0                                                       
         XC    SUPPFAX,SUPPFAX                                                  
         MVC   SUPPNAME,SPACES                                                  
         MVC   SUPPADR,SPACES                                                   
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BM    VSXNO                                                            
         CLI   8(R2),C'*'          USER OVERRIDES U/L                           
         BE    VS03                                                             
         MVC   ACTKUNT(L'SUPPUL),SUPPUL                                         
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),8(R2)                                                 
         B     VS05                                                             
*                                                                               
VS03     BCTR  R1,0                LESS ONE FOR '*'                             
         EX    R1,*+4                                                           
         MVC   ACTKUNT(0),9(R2)    GET THE WHOLE SHABANG                        
*                                                                               
VS05     MVC   SUPPOUL,ACTKUNT                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   VSXNO                                                            
         GOTO1 GETREC                                                           
         MVC   SUPPACT,ACTKACT                                                  
*                                                                               
         USING NAMELD,R4                                                        
         L     R4,AIO                                                           
         MVI   ELCODE,NAMELQ       X'20' NAME ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   VS10                                                             
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    VS10                                                             
         EX    R1,*+4                                                           
         MVC   SUPPNAME(0),NAMEREC                                              
*                                                                               
         USING ADRELD,R4                                                        
VS10     L     R4,AIO                                                           
         MVI   ELCODE,OADELQ       X'8C' OVERRIDE ADR ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    VS15                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,ADRELQ       X'22' ADR ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   VS20                                                             
VS15     ZIC   R1,ADRLN                                                         
         SH    R1,=H'1'                                                         
         BM    VS20                                                             
         EX    R1,*+4                                                           
         MVC   SUPPADR(0),ADREL                                                 
*                                                                               
         USING RSTELD,R4                                                        
VS20     L     R4,AIO                                                           
         MVI   ELCODE,RSTELQ       X'30' STATUS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   VS30                                                             
         MVC   SUPPSTAT,RSTSTAT1                                                
*                                                                               
         USING FFTELD,R4                                                        
VS30     L     R4,AIO                                                           
         MVI   ELCODE,FFTELQ       X'DB' FREEFORM TEXT ELEM FOR FAX#            
         BAS   RE,GETEL                                                         
         BNE   VSXYES                                                           
         CLI   FFTTYPE,FFTTPFAX                                                 
         BNE   VSXYES                                                           
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SUPPFAX(0),FFTDATA                                               
*                                                                               
VSXYES   B     XYES                                                             
VSXNO    B     XNO                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        GET ORDER CONTROL RECORD AND LOCK                                      
*        OUTPUT                    NXTPONUM = CL6 NEXT ORDER NUMBER             
*                                  NXTPOBIN = XL4 NEXT ORDER NUMBER             
***********************************************************************         
*                                                                               
         USING ORDRECD,R6                                                       
VGETPO#  DS    0H                                                               
VGP10    XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,CMPY                                                     
         MVC   ORDKORD,=6C'0'                                                   
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ORDKEY),KEYSAVE                                         
         BE    VGP50                                                            
*                                                                               
         L     R6,AIO                                                           
         XC    0(100,R6),0(R6)                                                  
         MVC   0(L'ORDKEY,R6),KEYSAVE                                           
         MVC   BIGKEY(L'ORDKEY),KEYSAVE                                         
         USING ONCELD,R4                                                        
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ONCEL,ONCELQ        X'66' ORDER NUMBER CONTROL ELEM              
         MVI   ONCLN,ONCLNQ                                                     
         MVC   ONCNUM,=6C'0'                                                    
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         B     VGP10               GO BACK AND READ FOR UPDATE                  
*                                                                               
         USING ONCELD,R4                                                        
VGP50    MVI   RDUPDATE,C'Y'       LOCK FOR UPDATE                              
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,ONCELQ       X'66'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NXTPONUM,ONCNUM                                                  
VGP70    PACK  DUB,NXTPONUM        BUMP NUMBER                                  
         CVB   R0,DUB                                                           
         AH    R0,=H'1'                                                         
         STCM  R0,15,NXTPOBIN                                                   
         EDIT  (R0),(6,NXTPONUM),FILL=0                                         
*                                                                               
         XC    BIGKEY,BIGKEY       CHECK RECORD EXISTS                          
         LA    R6,BIGKEY                                                        
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,CMPY                                                     
         MVC   ORDKORD,NXTPONUM                                                 
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ORDKEY),KEYSAVE                                         
         BE    VGP70                                                            
*                                                                               
VGPX     B     XYES                                                             
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        GET ORDER CONTROL RECORD AND UPDATE NUMBER                             
*        INPUT                     NXTPONUM = CL6 NEXT ORDER NUMBER             
***********************************************************************         
*                                                                               
         USING ORDRECD,R6                                                       
VUPDPO#  DS    0H                                                               
VUP10    XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,CMPY                                                     
         MVC   ORDKORD,=6C'0'                                                   
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ORDKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ONCELD,R4                                                        
         MVI   RDUPDATE,C'Y'       LOCK FOR UPDATE                              
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,ONCELQ       X'66'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ONCNUM,NXTPONUM                                                  
         GOTO1 PUTREC                                                           
VUPX     B     XYES                                                             
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS *         
***********************************************************************         
*                                                                               
VMYERR   OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
         MVI   GTMSYS,6            DEFAULT TO ACC SYSTEM                        
         OC    GMSYS,GMSYS         OVERRIDE?                                    
         BZ    *+10                                                             
         MVC   GTMSYS,GMSYS        WHAT SYSTEM # MESSAGES                       
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*        LOCAL EXIT/ERROR ROUTINES                                    *         
***********************************************************************         
*                                                                               
CANTPUSH MVC   GERROR,=AL2(ACEPFK)           PUSH ERROR                         
         B     RETCURS                                                          
PFERR    MVC   GERROR,=AL2(ACEPFK)           INVALID PF KEY                     
RETCURS  LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         GOTO1 MYERR                                                            
*                                                                               
PLSENTER MVI   GERROR1,2           PLEASE ENTER FIELDS AS REQUIRED              
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         MVI   GMSYS,X'FF'                                                      
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
         MVI   GMSYS,X'FF'                                                      
         MVI   OKNO,0              CLEAR OKNO SO WON'T LOOP ENDLESSLY           
         L     R2,AFRSTREC         R2 TO 1ST SEL FIELD                          
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        STUFF                                                        *         
***********************************************************************         
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
RELO     DS    F                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QOFFAL)                                                      
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QTSAR)                                                       
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
**********************************************************************          
*        LTORG                                                       *          
**********************************************************************          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        RECORD AND ACTION TABLE (RECORDS)                            *         
***********************************************************************         
*                                                                               
RECACT   DS    0CL16                                                            
*                                                                               
RECACT1  DC    X'01'               X'01' = AVAILABLE RECORDS                    
         DC    CL8'CTAGS'          EXPANDED RECORD NAME                         
         DC    AL1(RTCTA)          RECORD NUMBER                                
         DC    X'0000'             PHASE NUM FOR DATA DICT/HELP SCREEN          
         DC    AL1(0,0,0,0)        SECURITY MASK BITS                           
*                                                                               
         DC    X'01'                                                            
         DC    CL8'FINANCE'                                                     
         DC    AL1(RTFIN)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DC    CL8'ORDER'                                                       
         DC    AL1(RTORD)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DC    CL8'DETAIL'                                                      
         DC    AL1(RTDET)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        RECORD AND ACTION TABLE (ACTIONS)                            *         
***********************************************************************         
*                                                                               
         DC    X'02'               X'02' = AVAILABLE ACTIONS                    
         DC    CL8'ADD'            EXPANDED ACTION NAME (ADD)                   
         DC    AL1(ACTADD)         ACTION NUMBER                                
         DC    AL1(ACTADD)         ACTION EQUATE                                
         DC    AL1(0)              SPARE                                        
         DC    AL1(0,0,0,0)        SECURITY MASK BITS                           
*                                                                               
         DC    X'02'                                                            
         DC    CL8'CHANGE'         CHANGE                                       
         DC    AL1(ACTCHA)                                                      
         DC    AL1(ACTCHA)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'DISPLAY'        DISPLAY                                      
         DC    AL1(ACTDIS)                                                      
         DC    AL1(ACTDIS)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'DELETE'         DELETE                                       
         DC    AL1(ACTDEL)                                                      
         DC    AL1(ACTDEL)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'LIST'           LIST                                         
         DC    AL1(ACTLIST)                                                     
         DC    AL1(ACTLIST)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'REPORT'         REPORT                                       
         DC    AL1(ACTREP)                                                      
         DC    AL1(ACTREP)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'RESTORE'        RESTORE                                      
         DC    AL1(ACTREST)                                                     
         DC    AL1(ACTREST)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'SELECT'         SELECT                                       
         DC    AL1(ACTSEL)                                                      
         DC    AL1(ACTSEL)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'BALANCE'        BALANCE                                      
         DC    AL1(11)                                                          
         DC    AL1(11)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'OPEN'           OPEN                                         
         DC    AL1(7)                                                           
         DC    AL1(7)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DC    CL8'CLOSE'          CLOSE                                        
         DC    AL1(8)                                                           
         DC    AL1(8)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        RECORD/ACTION COMBINATION TABLE                              *         
***********************************************************************         
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS 01=MY MAINT         
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                  XL4 SECURITY MASK BITS                       
*                                                                               
*                                                                               
RECACT3  DC    X'03',AL1(RTCTA,ACTDIS),X'F202000081'        CTAGS               
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTCTA,ACTCHA),X'F202000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTCTA,ACTADD),X'F202000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTFIN,ACTDIS),X'F404000081'        FINANCE             
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTFIN,ACTSEL),X'F404000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTFIN,ACTLIST),X'F304000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTFIN,ACTREP),X'F704000058'                            
         DC    C'BCBC',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTFIN,11),X'F903000081'                                
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTORD,ACTDIS),X'F505000081'        ORDER               
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTORD,ACTCHA),X'F505000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTORD,ACTADD),X'F505000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTORD,ACTDEL),X'F505000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTORD,ACTREP),X'F505000040'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTORD,ACTSEL),X'F505000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTORD,ACTLIST),X'F805000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTDET,ACTDIS),X'F605000081'        DETAILS             
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTDET,ACTCHA),X'F605000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTDET,ACTADD),X'F605000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTDET,ACTDEL),X'F605000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTDET,ACTREP),X'F605000040'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTDET,ACTSEL),X'F605000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTDET,ACTLIST),X'F805000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        DMOPEN FILE LISTS                                                      
***********************************************************************         
*                                                                               
SPFLIST  DS    0F                                                               
         DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
PRFLIST  DS    0F                                                               
         DC    CL8'NPRTDIR'                                                     
         DC    CL8'NPRTFILE'                                                    
         DC    C'X'                                                             
         SPACE 1                                                                
CTFLIST  DS    0F                                                               
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE                                                  *         
***********************************************************************         
*                                                                               
PFTABLE  DS    0C                                                               
         DC    AL1(PF02X-*,02,PFTCPROG,0,0)                                     
         DC    CL3' ',CL8'CTAGS   ',CL8'DISPLAY '                               
PF02X    EQU   *                                                                
*                                                                               
         DC    AL1(PF03X-*,03,PFTCPROG,0,0)                                     
         DC    CL3' ',CL8'FIN     ',CL8'LIST    '                               
PF03X    EQU   *                                                                
*                                                                               
         DC    AL1(PF04X-*,04,PFTCPROG,0,0)                                     
         DC    CL3' ',CL8'FIN     ',CL8'DISPLAY '                               
PF04X    EQU   *                                                                
*                                                                               
         DC    AL1(PF05X-*,05,PFTCPROG,0,0)                                     
         DC    CL3' ',CL8'ORDER   ',CL8'DISPLAY '                               
PF05X    EQU   *                                                                
*                                                                               
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLE OF VALID CATEGORIES                                    *         
***********************************************************************         
*                                                                               
CATTABLE DS    0C                                                               
         DC    CL10'MDSE      ',CL2'MD'                                         
         DC    CL10'AMEX      ',CL2'AX'                                         
         DC    CL10'CASH      ',CL2'CA'                                         
         DC    CL10'BP        ',CL2'CA'                                         
         DC    CL10'CASH/BP   ',CL2'CA'                                         
         DC    CL10'HOTEL     ',CL2'HL'                                         
         DC    CL10'SCRIP     ',CL2'SC'                                         
         DC    CL10'OTHER     ',CL2'OR'                                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLE OF SCREENS AND THE A(PFKEY LINE TABLE)                 *         
***********************************************************************         
*                                                                               
SCRTABL  DS    0C                                                               
         DC    X'00',AL4(PFLINEFF)                                              
         DC    X'F2',AL4(PFLINEF2)                                              
         DC    X'F3',AL4(PFLINEF3)                                              
         DC    X'F4',AL4(PFLINEF4)                                              
         DC    X'F5',AL4(PFLINEF5)                                              
         DC    X'F6',AL4(PFLINEF6)                                              
         DC    X'F7',AL4(PFLINEF7)                                              
         DC    X'F8',AL4(PFLINEF8)                                              
         DC    X'F9',AL4(PFLINEF9)                                              
         DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PFKEYS TABLES PER SCREEN FOR BUILDING PFKEY DISPLAY LINE     *         
***********************************************************************         
*                                                                               
PFLINEF2 DS    0C                                                               
         DC    AL1(RTFIN,ACTLIST,X'F3',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DC    CL15'FIN List'                                                   
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(RTORD,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,5)                                             
         DC    CL15'Order'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTBAL,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF9=  ',AL1(4,7)                                             
         DC    CL15'FIN Bal'                                                    
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF3 DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(RTORD,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,5)                                             
         DC    CL15'Order'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTBAL,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF9=  ',AL1(4,7)                                             
         DC    CL15'FIN Bal'                                                    
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF4 DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTLIST,X'F3',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DC    CL15'FIN List'                                                   
*                                                                               
         DC    AL1(RTORD,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,5)                                             
         DC    CL15'Order'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTBAL,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF9=  ',AL1(4,7)                                             
         DC    CL15'FIN Bal'                                                    
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF5 DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTLIST,X'F3',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DC    CL15'FIN List'                                                   
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(RTDET,ACTDIS,X'F6',PFLSSEC+PFLSRET)                          
         DC    CL6'PF6=  ',AL1(4,6)                                             
         DC    CL15'Detail'                                                     
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF6 DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTLIST,X'F3',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DC    CL15'FIN List'                                                   
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(RTORD,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,5)                                             
         DC    CL15'Order'                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF7 DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(RTORD,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,5)                                             
         DC    CL15'Order'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTBAL,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF9=  ',AL1(4,7)                                             
         DC    CL15'FIN Bal'                                                    
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF8 DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTLIST,X'F3',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DC    CL15'FIN List'                                                   
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF9 DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTLIST,X'F3',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DC    CL15'FIN List'                                                   
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(RTORD,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,5)                                             
         DC    CL15'Order'                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'Return'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEFF DS    0C                                                               
         DC    AL1(RTCTA,ACTDIS,X'F2',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,5)                                             
         DC    CL15'CTAGS'                                                      
*                                                                               
         DC    AL1(RTFIN,ACTLIST,X'F3',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DC    CL15'FIN List'                                                   
*                                                                               
         DC    AL1(RTFIN,ACTDIS,X'F4',PFLSSEC+PFLSRET)                          
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DC    CL15'FIN Dis'                                                    
*                                                                               
         DC    AL1(RTORD,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,5)                                             
         DC    CL15'Order'                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DC    CL15'RETURN'        RETURN                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
*        SCREENS                                                     *          
**********************************************************************          
*                                                                               
       ++INCLUDE ACCTAFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
**********************************************************************          
*        ACCTADSECT AND ACCTAWORKD                                   *          
**********************************************************************          
*                                                                               
       ++INCLUDE ACCTADSECT                                                     
         EJECT                                                                  
       ++INCLUDE ACCTAWORKD                                                     
         EJECT                                                                  
**********************************************************************          
*        PFKEY DISPLAY LINE DSECT                                    *          
**********************************************************************          
*                                                                               
PFLISTD  DSECT                                                                  
PFLREC   DS    XL1                 RECORD EQUATE                                
PFLACT   DS    XL1                 ACTION EQUATE                                
PFLSCR   DS    XL1                 SCREEN THAT WOULD BE LOADED                  
PFLSTAT  DS    XL1                                                              
PFLSSEC  EQU   X'80'               VALIDATE SECURITY                            
PFLSRET  EQU   X'40'               VALIDATE NOT = RETURN SCREEN                 
PFLSNRET EQU   X'20'               IS RETURN VALID                              
PFLSNXTS EQU   X'10'               ARE THERE ANY PENDING SELECTS                
PFLSDDS  EQU   X'08'               DISPLAY ONLY ON DDS TERMINAL                 
PFLS2ND  EQU   X'04'               DISPLAY 2ND LINE OF PFKEYS                   
PFLPF#   DS    CL6                 PF##=                                        
PFLPF#LN DS    XL1                 LEN TO MOVE FOR PF#=                         
PFLDLEN  DS    XL1                 DESCRIPTION LENGTH                           
PFLDESC  DS    CL15                DATA DICT DESCRIPTION                        
PFLLENQ  EQU   *-PFLISTD                                                        
*                                                                               
**********************************************************************          
*        CATEGORY TABLE DSECT                                        *          
**********************************************************************          
*                                                                               
CATTBLD  DSECT                                                                  
CATWORD  DS    CL10                CHARACTER CATEGORY                           
CATWRKCD DS    CL2                 CATEGORY WORK CODE                           
CATTLNQ  EQU   *-CATTBLD                                                        
         EJECT                                                                  
**********************************************************************          
*        EQUATES                                                     *          
**********************************************************************          
*                                                                               
LENSPOOL EQU   SPOOLEND-SPOOLD                                                  
LENGEND  EQU   GENDEND-GEND                                                     
LENSYSD  EQU   SYSDEND-SYSD                                                     
LENWORK  EQU   LENSPOOL+LENGEND+LENSYSD+LENIOAS+DISBLKLN                        
         EJECT                                                                  
**********************************************************************          
*        OTHER INCLUDES                                              *          
**********************************************************************          
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCTA                                                       
       ++INCLUDE SPGENCTR                                                       
*PREFIX=C                                                                       
       ++INCLUDE CTGENFILE                                                      
*PREFIX=                                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACCTA00   05/01/02'                                      
         END                                                                    
