*          DATA SET SPOMS00    AT LEVEL 086 AS OF 03/14/16                      
*PHASE T23400C                                                                  
*INCLUDE BINSRCH2                                                               
***INCLUDE DPTRD                 **REMOVED**                                    
***INCLUDE MEDGET                **REMOVED**                                    
*INCLUDE SPOMCOM                                                                
T23400   TITLE 'SPOMS00 - DARE CONTROLLER'                                      
T23400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T23400,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         L     RA,4(R1)            A(TWA)                                       
         USING T234FFD,RA                                                       
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
         BNE   XIT                                                              
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE                                   
         BNZ   MAIN02              THEN SKIP DDLINK STUFF                       
         L     R2,SYSPARMS         TEST IF CALLED FROM DDLINK UPLOAD            
         L     R2,16(R2)           R2=A(COMFACS)                                
         L     RF,CGLOBBER-COMFACSD(R2)                                         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,10,GLVDLUWF                             
         CLI   8(R1),0             WORKER FILE GLOBAL NOT PRESENT               
         BNE   MAIN02                                                           
         L     RF,CGENNEW-COMFACSD(R2)                                          
         MVI   GENSTAT6,GES$UPLD   ENABLE DDLINK AUTO UPLOAD FOR                
         MVI   MODE,TESTGLOB       SPOT DESKTOP ORDER RECALLS                   
         GOTO1 (RF),DMCB,SPOOLD                                                 
         DC    H'0'                GENNEW SHOULD RETURN TO MONITOR              
*                                                                               
MAIN02   OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR SAVED MESSAGES                         
         OI    CONHEADH+6,X'80'                                                 
         XC    CONHED2,CONHED2                                                  
         OI    CONHED2H+6,X'80'                                                 
*                                                                               
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BE    MAIN10                                                           
         CLC   TWAAGY,=C'TM'       AND NOT TMNY USER                            
         BE    MAIN10                                                           
         MVC   CONHEAD(37),=C'The OM program is no longer available'            
         OI    CONHEADH+1,X'08'    HIGH INTENSITY                               
         OI    CONHEADH+6,X'80'                                                 
         B     XIT                                                              
*                                                                               
MAIN10   OC    TWAVPRNT,TWAVPRNT   IF OFFLINE                                   
         BNZ   MAIN20              THEN SKIP GLOBBER STUFF                      
         GOTO1 =A(CALLDARE),RR=RELO                                             
*                                                                               
MAIN20   TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
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
         L     RE,SYSPARMS                                                      
         L     RE,8(RE)            GET A(SPOT FACS)                             
         USING SPSYSFAC,RE                                                      
         MVC   RECUP,SRECUP                                                     
         DROP  RE                                                               
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
         ST    RF,CALLOV           SAVE IN COMMON STORAGE                       
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
* NEED TO LOAD GETRATE SEPARATELY SO ADDRESS GOES IN VTYPES                     
         MVI   DMCB+7,QGETRATE                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   GETRATE,0(R1)                                                    
*                                                                               
         MVI   DMCB+7,QSTAPACK                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   VSTAPACK,DMCB        SAVE MODULE ADDRESS                         
*                                                                               
         BRAS  RE,GETGO29          GET LINKAGE ROUTINE ADDRESS                  
         ST    R1,VGOTO29                                                       
*                                                                               
         LA    R0,GOMSPACK                                                      
         ST    R0,MSPACK                                                        
         LA    R0,GOMSUNPK                                                      
         ST    R0,MSUNPK                                                        
*                                                                               
         MVI   SYSTEM,C'S'         SPOT                                         
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 6000 BYTES                       
         XC    SYSDUMMY,SYSDUMMY                                                
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
         MVC   SYSPHASE,=X'D9023400'    PRESET FOR SYSTEM CALLOVS               
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
         LA    R1,RECACT2          DON'T ALLOW ALL RECORD TYPES                 
         ST    R1,ARECACT2                                                      
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
*&&DO                                                                           
         LA    R1,TREPTAB                                                       
         ST    R1,ATREPTAB                                                      
         LA    R1,RREPTAB                                                       
         ST    R1,ARREPTAB                                                      
*&&                                                                             
         OI    GENSTAT1,NOSETEFH+USKYMRG+APPLIC                                 
*        OI    GENSTAT2,RETEQSEL                                                
         OI    GENSTAT3,OKVALSEL+RESTXE00                                       
*                                                                               
         CLI   TWASCR,X'B4'        IS ORDER PMKGD SCREEN LOADED?                
         BNE   *+8                                                              
         OI    GENSTAT3,MULTFILS   YES, TURN ON MULTI FILES                     
*                                                                               
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
         OC    TWASAGN,TWASAGN     ON NEW SECURITY                              
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS    OR HAVE LIMIT ACCESS                       
         BZ    XIT                                                              
*                                                                               
         LHI   RF,SECBLK-T234FFD                                                
         AR    RF,RA                                                            
         ST    RF,ASECBLK                                                       
         XC    0(256,RF),0(RF)     SO SECRET WILL INIT PROPERLY                 
*                                                                               
         L     R2,SYSPARMS                                                      
         L     R2,16(R2)           R2=A(COMFACS)                                
         L     RF,CSECRET-COMFACSD(R2)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
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
         NOP   *                                                                
         B     VEST                                                             
         B     VREFN                                                            
         B     VBUYER                                                           
         B     VSTNDCOM                                                         
         B     VRD4ROUT                                                         
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
VUSER    GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
* COMMENTED BECAUSE WE NEED TO KNOW IF PASSWORD REQUIRED FOR SCRIPTS            
*&&DO                                                                           
         CLI   OFFLINE,C'Y'        ALWAYS DO THIS OFFLINE                       
         BE    VUSER10                                                          
         CLI   TWAFIRST,0          TEST FIRST TIME                              
         BE    VUSER10             YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER20                                                          
*&&                                                                             
VUSER10  MVI   TWAFIRST,1          WE'VE BEEN THROUGH HERE                      
*                                                                               
***  CHECK TO SEE IF IT'S YN FR M2 AND H7                                       
         NI    OMBASFL2,X'FF'-OM2DACT                                           
         CLC   TWAAGY,=C'YN'                                                    
         BE    VUSER11                                                          
         CLC   TWAAGY,=C'FR'                                                    
         BE    VUSER11                                                          
         CLC   TWAAGY,=C'M2'                                                    
         BE    VUSER11                                                          
         CLC   TWAAGY,=C'H7'                                                    
         BNE   *+8                                                              
VUSER11  OI    OMBASFL2,OM2DACT                                                 
*                                                                               
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
         MVI   ELCODE,X'02'        GET USER ID                                  
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVUSERID,2(R6)                                                   
*                                                                               
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,7                                                         
         NI    OMBASFLG,X'FF'-OMFLPSWD                                          
         BAS   RE,FIRSTEL                                                       
         BNE   VUSER12             NOT REQUIRED IF NO X'07' ELEM                
         TM    2(R6),X'80'         PASSWORD REQUIRED?                           
         BZ    *+8                                                              
         OI    OMBASFLG,OMFLPSWD                                                
         DROP  R4                                                               
*                                                                               
VUSER12  L     R4,AIO                                                           
         USING CTIKEY,R4                                                        
         LA    R6,CTIDATA                                                       
         MVI   DARPTNER,0          CLEAR THE DARE PARTNER CODE                  
         XC    DARROUTE,DARROUTE      AND DARE ROUTING CODE                     
         MVI   ELCODE,X'33'        DARE PARTNER INFO                            
         BAS   RE,FIRSTEL                                                       
         BNE   VUSER15                                                          
         USING CTUSAD,R6                                                        
         MVC   DARPTNER,CTUSADPI   SAVE THIS INFORMATION FOR LATER              
         MVC   DARROUTE,CTUSADRC                                                
         DROP  R6                                                               
*                                                                               
VUSER15  L     R4,AIO                                                           
         USING CTIKEY,R4                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    VUSER16                                                          
         LA    R2,CONHEADH                                                      
         XC    8(L'CONHEAD,R2),8(R2)                                            
         MVC   8(L'MSNGORGN,R2),MSNGORGN                                        
         OI    6(R2),X'80'                                                      
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
MSNGORGN DC    C'MISSING ORIGIN INFO IN IDI RECORD'                             
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
VUSER16  MVC   USERNAME,CTORGNAM                                                
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
         BNE   VUSER25                                                          
         B     VUSER20A                                                         
*                                                                               
         CLI   TWASCR,X'F4'        IS AVMKT MAINT SCREEN LOADED?                
         BNE   *+18                                                             
         CLC   =C'AVM',CONREC      RECORD TYPE AVMKT?                           
         BNE   VUSER25                                                          
         B     VUSER20A                                                         
*                                                                               
         CLI   TWASCR,X'F8'        IS AVMKT MAINT SCREEN LOADED?                
         BNE   VUSER25                                                          
         CLC   =C'AVC',CONREC      RECORD TYPE AVMKT?                           
         BNE   VUSER25                                                          
*                                                                               
VUSER20A CLC   =C'SEL',CONACT      ACTION SELECT?                               
         BNE   VUSER40                                                          
         CLI   THISLSEL,C'C'       'C' ENTERED AS LIST SELECTION?               
         BNE   VUSER40                                                          
         CLI   PFKEY,3             ERASE A LINE?                                
         BE    *+12                                                             
         CLI   PFKEY,4             INSERT A LINE?                               
         BNE   VUSER40                                                          
         MVC   CONACT(3),=C'CHA'   FORCE ACTION TO CHANGE                       
         B     VUSER40                                                          
*                                                                               
VUSER25  DS    0H                                                               
*        CLI   TWASCR,X'FE'        IS ORDER OFFR SCREEN LOADED?                 
*        BNE   VUSER40                                                          
         CLC   =C'ORD',CONREC      RECORD TYPE COMMENT?                         
         BNE   VUSER40                                                          
         CLI   CONACT,C'M'                                                      
         BNE   VUSER40                                                          
         MVC   CONACT(3),=C'OFF'   FORCE ACTION TO STATUS                       
*                                                                               
VUSER40  DS    0H                                                               
         TM    GOTGLOB,GGLBDAR                                                  
         BO    VUSERX                                                           
*                                                                               
         TM    TRNSTAT,RCHANG      IF RECORD FIELD HAS CHANGED                  
         BZ    VUSER50                                                          
         MVI   CALLSP,0            CLEAR CALLPROG STACK                         
         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
*                                                                               
VUSER50  TM    TRNSTAT,ACHANG      TEST ACTION CHANGED TO LIST                  
         BZ    VUSERX                                                           
         MVI   CALLSP,0            CLEAR CALLPROG STACK                         
         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
*                                                                               
VUSERX   B     XIT                                                              
*******                                                                         
VINITIAL GOTO1 =A(VINIT),RR=RELO                                                
         B     XIT                                                              
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
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   VMYERR                                                           
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
         MVC   CLTOFFCE,COFFICE    SAVE THE OFFICE FOR LATER                    
         GOTO1 CLUNPK,DMCB,(CPROF+6,BCLT),QCLT   IN CASE THEY USE AAN           
         MVC   SVACCAGY,CACCAGY                                                 
         MVC   SVACCOFC,CACCOFC                                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0FX'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
         CLI   CLTOFFCE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFCE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         MVC   PFFXUOFL(2),WORK+16                                              
*&&DO                                                                           
**  THIS IS OLD CODE THAT DOES NOT SUPPORT CLIENT LIST SECURITY                 
         MVI   GERROR1,SECLOCK                                                  
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    XIT                                                              
         OC    TWAACCS(2),TWAACCS  ANY SECURITY LIMIT?                          
         BZ    XIT                                                              
         CLI   TWAACCS,C'*'        OFFICE LOCKOUT?                              
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'+'        MKT LOCKOUT?                                 
         BE    XIT                 YES                                          
         CLI   TWAACCS,C'$'        OFFICE LIST?                                 
         BE    CLT30               YES                                          
         CLC   TWAACCS(2),BCLT                                                  
         BNE   VMYERR                                                           
         B     XIT                                                              
*                                                                               
CLT20    LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,COFFICE                                                       
         LA    R0,1                                                             
*                                                                               
CLT25    CLC   TWAACCS+1(1),0(R1)                                               
         BE    XIT                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,CLT25                                                         
         B     VMYERR                                                           
*&&                                                                             
CLT30    MVC   SVCACCS,CACCESS                                                  
         MVI   SVMACCS,X'FF'                                                    
         MVC   SVOFFC,COFFICE                                                   
*                                                                               
         CLI   TWAOFFC,C'*'        IGNORE SECUIRTY IF DDS TERMINAL              
         BE    XIT                                                              
         CLI   TWAACCS,C'+'        TEST MARKET LIMIT ACCESS?                    
         BE    XIT                                                              
         BRAS  RE,CALLOFCR                                                      
         CLI   GERROR1,0                                                        
         BE    XIT                                                              
*                                                                               
* ON OFFICER ERROR, WE DON'T CARE IF WE'RE OFFER/LIST OR ORDER/MG               
         CLI   RECNUM,RECOFFR      OFFER ONLY DISPLAY CLIENT ON LIST            
         BE    XIT                                                              
*                                                                               
         CLI   RECNUM,RECORDR                                                   
         BNE   VMYERR                                                           
         CLI   ACTNUM,ACTMAKGD                                                  
         BNE   VMYERR                                                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE EBCDIC PRODUCT CODE                                                  
*                                                                               
* ON ENTRY:    (R2)                A(PRODUCT FIELD HEADER)                      
*                                                                               
* ON EXIT:     BPRD                BINARY PRODUCT CODE                          
*              QPRD                EBCDIC PRODUCT                               
*              PRDNM               PRODUCT NAME                                 
***********************************************************************         
VPROD    MVI   GERROR1,INVPROD                                                  
         CLC   =C'AAA',8(R2)                                                    
         BE    VMYERR                                                           
         CLI   5(R2),2                                                          
         BL    VMYERR                                                           
         CLI   5(R2),3                                                          
         BH    VMYERR                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),8(R2)                                                   
         OC    KEY+4(3),SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
         MVC   BPRD,PCODE+1                                                     
         MVC   QPRD,KEY+4          NEED THE SPACE-PADDED PRODUCT                
         MVC   PRDNM,PNAME                                                      
         B     XIT                                                              
         DROP  R6                                                               
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
*                                                                               
         CVD   RE,DUB              ALWAYS HAVE ZERO LEADING                     
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST(3),DUB                                                      
*                                                                               
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
         MVC   SVEDAILY,EDAILY                                                  
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
***********************************                                             
* MEDIA NOT RADIO                                                               
***********************************                                             
         CLI   1(R4),0             TEST SUB-MEDIA ENTERED WITH A '-'            
         BNE   VSTA4               YES                                          
*                                                                               
         CLI   0(R4),4                                                          
         BNH   VSTA3                                                            
         CLI   QMED,C'T'                                                        
         BNE   VSTA3                                                            
*                                                                               
         CLI   12+4(R4),C'L'                                                    
         BNE   *+12                                                             
         MVI   QSTA+4,C'L'                                                      
         B     VSTA10                                                           
*                                                                               
         CLI   12+4(R4),C'D'       DIGITAL VIDEO?                               
         BNE   VSTA3                                                            
         MVI   QSTA+4,C'D'                                                      
         B     VSTA10                                                           
*                                                                               
VSTA3    MVC   QSTA+4(1),QMED      ELSE SET SUB-MED = MEDIA                     
         B     VSTA10                                                           
*                                                                               
VSTA4    MVC   QSTA+4(1),22(R4)    MOVE SUB-MEDIA                               
         CLI   1(R4),1                                                          
         BNE   VMYERR                                                           
         CLC   QSTA+4(1),QMED      IF INPUT, MUST MATCH MEDIA CODE              
         BE    VSTA10                                                           
         CLI   QMED,C'T'           UNLESS MEDIA T                               
         BNE   VMYERR                                                           
         CLI   QSTA+4,C'L'         WHERE WE CAN HAVE LOW POWER STATION          
         BE    VSTA10                                                           
         CLI   QSTA+4,C'D'                       AND DIGITAL VIDEO              
         BE    VSTA10                                                           
         B     VMYERR                                                           
***********************************                                             
* MEDIA = RADIO - AM, FM OR SM IS REQUIRED                                      
***********************************                                             
VSTA6    CLI   1(R4),0             BAND SEPARATED BY A '-'?                     
         BNE   VSTA7               YES                                          
         CLI   0(R4),4             NO, ANY BAND?                                
         BNH   VMYERR                  NO, ERROR                                
         LA    RE,12+4(R4)                                                      
         B     *+8                                                              
*                                                                               
VSTA7    LA    RE,22(R4)                                                        
*                                                                               
         CLI   0(RE),C'A'          AM, FM OR SM?                                
         BE    VSTA8                                                            
         CLI   0(RE),C'F'                                                       
         BE    VSTA8                                                            
         CLI   0(RE),C'S'                                                       
         BE    VSTA8                                                            
         CLI   0(RE),C'C'                                                       
         BNE   VMYERR              NO, ERROR                                    
VSTA8    MVC   QSTA+4(1),0(RE)     YES, COPY BAND                               
         B     VSTA10                                                           
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
         MVI   ERROPT,C'Y'         RETURN IF ERROR ON READ                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',KEY,AIO                  
         MVI   ERROPT,C'N'         RESET OTHERWISE PRG RETURNS ON ERROR         
         TM    8(R1),X'10'         RECORD NOT FOUND?                            
         BZ    *+8                                                              
         MVI   GERROR1,INVSTAT                                                  
         CLI   8(R1),0                                                          
         BNE   VMYERR                                                           
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         MVC   QMKT,SMKT                                                        
         GOTO1 MSPACK,DMCB,QMKT,QSTA,BMKTSTA                                    
*&&DO                                                                           
         CLI   QMED,C'R'                                                        
         BNE   VSTA13                                                           
         CLC   =C'000',STAKCLT     NON-CLIENT SPECIFIC STATION?                 
         BE    VSTA12X             NO                                           
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
         LA    R6,1000(R6)         READ NON-SPECIFIC INTO HERE                  
         ST    R6,AIO                                                           
         MVI   ERROPT,C'Y'         RETURN IF ERROR ON READ                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',KEY,AIO                  
         MVI   ERROPT,C'N'         RESET OTHERWISE PRG RETURNS ON ERROR         
         MVC   SVAFFL,SNETWRK                                                   
VSTA12X  MVC   QUNIQID,STUNIQID    SAVE STATION'S UNIQUE ID                     
*&&DO                                                                           
         OC    QUNIQID,QUNIQID     RADIO STATION SHOULD HAVE UNIQUE ID          
         BNZ   VSTA12Y                                                          
         MVC   GERROR,=AL2(1172)                                                
         B     VMYERR              ELSE ERROR                                   
*&&                                                                             
VSTA12Y  L     R6,AIO1                                                          
         EJECT                                                                  
* READ MARKET RECORD TO IO1+1000                                                
*&&                                                                             
VSTA13   LA    R6,1000(R6)                                                      
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
*&&DO                                                                           
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VSTA20                                                           
         LA    RF,TWAACCS                                                       
         CLI   TWAACCS,C'+'        MARKET LOCKOUT?                              
         BE    *+16                                                             
         LA    RF,TWAACCS+2                                                     
         CLI   TWAACCS+2,C'+'        MARKET LOCKOUT?                            
         BNE   VSTA20                                                           
*                                                                               
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
         CLC   1(1,RF),0(R1)                                                    
         BE    VSTA20                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         MVI   GERROR1,NOMKTACC                                                 
         B     VMYERR                                                           
*&&                                                                             
         MVC   SVMACCS,MKTLTACC                                                 
         CLI   TWAACCS,C'+'                                                     
         BNE   VSTA20                                                           
         BRAS  RE,CALLOFCR                                                      
         CLI   GERROR1,0                                                        
         BNE   VMYERR                                                           
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
         LA    R1,SLNTAB                                                        
         LA    R0,15                                                            
         CLC   0(1,R1),WORK+4      COMPARE BINARY VALUES                        
         BE    VSLN10                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     VMYERR                                                           
*                                                                               
VSLN10   MVC   WORK+4(1),11(R4)                                                 
         B     XIT                                                              
*                                                                               
SLNTAB   DC    AL1(10,15,20,30,40,45,50,60,90,120,105,150,75,5)                 
         DC    5AL1(0)                                                          
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
         MVC   GTMSYS,GETMSYS      SYSTEM MESSAGES                              
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
         OC    0(3,R2),SPACES                                                   
         GOTO1 RCPACK,DMCB,(C'P',(R2)),(X'80',KEY)                              
         BNE   VBYR10              INVALID BUYER                                
*                                                                               
         LA    R4,KEY              SET UP THE KEY                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKBYR,0(R2)                                                    
         OC    BYRKBYR,SPACES                                                   
         MVC   QBYR,BYRKBYR                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BE    *+14                                                             
VBYR10   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY BEING USED                       
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
* READ DARE STATION RECORD TO GET REP CODE                                      
*                                                                               
* ON ENTRY:    PARAM 1              A(DEST FIELD HEADER)                        
*              PARAM 2              A(ROUTE FIELD HEADER)                       
*                                                                               
* ** WARNING ***                                                                
*   AIO2 & AIO3 GET CLOBBERED                                                   
***********************************************************************         
VRD4ROUT GOTO1 =A(RD4ROUT),RR=RELO                                              
         B     XIT                                                              
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
         EJECT                                                                  
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=SYSRB,WORK=(R4,8),LABEL=*                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
***      MVC   STAPQSTA(8),0(RE)                                                
         MVC   STAPQSTA,0(RE)                                                   
***      CLI   STAPQSTA,C'0'                                                    
***      BNE   *+10                                                             
***      XC    STAPQNET,STAPQNET                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         B     *+6     <---- WHOA 18FEB98 IGNORE ERRORS                         
         DC    H'0'                                                             
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GOMSUNPK NTR1  BASE=SYSRB,WORK=(R4,8),LABEL=*                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         B     *+6    <--- MHER 1/17/95  IGNORE ERRORS                          
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
RELO     DS    F                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    V(SPOMCOM)                                                       
         DC    A(0)                                                             
         DC    A(0)                A(LINKED RECP) GOES HERE                     
         DC    V(BINSRCH)                                                       
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                A(GETRATE) GOES HERE                         
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QQSORT)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QGETIDS)                                                     
         DC    AL1(QSPAUTH)                                                     
         DC    AL1(QRCPACK)                                                     
         DC    AL1(QGETDARE)                                                    
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
         DC    X'01',C'COMMENT ',AL1(06),X'0000'                                
         DC    X'01',C'ORDER   ',AL1(09),X'0000'                                
         DC    X'01',C'FLIGHT  ',AL1(05),X'0000'                                
         DC    X'01',C'BYRMOVE ',AL1(02),X'0000'                                
         DC    X'01',C'MARKET  ',AL1(08),X'0000'                                
         DC    X'01',C'OFFER   ',AL1(10),X'0000'                                
         DC    X'01',C'DESTOV  ',AL1(11),X'0000'                                
         DC    X'01',C'EMAIL   ',AL1(12),X'0000'                                
         DC    X'01',C'LSTMTHD ',AL1(13),X'0000'                                
         DC    X'01',C'OCOM    ',AL1(14),X'0000'                                
         DC    X'01',C'CLTORDER',AL1(15),X'0000'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
RECACT2  DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,04,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,02,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,05,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,03,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,05,00)                                  
         DC    X'02',C'STATUS  ',AL1(07,07,00)                                  
         DC    X'02',C'MAKEGOOD',AL1(08,08,00)                                  
         DC    X'02',C'MG      ',AL1(08,08,00)                                  
         DC    X'02',C'OFFER   ',AL1(08,08,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'SEND    ',AL1(13,13,00)                                  
         DC    X'02',C'RECALL  ',AL1(14,14,00)                                  
         DC    X'02',C'NOTDARE ',AL1(15,15,00)                                  
         DC    X'02',C'MAINT   ',AL1(16,16,00)                                  
         DC    X'02',C'MOVE    ',AL1(17,17,00)                                  
         DC    X'02',C'TRANSMIT',AL1(18,18,00)                                  
         DC    X'02',C'BYRCNFM ',AL1(19,19,00)                                  
         DC    X'02',C'INVITE  ',AL1(20,20,00)                                  
         DC    X'02',C'PREVIEW ',AL1(21,21,00)                                  
         DC    X'02',C'BRANDS  ',AL1(22,22,00)                                  
         DC    X'02',C'SUMMARY ',AL1(23,23,00)                                  
         DC    X'02',C'PQONLY  ',AL1(24,13,00)  FOR FAX TO PQ ONLY              
         DC    X'02',C'PATCH   ',AL1(10,25,00)                                  
         DC    X'02',C'PSTATUS ',AL1(02,26,00)                                  
         DC    X'02',C'PSEND   ',AL1(02,27,00)                                  
         DC    X'02',C'PSUPP   ',AL1(02,28,00)                                  
         DC    X'02',C'PMKGD   ',AL1(02,29,00)                                  
         DC    X'02',C'PHISTORY',AL1(10,30,00)                                  
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
         DC    X'03',AL1(06,01),X'F606000080',C'    '  COMMENT  ADD             
         DC    X'03',AL1(06,04),X'F606000080',C'    '  COMMENT  CHA             
         DC    X'03',AL1(06,02),X'F606000080',C'    '  COMMENT  DIS             
         DC    X'03',AL1(06,05),X'F606000080',C'    '  COMMENT  DEL/RES         
         DC    X'03',AL1(06,03),X'F606000080',C'    '  COMMENT  SEL             
         DC    X'03',AL1(06,10),X'E606000080',C'    '  COMMENT  LIST            
         DC    X'03',AL1(06,21),X'D020000081',C'    '  COMMENT  PREVIEW         
*                                                                               
         DC    X'03',AL1(09,07),X'D904000080',C'    '  ORDER   STATUS           
         DC    X'03',AL1(09,08),X'FE03000080',C'    '  ORDER   OFFER            
         DC    X'03',AL1(09,10),X'E908000080',C'    '  ORDER   LIST             
         DC    X'03',AL1(09,25),X'B010000080',C'    '  ORDER   PATCH            
         DC    X'03',AL1(09,26),X'B111000080',C'    '  ORDER   PSTATUS          
         DC    X'03',AL1(09,27),X'B212000080',C'    '  ORDER   PSEND            
         DC    X'03',AL1(09,28),X'B313000080',C'    '  ORDER   PSUPP            
         DC    X'03',AL1(09,29),X'B414000080',C'    '  ORDER   PMKGD            
         DC    X'03',AL1(09,30),X'B515000080',C'    '  ORDER   PHISTORY         
         DC    X'03',AL1(09,13),X'F9090009C1',C'ORDA'  ORDER   SEND             
         DC    X'03',AL1(09,18),X'F9090009C1',C'ORDA'  ORDER   TRANSMIT         
         DC    X'03',AL1(09,14),X'F9090009C1',C'ORDA'  ORDER   RECALL           
         DC    X'03',AL1(09,15),X'F9090009C1',C'ORDA'  ORDER   NOTDARE          
         DC    X'03',AL1(09,12),X'F202000230',C'ORDA'  ORDER   REPORT           
         DC    X'03',AL1(09,19),X'FB0B000081',C'    '  ORDER   BYRCNFM          
         DC    X'03',AL1(09,05),X'D222000080',C'    '  ORDER   DELETE           
         DC    X'03',AL1(09,21),X'F9090009C1',C'ORDA'  ORDER   PREVIEW          
         DC    X'03',AL1(09,22),X'F9090009C1',C'ORDA'  ORDER   BRANDS           
         DC    X'03',AL1(09,23),X'FC0C000C30',C'ORDA'  ORDER   SUMMARY          
*                                                                               
         DC    X'03',AL1(10,07),X'F303000081',C'    '  OFFER   STATUS           
*                                                                               
         DC    X'03',AL1(02,17),X'C907000041',C'BMBV'  BYRMOVE  REPORT          
         DC    X'03',AL1(02,12),X'C907000098',C'BMBV'  BYRMOVE  REPORT          
*                                                                               
         DC    X'03',AL1(05,01),X'F505000080',C'    '  FLIGHT   ADD             
         DC    X'03',AL1(05,04),X'F505000080',C'    '  FLIGHT   CHA             
         DC    X'03',AL1(05,02),X'F505000080',C'    '  FLIGHT   DIS             
         DC    X'03',AL1(05,03),X'F505000080',C'    '  FLIGHT   SEL             
         DC    X'03',AL1(05,10),X'E505000080',C'    '  FLIGHT   LIST            
*                                                                               
         DC    X'03',AL1(08,10),X'EA0A000080',C'    '  MARKET   LIST            
*                                                                               
         DC    X'03',AL1(11,16),X'C717000081',C'    '  DESTOV   MAINT           
*                                                                               
         DC    X'03',AL1(12,20),X'C818000081',C'    '  EMAIL    INVITE          
*                                                                               
         DC    X'03',AL1(13,02),X'C616000080',C'    '  LSTMTHD  DIS             
         DC    X'03',AL1(13,05),X'C616000080',C'    '  LSTMTHD  DEL/RES         
         DC    X'03',AL1(13,03),X'C616000080',C'    '  LSTMTHD  SEL             
         DC    X'03',AL1(13,10),X'CA16000080',C'    '  LSTMTHD  LIST            
*                                                                               
         DC    X'03',AL1(14,01),X'C119000080',C'    '  OCOMMENT ADD             
         DC    X'03',AL1(14,04),X'C119000080',C'    '           CHANGE          
         DC    X'03',AL1(14,02),X'C119000080',C'    '           DISP            
         DC    X'03',AL1(14,05),X'C119000080',C'    '           DELETE          
         DC    X'03',AL1(14,03),X'C119000080',C'    '           SELECT          
         DC    X'03',AL1(14,05),X'C119000080',C'    '           RESTOR          
         DC    X'03',AL1(14,10),X'C219000080',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(15,12),X'D1210000F0',C'C6C6'  CLTORDER REPORT          
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
CALLOFCR NTR1  BASE=*,LABEL=*,WORK=(R4,8)                                       
         XC    GERROR,GERROR                                                    
         OC    TWAACCS(2),TWAACCS       TEST ANY LIMIT ACCESS                   
         JZ    XIT                      NO                                      
*                                                                               
         XC    0(64,R4),0(R4)                                                   
         USING OFFICED,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         L     RF,OFFICER                                                       
         GOTO1 (RF),DMCB,(C'N',(R4)),ACOMFACS                                   
         CLI   0(R1),0                                                          
         JE    XIT                                                              
*                                                                               
         MVI   GERROR1,SECLOCK                                                  
         CLI   TWAACCS,C'+'                                                     
         BNE   *+8                                                              
         MVI   GERROR1,NOMKTACC                                                 
         J     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* READ DARE STATION RECORD TO GET REP CODE                                      
*                                                                               
* ON ENTRY:    PARAM 1 BYTE 0       X'80' = IGNORE LAST METHOD RECORD           
*                                   X'40' = CALLER'S LAST METHD RECORD          
*                                   X'20' = CALLING FOR DESTOV/MAINT            
*                      BYTE 1-3     A(DEST FIELD HEADER)                        
*                                                                               
*              PARAM 2 BYTE 0       X'80' = PREVIOUS (ROUT=EML OR FAX)          
*                      BYTE 1-3     A(ROUTE FIELD HEADER)                       
*                                                                               
*              PARAM 3 BYTE 0       MISC FLG BYTE (SEE SPOMS0A & 09)            
*                                   X'80' = RADIO ORDER                         
*                                   X'40' = FAX ALL RADIO ORDERS                
*                                                                               
* ON EXIT:     CC                   CONDITION CODE                              
*              GERROR               ERROR CODE FOR THE  <>  CONDITION           
*              PARAM 1 BYTE 0       X'80' = REP DOES NOT APPEND OFFICE          
*                                                                               
* ** WARNING ***                                                                
*   AIO2 & AIO3 GET CLOBBERED                                                   
***********************************************************************         
RD4ROUT  NTR1  BASE=*,LABEL=*                                                   
         XC    GERROR,GERROR                                                    
         L     R3,DMCB             R3 = A(DEST FIELD HEADER)                    
         OI    6(R3),X'80'                                                      
*                                                                               
         NI    OMBASFLG,X'FF'-OMFIGNLM-OMFCLRLM-OMFPRVOR-OMFNOOFF               
         NI    OMBASFLG,X'FF'-OMFFXRAD                                          
         NI    OMBASFL2,X'FF'-OM2DSTOV                                          
*                                                                               
         TM    DMCB,X'80'                                                       
         BZ    *+8                                                              
         OI    OMBASFLG,OMFIGNLM   IGNORE LAST METHOD RECORD                    
*                                                                               
         TM    DMCB,X'40'                                                       
         BZ    *+8                                                              
         OI    OMBASFLG,OMFCLRLM   CALLER'S LAST METHD REC IN AIO3              
*                                                                               
         TM    DMCB,X'20'          CALLED BY DESTOV                             
         BZ    *+8                                                              
         OI    OMBASFL2,OM2DSTOV                                                
*                                                                               
         TM    DMCB+4,X'80'                                                     
         BZ    *+8                                                              
         OI    OMBASFLG,OMFPRVOR   PREV ORDER NOW                               
*                                                                               
         TM    DMCB+8,X'40'        FAX ALL RADIO ORDERS                         
         BZ    *+8                                                              
         OI    OMBASFLG,OMFFXRAD   YES                                          
*                                                                               
         L     R5,DMCB+4           R5 = A(ROUTE FIELD HEADER)                   
         OI    6(R5),X'80'         NOTE: HAS TO BE SOFT                         
         ZIC   R1,0(R5)                                                         
         SHI   R1,8+1              L'HEADER AND 1 FOR EX                        
         TM    1(R5),X'02'         EXTENDED FLDHDR?                             
         BZ    *+8                                                              
         SHI   R1,8                JUST IN CASE                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R5),8(R5)                                                    
         LA    R2,8(R5)                                                         
         MVI   SVDARSTA,0                                                       
*                                                                               
D        USING COMPDATD,WORK                                                    
         XC    WORK,WORK                                                        
         SR    R4,R4                                                            
         ICM   R4,7,DMCB+9                                                      
         BZ    RD4RLM00                                                         
         MVC   D.EDIDATB,0(R4)                                                  
         OC    D.EDIDATB,D.EDIDATB                                              
         BZ    RD4RLM00                                                         
         GOTO1 DATCON,DMCB,(8,D.EDIDATB),(0,D.EDIDATC)                          
***************                                                                 
* LAST METHOD RECORD                                                            
***************                                                                 
RD4RLM00 MVC   AIO,AIO3                                                         
         TM    OMBASFLG,OMFCLRLM   CALLER SETUP LAST MTHD IN AIO3?              
         BNZ   RD4RLM06            YES, WE'LL USE THAT INSTEAD                  
*                                                                               
         L     RF,AIO                                                           
         XC    0(2,RF),0(RF)       SO WE KNOW NOTHING WAS HERE                  
*                                                                               
         TM    OMBASFLG,OMFIGNLM   IGNORE LAST METHOD RECORD                    
         BNZ   RD4RDS00            YES                                          
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING DMTHKEY,R6                                                       
         MVI   DMTHTYPE,DMTHTYPQ   X'0D3E'                                      
         MVI   DMTHSBTY,DMTHSBTQ                                                
         MVC   DMTHAGMD,BAGYMD                                                  
         MVC   DMTHBYR,QBYR                                                     
         MVC   DMTHSTA,BSTA                                                     
         MVC   DMTHCLT,BCLT                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     ANY LAST METHOD BY THIS CLIENT?              
         BE    RD4RLM03            YES                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    DMTHCLT,DMTHCLT     MAYBE NOT CLIENT SPECIFIC?                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     ANY LAST METHOD BY THIS CLIENT?              
         BNE   RD4RDS00            NEITHER, DEFAULT TO DSTA RECORD              
*                                                                               
RD4RLM03 L     RE,AIO              SO WE DON'T GET GARBAGE                      
         LHI   RF,LIOS                                                          
         XCEFL                                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              PUT OUR RECORD IN AIO3                       
         TM    4(R3),X'20'         DEST FIELD CHANGED?                          
         BZ    RD4RDS00            YES, LAST METHOD GOING TO CHANGE             
RD4RLM06 L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         USING DMTHELD,R6                                                       
*                                                                               
         CLI   DMTHDEST,C'R'                                                    
         BNE   *+14                                                             
         MVC   8(3,R3),=C'REP'                                                  
         B     RD4RLM07                                                         
         CLI   DMTHDEST,C'S'                                                    
         BNE   *+14                                                             
         MVC   8(3,R3),=C'STA'                                                  
         B     RD4RLM07                                                         
*                                                                               
         L     RF,AIO                                                           
         XC    0(2,RF),0(RF)       SO WE LAST METHOD NOT VALID                  
         B     RD4RDS00                                                         
*                                                                               
RD4RLM07 MVI   5(R3),3                                                          
         CLI   DMTHMTHD,C'E'       EMAIL?                                       
         BNE   RD4RLM10                                                         
         MVC   0(5,R2),=C'EML: '                                                
         ZIC   R1,0(R5)                                                         
         SHI   R1,8+5+1                                                         
         CHI   R1,L'DMTHBDEC-1      FIELD LONGER THAN DATA?                     
         BL    *+8                                                              
         LA    R1,L'DMTHBDEC-1                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R2),DMTHBDEC                                                 
         B     RD4RDS00                                                         
*                                                                               
RD4RLM10 CLI   DMTHMTHD,C'F'       FAX                                          
         BNE   RD4RDS00                                                         
         MVC   0(5,R2),=C'FAX: '                                                
         CLI   DMTHDEST,C'S'       WAS FAXING TO STA?                           
         BNE   *+10                                                             
         MVC   5(L'QSTA,R2),QSTA                                                
         DROP  R6                                                               
***************                                                                 
* DSTA RECORD                                                                   
***************                                                                 
RD4RDS00 DS    0H                                                               
         MVC   AIO,AIO2            USE AIO2 FOR THIS                            
         TM    OMBASFLG,OMFFXRAD   FAXING RADIO ORDERS ONLY?                    
         BNZ   RD4R200                                                          
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING CT$STAKEY,R6                                                     
         MVI   CT$STAKEY,CT$STAKSYSQ     X'005A'                                
         MVI   CT$STAKTYP,CT$STAKTYPQ                                           
         MVC   CT$STAKMEDA,QMED                                                 
         MVC   CT$STAKSTIN,QSTA                                                 
         MVC   KEYSAVE,KEY                                                      
*                                                                               
RD4RDS01 GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                   
RD4RDS02 CLC   KEY(CT$STAKEFDA-CT$STAKEY),KEYSAVE     STATION MATCH?            
         BNE   RD4RDS20            NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,D.TDYDATE)  GET TODAYS DATE                 
*                                                                               
         CLC   KEY(32),KEYSAVE     REREADING LIVE STATION?                      
         BE    RD4RDS25            YES                                          
*                                                                               
RD4RDS05 XR    RE,RE               DSTA DATES ARE NEGATED NOT FF'D              
         ICM   RE,7,KEY+24                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,D.EFDATEB                                                   
         GOTO1 DATCON,DMCB,(3,D.EFDATEB),(0,D.EFDATEC) CONVERT TO CHAR          
         CLC   D.EFDATEC,D.TDYDATE  EFFECTIVE DATE AFTER TODAY?                 
         BNH   RD4RDS07             YES, NOT EFFECTIVE YET                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEYSAVE,KEY                   
         B     RD4RDS02             YES, NOT EFFECTIVE YET                      
*                                                                               
RD4RDS07 OC    D.EDIDATC,D.EDIDATC ALREADY SENT EDI?                            
         BZ    RD4RDS10            NO                                           
         CLC   D.EDIDATC,D.EFDATEC EDI DATE >= EFFECTIVE DATE?                  
         BH    RD4RDS25            YES, USE CURRENT DSTA                        
         B     RD4RDS12                                                         
*                                                                               
RD4RDS10 GOTO1 ADDAY,DMCB,D.EFDATEC,D.EFDATEC,300   ADD 300 DAYS                
         CLC   D.TDYDATE,D.EFDATEC  IS EFF. DATE 300 DAYS OF TODAY              
         BH    RD4RDS25             NO                                          
*                                                                               
RD4RDS12 MVC   KEYSAVE,KEY         SAVE LIVE STATION                            
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEYSAVE,KEY                   
         CLC   KEY(24),KEYSAVE     CHECK FOR ANY DARK STATIONS?                 
         BNE   RD4RDS17            - NONE                                       
         OC    D.EDIDATC,D.EDIDATC - YES, BUT DID WE SENT IT EDI?               
         BNZ   RD4RDS05              - YES, CHECK EFFECTIVE DATE                
         OI    SVDARSTA,DARDRKST     - NO, SET FLAG FOR DARK STATION            
RD4RDS17 MVC   KEY,KEYSAVE         REREAD LIVE STATION                          
         B     RD4RDS01                                                         
         DROP  D                                                                
***************                                                                 
* DSTA RECORD WAS NOT FOUND!!!                                                  
***************                                                                 
RD4RDS20 TM    4(R3),X'20'         WAS DEST FIELD CHANGED?                      
         BZ    RD4RDS22            YES                                          
         L     R6,AIO3                                                          
         OC    0(2,R6),0(R6)       ANY LAST METHOD RECORD?                      
         BZ    RD4R200             NONE                                         
         MVI   ELCODE,DMFXELQ      X'10'                                        
         BAS   RE,GETEL                                                         
         BNE   RD4R200                                                          
         LR    R1,R6                                                            
         B     RD4R156             DISPLAY THE OVERRIDE NUMBER WE SAVED         
*                                                                               
RD4RDS22 CLC   =C'REP',8(R3)       DID THEY CHANGE DEST TO REP                  
         BNE   RD4R200             HOW ABOUT BDE RECORD FOR STATION?            
         MVC   8(5,R5),=C'FAX: '   SO REP/FAX IS SAVED IN LSTMTHD REC           
         B     RD4RE2              REP REQUIRES SPCL FAX#, USE DESTOV!          
***************                                                                 
* WE GOT OUR DSTA RECORD!!!                                                     
***************                                                                 
RD4RDS25 L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,(R6),DMWORK            
*                                                                               
         XC    SVREP,SVREP         CLEAR FOR REP                                
*                                                                               
         CLC   AGENCY,=C'H9'       MEDIAVEST                                    
         BE    *+14                                                             
         CLC   AGENCY,=C'SJ'       FOR TESTING PURPOSES                         
         BNE   RD4RDS28                                                         
*                                                                               
         LA    RF,VSSTATAB         IS IT A VSG STATION?                         
RD4RVS00 CLI   0(RF),X'FF'         EOT?                                         
         BE    RD4RDS28            YES, NO WORRIES                              
         CLC   QSTA(4),0(RF)                                                    
         BE    *+12                                                             
         AHI   RF,L'VSSTATAB                                                    
         B     RD4RVS00                                                         
*                                                                               
         LA    RF,VSCNETAB         NOW SEE IF OUR CLT AND EST MATCHES           
RD4RVS10 CLI   0(RF),X'FF'                                                      
         BE    RD4RDS28            NO, NO WORRIES                               
         CLC   QCLT,0(RF)                                                       
         BNE   RD4RVS15                                                         
         CLC   QEST,3(RF)                                                       
         BE    *+12                                                             
RD4RVS15 AHI   RF,L'VSCNETAB                                                    
         B     RD4RVS10                                                         
*                                                                               
         CLC   QCLT,=C'WH '        THE WH /100 ENTRY IS ONLY FOR SJR            
         BNE   RD4RVS17                                                         
         CLC   AGENCY,=C'SJ'       FOR TESTING PURPOSES                         
         BNE   RD4RDS28            MEDIAVEST MIGHT HAVE A WH /100               
*                                                                               
RD4RVS17 L     R6,AIO                                                           
         USING CT$STAKEYD,R6                                                    
         LA    R6,CT$STAFSTEL      POINT TO FIRST ELEMENT                       
         XR    R0,R0                                                            
RD4RVS20 CLI   0(R6),0             E-O-R?                                       
         BNE   *+6                                                              
         DC    H'0'                BAD ENUF HERE                                
         CLI   0(R6),CT$STAREPCQ   IT SHOULD BE REP INFO ELEMENT                
         BE    RD4RVS30                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD4RVS20                                                         
*                                                                               
         USING CT$STAREPD,R6                                                    
RD4RVS30 MVC   CT$STAREPCR,=C'VSG'   PURPOSEFUL OVERRIDE TO VSG FOR H9          
         CLC   =C'DCBS',QSTA                                                    
         BNE   *+10                                                             
         MVC   CT$STAREPCR,=C'EJO'   EJOR FOR DCBS AND CLT/EST WH/100           
         DROP  R6                                                               
*                                                                               
RD4RDS28 L     R4,AIO3                                                          
         OC    0(2,R4),0(R4)       ANY LAST METHOD RECORD?                      
         BZ    RD4R00              NONE                                         
*                                                                               
         TM    OMBASFLG,OMFCLRLM   CALLER'S LAST METHOD?                        
         BZ    RD4RDS30            NO, WE SHOULD LOOK FOR HOME MARKET           
         CLC   =C'FAX: ',0(R2)                                                  
         BE    RD4R14                                                           
         CLC   =C'EML: ',0(R2)                                                  
         BE    RD4R14              YES, DON'T LOOK FOR HOME MKT EITHER          
         B     RD4RFH00                                                         
*                                                                               
         USING DMTHKEY,R4                                                       
RD4RDS30 LA    R4,DMTHFRST                                                      
         DROP  R4                                                               
         XR    R0,R0                                                            
RD4RDS35 CLI   0(R4),0             R4 = A(DATETIME ELEM) IN LAST METHOD         
         BE    RD4RDS55            NONE, THEN CHECK ACTIVD ELEM                 
         CLI   0(R4),X'D1'                                                      
         BE    RD4RDS40                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RD4RDS35                                                         
*                                                                               
         USING CT$STAKEY,R6                                                     
RD4RDS40 LA    R6,CT$STAFSTEL                                                   
RD4RDS45 CLI   0(R6),0             R6 = A(DATETIME ELEM) IN DSTA RECORD         
         BE    RD4RDS55            NONE, THEN CHECK ACTIVD ELEM                 
         CLI   0(R6),X'D1'                                                      
         BE    RD4RDS50                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD4RDS45                                                         
*                                                                               
         USING DATTIMD,R6                                                       
RD4RDS50 CLC   DATTMGDT,DATTMGDT-DATTIMD(R4) LAST METHOD MORE RECENT?           
         BH    RD4RDS90                      NO CLEAR IT                        
         BL    RD4RDS85                      YES                                
*                                            DATE SAME!                         
         CLC   DATTMGTM,DATTMGTM-DATTIMD(R4) LSTMTHD TIME MORE RECENT?          
         BH    RD4RDS90                      NO CLEAR IT                        
         B     RD4RDS85                                                         
         DROP  R6                                                               
*                                                                               
RD4RDS55 L     R6,AIO                                                           
         L     R4,AIO3                                                          
*                                                                               
         USING DMTHKEY,R4                                                       
RD4RDS60 LA    R4,DMTHFRST                                                      
         DROP  R4                                                               
         XR    R0,R0                                                            
RD4RDS65 CLI   0(R4),0             R4 = A(ACTIVITY ELEM) IN LAST METHOD         
         BE    RD4R00              DON'T DIE IF NONE, USE DSTA                  
         CLI   0(R4),X'F1'                                                      
         BE    RD4RDS70                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RD4RDS65                                                         
*                                                                               
         USING CT$STAKEY,R6                                                     
RD4RDS70 LA    R6,CT$STAFSTEL                                                   
RD4RDS75 CLI   0(R6),0             R6 = A(ACTIVITY ELEM) IN DSTA RECORD         
         BE    RD4RFH00            DON'T DIE IF NONE, USE LAST METHOD           
         CLI   0(R6),X'F1'                                                      
         BE    RD4RDS80                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD4RDS75                                                         
*****                                                                           
* SO HOME MARKET MIGHT NOT TAKE PRECEDENT IF I HAVE A MORE RECENT LAST          
* METHOD RECORD.                                                                
*****                                                                           
         USING ACTVD,R6                                                         
RD4RDS80 CLC   ACTVCHDT,ACTVCHDT-ACTVD(R4)  LAST METHOD MORE RECENT?            
         BH    RD4RDS90                     NO, CLEAR IT OUT                    
RD4RDS85 CLC   =C'FAX: ',0(R2)     METHOD FAX OR EMAIL?                         
         BE    RD4R14                                                           
         CLC   =C'EML: ',0(R2)                                                  
         BE    RD4R14              YES                                          
         B     RD4RFH00            NO, INBOX, LET IT FIND INBOX IF ANY          
*                                                                               
RD4RDS90 TM    4(R3),X'20'         WAS DEST FIELD CHANGED?                      
         BZ    RD4R00              YES, DON'T CLEAR                             
         XC    8(3,R3),8(R3)          CLEAR WHAT THE LAST METHOD                
         MVI   5(R3),0                    MIGHT HAVE PUT OUT ONTO THE           
         XC    0(5,R2),0(R2)              DEST AND ROUTE FIELDS                 
**************************************                                          
* WE'RE GOING TO USE THE DSTA RECORD                                            
**************************************                                          
RD4R00   L     RF,AIO3                CLEAR THE LAST METHOD                     
         XC    0(2,RF),0(RF)                                                    
***********************************************************************         
* CODE TO CHECK IF WE NEED TO CREATE A FAKE HOME MARKET                         
*                                                                               
* NOTE: DSTA RECORD NEEDS A X'31' ELEMENT FOR THE RECEIVING ID SO THAT          
*       THE DARE DISPATCHER KNOWS THIS IS HARD-CODED                            
***********************************************************************         
RD4RFH00 MVC   WORK(10),SPACES                                                  
*                                                                               
*****    LA    RF,HOVTAB           HAD ADDRESSIBILITY PROBLEM                   
         BRAS  RE,GETHOVTB                                                      
         LR    RF,R1                                                            
         USING HOVTABD,RF                                                       
RD4RFH10 CLI   HOVTSTA,X'FF'       EOT?                                         
         BE    RD4R10              YES, NOTHING TO WORRY ABOUT THEN             
         CLC   QSTA(4),HOVTSTA     MATCHES ON THE STATION IN TABLE?             
         BNE   RD4RFH15                                                         
         CLC   AGENCY,HOVTAGY      MATCHES ON AGENCY?                           
         BNE   RD4RFH15                                                         
         CLC   DARROUTE+3(2),HOVTOFF                                            
         BE    RD4RFH20                                                         
RD4RFH15 LA    RF,HOVTABL(RF)                                                   
         B     RD4RFH10                                                         
*                                                                               
RD4RFH20 MVC   WORK(4),HOVTSTA                                                  
         DROP  RF                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CT$STAHOMCQ                                               
         MVC   HALF,DATADISP                                                    
         MVC   DATADISP,=H'42'                                                  
         BAS   RE,GETEL                                                         
         MVC   DATADISP,HALF                                                    
         BE    RD4RFH25                                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING CT$STAHOMD,R4          HOME MARKET ELEMENT DSECT                 
         MVI   CT$STAHOMC,CT$STAHOMCQ                                           
         MVI   CT$STAHOMLN,CT$STAHOMLQ                                          
         GOTO1 RECUP,DMCB,(X'FE',AIO),ELEM,(R6),=X'002A002007D0'                
         DROP  R4                                                               
*                                                                               
         USING CT$STAHOMD,R6                                                    
RD4RFH25 MVC   CT$STAHOMIC,WORK                                                 
         B     RD4RHM20                                                         
*                                                                               
RD4R10   L     R6,AIO                                                           
         USING CT$STAKEY,R6                                                     
         LA    R6,CT$STAFSTEL         POINT TO FIRST ELEMENT                    
         XR    R0,R0                                                            
RD4R12   CLI   0(R6),0                ANY HOME MARKET ELEM?                     
         BNE   RD4R15                                                           
RD4R14   CLC   =C'STA',8(R3)          NONE, DEST TO STA?                        
         BE    RD4R200                      YES, LOOK FOR DESTN RECORD          
         B     RD4R25                       NO, LOOK AT THE REP                 
*                                                                               
RD4R15   CLI   0(R6),CT$STAHOMCQ      X'30' HOME MARKET ELEM?                   
         BE    RD4RHM00                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD4R12                                                           
*                                                                               
         USING CT$STAHOMD,R6                                                    
RD4RHM00 CLC   CT$STAHOMCT,DARROUTE+3 SAME HOME OFFICE MARKET?                  
         BE    RD4RHM20               YES, DEFINITELY HOME MARKET               
         TM    4(R3),X'20'            WAS DEST CHANGED?                         
         BZ    RD4RHM10               YES                                       
         TM    OMBASFLG,OMFIGNLM      NO, IGNORE LAST METHOD RECORD             
         BNZ   RD4R14                                                           
         L     R4,AIO3                NO, DO WE HAVE LAST METHOD?               
         OC    0(2,R4),0(R4)                                                    
         BZ    RD4R25                                                           
         LA    R4,24(R4)              STATION INBOX FROM LAST METHOD?           
         USING DMTHELD,R4                                                       
         CLI   DMTHDEST,C'S'                                                    
         BNE   RD4R25                                                           
         CLI   DMTHMTHD,C'I'                                                    
         BE    RD4RHM10             YES                                         
         B     RD4R200              NO, FAX OR EMAIL                            
*                                                                               
INVLLMTH L     R4,AIO3              NO, INVALIDATE LAST METHOD                  
         XC    0(2,R4),0(R4)                                                    
         ZIC   R1,0(R5)                                                         
         SHI   R1,8+1              L'HEADER AND 1 FOR EX                        
         TM    1(R5),X'02'         EXTENDED FLDHDR?                             
         BZ    *+8                                                              
         SHI   R1,8                JUST IN CASE                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R5),8(R5)                                                    
         LA    R2,8(R5)            RESET POINTER                                
         MVI   SVDARSTA,0                                                       
         B     RD4RDS00                                                         
         DROP  R4                                                               
*                                                                               
RD4RHM10 CLC   =C'STA',8(R3)        DEST CHANGED TO STA?                        
         BNE   RD4R25               NO, CHANGED TO REP, SO NOT HOMEMKT          
         CLC   CT$STAHOMIC,SPACES   YES, USE INBOX CODE IF ANY                  
         BNH   RD4R200                   OTHERWISE USE DESTN                    
*                                                                               
RD4RHM20 OI    SVDARSTA,DARHMKTQ                                                
         CLC   =C'REP',8(R3)       DID USER CHANGE DEST TO REP                  
         BNE   RD4RHM25                                                         
*****                                                                           
* SPECIAL CODE FOR VSG  MVNY  WCBS                                              
*****                                                                           
         CLC   =C'WCBST',QSTA      WE LOOKING AT WCBS?                          
         BNE   RD4RHM24            NO, SPECIAL ONLY FOR VSG                     
         CLC   AGENCY,=C'H9'                                                    
         BE    *+14                                                             
         CLC   AGENCY,=C'SJ'                                                    
         BNE   RD4RHM24                                                         
*                                                                               
         LA    RF,VSCNETAB         NOW SEE IF OUR CLT AND EST MATCHES           
RD4RHM21 CLI   0(RF),X'FF'                                                      
         BE    RD4RHM24            NOPE, NOT FOR VSG                            
         CLC   QCLT,0(RF)                                                       
         BNE   RD4RHM22                                                         
         CLC   QEST,3(RF)                                                       
         BE    RD4R25              HAS TO BE REP/INBOX FOR VSG                  
RD4RHM22 AHI   RF,L'VSCNETAB                                                    
         B     RD4RHM21                                                         
*****                                                                           
* SPECIAL CODE FOR VSG  MVNY  WCBS                                              
*****                                                                           
RD4RHM24 TM    4(R3),X'20'         WAS DEST FIELD CHANGED?                      
         BNZ   RD4RHM25            NO, NOT A PROBLEM                            
         MVI   GERROR1,INVALID                                                  
         B     NO                  THEN INVALID                                 
*                                                                               
RD4RHM25 MVC   8(3,R3),=C'STA'     THEN SHOW THAT IT IS GOING TO STA            
*                                                                               
RD4RHM30 XC    SVREP,SVREP         CLEAR FOR REP                                
         MVC   SVREP+15(L'CT$STAHOMIC),SPACES                                   
         TM    OMBASFLG,OMFPRVOR   CANNOT BE PREV & HOME MARKET                 
         BNZ   RD4R20              GO TO FAX                                    
         CLC   CT$STAHOMIC,SPACES                                               
         BNH   RD4R20                                                           
*  USE RECEIVING ID FOR DARE                                                    
         MVC   SVREP+15(L'CT$STAHOMIC),CT$STAHOMIC                              
         B     RD4R100                                                          
*                                                                               
RD4R20   OI    SVDARSTA,DARFXNGQ   WE'RE GOING TO FAX                           
         MVC   0(5,R2),=C'FAX: '   SHOW THIS TO THE USER                        
         LA    R2,5(R2)                                                         
         XC    SVREP+15(L'CT$STAHOMIC),SVREP+15                                 
         MVC   SVREP+15(L'QSTA),QSTA                                            
         CLI   QSTA+4,C'T'         TV STATION?                                  
         BNE   RD4R100                                                          
         MVI   SVREP+15+4,C' '     THEN WE DON'T NEED THE T IN SVREP            
         B     RD4R100                                                          
***************                                                                 
* NOT HOME MARKET, SO WE KNOW IT IS A REP                                       
***************                                                                 
RD4R25   L     R6,AIO                                                           
         USING CT$STAKEYD,R6                                                    
         LA    R6,CT$STAFSTEL      POINT TO FIRST ELEMENT                       
         XR    R0,R0                                                            
RD4R30   CLI   0(R6),0             E-O-R?                                       
         BE    RD4R100                                                          
         CLI   0(R6),CT$STAREPCQ   IT SHOULD BE REP INFO ELEMENT                
         BE    RD4R35                                                           
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD4R30                                                           
*                                                                               
         USING CT$STAREPD,R6                                                    
RD4R35   LA    RE,CT$STAREPST_ND                                                
         LA    RF,CT$STAREPCR                                                   
         OC    CT$STAREPED,CT$STAREPED ANY EFFECTIVE DATE?                      
         BZ    RD4R40              NONE                                         
         CLC   JDTTODAY,CT$STAREPED TODAY ON OR AFTER EFFECTIVE DATE?           
         BNL   RD4R40                                                           
         LA    RE,CT$STAREPST_PND                                               
         LA    RF,CT$STAREPPR      NO, USE PREVIOUS REP THEN                    
*                                                                               
RD4R40   EX    RE,RD4R40TM                                                      
         BO    RD4R40FX                                                         
         CLC   =C'FAX: ',0(R2)     LSTMETHOD SAID FAX                           
         BE    RD4R40FX                                                         
         TM    OMBASFLG,OMFPRVOR   CANNOT BE PREV AND DARE REP                  
         BZ    RD4R45                                                           
RD4R40FX OI    SVDARSTA,DARFXNGQ   NON-DARE IS ALSO FAXING                      
         B     RD4R45                                                           
RD4R40TM TM    CT$STAREPST,0    **EXECUTED**                                    
*                                                                               
RD4R45   NI    OMBASFLG,X'FF'-OMFJDSRP NOT A JDS REP YET                        
         LA    R1,JDSREPS          DETERMINE IF WE HAVE A JDS REP               
RD4R50   CLI   0(R1),0                                                          
         BE    RD4R55                                                           
         CLC   0(L'JDSREPS,R1),0(RF)                                            
         BE    *+12                                                             
         LA    R1,L'JDSREPS(R1)                                                 
         B     RD4R50                                                           
         OI    OMBASFLG,OMFJDSRP   WE HAVE A JDS REP                            
*                                                                               
RD4R55   GOTOR GETDARE,DMCB,(C'R',0),(RF),BLOCK,A(480),DATAMGR                  
         BNE   RD4RX                                                            
         LA    R1,BLOCK                                                         
         USING DAREPTD,R1                                                       
DR       USING DRREPTBD,SVREP                                                   
         MVC   DR.DRREPCOD,DAPTCODE                                             
         MVC   DR.DRREPNAM,DAPTNAME                                             
         MVC   DR.DRREPFLG,DAPTFLG1                                             
         MVC   DR.DRREPLPF,DAPTLPFX                                             
         MVC   DR.DRREPPFX,DAPTRPPF                                             
*                                                                               
         CLC   =C'FAX: ',0(R2)     LSTMETHOD SAID FAX                           
         BE    RD4R70                                                           
         TM    OMBASFLG,OMFPRVOR   IF PREV, SHOW "FAX: " ON SCREEN              
         BNZ   RD4R70                                                           
         TM    DAPTFLG1,DPF1NODR   X'20' A DARE REP?                            
         BZ    RD4R75              YES                                          
***********************************                                             
* AGENCY ALPHA CODE TESTING                                                     
***  UNIVISION AND  ROB  ***                                                    
*  A NON-DARE REP WOULD ONLY LIKE TO RECEIVE EDI FOR A STATION FROM AN          
*    AGENCY AND OFFICE OF THAT AGENCY.  AFTER KNOWING THAT THE REP IS           
*    A NON-DARE REP, WE CAN CHECK AGAINST THE TABLE.                            
***********************************                                             
         LA    RF,UNITAB                                                        
         USING UNITABD,RF                                                       
RD4R61   CLI   UNITREP,X'FF'       HIT THE END OF SPECIAL TABLE?                
         BE    RD4R63              MIGHT NEED ROUTING CODE TESTING              
         CLC   UNITREP,SVREP       MAKE SURE THE REP MATCHES                    
         BNE   RD4R62                                                           
         CLC   UNITSTA,QSTA        MATCH ON STATION?                            
         BNE   RD4R62                                                           
         CLC   UNITAGY,AGENCY      MATCH ON POWER CODE?                         
         BNE   RD4R62                                                           
         CLC   UNITOFF,DARROUTE+3                                               
         BE    RD4R75              TREAT AS IF IT IS A DARE REP                 
RD4R62   LA    RF,UNITABL(RF)                                                   
         B     RD4R61                                                           
         DROP  RF                                                               
*                                                                               
*** SPECIAL TABLE FOR REP, STATION, AGENCY & OFFICE                             
UNITAB   DS    0XL(UNITABL)                                                     
         DC    C'ROB',C'DMON',C'SJ',C'NY'                                       
*&&DO*&& DC    C'UNI',C'KMEX',C'TR',C'DA'                                       
         DC    X'FF'                                                            
*                                                                               
UNITABD  DSECT                                                                  
UNITREP  DS    CL3                 NON-DARE REP TO MATCH AGAINST SVREP          
UNITSTA  DS    CL4                 STATION CALL LETTERS                         
UNITAGY  DS    CL2                 AGENCY POWER CODE                            
UNITOFF  DS    CL2                 OFFICE IN ROUTING CODE                       
UNITABL  EQU   *-UNITABD                                                        
T23400   CSECT                                                                  
***********************************                                             
* ROUTING CODE TESTING                                                          
***  ABC  AND  ROB  ***                                                         
*  A NON-DARE REP WOULD ONLY LIKE TO RECEIVE EDI FROM A PARTICULAR              
*    AGENCY AND OFFICE OF THAT AGENCY.  AFTER KNOWING THAT THE REP IS           
*    A NON-DARE REP, WE CAN CHECK AGAINST THE TABLE.                            
***********************************                                             
RD4R63   LA    RF,ABCTAB                                                        
         USING ABCTABD,RF                                                       
RD4R64   CLI   ABCTREP,X'FF'       HIT THE END OF SPECIAL TABLE?                
         BE    RD4R69              YES, REP STAYS AS FAX                        
         CLC   ABCTREP,SVREP       MAKE SURE THE REP MATCHES                    
         BNE   RD4R65                                                           
         CLC   DARROUTE,ABCTRAGY   DARROUTE = AGENCY(3) + OFFICE(2)             
         BE    RD4R75              TREAT AS IF IT IS A DARE REP                 
RD4R65   LA    RF,ABCTABL(RF)                                                   
         B     RD4R64                                                           
         DROP  RF                                                               
*                                                                               
*** SPECIAL TABLE FOR REP, AGENCY & OFFICE                                      
ABCTAB   DS    0XL(ABCTABL)                                                     
         DC    C'ROB',C'SJ ',C'CH'                                              
         DC    C'MOB',C'SJR',C'LA'  DAMN!! WHY IS IT SJR AND NOT SJ???          
         DC    C'ABC',C'SJR',C'LA'  20070105 so MO can test with LIVE           
*                                                                               
         DC    C'ABC',C'STR',C'CH'  STARCOM/STARLINK                            
         DC    C'ABC',C'STL',C'CH'                                              
*                                                                               
         DC    C'ABC',C'GMP',C'AT'  GM PLANWORKS                                
         DC    C'ABC',C'GMP',C'CH'                                              
         DC    C'ABC',C'GMP',C'DA'                                              
         DC    C'ABC',C'GMP',C'DE'                                              
         DC    C'ABC',C'GMP',C'LA'                                              
         DC    C'ABC',C'GMP',C'MI'                                              
         DC    C'ABC',C'GMP',C'NY'                                              
*                                                                               
         DC    C'ABC',C'RP ',C'LA'  RUBEN POSTAER                               
         DC    C'ABC',C'RP ',C'PH'                                              
         DC    C'ABC',C'RP ',C'DA'                                              
         DC    C'ABC',C'RP ',C'AT'                                              
         DC    C'ABC',C'RP ',C'CH'                                              
*                                                                               
         DC    C'ABC',C'RPD',C'LA'  RUBEN POSTAER DIRECT                        
*                                                                               
         DC    C'ABC',C'MUW',C'CA'  MULLEN                                      
         DC    C'ABC',C'MUW',C'BO'                                              
*&&DO                                                                           
         DC    C'ABC',C'STR',C'LA'  TAKEN OUT NOV30/05 AS PER KLEF              
                                                                                
         DC    C'ABC',C'GMP',C'CA'  THESE OFFICES WILL GO IN LATER              
         DC    C'ABC',C'GMP',C'EF'                                              
         DC    C'ABC',C'GMP',C'HO'                                              
         DC    C'ABC',C'GMP',C'NO'                                              
         DC    C'ABC',C'GMP',C'SL'                                              
         DC    C'ABC',C'GMP',C'SE'                                              
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
ABCTABD  DSECT                                                                  
ABCTREP  DS    CL3                 NON-DARE REP TO MATCH AGAINST SVREP          
ABCTRAGY DS    CL3                 AGENCY ROUTING CODE                          
ABCTOFF  DS    CL2                 OFFICE IN ROUTING CODE                       
ABCTABL  EQU   *-ABCTABD                                                        
T23400   CSECT                                                                  
*                                                                               
RD4R69   OI    SVDARSTA,DARFXNGQ   WE'RE FAXING                                 
*                                                                               
RD4R70   MVC   0(5,R2),=C'FAX: '                                                
         LA    R2,5(R2)                                                         
         CLC   =C'NOR',SVREP                                                    
         BE    RD4R80               MIGHT BE NOR, THEN DEST IS STA              
*                                                                               
RD4R75   TM    4(R3),X'20'         WAS DEST FIELD CHANGED?                      
         BNZ   RD4R78              NO, NOT A PROBLEM                            
         CLC   =C'STA',8(R3)       DID THEY CHANGE DEST TO REP                  
         BNE   RD4R78                                                           
         MVI   GERROR1,INVALID                                                  
         B     NO                  THEN INVALID                                 
*                                                                               
RD4R78   CLI   5(R3),0             OR NOTHING IN DEST FIELD                     
         BNE   RD4R80                                                           
         MVC   8(3,R3),=C'REP'     THEN SHOW THAT IT IS GOING TO REP            
*                                                                               
RD4R80   TM    DAPTFLG1,DPF1NOOF   X'80' DO WE APPEND THE OFFICE?               
         BNO   *+12                YES                                          
         OI    OMBASFLG,OMFNOOFF   NO                                           
         B     RD4R95                                                           
         DROP  R1                                                               
*                                                                               
         SR    R6,R6                                                            
         IC    R6,SVREP+14         GET LENGTH OF USERID                         
         LA    R1,SVREP+15(R6)                                                  
         MVC   0(2,R1),DARROUTE+3                                               
         DROP  R6                                                               
*                                                                               
         LA    R6,KEY              GET DAGYROUT RECORD                          
         XC    KEY,KEY                                                          
         USING AGRKEYD,R6                                                       
         MVI   AGRKSYS,AGRKSYSQ                                                 
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKMEDA,QMED                                                    
         MVC   AGRKAGRT,DARROUTE                                                
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                   
         CLC   KEY(32),KEYSAVE                                                  
         BNE   RD4R95                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 (RF),(R1),=C'GETREC',=C'GENFIL',KEY+36,(R6),DMWORK               
*                                                                               
         LA    R6,AGRFSTEL         POINT TO FIRST ELEMENT                       
         USING AGROVRD,R6                                                       
*                                                                               
RD4R82   CLI   0(R6),0             END OF ELEMENT                               
         BE    RD4R95                                                           
         CLI   0(R6),AGROVRCQ      LOOK FOR OVERRIDE                            
         BE    RD4R90                                                           
*                                                                               
RD4R85   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD4R82                                                           
*                                                                               
RD4R90   CLC   AGROVRCR,SVREP      LOOK FOR REP CODE WE HAVE                    
         BNE   RD4R85              KEEP LOOKING                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,SVREP+14         GET LENGTH OF USERID                         
         LA    R1,SVREP+15(RF)                                                  
         MVC   0(2,R1),AGROVROF    REPLACE OFFICE CODE                          
         DROP  R6                                                               
*                                                                               
RD4R95   CLC   =C'NOR',SVREP                                                    
         BNE   RD4R100                                                          
         OI    SVDARSTA,DARFXNGQ+DARHMKTQ  TREAT AS FAX & HOME MARKET           
         MVC   SVREP+15(L'QSTA),QSTA                                            
         CLI   QSTA+4,C'T'         TV STATION?                                  
         BNE   RD4R100                                                          
         MVI   SVREP+15+4,C' '     THEN WE DON'T NEED THE T IN SVREP            
*                                                                               
RD4R100  MVC   0(10,R2),SVREP+15                                                
*                                                                               
         CLC   =C'FAX: ',8(R5)     FAX REP?                                     
         BNE   RD4RX                                                            
         OI    SVDARSTA,DARFXNGQ   WE'RE GOING TO FAX                           
         TM    SVDARSTA,DARHMKTQ   HOME MARKET STA?                             
         BNZ   RD4R200                                                          
*********                                                                       
* IN THE CASE WHERE THE USER WANTS REP/FAX USING DESTOV                         
*********                                                                       
         TM    OMBASFLG,OMFPRVOR   DON'T CHANGE TO INBOX EVER?                  
         BNZ   RD4R130             NEVER                                        
***********************************                                             
* LOOKING FOR SPECIFIC OFFICE(S) OF INTERREP REPS WILL NOT BE FAXED             
***********************************                                             
         CLI   QMED,C'R'           RADIO REP?                                   
         BNE   RD4R105             NO, CHECK THE TV REPS                        
***************                                                                 
* INTEREP IS NO LONGER EDI-ABLE.  WHO KNOWS IF THEY'RE GOING TO BE              
*   XML-ABLE LIKE KATZ VIA SPOT-BY-SPOT                                         
***************                                                                 
         B     RD4R130                                                          
***********************************                                             
* LOOKING FOR SPECIFIC OFFICE(S) OF ADAM YOUNG WILL NOT BE FAXED                
***********************************                                             
RD4R105  CLC   =C'SB2AY',0(R2)                                                  
         BNE   RD4R130                                                          
         LA    RE,SB2AYOFF         ADAM YOUNG DARE-ABLE OFFICES                 
RD4R110  CLI   0(RE),0             EOT?                                         
         BE    RD4R130             NOT DARE-ABLE                                
         CLC   0(L'SB2AYOFF,RE),5(R2)                                           
         BE    RD4R120                                                          
         AHI   RE,L'SB2AYOFF                                                    
         B     RD4R110                                                          
*                                                                               
RD4R120  MVC   8(10,R5),0(R2)      DON'T WANT   'FAX: ' ON ROUTE                
         ZIC   R1,0(R5)                                                         
         SHI   R1,8+10+1                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    18(0,R5),18(R5)     CLEAR EVERYTHING AFTER THE FAX               
         LA    R2,8(R5)                                                         
         NI    SVDARSTA,X'FF'-DARFXNGQ   NOT FAXING ANYMORE                     
         B     RD4RX                                                            
*                                                                               
SB2AYOFF DS    0CL2                ADAM YOUNG OFFICES THAT WILL NOT             
         DC    C'BO'                  FAXED                                     
         DC    C'DA'                                                            
         DC    C'MN'                                                            
         DC    C'CH'                                                            
         DC    C'LA'                                                            
         DC    C'AT'                                                            
         DC    C'CL'                 CHARLOTTE                                  
         DC    C'NY'                                                            
         DC    X'00'                                                            
*                                                                               
RD4R130  ZIC   R1,0(R5)              CLEAR EVERYTHING AFTER THE FAX             
         SHI   R1,8+5+1            LESS THE FLDHDR, FAX: , AND EX               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)        CLEAR EVERYTHING AFTER THE FAX              
         MVC   0(10,R2),SVREP+3      SHOW NAME, NOT USER ID PREFIX              
*                                                                               
         L     R6,AIO3                                                          
         OC    0(2,R6),0(R6)       ANY PREVIOUS LAST METHOD?                    
         BZ    RD4R140             NONE                                         
         LA    R6,24(R6)                                                        
         USING DMTHELD,R6                                                       
         CLI   DMTHDEST,C'S'                                                    
         BE    INVLLMTH            CLEAR LSTMTHD IN AIO3 (NOT ERROR!!!)         
*                                                                               
         CLI   RECNUM,RECDSTOV     IN DESTOV/MAINT?                             
         BE    RD4R140             THEN DON'T CARE ABOUT KEEP FAX               
         TM    DMTHFLG1,DMF1KFAX   LAST METHOD SAYS TO KEEP FAX                 
         BNZ   RD4R150             YES, DON'T LOOK FOR BDE                      
*                                                                               
RD4R140  DS    0H                  NO MORE BDE CODE                             
*                                                                               
RD4R150  L     R1,AIO3                                                          
         L     R1,AIO3                                                          
         OC    0(2,R1),0(R1)       ANY LAST METHOD?                             
         BZ    RD4R160             NONE                                         
         LA    R1,24(R1)                                                        
         XR    R0,R0                                                            
RD4R153  CLI   0(R1),0                                                          
         BE    RD4R160                                                          
         CLI   0(R1),DMFXELQ       X'10' - TEMPORARY FAX NUMBER                 
         BE    RD4R156                                                          
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RD4R153                                                          
****                                                                            
         USING DMFXELD,R1                                                       
RD4R156  CLC   DMFXDATE,JDTTODAY   IS IT ACTIVE TODAY?                          
         BNE   RD4R160             NO                                           
*                                                                               
         LA    RE,L'DMFXOVRD-1     RE = NUMBER OF BYTES(FAX #)-1                
         LA    RF,DMFXOVRD+L'DMFXOVRD-1                                         
RD4R156A CLI   0(RF),C' '                                                       
         BH    RD4R157                                                          
         BCTR  RF,0                                                             
         BCT   RE,RD4R156A         WILL MOVE AT LEAST 1 BYTE                    
****                                                                            
RD4R157  DS    0H                                                               
         LR    R0,RE                                                            
         ZIC   RE,0(R5)                                                         
****     SHI   RE,8+18+1                                                        
         SHI   RE,8+16+1                                                        
*                                                                               
         CR    RE,R0                                                            
         BL    RD4R159             FAX NUMBER WILL FIT ON FIELD                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    11(0,R2),11(R2)                                                  
         LR    RE,R0                                                            
RD4R159  EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   11(0,R2),DMFXOVRD   SHOW TEMPORARY FAX NUMBER                    
         B     RD4RX                                                            
         DROP  R1                                                               
*                                                                               
RD4R160  XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTFXREC,R6                                                       
         MVI   CTFXKTYP,CTFXEQU    X'09'                                        
         MVC   CTFXAGY,=C'D7'      DARADM  ID FOR DARE FAX NUMBERS              
         MVC   CTFXCODE,SVREP+15   FAX CODE                                     
         OC    CTFXCODE,=7X'40'    BLANK FILL                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO                       
         L     R6,AIO                                                           
         CLC   KEY(25),0(R6)       SAME FAX ID?                                 
         BNE   RD4RE2              NO DARADM RECORD FOR THIS REP                
*                                                                               
         LA    R6,CTFXEL1                                                       
         USING CTFX1EL,R6                                                       
         CLI   0(R6),CTFX1ELQ                                                   
         BNE   RD4RX                                                            
         XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         SHI   R0,2+1              ELCODE, LENGTH AND 1 FOR EX                  
*                                                                               
         ZIC   RE,0(R5)                                                         
         SHI   RE,8+15+1                                                        
*                                                                               
         CR    RE,R0                                                            
         BL    RD4R165             FAX NUMBER WILL FIT ON FIELD                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    10(0,R2),10(R2)     DOWIG GETS FROM RECORD SO 10(R2) IS          
         LR    RE,R0                 DARADM, 11(R2) IS LSTM OVRD                
RD4R165  EX    RE,*+8                                                           
         B     RD4RX                                                            
         MVC   10(0,R2),CTFX1NUM                                                
         DROP  R6                                                               
***********************************                                             
* COULDN'T FIND DSTA RECORD, SO DEST IS TO THE STATION                          
* LET'S SEE IF WE HAVE BDE RECORD FOR THIS STATION                              
***********************************                                             
RD4R200  CLI   5(R3),0             NOTHING IN DEST FIELD                        
         BE    RD4R201                                                          
         CLI   QMED,C'R'           DESTINE REC WILL DETERMINE REP/STA           
         BE    RD4R202               FOR RADIO MEDIA                            
         TM    4(R3),X'20'         IF NOT CHANGED                               
         BZ    RD4R202                                                          
RD4R201  MVC   8(3,R3),=C'STA'     THEN SHOW THAT IT IS GOING TO STA            
*                                                                               
RD4R202  OI    SVDARSTA,DARFXNGQ   WE'RE GOING TO FAX                           
         LA    R2,8(R5)                                                         
         MVC   0(5,R2),=C'FAX: '                                                
         MVC   5(L'QSTA,R2),QSTA                                                
         MVC   SVREP+15(L'QSTA),QSTA                                            
         CLI   QSTA+4,C'T'         TV STATION?                                  
         BNE   *+8                                                              
         MVI   SVREP+15+4,C' '     THEN WE DON'T NEED THE T IN SVREP            
*                                                                               
RD4R205  L     R6,AIO3                                                          
         OC    0(2,R6),0(R6)       ANY PREVIOUS LAST METHOD?                    
         BZ    RD4R210             NONE                                         
         LA    R6,24(R6)                                                        
         USING DMTHELD,R6                                                       
         CLI   DMTHDEST,C'R'       LAST METHOD WAS TO A REP?                    
         BNE   RD4R207                                                          
         TM    OMBASFLG,OMFFXRAD   FAX ALL RADIO ORDERS?                        
         BZ    INVLLMTH            CLEAR LSTMTHD IN AIO3 (NOT ERROR!!!)         
*                                                                               
RD4R207  CLI   RECNUM,RECDSTOV     IN DESTOV/MAINT?                             
         BE    RD4R210             THEN DON'T CARE ABOUT KEEP FAX               
         TM    DMTHFLG1,DMF1KFAX   LAST METHOD SAYS TO KEEP FAX                 
         BNZ   RD4R300             YES, DON'T LOOK FOR BDE                      
         DROP  R6                                                               
*                                                                               
RD4R210  DS    0H                                                               
***********************************                                             
* COULDN'T FIND DSTA RECORD, LET'S SEE IF WE HAVE A DESTN RECORD FOR            
*   THIS STATION                                                                
***********************************                                             
RD4R300  L     R1,AIO3                                                          
         OC    0(2,R1),0(R1)       ANY LAST METHOD?                             
         BZ    RD4R320             NONE                                         
         LA    R1,24(R1)                                                        
         XR    R0,R0                                                            
RD4R303  CLI   0(R1),0                                                          
         BE    RD4R315                                                          
         CLI   0(R1),DMFXELQ       X'10' - TEMPORARY FAX NUMBER                 
         BE    RD4R306                                                          
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RD4R303                                                          
*                                                                               
         USING DMFXELD,R1                                                       
RD4R306  CLC   DMFXDATE,JDTTODAY   IS IT ACTIVE TODAY?                          
         BNE   RD4R315             NO                                           
*                                                                               
         LA    RE,L'DMFXOVRD-1     RE= NUMBER OF BYTES(FAX #) -1                
         LA    RF,DMFXOVRD+L'DMFXOVRD-1                                         
RD4R306A CLI   0(RF),C' '                                                       
         BH    RD4R307                                                          
         BCTR  RF,0                                                             
         BCT   RE,RD4R306A                                                      
*                                                                               
RD4R307  LR    R0,RE                                                            
         ZIC   RE,0(R5)                                                         
         SHI   RE,8+11+1                                                        
         CR    RE,R0                                                            
         BL    RD4R310                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    11(0,R2),11(R2)                                                  
         LR    RE,R0                                                            
RD4R310  EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   11(0,R2),DMFXOVRD   SHOW TEMPORARY FAX NUMBER                    
*                                                                               
         L     R1,AIO3                                                          
         AHI   R1,24                                                            
         USING DMTHELD,R1                                                       
         CLI   DMTHDEST,C'S'                                                    
         BE    RD4RX               EXIT IF STATION                              
         MVC   8(3,R3),=C'REP'                                                  
*        XC    11(12,R2),11(R2)    WHY DID WE CLEAR AFTER WE MOVE IT IN         
         B     RD4R320                                                          
         DROP  R1                                                               
*                                                                               
RD4R315  L     R1,AIO3                                                          
         XC    0(2,R1),0(R1)       INVALIDATE LSTMTHD SO WE KNOW THAT           
*                                  WE ARE NOT USING THAT NUMBER                 
RD4R320  BRAS  RE,RD4DESTN                                                      
         OC    GERROR,GERROR                                                    
         BNZ   RD4RE1A                                                          
*                                                                               
RD4RX    TM    OMBASFLG,OMFPRVOR                                                
         BNO   RD4RX5                                                           
         CLC   =C'EML: ',8(R5)                                                  
         BE    RD4RX5                                                           
         CLC   =C'FAX: ',8(R5)                                                  
         BNE   RD4RE3                                                           
*                                                                               
RD4RX5   MVC   AIO,AIO1            RESTORE IO AREA                              
*                                                                               
         XC    DMCB,DMCB                                                        
         TM    OMBASFLG,OMFNOOFF   REP APPENDS OFFICE CODE?                     
         BZ    RD4RX6                                                           
         OI    DMCB,X'80'                                                       
*                                                                               
RD4RX6   OC    GERROR,GERROR       TEST ERROR HAS OCCURRED                      
         B     XIT                 AND  RETURN                                  
******   GOTO1 MYERR                                                            
*                                                                               
RD4RE1A  TM    OMBASFL2,OM2DSTOV   COMING FROM DESTOV                           
         BZ    RD4RE1X             NO                                           
         XC    GERROR,GERROR                                                    
RD4RE1X  LR    R2,R5                POINT TO THE FIELD IN QUESTION              
         B     RD4RX5                                                           
*                                                                               
RD4RE2   MVC   GERROR,=AL2(NODRADM) NO DARE FAX RECORD FOR REP                  
         B     RD4RE1A                                                          
RD4RE3   MVC   GERROR,=AL2(NOEMLDOV) ROUTE MUST BE EML OF FAX                   
         B     RD4RE1X                                                          
RD4RE5   MVC   GERROR,=AL2(DARKSTA)  DARK STATION EXISTS                        
         B     RD4RE1A                                                          
         LTORG                                                                  
       ++INCLUDE SPDARJDSRP                                                     
*                                                                               
       ++INCLUDE DDVSREPTAB                                                     
***************                                                                 
* NOTE: DSTA RECORD NEEDS A X'31' ELEMENT FOR THE RECEIVING ID SO THAT          
*       THE DARE DISPATCHER KNOWS THIS IS HARD-CODED                            
***************                                                                 
GETHOVTB BASR  R1,RE                                                            
HOVTAB   DS    0XL(HOVTABL)        LOCAL DIRECT TO STATION BY AGENCY            
         DC    C'QATT',C'SJ',C'NY'                                              
         DC    C'PKUV',C'SJ',C'DA'                                              
         DC    C'PKST',C'SJ',C'DA'                                              
         DC    C'PWAT',C'SJ',C'AT'                                              
         DC    C'PWGN',C'SJ',C'CH'                                              
         DC    C'PKSA',C'SJ',C'DA'                                              
         DC    C'PKDF',C'SJ',C'DA'  ADDED OCT13/06                              
         DC    C'YYML',C'SJ',C'AT'  ADDED FEB12/07                              
         DC    C'YYMO',C'SJ',C'AT'  ADDED 2007APR30                             
         DC    C'YYCN',C'SJ',C'LA'  ADDED 2008JUL03                             
*                                                                               
         DC    C'WCAU',C'RP',C'PH'                                              
*                                                                               
         DC    C'WMAQ',C'H9',C'CH'                                              
         DC    C'WMAQ',C'RP',C'CH'                                              
         DC    C'WMAQ',C'BN',C'CH'                                              
         DC    C'WMAQ',C'H7',C'CH'                                              
         DC    C'WMAQ',C'GZ',C'CH'                                              
*                                                                               
         DC    C'WNBC',C'YN',C'NY'                                              
         DC    C'WNBC',C'H7',C'NY'                                              
         DC    C'WNBC',C'GZ',C'NY'                                              
         DC    C'WNBC',C'MC',C'NY'                                              
         DC    C'WNBC',C'TH',C'NY'                                              
         DC    C'WNBC',C'WI',C'NY'                                              
         DC    C'WNBC',C'UB',C'NY'                                              
*                                                                               
**HW**   DC    C'WGN ',C'GZ',C'CH' LIVE FOR JUN19/06, REMOVED JUN25/07          
*                                                                               
         DC    C'WATL',C'GZ',C'AT'                                              
         DC    C'WATL',C'FR',C'AT'   ADDED 2009MAY26                            
         DC    C'WATL',C'H7',C'NY'   ADDED 2009JUN01                            
*                                                                               
         DC    C'WXIA',C'FR',C'AT'   ADDED 2009MAY26                            
         DC    C'WXIA',C'H7',C'NY'   ADDED 2009JUN01                            
*                                                                               
         DC    C'KUVN',C'GZ',C'DA'                                              
         DC    C'KUVN',C'H7',C'DA'                                              
         DC    C'KUVN',C'TR',C'DA'                                              
         DC    C'KUVN',C'UB',C'DA'                                              
         DC    C'KUVN',C'OO',C'DA'                                              
         DC    C'KUVN',C'YN',C'DA'   SET FOR 2007-07-16                         
         DC    C'KUVN',C'FR',C'DA'   SET FOR 2007-07-23                         
         DC    C'KUVN',C'M2',C'DA'   SET FOR 2008-09-30                         
*                                                                               
         DC    C'KSTR',C'GZ',C'DA'                                              
         DC    C'KSTR',C'H7',C'DA'                                              
         DC    C'KSTR',C'TR',C'DA'                                              
         DC    C'KSTR',C'UB',C'DA'                                              
         DC    C'KSTR',C'OO',C'DA'                                              
         DC    C'KSTR',C'YN',C'DA'   SET FOR 2007-07-16                         
         DC    C'KSTR',C'FR',C'DA'   SET FOR 2007-07-23                         
         DC    C'KSTR',C'M2',C'DA'   SET FOR 2008-09-30                         
*                                                                               
         DC    C'KSAT',C'GZ',C'DA'                                              
         DC    C'KSAT',C'MC',C'DA'                                              
         DC    C'KSAT',C'YN',C'DA'                                              
         DC    C'KSAT',C'H7',C'DA'                                              
         DC    C'KSAT',C'RP',C'DA'                                              
         DC    C'KSAT',C'QU',C'DA'                                              
         DC    C'KSAT',C'G7',C'DA'                                              
         DC    C'KSAT',C'TR',C'DA'                                              
         DC    C'KSAT',C'UB',C'DA'                                              
         DC    C'KSAT',C'FR',C'DA'   SET FOR 2007-07-23                         
         DC    C'KSAT',C'B$',C'KC'   SET FOR 2007-10-02                         
         DC    C'KSAT',C'PC',C'DA'   SET FOR 2008-02-25                         
*                                                                               
         DC    C'NSAT',C'GZ',C'DA'                                              
         DC    C'NSAT',C'MC',C'DA'                                              
         DC    C'NSAT',C'YN',C'DA'                                              
         DC    C'NSAT',C'H7',C'DA'                                              
         DC    C'NSAT',C'RP',C'DA'                                              
         DC    C'NSAT',C'QU',C'DA'                                              
         DC    C'NSAT',C'G7',C'DA'                                              
         DC    C'NSAT',C'TR',C'DA'                                              
         DC    C'NSAT',C'UB',C'DA'                                              
         DC    C'NSAT',C'FR',C'DA'   SET FOR 2007-07-23                         
         DC    C'NSAT',C'B$',C'KC'   SET FOR 2007-10-02                         
         DC    C'NSAT',C'PC',C'DA'   SET FOR 2008-02-25                         
*                                                                               
         DC    C'RSAT',C'GZ',C'DA'                                              
         DC    C'RSAT',C'MC',C'DA'                                              
         DC    C'RSAT',C'YN',C'DA'                                              
         DC    C'RSAT',C'H7',C'DA'                                              
         DC    C'RSAT',C'RP',C'DA'                                              
         DC    C'RSAT',C'QU',C'DA'                                              
         DC    C'RSAT',C'G7',C'DA'                                              
         DC    C'RSAT',C'TR',C'DA'                                              
         DC    C'RSAT',C'UB',C'DA'                                              
         DC    C'RSAT',C'FR',C'DA'   SET FOR 2007-07-23                         
         DC    C'RSAT',C'B$',C'KC'   SET FOR 2007-10-02                         
         DC    C'RSAT',C'PC',C'DA'   SET FOR 2008-02-25                         
         DC    C'RSAT',C'M2',C'DA'   SET FOR 2008-07-09                         
         DC    C'RSAT',C'H7',C'NY'   SET FOR 2008-07-25                         
*                                                                               
         DC    C'KDFI',C'GZ',C'DA'   SET FOR 2006-11-27                         
         DC    C'KDFI',C'H7',C'DA'   SET FOR 2007-09-11                         
         DC    C'KDFI',C'FR',C'DA'   SET FOR 2008-01-22                         
         DC    C'KDFI',C'M2',C'DA'   SET FOR 2008-07-08                         
         DC    C'KDFI',C'YN',C'DA'   SET FOR 2008-08-08                         
         DC    C'KDFI',C'H7',C'NY'   SET FOR 2009-03-04                         
         DC    C'KDFI',C'GZ',C'CH'   SET FOR 2009-07-17                         
*                                                                               
         DC    C'KDFW',C'GZ',C'DA'   SET FOR 2006-11-27                         
         DC    C'KDFW',C'H7',C'DA'   SET FOR 2007-09-11                         
         DC    C'KDFW',C'FR',C'DA'   SET FOR 2008-01-22                         
         DC    C'KDFW',C'H7',C'DA'   SET FOR 2007-09-11                         
         DC    C'KDFW',C'FR',C'DA'   SET FOR 2008-01-22                         
         DC    C'KDFW',C'M2',C'DA'   SET FOR 2008-07-08                         
         DC    C'KDFW',C'YN',C'DA'   SET FOR 2008-07-08                         
         DC    C'KDFW',C'GZ',C'CH'   SET FOR 2009-01-16                         
         DC    C'KDFW',C'H7',C'NY'   SET FOR 2009-03-04                         
*                                                                               
         DC    C'WGCL',C'GZ',C'AT'   SET FOR 2007-02-12                         
         DC    C'WGCL',C'GZ',C'CH'   SET FOR 2008-12-18                         
         DC    C'WGCL',C'YN',C'AT'   SET FOR 2008-12-18                         
         DC    C'WGCL',C'H7',C'AT'   SET FOR 2008-12-18                         
         DC    C'WGCL',C'M2',C'AT'   SET FOR 2008-12-18                         
         DC    C'WGCL',C'FR',C'AT'   SET FOR 2008-12-18                         
*                                                                               
         DC    C'WUPA',C'GZ',C'AT'   SET FOR 2007-05-14                         
         DC    C'WUPA',C'RP',C'AT'   SET FOR 2007-07-16                         
         DC    C'WUPA',C'M2',C'AT'   SET FOR 2008-06-30                         
         DC    C'WUPA',C'YN',C'AT'   SET FOR 2008-06-30                         
         DC    C'WUPA',C'H7',C'AT'   SET FOR 2008-06-30                         
         DC    C'WUPA',C'FR',C'AT'   SET FOR 2008-06-30                         
         DC    C'WUPA',C'UB',C'AT'   SET FOR 2008-09-11                         
         DC    C'WUPA',C'H7',C'NY'   SET FOR 2009-01-23                         
         DC    C'WUPA',C'WI',C'AT'   SET FOR 2009-05-11                         
*                                                                               
         DC    C'KOBI',C'BN',C'LA'   SET FOR 2008-07-07                         
         DC    C'KOBI',C'YN',C'SF'   SET FOR 2008-07-07                         
         DC    C'KOBI',C'M2',C'SF'   SET FOR 2008-07-07                         
         DC    C'KOBI',C'H7',C'SF'   SET FOR 2008-07-07                         
         DC    C'KOBI',C'FR',C'SF'   SET FOR 2008-07-07                         
*                                                                               
         DC    C'KGO ',C'GZ',C'LA'   SET FOR 2008-02-25                         
*                                                                               
         DC    C'KCNC',C'OO',C'SE'   SET FOR 2008-09-17                         
                                                                                
         DC    C'KPRC',C'RP',C'DA'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'GZ',C'CH'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'YN',C'DA'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'H7',C'DA'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'H7',C'NY'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'M2',C'DA'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'FR',C'DA'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'FR',C'NY'   SET FOR 2008-12-08                         
         DC    C'KPRC',C'OO',C'DA'   SET FOR 2009-03-12                         
         DC    C'KPRC',C'WI',C'AT'   SET FOR 2009-04-24                         
                                                                                
         DC    C'WPCH',C'YN',C'AT'   SET FOR 2008-12-22                         
         DC    C'WPCH',C'H7',C'AT'   SET FOR 2008-12-22                         
         DC    C'WPCH',C'M2',C'AT'   SET FOR 2008-12-22                         
         DC    C'WPCH',C'FR',C'AT'   SET FOR 2008-12-22                         
                                                                                
         DC    C'WNYW',C'YN',C'NY'   SET FOR 2009-01-07                         
         DC    C'WNYW',C'H7',C'NY'   SET FOR 2009-01-07                         
         DC    C'WNYW',C'M2',C'NY'   SET FOR 2009-01-07                         
         DC    C'WNYW',C'FR',C'NY'   SET FOR 2009-01-07                         
                                                                                
         DC    C'WWOR',C'YN',C'NY'   SET FOR 2009-01-07                         
         DC    C'WWOR',C'H7',C'NY'   SET FOR 2009-01-07                         
         DC    C'WWOR',C'M2',C'NY'   SET FOR 2009-01-07                         
         DC    C'WWOR',C'FR',C'NY'   SET FOR 2009-01-07                         
                                                                                
         DC    C'WAGA',C'YN',C'AT'   SET FOR 2009-01-07                         
         DC    C'WAGA',C'H7',C'AT'   SET FOR 2009-01-07                         
         DC    C'WAGA',C'M2',C'AT'   SET FOR 2009-01-07                         
         DC    C'WAGA',C'FR',C'AT'   SET FOR 2009-01-07                         
         DC    C'WAGA',C'H7',C'NY'   SET FOR 2009-01-23                         
                                                                                
         DC    C'WFXT',C'YN',C'BO'   SET FOR 2009-01-07                         
         DC    C'WFXT',C'H7',C'BO'   SET FOR 2009-01-07                         
         DC    C'WFXT',C'M2',C'BO'   SET FOR 2009-01-07                         
         DC    C'WFXT',C'FR',C'BO'   SET FOR 2009-01-07                         
         DC    C'WFXT',C'H7',C'NY'   SET FOR 2009-01-16                         
                                                                                
         DC    C'WJBK',C'YN',C'DE'   SET FOR 2009-01-07                         
         DC    C'WJBK',C'H7',C'DE'   SET FOR 2009-01-07                         
         DC    C'WJBK',C'M2',C'DE'   SET FOR 2009-01-07                         
         DC    C'WJBK',C'FR',C'DE'   SET FOR 2009-01-07                         
                                                                                
         DC    C'KLSR',C'FR',C'SF'   SET FOR 2009-01-09                         
                                                                                
         DC    C'KEVU',C'FR',C'SF'   SET FOR 2009-01-09                         
                                                                                
         DC    C'WFLD',C'YN',C'CH'   SET FOR 2009-01-14                         
         DC    C'WFLD',C'H7',C'CH'   SET FOR 2009-01-14                         
         DC    C'WFLD',C'M2',C'CH'   SET FOR 2009-01-14                         
         DC    C'WFLD',C'FR',C'CH'   SET FOR 2009-01-14                         
                                                                                
         DC    C'WPWR',C'YN',C'CH'   SET FOR 2009-01-14                         
         DC    C'WPWR',C'H7',C'CH'   SET FOR 2009-01-14                         
         DC    C'WPWR',C'M2',C'CH'   SET FOR 2009-01-14                         
         DC    C'WPWR',C'FR',C'CH'   SET FOR 2009-01-14                         
                                                                                
         DC    C'KTTV',C'YN',C'LA'   SET FOR 2009-01-14                         
         DC    C'KTTV',C'H7',C'LA'   SET FOR 2009-01-14                         
         DC    C'KTTV',C'M2',C'LA'   SET FOR 2009-01-14                         
         DC    C'KTTV',C'FR',C'LA'   SET FOR 2009-01-14                         
                                                                                
         DC    C'KCOP',C'YN',C'LA'   SET FOR 2009-01-14                         
         DC    C'KCOP',C'H7',C'LA'   SET FOR 2009-01-14                         
         DC    C'KCOP',C'M2',C'LA'   SET FOR 2009-01-14                         
         DC    C'KCOP',C'FR',C'LA'   SET FOR 2009-01-14                         
                                                                                
         DC    C'WDIV',C'GZ',C'CH'   SET FOR 2009-03-26                         
         DC    C'WDIV',C'FR',C'DE'   SET FOR 2009-03-26                         
         DC    C'WDIV',C'H7',C'DE'   SET FOR 2009-03-26                         
         DC    C'WDIV',C'BN',C'DE'   SET FOR 2009-03-26                         
         DC    C'WDIV',C'MC',C'DE'   SET FOR 2009-03-26                         
         DC    C'WDIV',C'WI',C'CH'   SET FOR 2009-03-26                         
                                                                                
         DC    C'WCVB',C'FR',C'BO'   SET FOR 2009-04-06                         
         DC    C'WCVB',C'H7',C'NY'   SET FOR 2009-04-06                         
                                                                                
         DC    X'FF'                                                            
*                                                                               
HOVTABD  DSECT                                                                  
HOVTSTA  DS    CL4                 STATION CODE                                 
HOVTAGY  DS    CL2                 AGENCY POWER CODE                            
HOVTOFF  DS    CL2                 AGENCY OFFICE                                
HOVTABL  EQU   *-HOVTABD                                                        
T23400   CSECT                                                                  
***************                                                                 
         EJECT                                                                  
***********************************************************************         
* READ FOR THE DESTINE RECORD                                                   
***********************************************************************         
RD4DESTN NTR1  BASE=*,LABEL=*                                                   
         LA    R6,KEY                                                           
         XC    KEY,KEY             GET THE DESTINE RECORD                       
         USING DSRKEY,R6                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D'                                        
         MVI   DSRKSBTY,DSRKSBTQ   X'3D'                                        
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
         MVC   DSRKCLT,BCLT         CLIENT LEVEL?                               
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE                                            
         BE    RD4R350                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DSRKEY,R6                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D'                                        
         MVI   DSRKSBTY,DSRKSBTQ   X'3D'                                        
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
         MVI   DSRKCLT,C'*'        OFFICE                                       
         MVC   DSRKCLT+1(1),CLTOFFCE                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE                                            
         BE    RD4R350                                                          
*                                                                               
         CLI   PFFXUOFL,C'Y'       USES OFFICE LIST?                            
         BNE   RD4R340                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DSRKEY,R6                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D'                                        
         MVI   DSRKSBTY,DSRKSBTQ   X'3D'                                        
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
         MVI   DSRKCLT,C'$'        OFFICE LIST                                  
         MVC   DSRKCLT+1(1),PFFXOLCD   OFFICE LIST CODE FROM PROFILE            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE                                            
         BE    RD4R350                                                          
*                                                                               
RD4R340  XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DSRKEY,R6                                                        
         MVI   DSRKTYPE,DSRKTYPQ   X'0D'                                        
         MVI   DSRKSBTY,DSRKSBTQ   X'3D'                                        
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKSTA,BSTA                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DSRKEY),KEYSAVE  DID WE FIND THE RECORD?                   
         BE    RD4R350                YES                                       
         OC    0(2,R1),0(R1)       NO, ARE WE USING ANY LAST METHOD?            
         BZ    RD4RE1                                                           
         B     RD4DSTX                                                          
*                                                                               
RD4R350  MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R1,AIO3                                                          
         OC    0(2,R1),0(R1)       ARE WE USING ANY LAST METHOD?                
         BNZ   RD4R365             YES                                          
*                                                                               
RD4R352  L     R6,AIO                                                           
         USING DSRKEY,R6                                                        
         LA    R6,DSRFRST                                                       
         USING DSRIDELD,R6                                                      
         ZIC   R1,0(R5)                                                         
         SHI   R1,8+13+1                                                        
         CHI   R1,L'DSRIDFAX-1                                                  
         BL    RD4R360                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    13(0,R2),13(R2)                                                  
         LA    R1,L'DSRIDFAX-1                                                  
RD4R360  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   13(0,R2),DSRIDFAX                                                
*                                                                               
* READ THE RADIO DESTINATION AND ROUTE ELEMENT                                  
*                                                                               
RD4R365  L     R6,AIO                                                           
         MVI   ELCODE,DSRDRELQ     X'20'                                        
         BAS   RE,GETEL                                                         
         BNE   RD4DSTX                                                          
         USING DSRDRELD,R6                                                      
*                                                                               
         TM    OMBASFLG,OMFFXRAD   FAX ALL RADIO ORDERS?                        
         BNZ   RD4R370             YES, SPECIAL RULE FOR DESTINE RECS           
*                                                                               
         CLI   DSRDRDES,DSRDRREP   FAX # FOR REP?                               
         BNE   RD4DSTX                                                          
         XC    13(20,R5),13(R5)    CLEAR STATION/FAX# DUE TO ERROR              
         B     RD4RE4                                                           
*                                                                               
RD4R370  MVC   8(3,R3),=C'STA'       DEFAULT TO STA                             
         CLI   DSRDRDES,DSRDRREP   FAX # FOR REP?                               
         BNE   RD4DSTX                                                          
         MVC   8(3,R3),=C'REP'     YES, MOVE IN REP                             
*                                                                               
         XC    13(5,R5),13(R5)                                                  
         MVC   13(3,R5),DSRDRROU                                                
         B     RD4DSTX                                                          
         DROP  R6                                                               
RD4RE1   MVC   GERROR,=AL2(NODESTN) NO DESTINE RECORD!                          
         B     RD4DSTX                                                          
*                                                                               
RD4RE4   MVC   GERROR,=AL2(NODSTDOV) DESTINE IS FOR REP, USE DESTOV             
RD4DSTX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY INITIALIZATION                                                          
*                                                                               
* ON ENTRY:    PARAM 1             A(PFKEY VAL. TABLE) OR ZEROS                 
***********************************************************************         
VINIT    NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,CHNGSEL          CHANGE ANY 'S' TO 'C' AS PER MEL             
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
         B     DUMMYERR            GO AGAIN TO GENCON                           
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
         CLI   RECNUM,RECORDR      IF ORDER RECORD                              
         BE    CSELX               THEN DON'T CHANGE THE SELECT CODE            
         CLI   RECNUM,RECMRKT      IF BATCH ORDER RECORD                        
         BE    CSELX               THEN DON'T CHANGE THE SELECT CODE            
         CLI   RECNUM,RECLSMTH     IF LAST METHOD RECORD                        
         BE    CSELX               THEN DON'T CHANGE THE SELECT CODE            
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
*                                                                               
         OI    8(R2),C' '                                                       
         TM    GENSTAT5,SEL1BYTE                                                
         BNZ   *+10                                                             
         OC    8(3,R2),SPACES                                                   
*                                                                               
         ZIC   R1,5(R2)            R1=L(SELECT INPUT)                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SEL'     MATCH ON EXACT SELECT CODE                   
         BNE   CSEL6                                                            
         TM    GENSTAT5,SEL1BYTE                                                
         BNZ   *+10                                                             
         XC    8(3,R2),8(R2)       CHANGE SELECT TO A SELECT FOR CHANGE         
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
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                              
*                                                                               
* ON ENTRY:    R3                  A(PFKEY TABLE)                               
***********************************************************************         
TESTSEL  NTR1                                                                   
         TM    CTLRFLG1,CF1CKLST   ON=CHECK EVEN IF NOT ACTION LIST             
         BNO   TSEL1                                                            
         NI    CTLRFLG1,X'FF'-CF1CKLST   YES, RESET FOR NEXT TIME               
         B     TSEL2                                                            
*                                                                               
TSEL1    CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
TSEL2    TM    CTLRFLG1,CF1TSELQ         DON'T TEST SEL FIELDS?                 
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1TSELQ   YES, RESET FOR NEXT TIME               
         B     TSELX                                                            
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
TSEL3    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
*                                                                               
         OI    8(R2),C' '                                                       
         TM    GENSTAT5,SEL1BYTE                                                
         BNZ   *+10                                                             
         OC    8(3,R2),SPACES                                                   
*                                                                               
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         XR    RE,RE                                                            
         TM    GENSTAT5,SEL1BYTE                                                
         BNZ   *+8                                                              
         LA    RE,2                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),PFTSEL      MATCH ON EXACT SELECT CODE                   
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
         BNE   TSEL3               SELECT FIELD                                 
         B     TSEL6                                                            
*                                                                               
TSEL8    DS    0H                                                               
         CLI   PFTAID,24           DON'T CLEAR THE LINE                         
         BH    TSEL10                                                           
         MVI   8(R2),C' '                                                       
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
TSEL10   LR    RE,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    RE,RA                                                            
         STH   RE,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT                                                              
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
         XC    DMCB+20(4),DMCB+20  USE DEFAULT LEN FOR DMGR & TEMPSTR           
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(SYSDEND-STARTSV)                                   
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
         XC    DMCB+20(4),DMCB+20  USE DEFAULT LEN FOR DMGR & TEMPSTR           
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
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(SYSDEND-STARTSV)                                   
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
RPROG10  CLI   0(R2),0                                                          
         BE    RPROG20             FIND END OF TWA                              
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     RPROG10                                                          
RPROG20  MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
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
         LTORG                                                                  
***********************************************************************         
* SEE IF WE HAVE TO CALL     ORDER/SEND  OR   ORDER/MG                          
***********************************************************************         
CALLDARE NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**CDAR**'                                                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    CDAREX                                                           
         MVI   GOTGLOB,0           NOT COMING FROM GLOBBER                      
*                                                                               
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CGLOBBER-COMFACSD(R1)                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,24,GLVXCTL   ANY XCTL ELEM?             
         CLI   8(R1),GLEGNF                                                     
         BE    CDAREX                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T HANDLE OTHER ERRORS                    
*                                                                               
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL     REMOVE IT                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T HANDLE OTHER ERRORS                    
*                                                                               
         LA    R1,BLOCK            CURRENTLY ONLY BUY & NWS CAN                 
         USING GLVXFRSY,R1           CALL THE DARE PROGRAM                      
         CLC   GLVXFRSY,=C'SPO'                                                 
         BNE   CDAREX                                                           
         CLC   GLVXFRPR,=C'BUY'                                                 
         BE    CDARE00                                                          
         CLC   GLVXFRPR,=C'NWS'                                                 
         BNE   CDAREX                                                           
*                                                                               
CDARE00  CLC   GLVXTOSY,=C'SPO'                                                 
         BNE   CDAREX                                                           
         CLC   GLVXTOPR,=C'DAR'                                                 
         BE    *+14                                                             
         CLC   GLVXTOPR,=C'OM '                                                 
         BNE   CDAREX                                                           
*                                                                               
         LA    RE,GGLBBUY          CALLED BY BUY PROGRAM                        
         CLC   GLVXFRPR,=C'BUY'                                                 
         BE    *+8                                                              
         LA    RE,GGLBNWS          CALLED BY NWS PROGRAM                        
*                                                                               
         TM    GLVXFLG1,GLV1RETN                                                
         BZ    CDARE05                                                          
         LA    RE,GGLBDAR          COMING BACK FROM THE BUY PROGRAM             
         TM    GLVXFLG1,GLV1SIDE   SESSION WE CAME BACK FROM SET?               
         BZ    CDARE05                                                          
         MVC   TMPSLSID,GLVXSESR   YES, SAVE SELECT SESSIONS CALLER/EE          
CDARE05  EX    RE,*+8                                                           
         BE    *+8                                                              
         OI    GOTGLOB,0                                                        
*                                                                               
         TM    GLVXFLG1,GLV1RETN   RETURN CALL?                                 
         BNZ   CDARE10                                                          
         MVC   TMPXFRID,GLVXSESR   SAVE IN OVERLAY OTHERWISE IT'S GONE          
         DROP  R1                                                               
*                                                                               
CDARE10  MVC   CONREC,=CL8'ORDER'                                               
         NI    CONRECH+4,X'FF'-X'20'     RECORD FIELD CHANGED                   
         MVI   CONRECH+5,8                                                      
         OI    CONRECH+6,X'80'                                                  
         MVC   CONACT,=CL8'OFFER'                                               
*                                                                               
         TM    GOTGLOB,GGLBDAR     TO MG OR LIST                                
         BZ    CDARE15                                                          
         GOTO1 (RF),DMCB,=C'GETD',WORK,3,GLVSPMKG                               
         CLI   8(R1),X'10'         MAKEGOOD GROUP NOT FOUND?                    
         BNE   CDARE50             IT WAS FOUND, SO MG                          
         MVC   CONACT,=CL8'LIST'   NOT FOUND, LIST ACTION                       
         B     CDARE50                                                          
*                                                                               
CDARE15  GOTO1 (RF),DMCB,=C'GETD',WORK,8,GLVSPOPT                               
         CLI   8(R1),X'10'         AUTO-RECALL OPTION?                          
         BE    *+12                                                             
         CLI   WORK,C'R'                                                        
         BE    CDARE19             YES                                          
CDARE18  MVC   CONACT,=CL8'SEND'   NO                                           
         B     CDARE50                                                          
*********                                                                       
* AUTO-RECALL REQUESTED FROM BUY PROGRAM                                        
*********                                                                       
CDARE19  MVC   CONACT,=CL8'RECALL'                                              
         OI    GOTGLOB,GGLBRCL                                                  
*** A-M/CLT/PRD/MKT/STA/EST (10)                                                
*** PRD1/PRD2/FLT  (3) SAVED IN TSTDARE                                         
         GOTO1 (RF),DMCB,=C'GETD',ELEM,13,GLVBUY1                               
         CLI   8(R1),X'10'         RECALL OPTION?                               
         BE    CDARE18             WE SHOULD, JUST GO TO ORDER SEND             
         GOTO1 (RF),DMCB,=C'DELE',,,GLVBUY1     REMOVE IT                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T HANDLE OTHER ERRORS                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD(3),ELEM     MOVE IN AGY/MED  AND CLIENT                  
         MVC   DCKEST,ELEM+BUYKEST-BUYKEY                                       
         MVC   DCKSTA,ELEM+BUYKSTAC-BUYKEY                                      
         MVC   DCKFLTNM,ELEM+12    FLIGHT NUMBER FROM BUY PROGRAM               
         CLI   ELEM+10,0           ANY PRIMARY PRODUCT?                         
         BNE   CDARE19A                                                         
         MVC   DCKPRD,ELEM+11      NO, IN THE SECOND PRODUCT FIELD              
         B     CDARE19C                                                         
*                                                                               
CDARE19A MVC   DCKPRD,ELEM+10                                                   
         MVC   DCKPRD2,ELEM+11                                                  
*                                                                               
CDARE19C TM    WORK+1,X'40'        RECALL FOR TRADE?                            
         BZ    *+8                                                              
         OI    DCKFLAG,DCKFTRDE                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CDATAMGR-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY,0                    
         CLC   KEY(L'DOKEY),KEYSAVE   CAN'T FIND THE ORDER?                     
         BNE   CDARE18                GO TO ORDER SEND INSTEAD                  
*                                                                               
         GOTO1 (RF),DMCB,=C'GETREC',=C'SPTFIL',KEY+14,IO,DMWORK                 
*                                                                               
         LA    R6,IO                                                            
         LA    R6,24(R6)                                                        
         USING DOIDELD,R6                                                       
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CGLOBBER-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,=C'PUTD',DOIDBYR,3,GLVSPBYR                            
         CLI   ELEM+12,0                                                        
         BE    CDARE19D                                                         
         GOTO1 (RF),(R1),=C'PUTD',ELEM+12,1,GLVSPEFL    FLIGHT                  
CDARE19D MVC   ELEM+10(2),DOIDPRD                                               
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         MVC   KEY+1(3),ELEM             MOVE IN AGY/MED  AND CLIENT            
         MVC   KEYSAVE,KEY                                                      
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CDATAMGR-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY,0                    
         CLC   KEY(L'DOKEY),KEYSAVE   CAN'T FIND THE ORDER?                     
         BNE   CDARE18                                                          
*                                                                               
         GOTO1 (RF),DMCB,=C'GETREC',=C'SPTFIL',KEY+14,IO,DMWORK                 
         LA    R6,IO                                                            
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,ELEM+10                                                       
CDARE20  LA    R4,CLIST                                                         
CDARE25  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                CAN'T FIND PRODUCT!!!!                       
         CLC   0(1,R1),3(R4)       MATCH ON PRODUCT CODE?                       
         BE    CDARE30             YES                                          
         LA    R4,4(R4)            NO, SKIP TO NEXT ENTRY IN LIST               
         B     CDARE25                                                          
*                                                                               
CDARE30  LA    R0,ELEM+11                                                       
         CR    R1,R0                                                            
         BE    CDARE35                                                          
         MVC   WORK(3),0(R4)                                                    
         LR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   CDARE20                                                          
         LA    R1,3                                                             
         B     CDARE40                                                          
*                                                                               
CDARE35  MVC   WORK+3(3),0(R4)                                                  
         LA    R1,6                                                             
CDARE40  ST    R1,DMCB+8                                                        
*                                                                               
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CGLOBBER-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,=C'PUTD',WORK,,GLVDRPRD                                
*                                                                               
CDARE50  NI    CONACTH+4,X'FF'-X'20'     ACTION FIELD CHANGED                   
         MVI   CONACTH+5,8                                                      
         OI    CONACTH+6,X'80'                                                  
*                                                                               
CDAREX   B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* THIS SUBROUTINE PROVIDES A LINKAGE FROM OMS09 TO OMS29                        
* ON RETURN, SPOMS09 IS RELOADED AND CONTROL RETURNED                           
*===============================================================                
                                                                                
GETGO29  BASR  R1,RE               RETURN ROUTINE ADDRESS IN R1                 
*                                                                               
GOTO29   NMOD1 0,**GO29**                                                       
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         GOTO1 CALLOV,DMCB,(X'29',0),0                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            GET A(SPOMS29)                               
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
         GOTO1 CALLOV,DMCB,(X'09',0),0      RELOAD 09                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                      AND RETURN TO IT                             
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD                                                     
RECMRKT  EQU   08                  BATCH RECORD                                 
RECORDR  EQU   09                  ORDER RECORD NUMBER EQUATE                   
RECOFFR  EQU   10                  OFFER RECORD NUMBER EQUATE                   
RECDSTOV EQU   11                  DESTOV RECORD NUMBER EQUATE                  
RECLSMTH EQU   13                  LAST METHOD RECORD                           
         EJECT                                                                  
       ++INCLUDE SPOMSDSCTS                                                     
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
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
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDESTN                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMTH                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE GEGENBDE                                                       
         EJECT                                                                  
*PREFIX=CT$                                                                     
       ++INCLUDE CTGENSTAD                                                      
*PREFIX=                                                                        
       ++INCLUDE CTGENAGRD                                                      
         EJECT                                                                  
       ++INCLUDE SPOMSFFD                                                       
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
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE SPSYSFAC                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDGETDARED                                                     
         PRINT ON                                                               
COMPDATD DSECT                                                                  
EDIDATB  DS    XL3                                                              
EDIDATC  DS    CL6                                                              
EFDATEB  DS    XL3                                                              
EFDATEC  DS    CL6                                                              
TDYDATE  DS    CL6                                                              
         SPACE 3                                                                
T234FFD  DSECT                                                                  
         ORG   T234FFD+TWAENDLQ    ORG TO BOTTOM OF TWA0                        
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086SPOMS00   03/14/16'                                      
         END                                                                    
