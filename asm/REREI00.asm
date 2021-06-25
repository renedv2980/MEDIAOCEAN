*          DATA SET REREI00    AT LEVEL 007 AS OF 04/27/07                      
*PHASE T82600A                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE DPTRD                                                                  
*INCLUDE EQVRD                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE TIMPK                                                                  
*INCLUDE TIMUNPK                                                                
*INCLUDE KHDUMMY                                                                
*&&      SET   TT=Y                                                             
         TITLE 'REREI00 - REP NEW INVOICE CONTROLLER'                           
T82600   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T82600,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         L     RA,4(R1)            A(TWA)                                       
         USING T826FFD,RA                                                       
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
         MVC   STAFIL,=C'STATION '                                              
         MVC   INVDIR,=C'XSPDIR  '                                              
         MVC   INVFIL,=C'XSPFIL  '                                              
*                                                                               
MAIN10   OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREAS                          
         XC    CONHED2,CONHED2                                                  
         OI    CONHED2H+6,X'80'                                                 
*                                                                               
         CLI   CONRECH+5,0         NO RECORD INPUTTED?                          
         BNE   *+14                                                             
         MVC   CONREC(7),=CL7'INVOICE'  ALWAYS INVOICE RECORD TYPE              
         MVI   CONRECH+5,7                                                      
*                                                                               
         CLI   CONACTH+5,0         NO ACTION INPUTTED?                          
         BNE   *+14                                                             
         MVC   CONACT(4),=CL4'LIST'    DEFAULT IS TO LIST                       
         MVI   CONACTH+5,4                                                      
*                                                                               
         BAS   RE,VALIACT          VALIDATE ACTION                              
         BNE   XIT                                                              
*                                                                               
         BAS   RE,CHKLOCAL                                                      
*                                                                               
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,RCHANG      THEN SET RCHANG FLAG                         
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    MAIN20                                                           
         OI    TRNSTAT,ACHANG      THEN SET ACHANG FLAG                         
*                                                                               
         CLC   =C'CHA',CONACT      AND IT CHANGED TO 'CHA'                      
         BNE   *+8                                                              
         OI    TRNSTAT,USERCHA     THEN SET USER CAUSED 'CHA' FLAG              
*                                                                               
MAIN20   BAS   RE,GOGENCON         GO OFF TO GENCON                             
*                                                                               
         OI    CONRECH+4,X'20'     VALIDATE RECORD/ACTION FIELDS                
         OI    CONACTH+4,X'20'                                                  
*                                                                               
         TM    MNIOFLAG,X'80'      DO WE NEED TO CLOSE MINIO BUFFER             
         BZ    XIT                                                              
*                                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         GOTO1 MINIO,DMCB,('MINCLS',(R5))    YES                                
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PROGRAM DEPENDENT VALUES *                                         
***********************************************************************         
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
         L     R1,8(R1)            A(COMFACS)                                   
         L     RF,CCALLOV-COMFACSD(R1)                                          
         MVC   DATAMGR,CDATAMGR-COMFACSD(R1)                                    
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
* FIX GETBROAD ADDRESS                                                          
         MVI   DMCB+7,QGETBRD                                                   
         GOTO1 (RF),(R1)                                                        
         MVC   GETBROAD,DMCB                                                    
* FIX RECUP TO USE FACPAK LINKED VERSION                                        
         L     RE,SYSPARMS                                                      
         L     RE,8(RE)            POINT TO FACILITIES LIST                     
         MVC   RECUP,28(RE)                                                     
*                                                                               
         LA    R0,GOMSPACK         REROUTE MSPACK/MSUNPK ROUTINES               
         ST    R0,MSPACK                                                        
         LA    R0,GOMSUNPK                                                      
         ST    R0,MSUNPK                                                        
*                                                                               
         MVI   SYSTEM,C'S'         SPOT                                         
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=Y(L'AGYKEY)   DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=Y(L'AGYCTL)                                             
         MVC   DATADISP,=Y(AGYEL-AGYHDR)                                        
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   ACTELOPT,C'N'       DON'T ADD ACTIVITY ELEMENT                   
         MVI   IOOPT,C'Y'          WE'LL DO OUR OWN IO'S                        
         MVI   GETMSYS,2           USES GETMSG FOR SYSTEM 2 (SPOT)              
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9082600'    PRESET FOR SYSTEM CALLOVS               
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
         LA    R1,RECACT2          OR ALL ACTIONS TYPES                         
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
*                                                                               
**       OI    CONWHENH+1,X'2C'                                                 
**       OI    CONWHENH+6,X'80'                                                 
         LA    R1,CONWHENH                                                      
         ST    R1,EFHWHEN                                                       
         OI    CONOUTH+1,X'2C'                                                  
         OI    CONOUTH+6,X'80'                                                  
         LA    R1,CONOUTH                                                       
         ST    R1,EFHOUT                                                        
         OI    CONDESTH+1,X'2C'                                                 
         OI    CONDESTH+6,X'80'                                                 
         LA    R1,CONDESTH                                                      
         ST    R1,EFHDEST                                                       
         OI    CONOTHH+1,X'2C'                                                  
         OI    CONOTHH+6,X'80'                                                  
         LA    R1,CONOTHH                                                       
         ST    R1,EFHOTH                                                        
*                                                                               
         LA    R1,CONTAGH                                                       
         ST    R1,EFHTAG                                                        
*                                                                               
         OI    GENSTAT1,NOSETEFH+RDUPAPPL                                       
         OI    GENSTAT3,OKVALSEL+RESTXE00                                       
         OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
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
SYSX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION TO SEE IF TERMINAL AUTHORIZED                                 
***********************************************************************         
VALIACT  NTR1                                                                   
         CLI   1(RA),C'*'          IF DDS TERMINAL                              
         BE    VACTYES             THEN DON'T CHECK IF AUTHORIZED               
*                                                                               
*                                                                               
         B     VACTYES             NO VALIDATION !!                             
*                                                                               
*                                                                               
         LA    R2,CONACTH          CHECK ENTERED ACTION                         
         LA    R3,RECACT2          AGAINST OUR ACTION TABLE                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
VACTLP   EX    R1,*+8              FIND CORRESPONDING ACTION                    
         B     *+10                                                             
         CLC   CONACT(0),1(R3)                                                  
         BE    VACT10                                                           
         LA    R3,L'RECACT(R3)                                                  
         CLI   0(R3),X'02'                                                      
         BNE   VACTYES                                                          
         B     VACTLP                                                           
*                                                                               
VACT10   MVC   CONACT,1(R3)                                                     
         MVI   CONACTH+5,L'CONACT                                               
         OI    CONACTH+6,X'80'                                                  
*                                                                               
         MVC   HALF,12(R3)         CHECK AUTHORIZATION CODE                     
         NC    HALF,12(RA)                                                      
         BZ    VACTYES                                                          
*                               IF ANY BIT IS ON, THEN NOT AUTHORIZED           
         MVC   CONHEAD(30),=CL30'ERROR: ACTION NOT AUTHORIZED'                  
         OI    CONHEADH+6,X'80'                                                 
         L     R1,ATIOB                                                         
         OI    TIOBINDS-TIOBD(R1),TIOBSETC+TIOBALRM                             
         SR    R2,RA                                                            
         STCM  R2,3,TIOBCURD-TIOBD(R1)                                          
*                                                                               
VACTNO   B     NO                                                               
VACTYES  B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF SIGN ON IS A LOCAL REP, IF SO, SET FLAG IN CTLRFLG1           
***********************************************************************         
CHKLOCAL NTR1                                                                   
         NI    CTLRFLG1,X'FF'-X'10' CLEAR "LOCAL REP" FLAG                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           READ REP RECORD                              
         MVC   KEY+25(2),TWAAGY                                                 
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEYSAVE,KEY,         X        
               0,0                                                              
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                REP RECORD NOT ON FILE?  HOW?                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IO,DMWORK,0           
*                                                                               
         LA    R6,IO                                                            
         LA    RE,34(R6)           START OF ELEMENT                             
*                                                                               
LOCAL20  DS    0H                                                               
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
*                                                                               
         CLI   0(RE),X'01'         PROGRAM PROFILE ELEMENT?                     
         BE    LOCAL40                                                          
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     LOCAL20                                                          
*                                                                               
LOCAL40  DS    0H                                                               
         TM    RREPFLGS-RREPELEM(RE),X'80'                                      
         BZ    LOCALNO             NOT A LOCAL REP                              
         OI    CTLRFLG1,X'10'                                                   
*                                                                               
LOCALYES B     YES                                                              
LOCALNO  B     NO                                                               
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
         B     VOFF                                                             
         B     VSTAT                                                            
         B     GADV                                                             
         B     GAGY                                                             
         B     GSAL                                                             
         B     VCON                                                             
         B     GCON                                                             
         B     VREPSYS                                                          
         B     VSPTSYS                                                          
         B     VCURSERR                                                         
         B     VMYERR                                                           
         B     VCLEARF                                                          
         B     VINITIAL                                                         
         B     VGETTWA                                                          
         B     VINITMNO                                                         
         B     VGTPRFIL                                                         
*                                                                               
BASER7   DC    A(0)                                                             
*                                                                               
TRAPERR  DS    0H                                                               
         GOTO1 ERREX                                                            
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
         MVC   SVSIGNON,SVSIGN                                                  
         B     VUSER60                                                          
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
         BE    VUSER15                                                          
         LA    R2,CONHEADH                                                      
         XC    8(L'CONHEAD,R2),8(R2)                                            
         MVC   8(L'MSNGORGN,R2),MSNGORGN                                        
         OI    6(R2),X'80'                                                      
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
MSNGORGN DC    C'MISSING ORIGIN INFO IN IDI RECORD'                             
*                                                                               
         USING CTORGD,R6                                                        
VUSER15  MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         DROP  R6                                                               
*                                                                               
VUSER20  DS    0H                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    VUSER35                                                          
*                                                                               
         LA    R2,CONHEADH                                                      
         XC    8(L'CONHEAD,R2),8(R2)                                            
         MVC   8(L'MSNGIDNM,R2),MSNGIDNM                                        
         OI    6(R2),X'80'                                                      
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
MSNGIDNM DC    C'MISSING ID NAME IN IDI RECORD'                                 
         DROP  R4                                                               
*                                                                               
         USING CTDSCD,R6                                                        
VUSER35  XC    SVSIGNON,SVSIGNON                                                
         MVC   SVSIGNON,CTDSC SAVE FOR FUTURE REF                               
         OC    SVSIGNON,SPACES                                                  
         MVC   SVSIGN,SVSIGNON SAVE FOR FUTURE REF                              
         DROP  R6                                                               
*                                                                               
VUSER60  MVC   RECNUM,TWALREC     MOVE SAVED REC/ACT TO CURRENT IN CASE         
         MVC   ACTNUM,TWALACT     WE TAKE ERROR BEFORE GENCON SETS THEM         
*                                                                               
         TM    TRNSTAT,RACHANG     IF RECORD & ACTION FLD HAS CHANGED           
         BNO   VUSERX                                                           
         MVI   CALLSP,0            CLEAR CALLPROG STACK                         
         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
*                                                                               
         MVI   CURRSYS,C'R'                                                     
*                                                                               
VUSERX   B     XIT                                                              
         EJECT                                                                  
* OVERLAY INITIALIZATION                                                        
*                                                                               
*                                  P1=A(PFKEY VAL. TABLE) OR ZEROS              
*                                                                               
VINITIAL ICM   R3,7,1(R1)          IF PFKEY VALIDATION TABLE PASSED             
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
INIT20   TM    CTLRFLG1,CF1NOCLR       DON'T CLEAR APPLICATION STORAGE?         
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1NOCLR  YES, BUT CLEAR IT NEXT TIME             
         B     INIT30                                                           
*                                                                               
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
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES                 IF RETURNS, RETURN CC EQUAL                  
         EJECT                                                                  
*              ROUTINE TO PROCESS PFKEY REQUEST                                 
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         TM    PFTSTAT2,PFTCLRKY   DO WE CLEAR THE PFKEY?                       
         BNZ   *+8                 NO                                           
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
         BZ    *+12                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         B     DUMMYERR            TAKE DUMMY ERROR XIT FOR GOAGAIN             
*                                                                               
         CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
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
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
*              R3=A(PFKEY TABLE)                                                
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         TM    CTLRFLG1,CF1TSELQ         DON'T TEST SEL FIELDS?                 
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1TSELQ   YES, RESET FOR NEXT TIME               
         B     TSELX                                                            
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
         LR    RE,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
*                                                                               
         CLI   7(R2),0             ANYTHING?                                    
         BNE   TSEL12              YES! PROCESS                                 
         ZIC   R1,0(R2)            SKIP TO THE NEXT FIELD AND CHECK             
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
TSEL12   MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    RE,RA                                                            
         STH   RE,CURDISP          SAVE DISP. TO FIELD                          
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
         MVC   CALLSTCK,TWASCR     SAVE SCREEN NUMBER ON STACK                  
         MVI   CALLSP,1                                                         
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
         MVI   CALLSP,0            DECREMENT STACK POINTER                      
         MVC   TWASCR,CALLSTCK     EXTRACT TWASCR                               
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
***********************************************************************         
* THIS ROUTINE VALIDATES THE OFFICE FIELD                                       
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF OFFICE FIELD                 
*                                                                               
* ON EXIT:                                                                      
***********************************************************************         
VOFF     GOTO1 ANY                 OFFICE                                       
*                                                                               
VOFFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE STATION CALL LETTERS                               
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF STATION FIELD                
*                                                                               
* ON EXIT:     QSTA                STATION CALL LETTERS                         
*              MKTNM               MARKET NAME                                  
***********************************************************************         
VSTAT    GOTO1 ANY                                                              
         MVC   QSTA,WORK                                                        
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         CLI   QSTA+4,C'L'                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVSTATN                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),TWAAGY                                                 
         MVC   KEY+22(5),QSTA                                                   
         CLI   KEY+26,C'T'         NO T ON REP STA RECS                         
         BNE   *+8                                                              
         MVI   KEY+26,C' '                                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    VSTA10                                                           
         B     NO                  <-- REC NOT FOUND                            
*                                                                               
VSTA10   GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO1,DMWORK,0          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RSTAREC,R2                                                       
         L     R2,AIO1                                                          
         LA    R2,RSTAELEM                                                      
         USING RSTAELEM,R2                                                      
         MVC   MKTNM,RSTAMKT                                                    
         DROP  R2                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE ADVERISER NAME FROM QADV                                
* ON ENTRY:    QADV                                                             
* ON EXIT :    ADVNM                                                            
***********************************************************************         
GADV     DS    0H                                                               
         XC    ADVNM,ADVNM                                                      
         XC    KEY,KEY                                                          
*                                                                               
         CLI   CURRSYS,C'R'                                                     
         BE    GADV05                                                           
         GOTO1 REPSYS                                                           
*                                                                               
GADV05   MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),QADV                                                   
         MVC   KEY+25(2),TWAAGY                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GADV10                                                           
         B     NO                  <-- REC NOT FOUND                            
*                                                                               
GADV10   GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO1,DMWORK,0          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RADVREC,R2                                                       
         L     R2,AIO1                                                          
         LA    R2,RADVELEM                                                      
         USING RADVELEM,R2                                                      
         MVC   ADVNM,RADVNAME                                                   
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE AGENCY NAME FROM QAGY                                   
* ON ENTRY:    QAGY                                                             
* ON EXIT :    AGYNM                                                            
***********************************************************************         
GAGY     DS    0H                                                               
         XC    AGYNM,AGYNM                                                      
         XC    KEY,KEY                                                          
*                                                                               
         CLI   CURRSYS,C'R'                                                     
         BE    GAGY05                                                           
         GOTO1 REPSYS                                                           
*                                                                               
GAGY05   MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),QAGY                                                   
         MVC   KEY+23(2),QAGYOFF                                                
         MVC   KEY+25(2),TWAAGY                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GAGY10                                                           
         B     NO                  <-- REC NOT FOUND                            
*                                                                               
GAGY10   GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO1,DMWORK,0          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RAGYREC,R2                                                       
         L     R2,AIO1                                                          
         LA    R2,RAGYELEM                                                      
         USING RAGYELEM,R2                                                      
         MVC   AGYNM,RAGYNAM1                                                   
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE SALESPERSON'S NAME                                      
* ON ENTRY:    QSAL                                                             
* ON EXIT :    SALNM                                                            
***********************************************************************         
GSAL     DS    0H                                                               
         XC    SALNM,SALNM                                                      
         XC    KEY,KEY                                                          
*                                                                               
         CLI   CURRSYS,C'R'                                                     
         BE    GSAL05                                                           
         GOTO1 REPSYS                                                           
*                                                                               
GSAL05   MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),QSAL                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GSAL10                                                           
         B     NO                  <-- REC NOT FOUND                            
*                                                                               
GSAL10   GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO1,DMWORK,0          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RSALREC,R2                                                       
         L     R2,AIO1                                                          
         LA    R2,RSALELEM                                                      
         USING RSALELEM,R2                                                      
         MVC   SALNM,RSALNAME                                                   
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE SALESPERSON                                        
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER                                 
*                                                                               
***********************************************************************         
VSAL     GOTO1 ANY                                                              
*                                                                               
VSALX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE CONTRACT                                           
*                                                                               
* ON ENTRY:    BCONT               PACKED CONTRACT - NOT COMPLEMENTED           
*                                                                               
* ON EXIT :    BCONT99             PACKED CONTRACT - COMPLEMENTED               
*                                                                               
***********************************************************************         
VCON     DS    0H                                                               
*                                                                               
         PACK  DUB,BCONT                                                        
         OI    DUB+7,X'0F'                                                      
         ZAP   WORK(8),=P'99999999'                                             
         SP    WORK(8),DUB                                                      
         UNPK  DUB,WORK(8)                                                      
         OI    DUB+7,X'F0'                                                      
         PACK  WORK(5),DUB(9)                                                   
         MVC   BCONT99,WORK                                                     
*                                                                               
VCONX    B     XIT                                                              
***********************************************************************         
* THIS ROUTINE READS THE CONTRACT AND SAVES VALUES                              
*                                                                               
* ON ENTRY:    BCONT99             PACKED CONTRACT - COMPLEMENTED               
*                                                                               
* ON EXIT :    QOFF                OFFICE                                       
*              QADV                ADVERTISER                                   
*              QAGY                AGENCY                                       
*              QAGYOFF             AGENCY OFFICE                                
*              QSAL                SALESPERSON                                  
*              QCONSTA             STATION FROM CONTRACT REC                    
*              QCONT               CHAR CONTRACT                                
***********************************************************************         
GCON     DS    0H                                                               
*                                                                               
         XC    QOFF,QOFF           CLEAR FIRST                                  
         XC    QADV,QADV                                                        
         XC    QAGY,QAGY                                                        
         XC    QAGYOFF,QAGYOFF                                                  
         XC    QSAL,QSAL                                                        
         XC    QCONSTA,QCONSTA                                                  
         XC    QCONT,QCONT                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+23(4),BCONT99                                                
         MVC   KEY+21(2),TWAAGY                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON10                                                           
         B     NO                  <-- REC NOT FOUND                            
*                                                                               
GCON10   GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO1,DMWORK,0          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCONKEY,R2                                                       
         L     R2,AIO1                                                          
*                                                                               
         MVC   QOFF,RCONKOFF                                                    
         MVC   QADV,RCONKADV                                                    
         MVC   QAGY,RCONKAGY                                                    
         MVC   QAGYOFF,RCONKAOF                                                 
         MVC   QCONSTA,RCONKSTA                                                 
*                                                                               
         ZAP   WORK(5),=P'99999999'                                             
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),BCONT99               UNPACK                          
         SP    WORK(5),WORK+20(5)                                               
         MVC   BLOCK(5),WORK                                                    
         EDIT  (P5,BLOCK),(8,QCONT),ALIGN=LEFT    CONTRACT #                    
*                                                                               
         LA    R2,RCONELEM         X'01' ELEM                                   
         USING RCONELEM,R2                                                      
         MVC   QSAL,RCONSAL                                                     
         MVC   QBUYER,RCONBUYR                                                  
         MVC   QPROD,RCONPRD                                                    
         MVC   QDATES,RCONDATE                                                  
         DROP  R2                                                               
*                                                                               
GCON20   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    GCONX                                                            
         CLI   0(R2),5                                                          
         BNE   GCON20                                                           
         USING RCONEXEL,R2                                                      
         MVC   QPRODNM,RCONEXPR                                                 
         DROP  R2                                                               
*                                                                               
GCONX    B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SWITCH TO SPOT SYSTEM                                                         
***********************************************************************         
VREPSYS  DS    0H                                                               
         GOTO1 SWITCH,DMCB,=C'REP ',0                                           
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    NO                                                               
         CLI   4(R1),1                                                          
         BE    NO                                                               
         MVI   CURRSYS,C'R'                                                     
         B     YES                                                              
***********************************************************************         
* SWITCH TO SPOT SYSTEM                                                         
***********************************************************************         
VSPTSYS  DS    0H                                                               
         GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    NO                                                               
         CLI   4(R1),1                                                          
         BE    NO                                                               
                                                                                
         MVI   CURRSYS,C'S'                                                     
         B     YES                                                              
***********************************************************************         
* SET CURSOR TO ERROR POSITION AND XIT TO ERROR ROUTINE                         
*                                                                               
* AT ENTRY, P1 BYTE  1   = LENGTH OF 2ND HALF OF SCANNER FIELDS                 
*              BYTES 2-4 = A(SCANNER BLOCK)                                     
*           P2 BYTE  1   = 0 -- GOTO1 ERREX                                     
*                        = 2 -- GOTO1 ERREX2                                    
*              BYTES 2-4 = A(XL1 CONTAINING INVALID FIELD NUMBER)               
*           R2 POINTS TO OFFENDING FIELD HEADER                                 
***********************************************************************         
VCURSERR SR    R4,R4                                                            
         ICM   R4,7,1(R1)          A(SCANNER BLOCK)                             
         ZIC   RE,0(R1)            LENGTH OF 2ND HALF OF SCANNER FIELDS         
         LA    RE,22(RE)           TOTAL LENGTH OF EACH SCANNER FIELD           
         LA    R3,8(R2)            A(NEW CURSOR POSITION)                       
         SR    R5,R5                                                            
         MVC   BYTE,4(R1)          ERROR ROUTINE SWITCH                         
         ICM   R5,7,5(R1)          A(INVALID FIELD NUMBER)                      
         CLI   0(R5),1             TEST FIRST FIELD IS INVALID                  
         BE    VC100               CURSOR NEED NOT BE ADJUSTED                  
         LA    RF,1                                                             
*                                                                               
VC20     ZIC   R1,0(R4)            LENGTH OF FIRST HALF OF FIELD                
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS ','              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    VC100                                                            
*                                                                               
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    VC40                LENGTH OF SECOND HALF OF FIELD               
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS '='              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    VC100                                                            
*                                                                               
VC40     LA    R4,0(RE,R4)         NEXT SCANNER ENTRY                           
         B     VC20                                                             
*                                                                               
VC100    L     R1,SYSPARMS                                                      
         L     R1,0(R1)            A(TIOB)                                      
         USING TIOBD,R1                                                         
         LR    RF,R2               COMPUTE DISPLACEMENT OF ERROR FLDH           
         S     RF,ATWA             FROM TWA START                               
         STCM  RF,3,TIOBCURD                                                    
         LR    RF,R2                                                            
         LA    RF,8(RF)            RF=A(FIELD START)                            
         SR    R3,RF               COMPUTE INDEX INTO FIELD FOR CURSOR          
         STC   R3,TIOBCURI                                                      
         OI    TIOBINDS,TIOBSETC                                                
*                                                                               
         CLI   BYTE,0              GO TO PROPER ERROR ROUTINE                   
         BE    VC200                                                            
         CLI   BYTE,2                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX2                                                           
VC200    GOTO1 ERREX                                                            
         DROP  R1                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS           
***********************************************************************         
VMYERR   TM    MNIOFLAG,X'80'      MUST CLOSE MINIO BUFFER?                     
         BZ    VMYERR10            NO                                           
*                                                                               
         LA    R5,MINBLOCK         YES, MUST CLOSE AFTER A MATCH                
         USING MINBLKD,R5                                                       
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
VMYERR10 OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
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
         BH    VMYERR20            NO                                           
         TM    GENSTAT2,USMYERSY   YES, USE GENERAL SYSTEM?                     
         BNZ   VMYERR20                 NO                                      
         MVI   GTMSYS,X'FF'             YES                                     
         DROP  RF                                                               
*                                                                               
VMYERR20 GOTO1 ERREX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ/WRITE TEMPSTR PAGES                                           
*                                                                               
* ON ENTRY:    PARAM 1  BYTE  0    BIT SETTINGS/PAGE NUMBER                     
*              PARAM 1  BYTES 1-3  READ/WRITE ADDRESS                           
***********************************************************************         
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
* INITIALIZE MINIO VALUES                                                       
***********************************************************************         
VINITMNO DS    0H                                                               
         LA    R0,MINBLOCK         CLEAR MINBLOCK                               
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         MVC   MINRECUP,RECUP                                                   
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,INVFIL       FILE NAME                                    
         MVC   MINDIR,INVDIR       DIR NAME                                     
         MVI   MINFKLEN,L'SNVKEY   KEY LENGTH                                   
         MVI   MINEKLEN,L'SNVRMIN    ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'SNVRMAST   DISPLACEMENT TO ELEMENT KEY                
         MVC   MINAGYC,AGENCY                                                   
         MVI   MINAGYD,36                                                       
         MVI   MINNCTL,L'SNVDSTAT  NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(3975) MAXIMUM RECORD LENGTH                        
         MVC   MINBUFF,AIO2        A(FIRST BUFFER)                              
         MVI   MINNBUF,2           USE TWO BUFFERS                              
         L     R1,ASYSD                                                         
         AH    R1,=Y(MINSTRT-SYSD)                                              
         ST    R1,MINRTAB          A(AREA FOR RECORD TABLE)                     
         MVC   MINRTABL,=Y(LENMINIO)  LENGTH OF RECORD TABLE                    
*                                                                               
         LA    RE,MELEM            A(AREA FOR ELEM OR CLUSTER)                  
         ST    RE,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'MELEM)   MAX LENGTH OF ELEM OF CLUSTER             
         XC    0(L'MELEM,RE),0(RE)   CLEAR MINELEM AREA                         
* BUILD MASTER KEY                                                              
         XC    MINMKEY,MINMKEY     CLEAR MASTER KEY FOR MINIO                   
         LA    R4,MINMKEY                                                       
         USING SNVKEY,R4                                                        
*                                                                               
         TM    CTLRFLG1,X'10'      IS THIS A LOCAL REP?                         
         BZ    MNO20               NO                                           
*                                                                               
         MVI   SNVLTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVLSUB,SNVLSUBQ    X'B3'                                        
         MVC   SNVLID,TWAORIG                                                   
         MVC   SNVLSTA,QSTA                                                     
         MVC   SNVLMOS,BMOSFF                                                   
         MVC   SNVLSORD,QSTAORD#                                                
         MVC   SNVLPOW,TWAAGY                                                   
         B     XIT                                                              
*                                                                               
MNO20    MVI   SNVRTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVRSUB,SNVRSUBQ    X'A3'                                        
         MVC   SNVREP,AGENCY                                                    
         MVC   SNVRSTA,QSTA                                                     
         MVC   SNVRMOS,BMOSFF                                                   
         MVC   SNVRCON,BCONT99                                                  
         MVC   SNVRFLG,QSNVRFLG                                                 
         MVC   SNVRINV,QINVOICE                                                 
         CLI   SNVRFLG,0                                                        
         BE    *+10                                                             
         MVC   SNVRINV,QORD#                                                    
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GETS PROFILE REC                                                              
*                                                                               
* ON ENTRY:    P1                  A(PROFILE PROGRAM CODE)                      
*              P2                  A(16 BYTE PROFILE AREA)                      
***********************************************************************         
VGTPRFIL DS    0H                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         CLI   CLTOFFIC,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFIC                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,0(R3),DATAMGR                                  
GTPRFX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VCLEARF - CLEAR AND FOUT FIELDS                                               
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
***********************************************************************         
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
*              LOCAL XIT/ERROR ROUTINES                                         
*                                                                               
CANTPUSH MVI   GERROR1,ERNOPUSH    PUSH ERROR - TOO MANY NEST LEVELS            
         MVI   GETMSYS,23                                                       
         B     RETCURS                                                          
PFERR    MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         MVI   GETMSYS,23                                                       
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
YES      SR    RC,RC               SET CC EQ                                    
NO       LTR   RC,RC               SET CC NEQ                                   
EXIT     EQU   *                                                                
XIT      XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
***********************************************************************         
* PROVIDE MSPACK ENTRY POINT FOR LINKAGE TO STAPACK                             
***********************************************************************         
GOMSPACK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
*        L     R8,ASPOOLD                                                       
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
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 STAPACK,(R4)                                                     
         CLI   STAPERR,0                                                        
         BNE   MSPKNO                                                           
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
*                                                                               
         MVC   BNTWK,STAPSTA+2                                                  
         CLI   SVAPROF+7,C'C'      CANADIAN?                                    
         BE    *+8                                                              
         NI    BNTWK,X'7F'                                                      
         DROP  R4                                                               
*                                                                               
MSPKYES  SR    RC,RC               SET CC EQ                                    
MSPKNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PROVIDE MSUNPK ENTRY POINT FOR LINKAGE TO STAPACK                             
***********************************************************************         
GOMSUNPK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
*        L     R8,ASPOOLD                                                       
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
         GOTO1 STAPACK,(R4)                                                     
         CLI   STAPERR,0                                                        
         BNE   MSUPNO                                                           
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         DROP  R4                                                               
*                                                                               
MSUPYES  SR    RC,RC               SET CC EQ                                    
MSUPNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(MEDGET)                                                        
         DC    V(RECUP)                                                         
         DC    V(BINSRCH)                                                       
         DC    V(DPTRD)                                                         
         DC    V(GETBROAD)                                                      
         DC    V(TWABLD)                                                        
         DC    V(TIMPK)                                                         
         DC    V(TIMUNPK)                                                       
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QQSORT)                                                      
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(QMINIO)                                                      
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QSPGETBU)                                                    
         DC    AL1(QREPFACS)                                                    
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                                                               
         DS    0F                                                               
RECACT   DS    0CL14                                                            
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                  CL2 ACCESS BITS (SPARE HERE)                 
*                                                                               
RECACT1  DC    X'01',C'INVOICE ',AL1(01),X'0000',AL2(0)                         
         DC    X'01',C'DETAIL  ',AL1(02),X'0000',AL2(0)                         
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                  CL2 ACCESS BITS (ON = RESTRICTED)            
*                                                                               
RECACT2  DS    0H                                                               
***      DC    X'02',C'ADD     ',AL1(01,01,00),X'8000'                          
***      DC    X'02',C'CHANGE  ',AL1(02,01,00),X'8000'                          
         DC    X'02',C'DISPLAY ',AL1(03,01,00),X'8000'                          
***      DC    X'02',C'DELETE  ',AL1(04,01,00),X'8000'                          
***      DC    X'02',C'RESTORE ',AL1(06,01,00),X'8000'                          
***      DC    X'02',C'UPDATE  ',AL1(07,07,00),X'8000'                          
         DC    X'02',C'LIST    ',AL1(10,10,00),X'8000'                          
         DC    X'02',C'REPORT  ',AL1(12,12,00),AL2(0)                           
***      DC    X'02',C'NET     ',AL1(13,13,00),X'8000'                          
***      DC    X'02',C'MOVE    ',AL1(14,14,00),X'8000'                          
***      DC    X'02',C'REQUEST ',AL1(15,15,00),AL2(0)                           
***      DC    X'02',C'COPY    ',AL1(16,16,00),X'8000'                          
***      DC    X'02',C'FIXFILMS',AL1(17,17,00),X'8000'                          
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
*                                  CL2 ACCESS BITS (SPARE HERE)                 
*                                                                               
RECACT3  DS    0C                                                               
         DC    X'03',AL1(01,01),X'FD02000081',C'    '  INVOICE  DISP            
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,10),X'FE010000C0',C'    '  INVOICE  LIST            
         DC    AL2(0)                                                           
         DC    X'03',AL1(02,01),X'FC030000C1',C'    '  DETAIL   DISP            
         DC    AL2(0)                                                           
         DC    X'03',AL1(02,12),X'FC030303C0',C'    '  DETAIL   REPORT          
         DC    AL2(0)                                                           
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREIWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSNVN                                                      
         EJECT                                                                  
       ++INCLUDE REREIFFD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* CTGENFILE                                                                     
* DDOFFICED                                                                     
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
* DDACTIVD                                                                      
* FAGETTXTD                                                                     
* FATIOB                                                                        
* FAFACTS                                                                       
* FASECRETD                                                                     
* DDMINBLK                                                                      
* SEACSFILE                                                                     
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENPRD                                                                      
* SPGENEST                                                                      
* SPGENSTA                                                                      
* SPGENMKT                                                                      
* SPSTABLK                                                                      
* SPSTAPACKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE SEACSFILE                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENAGY                                                       
       ++INCLUDE REGENADV                                                       
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE REPFACSQ                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007REREI00   04/27/07'                                      
         END                                                                    
