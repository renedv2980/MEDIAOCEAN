*          DATA SET PPMAT00    AT LEVEL 068 AS OF 10/31/05                      
*PHASE T40200A                                                                  
*INCLUDE PUBVAL                                                                 
*INCLUDE PUBEDIT                                                                
*INCLUDE PUBFLOAT                                                               
*INCLUDE PPGETADR                                                               
*INCLUDE SRCHCALL                                                               
*INCLUDE RECUP                                                                  
*INCLUDE KHDUMMY                                                                
T40200   TITLE 'PPMAT00 - PRINT MATCHING CONTROLLER'                            
T40200   CSECT                                                                  
***********************************************************************         
* NOTE: if you are making changes to this program that use the field            
* 'qprd' or that call any already existing print match subroutines              
* be aware that for product various qprd is *** and the actual specific         
* product is contained in the minio detail element (field pimsprd).             
* Be sure to look carefully at all subroutines you call and to check            
* other calls to it for comparison. Thanks, Abbey                               
***********************************************************************         
*                                                                               
*  CHANGE LOG                                                                   
*                                                                               
*   SMYE  10/05    NEW LIMIT ACCESS SECURITY AGAIN                              
*                                                                               
*   SMYE  08/02    GO BACK TO OLD LIMIT ACCESS                                  
*                                                                               
*   SMYE  08/02    USE SECBLK AREA FOR SECRET BLOCK                             
*                                                                               
*   SMYE  07/02    FIX LOCK TESTING FOR SUB-CLIENTS                             
*                                                                               
*   SMYE  05/02    NEW LIMIT ACCESS SECURITY                                    
*                                                                               
*   BPLA 01/30/02  ANOTHER FIX TO VFINDLIN TO PREVENT LOOPING                   
*                                                                               
*   SMYE 12/13/01  IN VGETFLD CHANGE 6-POS'N NUMERIC TEST TO AN                 
*                  8-POS'N NUMERIC TEST (FIELD DEFINED AS MAX 8)                
*                                                                               
*   SMYE 11/09/01  IN SHOWDTL CHANGE PREMIUM DISPLAY TO ELIMINATE               
*                  "0C/" PRECEDING PREMIUM                                      
*                                                                               
*   SMYE  06/01    ADD LOCK TESTING FOR UPDATIVE SOON CONDITIONS                
*                                                                               
*   SMYE  4/16/01  FIX VFINDLIN TO CHECK FOR END OF LINE ("MAYBE")              
*                  WHEN LOOKING FOR CHANGES (SEE BPLA 3/22/99 BELOW)            
*                                                                               
*   SMYE  03/01    OC SPACES FOR PBINVPRD IN PBINVELM IN BUYREC -               
*                  2-CHAR PRD CODE HAS BINARY ZERO IN 3RD POSITION              
*                  WHICH SHOULD BE A SPACE (X'40').                             
*                                                                               
*   SMYE  02/00    FIX BUG IN VDTLGROS WHICH ONLY OCCURS UNDER STEREO.          
*                  2-CHAR QPRD HAS BINARY ZERO IN 3RD POSITION WHICH            
*                  SHOULD BE A SPACE (X'40').                                   
*                                                                               
*   SMYE  02/00    FIX BUG IN VDTLGROS ("GOTO1 CALCDTLG" GOES TO HERE)          
*                  WAS NOT HANDLING GST=X CANADIAN BUYS CORRECTLY               
*                                                                               
*   BPLA  3/22/99  FIX VFINDLIN TO CHECK FOR END OF SCREEN                      
*                  WHEN LOOKING FOR CHANGES                                     
*                                                                               
*   BPLA  10/13/98 FIX BUG IN VDTLGROS INTRODUCED BY ASTE ON                    
*                  1/12/97  WAS NOT HANDLING PRODUCT ***                        
*                  CORRECTLY                                                    
*                                                                               
*   ASTE 1/12/97   CHANGED VDTLGROS - 'FLD' IS FILLED WITH THE PRODUCT          
*                  FROM THE CORRECT SCREEN ('CHKPRD'- CHECK SCREEN,             
*                  'CKRPRD'- REPORT SCREEN) FOR USE IN PPMAT07                  
*                                                                               
*   BPLA 9/15/97   GETINS MADE CORE RESIDENT                                    
*                                                                               
         PRINT NOGEN                                                            
         NMODL LENWORK,T40200,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         L     RA,4(R1)            A(TWA)                                       
         USING T402FFD,RA                                                       
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
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE PROGRAM DEPENDENT VALUES          
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREAS                          
*                                                                               
         CLI   CONRECH+5,0         NO RECORD INPUTTED?                          
         BNE   *+14                                                             
         MVC   CONREC(7),=CL7'INVOICE'    ALWAYS INVOICE RECORD TYPE            
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
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,RCHANG      THEN SET RCHANG FLAG                         
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    T40200A                                                          
         OI    TRNSTAT,ACHANG      THEN SET ACHANG FLAG                         
         CLC   =C'CHA',CONACT      AND IT CHANGED TO 'CHA'                      
         BNE   *+8                                                              
         OI    TRNSTAT,USERCHA     THEN SET USER CAUSED 'CHA' FLAG              
*                                                                               
T40200A  BAS   RE,GOGENCON         GO OFF TO GENCON                             
*                                                                               
         OI    CONRECH+4,X'20'     SET RECORD/ACTION FLDS VALID                 
         OI    CONACTH+4,X'20'                                                  
*                                                                               
         TM    MNIOFLAG,X'80'      DO WE NEED TO CLOSE MINIO BUFFER             
         BZ    XIT                                                              
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         GOTO1 MINIO,DMCB,('MINCLS',(R5))    YES                                
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
         EJECT                                                                  
******************************************************************              
*                                                                               
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
         L     R1,8(R1)            A(COMFACS)                                   
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
         MVI   SYSTEM,C'P'         PRINT                                        
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'25'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'33'     USUALLY PRTFILE                              
         MVC   SYSFIL,=C'PRTFIL  '                                              
         MVC   SYSDIR,=C'PRTDIR  '                                              
         MVI   ACTELOPT,C'N'       DON'T ADD ACTIVITY ELEMENT                   
         MVI   GETMSYS,24          USES GETMSG FOR SYSTEM 24                    
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'PP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9040200'    PRESET FOR SYSTEM CALLOVS               
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
         OI    GENSTAT1,NOSETEFH+RDUPAPPL                                       
         OI    GENSTAT3,OKVALSEL+RESTXE00                                       
         OI    GLSTSTAT,CHNGLIST+APPLCDSP+RETEXTRA                              
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
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION TO SEE IF TERMINAL AUTHORIZED                                 
***********************************************************************         
VALIACT  NTR1                                                                   
         CLI   1(RA),C'*'          IF DDS TERMINAL                              
         BE    VACTYES             THEN DON'T CHECK IF AUTHORIZED               
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
* ROUTINE HANDLES PASSING OF CONTROL TO AND FROM GENCON                         
*                                                                               
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
VBRANCH  B     VAGY                                                             
         B     VMED                                                             
         B     VCLT                                                             
         B     VPRD                                                             
         B     VEST                                                             
         B     VPUB                                                             
         B     VSTD                                                             
         B     VEND                                                             
         B     VCURSERR                                                         
         B     VMYERR                                                           
         B     VGETFLD                                                          
         B     VCLEARF                                                          
         B     VINITIAL                                                         
         B     VGETTWA                                                          
         B     VMTCHUNS                                                         
         B     VINITMNO                                                         
         B     VGINVHDR                                                         
         B     VFINDLIN                                                         
         B     VNXDTLSQ                                                         
         B     VRSEQNCE                                                         
         B     VDTLGROS                                                         
         B     VREP                                                             
         B     VDISPDTL                                                         
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
VUSER20  DS    0H                                                               
*                                                                               
VUSER25  MVC   RECNUM,TWALREC     MOVE SAVED REC/ACT TO CURRENT IN CASE         
         MVC   ACTNUM,TWALACT     WE TAKE ERROR BEFORE GENCON SETS THEM         
*                                                                               
         TM    TRNSTAT,RACHANG     IF RECORD OR ACTION FLD HAS CHANGED          
         BZ    VUSERX                                                           
         MVI   CALLSP,0            CLEAR CALLPROG STACK                         
*         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO          
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
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
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
         CLI   CALLSP,L'CALLSTCK   IF ALREADY HAVE MAX NEST LEVELS              
         BNL   CANTPUSH            ERROR FOR STACK OVERFLOW                     
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
         EX    RE,*+4                                                           
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
VAGY     DS    0H                  GET AGY REC IN MEDIA VALIDATION              
*                                                                               
         BRAS  RE,PID              INIT PID                                     
*                                                                               
         B     XIT                 GET AGY REC IN MEDIA VALIDATION              
         SPACE 2                                                                
* VALIDATE MEDIA CODE *                                                         
         SPACE 1                                                                
VMED     XC    SVKEY,SVKEY         CLEAR SAVED KEY AREA                         
         GOTO1 GETFLD                                                           
*                                                                               
         MVI   ERROR,INVMED                                                     
         CLI   FLDH+5,1            INPUT LEN MUST BE 1                          
         BNE   TRAPERR                                                          
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING PAGYKEY,R4                                                       
         MVC   PAGYKAGY,AGENCY                                                  
         MVC   PAGYKMED,FLD        MEDIA                                        
         MVI   PAGYKRCD,X'01'                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,INVMED                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING PAGYELEM,R6                                                      
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   TRAPERR                                                          
*                                                                               
         MVC   USERNAME,PAGYNAME                                                
         MVC   USERADDR,PAGYADDR                                                
         MVC   SVUSER,USERNAME        SAVE FOR FUTURE REF                       
         MVC   QMED,8(R2)             SAVE INPUT MEDIA CODE                     
         MVC   MEDNM,PAGYMED          MEDIA NAME                                
         MVC   SVAGPROF,PAGYPROF      SAVE AGENCY PROFILE                       
         MVC   SVAGNATL,PAGYNAT       SAVE AGENCY NATIONALITY                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE CLIENT *                                                             
         SPACE 1                                                                
VCLT     DS    0H                                                               
         MVC   BYTE,DMCB           COPY OPTION BIT TO SEE IF WE RETURN          
*                                      THE ERROR CODE BACK TO CALLER            
         GOTO1 GETFLD                                                           
         MVC   QCLT,FLD                                                         
*                                                                               
         XC    KEY,KEY             GET CLIENT RECORD                            
         LA    R6,KEY                                                           
         USING PCLTRECD,R6                                                      
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,QCLT                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,INVCLI                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
*NOP*    CLI   OFFLINE,C'Y'       REPORT IN PROCESS ?                           
*NOP*    BE    VCLT50             YES - SKIP LIMIT ACCESS                       
*                                                                               
         L     RF,ATWA                                                          
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    VCLT50                                                           
*                                                                               
         LH    RE,=Y(SECBLK-T402FFD)                                            
         AR    RE,RF                                                            
         ST    RE,ASECBLK                                                       
*                                                                               
         L     R0,ASECBLK          CLEAR SECRET BLOCK                           
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
*  INITIALIZE SECURITY BLOCK                                                    
*                                                                               
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         MVC   CLTOFICE,PCLTOFF   SAVE PCLTOFF FOR LIMIT ACCESS TESTING         
*                                                                               
         BRAS  RE,CKTRAFID        TRAFFIC ID ?                                  
         BNE   VCLT20             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         L     R6,AIO1            POINT TO CLIENT REC                           
         MVI   ELCODE,X'50'       CLIENT TRAFFIC OFFICE ELEM CODE               
         BAS   RE,GETEL                                                         
         BNE   VCLT20             NO TRAFFIC OFFICE FOUND                       
         MVC   CLTOFICE,2(R6)     REPLACE PCLTOFF SAVED IN CLTOFICE             
*                                 WITH CLIENT TRAFFIC OFFICE CODE               
VCLT20   DS    0H                                                               
*                                                                               
         BRAS  RE,LIMACC          LIMIT ACCESS TESTING                          
         BE    VCLT50             OK                                            
*                                                                               
ACCERR   MVI   ERROR,SECLOCK        SECURITY LOCK-OUT             L05           
         TM    BYTE,X'80'          RETURN ERROR TO CALLER?                      
         BNZ   NO                                                               
         B     TRAPERR                                            L05           
*                                                                               
VCLT50   DS    0H                                                               
         L     R6,AIO1            POINT TO CLIENT REC                           
         MVC   CLTNM,PCLTNAME                                                   
         MVC   CLTOFICE,PCLTOFF                                                 
         MVC   SVCLPROF,PCLTPROF                                                
         MVC   SVESPROF+30(1),PCLTFIN    SAVE FINANCIAL INDICATOR IN            
*                                            SVESPROF+30 (SEE PPBUY01)          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE PRODUCT                                                              
*                                                                               
VPRD     DS    0H                                                               
         GOTO1 GETFLD                                                           
         MVC   QPRD,FLD                                                         
         CLC   QPRD,=C'***'                                                     
         BE    *+10                                                             
         OC    QPRD,SPACES                                                      
*                                                                               
         XC    KEY,KEY             GET PRODUCT RECORD                           
         LA    R4,KEY                                                           
         USING PPRDKEY,R4                                                       
         MVC   PPRDKAGY,AGENCY                                                  
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,QCLT                                                    
         MVC   PPRDKPRD,QPRD                                                    
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,INVPRD                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PPRDELEM,R6                                                      
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   TRAPERR                                                          
         MVC   PRDNM,PPRDNAME                                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE ESTIMATE *                                                           
         SPACE 1                                                                
VEST     DS    0H                                                               
         GOTO1 GETFLD                                                           
         MVI   ERROR,INVEST                                                     
         STH   R0,BEST             CONTAINS BINARY AFTER GETFLD                 
         LTR   R0,R0                                                            
         BZ    TRAPERR                                                          
*                                                                               
         XC    KEY,KEY             GET ESTIMATE RECORD                          
         LA    R4,KEY                                                           
         USING PESTKEY,R4                                                       
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,QCLT                                                    
         MVC   PESTKPRD,QPRD                                                    
         MVC   PESTKEST,BEST                                                    
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PESTELEM,R6                                                      
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ESTNM,PESTNAME                                                   
         MVC   ESTSTDT,PESTST                                                   
         MVC   ESTNDDT,PESTEND                                                  
         MVC   SVESPROF(29),PESTPROF   DON'T OVERWRITE FINANCIAL INDIC.         
*                                          IN SVESPROF+30 FROM CLTHDR           
         MVC   SVESPROF+31(1),PESTPROF+31                                       
         MVC   SVESPROF(1),PESTSTAT    SAVE LOCKOUT STATUS IN 1ST BYTE          
         MVC   SVESPROF+15(4),PESTREP  SAVE SPECIAL REP                         
         MVC   SVESPROF+29(1),PESTTEST SAVE TEST STATUS, X'80'-TEST EST         
         MVC   SVESPROF+28(1),PESTRTYP SAVE RATE TYPE                           
         OC    SVESPROF+15(4),SPACES   BLANK WILL BE NO ESTIMATE REP            
         EDIT  (2,BEST),(3,QEST)                                                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE START DATE                                                           
*                                                                               
VSTD     DS    0H                                                               
         GOTO1 GETFLD                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,FLD,QSTART (YYMMDD)                                  
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(3,BSTART) (YMD)                              
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
* VALIDATE END DATE                                                             
*                                                                               
VEND     DS    0H                                                               
         GOTO1 GETFLD                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,FLD,QEND (YYMMDD)                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   QEND,QSTART         SEL END/SEL START                            
         BNL   *+12                                                             
         MVI   ERROR,INVDTSEQ      END DATE BEFORE START DATE                   
         B     TRAPERR                                                          
         GOTO1 DATCON,DMCB,QEND,(3,BEST) (YMD)                                  
*                                                                               
VENDX    B     XIT                                                              
         EJECT                                                                  
* VALIDATE PUBLICATION                                                          
*                                                                               
VPUB     DS    0H                                                               
         ZIC   R0,5(R2)                                                         
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),BPUB                                   
*                                                                               
         MVI   ERROR,INVPUB                                                     
         CLI   0(R1),X'FF'                                                      
         BNE   VPUBKEY             PUB "VALID"                                  
*                                  TEST FOR "ALL"                               
         SH    R0,=H'4'                                                         
         BM    TRAPERR                                                          
         LA    R4,8(R2)                                                         
         AR    R4,R0                                                            
         CLC   0(4,R4),=C',ALL'    CHECK FOR ALL ZONES,EDTS                     
         BNE   TRAPERR                                                          
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),BPUB                                   
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         OC    BPUB+4(2),BPUB+4                                                 
         BNZ   TRAPERR                                                          
         OI    GLOBFLG1,X'80'      SET ALL ZONES AND EDITIONS BIT               
*                                                                               
VPUBKEY  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),BPUB     MOVE PUB/ZONE/EDTN                           
VPUBAGY  MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ERROR,INVPUB                                                     
         CLC   KEY(L'PUBKEY),KEYSAVE                                            
         BE    VPUB00                                                           
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    TRAPERR             NO                                           
         CLC   KEY(PUBKZON-PUBKEY),KEYSAVE                                      
         BNE   TRAPERR                                                          
         CLC   PUBKAGY(L'PUBKAGY+L'PUBKCOD),KEYSAVE+PUBKAGY-PUBKEY              
         BE    VPUB00                                                           
         BL    VPUBAGY                                                          
*                                                                               
         CLC   =X'FFFF',PUBKZON                                                 
         BE    TRAPERR                                                          
         ZICM  R1,PUBKZON,2                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,PUBKZON                                                     
         B     VPUBAGY                                                          
*                                                                               
VPUB00   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   PUBGSTAX,0                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   VPUB05                                                           
         USING PUBNAMEL,R6                                                      
         MVC   PUBGSTAX,PUBGST                                                  
*                                                                               
VPUB05   L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    VPUB10                                                           
         ZAP   PUBCASHD,=P'20'     CASH DISCOUNT NORMALLY 2.0% OF NET           
         ZAP   PUBAGYCM,=P'15000'  NO AGENCY COMMISSION (NET = GROSS)           
         XC    MAGFREQ,MAGFREQ                                                  
         B     VPUB20                                                           
*                                                                               
         USING PUBGENEL,R6                                                      
VPUB10   ZAP   PUBCASHD,PUBCD      COPY THE CASH DISCOUNT                       
         ZAP   PUBAGYCM,PUBAC      COPY THE AGENCY COMMISSION                   
         MVC   MAGFREQ,PUBMFREQ                                                 
*                                                                               
VPUB20   L     R6,AIO                                                           
         XC    PUBPYREP,PUBPYREP                                                
         MVI   ELCODE,X'14'        SEE IF ANY PAYING REP                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VPUB25   BAS   RE,NEXTEL                                                        
         BNE   VPUB30+6                                                         
         USING PUBREPEL,R6                                                      
*                                                                               
VPUB28   CLC   PUBRPOFF,=3X'FF'    AGENCY ALREADY?                              
         BE    VPUB30                                                           
         CLC   PUBRPOFF,QCLT       CLIENT MATCHES?                              
         BE    VPUB30                                                           
         CLI   PUBRPOFF,X'FF'      OFFICE                                       
         BNE   VPUB25                                                           
         CLC   PUBRPOFF+1(1),CLTOFICE  OFFICE MATCHES?                          
         BNE   VPUB25                                                           
*                                                                               
VPUB30   MVC   PUBPYREP,PUBPAREP   COPY THE PAYING REP                          
*                                                                               
         MVC   SVREPELM,PUBREPEL   SAVE REP ELEMENT                             
*                                                                               
         GOTO1 VPUBFLT,DMCB,AIO,PUBNM                                           
*                                                                               
         L     R6,AIO                                                           
         XC    PUBTAXES,PUBTAXES                                                
         MVI   ELCODE,X'22'        GET THE TAX ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   VPUBADDR                                                         
         USING PUBTAXEL,R6                                                      
         MVC   PUBTAXES,PUBTAX1    COPY THE TAX RATES AND DATES                 
*                                                                               
VPUBADDR GOTO1 =A(GTPUBADR),DMCB,(RC),RR=RELO                                   
*                                                                               
VPUBX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SET CURSOR TO ERROR POSITION AND XIT TO ERROR ROUTINE                         
*                                                                               
* AT ENTRY, P1 BYTE  1   = LENGTH OF 2ND HALF OF SCANNER FIELDS                 
*              BYTES 2-4 = A(SCANNER BLOCK)                                     
*           P2 BYTE  1   = 0 -- GOTO1 ERREX                                     
*                        = 2 -- GOTO1 ERREX2                                    
*              BYTES 2-4 = A(XL1 CONTAINING INVALID FIELD NUMBER)               
*           R2 POINTS TO OFFENDING FIELD HEADER                                 
*                                                                               
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
* GETFLD ROUTINE - EXTRACT DATA FROM SCREEN FIELD                               
*                                                                               
*              INPUTS              R2=A(FIELD HEADER)                           
*                                  FLDOPT=1 FIELD IS OPTIONAL                   
*              OUTPUTS             FLDH  CONTAINS FIELD HEADER                  
*                                  FLD   FIELD DATA SPACE FILLED                
*                                  R0    BINARY VALUE IF FIELD NUMERIC          
*                                  R1    FIELD LENGTH                           
*                                  CONDITION CODE ZERO IF R1=0                  
*                                                                               
VGETFLD  DS    0H                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         CLI   5(R2),0             TEST NO INPUT                                
         BE    GTFERRCK                                                         
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BNO   *+8                                                              
         SH    R1,=H'8'            (SUBTRACT 8 MORE)                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R2)                                                     
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
*                                                                               
VGETFLD1 CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    VGETFLD2                                                         
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,VGETFLD1                                                      
*                                                                               
VGETFLD2 STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GTFERRCK                                                         
*                                                                               
VGETFLD4 LR    RE,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  RE,0                                                             
         CH    RE,=H'7'            LIMIT TO MAX 8 DIGITS                        
         BH    GTNOTNUM                                                         
         MVC   WORK(8),=8X'F0'     TEST FOR NUMERIC FIELD (8 DIGITS)            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(8),=8X'F0'     SEE ABOVE (MAX 8 DIGITS, NOT 6)              
         BNE   GTNOTNUM                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
         B     VGETFLDX                                                         
*                                                                               
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         B     VGETFLDX                                                         
*                                                                               
GTFERRCK MVI   ERROR,MISSING                                                    
         CLI   FLDOPT,C'Y'         IS THIS OK?                                  
         BNE   TRAPERR                                                          
         XC    FLD,FLD                                                          
         B     VGETFLDX                                                         
*                                                                               
VGETFLDX MVI   FLDOPT,C'N'         RESET OPTIONAL BITS                          
         LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS           
*                                                                               
VMYERR   TM    MNIOFLAG,X'10'      MUST CLOSE MINIO BUFFER?                     
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
         MVI   GTMSYS,24           SYSTEM 24 MESSAGES                           
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    *+8                 NO -- USE CONTROL SYSTEM MESSAGES            
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
*                                                                               
         GOTO1 ERREX                                                            
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
         MVC   MINRECUP,VRECUP                                                  
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,SYSFIL       FILE NAME                                    
         MVC   MINDIR,SYSDIR       DIR NAME                                     
         MVI   MINFKLEN,L'PINVKEY  KEY LENGTH                                   
         MVI   MINEKLEN,L'PINVMINI   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'PINVMAST   DISPLACEMENT TO ELEMENT KEY                
         MVI   MINNCTL,2           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(2900) MAXIMUM RECORD LENGTH                        
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
         USING PINVKEY,R4                                                       
         MVC   PINVAGY,AGENCY                                                   
         MVC   PINVMED,QMED                                                     
         MVI   PINVTYPE,PINVTYPQ                                                
         MVC   PINVCLT,QCLT                                                     
         MVC   PINVPRD,QPRD                                                     
         MVC   PINVPUB,BPUB                                                     
         MVC   PINVYS,QYEAR                                                     
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MATCHES UNMATCHED INVOICE DETAILS TO THE BUYS.                   
* ALL THE FIELDS EXCEPT FOR ESTIMATE (IF NONE MATCH ACROSS ESTIMATES)           
* MUST MATCH EXACTLY.                                                           
*                                                                               
*                VALUES SHOULD BE SET                                           
* ON ENTRY:    LSTHDRSQ            INVOICE HEADER SEQUENCE NUMBER               
*              BEST                ESTIMATE USED IN THE INVOICE HEADER          
*              SPCLREP             SPECIAL REP FOR THE INVOICE HEADER           
*              INVSTDT             PERIOD START DATE OF INVOICE HEADER          
*              INVENDDT            PERIOD END DATE OF INVOICE HEADER            
***********************************************************************         
VMTCHUNS DS    0H                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                                                               
         L     R0,ASYSD            CLEAR MATCH TABLE                            
         AH    R0,=Y(MATCHTBL-SYSD)                                             
         LH    R1,=Y(MATCHLEN)                                                  
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI    MNIOFLAG,X'40'      PUT UP 'NUMBER MATCHED: ' MESSAGE            
         NI    MNIOFLAG,X'FF'-X'20'    IF ON, NOT A SUCCESSFUL MATCH            
         SR    R2,R2               R2 = COUNTER OF MINIO ELEMENTS               
*                                                                               
         L     R3,ASYSD            R3 = A(MATCH TABLE)                          
         AH    R3,=Y(MATCHTBL-SYSD)                                             
         USING MTCHLIND,R3                                                      
         L     R6,MINELEM          R6 = A(MINIO ELEMENT)                        
         USING PIMDTLEL,R6                                                      
*                                                                               
         XC    MINEKEY,MINEKEY     SET UP MINIO ELEMENT KEY                     
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINHI',(R5))  GET FIRST INVOICE DETAIL              
*                                                                               
MUNS10   CLI   MINERR,0                                                         
         BE    MUNS20                                                           
         CLI   MINERR,MINEEOF      NO MORE DETAILS, COMPARE THE BUYS            
         BE    MUNS50                                                           
         DC    H'0'                                                             
*                                                                               
MUNS20   DS    0H                                                               
         CLI   PIMDTLEL,PIMDTLEQ   STILL AN INVOICE DETAIL?                     
         BNE   MUNS50              NO, GO COMPARE THE BUYS                      
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ   STILL ONE OF OUR HEADER'S DETAILS?           
         BNE   MUNS50              NO, GO COMPARE THE BUYS                      
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED?                                     
         BNZ   MUNS40                                                           
*                                                                               
         MVC   MTCHSEQ,PIMDTLS2    NO                                           
         MVC   MTCHIDAT,PIMIDATE   COPY DATE                                    
         MVC   MTCHSPRD,PIMSPRD    SPECIFIC PRODUCT FOR PRD ***                 
         CLC   PIMCPRD,=CL3' '     IF CORRECTED PRODUCT PRESENT                 
         BNH   *+10                                                             
         MVC   MTCHSPRD,PIMCPRD       USE IT                                    
         MVC   MTCHZONE(2),PIMBZONE ZONE/ED                                     
         TM    GLOBFLG1,X'80'                                                   
         BO    *+14                                                             
         LA    R1,MINMKEY                                                       
         MVC   MTCHZONE(2),PINVPUB+4-PINVKEY(R1)                                
         MVC   MTCHIEST,PIMIEST                                                 
         MVC   MTCHDATE(PIMDSTAT-PIMBDATE),PIMBDATE   UP TO COLORS              
*&&DO                                                                           
*      NEWSPAPER SHOULD BE SPACE FILLED                                         
         CLI   SCRTYPE,C'N'        NEWSPAPER TYPE?                              
         BNE   MUNS25              NO                                           
         CLI   PIMUIND,0                                                        
         BNE   MUNS30                                                           
         CP    PIMUNITS,=P'0'                                                   
         BNE   MUNS30                                                           
         CP    PIMCLMS,=P'0'                                                    
         BE    MUNS28              IF JUST A DESCR. THEN SPACE FILL             
         B     MUNS30                                                           
*&&                                                                             
MUNS25   CLI   QMED,C'O'           OUTDOORS NEEDS IT ONLY IF SPACE              
         BNE   *+12                    IS NOT THE 3 PACKED FIELDS               
         CLI   MTCHSPCE,X'FF'                                                   
         BE    MUNS30                                                           
*                                                                               
MUNS28   OC    MTCHSPCE,MTCHSPCE   YES,  N, M, S, & T  ARE SPACE FILLED         
         BZ    MUNS30              UNLESS SPACE IS NULLS                        
         OC    MTCHSPCE,SPACES                                                  
*                                                                               
MUNS30   ZAP   MTCHCLMS,=P'0'                                                   
         OC    PIMCLMS,PIMCLMS                                                  
         BZ    *+10                                                             
         ZAP   MTCHCLMS,PIMCLMS                                                 
*                                                                               
         CLI   MTCHUIND,0          LINES?                                       
         BNE   *+8                                                              
         MVI   MTCHUIND,C'L'       MAKE IT C'L'                                 
*                                                                               
         MVI   MTCHCTYP,C'U'       DEFAULT IS UNIT COST                         
         TM    PIMDSTAT,X'80'      IS IT UNIT COST?                             
         BNZ   *+8                                                              
         MVI   MTCHCTYP,C'T'       NO, TOTAL COST                               
*                                                                               
         CLI   SCRTYPE,C'M'                                                     
         BNE   *+8                                                              
         MVI   MTCHCTYP,0          NO COST TYPE FOR MAGAZINE                    
*                                                                               
         MVI   MTCHCD,C'Y'         DEFAULT IS CASH DISCOUNT                     
         TM    PIMDSTAT,X'40'      ANY CASH DISCOUNT?                           
         BZ    *+8                                                              
         MVI   MTCHCD,C'N'         NONE                                         
*                                                                               
         MVI   MTCHCIND,C' '       COST INDICATOR IS SPACE IF NULL              
         OC    PIMCSIND,PIMCSIND                                                
         BZ    *+10                                                             
         MVC   MTCHCIND,PIMCSIND                                                
*                                                                               
         LA    R3,MTCHNXTL         R3 = A(NEXT MATCH TABLE ENTRY)               
         LA    R2,1(R2)            INCREMENT THE COUNTER OF ENTRIES             
*                                                                               
         CH    R2,=Y(MXDTLITM)                                                  
         BNH   MUNS40                                                           
         MVC   CONHEAD(27),=C'TOO MANY DETAILS - CALL DDS'                      
         OI    CONHEADH+6,X'80'    TRANSMIT                                     
         OI    CONRECH+6,X'40'+X'80' POSITION CURSOR AND TRANSMIT               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
MUNS40   GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         B     MUNS10                                                           
         DROP  R6,R3                                                            
*                                                                               
*                                  GO THRU ALL THE BUYS SO WE GET THE           
MUNS50   DS    0H                      THE RIGHT MESSAGE                        
*                                                                               
*MUNS50   LTR   R2,R2               ANY UNMATCHED INVOICE DETAILS?              
*         BNZ   *+12                                                            
*         OI    MNIOFLAG,X'20'      DIDN'T TEST THE BUYS                        
*         B     MUNSX               NO, RETURN TO CALLER                        
*                                                                               
         L     RE,ASYSD            CLEAR MATCH TABLE                            
         AH    RE,=Y(MATCHTBL-SYSD)                                             
         SR    R1,R1                                                            
         USING MTCHLIND,RE                                                      
MUNSCT   OC    MTCHSEQ,MTCHSEQ                                                  
         BZ    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,MTCHNXTL                                                      
         B     MUNSCT                                                           
         DROP  RE                                                               
*                                                                               
         ST    R1,DMCB+4                                                        
         L     R0,ASYSD            CLEAR MATCH TABLE                            
         AH    R0,=Y(MATCHTBL-SYSD)                                             
         ST    R0,DMCB                                                          
         GOTO1 QSORT,DMCB,,,A(MTCHNXTL-MTCHLIND),                      +        
               A(MTCHDATE-MTCHSPRD),5,0                                         
*                                                                               
         GOTO1 =A(MTCHBUYS),DMCB,(RC),RR=RELO                                   
*                                                                               
MUNSX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE INVOICE HEADER BASED ON THE INVOICE NUMBER.             
*                                                                               
* ON ENTRY:    (P1)                A(INVOICE NUMBER)                            
*              (P2)                A(EBCDIC ESTIMATE NUMBER)                    
*              SPCLREP             SPECIAL REP FOR THE INVOICE HEADER           
*              INVSTDT             PERIOD START DATE FOR INVOICE HEADER         
***********************************************************************         
VGINVHDR DS    0H                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                                                               
         L     R2,0(R1)            R2 = A(INVOICE NUMBER)                       
         L     R3,4(R1)            R3 = A(EBCDIC ESTIMATE NUMBER)               
*                                                                               
         XC    MINEKEY,MINEKEY     READ THE INVOICE HEADER                      
         MVI   MINEKEY,PIMHDREQ                                                 
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BNE   GHDRNO                                                           
*                                                                               
GHDR10   L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         CLC   PIMINVNO,0(R2)      INVOICE NUMBERS MATCH?                       
         BNE   GHDRNXT                                                          
         CLC   PIMEST,0(R3)        ESTIMATES MATCH?                             
         BNE   GHDRNXT                                                          
         CLC   PIMSREP,SPCLREP     SPECIAL REPS MATCH?                          
         BNE   GHDRNXT                                                          
         CLC   PIMSTDT,INVSTDT     IF ONE DATE MATCHES                          
         BE    GHDRYES             THEN WE GO IT, PERIODS CAN'T CROSS           
*                                                                               
GHDRNXT  GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0                                                         
         BE    GHDR10                                                           
*                                                                               
GHDRNO   B     NO                                                               
*                                                                               
GHDRYES  MVC   LSTHDRSQ,PIMHDRSQ   SAVE LAST HEADER SEQUENCE USED               
         B     YES                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF A LINE ON THE SCREEN HAS BEEN CHANGED.          
* A 'YES' IS RETURNED IF THERE IS ONE.  A 'NO' IF NOT.                          
*                                                                               
* ON ENTRY:    (P1)                A(LINE TO BE CHECKED)                        
*              (P2)                A(LAST LINE TO BE CHECKED)                   
*              (P3)                A(ENTRY FOR LINE IN KEY TABLE)               
*              (P4)                LENGTH OF EACH ENTRY IN KEY TABLE            
*                                                                               
* ON EXIT:     (P1)                A(LINE THAT WAS CHANGED)                     
*              (P2)                0 = NO DATA ON CHANGED LINE                  
*              (P3)                A(ENTRY FOR LINE IN KEY TABLE)               
***********************************************************************         
VFINDLIN DS    0H                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R5,8(R1)            R5 = A(ENTRY FOR CHANGED LINE)               
         L     R6,12(R1)           R6 = LENGTH OF ENTRIES                       
*                                                                               
         SR    R0,R0                                                            
*                                                                               
FLN10    LR    RE,R2                                                            
*                                                                               
         USING SCRLIN1D,R2         RF = A(LAST FLDHDR OF LINE)                  
         LA    RF,SLN1ESTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    FLNLOOP                                                          
         USING SCRLIN2D,R2                                                      
         LA    RF,SLN2ESTH                                                      
         DROP  R2                                                               
*                                                                               
FLNLOOP  TM    4(RE),X'20'         IF ANY COLUMNS CHANGED ON THE LINE           
         BZ    FLNYES              THEN RETURN A 'YES'                          
         CR    RE,RF               CHECKED THE LAST COLUMN ON LINE?             
         BNL   FLNNEXT             YES                                          
*                                                                               
         CLI   0(RE),0             CAN WE BUMP TO NEXT COLUMN ?                 
*NOP*    BE    FLNNEXT             NO - TRY NEXT LINE                           
         BE    FLNYES15            NO - GET OUT                                 
*                                                                               
         IC    R0,0(RE)            BUMP TO THE NEXT COLUMN                      
         AR    RE,R0                                                            
         B     FLNLOOP             AND CHECK IT                                 
*                                                                               
FLNNEXT  DS    0H                                                               
         CR    R2,R3               ANYMORE LINES?                               
         BNL   FLNNO               NO MORE, RETURN A 'NO', NO CHANGES           
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)      R2 = A(NEXT LINE)                            
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)                                                   
*                                                                               
         AR    R5,R6               BUMP TO NEXT ENTRY IN LIST                   
*                                                                               
         B     FLN10                                                            
*                                                                               
FLNYES   ST    R2,DMCB             SAVE THE A(CHANGED LINE)                     
         ST    R5,DMCB+8           SAVE A(ENTRY FOR CHANGED LINE)               
*                                                                               
         LR    RE,R2                                                            
         USING SCRLIN1D,R2         RF = A(LAST FLDHDR OF LINE)                  
         LA    RF,SLN1ESTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    RF,SLN2ESTH                                                      
         DROP  R2                                                               
*                                                                               
FLNYES10 DS    0H                  FIRST CHECK FOR END OF SCREEN                
*                                  TO PREVENT LOOPING                           
         CLI   0(RE),0                                                          
         BE    FLNYES15                                                         
*                                                                               
         CLI   5(RE),0             DATA IN COLUMNS OF CHANGED LINE?             
         BNE   FLNYESX                                                          
*                                                                               
         CR    RE,RF               CHECKED THE LAST COLUMN ON LINE?             
         BL    *+14                                                             
FLNYES15 XC    DMCB+4(4),DMCB+4    YES, NO DATA IN CHANGED LINE                 
         B     FLNYESX                                                          
*                                                                               
         IC    R0,0(RE)            NO, BUMP TO THE NEXT COLUMN                  
         AR    RE,R0                                                            
         B     FLNYES10            AND CHECK IT                                 
*                                                                               
FLNYESX  B     YES                                                              
*                                                                               
FLNNO    B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATES THE NEXT CORRECTED DETAIL SEQ # TO BE USED            
*                                                                               
* ON ENTRY:    MELEM               CONTAINS ELEMENT NEEDING SEQUENCE #          
*              LSTHDRSQ            SEQUENCE NUMBER OF HEADER FOR DETAIL         
***********************************************************************         
VNXDTLSQ DS    0H                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         MVC   MELEMX,MELEM                                                     
         SR    R2,R2               R2 = LOWER SEQUENCE                          
*                                                                               
         L     R6,MINELEM          R6 = A(MINIO ELEMENT)                        
         USING PIMDTLEL,R6                                                      
*                                                                               
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
*                                                                               
NXDTL10  CLI   MINERR,0                                                         
         BE    NXDTL20                                                          
         CLI   MINERR,MINEEOF                                                   
         BE    NXDTL25                                                          
         DC    H'0'                                                             
*                                                                               
NXDTL20  CLI   PIMDTLEL,PIMDTLEQ   IF NOT AN INVOICE DETAIL                     
         BNE   NXDTL25                                                          
         CLC   PIMDTLS1,LSTHDRSQ   OR HEADER SEQUENCE DOESN'T MATCH             
         BE    NXDTL30                                                          
*                                                                               
NXDTL25  AH    R2,=H'10'           THEN ADD 10 TO PREV SEQUENCE NUMBER          
         STH   R2,HALF                                                          
         MVC   MELEM,MELEMX                                                     
         B     NXDTLYES                                                         
*                                                                               
NXDTL30  CLC   PIMIDATE,MELEMX+PIMIDATE-PIMDTLEL                                
         BNH   NXDTL40                                                          
         ZICM  R0,PIMDTLS2,2       R0 = HIGHER SEQUENCE                         
         AR    R0,R2               GET A SEQUENCE BETWEEN THE TWO               
         SRL   R0,1                                                             
*                                                                               
         CR    R0,R2               IF NEW SEQ IS SAME AS LOWER SEQ              
         BNE   *+14                                                             
         MVC   MELEM,MELEMX                                                     
         B     NXDTLNO             THEN RETURN A 'NO'                           
*                                                                               
         STH   R0,HALF                                                          
         MVC   MELEM,MELEMX                                                     
         B     NXDTLYES            ELSE RETURN A 'YES'                          
*                                                                               
NXDTL40  ZICM  R2,PIMDTLS2,2       R2 = NEW LOWER SEQUENCE                      
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         B     NXDTL10                                                          
*                                                                               
NXDTLYES B     YES                                                              
NXDTLNO  B     NO                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RESEQUENCES THE INVOICE DETAIL ELEMENTS                          
*                                                                               
* CAUTION:     DUB GETS CLOBBERED                                               
*                                                                               
* ON ENTRY:    (P1)                A(TABLE OF DETAILS SHOWN ON SCREEN)          
*              (P2)                A(SECOND TABLE)                              
***********************************************************************         
VRSEQNCE DS    0H                                                               
         L     R2,0(R1)            A(FIRST TABLE) STORED IN DUB                 
         ST    R2,DUB                  LEN(FIRST TABLE) = L'PINVMINI            
         CLI   ACTNUM,ACTCHECK     DO WE NEED TWO TABLES?                       
         BNE   *+12                                                             
         L     R4,4(R1)            YES, A(SECOND TABLE) STORED IN DUB+4         
         ST    R4,DUB+4                LEN(SECOND TABLE) = L'PBUYKEY            
*                                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                  PHASE 1 - NUMBER SEQUENCES IN ORDER          
         LA    R0,1                1ST INVOICE DETAIL ELEMENT                   
         MVC   MELEMX,MELEM                                                     
         L     R6,MINELEM          R6 = A(MINIO ELEMENT)                        
         USING PIMDTLEL,R6                                                      
         SR    R3,R3                                                            
*                                                                               
RSEQ10   XC    MINEKEY,MINEKEY     SET MINIO ELEMENT KEY TO READ FIRST          
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         STCM  R0,3,MINEKEY+2                                                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BNE   RSEQ50              DONE WITH FIRST PHASE IF NO MORE             
*                                                                               
         CLI   PIMDTLEL,PIMDTLEQ   IF NOT INVOICE DETAIL WITH SAME              
         BNE   RSEQ50                  HEADER SEQUENCE                          
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BNE   RSEQ50              THEN DONE WITH FIRST PHASE                   
         ICM   R3,3,PIMDTLS2       R3 = DETAIL SEQUENCE # WE FOUND              
*                                                                               
RSEQ13   CLI   0(R2),0                                                          
         BE    RSEQ16                                                           
*                                                                               
         CLC   PIMDTLS2,2(R2)                                                   
         BL    RSEQ16                                                           
         BH    *+16                                                             
         STCM  R0,3,2(R2)                                                       
         LA    R2,L'PINVMINI(R2)                                                
         B     RSEQ16                                                           
         LA    R2,L'PINVMINI(R2)                                                
         B     RSEQ13                                                           
*                                                                               
RSEQ16   CLI   ACTNUM,ACTCHECK                                                  
         BNE   RSEQ19                                                           
*                                                                               
         OC    DUB+4(4),DUB+4      IF NO SECOND TABLE                           
         BZ    RSEQ19              THEN DON'T BOTHER                            
*                                                                               
         L     R4,DUB+4                                                         
*                                                                               
RSEQ16A  OC    0(L'PBUYKEY,R4),0(R4) NO BUY KEY?                                
         BZ    RSEQ19              NONE                                         
*                                                                               
         LA    R4,L'PBUYKEY(R4)                                                 
*                                                                               
RSEQ16B  CLI   L'PINVMINI(R4),0    BUY KEY AGAIN?                               
         BE    *+12                YES                                          
         LA    R4,L'PBUYKEY(R4)                                                 
         B     RSEQ16B             YES                                          
*                                                                               
         OC    0(L'PINVMINI,R4),0(R4)  ANY CORRECTION ELEMENT?                  
         BNZ   *+12                    YES                                      
RSEQ16C  LA    R4,L'PBUYKEY(R4)                                                 
         B     RSEQ16A             NO                                           
*                                                                               
         CLC   PIMDTLS2,2(R4)      YES, IS THIS THE ONE?                        
         BNE   RSEQ16C             NO                                           
*                                                                               
         STCM  R0,3,2(R4)          YES, CHANGE SEQ # TO NEW ONE                 
*                                                                               
RSEQ19   STCM  R0,3,PIMDTLS2       RENUMBER THE SEQUENCE                        
*                                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RSEQ20   TM    PIMDSTAT,X'04'      ANY COMMENTS?                                
         BZ    RSEQ40              NONE                                         
*                                                                               
         XC    MINEKEY,MINEKEY     CHANGE COMMENTS WITH THAT DETAIL SEQ         
         MVI   MINEKEY,PIMCOMEQ        TO THE NEW DETAIL SEQ                    
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         STCM  R3,3,MINEKEY+2                                                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0            COMMENT HAD BETTER EXIST IF FLAGGED          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PIMCOMEL,R6                                                      
         STCM  R0,3,PIMCOMS2                                                    
*                                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RSEQ40   AH    R0,=H'1'            GET NEXT ELEMENT                             
         B     RSEQ10                                                           
         EJECT                                                                  
RSEQ50   SH    R0,=H'1'            PHASE 2 - MULTIPLY SEQ NUMBERS BY 10         
         SH    R2,=Y(L'PINVMINI)                                                
*                                                                               
RSEQ60   XC    MINEKEY,MINEKEY     SET MINIO ELEMENT KEY TO READ FIRST          
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         STCM  R0,3,MINEKEY+2                                                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R3,R0               MULTIPLY SEQUENCES BY 10                     
         MH    R3,=H'10'                                                        
*                                                                               
RSEQ62   CLI   0(R2),0                                                          
         BE    RSEQ66                                                           
*                                                                               
         USING PIMDTLEL,R6                                                      
         CLC   PIMDTLS2,2(R2)                                                   
         BH    RSEQ66                                                           
         BL    RSEQ64                                                           
         STCM  R3,3,2(R2)                                                       
         L     RF,DUB              IF WE CHANGED ALL OF THOSE                   
         CR    R2,RF                   THAT WERE DISPLAYED                      
         BE    *+8                 THEN NO NEED TO GO BACK MORE                 
         SH    R2,=Y(L'PINVMINI)                                                
         B     RSEQ66                                                           
RSEQ64   L     RF,DUB              IF WE CHANGED ALL OF THOSE                   
         CR    R2,RF                   THAT WERE DISPLAYED                      
         BE    RSEQ66              THEN NO NEED TO GO BACK MORE                 
         SH    R2,=Y(L'PINVMINI)                                                
         B     RSEQ62                                                           
*                                                                               
RSEQ66   CLI   ACTNUM,ACTCHECK                                                  
         BNE   RSEQ68                                                           
*                                                                               
         OC    DUB+4(4),DUB+4      IF NO SECOND TABLE                           
         BZ    RSEQ68              THEN DON'T BOTHER                            
*                                                                               
         L     R4,DUB+4            R4 = A(SECOND TABLE)                         
*                                                                               
RSEQ66A  OC    0(L'PBUYKEY,R4),0(R4) NO BUY KEY?                                
         BZ    RSEQ68              NONE                                         
*                                                                               
         LA    R4,L'PBUYKEY(R4)                                                 
*                                                                               
RSEQ66B  CLI   L'PINVMINI(R4),0    BUY KEY AGAIN?                               
         BE    *+12                YES                                          
         LA    R4,L'PBUYKEY(R4)                                                 
         B     RSEQ66B             YES                                          
*                                                                               
         OC    0(L'PINVMINI,R4),0(R4)  ANY CORRECTION ELEMENT?                  
         BNZ   *+12                    YES                                      
RSEQ66C  LA    R4,L'PBUYKEY(R4)                                                 
         B     RSEQ66A             NO                                           
*                                                                               
         CLC   PIMDTLS2,2(R4)      YES, IS THIS THE ONE?                        
         BNE   RSEQ66C             NO                                           
*                                                                               
         STCM  R3,3,2(R4)          YES, CHANGE SEQ # TO NEW ONE                 
*                                                                               
RSEQ68   STCM  R3,3,PIMDTLS2       RENUMBER THE SEQUENCE                        
*                                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RSEQ70   TM    PIMDSTAT,X'04'      ANY COMMENTS?                                
         BZ    RSEQ80              NONE                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMCOMEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         STCM  R0,3,MINEKEY+2                                                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PIMCOMEL,R6                                                      
         STCM  R3,3,PIMCOMS2                                                    
*                                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RSEQ80   BCT   R0,RSEQ60           GET NEXT ELEMENT                             
*                                                                               
RSEQX    MVC   MELEM,MELEMX                                                     
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATES THE GROSS, NET, CD FROM THE INVOICE ITEM              
* STORED IN MINELEM                                                             
*                                                                               
* ON ENTRY:    MINELEM             CONTAINS THE A(INVOICE ITEM)                 
*              (P1)                A(GETINSA)                                   
*                                                                               
* ON EXIT:     GETINSA             ORDERED DATA IN GETINSA IS SET               
***********************************************************************         
VDTLGROS DS    0H                                                               
         L     R3,0(R1)            STORE WHAT WE CALCULATED IN GETINSA          
         XC    0(200,R3),0(R3)         CLEAR IT FIRST                           
*                                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         L     R2,MINELEM                                                       
         DROP  R5                                                               
         USING PIMDTLEL,R2                                                      
*                                                                               
         ZAP   FULL,PUBAGYCM                                                    
*                                                                               
         TM    PIMDSTAT,X'10'      DETAIL IS MATCHED                            
         BZ    DTLG00                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PBUYKEY,R1                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
* FIX FOR STEREO (1/31/01) BELOW                                                
         OC    QCLT,SPACES                                                      
         OC    QPRD,SPACES                                                      
* FIX FOR STEREO (1/31/01) ABOVE                                                
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,QPRD                                                    
*                                                                               
         CLC   =C'***',QPRD        QPRD CONTAINS PRD FROM SCREEN                
         BNE   *+26                                                             
         MVC   PBUYKPRD,PIMSPRD                                                 
         CLC   PIMCPRD,=CL3' '     IF CORRECTED PRODUCT PRESENT                 
         BNH   *+10                                                             
         MVC   PBUYKPRD,PIMCPRD       USE IT                                    
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         MVC   PBUYKZON,PIMBZONE                                                
         MVC   PBUYKEDT,PIMBEDTN                                                
         MVC   PBUYKDAT,PIMBDATE                                                
         MVC   PBUYKEST,PIMBEST                                                 
         MVC   PBUYKLIN,PIMBLINE                                                
         DROP  R1                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'  RESET DELETED RECORDS                       
         LA    R1,KEY                                                           
         USING PBUYKEY,R1                                                       
         CLC   PBUYKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    PBUYLEN,X'80'       CONTROL BYTE FOR KEY (DELETED?)              
         BNZ   DTLGX               THEN NOTHING TO CALCULATE                    
         DROP  R1                                                               
*                                                                               
         MVI   ERROR,DATALOK       RECORDS LOCKED FOR OFFLINE PROCESS           
         BRAS  RE,TSTLOCK          LOCKED ?                                     
         BNE   TRAPERR             YES                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         ZAP   FULL,PBDACP         SAVE THE BUY'S AGENCY COMMISSION             
         MVC   SVPBDGST,PBDGST       AND THE GST INDICATOR                      
         DROP  R6                                                               
*                                                                               
DTLG00   L     R6,AIO                                                           
         LR    R0,R6                                                            
         LH    R1,=Y(LIOS)                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING PBUYREC,R6                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'      BUY RECORD CODE                              
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,QPRD                                                    
*                                                                               
         CLC   =C'***',QPRD        PRD FROM SCREEN                              
         BNE   *+26                                                             
         MVC   PBUYKPRD,PIMSPRD                                                 
         CLC   PIMCPRD,=CL3' '     IF CORRECTED PRODUCT PRESENT                 
         BNH   *+10                                                             
         MVC   PBUYKPRD,PIMCPRD       USE IT                                    
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         MVC   PBUYKDAT,PIMIDATE                                                
         MVC   PBUYKEST,PIMIEST                                                 
         MVC   PBUYLEN,=X'0021'    LENGTH OF NO ELEMENT RECORD                  
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING PBDELEM,R5                                                       
         MVI   PBDELEM,X'20'                                                    
         MVI   PBDELEM+1,X'74'                                                  
         ZAP   PBDACP,FULL                                                      
         MVC   PBDBDATE,PIMIDATE                                                
*                                                                               
         TM    PIMDSTAT,X'40'      ANY CASH DISCOUNT?                           
         BZ    *+14                                                             
         ZAP   PBDCD,=P'0'         NONE                                         
         B     *+10                                                             
         MVC   PBDCD,PUBCASHD      YES, USE PUB REC'S CASH DISCOUNT             
*                                                                               
         CLI   SVAGNATL,C'C'       CANADIAN AGENCY?                             
         BNE   *+8                                                              
         OI    PBDCNDA,X'80'       YES                                          
*                                                                               
         MVC   PBDCOS,PIMCOST                                                   
         MVC   PBDCOSIN,PIMCSIND                                                
*                                                                               
         MVI   PBDCOSTY,C'T'       ASSUME TOTAL RATE FIRST                      
         TM    PIMDSTAT,X'80'                                                   
         BZ    *+8                                                              
         MVI   PBDCOSTY,C'U'       DETAIL HAS UNIT RATE                         
*                                                                               
*NOP*    MVC   PBDGST,PUBGSTAX     NO GOOD FOR GST=X BUYS                       
         MVC   PBDGST,SVPBDGST     INSTEAD, USE FIELD FROM BUYREC               
         MVC   PBDPDATE,PIMIDATE                                                
         MVC   PBDPRCOS,PIMPREM                                                 
*        MVC   PBDSTAT                                                          
*                                                                               
         CLC   PIMIDATE,PUBTAXD1   USE THE APPROPRIATE TAX RATE BASED           
         BL    DTLG10                  ON THE EFFECTIVE DATE                    
         MVC   PBDTAX,PUBTAXR1                                                  
*******  B     DTLG10        must keep looking                                  
         CLC   PIMIDATE,PUBTAXD2                                                
         BL    DTLG10                                                           
         MVC   PBDTAX,PUBTAXR2                                                  
******** B     DTLG10         must keep looking                                 
         CLC   PIMIDATE,PUBTAXD3                                                
         BL    DTLG10                                                           
         MVC   PBDTAX,PUBTAXR3                                                  
*                                                                               
DTLG10   MVC   PBDUIND,PIMUIND                                                  
         MVC   PBDUNITS,PIMUNITS                                                
*        MVC   PBDWTSUM                                                         
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         GOTO1 GETINS,DMCB,AIO,(R3),PBUYKPRD,INVSTDT,=C'GST'                    
         DROP  R2                                                               
         DROP  R6                                                               
*                                                                               
         L     R1,DMCB+16                                                       
         USING PVALUES,R3                                                       
         MVC   GVALUES(GSTTAXBL+L'GSTTAXBL-GVALUES),0(R1)                       
         DROP  R3                                                               
*                                                                               
DTLGX    B     XIT                                                              
*                                                                               
SVPBDGST DS    X                   TEMP STORE - GST FIELD FROM BUYREC           
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE REP.                                               
***********************************************************************         
VREP     DS    0H                                                               
         GOTO1 GETFLD                                                           
*                                                                               
         MVI   ERROR,INVLREP                                                    
         LTR   R0,R0                                                            
         BZ    TRAPERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PREPKEY,R4                                                       
         MVC   PREPKAGY,AGENCY                                                  
         MVC   PREPKMED,QMED                                                    
         MVI   PREPKRCD,X'11'                                                   
         CVD   R0,DUB                                                           
         UNPK  PREPKREP,DUB                                                     
         OI    PREPKREP+3,X'F0'                                                 
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'PREPKEY),KEYSAVE   IF REP RECORD DOESN'T EXIST             
         BNE   TRAPERR                  THEN ERROR                              
*                                                                               
         MVC   8(L'PREPKREP,R2),PREPKREP                                        
         OI    6(R2),X'80'                                                      
*                                                                               
VREPX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE DETAIL ELEMENT                                      
*                                                                               
* ON ENTRY:    MINELEM             A(MINIO ELEMENT)                             
*              (R2)                LINE ON WHICH ELEMENT WILL BE SHOWN          
*              FULL                A(BUY KEY)                                   
***********************************************************************         
VDISPDTL DS    0H                                                               
         GOTO1 =A(SHOWDTL),DMCB,(RC),RR=RELO                                    
         B     XIT                                                              
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
*              LOCAL XIT/ERROR ROUTINES                                         
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
YES      SR    RC,RC               SET CC EQ                                    
NO       LTR   RC,RC               SET CC NEQ                                   
EXIT     EQU   *                                                                
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
RELO     DS    F                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(PUBVAL)                                                        
         DC    V(RECUP)                                                         
         DC    V(SRCHCALL)                                                      
         DC    V(PUBFLOAT)                                                      
         DC    V(PUBEDIT)                                                       
         DC    V(PPGETADR)                                                      
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QMINIO)                                                      
         DC    AL1(QQSORT)                                                      
         DC    AL1(QGETINS)                                                     
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                                                               
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
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                  CL2 ACCESS BITS (ON = RESTRICTED)            
*                                                                               
RECACT2  DC    X'02',C'CHECK   ',AL1(07,07,00),X'8000'                          
         DC    X'02',C'UPDATE  ',AL1(08,08,00),X'8000'                          
         DC    X'02',C'SUPERCK ',AL1(09,09,00),X'8000'                          
         DC    X'02',C'LIST    ',AL1(10,10,00),X'8000'                          
         DC    X'02',C'REPORT  ',AL1(12,12,00),AL2(0)                           
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
         DC    X'03',AL1(01,07),X'FB02000080',C'    '  INVOICE  CHECK           
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,08),X'FB02000080',C'    '  INVOICE  UPDATE          
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,09),X'FB02000080',C'    '  INVOICE  SUPERCK         
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,10),X'FE01000080',C'    '  INVOICE  LIST            
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,12),X'DE25000038',C'IMIM'  INVOICE  REPORT          
         DC    AL2(0)                                                           
*                                                                               
         DC    X'FF'                                                            
*****    EJECT                                                                  
*****    LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MATCHES THE BUYS TO THE DETAILS IN OUR MATCH TABLE               
* CALLED BY VMTCHUNS.                                                           
***********************************************************************         
MTCHBUYS DS    0H                                                               
         NMOD1 0,**MBUY**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         L     R3,ASYSD            POINT TO BEGINNING OF MATCH TABLE            
         AH    R3,=Y(MATCHTBL-SYSD)                                             
         USING MTCHLIND,R3                                                      
*                                                                               
         XC    KEY,KEY                                                          
         XC    OLDZONED,OLDZONED                                                
*                                                                               
         LA    R2,KEY              SET KEY TO SAME YEAR AND MONTH               
         USING PBUYKEY,R2              AS THE INVOICE                           
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
*                                                                               
         MVC   PBUYKPRD,MTCHSPRD                                                
         CLC   =C'***',CHKPRD       PRODUCT VARIOUS                             
         BE    MBUY00                                                           
         CLC   =C'***',LSTPRD                                                   
         BE    MBUY00                                                           
         MVC   PBUYKPRD,QPRD                                                    
*                                                                               
MBUY00   MVC   PBUYKPUB(L'BPUB),BPUB                                            
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON                                             
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
*                                                                               
MBUYHIGH L     R3,ASYSD            POINT TO BEGINNING OF MATCH TABLE            
         AH    R3,=Y(MATCHTBL-SYSD)                                             
         USING MTCHLIND,R3                                                      
         GOTO1 HIGH                GET FIRST BUY RECORD                         
*                                                                               
MBUY20   L     R3,ASYSD            POINT TO BEGINNING OF MATCH TABLE            
         AH    R3,=Y(MATCHTBL-SYSD)                                             
         USING MTCHLIND,R3                                                      
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE                                    
         BNE   MBUYX               DONE IF NOT SAME BASIC INFORMATION           
         CLC   =C'***',CHKPRD                                                   
         BE    MBUY22                                                           
         CLC   =C'***',LSTPRD                                                   
         BE    MBUY22                                                           
         CLC   KEY(PBUYKPUB-PBUYKEY),KEYSAVE   PRODUCT MATCH?                   
         BNE   MBUYX               IT MUST BE IF NOT VARIOUS PRD                
*                                                                               
MBUY22   CLC   PBUYKPUB,BPUB                                                    
         BE    MBUY25                                                           
         BL    MBUYSEQ                                                          
         CLC   =C'***',CHKPRD      IF  PRD VARIOUS...                           
         BE    *+14                ...THEN BUMP TO NEXT PUB                     
         CLC   =C'***',LSTPRD      NOT PRD VARIOUS...                           
         BNE   MBUYX               ...THEN PUB MUST BE SAME                     
         MVI   PBUYKPUB,X'FF'      ELSE BUMP TO NEXT PRD                        
         B     MBUYHIGH                                                         
*                                                                               
MBUY25   CLC   PBUYKZON(2),BPUB+4                                               
         BE    MBUY30              ZON/ED MUST BE EQUAL ....                    
         TM    GLOBFLG1,X'80'                                                   
         BNZ   MBUY30                                                           
         CLC   =C'***',CHKPRD                                                   
         BE    MBUY30                                                           
         CLC   =C'***',LSTPRD                                                   
         BNE   MBUYX                                                            
*                                                                               
MBUY30   CLC   PBUYKDAT,INVSTDT    BUY'S DATE BELOW PERIOD START DATE?          
         BNL   MBUY40                                                           
         TM    GLOBFLG1,X'80'      YES, UNDER ALL ZONES AND EDITIONS?           
         BNZ   MBUY35              CAN'T BE POSSIBLE IF NOT                     
         CLC   =C'***',CHKPRD                                                   
         BE    *+14                                                             
         CLC   =C'***',LSTPRD                                                   
         BNE   MBUYX                                                            
MBUY35   MVC   PBUYKDAT,INVSTDT                                                 
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     MBUYHIGH            YES, CHECK THIS PUB FROM START DATE          
*                                                                               
MBUY40   CLC   PBUYKDAT,INVENDDT   BUY'S DATE AFTER PERIOD?                     
         BNH   MBUY45                                                           
         TM    GLOBFLG1,X'80'      YES, UNDER ALL ZONES AND EDITIONS?           
         BNZ   MBUY42              DONE IF NOT                                  
         CLC   =C'***',CHKPRD                                                   
         BE    *+14                                                             
         CLC   =C'***',LSTPRD                                                   
         BNE   MBUYX                                                            
MBUY42   MVI   PBUYKDAT,X'FF'                                                   
         B     MBUYHIGH            YES, FORCE KEY TO READ NXT ZONE/EDTN         
*                                                                               
MBUY45   OC    BEST,BEST                                                        
         BZ    MBUY50                                                           
*                                                                               
         CLC   PBUYKEST,BEST                                                    
         BNE   MBUYSEQ                                                          
*                                                                               
MBUY50   OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE   (ACTIVE PRODUCT)         
         BNZ   MBUYSEQ             THEN SKIP THE RECORD    (ASK MEL)            
         DROP  R2                                                               
*                                                                               
         BRAS  RE,TSTLOCK          LOCKED ?                                     
         BE    MBUY58              NO                                           
         MVI   ERROR,DATALOK       RECORDS LOCKED FOR OFFLINE PROCESS           
         GOTO1 ERREX                                                            
*                                                                               
MBUY58   MVI   RDUPDATE,C'Y'       ALWAYS READ FOR UPDATE HERE                  
         GOTO1 GETREC              BUY DATE < INVOICE DATE                      
MBUY60   L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    MBUYSEQ             YES                                          
*                                                                               
         TM    PBDSTAT,X'40'       MATCHED TO A DETAIL ALREADY?                 
         BNZ   MBUYSEQ             YES                                          
*****                                                                           
* BUYS FROM HERE ON ARE ALL UNMATCHED                                           
*****                                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        SEE IF THIS BUY IS PAID BUT NOT              
         BAS   RE,GETEL                MATCHED                                  
MBUY61   BNE   MBUY62                                                           
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE     DON'T COUNT THIS UNMATCHED PAID BUY          
         BNZ   MBUYSEQ                                                          
         BAS   RE,NEXTEL                                                        
         B     MBUY61                                                           
*                                                                               
MBUY62   L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLC   PBUYKDAT,MTCHIDAT   DATE SAME AS DETAIL'S?                       
         BE    MBUY80              YES                                          
*                                                                               
         BH    MBUY70              NO, >, CHECK NEXT INVOICE DETAIL             
         TM    GLOBFLG1,X'80'         ALL ZONES/ED                              
         BNZ   MBUY70              GET NEXT INV                                 
         CLC   =C'***',CHKPRD      OR PRD VAR?                                  
         BE    MBUY70              GET NEXT INV                                 
         CLC   =C'***',LSTPRD      OR PRD VAR?                                  
         BE    MBUY70              GET NEXT INV                                 
         CLI   PBDBFD,C'W'         WEEK OF THAT BUY DATE                        
         BE    MBUY65                                                           
         CLI   PBDBFD,C'B'             OR BEST FOOD DATE?                       
         BE    MBUY65                                                           
         OI    MNIOFLAG,X'20'      THIS BUY DIDN'T MATCH                        
         B     MBUYSEQ             NO, CHECK NEXT BUY                           
*                                                                               
MBUY65   GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,DUB)   ADD 7 TO BUY DATE             
         GOTO1 ADDAY,DMCB,DUB,BLOCK,7                                           
         GOTO1 DATCON,DMCB,(0,BLOCK),(3,DUB)                                    
*                                                                               
         CLC   MTCHIDAT,DUB        INVOICE DATE IN THAT 7 DAY WEEK?             
         BL    MBUY80              YES                                          
         OI    MNIOFLAG,X'20'      THIS BUY DIDN'T MATCH                        
         B     MBUYSEQ             NO, GET NEXT BUY                             
*                                                                               
MBUY70   LA    R3,MTCHNXTL         GET NEXT ENTRY IF DATE > DETAIL              
         OC    MTCHSEQ,MTCHSEQ     NO MORE ENTRIES?                             
         BNZ   MBUY62              THERE ARE MORE, LOOP BACK                    
*                                                                               
         TM    GLOBFLG1,X'80'                                                   
         BZ    *+8                                                              
         B     MBUYSEQ                                                          
*                                                                               
         CLC   =C'***',CHKPRD                                                   
         BE    *+14                                                             
         CLC   =C'***',LSTPRD                                                   
         BNE   *+8                                                              
         B     MBUYSEQ                                                          
*                                                                               
         OI    MNIOFLAG,X'20'      BUY WASN'T MATCHED                           
         B     MBUYX               NOT ALL ZON/ED OR PUB,ALL SO EXIT            
*                                                                               
MBUY80   CLC   CHKPRD,=C'***'                                                   
         BE    *+14                                                             
         CLC   LSTPRD,=C'***'                                                   
         BNE   MBUY82                                                           
         CLC   PBUYKPRD,MTCHSPRD                                                
         BE    MBUY82                                                           
         LA    R3,MTCHNXTL         GET NEXT ENTRY IF DATE > DETAIL              
         OC    MTCHSEQ,MTCHSEQ     NO MORE ENTRIES?                             
         BNZ   MBUY60              THERE ARE MORE, LOOP BACK                    
         B     MBUYSEQ                                                          
*                                                                               
MBUY82   MVI   ELCODE,X'80'        SEE IF THERE IS A SPECIAL REP ELEM           
         BAS   RE,GETEL                                                         
         BE    MBUY85                                                           
         OC    SPCLREP,SPCLREP     IF THERE ISN'T SEE IF WE NEED IT             
         BZ    MBUY100                                                          
         OI    MNIOFLAG,X'20'      THIS BUY DIDN'T MATCH                        
         B     MBUYSEQ             WE DO NEED IT, CHECK NEXT DETAIL             
*                                                                               
         USING PBSREPEL,R6                                                      
MBUY85   CLC   PBSREP,SPCLREP      SAME SPECIAL REP?                            
         BE    *+12                                                             
         OI    MNIOFLAG,X'20'      THIS BUY DIDN'T MATCH                        
         B     MBUYSEQ             NO, CHECK NEXT DETAIL                        
*                                                                               
MBUY100  L     R6,AIO              CHECK THE DETAIL TABLE                       
         GOTO1 =A(CKDTLTBL),DMCB,(RC),(R3),RR=RELO                              
*                                                                               
MBUYSEQ  GOTO1 SEQ                 GET NEXT BUY KEY                             
         B     MBUY20                                                           
*                                                                               
MBUYX    DS    0H                                                               
         TM    MNIOFLAG,X'20'                                                   
         BNZ   XIT                                                              
         BAS   RE,CKMTCHTB         CHECK MATCH TABLE IF ALL MATCHED             
         B     XIT                                                              
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS MATCH TABLE TO SEE IF ALL DETAILS HAVE A BUYLINE          
***********************************************************************         
CKMTCHTB NTR1                                                                   
         L     R1,ASYSD            CHECK IF BUY HAS BEEN CORRECTED              
         AH    R1,=Y(MATCHTBL-SYSD)                                             
         USING MTCHLIND,R1                                                      
*                                                                               
CKMTLP   OC    MTCHSEQ,MTCHSEQ                                                  
         BZ    CKMTX                                                            
*                                                                               
         CLI   MTCHLINE,0                                                       
         BNE   CKMTNXT                                                          
         OI    MNIOFLAG,X'20'      THIS DETAIL IS UNMATCHED                     
         B     CKMTX                                                            
*                                                                               
CKMTNXT  LA    R1,MTCHNXTL                                                      
         B     CKMTLP                                                           
*                                                                               
CKMTX    B     XIT                                                              
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS THE DETAIL MATCH TABLE                                    
***********************************************************************         
CKDTLTBL DS    0H                                                               
         NMOD1 0,**CHKD**                                                       
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                                                               
         USING MTCHLIND,R3                                                      
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         OC    BEST,BEST           IF ESTIMATE NOT REQUIRED                     
         BZ    CHKD20              THEN SEE IF USER PUT IN AN ESTIMATE          
*                                                                               
CHKD10   CLC   PBUYKEST,MTCHIEST   ESTIMATES DON'T MATCH?                       
         BNE   CHKDNXT             CHECK AGAINST NEXT DETAIL                    
         B     CHKD30                                                           
*                                                                               
CHKD20   OC    MTCHIEST,MTCHIEST   IF THERE IS A ESTIMATE ENTERED WHEN          
         BNZ   CHKD10                  WE DON'T NEED IT, CHECK BUY EST          
*                                                                               
CHKD30   MVC   BLOCK(L'PBDSPACE),PBDSPACE                                       
         CLI   QMED,C'O'           OUTDOORS NEEDS IT ONLY IF SPACE              
         BNE   *+12                    IS NOT THE 3 PACKED FIELDS               
         CLI   PBDSPACE,X'FF'                                                   
         BE    CHKD30A             3 PACKED FIELDS                              
*                                                                               
         OC    BLOCK(L'PBDSPACE),BLOCK                                          
         BZ    CHKD30A             UNLESS PBDSPACE IS NULLS                     
         OC    BLOCK(L'PBDSPACE),SPACES                                         
*                                                                               
CHKD30A  CLI   SCRTYPE,C'N'        TO AVOID COMPARING SPACE                     
         BNE   CHKD30B             ...MUST BE NEWSPAPER                         
*                                                                               
         CLI   PBDUIND,X'93'       ...AND LINES OR                              
         BE    CKD30A                                                           
         CLI   PBDUIND,C'L'                                                     
         BE    CKD30A                                                           
         CLI   PBDUIND,C'I'                                                     
         BE    CKD30A                                                           
         CLI   PBDUIND,X'89'       ...INCHES                                    
         BNE   CHKD30BA                                                         
*                                                                               
CKD30A   CLC   PBDUIND,MTCHUIND    SAME INDICATOR?                              
         BNE   CHKD30BA                                                         
         CP    PBDUNITS,MTCHUNIT   SAME # OF INCHES OR LINES?                   
         BNE   CHKD30BA                                                         
         CP    PBDCLMS,MTCHCLMS    SAME # OF COLUMNS?                           
         BE    CHKD37                                                           
*                                                                               
CHKD30BA CLC   MTCHSPCE,SPACES     IF NOTHING ENTERED,                          
         BE    CHKD31             ...THEN BEING EQUAL IS MEANINGLESS            
         CLC   MTCHSPCE(8),BLOCK   NEWSPAPER SPACE IS ONLY 8 BYTES              
         BNE   CHKD31                                                           
         BE    *+14                                                             
*                                                                               
CHKD30B  CLC   MTCHSPCE,BLOCK      DOES SPACE MATCH?                            
         BNE   CHKD31                                                           
         CLI   BLOCK,X'F0'         CHARACTER IS A DIGIT?                        
         BL    CHKD37              NOT A DIGIT, DON'T CHECK UNITS               
         B     CHKD34                                                           
*                                                                               
CHKD31   CLI   QMED,C'O'           SEE IF OUTDOORS                              
         BNE   CHKD60                                                           
*                                                                               
         CLI   MTCHSPCE,X'FF'      IF DETAIL DOESN'T HAVE SHOW/REG/ILL          
         BE    CHKD60                                                           
         CP    PBDSHOW,=P'0'       THEN SEE IF BUY HAS IT                       
         BNE   CHKD60                                                           
         CP    PBDREG,=P'0'                                                     
         BNE   CHKD60                                                           
         CP    PBDILLUM,=P'0'                                                   
         BNE   CHKD60              WON'T MATCH IF BUY HAS IT                    
*                                                                               
         MVI   ELCODE,X'66'        IF BUY DOESN'T HAVE A COMMENT                
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
CHKD32   L     R6,AIO                                                           
         B     CHKD60                                                           
*                                                                               
         USING PCOMELEM,R6                                                      
         ZIC   R1,PCOMELEM+1                                                    
         SH    R1,=H'2'                                                         
         CH    R1,=H'17'           IF COMMENT IS MORE THAN 17 CHARS             
         BH    CHKD32              THEN IT DOESN'T MATCH                        
         XC    WORK,WORK                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),PCOMELEM+2                                               
         CLC   MTCHSPCE,WORK       COMMENT DOESN'T MATCH                        
         BNE   CHKD32                                                           
         L     R6,AIO                                                           
         B     CHKD37                                                           
*                                                                               
         USING PBUYREC,R6                                                       
CHKD34   CP    PBDUNITS,MTCHUNIT   SAME NUMBER OF UNITS?                        
         BNE   CHKD35                                                           
*                                                                               
         CLC   PBDUIND,MTCHUIND    SAME UNIT INDICATOR?                         
         BE    CHKD37              YES, BOTH THE SAME, CHECK SPACE              
*                                                                               
         CLI   MTCHUIND,C'L'       IF LINES (C'L' OR 0)                         
         BE    *+12                                                             
         CLI   MTCHUIND,0                                                       
         BNE   CHKD60                                                           
*                                                                               
         CLI   PBDUIND,0                                                        
         BE    CHKD37              IF BOTH LINE UNITS, CHECK SPACE              
         CLI   PBDUIND,C'L'                                                     
         BE    CHKD37                                                           
         B     CHKD60                                                           
*                                                                               
CHKD35   CLC   PBDUIND,MTCHUIND    NOT THE SAME UNITS, BUT SAME IND?            
         BE    CHKD60              YES, NO MATCH                                
*                                                                               
         CLI   MTCHUIND,C'L'       IS DETAIL IN LINES (C'L' OR 0)?              
         BE    *+12                                                             
         CLI   MTCHUIND,0                                                       
         BNE   CHKD35A             NO, DETAIL IN INCHES                         
*                                                                               
         CLI   PBDUIND,0           IS BUY IN LINES?                             
         BE    CHKD60                                                           
         CLI   PBDUIND,C'L'                                                     
         BE    CHKD60                                                           
         B     CHKD35B             NO, DETAIL IN LINES, BUY IN INCHES           
*                                                                               
CHKD35A  MVC   HALF(L'PBDUIND),PBDUIND                                          
         OI    HALF,X'40'                                                       
         MVC   HALF+1(L'MTCHUIND),MTCHUIND                                      
         OI    HALF+1,X'40'                                                     
         CLC   HALF(L'PBDUIND),HALF+1   C'I' AND X'89'?                         
         BNE   CHKD35B             NO                                           
*                                                                               
         ZAP   DUB(4),PBDUNITS                                                  
         CLI   PBDUIND,C'I'                                                     
         BNE   *+10                                                             
         SRP   DUB(4),2,0          MULTIPLY BY 100                              
*                                                                               
         ZAP   DUB+4(4),MTCHUNIT                                                
         CLI   MTCHUIND,C'I'                                                    
         BNE   *+10                                                             
         SRP   DUB+4(4),2,0        MULTIPLY BY 100                              
*                                                                               
         CP    DUB(4),DUB+4(4)                                                  
*                                  MATCH BTWN C'I' AND X'89', SKIP SPCE         
         BE    CHKD37                  TEST (IE: 10I AND 2X5)                   
         B     CHKD60                                                           
*                                                                               
CHKD35B  OC    PBDSPACE,PBDSPACE   CHECK  LINES -> INCHES                       
         BNZ   CHKD60                 OR INCHES -> LINES                        
         OC    MTCHSPCE,MTCHSPCE                                                
         BNZ   CHKD60                                                           
*                                                                               
         CP    PBDCLMS,=P'0'                                                    
         BNE   CHKD60                                                           
         CP    MTCHCLMS,=P'0'                                                   
         BNE   CHKD60                                                           
*                                                                               
         CLI   PBDUIND,C'L'        THE BUY IS THE ONE WITH LINES?               
         BNE   CHKD36              NO, THE DETAIL IS THE ONE WITH LINES         
         ZAP   P11,MTCHUNIT        YES, DETAIL HAS INCHES                       
         MP    P11,=P'14'               CONVERT INCHES TO # OF LINES            
*                                                                               
         CLI   MTCHUIND,X'89'      2 DECIMAL PLACES?                            
         BNE   CHKD35C             NO                                           
*                                                                               
         SRP   P11,1,0             MOVE THE HUNDREDS OUT OF LAST TWO            
         CP    P11+9(2),=P'0'                                                   
         BNE   CHKD60                                                           
         SRP   P11,61,0            DIVIDE BY 1000 BECAUSE OF DECIMALS           
*                                                                               
CHKD35C  ZAP   DUB,PBDUNITS        YES, BUY HAS LINES                           
         CP    DUB,P11                                                          
         BNE   CHKD60                                                           
         B     CHKD35                                                           
*                                                                               
CHKD36   ZAP   P11,PBDUNITS        BUY HAS INCHES                               
         MP    P11,=P'14'               CONVERT INCHES TO # OF LINES            
*                                                                               
         CLI   PBDUIND,X'89'       2 DECIMAL PLACES?                            
         BNE   CHKD36A             NO                                           
*                                                                               
         SRP   P11,1,0             MOVE THE HUNDREDS OUT OF LAST TWO            
         CP    P11+9(2),=P'0'                                                   
         BNE   CHKD60                                                           
         SRP   P11,61,0            DIVIDE BY 1000 BECAUSE OF DECIMALS           
*                                                                               
CHKD36A  ZAP   DUB,MTCHUNIT        YES, BUY HAS LINES                           
         CP    DUB,P11                                                          
         BNE   CHKD60                                                           
         B     CHKD35                                                           
*                                                                               
         USING PBUYREC,R6                                                       
CHKD37   CLC   PBDCOSIN,MTCHCIND                                                
         BNE   CHKD60                                                           
*                                                                               
         CLC   PBDCOSTY,MTCHCTYP                                                
         BE    *+16                                                             
         BAS   RE,CKTOTALS                                                      
         BNE   CHKD60                                                           
         B     CHKD38                                                           
*                                                                               
         CP    PBDCOS,MTCHCOST                                                  
         BNE   CHKD60                                                           
*                                                                               
CHKD38   CP    PBDPRCOS,MTCHPREM                                                
         BNE   CHKD60                                                           
         CLC   PBDCL,MTCHCLRS                                                   
         BNE   CHKD60                                                           
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    *+14                                                             
         CLC   PBUYKZON(PBUYKDAT-PBUYKZON),MTCHZONE                             
         BNE   CHKD60                                                           
*                                                                               
         CLI   MTCHCD,C'Y'         ANY CASH DISCOUNT?                           
         BE    CHKD40              YES                                          
         CP    PBDCD,=P'0'         NONE, SEE IF BUY HAS NONE ALSO               
         BNE   CHKD60              IF HAS A CASH DISCOUNT                       
         B     CHKD50                                                           
*                                                                               
CHKD40   CP    PBDCD,=P'0'         SEE IF BUY HAS CASH DISCOUNT ALSO            
         BE    CHKD60              NO, IT DOESN'T                               
*                                                                               
CHKD50   DS    0H                                                               
         CLI   MTCHLINE,0          EVERYTHING MATCHES, ANY BUY LINE?            
         BE    CHKD65                                                           
*                                  YES, THIS BUY'S CORRECTED DETAIL?            
         CLC   PBUYKZON(2),MTCHZONE                                             
         BNE   CHKDNXT                                                          
         CLC   PBUYKEST,MTCHIEST                                                
         BNE   CHKDNXT                                                          
         CLC   PBUYKDAT,MTCHIDAT                                                
         BNE   CHKDNXT                                                          
         CLC   PBUYKLIN,MTCHLINE                                                
         BNE   CHKDNXT             NO, SOME OTHER CORRECTED DETAIL              
         B     CHKD80              YES, CORRECTED INSERTION MATCHES NOW         
*                                                                               
CHKD60   CLI   MTCHLINE,0          CORRECTED DETAIL?                            
         BE    CHKDNXT             NO, CHECK NEXT DETAIL                        
*                                                                               
*                                  YES, THIS BUY'S CORRECTED DETAIL?            
         CLC   PBUYKZON(2),MTCHZONE                                             
         BNE   CHKDNXT                                                          
*                                                                               
         OC    BEST,BEST           IF MATCH ACROSS EST                          
         BNZ   *+14                                                             
         OC    MTCHIEST,MTCHIEST                                                
         BZ    *+14                                                             
         CLC   PBUYKEST,MTCHIEST   SKIP THIS TEST                               
         BNE   CHKDNXT                                                          
*                                                                               
         CLC   PBUYKDAT,MTCHIDAT                                                
         BNE   CHKDNXT                                                          
         CLC   PBUYKLIN,MTCHLINE                                                
         BNE   CHKDNXT             NO, SOME OTHER CORRECTED DETAIL              
         OI    MNIOFLAG,X'20'                                                   
         B     CHKDX               YES, CORRECTED INSERTION, NEXT BUY           
*                                                                               
CHKD65   L     R1,ASYSD            CHECK IF BUY HAS BEEN CORRECTED              
         AH    R1,=Y(MATCHTBL-SYSD)                                             
CHKD70   DS    0H                                                               
         OC    0(L'MTCHSEQ,R1),0(R1)                                            
         BZ    CHKDX                                                            
         CLC   PBUYKZON(2),MTCHZONE-MTCHLIND(R1)                                
         BNE   CHKD70NX                                                         
*                                                                               
         OC    BEST,BEST           IF MATCH ACROSS EST                          
         BNZ   *+14                                                             
         OC    MTCHIEST,MTCHIEST                                                
         BZ    *+14                                                             
         CLC   PBUYKEST,MTCHIEST-MTCHLIND(R1)                                   
         BNE   CHKD70NX                                                         
*                                                                               
         CLC   PBUYKDAT,MTCHIDAT-MTCHLIND(R1)                                   
         BNE   CHKD70NX                                                         
         CLC   PBUYKLIN,MTCHLINE-MTCHLIND(R1)                                   
         BE    CHKDNXT             CORRECTED, GET NEXT DETAIL                   
         B     CHKD80              NOT CORRECTED SO WE HAVE A MATCH             
CHKD70NX LA    R1,MTCHNXTL-MTCHLIND(R1)                                         
         B     CHKD70                                                           
*                                                                               
CHKD80   DS    0H                                                               
         CLC   QPRD,=C'***'                                                     
         BNE   *+14                                                             
         CLC   PBUYKPRD,MTCHSPRD                                                
         BNE   CHKDNXT                                                          
         CLC   PBUYKZON(2),MTCHZONE                                             
         BNE   CHKDNXT                                                          
*                                                                               
         OC    BEST,BEST           IF MATCH ACROSS EST                          
         BNZ   *+14                                                             
         OC    MTCHIEST,MTCHIEST                                                
         BZ    *+14                                                             
         CLC   PBUYKEST,MTCHIEST                                                
         BNE   CHKDNXT                                                          
*                                                                               
         CLC   PBUYKDAT,MTCHIDAT                                                
         BNE   CHKDNXT                                                          
*                                                                               
         BRAS  RE,TSTLOCK          LOCKED ?                                     
         BE    CHKD82              NO                                           
         MVI   ERROR,DATALOK       RECORDS LOCKED FOR OFFLINE PROCESS           
         GOTO1 ERREX                                                            
*                                                                               
CHKD82   BAS   RE,CHKDTLS                                                       
         BNE   CHKD85                                                           
*                                                                               
         XC    MINEKEY,MINEKEY     READ THE  INVOICE HEADER DETAIL              
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,MINELEM                                                       
         USING PIMHDREL,R1                                                      
*                                                                               
         OI    PIMSTAT,X'04'       NEW VERSION MATCH                            
*                                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKD85   XC    MINEKEY,MINEKEY     MATCH THE INVOICE DETAIL                     
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         MVC   MINEKEY+2(L'MTCHSEQ),MTCHSEQ                                     
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,MINELEM                                                       
         USING PIMDTLEL,R1                                                      
         OI    PIMDSTAT,X'10'                                                   
         MVC   PIMBZONE(PIMBLINE-PIMBZONE),PBUYKZON                             
         MVC   PIMBLINE,PBUYKLIN                                                
         MVC   PIMIEST,PBUYKEST                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'95'        IF BUY HAS A TEARSHEET ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    PIMDSTAT,X'08'      THEN TURN ON TEARSHEET BIT IN DETAIL         
         L     R6,AIO                                                           
*                                                                               
         DROP  R1                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    MNIOFLAG,X'80'                                                   
*                                                                               
         MVI   RDUPDATE,C'Y'       ALWAYS READ FOR UPDATE HERE                  
         GOTO1 GETREC              GET AGAIN IN CASE OF MINIO SPLIT             
         OI    PBDSTAT,X'40'       MATCH THE BUY RECORD                         
         L     R1,MINELEM                                                       
         USING PIMDTLEL,R1                                                      
         TM    PIMDSTAT,X'08'      IF DETAIL HAS TEARSHEET                      
         BZ    *+8                                                              
         OI    PBDSTAT,X'10'       THEN SO DOES THE INSERTION                   
         DROP  R1                                                               
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         XC    ELEM,ELEM           PUT THE INV MATCH ELEMENT ON BUYREC          
         LA    R2,ELEM                                                          
         USING PBINVELM,R2                                                      
*                                                                               
         MVI   PBINVELM,PBINVELQ                                                
         MVI   PBINVLEN,PBINVELL                                                
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   CHKD90                                                           
         MVC   PBINVPRD,LSTPRD                                                  
         OC    PBINVPRD,SPACES     MAKE SURE NO NULLS                           
         MVC   PBINVYR,LSTYEAR                                                  
         GOTO1 HEXIN,DMCB,LSTPUB,PBINVPUB,9                                     
*                                                                               
         LR    R1,RA                                                            
         AH    R1,CURDISP                                                       
         USING LINSELH,R1                                                       
         MVC   PBINVNUM,LININV                                                  
         MVC   PBINVEST,LINEST                                                  
         DROP  R1                                                               
         B     CHKD95                                                           
CHKD90   MVC   PBINVPRD,CHKPRD                                                  
         OC    PBINVPRD,SPACES     MAKE SURE NO NULLS                           
         GOTO1 HEXIN,DMCB,CHKPUB,PBINVPUB,9                                     
         MVC   PBINVYR,CHKYEAR                                                  
*                                                                               
         MVC   PBINVNUM,CHKINVN                                                 
         MVC   PBINVEST,CHKESTM                                                 
CHKD95   GOTO1 DATCON,DMCB,(5,0),(3,PBINVMDT)                                   
         MVC   PBINVPID,SVMATPID                                                
         DROP  R2                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   MTCHZONE(2),PBUYKZON                                             
         MVC   MTCHDATE(MTCHLINE-MTCHDATE),PBUYKDAT                             
         MVC   MTCHLINE,PBUYKLIN                                                
*                                                                               
         B     CHKDX               WE MATCHED THIS BUY                          
*                                                                               
CHKDNXT  LA    R3,MTCHNXTL                                                      
         OC    MTCHSEQ,MTCHSEQ     IF NO MORE DETAILS                           
         BNZ   *+12                                                             
CHKDNOT  OI    MNIOFLAG,X'20'      THIS BUY ISN'T MATCHED                       
         B     CHKDX               THEN WE'RE DONE                              
*                                                                               
         CLC   PBUYKDAT,MTCHIDAT   DATE SAME AS DETAIL'S?                       
         BE    CHKD30              YES, CHECK THE DATA                          
         BH    CHKDNXT             EXIT, THIS CASE SHOULDN'T HAPPEN             
         CLI   PBDBFD,C'W'         WEEK OF THAT BUY DATE                        
         BE    *+12                                                             
         CLI   PBDBFD,C'B'             OR BEST FOOD DATE?                       
         BNE   CHKDNXT             NO, CHECK NEXT BUY                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,DUB)   ADD 7 TO BUY DATE             
         GOTO1 ADDAY,DMCB,DUB,BLOCK,7                                           
         GOTO1 DATCON,DMCB,(0,BLOCK),(3,DUB)                                    
*                                                                               
         CLC   MTCHIDAT,DUB        INVOICE DATE IN THAT 7 DAY WEEK?             
         BL    CHKD30              YES, CHECK THE DATA                          
*                                                                               
CHKDX    DS    0H                                                               
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS THE DETAILS - WERE ANY MATCHED THE 'OLD' WAY?             
***********************************************************************         
CHKDTLS  NTR1                                                                   
*                                                                               
         XC    MINEKEY,MINEKEY     MATCH THE INVOICE DETAIL                     
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,MINELEM                                                       
         USING PIMDTLEL,R1                                                      
         B     CKDTLCK                                                          
*                                                                               
CKDTLLP  GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    CKDTLCK                                                          
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    CKDTLX                                                           
*                                                                               
CKDTLCK  TM    PIMDSTAT,X'10'      MATCHED                                      
         BO    NO                                                               
         TM    PIMDSTAT,X'02'      MATCHED AND PAID                             
         BO    NO                                                               
*                                                                               
         B     CKDTLLP                                                          
*                                                                               
CKDTLX   B     YES                                                              
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE IF TOTAL RATE IS EQUAL TO GROSS TOTAL                            
***********************************************************************         
CKTOTALS NTR1                                                                   
         USING MTCHLIND,R3                                                      
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDCOSTY,C'U'       DOES THE BUY HAVE UNIT RATE?                 
         BNE   CTOT10                                                           
         CLI   MTCHCTYP,C'T'                                                    
         BNE   CTOTX                                                            
*                                                                               
         LA    R2,GETINSWK                                                      
         USING PVALUES,R2                                                       
CTOT5    GOTO1 GETINS,DMCB,AIO,(R2),PBUYKPRD,INVSTDT,=C'GST'                    
         L     R1,GROSS                                                         
         L     R0,PREMIUM                                                       
         SR    R1,R0               DON'T INCLUDE THE PREMIUM                    
         CVD   R1,DUB                                                           
         CP    MTCHCOST,DUB        SEE IF AMOUNTS MATCH                         
         B     CTOTX               AND REPORT BACK                              
*                                                                               
CTOT10   CLI   MTCHCTYP,C'U'       DOES THE DETAIL HAVE UNIT RATE?              
         BNE   CTOTX                                                            
         CLI   PBDCOSTY,C'T'                                                    
         BNE   CTOTX                                                            
*                                                                               
         LA    R6,BLOCK                                                         
         ST    R6,AIO                                                           
*                                                                               
         LR    R0,R6                                                            
         LA    R1,480                                                           
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,MINBLOCK                                                      
         USING MINBLKD,R1                                                       
         L     RE,MINELEM                                                       
         USING PIMDTLEL,RE                                                      
*                                                                               
         USING PBUYREC,R6                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'      BUY RECORD CODE                              
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,QPRD                                                    
*                                                                               
         CLC   =C'***',CHKPRD       PRODUCT VARIOUS                             
         BE    *+14                                                             
         CLC   =C'***',LSTPRD       PRODUCT VARIOUS                             
         BNE   *+26                                                             
         MVC   PBUYKPRD,PIMSPRD                                                 
         CLC   PIMCPRD,=CL3' '     IF CORRECTED PRODUCT PRESENT                 
         BNH   *+10                                                             
         MVC   PBUYKPRD,PIMCPRD       USE IT                                    
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         MVC   PBUYKDAT,MTCHIDAT                                                
         MVC   PBUYKEST,MTCHIEST                                                
         MVC   PBUYLEN,=X'0021'    LENGTH OF NO ELEMENT RECORD                  
*                                                                               
         DROP  RE,R1                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING PBDELEM,R5                                                       
         MVI   PBDELEM,X'20'                                                    
         MVI   PBDELEM+1,X'74'                                                  
         MVC   PBDACP,PUBAGYCM                                                  
         MVC   PBDBDATE,MTCHIDAT                                                
         MVC   PBDCD,PUBCASHD                                                   
         OI    PBDCNDA,X'80'                                                    
         MVC   PBDCOS,MTCHCOST                                                  
         MVC   PBDCOSIN,MTCHCIND                                                
         MVC   PBDCOSTY,MTCHCTYP                                                
         MVC   PBDGST,PUBGSTAX                                                  
         MVC   PBDPDATE,MTCHIDAT                                                
         MVC   PBDPRCOS,MTCHPREM                                                
*        MVC   PBDSTAT                                                          
         CLC   MTCHIDAT,PUBTAXD1                                                
         BL    CTOT20                                                           
         MVC   PBDTAX,PUBTAXR1                                                  
         B     CTOT20                                                           
         CLC   MTCHIDAT,PUBTAXD2                                                
         BL    CTOT20                                                           
         MVC   PBDTAX,PUBTAXR2                                                  
         B     CTOT20                                                           
         CLC   MTCHIDAT,PUBTAXD3                                                
         BL    CTOT20                                                           
         MVC   PBDTAX,PUBTAXR3                                                  
CTOT20   MVC   PBDUIND,MTCHUIND                                                 
         MVC   PBDUNITS,MTCHUNIT                                                
*        MVC   PBDWTSUM                                                         
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GETINSWK                                                      
         USING PVALUES,R2                                                       
CTOT25   GOTO1 GETINS,DMCB,BLOCK,(R2),PBUYKPRD,INVSTDT,=C'GST'                  
         DROP  R6                                                               
*                                                                               
         L     R1,GROSS                                                         
         L     R0,PREMIUM                                                       
         SR    R1,R0               DON'T INCLUDE THE PREMIUM                    
         CVD   R1,DUB                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CP    PBDCOS,DUB          SEE IF AMOUNTS MATCH                         
*                                                                               
CTOTX    B     XIT                                                              
         DROP  R2,R3,R6                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE DETAIL ELEMENT                                      
*                                                                               
* ON ENTRY:    MINELEM             A(MINIO ELEMENT)                             
*              (R2)                LINE ON WHICH ELEMENT WILL BE SHOWN          
*              FULL                A(BUY KEY)                                   
***********************************************************************         
SHOWDTL  DS    0H                                                               
         NMOD1 0,**DSPD**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         CLI   PIMCSIND,0                                                       
         BNE   *+8                                                              
         MVI   PIMCSIND,C' '                                                    
*                                                                               
         LA    R0,CK1ITEMH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R0,CK2ITEMH                                                      
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BE    DD00                                                             
*                                                                               
         LA    R0,UP1ITEMH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R0,UP2ITEMH                                                      
*                                                                               
DD00     OC    FULL,FULL                                                        
         BZ    DD01                                                             
         L     R1,FULL                                                          
         USING PBUYKEY,R1                                                       
         CLC   PBUYKDAT,PIMIDATE                                                
         BE    DD06                                                             
         DROP  R1                                                               
*                                                                               
         USING SCRLIN1D,R2                                                      
DD01     LA    R3,SLN1IDT                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2IDT                                                       
         DROP  R2                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,PIMIDATE),(12,0(R3))                              
*                                                                               
         CLI   PIMIDATE+2,1                                                     
         BH    DD06                                                             
         CLI   MAGFREQ,C'M'                                                     
         BNE   *+10                                                             
         XC    3(2,R3),3(R3)                                                    
*                                                                               
         USING SCRLIN1D,R2                                                      
DD06     LA    R3,SLN1ESTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2ESTH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   PIMBLINE,0          CORRECTED OR MATCHED?                        
         BE    DD08                                                             
*                                                                               
         CLI   ACTNUM,ACTUPDTE     YES, ARE WE IN UPDATE?                       
         BNE   DD08                                                             
*                                                                               
         OI    6(R3),X'20'         YES, PROTECT ESTIMATE COLUMN AND             
         EDIT  (B2,PIMBEST),(3,8(R3)),FILL=0   SHOW ESTIMATE                    
*****    B     DD10                                                             
*                                                                               
DD08     CLI   SVPROF+3,C'N'       MULTI-ESTIMATE?                              
         BNE   DD08A                                                            
*                                                                               
         OC    MYBEST,MYBEST                                                    
         BZ    DD08A                                                            
*                                                                               
         OI    6(R3),X'20'         PROTECT ESTIMATE NUMBER                      
*                                                                               
         B     DD10                                                             
*                                                                               
DD08A    OC    PIMIEST,PIMIEST     YES, ANY ESTIMATE?                           
         BZ    DD10                                                             
*                                                                               
         EDIT  (B2,PIMIEST),(3,8(R3)),FILL=0   YES, SHOW ESTIMATE               
*                                                                               
DD10     DS    0H                                                               
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1COMH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2COMH                                                      
         DROP  R2                                                               
*                                                                               
         TM    PIMDSTAT,X'04'      COMMENT?                                     
         BZ    *+12                                                             
         OI    6(R3),X'08'         YES, HIGHLIGHT A 'C'                         
         MVI   8(R3),C'C'                                                       
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   DD90                                                             
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1SIZ                                                       
*                                                                               
         CP    PIMUNITS,VALUNITS   IF SAME SIZE                                 
         BNE   DD10A                                                            
*                                                                               
         OC    PIMCLMS,PIMCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PIMCLMS,=P'0'                                                    
*                                                                               
         CP    PIMCLMS,VALCLMS         AND SAME NUMBER OF COLUMNS               
         BNE   DD10A                                                            
         CLC   PIMSPACE,VALSPACE                                                
         BE    DD50                THEN NO NEED TO DISPLAY IT AGAIN             
         B     DD10B                                                            
*                                                                               
DD10A    CLI   PIMSPACE,C' '       IF 1ST BYTE > SPACE?                         
         BNH   DD10C                                                            
         CLI   PIMSPACE,X'FF'      3 BYTES PACKED FOLLOWS?                      
         BNE   DD10B                                                            
         EDIT  (P3,12(R6)),(8,0(R3)),ALIGN=LEFT   YES, IT'S 12(ELEM)            
         B     DD50                                                             
*                                                                               
DD10B    MVC   0(8,R3),PIMSPACE                                                 
         B     DD50                                                             
*                                                                               
DD10C    CLI   PIMUIND,0           PRINT SIZE WITH NO DECIMALS?                 
         BE    DD20                                                             
         CLI   PIMUIND,C'L'                                                     
         BE    DD20                                                             
         CLI   PIMUIND,C'I'                                                     
         BNE   DD40                                                             
*                                                                               
DD20     EDIT  (P3,PIMUNITS),(8,0(R3)),ALIGN=LEFT     YES                       
*                                                                               
         SR    R1,R1               THEN PUT AN 'I' AFTER THE NUMBER             
DD20LP   LA    R3,SLN1SIZ                                                       
         AR    R3,R1                                                            
         CLI   0(R3),C' '                                                       
         BE    DD30                                                             
         LA    R1,1(R1)                                                         
         CH    R1,=Y(L'SLN1SIZ)                                                 
         BL    DD20LP                                                           
         B     DD45                                                             
*                                                                               
DD30     CLI   PIMUIND,C'I'        IF INCHES                                    
         BE    *+12                                                             
         CLI   PIMUIND,X'89'                                                    
         BNE   DD45                                                             
*                                                                               
         MVI   0(R3),C'I'                                                       
         LA    R3,1(R3)                                                         
         B     DD45                                                             
*                                                                               
DD40     CLI   PIMUIND,X'89'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P3,PIMUNITS),(8,SLN1SIZ),2,ALIGN=LEFT                           
         SR    R1,R1                                                            
         B     DD20LP                                                           
*                                                                               
DD45     OC    PIMCLMS,PIMCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PIMCLMS,=P'0'                                                    
         ZAP   DUB,PIMCLMS                                                      
         BZ    DD50                                                             
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         MVC   WORK+32(L'SLN1SIZ),SLN1SIZ                                       
         LA    R0,SLN1SIZ                                                       
         LR    R1,R3                                                            
         SR    R1,R0                                                            
         LA    R3,WORK+32(R1)                                                   
         EDIT  (P8,DUB),(3,0(R3)),ALIGN=LEFT                                    
         MVC   SLN1SIZ,WORK+32                                                  
*                                                                               
DD50     CP    PIMCOST,VALCOST     IF SAME RATE                                 
         BNE   DD52                                                             
         CLC   PIMCSIND,VALCOSIN      SAME COST INDICATOR                       
         BNE   DD52                                                             
         CP    VALCOST,=P'0'          AND NOT OFF A CORRECTION                  
         BNE   DD70                                                             
         MVC   SLN1RTE(4),=C'FREE'  THEN IT'S FREE                              
         B     DD70                                                             
*                                                                               
DD52     ZAP   P11,PIMCOST                                                      
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BE    DD52A                                                            
*                                                                               
         CLI   MYGRSNET,C'N'       SHOW NET AMOUNTS?                            
         BNE   DD52A                                                            
         ZAP   GROSSAMT,P11        YES                                          
         ZAP   PERCENTG,PUBAGYCM                                                
         BAS   RE,GRSTONET                                                      
         ZAP   P11,NETAMNT                                                      
*                                                                               
DD52A    TM    PIMDSTAT,X'80'      UNIT COST?                                   
         BZ    DD60                NO                                           
*                                                                               
         LA    R3,SLN1RTE                                                       
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BNE   *+12                                                             
         MVI   SLN1RTE,C'S'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
         CP    P11,=P'-99999999'    GREATER THAN 999.99999?                     
         BL    *+14                                                             
         CP    P11,=P'99999999'                                                 
         BNH   DD55                  NO, 5 DECIMALS                             
         ZAP   GROSSAMT,P11                                                     
         SRP   GROSSAMT,64-3,0     DIVIDE BY 1000 BUT DON'T ROUND               
         ZAP   P11,GROSSAMT                                                     
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BE    DD52B                                                            
*                                                                               
         EDIT  (P11,P11),(10,(R3)),2,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     DD70                                           2 DEC             
*                                                                               
DD52B    EDIT  (P11,P11),(9,(R3)),2,ALIGN=LEFT,MINUS=YES                        
         B     DD70                                                             
*                                                                               
DD55     CLI   PIMCSIND,C'S'                                                    
         BE    DD55A                                                            
*                                                                               
         EDIT  (P11,P11),(10,(R3)),5,ALIGN=LEFT,MINUS=YES  UNIT RATE            
         B     DD70                                             5 DEC           
*                                                                               
DD55A    SRP   P11,64-1,5          DIVIDE BY 10 AND ROUND                       
         EDIT  (P11,P11),(9,(R3)),4,ALIGN=LEFT,MINUS=YES                        
         B     DD70                                                             
*                                                                               
DD60     LA    R3,SLN1RTE                                                       
         CLI   PIMCSIND,C'S'                                                    
         BNE   DD63                                                             
         MVI   0(R3),C'S'                                                       
         LA    R3,1(R3)                                                         
         B     DD66                                                             
*                                                                               
DD63     MVI   SLN1RTE,C'T'                                                     
         LA    R3,1(R3)                                                         
*                                                                               
DD66     EDIT  (P11,P11),(9,(R3)),2,ALIGN=LEFT,MINUS=YES                        
*                                                                               
DD70     DS    0H                                                               
*                                                                               
         CLC   PIMCLRS,VALCL       SKIP IF NO CHANGE IN COLORS?                 
         BNE   *+10                                                             
         CP    PIMPREM,VALPRCOS    AND     NO CHANGE IN PREM CHARGE             
         BE    DD100                                                            
*                                                                               
         CLI   PIMCLRS,0                                                        
         BE    DD85                                                             
         EDIT  (B1,PIMCLRS),(1,SLN1PRM)                                         
         MVI   SLN1PRM+1,C'C'                                                   
         MVI   SLN1PRM+2,C'/'                                                   
         LA    R1,SLN1PRM+3                                                     
         EDIT  (P5,PIMPREM),(8,(R1)),2,ALIGN=LEFT,MINUS=YES                     
         B     DD100                                                            
*                                                                               
DD85     DS    0H                                                               
         CP    PIMPREM,VALPRCOS    NO CHANGE IN PREM CHARGE                     
         BE    DD100                                                            
         EDIT  (P5,PIMPREM),(11,SLN1PRM),2,ALIGN=LEFT,MINUS=YES                 
         B     DD100                                                            
         DROP  R2                                                               
*                                                                               
         USING SCRLIN2D,R2                                                      
DD90     CLC   PIMSPACE,VALSPACE   MAGAZINE USES SPACE                          
         BE    DD100                                                            
         CLI   PIMSPACE,X'FF'                                                   
         BE    DD95                                                             
         MVC   SLN2SPC,PIMSPACE                                                 
         B     DD100                                                            
*                                                                               
DD95     LA    R3,SLN2SPC                                                       
         MVC   0(4,R3),=C'SRI='    DISPLAY  SHOW/REG/ILLUM                      
         LA    R3,4(R3)                                                         
*                                                                               
         CP    PIMSHOW,=P'99999'                                                
         BNE   DD95A                                                            
         MVC   0(3,R3),=C'SPC'                                                  
         LA    R3,3(R3)                                                         
         B     DD95B                                                            
*                                                                               
DD95A    EDIT  (P3,PIMSHOW),(3,0(R3)),ALIGN=LEFT                                
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
DD95B    MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (P3,PIMREG),(4,0(R3)),ALIGN=LEFT                                 
         LA    R3,3(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (P3,PIMILLUM),(4,0(R3)),ALIGN=LEFT                               
         DROP  R2                                                               
*                                                                               
         USING SCRLIN1D,R2                                                      
DD100    LA    R3,SLN1CTPH         CTPBI COLUMN                                 
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2CTPH                                                      
         DROP  R2                                                               
*                                                                               
         CLC   =C'***',CHKPRD      BECOMES PRODUCT COLUMN IF VARIOUS            
         BNE   DD110                                                            
*                                                                               
         MVC   8(3,R3),PIMSPRD                                                  
         CLC   PIMCPRD,=C'   '     IF CORRECTED PRODUCT EXISTS                  
         BNH   *+10                                                             
         MVC   8(3,R3),PIMCPRD        DISPLAY IT                                
*                                  IF WE DON'T HAVE CTPBI HERE                  
         CLC   =C'CT',CHKOPTN        DISPLAY IT IN NET FIELD?                   
         BNE   DD113                 NO                                         
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1NETH           YES                                        
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2NETH                                                      
         DROP  R2                                                               
         NI    1(R3),X'FF'-X'20'   ENABLE EDITING OF THIS FIELD                 
*                                                                               
DD110    MVI   8(R3),C'N'                                                       
         TM    PIMDSTAT,X'40'      NO CASH DISCOUNT?                            
         BNZ   *+8                 NONE                                         
         MVI   8(R3),C'Y'          YES, THERE IS A CASH DISCOUNT                
*                                                                               
         TM    PIMDSTAT,X'08'      TEAR SHEET PROOF?                            
         BZ    *+8                 NO                                           
         MVI   9(R3),C'T'          YES                                          
*                                                                               
         TM    PIMDSTAT,X'02'      MATCHED AND PAID?                            
         BZ    *+8                 NO                                           
         MVI   10(R3),C'P'         YES                                          
*                                                                               
DD113    GOTO1 CALCDTLG,DMCB,GETINSA                                            
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
*                                                                               
DD115    CLI   SCRTYPE,C'M'                                                     
         BNE   DD120                                                            
         CP    PIMCOST,=P'0'                                                    
         BNE   DD120                                                            
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2GRS                                                       
         DROP  R2                                                               
         MVC   0(4,R1),=C'FREE'                                                 
         B     DD130                                                            
*                                                                               
DD120    CLI   PIMCSIND,C'S'       NET IS SAME AS GROSS?                        
         BNE   *+10                                                             
         XC    AGYCOM,AGYCOM       THEN NO AGENCY COMMISSION                    
*                                                                               
         L     R1,GROSS                                                         
         SR    R0,R0                                                            
         CLI   MYGRSNET,C'G'                                                    
         BE    *+8                                                              
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R1,SLN1GRSH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2GRSH                                                      
         DROP  R2                                                               
*                                                                               
         EDIT  (B4,FULL),(11,8(R1)),2,ALIGN=LEFT,MINUS=YES                      
*                                                                               
DD130    CLI   CHKOPTN+1,C'.'      ONE BYTE OPTION?                             
         BE    *+12                                                             
         CLI   CHKOPTN+1,0                                                      
         BNE   DD132                                                            
*                                                                               
DD130C   CLI   CHKOPTN,C'C'        CASH DISCOUNT?                               
         BNE   DD130G                                                           
*                                                                               
         TM    PIMDSTAT,X'40'      ANY CASH DISCOUNT?                           
         BNZ   *+14                                                             
         OC    CSHDSC,CSHDSC                                                    
         BNZ   DD130C1                                                          
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R1,SLN1NET                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2NET                                                       
         DROP  R2                                                               
         MVC   0(4,R1),=C'NONE'    NO                                           
         B     DDX                                                              
*                                                                               
DD130C1  MVC   FULL,CSHDSC                                                      
         B     DD130SHW                                                         
*                                                                               
DD130G   CLI   CHKOPTN,C'G'        GROSS?                                       
         BNE   DD130N                                                           
*                                                                               
         MVC   FULL,GROSS                                                       
         B     DD130SHW                                                         
*                                                                               
DD130N   CLI   CHKOPTN,C'N'        NET?                                         
         BNE   DD130T                                                           
*                                                                               
         L     R1,GROSS            YES                                          
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         B     DD130SHW                                                         
*                                                                               
DD130T   CLI   CHKOPTN,C'T'        TAX?                                         
         BNE   DD130Z                                                           
*                                                                               
         MVC   FULL,TAX            YES                                          
         B     DD130SHW                                                         
*                                                                               
DD130Z   CLI   CHKOPTN,C'Z'        ZONE/ED?                                     
         BNE   DDX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),BPUB                                                      
         MVC   KEY+4(2),PIMBZONE                                                
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 VPUBEDIT,DMCB,(8,KEY),(C'S',WORK)                                
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R1,SLN1NETH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2NETH                                                      
         DROP  R2                                                               
*                                                                               
         MVC   8(6,R1),WORK+9                                                   
         OI    6(R1),X'80'                                                      
         B     DDX                                                              
*                                                                               
         USING SCRLIN1D,R2                                                      
DD130SHW LA    R1,SLN1NET                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2NET                                                       
         DROP  R2                                                               
*                                                                               
         EDIT  (B4,FULL),(11,0(R1)),2,ALIGN=LEFT,MINUS=YES                      
         B     DDX                                                              
*                                                                               
DD132    DS    0H                                                               
*                                                                               
DD132GL  CLC   =C'GL',CHKOPTN      GROSS LESS CASH DISCOUNT?                    
         BNE   DD132GS                                                          
         MVC   FULL,BLABLE                                                      
         B     DD130SHW                                                         
*                                                                               
DD132GS  CLC   =C'GS',CHKOPTN      GST TAX?                                     
         BNE   DD132NL                                                          
         MVC   FULL,GSTTAX                                                      
         B     DD130SHW                                                         
*                                                                               
DD132NL  CLC   =C'NL',CHKOPTN      NET LESS CASH DISCOUNT?                      
         BNE   DDX                                                              
         MVC   FULL,PYABLE                                                      
         B     DD130SHW                                                         
*                                                                               
DDX      B     XIT                                                              
         DROP  R3,R6               DON'T NEED GETINS VALUES ANYMORE             
***********************************************************************         
* THIS ROUTINE CALCULATES GROSS AMOUNTS TO NET AMOUNTS BASED ON A               
* PERCENTAGE.                                                                   
*                                                                               
* ON ENTRY:    GROSSAMT    P       GROSS AMOUNT                                 
*              PERCENTG    P       PERCENTAGE OF GROSS THAT IS NET              
*                                                                               
* ON EXIT:     NETAMNT     P       NET AMOUNT                                   
*                                                                               
* WARNING:     ALL 3 SPECIFIED VARIABLES WILL GET CLOBBERED                     
***********************************************************************         
GRSTONET NTR1                                                                   
         CP    PERCENTG,=P'0'      IF NO PERCENTAGE                             
         BE    GTON10              THEN NET = GROSS                             
*                                                                               
         ZAP   NETAMNT,=P'100000'  NET = GROSS * (1-%AGE)                       
         SP    NETAMNT,PERCENTG                                                 
         ZAP   PERCENTG,NETAMNT                                                 
         MP    GROSSAMT,PERCENTG                                                
         SRP   GROSSAMT,64-5,5     DIVIDE BY 100000 AND ROUND                   
GTON10   ZAP   NETAMNT,GROSSAMT                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE PUB PAYING ADDRESS, WHAT A BIG PAIN IN THE BUTT         
***********************************************************************         
GTPUBADR DS    0H                                                               
         NMOD1 0,**GPBA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
*                                                                               
         XC    REPNAME,REPNAME                                                  
         XC    REPADR1,REPADR1                                                  
         XC    REPADR2,REPADR2                                                  
*                                                                               
         XC    KEY,KEY             READ PUB REC                                 
         LA    R3,KEY                                                           
         USING PUBRECD,R3                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),BPUB                                                  
         LA    R5,6                                                             
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES/EDTS?                              
         BZ    GPA20                                                            
GPA10    LA    R5,4                                                             
         XC    PUBKZON(2),PUBKZON  YES, GET BASE PUB INSTEAD                    
*                                                                               
GPA20    MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR  '                                          
         GOTO1 HIGH                                                             
*                                                                               
         EX    R5,*+12             MAKE SURE MEDIA AND PUB SAME                 
         BNE   GPAX                                                             
         BE    *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
*                                                                               
         CLC   KEY+7(3),KEYSAVE+7  MAKE SURE AGENCY AND STILL PUB REC           
         BNE   GPAX                                                             
*                                                                               
         MVC   FILENAME,=CL8'PUBFILE '                                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING PUBNAMEL,R6                                                      
         MVC   REPNAME,=CL30'PAY PUB DIRECT'                                    
         MVC   REPADR1,PUBLINE1                                                 
         MVC   REPADR2,PUBLINE2                                                 
*                                                                               
         CH    R5,=H'4'                                                         
         BE    *+12                                                             
         CLI   SVAGPROF+13,C'0'    IF COMBINED PAYMENTS                         
         BNE   GPA10               GO BACK AND GET BASE PUB FOR ADDR            
*                                                                               
         MVC   FULL(3),=3X'FF'                                                  
*                                                                               
*****         L     R6,AIO              PAY ADDRESS OVERRIDE?                   
*****         MVI   ELCODE,X'08'                                                
*****         BAS   RE,GETEL                                                    
*****         CLI   0(R6),X'08'                                                 
*****         BE    GPA40               YES                                     
*                                                                               
*****GPA30    BAS   RE,NEXTEL                                                   
*****         BNE   GPAX                                                        
*****         USING PUBAOVEL,R6                                                 
*****         CLC   PUBAOFF,QCLT                                                
*****         BE    GPA40                                                       
*****         CLI   PUBAOFF,X'FF'                                               
*****         BNE   GPA30                                                       
*****         CLC   PUBAOFF+1(1),CLTOFICE                                       
*****         BE    GPA40                                                       
*****         CLC   PUBAOFF,=3X'FF'                                             
*****         BNE   GPA30                                                       
*                                                                               
*****GPA40    MVC   REPNAME,PUBAONAM                                            
*****         MVC   REPADR1,PUBAOLN1                                            
*****         MVC   REPADR2,PUBAOLN2                                            
*****         MVC   FULL,PUBAOFF                                                
*                                                                               
*                                       PAY ADDRESS OVERRIDE?                   
         MVC   CLTAGY,PUBKAGY                                                   
         MVC   CLTMED,PUBKMED                                                   
         MVC   CLTCODE,QCLT                                                     
         MVC   CLTOFF,CLTOFICE                                                  
*                                                                               
         GOTO1 VPGETADR,DMCB,(=C'P',CLTDATA),PUBREC,DATAMGR,0                   
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),0             ADDRESS RECORD FOUND ?                       
         BE    GPAX                NO - DONE                                    
*                                  PAY ADDRESS REC FOUND                        
         L     R6,4(R1)            A(ADDRESS INFO FROM CALL)                    
         USING PGETADRD,R6                                                      
         MVC   REPNAME,PGADNAME                                                 
         MVC   REPADR1,PGADLIN1                                                 
         MVC   REPADR2,PGADLIN2                                                 
         MVC   FULL(3),1(R1)       ADDRESS 'LEVEL'                              
*                                                                               
GPAX     L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
*                                                                               
         DROP  R6,R3                                                            
*                                                                               
CLTDATA  DS    0CL7         USED TO PASS KEY INFO TO PPGETADR MODULE            
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
         DS    0D                                                               
******************************************************************              
**  THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK                               
**  WHICH ARE "PERSONAL ID"                                                     
**                                                                              
******************************************************************              
PID      NTR1  BASE=*,LABEL=*                                                   
         XC    SVMATPID,SVMATPID         PASSWORD ID NUMBER CLEARED             
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
         DROP  RF                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'             CHECK IF SECET CODE IS THERE           
         BZ    *+10                                                             
         MVC   SVMATPID,FAPASSWD         SAVE PASSWORD ID NUMBER                
         DROP  R1                                                               
         XIT1                                                                   
******************************************************************              
*                                                                               
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         DROP  RE                                                               
*                                                                               
TSTLOK20 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK20                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         BNE   TSTLOK28            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK24 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK24                                                         
*                                  CHECK CLIENT/PUB LOCK                        
TSTLOK28 LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT/PUB LOCK                              
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         MVC   L.LOCKPUB,PBUYKPUB                                               
         XC    L.LOCKPUB,=4X'FF'   TO ELIMINATE X'00' FIELDS                    
         DROP  RE                                                               
*                                                                               
TSTLOK30 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK30                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         BNE   TSTLOK40            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK34 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK34                                                         
*                                                                               
TSTLOK40 DS    0H                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB               REC LOCKED - SEND MESSAGE                    
         XIT1                                                                   
*                                                                               
LKUPKEY  DS    XL16                DATA LOCKING KEY                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
CKTRAFID NTR1  BASE=*,LABEL=*,WORK=(R4,200)                                     
*                                   CHECKING FOR TRAFFIC ID SIGN-ON             
         LR    R0,R4                                                            
         LHI   R1,1600                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CTIREC,R4           SEE WORK= IN NTR1 ABOVE                      
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         L     RF,ATWA                                                          
         MVC   CTIKNUM,10(RF)      ID NUMBER                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
LIMACC   NTR1  BASE=*,LABEL=*   *****  LIMIT ACCESS TESTING   *****             
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         L     RF,ATWA                                                          
         L     R6,AIO1            POINT TO CLIENT REC                           
         USING PCLTRECD,R6                                                      
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RF)                                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,CLTOFICE    CLT OR CLT TRAFFIC OFFICE CODE                
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RF)                                                  
         MVC   OFCSECD,ASECBLK    A("SECRET BLOCK")                             
         DROP  R1,R6                                                            
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS    OFFICER CALL                   
         CLI   0(R1),0                                                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* LIST LINE DSECT                                                               
*                                                                               
LINDSECT DSECT                                                                  
LINSELH  DS    CL8                                                              
LINSEL   DS    CL3                                                              
LINIDTH  DS    CL8                                                              
LINIDT   DS    CL8                                                              
LININVH  DS    CL8                                                              
LININV   DS    CL11                                                             
LINPERH  DS    CL8                                                              
LINPER   DS    CL17                                                             
LINESTH  DS    CL8                                                              
LINEST   DS    CL3                                                              
LINGNH   DS    CL8                                                              
LINGN    DS    CL1                                                              
LINAMTH  DS    CL8                                                              
LINAMT   DS    CL11                                                             
LINCDH   DS    CL8                                                              
LINCD    DS    CL1                                                              
LINSREPH DS    CL8                                                              
LINSREP  DS    CL4                                                              
LINNEXTL DS    0C                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE PPMATWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PPMATWK02D                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
         SPACE 2                                                                
       ++INCLUDE PUBNAMEL                                                       
         SPACE 2                                                                
       ++INCLUDE PUBAOVEL                                                       
         SPACE 2                                                                
       ++INCLUDE PUBREPEL                                                       
         SPACE 2                                                                
       ++INCLUDE PUBGENEL                                                       
         SPACE 2                                                                
       ++INCLUDE PUBTAXEL                                                       
         SPACE 2                                                                
       ++INCLUDE PUBLEQEL                                                       
         EJECT                                                                  
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
         EJECT                                                                  
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE BUYDSECTS                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE PPMATFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFED                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATDED          (REPORT SCREEN)                              
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEED          (OUR SUPERCK SCREEN FOR NEWSPAPER)           
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEDD          (OUR SUPERCK SCREEN FOR MAGAZINE)            
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEBD          (OUR CHECK SCREEN FOR NEWSPAPER)             
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATECD          (OUR UPDATE SCREEN FOR NEWSPAPER)            
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDBD          (OUR CHECK SCREEN FOR MAGAZINE)              
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDCD          (OUR UPDATE SCREEN FOR MAGAZINE)             
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATCBD          (OUR COMMENTS SCREEN)                        
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
*                                                                               
*        PROGRAM SAVED STORAGE AT BOTTOM OF TWA0                                
*                                                                               
         ORG   T402FFD+TWAENDLQ    ORG TO BOTTOM OF TWA0                        
*                                                                               
STSAVE   EQU   *                                                                
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
SVSPARE  DS    CL(TWAMXLEN-(*-STSAVE))  SPARE - DEFINE NEW AREAS ABOVE          
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
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
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                 BASE PUB ONLY                                
         DS    XL2                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068PPMAT00   10/31/05'                                      
         END                                                                    
