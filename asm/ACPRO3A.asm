*          DATA SET ACPRO3A    AT LEVEL 018 AS OF 04/28/03                      
*PHASE T60B3AA,*                                                                
         TITLE 'T60B3A - SESSION ESTIMATING INTERFACE'                          
T60B3A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B3A,R7,RR=R2                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         ST    R2,RELO                                                          
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASUBSYSD         R9=ROOT/LOCAL STORAGE                        
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
*                                                                               
         BAS   RE,INITIAL                                                       
         MVI   IOOPT,C'Y'          WRITES/ADDS HANDLED HERE                     
         CLI   ACTNUM,ACTNADD      AM I ADDING?                                 
         BNE   *+8                 NO                                           
         MVI   IOOPT,C'N'          LET GENCON DO THE ADDS                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              MODE CONTROLLER ROUTINES                               *         
***********************************************************************         
*                                                                               
         USING MYSAVED,R5          R5 SET IN INITIAL                            
         L     R5,AMYSAVE                                                       
         BAS   RE,SAVEPF           SAVE PF KEY FOR WHEN I NEED IT               
*                                                                               
         USING TEBLOCK,R6                                                       
         L     R6,AMYTEBLK                                                      
         CLI   RACHANGE,C'Y'       NEW REC/ACT                                  
         BNE   SES5                                                             
         OI    TESTAT,TESTINIT     SET TO REINITIALIZE SCREEN                   
*                                                                               
SES5     CLI   TWALACT,ACTNADD                                                  
         BE    SES10                                                            
         CLI   ACTNUM,ACTNADD                                                   
         BNE   *+8                                                              
         OI    TESTAT,TESTINIT     SET TO REINITIALIZE SCREEN                   
*                                  CAUSE YOU DON'T GET VALKEY                   
*                                                                               
SES10    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   SES11                                                            
         CLI   PREVPF,PFJEST       DO THEY WANT TO CALL JOB EST?                
         BNE   *+8                 NO                                           
         BAS   RE,CHKPF            LET THEM W/O VALIDATING KEY (DELETE)         
         BAS   RE,VKEY                                                          
         CLI   RACHANGE,C'Y'       NEW RA                                       
         BNE   *+8                 NO                                           
         OI    TESCLIH+4,X'80'     SO WE GET DISPREC BEFORE VALREC              
         B     SESX                                                             
*                                                                               
*        CALL VKEY IN CASE GENCON DECIDED YOU DIDN'T WANT TO                    
*                                                                               
SES11    CLI   RETURNED,0          HAVE I JUST RETURNED                         
         BNE   SES12               YES                                          
         CLI   MODE,RECREST        RESTORING                                    
         BNE   SES13               NO                                           
*                                                                               
SES12    BAS   RE,VKEY             RE-VAL KEY, YOU NEVER KNOW                   
*                                                                               
*        PROCESS THE RECORD                                                     
*                                                                               
SES13    BAS   RE,VACT             VALIDATE ACTION                              
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     SESX                                                             
*                                                                               
         CLI   MODE,VALREC         IF VALIDATE RECORD                           
         BNE   SES15                                                            
         BAS   RE,CHKPF            PROCESS ANY PF'S PRESSED                     
         B     SESX                                                             
*                                                                               
SES15    CLI   MODE,DISPREC        IF DISPLAY RECORD                            
         BNE   SES20                                                            
         BAS   RE,SAVEWRK          SAVE WORKCODE DATA FOR ESTUPD                
         BAS   RE,CHKPF            PROCESS ANY PF'S PRESSED                     
         B     SESX                                                             
*                                                                               
SES20    CLI   MODE,XRECADD        IF RECORD ADDED                              
         BNE   SESX                                                             
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(3),=C'CHA'                                                
         MVI   CONACTH+5,X'3'                                                   
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   ACTNUM,ACTCHA                                                    
         B     SESX                                                             
*                                                                               
SESX     BAS   RE,GOSYSEST         GIVE TASYSEST A SHOT                         
*                                                                               
         CLI   PREVPF,PFUPD         UPDATE REQUESTED                            
         BNE   SESY                                                             
         MVI   PREVPF,0                                                         
*                                                                               
         MVI   PREVPF,0                                                         
         MVI   MYMSGNO1,IESTUPD                                                 
         LA    R2,TESESTH                                                       
         B     INFEXIT                                                          
*                                                                               
SESY     CLI   MODE,DISPREC        IF MODE IS DISPLAY RECORD                    
         BNE   SESZ                                                             
         CLI   RETURNED,0          HAD I JUST RETURNED                          
         BE    SESZ                                                             
*                                                                               
         LA    R2,CONRECH                                                       
         SR    RF,RF                                                            
SESY40   IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   SESY40                                                           
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT WHOLE SCREEN           
*                                                                               
SESZ     EQU   *                                                                
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE ACTION                                    *         
***********************************************************************         
*                                                                               
VACT     NTR1                                                                   
         CLI   SVESTAPP,C'Y'       IS THE ESTIMATE APPROVED                     
         BNE   VACTX               NO ACCEPT ALL ACTIONS                        
         CLI   ACTNUM,ACTNADD                                                   
         BE    VACT20                                                           
         CLI   ACTNUM,ACTNCHA                                                   
         BNE   VACTX                                                            
*                                                                               
VACT20   MVI   ERROR,ESTISAP                                                    
         LA    R2,TESESTH                                                       
         ST    R2,ACURFORC                                                      
         B     ERREND                                                           
*                                                                               
VACTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE KEY                                       *         
***********************************************************************         
*                                                                               
VKEY     NTR1                                                                   
         BAS   RE,VALHED                                                        
         BAS   RE,RDOPT            CHECK OPTIONS FOR SCHEME                     
         BAS   RE,VALEST                                                        
*                                                                               
         BAS   RE,VALTMEDF                                                      
*                                                                               
         LA    R2,TESMEDH          SEE IF MEDIA IS NEW  (I/P)                   
         BAS   RE,TSTKEY                                                        
*                                                                               
         USING TEBLOCK,R6                                                       
         L     R6,AMYTEBLK                                                      
         MVC   TEMEDIA,TESMED      SAVE MEDIA IN TAL INTERFACE BLOK             
*                                                                               
VKEYX    CLI   KEYCHG,C'Y'         KEY CHANGED?                                 
         BNE   VKEYXXX                                                          
VKEYXX   OI    TESTAT,TESTINIT     SET TO REINITIALIZE SCREEN                   
         MVI   PREVPF,0                                                         
         MVC   TESDES,SPACES                                                    
         MVC   TESFLT,SPACES                                                    
*                                                                               
VKEYXXX  BAS   RE,BLDSESKY         SET KEY                                      
         MVC   AIO,AIO1            FOR ACTION ADD, GENCON WILL INITIAL          
*                                  AIO, SO MAKE SURE ITS SET TO THE             
*                                  IOAREA YOU WANT TO USE                       
         B     XIT                                                              
*                                                                               
BLDSESKY LA    R4,KEY              BUILD KEY                                    
         USING SESRECD,R4                                                       
         XC    SESKEY,SESKEY                                                    
         MVI   SESKTYP,SESKTYPQ                                                 
         MVI   SESKSUB,SESKSUBQ                                                 
         MVC   SESKCUL,CUL                                                      
         MVC   SESKCLI,CLICODE                                                  
         MVC   SESKPRO,PRODCODE                                                 
         MVC   SESKJOB,JOBNUM                                                   
         MVC   SESKTYPE,TESEST     R OR P FROM SCREEN                           
         MVC   SESKVER,ESTNUM      SET IN VALEST                                
         MVC   SESKMED,TESMED                                                   
         MVC   SVSESKEY,SESKEY     SAVE IN SAVED STORAGE                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE MEDIA                                     *         
***********************************************************************         
*                                                                               
VALTMEDF NTR1                                                                   
         LA    R2,TESMEDH                                                       
         CLI   KEYCHG,C'Y'         HAS KEY CHANGED                              
         BNE   *+8                 NO                                           
         NI    4(R2),X'FF'-X'20'   YES, UNVALIDATE MEDIA                        
*                                                                               
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED IN CASE SET            
*                                                                               
         USING SESRECD,R6                                                       
         LA    R6,KEY                                                           
         BAS   RE,GETMEDIA                                                      
*                                                                               
         CLI   5(R2),0             ANY I/P                                      
         BE    VALT10              NO                                           
*                                                                               
         TM    4(R2),X'80'         I/P THIS TIME?                               
         BO    VALT50              YES, VALIDATE                                
*                                                                               
         TM    4(R2),X'20'         PREVOIUSLY VALIDATED                         
         BO    VALT50              YES, USE IT                                  
*                                                                               
         MVI   5(R2),0             REFRESH FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   8(R2),C' '                                                       
*                                                                               
VALT10   BAS   RE,GETSES           GET EXISTING RECORD                          
         BNE   VALT20                                                           
*                                                                               
         MVC   8(1,R2),SESKMED     USE MEDIA FROM RECORD                        
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         B     VALT50                                                           
*                                                                               
VALT20   OC    TAMEDIA,TAMEDIA     IS THE MEDIA DEFINED ON THE JOB              
         BZ    VALT30              NO                                           
*                                                                               
         MVC   8(1,R2),TAMEDIA     USE MEDIA AS DEFINED                         
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         B     VALT50                                                           
*                                                                               
*                                                                               
VALT30   MVI   ERROR,MISSING       NOT DEFINED ON JOB, GET FROM SCREEN          
         CLI   5(R2),0                                                          
         BE    VALTERR                                                          
*                                                                               
*                                                                               
VALT50   MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'T'          TV                                           
         BE    VALTMOK                                                          
         CLI   8(R2),C'R'                                                       
         BE    VALTMOK                                                          
         B     VALTERR                                                          
*                                                                               
VALTMOK  CLI   ACTNUM,ACTNADD      CHECK FOR ACTION ADD OR RESORE               
         BE    VALTMOK5                                                         
*                                                                               
         CLI   ACTNUM,ACTNRES                                                   
         BNE   VALTX                                                            
*                                                                               
VALTMOK5 MVI   ERROR,BADTMED                                                    
         BAS   RE,GETSES                                                        
         BNE   VALTX                          NP                                
*                                                                               
         CLC   SESKMED,8(R2)       DOES SESSION EXIST W/ A DIF MEDIA            
         BNE   VALTERR             YES                                          
*                                                                               
         CLI   ACTNUM,ACTNADD                                                   
         BNE   VALTX                                                            
*                                                                               
         LA    R3,KEY                                                           
         TM    ACCOSTAT(R3),X'80'                                               
*                                                                               
         MVI   ERROR,RECEXIST      IF ADDING, FLAGE AS ALREADY EXISTING         
         B     VALTERR                                                          
*                                                                               
VALTX    OI    TESMEDH+4,X'20'                                                  
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED                        
         B     XIT                                                              
*                                                                               
VALTERR  NI    TESCLIH+4,X'FF'-X'20' IF ERROR, UNVAL CLIENT                     
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED                        
         B     ERREND                                                           
*                                                                               
GETSES   ST    RE,SAVERE                                                        
         BAS   RE,BLDSESKY                                                      
         XC    SESKMED,SESKMED                                                  
         XC    SESKSEQ,SESKSEQ                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         L     RE,SAVERE                                                        
         CLC   SESKEY(SESKMED-SESKEY),KEYSAVE SAME ESTIMATE                     
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE A RECORD                                      *         
***********************************************************************         
*                                                                               
VREC     NTR1                                                                   
         LA    R2,TESDESH          WRITE THE NAME OF THIS REC                   
         MVI   ELCODE,NAMELQ                                                    
         GOTO1 REMELEM             REMOVE FILTER VALUE ELEMENT                  
         CLI   5(R2),0             ANY NAME                                     
         BE    VREC10              NO                                           
         GOTO1 NAMEIN                                                           
*                                                                               
VREC10   GOTO1 PERSIN              ADD A ACTIVITY ELEMENT                       
*                                                                               
         LA    R2,TESFLTH                                                       
*                                                                               
*                                                                               
         USING FFTELD,R6                                                        
         MVI   ELCODE,FFTELQ                                                    
         GOTO1 REMELEM             REMOVE FILTER VALUE ELEMENT                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
*                                                                               
         XC    ELEMENT,ELEMENT     ADD SESSION FILTER EL                        
         USING FFTELD,R6                                                        
         LA    R6,ELEMENT                                                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTTFLT                                                 
         MVC   FFTDATA(L'TESFLT),SPACES                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FFTDATA(0),TESFLT                                                
*                                                                               
         LA    R1,1(R1)            SET ELEMENT LENGTHS                          
         STC   R1,FFTDLEN                                                       
         AH    R1,=Y(FFTDATA-FFTELD)                                            
         STC   R1,FFTLN                                                         
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VRECX    L     R1,AIO                                                           
         CLI   1(R1),X'3C'         BAD REC IN AIO                               
         BE    *+6                 NOT YET                                      
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DISPLAY THE KEY                                        *         
***********************************************************************         
*                                                                               
DKEY     NTR1                                                                   
         USING TEBLOCK,R6                                                       
         L     R6,AMYTEBLK                                                      
         CLC   SVKEY,KEY           IF KEY CHANGED                               
         BE    *+8                                                              
         OI    TESTAT,TESTINIT     SET TO RE-INITIALIZE                         
*                                                                               
         USING SESRECD,R4                                                       
         L     R4,AIO1                                                          
         MVC   TESCLI,SESKCLI                                                   
         MVC   TESPRO,SESKPRO                                                   
         MVC   TESJOB,SESKJOB                                                   
         MVC   TESEST,SESKTYPE                                                  
         MVC   TESMED,SESKMED                                                   
         OI    TESCLIH+6,X'80'                                                  
         OI    TESPROH+6,X'80'                                                  
         OI    TESJOBH+6,X'80'                                                  
         OI    TESESTH+6,X'80'                                                  
         OI    TESMEDH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DISPLAY THE RECORDS                                    *         
***********************************************************************         
*                                                                               
DREC     NTR1                                                                   
         MVC   TESDES,SPACES                                                    
         LA    R2,TESDESH          WRITE THE NAME OF THIS REC                   
         GOTO1 NAMEOUT                                                          
         OI    TESDESH+6,X'80'                                                  
*                                                                               
         GOTO1 PERSOUT                                                          
*                                                                               
         LA    R2,TESACTVH                                                      
         MVC   8(L'TESACTV,R2),WORK+20  "NAME" ON "DATE"                        
         OI    6(R2),X'80'                                                      
*                                                                               
         USING FFTELD,R6                                                        
         L     R6,AIO              DISPLAY TALENT MEDIA                         
         MVI   ELCODE,FFTELQ                                                    
         BAS   RE,GETEL                                                         
*                                                                               
DREC20   LA    R2,TESFLTH                                                       
         MVC   TESFLT,SPACES                                                    
         OI    6(R2),X'80'                                                      
         CLI   FFTTYPE,FFTTTFLT    IS THIS SESSION FILTER VALUES                
         BNE   DREC70              NO                                           
         ZIC   R1,FFTDLEN                                                       
         LTR   R1,R1                                                            
         BZ    DREC70                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TESFLT(0),FFTDATA                                                
*                                                                               
DREC70   BAS   RE,NEXTEL                                                        
         BE    DREC20                                                           
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE HEADLINE FIELDS                           *         
***********************************************************************         
*                                                                               
VALHED   NTR1  ,                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 SETHEIR                                                          
         MVI   OPTION,0            NO NAMES TO DISPLAY                          
         MVI   KEYCHG,C'N'                                                      
*                                                                               
         LA    R2,TESCLIH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALCLI                                                           
         MVC   SVCLI,CLICODE                                                    
*                                                                               
         LA    R2,TESPROH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALPROD                                                          
         MVC   SVPROD,PRODCODE                                                  
*                                                                               
         LA    R2,TESJOBH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   OPTION,C'Y'         GET JOB NAME                                 
         GOTO1 VALJOB                                                           
         MVC   SVJOB,JOBNUM                                                     
         MVI   OPTION,0            RESET FOR NEXT                               
*                                                                               
         MVI   ERROR,OLDESERR                                                   
         TM    JOBJSTAT,JOBSNEST   INSURE JOB USES NEW ESTIMATES                
         BZ    ERREND                                                           
*                                                                               
         OI    TESCLIH+4,X'20'     TURN ON PREV VALID BITS                      
         OI    TESPROH+4,X'20'                                                  
         OI    TESJOBH+4,X'20'                                                  
         OI    TESESTH+4,X'20'                                                  
         OI    TESMEDH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE AN ESTIMATE EXPRESSION                        *         
*              RETURNS VERSION NUMBER IN ESTNUM                       *         
***********************************************************************         
*                                                                               
VALEST   NTR1                                                                   
         LA    R2,TESESTH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R4,KEY                                                           
         USING EVERECD,R4                                                       
         XC    EVEKEY,EVEKEY                                                    
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
         MVC   EVEKCLI,CLICODE                                                  
         MVC   EVEKPRO,PRODCODE                                                 
         MVC   EVEKJOB,JOBNUM                                                   
*                                                                               
         LA    R2,TESESTH                                                       
         MVC   EVEKTYPE,8(R2)                                                   
*                                                                               
         ZIC   R6,5(R2)                                                         
         BCTR  R6,0                                                             
         CH    R6,=H'3'            TEST MORE THAN THREE DIGITS                  
         BH    VALESTN                                                          
*                                                                               
         CLI   8(R2),C'R'                                                       
         BE    VALEST5                                                          
         CLI   8(R2),C'P'                                                       
         BNE   VALESTN                                                          
*                                                                               
VALEST5  LA    RE,9(R2)            RE=A(NUMBER)                                 
         LR    RF,R6               RF=LOOP COUNTER                              
VALEST7  CLI   0(RE),C'0'          TEST FOR VALID VERSION NUMBER                
         BL    VALESTN                                                          
         CLI   0(RE),C'9'                                                       
         BH    VALESTN                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VALEST7                                                       
*                                                                               
VALEST8  BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,9(0,R2)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    VALESTN             CANNOT BE VERSION ZERO                       
         STC   R0,EVEKVER                                                       
         MVC   ESTNUM,EVEKVER                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 READ                                                             
*                                                                               
         MVI   SVESTAPP,C'N'       ASSUME EST IS NOT APPROVED                   
         CLI   GONEEDAE,C'Y'       NEED APPROVAL HERE?                          
         BNE   VALEST20            NO                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EAPELQ       IS ESTIMATE APPROVED                         
         BAS   RE,GETEL                                                         
         BNE   VALEST20            NO                                           
         MVI   SVESTAPP,C'Y'                                                    
*                                                                               
VALEST20 MVC   AIO,AIO1                                                         
         B     VALESTX                                                          
*                                                                               
VALESTN  EQU   *                                                                
         B     ERREND                                                           
*                                                                               
VALESTX  OI    TESESTH+4,X'20'                                                  
         MVC   SVESTKEY,KEY        SAVE IN SAVED STORAGE                        
         B     YESXIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              TEST IF ANYTHING CHANGED                               *         
***********************************************************************         
*                                                                               
TSTKEY   TM    4(R2),X'80'         I/P THIS TIME?                               
         BO    TSTKY                                                            
         TM    4(R2),X'20'         PREV VALIDATED                               
         BOR   RE                                                               
         TM    4(R2),X'40'         I/P PREVOIUSLY?                              
         BOR   RE                                                               
TSTKY    MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              GET THE SJ MEDIA                                       *         
***********************************************************************         
*                                                                               
GETMEDIA NTR1                                                                   
         XC    TAMEDIA,TAMEDIA                                                  
*                                                                               
GETM40   TM    JOBJSTAT,JOBSRAD    IS THIS A RADIO JOB                          
         BO    GETMR                                                            
*                                                                               
         TM    JOBJSTAT,JOBSTV     IS THIS A TV JOB                             
         BO    GETMT                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO2                                                         
         USING PMDRECD,R6                                                       
         LA    R6,KEY                                                           
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUL                                                      
         MVC   PMDKMED,MEDIA                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(PMDKEND),KEYSAVE                                             
         BNE   ERREND                                                           
*                                                                               
         USING PMDELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,PMDELQ                                                    
         BAS   RE,GETEL                                                         
         MVC   AIO,SVAIO                                                        
         TM    PMDSTAT,PMDSTV                                                   
         BO    GETMT                                                            
         TM    PMDSTAT,PMDSRAD                                                  
         BNO   NOXIT                                                            
*                                                                               
GETMR    MVI   TAMEDIA,C'R'                                                     
         B     YESXIT                                                           
GETMT    MVI   TAMEDIA,C'T'                                                     
         B     YESXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              READ THE JOB'S OPTIONS                                 *         
***********************************************************************         
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         LA    RE,GOBLOCK                                                       
         LA    RF,L'GOBLOCK                                                     
         XCEF                                                                   
*                                                                               
         MVC   GOADM,DATAMGR       READ THE JOB'S OPTIONS                       
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVI   GOWHICH,C'N'        ONLY NEW OPTIONS                             
*        MVC   GOAJOB,AJOB                                                      
         LA    R1,GOXBLOCK                                                      
         ST    R1,GOAEXT                                                        
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVI   ERROR,MISSSCH                                                    
         LA    R2,TESCLIH                                                       
         OC    GOSCHEME,GOSCHEME                                                
         BZ    ERREND              NO                                           
*                                                                               
         USING MYSAVED,R5          R5 SET IN INITIAL                            
         L     R5,AMYSAVE                                                       
         MVC   SVEFFOFC,GOEFFOFC                                                
RDOPTX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              ALLOCATE SAVED STORAGE                                 *         
***********************************************************************         
*                                                                               
INITIAL  NTR1                                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(TWAENDLQ)                                                  
         ST    RE,AMYTEBLK                                                      
         LA    RE,TEBLKLNQ(RE)                                                  
         ST    RE,AMYSAVE                                                       
         LA    RE,MYSAVELN(RE)                                                  
         ST    RE,ATAWORK                                                       
         MVC   RCPROG+2(2),=C'SE'                                               
*                                                                               
         XI    DMINBTS,X'80'       TURN OFF FULLTRACK READ, INCASE SET          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              CHECK PF KEYS                                          *         
***********************************************************************         
*                                                                               
CHKPF    NTR1                                                                   
         CLI   PREVPF,PFUPD         UPDATE REQUESTED                            
         BNE   CHKP10                                                           
*                                                                               
         GOTO1 =A(UPDTEST),DMCB,(RC),RR=RELO                                    
         MVI   ERROR,0             FOR GENCON                                   
         MVI   PFKEY,0             FOR PRO00                                    
         MVI   PFAID,0             FOR TASYSEST                                 
         BAS   RE,GETCALL                                                       
         CLI   BYTE,X'32'                                                       
         BE    CHKPRET             RETURN TO JOB EST                            
*                                                                               
         B     XIT                 XIT W/O CLEARING PREVPF                      
CHKP10   CLI   PREVPF,PFJEST       CALL JOB ESTIMATE                            
         BNE   CHKP40              NO                                           
         MVI   ERROR,0             FOR GENCON                                   
         MVI   PFKEY,0             FOR PRO00                                    
         MVI   PFAID,0             FOR TASYSEST                                 
*                                                                               
         BAS   RE,GETCALL                                                       
         CLI   BYTE,X'32'          CALLED FROM JOB EST                          
         BE    CHKPX               YES                                          
*                                                                               
         GOTO1 VCALL,WORK,RECNJOB,ACTNEST,(6,SVCLI),(6,SVPROD),        X        
               (6,SVJOB),(4,TESEST),0                                           
*                                                                               
CHKP40   CLI   PREVPF,PFREP        REPORT                                       
         BNE   CHKPX               NO                                           
         MVI   ERROR,0             FOR GENCON                                   
         MVI   PFKEY,0             FOR PRO00                                    
         MVI   PFAID,0             FOR TASYSEST                                 
         MVI   PREVPF,0                                                         
*                                                                               
         BAS   RE,SESREP                                                        
*                                                                               
CHKPX    MVI   PREVPF,0                                                         
         B     XIT                                                              
*                                                                               
CHKPRET  MVI   PREVPF,0                                                         
         GOTO1 VRETURN             RETURN TO JOB EST                            
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
*                                                                               
SAVEPF   EQU   *                   SAVE PFAID, IF PRESSED                       
         ST    RE,SAVERE                                                        
         CLI   PFAID,0                                                          
         BE    *+10                                                             
         MVC   PREVPF,PFAID                                                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              RETURN OVERLAY OF CALLER IN BYTE                       *         
***********************************************************************         
*                                                                               
GETCALL  ST    RE,SAVERE                                                        
         MVI   BYTE,0                                                           
         SR    RE,RE                                                            
         ICM   RE,1,CALLSP                                                      
         LTR   RE,RE               ANYTHING IN CALL STACK                       
         BZ    GETCALLX            NO                                           
         BCTR  RE,0                                                             
         LA    RE,CALLSTK(RE)                                                   
         MVC   BYTE,0(RE)                                                       
GETCALLX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD THE SESSION REPORT                               *         
***********************************************************************         
*                                                                               
SESREP   NTR1  WORK=(R2,BUFLEN)                                                 
         ST    R2,ASESREC                                                       
*                                                                               
         GOTO1 =A(GETPRO),DMCB,(RC),RR=RELO GET PROFILE VALUES                  
*                                                                               
         MVC   REMUSER,TWAALIAS    SUPPLY REQUESTOR ID                          
         GOTO1 OPENPQ                                                           
*                                                                               
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         L     R1,=A(PRTSPECS)     LANDSCAPE PRINT SPECS                        
         L     R2,=A(PRTTAB)       PRINT DSECT                                  
         L     R3,=A(PRTHEAD)      HEADERS                                      
         MVI   MAXLINES,60                                                      
*                                                                               
         CLI   PROGPROF+6,C'Y'     DO THEY WANT PORTRAIT PRINTING?              
         BNE   SESR10              NO                                           
*                                                                               
         L     R1,=A(PORSPECS)                                                  
         L     R2,=A(PORTAB)                                                    
         L     R3,=A(PORHEAD)                                                   
         MVI   MAXLINES,88                                                      
*                                                                               
SESR10   A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         A     R2,RELO                                                          
         ST    R2,FLDTAB                                                        
         A     R3,RELO                                                          
         ST    R3,HEADTAB                                                       
*                                                                               
         CLI   PROGPROF+7,0        MAXLINES PASSED IN PROFILE                   
         BE    *+10                NO                                           
         MVC   MAXLINES,PROGPROF+7                                              
*                                                                               
         BAS   RE,ACCNAME          GET NAMES                                    
*                                                                               
         MVC   KEY,SVSESKEY        SET KEY                                      
*                                                                               
         BAS   RE,GETSREC          GET SESSION RECORD FROM FILE                 
         BE    SESR20                                                           
*                                                                               
         LA    R2,TESMEDH                                                       
         MVI   ERROR,NOTFOUND                                                   
         B     ERREND                                                           
*                                                                               
SESR20   EQU   *                                                                
         XC    PREVTYPE,PREVTYPE                                                
*                                                                               
         L     R6,ASESREC                                                       
         MVI   ELCODE,TECELQ       GET CAST DETAILS                             
         BAS   RE,GETEL                                                         
         BNE   SESR60              NO DETAILS, NO REPORT                        
*                                                                               
         USING TAECD,R6                                                         
SESR25   L     R5,FLDTAB           MOVE DETAIL FIELDS INTO PRINT LINE           
         XR    R1,R1                                                            
*                                                                               
SESR30   CLI   0(R5),X'FF'                                                      
         BNE   *+6                 UNKNOWN TYPE IN DETAIL ELEMENT               
         DC    H'0'                                                             
         CLC   TAECTYPE,0(R5)                                                   
         BE    SESR40                                                           
*                                                                               
         IC    R1,1(R5)            GET NEXT TYPE FROM TABLE                     
         LA    R5,0(R1,R5)                                                      
         B     SESR30                                                           
*                                                                               
SESR40   CLC   TAECTYPE,PREVTYPE   IS THIS A NEW TYPE                           
         BE    SESR49              NO                                           
         OC    PREVTYPE,PREVTYPE   IS THIS THE FIRST TYPE                       
         BZ    SESR45              YES                                          
         BAS   RE,TYPECOMM         PRINT THE COMMENT DATA                       
         BAS   RE,SUBTOTS          PRINT SUBTOTALS                              
         BAS   RE,PRINTEM          SKIP A LINE                                  
*                                                                               
SESR45   MVC   BYTE,TAECTYPE                                                    
         BAS   RE,SETHEAD                                                       
         BAS   RE,SUBCLR           CLEAR SUBTOTALS                              
*                                                                               
SESR49   MVC   PREVTYPE,TAECTYPE                                                
         CLI   PREVTYPE,TAECTYPM   MISCELLANEOUS?                               
         BE    SESR80              SPECIAL STUFF                                
*                                                                               
         IC    R1,2(R5)            NUMBER OF FIELDS NEEDED IN THIS EL           
         LA    R5,3(R5)                                                         
*                                                                               
         USING PRTTABD,R5                                                       
SESR50   BAS   RE,EDITIT           MOVE THE FIELD TO P                          
         LA    R5,PRTTNEXT                                                      
         BCT   R1,SESR50                                                        
*                                                                               
         BAS   RE,PRINTEM          PRINT THE LINE                               
         BAS   RE,ADD2SUB          ADD BUCKETS TO SUBTOTALS                     
*                                                                               
         MVI   ELCODE,TECELQ       GET MORE DETAILS                             
         BAS   RE,NEXTEL                                                        
         BE    SESR25                                                           
*                                                                               
         OC    PREVTYPE,PREVTYPE   ANYTHING HAPPEN?                             
         BZ    SESR60              NO                                           
*                                                                               
         BAS   RE,TYPECOMM         PRINT COMMENT DATA                           
         BAS   RE,SUBTOTS          PRINT SUBTOTALS                              
         BAS   RE,FINLTOTS         PRINT FINAL TOTALS                           
*                                                                               
SESR60   MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,CONHEAD          R4=OUTPUT POINTER                            
         MVC   0(2,R4),=C'**'                                                   
         MVC   2(3,R4),SPOOLID                                                  
         MVI   5(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,6(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,7(R4)                                                         
         MVC   0(9,R4),=C'SPOOLED**'                                            
         LA    R2,TESESTH                                                       
         ST    R2,ACURFORC                                                      
         MVI   ERROR,X'FE'                                                      
         OI    GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
*                                                                               
SESR80   LA    R1,4                DO FIRST 4 NORMALLY                          
         LA    R5,3(R5)                                                         
*                                                                               
         USING PRTTABD,R5                                                       
SESR85   BAS   RE,EDITIT           MOVE THE FIELD TO P                          
         LA    R5,PRTTNEXT                                                      
         BCT   R1,SESR85                                                        
*                                                                               
         BAS   RE,EDITIT2          TOTAL T&H AND PRINT                          
         LA    R5,PRTTNEXT         BUMP PAST HANDLING                           
         LA    R5,PRTTNEXT         BUMP TO COMMENTS                             
         LA    R1,1                ONLY 1 LEFT                                  
         B     SESR50              JOIN EXISTING CODE                           
         EJECT                                                                  
***********************************************************************         
*              SEND IT TO SPOOL                                       *         
***********************************************************************         
*                                                                               
PRINTEM  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PRINT A FIELD, 0(R6), IN PRINT POSITION, 0(R5)         *         
***********************************************************************         
*                                                                               
EDITIT   NTR1                                                                   
         USING PRTTABD,R5                                                       
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         XC    EBLOCK,EBLOCK                                                    
         IC    RF,PRTTEDIT                                                      
         BCTR  RF,0                RF MINUS 1                                   
         SLL   RF,2                TIMES 4                                      
         B     EDITIT20(RF)                                                     
*                                                                               
EDITIT20 B     AEDNUM                                                           
         B     AEDCHAR                                                          
         B     AEDCOM                                                           
         B     AEDSTAT                                                          
         B     AEDAMNT                                                          
         B     AEDDOL                                                           
         DC    H'0'                                                             
EDNUM    EQU   1                                                                
EDCHAR   EQU   2                                                                
EDCOM    EQU   3                                                                
EDSTAT   EQU   4                                                                
EDAMNT   EQU   5                                                                
EDDOL    EQU   6                                                                
*                                                                               
AEDNUM   EQU   *                                                                
         MVI   EBDECS,0                                                         
         MVI   EBFLOAT,C'-'                                                     
         B     CALLEDIT                                                         
*                                                                               
AEDAMNT  EQU   *                                                                
         MVI   EBDECS,2                                                         
         MVI   EBOPT,EBOQMEY                                                    
         B     CALLEDIT                                                         
*                                                                               
AEDDOL   EQU   *                                                                
         MVI   EBDECS,2                                                         
         MVI   EBOPT,EBOQMEY+EBOQZEN                                            
         B     CALLEDIT                                                         
*                                                                               
CALLEDIT MVI   EBTIN,C'B'          BINARY                                       
         MVI   EBSCIN,0                                                         
*                                                                               
         MVI   EBTRIM,X'80'                                                     
         MVC   EBLIN,PRTTFLLN                                                   
         MVC   EBLOUT,PRTTPRLN                                                  
*                                                                               
         LA    R4,P                SET O/P FIELDS                               
         IC    R1,PRTTPROF                                                      
         LA    R4,0(R1,R4)                                                      
         ST    R4,EBAOUT           OUTPUT AREA                                  
*                                                                               
         LR    R4,R6               SET I/P FIELDS                               
         IC    R1,PRTTFLOF                                                      
         LA    R4,0(R1,R4)                                                      
         ST    R4,EBAIN            OUTPUT DATA                                  
*                                                                               
         GOTO1 EDITOR,PARAS,EBLOCK                                              
         B     EDITITX                                                          
*                                                                               
AEDCHAR  EQU   *                                                                
         LR    R3,R6                                                            
         IC    R1,PRTTFLOF                                                      
         LA    R3,0(R1,R3)                                                      
*                                                                               
         LA    R4,P                SET O/P FIELDS                               
         IC    R1,PRTTPROF                                                      
         LA    R4,0(R1,R4)                                                      
*                                                                               
         IC    R1,PRTTFLLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         B     EDITITX                                                          
*                                                                               
AEDCOM   EQU   *                   EDIT THE VARIBLE LENGTH COMMENT              
         LR    R3,R6                                                            
         LA    R4,P                SET O/P FIELDS                               
         IC    R1,PRTTPROF                                                      
         LA    R4,0(R1,R4)                                                      
         IC    R1,PRTTFLOF                                                      
         LA    R3,0(R1,R3)                                                      
*                                                                               
         IC    R1,1(R6)            ESTABLISH LENGTH FROM ELEMENT DATA           
         SH    R1,=Y(TAECCMNT-TAECD)                                            
         BNP   EDITITX                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         B     EDITITX                                                          
*                                                                               
AEDSTAT  EQU   *                                                                
         LR    R3,R6                                                            
         IC    R1,PRTTFLOF                                                      
         LA    R3,0(R1,R3)                                                      
         LA    R4,P                                                             
         IC    R1,PRTTPROF                                                      
         LA    R4,0(R1,R4)                                                      
         IC    R1,PRTTBIT          BIT VALUE TO TEST FOR                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R3),0                                                          
         BZ    EDITITX                                                          
         MVC   0(1,R4),PRTTCHAR    IF BIT IS ON, PRINT CHAR                     
EDITITX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PRINT DETAIL INFORMATION                               *         
*              DATA = 0(R6), PRINT POSITION = 0(R5)                   *         
*              SPECIAL VERSION OF EDITIT TO HANDLE TAX AND HANDLING   *         
***********************************************************************         
*                                                                               
EDITIT2  NTR1                                                                   
         USING PRTTABD,R5                                                       
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBDECS,2                                                         
         MVI   EBOPT,EBOQMEY+EBOQZEN                                            
         MVI   EBTIN,C'B'          BINARY                                       
         MVI   EBSCIN,0                                                         
*                                                                               
         MVI   EBTRIM,X'80'                                                     
         MVC   EBLIN,PRTTFLLN                                                   
         MVC   EBLOUT,PRTTPRLN                                                  
*                                                                               
         LA    R4,P                TAX AND HANDLING PRINT TOGETHER              
         IC    R1,PRTTPROF                                                      
         LA    R4,0(R1,R4)                                                      
         ST    R4,EBAOUT                                                        
*                                                                               
         LR    R4,R6               SET I/P FIELDS                               
         IC    R1,PRTTFLOF         GET TAX                                      
         LA    R4,0(R1,R4)                                                      
         ST    R4,EBAIN                                                         
*                                                                               
         LA    R5,PRTTNEXT         GET HANDLING                                 
         IC    R1,PRTTFLOF                                                      
         LA    RF,0(R1,R6)                                                      
*                                                                               
         ICM   R1,15,0(R4)         ADD TAX AND HANDLING                         
         A     R1,0(RF)                                                         
         STCM  R1,15,0(R4)         STORE BACK IN TAX                            
*                                                                               
         BAS   RE,ADDEMUP          ADD TO FINAL TOTAL                           
*                                                                               
         GOTO1 EDITOR,PARAS,EBLOCK                                              
*                                                                               
         CLI   BYTE,FINLHEAD       DOING FINAL TOTALS?                          
         BNE   EDITITX             NO                                           
*                                                                               
         LA    R4,P                GET ADDRESS OF TOTAL                         
         SR    R1,R1                                                            
         IC    R1,PRTTPROF                                                      
         LA    R4,0(R1,R4)                                                      
         ST    R4,EBAOUT                                                        
*                                                                               
         LA    R4,FINTOTAL         PRINT FINAL TOTAL                            
         ST    R4,EBAIN                                                         
         GOTO1 EDITOR,PARAS,EBLOCK                                              
         B     EDITITX                                                          
         EJECT                                                                  
***********************************************************************         
*              SET THE HEADINGS UP                                    *         
***********************************************************************         
*                                                                               
SETHEAD  NTR1                                                                   
         L     R2,HEADTAB                                                       
SETH10   CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   BYTE,0(R2)                                                       
         BE    SETH20                                                           
         LA    R2,L'PRTHEAD(R2)                                                 
         B     SETH10                                                           
*                                                                               
SETH20   MVC   MID1,1(R2)                                                       
         MVI   FORCEMID,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADD TAECAMTS TO SUBTOTAL                                     *         
***********************************************************************         
*                                                                               
         USING TAECD,R6                                                         
ADD2SUB  NTR1                                                                   
         LA    R5,SUBTOTAL         EDIT THE FIRST TOTAL BUCKET                  
         LA    R6,TAECAMTS                                                      
         LA    R0,TAECAMTL/L'TAECAMTS                                           
*                                                                               
ADD2S02  ICM   R1,15,0(R5)                                                      
         A     R1,0(R6)                                                         
         STCM  R1,15,0(R5)                                                      
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BCT   R0,ADD2S02                                                       
*                                                                               
         ZIC   R1,TYPECNT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,TYPECNT                                                       
         B     XIT                                                              
*                                                                               
*                                                                               
SUBCLR   NTR1                                                                   
         XC    TYPECNT,TYPECNT                                                  
         LA    R2,SUBTOTAL                                                      
         LA    R0,TAECAMTL/L'TAECAMTS                                           
*                                                                               
SUBC02   XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         BCT   R0,SUBC02                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DO SUBTOTALS, IF MORE THAN 1 LINE                      *         
***********************************************************************         
*                                                                               
SUBTOTS  NTR1                                                                   
         CLI   TYPECNT,1           MORE THAN ONE LINE HERE                      
         BNH   XIT                 NO                                           
*                                                                               
         CLI   PREVTYPE,TAECTYPM   WAS THIS MISC                                
         BE    SUBT04              YES, DIFFRENT TOTALING                       
*                                                                               
         USING PRTD,R4                                                          
         LA    R4,P                                                             
         LA    R4,PRCOM+10                                                      
         LA    R5,EDTOT0           EDIT THE FIRST TOTAL BUCKET                  
         CLI   PROGPROF+6,C'Y'                                                  
         BNE   SUBT02                                                           
*                                                                               
         USING PORD,R4                                                          
         LA    R4,P                                                             
         LA    R4,POCOM+10                                                      
         LA    R5,POEDTOT0                                                      
*                                                                               
SUBT02   MVC   0(9,R4),=C'Subtotal:'                                            
         LA    R6,SUBTOTAL                                                      
         BAS   RE,EDITIT                                                        
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
*                                                                               
         USING PRTTABD,R5                                                       
SUBT04   LA    R0,4                MISCELLANEOUS FIELDS                         
         MVC   P(9),=C'Subtotal:'                                               
         LA    R5,EDTOTM           LANDSCAPE FORMAT                             
         CLI   PROGPROF+6,C'Y'                                                  
         BNE   *+8                                                              
         LA    R5,POEDTOTM         PORTRAIT FORMAT                              
         LA    R6,SUBTOTAL                                                      
*                                                                               
SUBT06   BAS   RE,EDITIT                                                        
         LA    R5,PRTTNEXT                                                      
         LA    R6,4(R6)                                                         
         BCT   R0,SUBT06                                                        
*                                                                               
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
EDTOT0   DC    AL1(0,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)                        
*                                                                               
EDTOTM   DC    AL1(0,L'TAECNET,PRMNET-PRTD,L'PRMNET,EDDOL)                      
         DC    AL1(0,L'TAECNET,PRMPH-PRTD,L'PRMPH,EDDOL)                        
         DC    AL1(0,L'TAECNET,PRMHW-PRTD,L'PRMHW,EDDOL)                        
         DC    AL1(0,L'TAECNET,PRMTH-PRTD,L'PRMTH,EDDOL)                        
*                                                                               
POEDTOT0 DC    AL1(0,L'TAECNET,PONET-PORD,L'PONET,EDDOL)                        
*                                                                               
POEDTOTM DC    AL1(0,L'TAECNET,POMNET-PORD,L'POMNET,EDDOL)                      
         DC    AL1(0,L'TAECNET,POMPH-PORD,L'POMPH,EDDOL)                        
         DC    AL1(0,L'TAECNET,POMHW-PORD,L'POMHW,EDDOL)                        
         DC    AL1(0,L'TAECNET,POMTH-PORD,L'POMTH,EDDOL)                        
         EJECT                                                                  
***********************************************************************         
*              DO FINAL TOTALS                                        *         
***********************************************************************         
*                                                                               
FINLTOTS NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZIC   R1,LINE             ROOM FOR TOTALS ON CURRENT PAGE              
         SH    R1,=H'5'                                                         
         CLM   R1,1,MAXLINES                                                    
         BNL   FINLT30             NO                                           
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM          SKIP 2 LINES                                 
*                                                                               
FINLT30  MVI   BYTE,FINLHEAD                                                    
         BAS   RE,SETHEAD                                                       
*                                                                               
         MVI   ELCODE,TETELQ                                                    
         L     R6,ASESREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO TOTAL EL                                  
*                                                                               
         USING PRTTABD,R5                                                       
         LA    R0,3                DO FIRST 3                                   
         LA    R5,EDFTOTS          FINAL TOTAL FIELDS                           
         CLI   PROGPROF+6,C'Y'                                                  
         BNE   *+8                                                              
         LA    R5,POEDFTOT                                                      
         XC    FINTOTAL,FINTOTAL                                                
*                                                                               
FINLT60  BAS   RE,EDITIT                                                        
         BAS   RE,ADDEMUP          ADD TO FINAL TOTAL                           
         LA    R5,PRTTNEXT                                                      
         BCT   R0,FINLT60                                                       
*                                                                               
         BAS   RE,EDITIT2          FINISH LAST TWO DIFFERENTLY                  
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
*                                                                               
ADDEMUP  L     R1,EBAIN            ADD TAX TO HANDLING AND                      
         LA    R2,FINTOTAL         RETURN TOTAL IN FINTOTAL                     
         ICM   RF,15,0(R2)                                                      
         A     RF,0(R1)                                                         
         STCM  RF,15,FINTOTAL                                                   
         BR    RE                                                               
*                                                                               
EDFTOTS  DC    AL1(TAETNET-TAETD,L'TAETNET,PRTNET-PRTD,L'PRTNET,EDDOL)          
         DC    AL1(TAETPNH-TAETD,L'TAETPNH,PRTPNH-PRTD,L'PRTPNH,EDDOL)          
         DC    AL1(TAETHNW-TAETD,L'TAETHNW,PRTHNW-PRTD,L'PRTHNW,EDDOL)          
         DC    AL1(TAETTAX-TAETD,L'TAETTAX,PRTTNH-PRTD,L'PRTTNH,EDDOL)          
         DC    AL1(TAETHND-TAETD,L'TAETHND,PRTTOT-PRTD,L'PRTTOT,EDDOL)          
*                                                                               
POEDFTOT DC    AL1(TAETNET-TAETD,L'TAETNET,POTNET-PORD,L'POTNET,EDDOL)          
         DC    AL1(TAETPNH-TAETD,L'TAETPNH,POTPNH-PORD,L'POTPNH,EDDOL)          
         DC    AL1(TAETHNW-TAETD,L'TAETHNW,POTHNW-PORD,L'POTHNW,EDDOL)          
         DC    AL1(TAETTAX-TAETD,L'TAETTAX,POTTNH-PORD,L'POTTNH,EDDOL)          
         DC    AL1(TAETHND-TAETD,L'TAETHND,POTTOT-PORD,L'POTTOT,EDDOL)          
         EJECT                                                                  
***********************************************************************         
*              PRINT THE COMMENT DATA FOR THE TYPE IN PREVTYPE        *         
***********************************************************************         
*                                                                               
TYPECOMM NTR1                                                                   
         USING PRTD,R5                                                          
         LA    R5,P                                                             
         LA    R5,PRECTAG                                                       
         CLI   PROGPROF+6,C'Y'                                                  
         BNE   TYPEC10                                                          
         USING PORD,R5                                                          
         LA    R5,P                                                             
         LA    R5,POECTAG                                                       
*                                                                               
TYPEC10  MVI   ELCODE,TEXELQ       GET EXTENDED COMMENTS                        
         L     R6,ASESREC                                                       
         BAS   RE,GETEL                                                         
         BNE   TYPECX                                                           
*                                                                               
         MVC   0(8,R5),=C'COMMENT:'                                             
*                                                                               
TYPEC30  CLC   PREVTYPE,2(R6)                                                   
         BNE   TYPEC40                                                          
*                                                                               
         IC    R1,1(R6)                                                         
         SH    R1,=H'5'            EL/LEN/TYPE/SEQ#+1                           
         BM    TYPEC40             NO DATA HERE                                 
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R5),4(R6)                                                    
         BAS   RE,PRINTEM                                                       
*                                                                               
TYPEC40  BAS   RE,NEXTEL                                                        
         BE    TYPEC30                                                          
*                                                                               
TYPECX   MVC   P,SPACES            JUST IN CASE                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              GET CLIENT, PRODUCT AND JOB NAMES                      *         
***********************************************************************         
*                                                                               
ACCNAME  NTR1  ,                                                                
         USING MYSAVED,R5          R5 SET IN INITIAL                            
         L     R5,AMYSAVE                                                       
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(3),SVCLI                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK                                           
*                                                                               
         MVC   KEY+6(3),SVPROD                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK+36                                        
*                                                                               
         MVC   KEY+9(6),SVJOB                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK+72                                        
*                                                                               
ACCNAMEX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PASS CONTROL TO TASYSEST OVERLAY                       *         
***********************************************************************         
*                                                                               
         USING MYSAVED,R5                                                       
GOSYSEST NTR1  WORK=(R2,BUFLEN)                                                 
         L     R5,AMYSAVE                                                       
*                                                                               
         USING TEBLOCK,R6                                                       
         L     R6,AMYTEBLK                                                      
         ST    R2,ASESREC                                                       
         AH    R2,=Y(MAXLEN)                                                    
         ST    R2,TEAOPFKS                                                      
*                                                                               
         CLI   MODE,VALKEY         VALKEY?                                      
         BE    GOSYS5              YES, DON'T NEED RECORD                       
*                                                                               
         CLI   ACTNUM,ACTNADD      IS THIS AN ADD?                              
         BNE   GOSYS1                                                           
*                                                                               
         MVC   ASESREC,AIO1        BUILD IN AIO1                                
         BAS   RE,BLDSESKY         ASSURE KEY IS SET                            
         B     GOSYS5                                                           
*                                                                               
GOSYS1   BAS   RE,GETSREC          GET SESSION RECORD FROM FILE                 
         BE    GOSYS5                                                           
*                                                                               
         LA    R2,TESMEDH                                                       
         MVI   ERROR,NOTFOUND                                                   
         B     ERREND                                                           
*                                                                               
GOSYS5   MVC   TEMAXLEN,=Y(MAXLEN)                                              
         MVC   TEAIO,ASESREC                                                    
         MVC   AIO,ASESREC                                                      
*                                                                               
         LA    RF,12               ADJUST PF                                    
         ZIC   RE,PFAID                                                         
         CR    RE,RF                                                            
         BNH   *+6                                                              
         SR    RE,RF                                                            
         STC   RE,PFAID                                                         
*                                                                               
         CLI   PFAID,PFUPD         FILTER PF'S HANDLED HERE                     
         BNE   *+8                                                              
         MVI   PFAID,0                                                          
         CLI   PFAID,PFJEST                                                     
         BNE   *+8                                                              
         MVI   PFAID,0                                                          
         CLI   PFAID,PFREP                                                      
         BNE   *+8                                                              
         MVI   PFAID,0                                                          
         CLI   PFAID,PFRETRN                                                    
         BNE   *+8                                                              
         MVI   PFAID,0                                                          
*                                                                               
         ST    RC,TEAGEND          A(GEND)                                      
         L     R1,ATAWORK                                                       
         ST    R1,TEAWORKD         A(AREA FOR TALENT SYSTEM WORK AREAS)         
         LA    R1,TESTAGH                                                       
*                                                                               
         ST    R1,TEAOVLYH         A(BEGINNING OF VARIABLE SCREEN)              
         LA    R1,TESOPTH                                                       
         ST    R1,TEAOPTSH         A(OPTIONS FIELD)                             
*                                                                               
         L     R1,=A(AGYTAB)                                                    
         A     R1,RELO                                                          
         LA    R0,AGYTABNM                                                      
         XC    TETALAGY,TETALAGY                                                
GOSYS10  CLC   12(6,R1),AGYSIGN    GET PROD SIGNON FROM TABLE                   
         BNE   GOSYS15                                                          
*                                                                               
         CLC   6(6,R1),SPACES      IS THERE A CLIENT DEFINED                    
         BE    GOSYS20             NO, USE THIS ENTRY                           
*                                                                               
         CLC   6(6,R1),SVCLI       MATCH ON CLIENT                              
         BE    GOSYS20             YUP                                          
*                                                                               
GOSYS15  LA    R1,L'AGYTAB(R1)     NEXT TABLE ENTRY                             
         BCT   R0,GOSYS10                                                       
         B     *+10                                                             
GOSYS20  MVC   TETALAGY,0(R1)                                                   
*                                                                               
         GOTO1 =A(GETPRO),DMCB,(RC),RR=RELO GET PROFILE VALUES                  
         XC    TENMISCS,TENMISCS   ASSUME NO MISC DEFINITIONS                   
         XC    TEAMISCS,TEAMISCS                                                
         OC    PROGPROF,PROGPROF   ANY?                                         
         BZ    GOSYS30             NOPE                                         
*                                                                               
         BAS   RE,SETMISC          SET TEAMISCS                                 
*                                                                               
GOSYS30  MVC   TEPROCLI,SVCLI                                                   
*                                                                               
         OC    TEMEDIA,TEMEDIA     IS MEDIA STILL SET                           
         BNZ   *+6                                                              
         DC    H'0'                CROAK IF NO MEDIA                            
*                                                                               
         XC    TEOSCRN,TEOSCRN                                                  
         CLI   TEMEDIA,C'T'                                                     
         BNE   *+10                                                             
         MVC   TEOSCRN(DEFSCTVQ),DEFSCTV                                        
         CLI   TEMEDIA,C'R'                                                     
         BNE   *+10                                                             
         MVC   TEOSCRN(DEFSCRAQ),DEFSCRA                                        
*                                                                               
         MVI   TEECELCD,TECELQ     ESTIMATE CAST DETAILS (X'EA')                
         MVI   TEETELCD,TETELQ     ESTIMATE TOTAL ELEMENT (X'EB')               
         MVI   TEEOELCD,TEOELQ     ESTIMATE OPTIONS ELEMENT (X'EC')             
         MVI   TEXCELCD,TEXELQ     ESTIMATE COMMENT ELEMENT (X'8C)              
         MVI   TEWCELCD,TEWELQ     ESTIMATE WORKCODE ELEMENT (X'B4')            
*                                                                               
         CLI   TEMEDIA,C'T'                                                     
         BNE   GOSYS40                                                          
         MVI   TEPFON,PFON         PFKEY TO ADD ON-CAMERA LINE                  
         MVI   TEPFOFF,PFOFF                    OFF-CAMERA                      
         MVI   TEPFSING,PFSING                  SINGER                          
         MVI   TEPFEXT,PFEXT                    EXTRA                           
         MVI   TEPFMUS,PFMUS                    MUSIC                           
         MVI   TEPFMISC,PFMISC                  MISCELLANEOUS                   
         MVI   TEPFCMNT,PFCMNT                  COMMENT                         
         B     GOSYS45                                                          
*                                                                               
GOSYS40  MVI   TEPFRAD,PFRAD                    ANNOUNCER                       
         MVI   TEPFRSNG,PFRSNG                  RADIO SINGER                    
         MVI   TEPFMUS,PFRMUS                   MUSIC                           
         MVI   TEPFMISC,PFRMSC                  MISCELLANEOUS                   
         MVI   TEPFCMNT,PFRCMT                  COMMENT                         
*                                                                               
*        MVI   TEPFREP,PFREP       PFKEY TO GENERATE REPORT                     
*                                                                               
GOSYS45  L     R2,=A(PFKTTAB)      ASSUME TV PF KEYS                            
         A     R2,RELO                                                          
         SR    RF,RF                                                            
         LA    RF,PFKTBLN                                                       
         CLI   TEMEDIA,C'T'                                                     
         BE    GOSYS50                                                          
         L     R2,=A(PFKRTAB)      USE RADIO PF KEYS                            
         A     R2,RELO                                                          
         LA    RF,PFKRTBLN                                                      
*                                                                               
GOSYS50  LA    R1,SESPFS                                                        
         BAS   RE,GETCALL                                                       
         CLI   BYTE,X'32'          CALLED FROM JOB EST                          
         BNE   *+8                                                              
         LA    R1,SESPFNOE         LOOSE THE JOB ESTIMATE CALL                  
*                                                                               
         L     RE,TEAOPFKS                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R2)       LOAD PFKEY TABLES INTO W/S                   
         LA    RE,1(RF,RE)         BUMP PAST FIRST SET OF KEYS                  
*                                                                               
         MVC   0(MYPFLN,RE),0(R1)  ADD ADDITIONAL PFKEYS                        
*                                                                               
         GOTO1 TASYSES,TEBLOCK     OFF TO TASYSEST                              
*                                                                               
         CLI   MODE,RECDEL         DELETING                                     
         BE    GOSYS70                                                          
*                                                                               
         CLI   MODE,RECREST        RESTORING                                    
         BE    GOSYS70                                                          
*                                                                               
         CLI   MODE,VALREC         CHANGEING                                    
         BNE   GOSYSX                                                           
*                                                                               
         CLI   ACTNUM,ACTNADD      AM I ADDING?                                 
         BE    GOSYS80             ADD FROM AIO1                                
*                                                                               
GOSYS70  BAS   RE,SPLIT            SPLIT SESSION REC INTO 1K ACCRECS            
         B     GOSYSX                                                           
*                                                                               
GOSYS80  BAS   RE,BLDSESKY         NO  KEY                                      
         MVC   AIO,AIO1            RECORD IN AIO1                               
         BAS   RE,VREC                                                          
         BAS   RE,DREC                                                          
         BAS   RE,PUTTRAIL         ADD A TRAILER EL                             
*                                                                               
*                                                                               
*                                                                               
GOSYSX   MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              BUILD MISCTAB FROM VALUES IN PROGPROF                  *         
***********************************************************************         
*                                                                               
SETMISC  NTR1                                                                   
         XR    R3,R3               OFFSET INTO PROGPROF                         
         XR    R2,R2                                                            
         XR    RF,RF               COUNTER                                      
         LA    R4,MISCTAB          O/P TABLE                                    
         LA    R5,MISCMAX                                                       
*                                                                               
SETM10   LA    RE,PROGPROF(R3)                                                  
         ICM   R2,1,0(RE)                                                       
         BZ    SETM20                                                           
         BAS   RE,MSCRESET         SET A(TABLE VALUES)                          
         CR    R2,R0               HIGHER THAN MAX                              
         BNH   SETM13                                                           
*                                                                               
         LA    R1,=CL16'ERROR IN CTL/PRO'                                       
         A     R1,RELO                                                          
         B     SETM15                                                           
*                                                                               
SETM13   BCTR  R2,0                GET VALUE FROM TABLE                         
         MH    R2,=Y(MISCLN)                                                    
         AR    R1,R2                                                            
SETM15   MVC   0(MISCLN,R4),0(R1)                                               
         LA    RF,1(RF)                                                         
*                                                                               
SETM20   LA    R3,1(R3)            NEXT PROFILE                                 
         LA    R4,MISCLN(R4)       NEXT AREA IN O/P TAB                         
         BCT   R5,SETM10                                                        
*                                                                               
         STC   RF,TENMISCS                                                      
         LA    R3,MISCTAB                                                       
         ST    R3,TEAMISCS                                                      
         B     XIT                                                              
*                                                                               
MSCRESET LA    R0,TVMISNM                                                       
         L     R1,=A(TVMISCS)                                                   
         A     R1,RELO                                                          
         USING TEBLOCK,R6                                                       
         L     R6,AMYTEBLK                                                      
         CLI   TEMEDIA,C'T'                                                     
         BER   RE                                                               
         LA    R0,RADMISNM                                                      
         L     R1,=A(RADMISCS)                                                  
         A     R1,RELO                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD A SESSION RECORD FROM MULTIPLE ACCOUNT RECORDS   *         
***********************************************************************         
*                                                                               
GETSREC  NTR1                                                                   
*                                                                               
         USING MYSAVED,R5          R5 SET IN INITIAL                            
         L     R5,AMYSAVE                                                       
*                                                                               
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED IN CASE SET            
         CLI   MODE,RECREST        AM I TO RESTORE RECORDS                      
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       TURN ON READ-DELETED                         
*                                                                               
         CLI   MODE,XRECDEL        POST DELETE                                  
         BNE   *+8                 NOPE                                         
         OI    DMINBTS,X'08'       TURN ON READ-DELETED                         
*                                                                               
         MVC   KEY,SVSESKEY        LOOK FOR HEADER REC                          
         MVC   AIO,ASESREC                                                      
         GOTO1 HIGH                                                             
*                                                                               
         LA    R3,KEY                                                           
         USING SESRECD,R3                                                       
         CLC   SESKEY,KEYSAVE                                                   
         BNE   GETRNO                                                           
*                                                                               
         CLI   MODE,RECREST        AM I TO RESTORE RECORDS                      
         BNE   GETR3               NO                                           
         MVI   ERROR,RECNTDEL      MAKE SURE THEY ARE DELETED                   
         TM    ACCOSTAT(R3),X'80'                                               
         BZ    ERREND                                                           
         BAS   RE,CHKTRAIL         LOOK FOR TRAILER EL                          
         BE    GETRYES                                                          
*                                                                               
GETR3    MVC   AIO,AIO1            READ INTO AIO1                               
*                                                                               
         GOTO1 SEQ                 LOOK FOR ANOTHER RECORD                      
         LA    R3,KEY                                                           
         CLC   SESKEY(SESKSEQ-SESKEY),KEYSAVE                                   
         BNE   GETRYES             DONE                                         
*                                                                               
         L     R3,AIO                                                           
         AH    R3,DATADISP                                                      
GETR4    CLI   0(R3),0             EOR                                          
         BE    GETR3               TRY NEXT RECORD                              
*                                                                               
         XC    ELEMENT,ELEMENT     EXTRACT ELEMENTS                             
         ZIC   R4,1(R3)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT,0(R3)                                                    
*                                                                               
         MVC   AIO,ASESREC                                                      
         BAS   RE,MYADDL           ADD THE EL TO THE BIG RECORD                 
         BAS   RE,CHKTRAIL         LOOK FOR TRAILER EL                          
         BE    GETRYES             GOT IT                                       
         LA    R3,1(R4,R3)         GET NEXT ELEMENT                             
         B     GETR4                                                            
*                                                                               
*                                                                               
GETRYES  MVC   KEY,SVSESKEY        RESTORE ORIGINAL KEY                         
*                                                                               
         MVC   AIO,ASESREC         DISPLAY HEADER FIELDS                        
         CLI   MODE,DISPREC                                                     
         BNE   *+8                                                              
         BAS   RE,DREC                                                          
         MVC   AIO,AIO1            RESTORE AIO                                  
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED IN CASE SET            
         B     YESXIT              ASESREC HAS A(RECORD)                        
*                                                                               
GETRNO   MVC   KEY,SVSESKEY        RESTORE ORIGINAL KEY                         
         MVC   AIO,AIO1            RESTORE AIO                                  
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED IN CASE SET            
         B     NOXIT               ASESREC IS BLANK                             
         EJECT                                                                  
***********************************************************************         
*              ADD ELEMENTS FOR 'BIG' RECORDS                         *         
***********************************************************************         
*                                                                               
MYADDL   NTR1                                                                   
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT,0                           
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         CLI   12(R1),5            TEST ERROR IS RECORD TOO LONG                
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
*        TASYSEST WILL MAKE SURE ITS NOT TOO TOO LONG                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              SPLIT 'BIG' RECORDS INTO SMALLER ONES                  *         
***********************************************************************         
*                                                                               
SPLIT    NTR1                                                                   
         L     R4,ASESREC          R4=A(BIG RECORD)                             
         L     R3,AIO1                                                          
         ST    R3,AIO              R3=A(RECORD TO BE WRITTEN BACK)              
*                                                                               
         L     RE,AIO              CLEAR AREA FOR ADDELEM                       
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         USING SESRECD,R3                                                       
         MVC   SESKEY,0(R4)        SET KEY IN RECORD                            
*                                                                               
         AH    R4,DATADISP         R4=A(FIRST ELEMENT)                          
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMENTS             
*                                                                               
SPL2     MVC   ACCORLEN(2,R3),DATADISP   (RE)INIT RECORD LENGTH                 
         XC    ACCOSTAT(10,R3),ACCOSTAT(R3)     CLEAR RECORD START              
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   SPL4                                                             
         OC    SESKSEQ,SESKSEQ     IS THIS THE HEADER REC                       
         BNZ   SPL4                                                             
         BAS   RE,VREC                                                          
         BAS   RE,DREC                                                          
*                                                                               
SPL4     ZIC   R1,1(R4)            R1 = L'ELEMENT                               
         AH    R1,ACCORLEN(R3)        + L'RECORD                                
         CH    R1,=H'930'         WILL RECORD BECOME TOO LONG                   
         BH    SPL6                YES - GO RELEASE IT                          
*                                                                               
         CLI   0(R4),GDAELQ        TRAILER ELEMENT                              
         BE    SPL5                YES, WITH PUTTRAIL                           
         CLI   MODE,VALREC                                                      
         BNE   SPL4A                                                            
*                                                                               
         CLI   0(R4),NAMELQ        NAME EL                                      
         BE    SPL5                YES, ADDED IN VREC                           
         CLI   0(R4),FFTELQ        FILTER                                       
         BE    SPL5                                                             
         CLI   0(R4),PACELQ        PERSON/ACTIVITY                              
         BE    SPL5                                                             
*                                                                               
SPL4A    MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC.                          
*                                                                               
SPL5     ZIC   R1,1(R4)            GET NEXT ELEMENT                             
         LA    R4,0(R1,R4)                                                      
         CLI   0(R4),0                                                          
         BNE   SPL4                                                             
*                                                                               
         BAS   RE,PUTTRAIL                                                      
*                                                                               
SPL6     CLC   ACCORLEN(2,R3),DATADISP   ANYTHING IN THIS RECORD?               
         BE    *+8                       NO                                     
         BAS   RE,RELEASE                YES, WRITE RECORD                      
*                                                                               
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
*                                                                               
         BAS   RE,DELETE           DELETE ANY REMAINING RECORDS                 
*                                                                               
         MVC   KEY,SVSESKEY        RESTORE KEY                                  
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLI   ACTNUM,ACTNDEL      DELETE CALL                                  
         BE    SPLX                YES                                          
*                                                                               
         GOTO1 READ                RESTORE HEADER REC INTO AIO                  
*                                                                               
         CLI   MODE,RECREST        IS THIS A RESTORED RECORD?                   
         BNE   *+8                 NO, ITS DISPLAYED                            
         BAS   RE,DREC             DISPLAY NOW                                  
*                                                                               
SPLX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              RELEASE SPLIT-UP ESTIMATE RECORDS                      *         
***********************************************************************         
*                                                                               
         USING SESRECD,R3          R3=A(RECORD TO BE WRITTEN BACK)              
RELEASE  NTR1                                                                   
         LA    R3,KEY                                                           
         USING SESRECD,R3                                                       
*                                                                               
         MVC   AIO,AIO2            SEE IF RECORD EXISTS                         
*                                                                               
         MVC   KEY,SESKEY          MOVE KEY FROM RECORD TO KEY                  
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO TO THE RECORD                    
*                                                                               
         L     R3,AIO                                                           
         CLI   MODE,RECDEL         AM I TO DELETE THIS                          
         BNE   *+12                                                             
         OI    ACCOSTAT(R3),X'80'  MARK KEY                                     
         B     REL9                AND WRITE                                    
*                                                                               
         CLI   MODE,RECREST        AM I TO RESTORE THIS?                        
         BNE   *+12                                                             
         NI    ACCOSTAT(R3),X'FF'-X'80'  UN-MARK KEY                            
         B     REL9                AND WRITE                                    
*                                                                               
REL5     LA    R3,KEY                                                           
         CLC   SESKEY,KEYSAVE      IS RECORD ALREADY ON FILE                    
         BE    REL8                WRITE                                        
*                                                                               
         MVC   SESKEY,KEYSAVE      NO, SO RESTORE SAVED KEY                     
         GOTO1 ADD                 AND ADD NEW RECORD TO FILE                   
         B     REL10                                                            
*                                                                               
REL8     L     R3,AIO              RECORD MARKED DELETED?                       
         TM    ACCOSTAT(R3),X'80'                                               
         BZ    *+8                                                              
         XI    ACCOSTAT(R3),X'80'  UNMARK IT                                    
REL9     GOTO1 WRITE               AND WRITE IT BACK                            
*                                                                               
*                                                                               
REL10    NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
*                                                                               
         USING SESRECD,R3                                                       
         L     R3,AIO              A(RECORD JUST WRITTEN/ADDED)                 
         ZIC   R1,SESKSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC.          
         LA    R1,1(R1)                                                         
         STC   R1,SESKSEQ                                                       
         LA    R3,KEY              BUMP KEY ALSO                                
         ZIC   R1,SESKSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SESKSEQ                                                       
RELX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DELETE SUBSEQUENT SESSION RECORDS                      *         
***********************************************************************         
*                                                                               
DELETE   NTR1                                                                   
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
DEL2     GOTO1 HIGH                READ RECORD AFTER LAST ONE WRITTEN           
*                                                                               
         USING SESRECD,R3                                                       
         LA    R3,KEY                                                           
         CLC   SESKEY(SESKSEQ-SESRECD),KEYSAVE  TEST STILL SAME SES             
         BNE   XIT                                                              
*                                                                               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 READ                                                             
         L     R3,AIO                                                           
         OI    ACCOSTAT(R3),X'80'  MARK KEY                                     
         GOTO1 WRITE               AND WRITE IT BACK                            
*                                                                               
         LA    R3,KEY              BUMP KEY                                     
         ZIC   R1,SESKSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SESKSEQ                                                       
         B     DEL2                LOOK FOR MORE                                
         EJECT                                                                  
***********************************************************************         
*              LOOK FOR TRAILER ELEMENT                               *         
***********************************************************************         
*                                                                               
CHKTRAIL EQU   *                                                                
         MVI   ELCODE,GDAELQ                                                    
         L     R6,AIO                                                           
         B     GETEL               HAVE GETEL RETURN TO CALLER                  
*                                                                               
*                                  PUT A TRAILER EL IN AIO                      
PUTTRAIL NTR1                                                                   
         BAS   RE,CHKTRAIL                                                      
         BE    PUTTX                                                            
         XC    ELEMENT,ELEMENT                                                  
         USING GDAELD,R6                                                        
         LA    R6,ELEMENT                                                       
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVC   GDADATE,TODAYP                                                   
         GOTO1 ADDELEM                                                          
PUTTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PRINT HOOK                                             *         
***********************************************************************         
*                                                                               
HOOK     NTR1  ,                                                                
         MVC   H4+10(L'TESCLI),TESCLI                                           
         MVC   H4+18(36),BLOCK     CLIENT NAME                                  
         MVC   H5+10(L'TESPRO),TESPRO                                           
         MVC   H5+18(36),BLOCK+36  PRODUCT NAME                                 
         MVC   H6+10(L'TESJOB),TESJOB                                           
         MVC   H6+18(36),BLOCK+72  JOB NAME                                     
         MVC   H8+10(L'TESEST),TESEST                                           
         MVC   H9+10(L'TESMED),TESMED                                           
         MVC   H9+18(5),=C'RADIO'                                               
         CLI   TESMED,C'R'                                                      
         BE    *+10                                                             
         MVC   H9+18(10),=C'TELEVISION'                                         
         MVC   H11+18(36),TESDES                                                
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              SAVE THE WORKCODE DATA BEFORE IT IS CHANGED            *         
***********************************************************************         
*                                                                               
SAVEWRK  NTR1                                                                   
         XC    PREVWORK,PREVWORK                                                
         MVC   KEY,SVSESKEY                                                     
         MVC   AIO,AIO1                                                         
         GOTO1 READ                                                             
         BNE   SAVEX                                                            
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,TEWELQ       GET WORKCODE SUMMARY ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   SAVEX               NOTHING TO SAVE                              
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PREVWORK(0),0(R6)                                                
*                                                                               
SAVEX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ERRORS, EXITS, ETC.                                    *         
***********************************************************************         
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
INFEXIT  ST    R2,ACURFORC         PRINT INFORMATIONAL MESSAGE                  
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 INFOXIT                                                          
*                                                                               
YESXIT   CR    RB,RB               SET CONDITION CODE                           
         B     XIT                                                              
NOXIT    LTR   RB,RB                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*        CONSTANTS, TABLES AND THE LIKE                                         
*                                                                               
TEMPSTR  DC    C'TEMPSTR'                                                       
DMWRT    DC    C'DMWRT'                                                         
DMREAD   DC    C'DMREAD'                                                        
*                                                                               
MAXLEN   EQU   3800                MAX SESSION RECORD LENGTH                    
PFTBLN   EQU   PFKTBLN+MYPFLN     L'(THE BIGGEST LIST OF PFKEYS)                
BUFLEN   EQU   (MAXLEN+PFTBLN)                                                  
*                                                                               
DEFSCTV  DS    0H                                                               
         DC    AL1(TAECTYPO,3),AL2(0),AL1(0)  3 ON-CAMERA                       
         DC    AL1(TAECTYPF,2),AL2(0),AL1(0)  2 OFF-CAMERA                      
         DC    AL1(TAECTYPS,2),AL2(0),AL1(0)  2 SINGERS                         
         DC    AL1(TAECTYPX,1),AL2(0),AL1(0)  2 EXTRA                           
         DC    AL1(0)                                                           
DEFSCTVQ EQU   *-DEFSCTV                                                        
         SPACE 3                                                                
*              DEFAULT RADIO SCREEN CONFIGURATION TABLE                         
*                                                                               
DEFSCRA  DS    0H                                                               
         DC    AL1(TAECTYPR,3),AL2(0),AL1(2)  4 RADIO W/ 2 COMMENTS             
         DC    AL1(TAECTYPT,3),AL2(0),AL1(0)  4 RADIO SINGERS                   
         DC    AL1(TAECTYPU,1),AL2(0),AL1(0)  2 MUSIC                           
         DC    AL1(0)                                                           
DEFSCRAQ EQU   *-DEFSCRA                                                        
         EJECT                                                                  
*                                                                               
*                                                                               
*        ADDITIONAL PFKEYS                                                      
*        APPEND THESE TO THE END OF THE PF KEYS YOU ARE SENDING                 
*        TALENT ESTIMATING                                                      
*                                                                               
SESPFS   DS    0C                                                               
         DC    AL1(254,L'PFKRREP+PFKLNQ,PFREP)                                  
PFKRREP  DC    C'Print'                                                         
         DC    AL1(254,L'PFUPDATE+PFKLNQ,PFUPD)                                 
PFUPDATE DC    C'Updt'                                                          
         DC    AL1(254,L'PFJBEST+PFKLNQ,PFJEST)                                 
PFJBEST  DC    C'JobEst'                                                        
         DC    AL1(254,L'PFRET+PFKLNQ,PFRETRN)                                  
PFRET    DC    C'Ret'                                                           
         DC    X'00'                                                            
MYPFLN   EQU   *-SESPFS                                                         
*                                                                               
SESPFNOE DS    0C                                                               
         DC    AL1(254,L'PFKNREP+PFKLNQ,PFREP)                                  
PFKNREP  DC    C'Print'                                                         
         DC    AL1(254,L'PFUPNOE+PFKLNQ,PFUPD)                                  
PFUPNOE  DC    C'Upd'                                                           
         DC    AL1(254,L'PFRNOE+PFKLNQ,PFRETRN)                                 
PFRNOE   DC    C'Ret'                                                           
         DC    X'00'                                                            
*                                                                               
PFON     EQU   2                   PFKEY TO ADD ON-CAMERA LINE                  
PFOFF    EQU   3                                OFF-CAMERA                      
PFSING   EQU   4                                SINGER                          
PFEXT    EQU   5                                EXTRA                           
PFMUS    EQU   6                                MUSIC                           
PFMISC   EQU   7                                MISCELLANEOUS                   
PFCMNT   EQU   8                                COMMENT                         
*                                                                               
PFRAD    EQU   2                                                                
PFRSNG   EQU   3                                                                
PFRMUS   EQU   4                                                                
PFRMSC   EQU   5                                                                
PFRCMT   EQU   6                                                                
*                                                                               
PFREP    EQU   9                   PFKEY TO GENERATE REPORT                     
PFUPD    EQU   10                  PFKEY TO UPDATE EST RECORD                   
PFJEST   EQU   11                  PFKEY TO UPDATE EST RECORD                   
PFRETRN  EQU   12                  RETURN TO SENDER                             
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              LANDSCAPE FORMAT HEADINGS                              *         
***********************************************************************         
*                                                                               
PRTHEAD  DS    0CL133                                                           
         DC    AL1(TAECTYPO)                                                    
*                                                                               
HDON     DS    0CL132                                                           
         DC    C'On-Camera '                                                    
         DC    C'  '                                                            
         DC    C'Over %'                                                        
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Spots'                                                         
         DC    C'  '                                                            
         DC    C'Days'                                                          
         DC    C'  '                                                            
         DC    C'Overtime'                                                      
         DC    C'  '                                                            
         DC    C'Doubletime'                                                    
         DC    C'  '                                                            
         DC    C'Tags'                                                          
         DC    C'  '                                                            
         DC    C'Travel'                                                        
         DC    C'  '                                                            
         DC    C'Prior Day Wardrobe'                                            
         DC    C'  '                                                            
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   HDON+L'HDON                                                      
*                                                                               
         DC    AL1(TAECTYPF)                                                    
HDOFF    DS    0CL132                                                           
         DC    C'Off-Camera'                                                    
         DC    C'  '                                                            
         DC    C'Over %'                                                        
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Spots'                                                         
         DC    C'  '                                                            
         DC    C'Days'                                                          
         DC    C'  '                                                            
         DC    C'        '                                                      
         DC    C'  '                                                            
         DC    C'          '                                                    
         DC    C'  '                                                            
         DC    C'Tags'                                                          
         DC    C'  '                                                            
         DC    C'Travel'                                                        
         DC    C'  '                                                            
         DC    C'                  '                                            
         DC    C'  '                                                            
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   HDOFF+L'HDOFF                                                    
*                                                                               
         DC    AL1(TAECTYPS)                                                    
HDSING   DS    0CL132                                                           
         DC    C'Singers   '                                                    
         DC    C'  '                                                            
         DC    C'Over %'                                                        
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Spots'                                                         
         DC    C'  '                                                            
         DC    C'Days'                                                          
         DC    C'  '                                                            
         DC    C'Category'                                                      
         DC    C'  '                                                            
         DC    C'On Camera?'                                                    
         DC    C'  '                                                            
         DC    C'         '                                                     
         DC    C'  '                                                            
         DC    C'      '                                                        
         DC    C'                 '                                             
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   HDSING+L'HDSING                                                  
*                                                                               
         DC    AL1(TAECTYPX)                                                    
HDEXT    DS    0CL132                                                           
         DC    C'Extras    '                                                    
         DC    C'  '                                                            
         DC    C'Over %'                                                        
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Spots'                                                         
         DC    C'  '                                                            
         DC    C'Days'                                                          
         DC    C'  '                                                            
         DC    C'Overtime'                                                      
         DC    C'  '                                                            
         DC    C'Double Time'                                                   
         DC    C'  '                                                            
         DC    C'Category'                                                      
         DC    C'  '                                                            
         DC    C'Non Union'                                                     
         DC    C'              '                                                
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   HDEXT+L'HDEXT                                                    
*                                                                               
         DC    AL1(TAECTYPU)                                                    
HDMUS    DS    0CL132                                                           
         DC    C'Musicians '                                                    
         DC    C'  '                                                            
         DC    C'Over %'                                                        
         DC    C'  '                                                            
         DC    C'Spots'                                                         
         DC    C'  '                                                            
         DC    C'Hours/Minutes'                                                 
         DC    C'  '                                                            
         DC    C'Category'                                                      
         DC    C'  '                                                            
         DC    C'Doubles'                                                       
         DC    C' '                                                             
         DC    C'Cartage'                                                       
         DC    C'                            '                                  
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   HDMUS+L'HDMUS                                                    
*                                                                               
         DC    AL1(TAECTYPR)                                                    
HDRAD    DS    0CL132                                                           
         DC    C'Announcers'                                                    
         DC    C'  '                                                            
         DC    C'Over %'                                                        
         DC    C'  '                                                            
         DC    C'Spots'                                                         
         DC    C'  '                                                            
         DC    C'Hours/Minutes'                                                 
         DC    C'  '                                                            
         DC    C'Tags'                                                          
         DC    C'  '                                                            
         DC    C'Demo  '                                                        
         DC    C'  '                                                            
         DC    C'          '                                                    
         DC    C'                             '                                 
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   HDRAD+L'HDRAD                                                    
*                                                                               
         DC    AL1(TAECTYPT)                                                    
HDRSI    DS    0CL132              RADIO SINGERS                                
         DC    C'Singers   '                                                    
         DC    C'  '                                                            
         DC    C'Over %'                                                        
         DC    C'  '                                                            
         DC    C'Spots'                                                         
         DC    C'  '                                                            
         DC    C'Hours/Minutes'                                                 
         DC    C'  '                                                            
         DC    C'Tags'                                                          
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Category'                                                      
         DC    C'                                 '                             
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   HDRSI+L'HDRSI                                                    
*                                                                               
         DC    AL1(TAECTYPM)                                                    
HDMIS    DS    0CL132                                                           
         DC    C'Description     '                                              
         DC    C' '                                                             
         DC    C'Net Amount'                                                    
         DC    C'          '                                                    
         DC    C'Pension && Health    '                                         
         DC    C'Health && Welfare    '                                         
         DC    C'Tax && Handling      '                                         
         DC    CL20'Comment'                                                    
         ORG   HDMIS+L'HDMIS                                                    
*                                                                               
         DC    AL1(FINLHEAD)                                                    
FINLHEAD EQU   X'FE'                                                            
HDFIN    DS    0CL132                                                           
         DC    C'Totals:'                                                       
         DC    C'   '                                                           
         DC    C'                 Net'                                          
         DC    C'    Pension && Health'                                         
         DC    C'    Health && Welfare'                                         
         DC    C'      Tax && Handling'                                         
         DC    C'               Total'                                          
         ORG   HDFIN+L'HDFIN                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              LANDSCAPE PRINT TABLE                                  *         
***********************************************************************         
*                                                                               
PRTTAB   DS    0C                                                               
ONSTRT   DC    AL1(TAECTYPO,ONEND-ONSTRT,NONFLDS)                               
ONFLDS   DC    AL1(TAECNUM-TAECD,L'TAECNUM,PRNUM-PRTD,L'PRNUM,EDNUM)            
*                                                                               
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,PROVER-PRTD,L'PROVER,EDAMNT)         
*                                                                               
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PRDEMO-PRTD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,PRSPOT-PRTD,L'PRSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PRDAYS-PRTD,L'PRDAYS,EDNUM)          
         DC    AL1(TAECOT-TAECD,L'TAECOT,PROT-PRTD,L'PROT,EDNUM)                
         DC    AL1(TAECDT-TAECD,L'TAECDT,PRDT-PRTD,L'PRDT,EDNUM)                
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,PRTAG-PRTD,L'PRTAG,EDNUM)            
         DC    AL1(TAECTRV-TAECD,L'TAECTRV,PRTRAV-PRTD,L'PRTRAV,EDAMNT)         
         DC    AL1(TAECPDW-TAECD,L'TAECPDW,PRPDW-PRTD,L'PRPDW,EDAMNT)           
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)            
ONEND    EQU   *                                                                
NONFLDS  EQU   (*-ONFLDS)/PRTTLN                                                
*                                                                               
OFFSTRT  DC    AL1(TAECTYPF,OFFEND-OFFSTRT,NOFFFLDS)                            
OFFFLDS  DC    AL1(TAECNUM-TAECD,L'TAECNUM,PRNUM-PRTD,L'PRNUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,PROVER-PRTD,L'PROVER,EDAMNT)         
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PRDEMO-PRTD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,PRSPOT-PRTD,L'PRSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PRDAYS-PRTD,L'PRDAYS,EDNUM)          
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,PRTAG-PRTD,L'PRTAG,EDNUM)            
         DC    AL1(TAECTRV-TAECD,L'TAECTRV,PRTRAV-PRTD,L'PRTRAV,EDAMNT)         
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)            
OFFEND   EQU   *                                                                
NOFFFLDS EQU   (*-OFFFLDS)/PRTTLN                                               
*                                                                               
SINSTRT  DC    AL1(TAECTYPS,SINEND-SINSTRT,NSINFLDS)                            
SINFLDS  DC    AL1(TAECNUM-TAECD,L'TAECNUM,PRNUM-PRTD,L'PRNUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,PROVER-PRTD,L'PROVER,EDAMNT)         
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PRDEMO-PRTD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,PRSPOT-PRTD,L'PRSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PRDAYS-PRTD,L'PRDAYS,EDNUM)          
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,PRCAT-PRTD,L'PRCAT,EDCHAR)           
         DC    AL1(TAECSTAT-TAECD,TAECSONC,PRONC-PRTD,Y,EDSTAT)                 
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)            
SINEND   EQU   *                                                                
NSINFLDS EQU   (*-SINFLDS)/PRTTLN                                               
*                                                                               
EXTSTRT  DC    AL1(TAECTYPX,EXTEND-EXTSTRT,NEXTFLDS)                            
EXTFLDS  DC    AL1(TAECNUM-TAECD,L'TAECNUM,PRNUM-PRTD,L'PRNUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,PROVER-PRTD,L'PROVER,EDAMNT)         
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PRDEMO-PRTD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,PRSPOT-PRTD,L'PRSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PRDAYS-PRTD,L'PRDAYS,EDNUM)          
         DC    AL1(TAECOT-TAECD,L'TAECOT,PREOT-PRTD,L'PREOT,EDNUM)              
         DC    AL1(TAECDT-TAECD,L'TAECDT,PREDT-PRTD,L'PREDT,EDNUM)              
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,PRECAT-PRTD,L'PRCAT,EDCHAR)          
         DC    AL1(TAECSTAT-TAECD,TAECSNON,PRENONU-PRTD,Y,EDSTAT)               
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)            
EXTEND   EQU   *                                                                
NEXTFLDS EQU   (*-EXTFLDS)/PRTTLN                                               
*                                                                               
MUSSTRT  DC    AL1(TAECTYPU,MUSEND-MUSSTRT,NMUSFLDS)                            
MUSFLDS  DC    AL1(TAECNUM-TAECD,L'TAECNUM,PRNUM-PRTD,L'PRNUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,PROVER-PRTD,L'PROVER,EDAMNT)         
         DC    AL1(TAECSP-TAECD,L'TAECSP,PRMSPOT-PRTD,L'PRMSPOT,EDNUM)          
         DC    AL1(TAECHRM-TAECD,L'TAECHRM,PRMHRM-PRTD,L'PRMHRM,EDAMNT)         
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,PRMCAT-PRTD,L'PRMCAT,EDCHAR)         
         DC    AL1(TAECDBL-TAECD,L'TAECDBL,PRMDUB-PRTD,L'PRMDUB,EDCHAR)         
         DC    AL1(TAECCART-TAECD,L'TAECCART,PRMCART-PRTD,L'PRMCART)            
         DC    AL1(EDAMNT)                                                      
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)            
MUSEND   EQU   *                                                                
NMUSFLDS EQU   (*-MUSFLDS)/PRTTLN                                               
*                                                                               
MISSTRT  DC    AL1(TAECTYPM,MISEND-MISSTRT,NMISFLDS)                            
MISFLDS  DC    AL1(TAECDESC-TAECD,L'TAECDESC,PRMDESC-PRTD,L'PRMDESC)            
         DC    AL1(EDCHAR)                                                      
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRMNET-PRTD,L'PRMNET,EDDOL)          
         DC    AL1(TAECPNH-TAECD,L'TAECPNH,PRMPH-PRTD,L'PRMPH,EDDOL)            
         DC    AL1(TAECHNW-TAECD,L'TAECHNW,PRMHW-PRTD,L'PRMHW,EDDOL)            
         DC    AL1(TAECTAX-TAECD,L'TAECTAX,PRMTH-PRTD,L'PRMTH,EDDOL)            
         DC    AL1(TAECHND-TAECD,L'TAECHND,PRMTH-PRTD,L'PRMTH,EDDOL)            
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
MISEND   EQU   *                                                                
NMISFLDS EQU   (*-MISFLDS)/PRTTLN                                               
*                                                                               
RADSTRT  DC    AL1(TAECTYPR,RADEND-RADSTRT,NRADFLDS)                            
RADFLDS  DC    AL1(TAECNUM-TAECD,L'TAECNUM,PRNUM-PRTD,L'PRNUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,PROVER-PRTD,L'PROVER,EDAMNT)         
         DC    AL1(TAECSP-TAECD,L'TAECSP,PRMSPOT-PRTD,L'PRMSPOT,EDNUM)          
         DC    AL1(TAECHRM-TAECD,L'TAECHRM,PRMHRM-PRTD,L'PRMHRM,EDAMNT)         
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,PRRTG-PRTD,L'PRRTG,EDNUM)            
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PRRDEM-PRTD,Y,EDSTAT)                
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)            
RADEND   EQU   *                                                                
NRADFLDS EQU   (*-RADFLDS)/PRTTLN                                               
*                                                                               
RSISTRT  DC    AL1(TAECTYPT,RSIEND-RSISTRT,NRSIFLDS)                            
RSIFLDS  DC    AL1(TAECNUM-TAECD,L'TAECNUM,PRNUM-PRTD,L'PRNUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,PROVER-PRTD,L'PROVER,EDAMNT)         
         DC    AL1(TAECSP-TAECD,L'TAECSP,PRMSPOT-PRTD,L'PRMSPOT,EDNUM)          
         DC    AL1(TAECHRM-TAECD,L'TAECHRM,PRMHRM-PRTD,L'PRMHRM,EDAMNT)         
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,PRRTG-PRTD,L'PRRTG,EDNUM)            
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PRRDEM-PRTD,Y,EDSTAT)                
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,PRRCAT-PRTD,L'PRRCAT,EDCHAR)         
         DC    AL1(TAECCMNT-TAECD,0,PRCOM-PRTD,L'PRCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PRNET-PRTD,L'PRNET,EDDOL)            
RSIEND   EQU   *                                                                
NRSIFLDS EQU   (*-RSIFLDS)/PRTTLN                                               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              PORTRAIT FORMAT HEADINGS                               *         
***********************************************************************         
*                                                                               
PORHEAD  DS    0CL133                                                           
         DC    AL1(TAECTYPO)                                                    
*                                                                               
PORON    DS    0CL132                                                           
*                                                                               
         DC    C'On-Cam '                                                       
         DC    C'  '                                                            
         DC    C'Ov% '                                                          
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Sp'                                                            
         DC    C'  '                                                            
         DC    C'Dy'                                                            
         DC    C'  '                                                            
         DC    C'OT'                                                            
         DC    C'  '                                                            
         DC    C'DT'                                                            
         DC    C'  '                                                            
         DC    C'Tag'                                                           
         DC    C'  '                                                            
         DC    C'Travl'                                                         
         DC    C'  '                                                            
         DC    C'PD-WD'                                                         
         DC    C'  '                                                            
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   PORON+L'PORON                                                    
*                                                                               
         DC    AL1(TAECTYPF)                                                    
POROFF   DS    0CL132                                                           
         DC    C'Off-Cam'                                                       
         DC    C'  '                                                            
         DC    C'Ov% '                                                          
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Sp'                                                            
         DC    C'  '                                                            
         DC    C'Dy'                                                            
         DC    C'  '                                                            
         DC    C'  '                                                            
         DC    C'  '                                                            
         DC    C'  '                                                            
         DC    C'  '                                                            
         DC    C'Tag'                                                           
         DC    C'  '                                                            
         DC    C'Travl'                                                         
         DC    C'  '                                                            
         DC    C'     '                                                         
         DC    C'  '                                                            
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   POROFF+L'POROFF                                                  
*                                                                               
         DC    AL1(TAECTYPS)                                                    
PORSING  DS    0CL132                                                           
         DC    C'Singers'                                                       
         DC    C'  '                                                            
         DC    C'Ov% '                                                          
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Sp'                                                            
         DC    C'  '                                                            
         DC    C'Dy'                                                            
         DC    C'  '                                                            
         DC    C'Cat  '                                                         
         DC    C'  '                                                            
         DC    C'On Camera?'                                                    
         DC    C'  '                                                            
         DC    C'  '                                                            
         DC    C'   '                                                           
         DC    C'   '                                                           
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   PORSING+L'PORSING                                                
*                                                                               
         DC    AL1(TAECTYPX)                                                    
POREXT   DS    0CL132                                                           
         DC    C'Extras '                                                       
         DC    C'  '                                                            
         DC    C'Ov% '                                                          
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Sp'                                                            
         DC    C'  '                                                            
         DC    C'Dy'                                                            
         DC    C'  '                                                            
         DC    C'OT'                                                            
         DC    C'  '                                                            
         DC    C'DT'                                                            
         DC    C'  '                                                            
         DC    C'Cat  '                                                         
         DC    C'  '                                                            
         DC    C'NonUnion'                                                      
         DC    C'    '                                                          
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   POREXT+L'POREXT                                                  
*                                                                               
         DC    AL1(TAECTYPU)                                                    
PORMUS   DS    0CL132                                                           
         DC    C'Music  '                                                       
         DC    C'  '                                                            
         DC    C'Ov% '                                                          
         DC    C'  '                                                            
         DC    C'Sp'                                                            
         DC    C'  '                                                            
         DC    C'Hours/Min''s'                                                  
         DC    C'  '                                                            
         DC    C'Cat'                                                           
         DC    C'  '                                                            
         DC    C'Doubles'                                                       
         DC    C' '                                                             
         DC    C'Cartage'                                                       
         DC    C'    '                                                          
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   PORMUS+L'PORMUS                                                  
*                                                                               
         DC    AL1(TAECTYPR)                                                    
PORRAD   DS    0CL132                                                           
         DC    C'Anncrs '                                                       
         DC    C'  '                                                            
         DC    C'Ov% '                                                          
         DC    C'  '                                                            
         DC    C'Sp'                                                            
         DC    C'  '                                                            
         DC    C'Hours/Min''s'                                                  
         DC    C'  '                                                            
         DC    C'Tags'                                                          
         DC    C'  '                                                            
         DC    C'Demo  '                                                        
         DC    C'  '                                                            
         DC    C'          '                                                    
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   PORRAD+L'PORRAD                                                  
*                                                                               
         DC    AL1(TAECTYPT)                                                    
PORRSI   DS    0CL132              RADIO SINGERS                                
         DC    C'Singers'                                                       
         DC    C'  '                                                            
         DC    C'Ov% '                                                          
         DC    C'  '                                                            
         DC    C'Sp'                                                            
         DC    C'  '                                                            
         DC    C'Hours/Min''s'                                                  
         DC    C'  '                                                            
         DC    C'Tags'                                                          
         DC    C'  '                                                            
         DC    C'Demo'                                                          
         DC    C'  '                                                            
         DC    C'Cat  '                                                         
         DC    C'  '                                                            
         DC    C'     '                                                         
         DC    CL20'Comment'                                                    
         DC    C'  '                                                            
         DC    C'Net Amount'                                                    
         ORG   PORRSI+L'PORRSI                                                  
*                                                                               
         DC    AL1(TAECTYPM)                                                    
PORMIS   DS    0CL132                                                           
         DC    C'Description     '                                              
         DC    C' '                                                             
         DC    C'  NET AMOUNT'                                                  
         DC    C' '                                                             
         DC    C'        P&&H '                                                 
         DC    C' '                                                             
         DC    C'        H&&W '                                                 
         DC    C' '                                                             
         DC    C'        T&&H '                                                 
         DC    C' '                                                             
         DC    CL20'Comment'                                                    
         ORG   PORMIS+L'PORMIS                                                  
*                                                                               
         DC    AL1(FINLHEAD)                                                    
PORFIN   DS    0CL132                                                           
         DC    C'Totals:   '                                                    
         DC    C'            Net '                                              
         DC    C'  Pension&&Health'                                             
         DC    C'  Health&&Welfare'                                             
         DC    C'    Tax&&Handling'                                             
         DC    C'           Total'                                              
         ORG   PORFIN+L'PORFIN                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              PORTRAIT PRINT TABLE                                   *         
*        NOTE: The following table must be a mirror image of          *         
*              prttab. The lengths within are as defined in prttab.   *         
***********************************************************************         
*                                                                               
PORTAB   DS    0C                                                               
*                                                                               
*        ON-CAMERA                                                              
         DC    AL1(TAECTYPO,ONEND-ONSTRT,NONFLDS)                               
         DC    AL1(TAECNUM-TAECD,L'TAECNUM,PONUM-PORD,L'PONUM,EDNUM)            
*                                                                               
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,POOVER-PORD,L'POOVER,EDAMNT)         
*                                                                               
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PODEMO-PORD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,POSPOT-PORD,L'POSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PODAYS-PORD,L'PODAYS,EDNUM)          
         DC    AL1(TAECOT-TAECD,L'TAECOT,POOT-PORD,L'POOT,EDNUM)                
         DC    AL1(TAECDT-TAECD,L'TAECDT,PODT-PORD,L'PODT,EDNUM)                
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,POTAG-PORD,L'POTAG,EDNUM)            
         DC    AL1(TAECTRV-TAECD,L'TAECTRV,POTRAV-PORD,L'POTRAV,EDAMNT)         
         DC    AL1(TAECPDW-TAECD,L'TAECPDW,POPDW-PORD,L'POPDW,EDAMNT)           
         DC    AL1(TAECCMNT-TAECD,0,POCOM-PORD,L'POCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PONET-PORD,L'PONET,EDDOL)            
*                                                                               
*        OFF CAMERA                                                             
*                                                                               
         DC    AL1(TAECTYPF,OFFEND-OFFSTRT,NOFFFLDS)                            
         DC    AL1(TAECNUM-TAECD,L'TAECNUM,PONUM-PORD,L'PONUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,POOVER-PORD,L'POOVER,EDAMNT)         
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PODEMO-PORD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,POSPOT-PORD,L'POSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PODAYS-PORD,L'PODAYS,EDNUM)          
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,POTAG-PORD,L'POTAG,EDNUM)            
         DC    AL1(TAECTRV-TAECD,L'TAECTRV,POTRAV-PORD,L'POTRAV,EDAMNT)         
         DC    AL1(TAECCMNT-TAECD,0,POCOM-PORD,L'POCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PONET-PORD,L'PONET,EDDOL)            
*                                                                               
*        SINGERS                                                                
*                                                                               
         DC    AL1(TAECTYPS,SINEND-SINSTRT,NSINFLDS)                            
         DC    AL1(TAECNUM-TAECD,L'TAECNUM,PONUM-PORD,L'PONUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,POOVER-PORD,L'POOVER,EDAMNT)         
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PODEMO-PORD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,POSPOT-PORD,L'POSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PODAYS-PORD,L'PODAYS,EDNUM)          
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,POCAT-PORD,L'POCAT,EDCHAR)           
         DC    AL1(TAECSTAT-TAECD,TAECSONC,POONC-PORD,Y,EDSTAT)                 
         DC    AL1(TAECCMNT-TAECD,0,POCOM-PORD,L'POCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PONET-PORD,L'PONET,EDDOL)            
*                                                                               
*                                                                               
*                                                                               
         DC    AL1(TAECTYPX,EXTEND-EXTSTRT,NEXTFLDS)                            
         DC    AL1(TAECNUM-TAECD,L'TAECNUM,PONUM-PORD,L'PONUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,POOVER-PORD,L'POOVER,EDAMNT)         
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PODEMO-PORD,Y,EDSTAT)                
         DC    AL1(TAECSP-TAECD,L'TAECSP,POSPOT-PORD,L'POSPOT,EDNUM)            
         DC    AL1(TAECDAY-TAECD,L'TAECDAY,PODAYS-PORD,L'PODAYS,EDNUM)          
         DC    AL1(TAECOT-TAECD,L'TAECOT,POEOT-PORD,L'POEOT,EDNUM)              
         DC    AL1(TAECDT-TAECD,L'TAECDT,POEDT-PORD,L'POEDT,EDNUM)              
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,POECAT-PORD,L'POCAT,EDCHAR)          
         DC    AL1(TAECSTAT-TAECD,TAECSNON,POENONU-PORD,Y,EDSTAT)               
         DC    AL1(TAECCMNT-TAECD,0,POCOM-PORD,L'POCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PONET-PORD,L'PONET,EDDOL)            
*                                                                               
*        MUSICIAN                                                               
*                                                                               
         DC    AL1(TAECTYPU,MUSEND-MUSSTRT,NMUSFLDS)                            
         DC    AL1(TAECNUM-TAECD,L'TAECNUM,PONUM-PORD,L'PONUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,POOVER-PORD,L'POOVER,EDAMNT)         
         DC    AL1(TAECSP-TAECD,L'TAECSP,POMSPOT-PORD,L'POMSPOT,EDNUM)          
         DC    AL1(TAECHRM-TAECD,L'TAECHRM,POMHRM-PORD,L'POMHRM,EDAMNT)         
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,POMCAT-PORD,L'POMCAT,EDCHAR)         
         DC    AL1(TAECDBL-TAECD,L'TAECDBL,POMDUB-PORD,L'POMDUB,EDCHAR)         
         DC    AL1(TAECCART-TAECD,L'TAECCART,POMCART-PORD,L'POMCART)            
         DC    AL1(EDAMNT)                                                      
         DC    AL1(TAECCMNT-TAECD,0,POCOM-PORD,L'POCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PONET-PORD,L'PONET,EDDOL)            
*                                                                               
*                                                                               
*                                                                               
         DC    AL1(TAECTYPM,MISEND-MISSTRT,NMISFLDS)                            
         DC    AL1(TAECDESC-TAECD,L'TAECDESC,POMDESC-PORD,L'POMDESC)            
         DC    AL1(EDCHAR)                                                      
         DC    AL1(TAECNET-TAECD,L'TAECNET,POMNET-PORD,L'POMNET,EDDOL)          
         DC    AL1(TAECPNH-TAECD,L'TAECPNH,POMPH-PORD,L'POMPH,EDDOL)            
         DC    AL1(TAECHNW-TAECD,L'TAECHNW,POMHW-PORD,L'POMHW,EDDOL)            
         DC    AL1(TAECTAX-TAECD,L'TAECTAX,POMTH-PORD,L'POMTH,EDDOL)            
         DC    AL1(TAECHND-TAECD,L'TAECHND,POMTH-PORD,L'POMTH,EDDOL)            
         DC    AL1(TAECCMNT-TAECD,0,POMCOM-PORD,L'POMCOM,EDCOM)                 
*                                                                               
*                                                    radio singer               
*                                                                               
         DC    AL1(TAECTYPR,RADEND-RADSTRT,NRADFLDS)                            
         DC    AL1(TAECNUM-TAECD,L'TAECNUM,PONUM-PORD,L'PONUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,POOVER-PORD,L'POOVER,EDAMNT)         
         DC    AL1(TAECSP-TAECD,L'TAECSP,POMSPOT-PORD,L'POMSPOT,EDNUM)          
         DC    AL1(TAECHRM-TAECD,L'TAECHRM,POMHRM-PORD,L'POMHRM,EDAMNT)         
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,PORTG-PORD,L'PORTG,EDNUM)            
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PORDEM-PORD,Y,EDSTAT)                
         DC    AL1(TAECCMNT-TAECD,0,POCOM-PORD,L'POCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PONET-PORD,L'PONET,EDDOL)            
*                                                                               
*                                                                               
*                                                                               
         DC    AL1(TAECTYPT,RSIEND-RSISTRT,NRSIFLDS)                            
         DC    AL1(TAECNUM-TAECD,L'TAECNUM,PONUM-PORD,L'PONUM,EDNUM)            
         DC    AL1(TAECOV1-TAECD,L'TAECOV1,POOVER-PORD,L'POOVER,EDAMNT)         
         DC    AL1(TAECSP-TAECD,L'TAECSP,POMSPOT-PORD,L'POMSPOT,EDNUM)          
         DC    AL1(TAECHRM-TAECD,L'TAECHRM,POMHRM-PORD,L'POMHRM,EDAMNT)         
         DC    AL1(TAECTAG-TAECD,L'TAECTAG,PORTG-PORD,L'PORTG,EDNUM)            
         DC    AL1(TAECSTAT-TAECD,TAECSDEM,PORDEM-PORD,Y,EDSTAT)                
         DC    AL1(TAECCAT-TAECD,L'TAECCAT,PORCAT-PORD,L'PORCAT,EDCHAR)         
         DC    AL1(TAECCMNT-TAECD,0,POCOM-PORD,L'POCOM,EDCOM)                   
         DC    AL1(TAECNET-TAECD,L'TAECNET,PONET-PORD,L'PONET,EDDOL)            
*                                                                               
         DC    X'FF'                                                            
*                                                                               
Y        EQU   C'Y'                                                             
         EJECT                                                                  
***********************************************************************         
*              TALENT AGENCY CODES                                    *         
***********************************************************************         
*                                                                               
*--------------+--------+-------+---------+                                     
*              | TAL ID | CLIENT| PROD ID |                                     
*--------------+--------+-------+---------+                                     
*                                                                               
AGYTAB   DS    0CL18                                                            
         DC     CL6'ARMY',CL6'  ',CL6'ARMY'                                     
*                                                                               
         DC     CL6'AFNY',CL6'  ',CL6'ACBO'                                     
*                                                                               
         DC     CL6'BDDE',CL6'  ',CL6'BDNY'                                     
         DC     CL6'BDAT',CL6'  ',CL6'BDNOSH'                                   
*                                                                               
         DC     CL6'BRAV',CL6'  ',CL6'YNBR'                                     
         DC     CL6'CEDE',CL6'  ',CL6'CEDE'                                     
*                                                                               
         DC     CL6'CEMN',CL6'AM',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'BR',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'CG',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'CL',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'DS',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'EB',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'GB',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'HX',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'KC',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'LC',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'MS',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'MY',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'NE',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'RG',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'SP',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'WN',CL6'CECH'                                     
         DC     CL6'CEMN',CL6'WTW',CL6'CECH'                                    
         DC     CL6'CEMN',CL6'9IE',CL6'CECH'                                    
         DC     CL6'CEMN',CL6'9NE',CL6'CECH'                                    
         DC     CL6'CECH',CL6'  ',CL6'CECH'                                     
*                                                                               
         DC     CL6'CEMN',CL6'  ',CL6'CEMN'                                     
         DC     CL6'CENY',CL6'   ',CL6'CENY'                                    
         DC     CL6'CENY',CL6'   ',CL6'CENYA'                                   
*                                                                               
         DC     CL6'DELA',CL6'   ',CL6'DELA'                                    
         DC     CL6'DENY',CL6'   ',CL6'DENY'                                    
*                                                                               
         DC     CL6'DNCH',CL6'   ',CL6'DNCH'                                    
         DC     CL6'DNDE',CL6'   ',CL6'DNDE'                                    
         DC     CL6'DNDENY',CL6'   ',CL6'DNNYA'                                 
         DC     CL6'DNDN',CL6'  ',CL6'DNDEN'                                    
         DC     CL6'DNLA',CL6'  ',CL6'DNLA'                                     
         DC     CL6'DNNY',CL6'  ',CL6'DNNY'                                     
         DC     CL6'DNSF',CL6'  ',CL6'DNSF'                                     
         DC     CL6'DNWA',CL6'  ',CL6'DNWA'                                     
*                                                                               
         DC     CL6'DWSF',CL6'  ',CL6'DWSF'                                     
*                                                                               
         DC     CL6'DWNY',CL6'  ',CL6'DWNY'                                     
*                                                                               
         DC     CL6'DYTO',CL6'  ',CL6'DYTO'                                     
*                                                                               
         DC     CL6'GBNY',CL6'  ',CL6'GBNY'                                     
*                                                                               
         DC     CL6'HRCH',CL6'  ',CL6'HRCH'                                     
         DC     CL6'HRNY',CL6'  ',CL6'HRNY'                                     
         DC     CL6'HRSF',CL6'  ',CL6'HRSF'                                     
*                                                                               
         DC     CL6'JWCH',CL6'  ',CL6'JWCH'                                     
*                                                                               
         DC     CL6'LCNY',CL6'  ',CL6'LCNY'                                     
*                                                                               
         DC     CL6'ODCH',CL6'  ',CL6'ODRCH'                                    
         DC     CL6'ODHO',CL6'  ',CL6'ODRHO'                                    
         DC     CL6'ODHO',CL6'  ',CL6'ODRHOA'                                   
         DC     CL6'ODLA',CL6'  ',CL6'ODRLA'                                    
         DC     CL6'ODNY',CL6'  ',CL6'ODRNY'                                    
         DC     CL6'OMAT',CL6'  ',CL6'OAAT'                                     
         DC     CL6'OMCH',CL6'  ',CL6'OACH'                                     
         DC     CL6'OMCH',CL6'  ',CL6'OACHA'                                    
         DC     CL6'OMDE',CL6'  ',CL6'OADE'                                     
         DC     CL6'OMHO',CL6'  ',CL6'OAHO'                                     
         DC     CL6'OMHO',CL6'  ',CL6'OAHOA'                                    
         DC     CL6'OMLA',CL6'  ',CL6'OALA'                                     
         DC     CL6'OMLA',CL6'  ',CL6'OALAA'                                    
         DC     CL6'OMNY',CL6'  ',CL6'OANY'                                     
         DC     CL6'OMNY',CL6'  ',CL6'OANYA'                                    
         DC     CL6'OMNY',CL6'  ',CL6'OANYB'                                    
         DC     CL6'OMNY',CL6'  ',CL6'OANYP'                                    
*                                                                               
         DC     CL6'RPLA',CL6'  ',CL6'RPLA'                                     
*                                                                               
         DC     CL6'SNNY',CL6'  ',CL6'SNNYC'                                    
*                                                                               
         DC     CL6'YRNY',CL6'  ',CL6'YNRB'                                     
         DC     CL6'YRNY',CL6'  ',CL6'YNRD'                                     
*                                                                               
         DC     CL6'0072',CL6'PRR',CL6'LMNYA'                                   
         DC     CL6'0071',CL6'  ',CL6'LMNYA'                                    
*                                                                               
         DC     CL6'0091',CL6'  ',CL6'SCA'                                      
*                                                                               
         DC     CL6'0258',CL6'  ',CL6'JWDE'                                     
         DC     CL6'0258',CL6'  ',CL6'JWDEP'                                    
*                                                                               
         DC     CL6'0268',CL6'  ',CL6'JWCH'                                     
*                                                                               
         DC     CL6'0368',CL6'  ',CL6'JWNY'                                     
         DC     CL6'0368',CL6'  ',CL6'JWNYP'                                    
*                                                                               
         DC     CL6'1927',CL6'  ',CL6'JWSF'                                     
*                                                                               
         DC     CL6'1950',CL6'  ',CL6'JWLA'                                     
*                                                                               
         DC     CL6'YRNY',CL6'  ',CL6'XYZ '                                     
         DC     CL6'YRNY',CL6'  ',CL6'SJR '                                     
         DC     CL6'TFNY',CL6'GTG',CL6'DDSB'                                    
         DC     CL6'TFNY',CL6'   ',CL6'DDSB'                                    
         DC     CL6'YRNY',CL6'  ',CL6'TRC'                                      
         DC     CL6'YRNY',CL6'  ',CL6'TRC1'                                     
AGYTABNM EQU   (*-AGYTAB)/L'AGYTAB                                              
         EJECT                                                                  
***********************************************************************         
*              MISCELLANEOUS DESCRIPTIONS                             *         
***********************************************************************         
*                                                                               
TVMISCS  DS    0CL16                                                            
         DC    CL16'3RD CALLBACKS'                                              
         DC    CL16'FITTINGS'                                                   
         DC    CL16'MEAL PENALTIES'                                             
         DC    CL16'WARDROBE ALLOW.'                                            
         DC    CL16'T/A PENALTIES'                                              
         DC    CL16'CONSEC. EMPLOY.'                                            
         DC    CL16'REHEARSAL DAYS '                                            
TVMISNM  EQU   (*-TVMISCS)/MISCLN                                               
*                                                                               
RADMISCS DS    0CL16                                                            
         DC    CL16'TRAVEL ALLOWANCE'                                           
RADMISNM EQU   (*-RADMISCS)/MISCLN                                              
         EJECT                                                                  
***********************************************************************         
*              TV PFKEYS AND THEIR CORRESPONDING TYPES                *         
***********************************************************************         
*                                                                               
PFKTTAB  DS    0C                                                               
         DC    AL1(TAECTYPO,L'PFKON+PFKLNQ,TEPFON-TEPFKEYS)                     
PFKON    DC    C'On'                                                            
         DC    AL1(TAECTYPF,L'PFKOFF+PFKLNQ,TEPFOFF-TEPFKEYS)                   
PFKOFF   DC    C'Off'                                                           
         DC    AL1(TAECTYPS,L'PFKSING+PFKLNQ,TEPFSING-TEPFKEYS)                 
PFKSING  DC    C'Sing'                                                          
         DC    AL1(TAECTYPX,L'PFKEXT+PFKLNQ,TEPFEXT-TEPFKEYS)                   
PFKEXT   DC    C'Ext'                                                           
         DC    AL1(TAECTYPU,L'PFKMUS+PFKLNQ,TEPFMUS-TEPFKEYS)                   
PFKMUS   DC    C'Mus'                                                           
         DC    AL1(TAECTYPM,L'PFKMISC+PFKLNQ,TEPFMISC-TEPFKEYS)                 
PFKMISC  DC    C'Misc'                                                          
         DC    AL1(255,L'PFKCMNT+PFKLNQ,TEPFCMNT-TEPFKEYS)                      
PFKCMNT  DC    C'Com'                                                           
*        DC    AL1(255,L'PFKREP+PFKLNQ,TEPFREP-TEPFKEYS)                        
*FKREP   DC    C'REP'                                                           
PFKTBLN  EQU   *-PFKTTAB                                                        
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*              RADIO PFKEYS AND THEIR CORRESPONDING TYPES             *         
***********************************************************************         
*                                                                               
PFKRTAB  DS    0C                                                               
         DC    AL1(TAECTYPR,L'PFKRAD+PFKLNQ,TEPFRAD-TEPFKEYS)                   
PFKRAD   DC    C'Ancr'                                                          
         DC    AL1(TAECTYPT,L'PFKRSNG+PFKLNQ,TEPFRSNG-TEPFKEYS)                 
PFKRSNG  DC    C'Sing'                                                          
         DC    AL1(TAECTYPU,L'PFKRMUS+PFKLNQ,TEPFMUS-TEPFKEYS)                  
PFKRMUS  DC    C'Mus'                                                           
         DC    AL1(TAECTYPM,L'PFKRMISC+PFKLNQ,TEPFMISC-TEPFKEYS)                
PFKRMISC DC    C'Misc'                                                          
         DC    AL1(255,L'PFKRCMNT+PFKLNQ,TEPFCMNT-TEPFKEYS)                     
PFKRCMNT DC    C'Com'                                                           
*        DC    AL1(255,L'PFKRREP+PFKLNQ,TEPFREP-TEPFKEYS)                       
*FKRREP  DC    C'REP'                                                           
PFKRTBLN EQU   *-PFKRTAB                                                        
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*              READ ET OR ER PROFILE                                  *         
***********************************************************************         
*                                                                               
         USING PROFKD,R2                                                        
GETPRO   NMOD1 0,*GETP*                                                         
         L     RC,0(R1)                                                         
         LA    R2,WORK                                                          
         XC    PROFKEY,PROFKEY                                                  
         XC    PROGPROF,PROGPROF                                                
         MVI   PROFKSYS,C'A'                                                    
         MVC   PROFKPGM,=C'0E '                                                 
*                                                                               
         USING TEBLOCK,R6                                                       
         L     R6,AMYTEBLK                                                      
         MVC   PROFKPGM+2(1),TEMEDIA                                            
*                                                                               
         MVC   PROFKAGY,AGYALPHA                                                
         MVC   PROFKUNL,=C'SJ'                                                  
         USING MYSAVED,R5          R5 SET IN INITIAL                            
         L     R5,AMYSAVE                                                       
         MVC   PROFKACC,SVCLI                                                   
         CLI   SVEFFOFC,X'00'     IS THERE AN OFFICE ?                          
         BE    GETP04              NO                                           
         TM    COMPSTA4,X'01'      NEW OFFICES ?                                
         BO    GETP02                                                           
         MVC   PROFKOFF,SVEFFOFC                                                
         MVI   PROFKAST,C'*'                                                    
         B     GETP04                                                           
*                                                                               
GETP02   MVI   PROFKAST,C'+'                                                    
         MVC   PROFKOFC,SVEFFOFC                                                
*                                                                               
GETP04   GOTO1 GETPROF,DMCB,PROFKEY,PROGPROF,DATAMGR                            
         XMOD1 1                                                                
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              UPDATE AN ESTIMATE RECORD                              *         
***********************************************************************         
*                                                                               
UPDTEST  NMOD1 0,UPDTES                                                         
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         USING MYSAVED,R5          R5 SET IN INITIAL                            
         L     R5,AMYSAVE                                                       
*                                                                               
         MVI   ERROR,APPVDERR                                                   
         CLI   SVESTAPP,C'Y'       IS ESTIMATE APPROVED                         
         BE    UPDTERR                                                          
*                                                                               
         MVC   AIO,AIO1            READ "HEADER" SESSION RECORD                 
         MVC   KEY,SVSESKEY                                                     
         GOTO1 READ                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,TEWELQ                                                    
         BAS   RE,GETEL2                                                        
         BNE   UPDTNO              NO DATA TO UPDATE                            
*                                                                               
         XC    TEMPEL,TEMPEL       SAVE W/C DATA                                
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPEL(0),0(R6)                                                  
*                                                                               
*                                                                               
         MVC   AIO,AIO2            READ ESTIMATE INTO AIO2                      
         MVI   RDUPDATE,C'Y'                                                    
         MVC   KEY,SVESTKEY                                                     
         GOTO1 READ                                                             
*                                                                               
*        BAS   RE,UPWORK           UPDATE THE LIST OF WORKCODES                 
*                                                                               
         USING WCD,R6                                                           
UPDTE50  LA    R6,TEMPEL           SAVED W/C DATA FROM SESSION REC              
         ZIC   R2,3(R6)            NUMBER OF W/C'S IN ELEMENT                   
         LA    R6,4(R6)            FIRST W/C                                    
*                                                                               
UPDTE70  ICM   R1,15,WCAMT                                                      
         CVD   R1,DUB                                                           
*                                                                               
*                                  DELETE OLD W/C FROM ESTIMATE                 
         USING EDAELD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         MVI   EDAEL,EDAELQ                                                     
         MVI   EDALN,EDALNQ1       WRITE OUT SHORT EL                           
         MVI   EDATYPE,EDATWORK    FLAG AS REGULAR EST ELEMENT                  
         MVC   EDAWORK,WCCODE                                                   
         ZAP   EDACOMM,DUB                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('EDAELQ',AIO),(3,EDATYPE),0            
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R6,WCNEXT                                                        
         BCT   R2,UPDTE70                                                       
*                                                                               
*        UPDATE ESTIMATE UPDATE ELEMENT                                         
*                                                                               
         MVI   ELCODE,EUPELQ       ESTIMATE UPDATE ELEMENT                      
         GOTO1 REMELEM                                                          
         LA    RE,ELEMENT          ELEMENT SAVED BY REMELEM                     
         USING EUPELD,RE                                                        
         MVC   EUPLAST,TODAYP                                                   
         MVC   EUPERS,TWAALIAS                                                  
         GOTO1 ADDELEM                                                          
         GOTO1 WRITE                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1            RESTORE SESSION RECORD                       
         MVC   KEY,SVSESKEY                                                     
         GOTO1 READ                                                             
UPDTEX   XIT1                                                                   
*                                                                               
UPDTERR  LA    R2,TESESTH                                                       
         GOTO1 VERRCUR                                                          
*                                                                               
UPDTNO   MVI   PREVPF,0                                                         
         B     UPDTEX                                                           
         EJECT                                                                  
***********************************************************************         
*              GETEL                                                  *         
***********************************************************************         
         GETEL2 (R6),DATADISP,ELCODE                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              LOOK TO SEE OF WORKCODE CHANGED                        *         
*              ADD UNUSED TO TEMPEL WITH ZERO AMOUNT                  *         
***********************************************************************         
*                                                                               
         USING WCD,R3                                                           
UPWORK   NTR1                                                                   
*                                                                               
UPWK00   LA    R1,PREVWORK         WORKCODES BEFORE UPDATE                      
         SR    R2,R2                                                            
         ICM   R2,1,3(R1)                                                       
         BZ    UPWKX               NO WORKCODES TO CHECK                        
         LA    R1,4(R1)            ADDRESS FIRST WORKCODE                       
*                                                                               
UPWK02   LA    R3,TEMPEL           GET WORKCODES AFTER UPDATE                   
         ZIC   R4,3(R3)            GET NUMBER OF WORKCODES                      
         LA    R3,4(R3)            ADDRESS FIRST WORKCODE                       
*                                                                               
UPWK04   CLC   WCCODE,WCCODE-WCD(R1)                                            
         BNE   UPWK06              NO MATCH, LOOK FURTHER                       
         LA    R1,WCLNQ(R1)        STILL THERE, CHECK NEXT                      
         BCT   R2,UPWK02                                                        
         B     UPWKX               ALL DONE                                     
*                                                                               
UPWK06   LA    R3,WCNEXT           GET NEXT NEW ONE                             
         BCT   R4,UPWK04           CHECK AGAIN                                  
*                                                                               
         LA    R3,TEMPEL           NONE LEFT, ADD THE WORKCODE                  
         ZIC   R4,3(R3)                                                         
         LA    R4,1(R4)                                                         
         STC   R4,3(R3)            INCREMENT THE COUNT                          
         BCTR  R4,0                                                             
         MH    R4,=Y(WCLNQ)                                                     
         LA    R3,4(R3,R4)         END TO END OF TABLE                          
*                                                                               
         MVC   WCCODE-WCD(L'WCCODE,R3),WCCODE-WCD(R1)                           
         B     UPWK00              START ALL OVER                               
*                                                                               
UPWKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LANDSCAPE PRINT SPECS                                  *         
***********************************************************************         
*                                                                               
PRTSPECS DS    0D                                                               
*                                                                               
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,49,C'SESSION ESTIMATE REPORT'                                 
         SSPEC H2,49,C'-----------------------'                                 
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,REPORT                                                     
         SSPEC H4,109,PAGE                                                      
*                                                                               
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'JOB'                                                      
*                                                                               
         SSPEC H8,2,C'ESTIMATE'                                                 
         SSPEC H9,2,C'MEDIA'                                                    
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*              PORTRAIT PRINT SPECS                                   *         
***********************************************************************         
*                                                                               
PORSPECS DS    0D                                                               
*                                                                               
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,34,C'SESSION ESTIMATE REPORT'                                 
         SSPEC H2,34,C'-----------------------'                                 
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H4,65,REPORT                                                     
         SSPEC H4,79,PAGE                                                       
*                                                                               
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'JOB'                                                      
*                                                                               
         SSPEC H8,2,C'ESTIMATE'                                                 
         SSPEC H9,2,C'MEDIA'                                                    
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*              SAVED STORAGE AFTER TWA. iT IS COVERED BY R5           *         
***********************************************************************         
*                                                                               
MYSAVED  DSECT                     DSECT TO COVER SAVED STORAGE                 
PREVPF   DS    CL1                 SAVED PF KEY                                 
PREVWORK DS    CL255               SAVED PREVIOUS WORKCODE SUMMARY              
SVESTKEY DS    CL(L'KEY)           SAVED ESTIMATE KEY                           
SVSESKEY DS    CL(L'KEY)           SAVED SESSION KEY                            
SVCLI    DS    CL6                 SAVED VALHED CLICODE                         
SVPROD   DS    CL6                 SAVED VALHED PRODCODE                        
SVJOB    DS    CL6                 SAVED VALHED JOBNUM                          
SVEFFOFC DS    CL2                 GETOPTS EFFECTIVE OFFICE                     
SVESTAPP DS    CL1                 Y, ESTIMATE IS APROVED                       
SVBIGLN  DS    H                   RECORD LENGTH OF BIG RECORD                  
MYSAVELN EQU   *-MYSAVED                                                        
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROCAD                                                       
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPOOK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDGETPROFD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETPROFD                                                     
         PRINT ON                                                               
       ++INCLUDE TAGENEQUS                                                      
SUBSYSD  DSECT                                                                  
         ORG   ACIOBLOK                                                         
LOCAL    DS    0C                                                               
SAVERE   DS    A                                                                
ATAWORK  DS    A                                                                
AMYSAVE  DS    A                   A(SAVED STORAGE)                             
AMYTEBLK DS    A                   A(TEBLK) IN SAVED STORAGE                    
ASESREC  DS    A                   A(SPACE TO BUILD BIG SESSION RECORD)         
SVAIO    DS    A                                                                
FLDTAB   DS    A                   TABLE OF FIELDS TO PRINT                     
HEADTAB  DS    A                   TABLE OF HEADER DATA                         
SUBTOTAL DS    5F                  SUBTOTAL TAECAMTS                            
FINTOTAL DS    F                   FINAL TOTAL                                  
SVKEY    DS    CL42                                                             
KEYCHG   DS    CL1                                                              
TAMEDIA  DS    CL1                 MEDIA FOR TALENT BLOCK                       
ESTNUM   DS    CL1                 BINARY ESTIMATE NUMBER                       
PREVTYPE DS    CL1                                                              
TYPECNT  DS    CL1                                                              
TEMPEL   DS    CL255               SAVE AN ELEMENT                              
*                                                                               
MISCTAB  DS    (MISCMAX)CL(MISCLN) TABLE OF MISC VALUES                         
MISCMAX  EQU   6                   MAX MISC LITERALS                            
MISCLN   EQU   16                  LENGTH OF EACH LITERAL                       
*                                                                               
*        GOBLOCK AND GOXBLOCK                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
         ORG   GOBLOCK             REUSE GOBLOCK FOR EDITOR BLOCK               
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
         EJECT                                                                  
PRTTABD  DSECT                                                                  
PRTTFLOF DS    CL1                 OFFSET OF FIELD INTO CAST DET EL             
PRTTFLLN DS    CL1                 LENGTH OF FIELD                              
         ORG   PRTTFLLN                                                         
PRTTBIT  DS    CL1                 FOR STAT EDIT, BIT TO TEST                   
PRTTPROF DS    CL1                 OFFSET OF PRINT FIELD INTO P                 
PRTTPRLN DS    CL1                 MAX LENGTH OF PRINT FIELD                    
         ORG   PRTTPRLN                                                         
PRTTCHAR DS    CL1                 FOR STAT EDIT, CHAR TO PRINT                 
PRTTEDIT DS    CL1                 EDIT ROUTINE                                 
PRTTNEXT EQU   *                                                                
PRTTLN   EQU   *-PRTTABD                                                        
         EJECT                                                                  
* DSECT TO COVER REQUEST HEADER AND CARD                                        
*                                                                               
REQHDR   DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80               REQUEST CARD LAYOUT                          
REQJCLID DS    CL2                 JCL ID                                       
REQAGYID DS    CL2                 AGENCY ID                                    
         DS    CL1                 N/D                                          
REQSIN   DS    CL6                 SYSTEM INPUT NUMBER                          
         ORG   REQUEST+L'REQUEST                                                
         EJECT                                                                  
PRTD     DSECT                     LANDSCAPE DSECT                              
PRNUM    DS    CL10                                                             
         DS    CL2                                                              
PROVER   DS    CL6                 OVER PERCENTAGE                              
         DS    CL2                                                              
PRDEMO   DS    CL4                                                              
         DS    CL2                                                              
PRSPOT   DS    CL5                                                              
         DS    CL2                                                              
PRDAYS   DS    CL4                                                              
         DS    CL2                                                              
*                                                                               
PRVAR    EQU   *                                                                
PRON     EQU   *                   PRINT FIELDS FOR ON/OFF CAMERA               
PROT     DS    CL8                                                              
         DS    CL2                                                              
PRDT     DS    CL10                                                             
         DS    CL2                                                              
PRTAG    DS    CL4                                                              
         DS    CL2                                                              
PRTRAV   DS    CL6                                                              
         DS    CL2                                                              
PRPDW    DS    CL18                PRIOR DAY WARDROBE                           
         DS    CL2                                                              
PRVARLN  EQU   *-PRVAR                                                          
*                                                                               
PRSING   ORG   PRVAR               PRINT FIELDS FOR SINGERS                     
PRCAT    DS    CL8                                                              
         DS    CL2                                                              
PRONC    DS    CL10                                                             
*                                                                               
PREXT    ORG   PRVAR               PRINT FIELDS FOR EXTRAS                      
PREOT    DS    CL8                                                              
         DS    CL2                                                              
PREDT    DS    CL10                                                             
         DS    CL3                                                              
PRECAT   DS    CL8                                                              
         DS    CL2                                                              
PRENONU  DS    CL9                 NON-UNION                                    
         DS    CL2                                                              
*                                                                               
PRMUS    ORG   PRDEMO              PRINT FIELDS FOR MUSIC LINE                  
PRMSPOT  DS    CL5                                                              
         DS    CL2                                                              
PRMHRM   DS    CL13                                                             
         DS    CL2                                                              
PRMCAT   DS    CL8                                                              
         DS    CL2                                                              
PRMDUB   DS    CL7                                                              
         DS    CL2                                                              
PRMCART  DS    CL7                                                              
         DS    CL2                                                              
*                                                                               
*                                                                               
PRRAD    ORG   PRMCAT              PRINT FIELDS FOR RADIO ANNOUNCER             
PRRTG    DS    CL4                                                              
         DS    CL2                                                              
PRRDEM   DS    CL4                                                              
*                                                                               
PRRSI    DS    0C                  RADIO SINGER ORGS ON TO ANNOUNCER            
         DS    CL2                                                              
PRRCAT   DS    CL8                 CATEGORY                                     
*                                                                               
PRMISC   ORG   PRTD                PRINT FIELDS FOR MISC                        
PRMDESC  DS    CL16                                                             
         DS    CL1                                                              
PRMNET   DS    CL10                                                             
         DS    CL10                                                             
PRMPH    DS    CL10                                                             
         DS    CL10                                                             
PRMHW    DS    CL10                                                             
         DS    CL10                                                             
PRMTH    DS    CL10                                                             
         DS    CL10                                                             
PRMCOM   DS    CL20                                                             
*                                                                               
PRFIN    ORG   PRTD                PRINT FIELDS FOR FINAL TOTALS                
         DS    CL10                                                             
PRTNET   DS    CL20                                                             
PRTPNH   DS    CL20                                                             
PRTHNW   DS    CL20                                                             
PRTTNH   DS    CL20                                                             
PRTTOT   DS    CL20                                                             
*                                                                               
         ORG   PRVAR                                                            
PRECTAG  DS    CL8                                                              
         DS    CL1                                                              
PRECOM   DS    CL60                                                             
         ORG   PRVAR+PRVARLN                                                    
PRCOM    DS    CL20                                                             
PRNET    DS    CL12                                                             
         EJECT                                                                  
PORD     DSECT                     PORTRAIT DSECT                               
PONUM    DS    CL7                                                              
         DS    CL2                                                              
POOVER   DS    CL4                 OVER PERCENTAGE                              
         DS    CL2                                                              
PODEMO   DS    CL4                                                              
         DS    CL2                                                              
POSPOT   DS    CL2                                                              
         DS    CL2                                                              
PODAYS   DS    CL2                                                              
         DS    CL2                                                              
*                                                                               
POVAR    EQU   *                                                                
POON     EQU   *                   PRINT FIELDS FOR ON/OFF CAMERA               
POOT     DS    CL2                                                              
         DS    CL2                                                              
PODT     DS    CL2                                                              
         DS    CL2                                                              
POTAG    DS    CL3                                                              
         DS    CL2                                                              
POTRAV   DS    CL5                                                              
         DS    CL2                                                              
POPDW    DS    CL5                 POIOR DAY WARDROBE                           
         DS    CL2                                                              
POVARLN  EQU   *-POVAR                                                          
*                                                                               
POSING   ORG   POVAR               PRINT FIELDS FOR SINGERS                     
POCAT    DS    CL3                                                              
         DS    CL4                                                              
POONC    DS    CL10                                                             
*                                                                               
POEXT    ORG   POVAR               PRINT FIELDS FOR EXTRAS                      
POEOT    DS    CL2                                                              
         DS    CL2                                                              
POEDT    DS    CL2                                                              
         DS    CL2                                                              
POECAT   DS    CL3                                                              
         DS    CL4                                                              
POENONU  DS    CL8                 NON-UNION                                    
         DS    CL2                                                              
*                                                                               
POMUS    ORG   PODEMO              PRINT FIELDS FOR MUSIC LINE                  
POMSPOT  DS    CL2                                                              
         DS    CL2                                                              
POMHRM   DS    CL11                                                             
         DS    CL2                                                              
POMCAT   DS    CL3                                                              
         DS    CL2                                                              
POMDUB   DS    CL7                                                              
         DS    CL2                                                              
POMCART  DS    CL7                                                              
         DS    CL2                                                              
*                                                                               
*                                                                               
PORAD    ORG   POMCAT              PRINT FIELDS FOR RADIO ANNOUNCER             
PORTG    DS    CL4                                                              
         DS    CL2                                                              
PORDEM   DS    CL4                                                              
*                                                                               
PORSI    DS    0C                  RADIO SINGER ORGS ON TO ANNOUNCER            
         DS    CL2                                                              
PORCAT   DS    CL3                 CATEGORY                                     
*                                                                               
POMISC   ORG   PORD                PRINT FIELDS FOR MISC                        
POMDESC  DS    CL16                                                             
         DS    CL1                                                              
POMNET   DS    CL12                                                             
         DS    CL1                                                              
POMPH    DS    CL12                                                             
         DS    CL1                                                              
POMHW    DS    CL12                                                             
         DS    CL1                                                              
POMTH    DS    CL12                                                             
         DS    CL1                                                              
POMCOM   DS    CL20                                                             
*                                                                               
POFIN    ORG   PORD                PRINT FIELDS FOR FINAL TOTALS                
         DS    CL10                                                             
POTNET   DS    CL16                                                             
POTPNH   DS    CL16                                                             
POTHNW   DS    CL16                                                             
POTTNH   DS    CL16                                                             
POTTOT   DS    CL16                                                             
*                                                                               
         ORG   PODEMO                                                           
POECTAG  DS    CL8                                                              
         DS    CL1                                                              
POECOM   DS    CL60                                                             
         ORG   POVAR+POVARLN                                                    
POCOM    DS    CL20                                                             
PONET    DS    CL12                                                             
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE TASYSESTD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACPRO3A   04/28/03'                                      
         END                                                                    
