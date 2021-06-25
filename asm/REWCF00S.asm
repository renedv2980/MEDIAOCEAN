*          DATA SET REWCF00S   AT LEVEL 019 AS OF 06/03/04                      
*PHASE T83A00B                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE UNBOOK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE REPRPUPV                                                               
*INCLUDE REGENVER                                                               
                                                                                
         TITLE 'REWCF00(T83A00) - REP WEB CONFIRM'                              
*********************************************************************           
* ALL UTIL ROUTINES SHOULD FOLLOW USE THE FOLLOWING OUTPUT FORMAT:              
*                                                                               
*   CONDITION CODE NOT EQUAL                                                    
*                                                                               
*       - INDICATES THE ROUTINE FAILED                                          
*            COULD BE BECAUSE OF INVALID DATA, NO DATA ON FILE, ETC.            
*       - REASON/ERROR CODE IN FIRST HALF WORD OF OUTPUT AREA                   
*                                                                               
*   CONDITION CODE EQUAL                                                        
*                                                                               
*       - INDICATES THE ROUTINE SUCCEEDED                                       
*       - OUPUT AREA HAS THE FOLLOWING FORMAT:                                  
*          BYTE 0-1    LENGTH OF DATA FOR A SINGLE ITEM (I)                     
*          BYTE 2      NUMBER OF SUBDIVISIONS IN A SINGLE DATA ITEM (N)         
*          BYTE 3-(3+2N) 1 BYTE LENGTH OF EACH DATA SUBDIVISION                 
*                        1 BYTE DATA TYPE CODE                                  
*          BYTE (4+2N)-(4+2N+JI)   DATA (J = 0 - NUMBER OF ITEMS)               
*          BYTE (5+2N+JI)          00  END OF DATA                              
*---------------------------------------------------------------------*         
*  HISTORY AS OF 07/19/2002                                           *         
*                                                                     *         
* 03/15/2002  JRD    COPIED FROM REEZP00                              *         
* 07/19/2002  JRD    FIRST VERSION FOR WEB DEPLOYED                   *         
* 07/29/2002  JRD    FIX PROBLEM READING MUTIPLE STATIONS IN BCCONDWN *         
*                      KEY WAS NOT BEING SAVED TO WORK2 AS EXPECTED   *         
* 07/29/2002  JRD    TRY AGAIN, STOP USING WORK2 AND ALWAYS REBUILD   *         
*                      THE KEY                                        *         
* 07/29/2002  JRD    TURNS OUT IT WAS A BAD COMPARE LENGTH THE USERID *         
*                      WAS NOT BEING CHECKED                          *         
* 09/23/2002  JRD    CHECK CONFIRMED NOW FLAG FOR WIP STATUS          *         
* 12/12/2002  JRD    CHECK STA-CF FLAG FOR REDI ORDERS                *         
* 03/05/2003  JRD    DISABLE STACF MO DID NOT IMPLEMENT XML           *         
* 06/03/2004  BU     FILTER OUT PURE FORECAST ORDERS                  *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T83A00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T83A00,RR=R2,CLEAR=YES                                   
         LR    R7,RB                                                            
         AH    R7,=Y(COMMON-T83A00)                                             
         USING COMMON,R7                                                        
*                                                                               
*---------------------*                                                         
* INITIALIZATION CODE *                                                         
*---------------------*                                                         
         USING WORKD,RC                                                         
         ST    R2,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,ASYSPARM                                                      
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ASYSFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         LR    RE,RC                                                            
         AH    RE,=Y(IOAREA1-WORKD)                                             
         ST    RE,AIOREC                                                        
         ST    RE,AIO1                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO2                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO3                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO4                                                          
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
         L     RE,=V(UNBOOK)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VUNBOOK                                                       
         L     RE,=V(RECUP)                                                     
         A     RE,BASERELO                                                      
         ST    RE,VRECUP                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         MVC   REPALPHA,TWAAGY                                                  
         DROP  RA                                                               
         USING T83AFFD,RA                                                       
MB       USING FAMSGD,FAMSGBLK                                                  
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VCOLY,CCALLOV                                                    
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VDEMAND,CDEMAND                                                  
         DROP  R1                                                               
*                                                                               
***********************************                                             
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
***********************************                                             
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BASERELO                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAX NUMBER OF PHASES (CORERES)            
         SR    R0,R0                                                            
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCOLY                                                         
*                                                                               
         GOTO1 (RF),(R1),0,(R0)    <==  SPECIAL FOR BOOKVAL BECAUSE             
         MVC   VBOOKVAL,0(R1)             QBOOKVAL IS EQUATED TO 0              
*                                                                               
INIT0002 ICM   R0,1,0(R2)          ANY ENTRY HERE?                              
         BZ    INIT0004            NONE, SKIP TO THE NEXT ENTRY                 
*                                                                               
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
INIT0004 LA    R2,1(R2)            BUMP TO THE NEXT ENTRY                       
         LA    R3,4(R3)                                                         
         BCT   R4,INIT0002                                                      
*                                                                               
         LR    RE,RB                                                            
         AH    RE,=Y(VROUTS-T83A00)                                             
         LA    R0,NUMROUTS                                                      
         SR    RF,RF                                                            
         LA    R1,VREAD                                                         
INIT0010 DS    0H                                                               
         ST    RE,0(R1)                                                         
         STC   RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT0010                                                      
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
K        USING RREPKEY,KEY         GET PARENT REP CODE                          
         XC    K.RREPKEY,K.RREPKEY                                              
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD                                
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RREPELEM-RREPREC(R6)                                          
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                LOST REP ELEMENT                             
*                                                                               
         USING RREPELEM,R6                                                      
         MVC   PARALPHA,RREPPAR                                                 
         MVC   REPNAME,RREPNAME                                                 
         MVC   REPADDR,RREPADDR                                                 
         MVC   LOWRND,RREPPROF+20                                               
         NI    LOWRND,X'0F'        MAKE IT HEX                                  
         MVC   HIRND,RREPPROF+21                                                
         NI    HIRND,X'0F'         MAKE IT HEX                                  
*                                                                               
* SAVE THE MASTER REP TEMPORARILY FOR THE INITAL DOWNLOAD                       
*  IN INVSEQ                                                                    
*                                                                               
         MVC   INVSEQ,REPALPHA     REP COMPANY                                  
         CLC   RREPMAST,=C'  '     NO MASTER CONTROL                            
         BNH   INIT0012                                                         
         CLC   RREPMAST,=X'FFFF'   THIS IS THE MASTER                           
         BE    INIT0012                                                         
*                                                                               
         MVC   INVSEQ,RREPMAST     SET THE MASTER                               
*                                                                               
INIT0012 DS    0H                                                               
         MVC   INVSEQ+2(1),RREPPROF+11                                          
         DROP  R6                                                               
*                                                                               
         CLI   LOWRND,5            MAKE SURE ROUNDING IS VALID                  
         BH    *+20                BAD VALUE - CLEAR ROUNDING                   
         CLI   HIRND,5                                                          
         BL    *+12                BAD VALUE - CLEAR ROUNDING                   
         CLI   HIRND,9                                                          
         BL    *+12                EVERYTHING IS OK                             
         MVI   LOWRND,0                                                         
         MVI   HIRND,0                                                          
*                                                                               
         GOTO1 GETPROF,DMCB,('RREPQCNT',CONPROFS)                               
*                                                                               
         BAS   RE,CHKGLOB                                                       
*                                                                               
INIT0030 DS    0H                                                               
         BAS   RE,INIFALNK         INITIALIZE FALINK BLOCK                      
         GOTO1 VFALINK,DMCB,FABLK  GIVE FALINK CONTROL                          
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
COMMON   DS    0D                                                               
         DC    CL8'**FAMAP*'                                                    
       ++INCLUDE REWCFMAP                                                       
         EJECT                                                                  
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    DS    0H                  SET CC LOW & FAMSGNO                         
         MVC   MB.FAMSGNO,ERROR                                                 
         CLI   *,FF                                                             
         B     EXIT                                                             
EXITNO   LTR   RB,RB               SET CC NOT EQUAL                             
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXIT     DS    0H                  JUST EXIT                                    
         XIT1                                                                   
*                                                                               
ETOOBIG  MVC   ERROR,=Y(804)                                                    
         B     EXITL                                                            
*                                                                               
EPARMSEQ MVC   ERROR,=Y(210)                                                    
         B     EXITL                                                            
*                                                                               
EINVLEN  MVC   ERROR,=Y(85)                                                     
         B     EXITL                                                            
*                                                                               
EINVUPG  MVC   ERROR,=Y(235)                                                    
         B     EXITL                                                            
*                                                                               
EXITINV  MVC   ERROR,=Y(2)                                                      
         B     EXITL                                                            
*                                                                               
EBADSTA  DS    0H                                                               
         MVC   ERROR,=Y(150)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADBOOK DS    0H                                                               
         MVC   ERROR,=Y(232)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADDEMO DS    0H                                                               
         MVC   ERROR,=Y(233)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADUPNM DS    0H                                                               
         MVC   ERROR,=Y(836)                                                    
         B     ADDFLD                                                           
*                                                                               
ADDFLD   DS    0H                  ADD FIELD IN WORK TO MESSAGE                 
         MVI   MB.FAMSGXTR,C'('                                                 
         ZIC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MB.FAMSGXTR+1(0),WORK+8                                          
         LA    RE,MB.FAMSGXTR+2(RE)                                             
         MVI   0(RE),C')'                                                       
         B     EXITL                                                            
*                                                                               
         GETEL R8,=Y(RCONELEM-RCONREC),ELCODE                                   
*                                                                               
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)                        
*                          - EXPECTS R1 TO HOLD MAPCODE                         
***********************************************************************         
ITER     DS    0H                                                               
         CLC   0(2,RF),=X'0000'    E.O.T.                                       
         BE    ITER04              UNKNOWN MAPCODE                              
         CLM   R1,3,0(RF)          R1 HOLDS MAPCODE                             
         BE    ITER02              MATCHED                                      
         LA    RF,FLDTABLQ(RF)                                                  
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,4(RF)         ROUTINE TO HANDLE THE VERB                   
         A     RF,BASERELO                                                      
         BR    RF                                                               
*                                                                               
ITER04   DS    0H                                                               
         MVC   ERROR,=Y(841)                                                    
         B     EXITL                                                            
***********************************************************************         
* INITIALIZE FALINK                                                             
***********************************************************************         
INIFALNK NTR1                                                                   
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         TM    MISCFLG1,MF1GLOB                                                 
         BNZ   *+10                                                             
         XC    VERSION,VERSION     CLEAR PC VERSION - UNLESS FROM GLOB          
*                                                                               
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,CONINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD         A(TWABLD)                                
         L     R1,ACOMFACS             A(SWITCH)                                
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
         LA    R1,*                                                             
         A     R1,=A(RECEIVE-(*-4))    A(MY RECEIVE ROUTINE)                    
         ST    R1,FALARCV                                                       
         LA    R1,*                                                             
         A     R1,=A(SEND-(*-4))       A(MY SEND ROUTINE)                       
         ST    R1,FALASND                                                       
***JRD   LA    R1,TRANSLATE            A(ELEMENT TRANSLATION ROUTINE)           
***JRD   ST    R1,FALATRN                                                       
         LA    R1,*                                                             
         A     R1,=A(BREAK-(*-4))      A(BREAK ROUTINE)                         
         ST    R1,FALASTP                                                       
         LA    R1,*                                                             
         A     R1,=A(RESUME-(*-4))     A(RESUME ROUTINE)                        
         ST    R1,FALARSM                                                       
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-T83AFFD) A(FALINK SAVED STORAGE)                  
         ST    R1,FALASVE                                                       
         LA    R1,*                                                             
         A     R1,=A(FAMAP-(*-4))      A(MAP TABLE)                             
         ST    R1,FALAMAP                                                       
         ST    R1,AMAPTAB          FOR OTHER OVERLAYS                           
         MVC   FALAPGS,TWAPGS                                                   
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
         EJECT                                                                  
***********************************************************************         
* CHKGLOB - CHECK FOR INCOMING/RETURN GLOBBER CALLS                             
**********************************************************************          
CHKGLOB  NTR1                                                                   
         NI    MISCFLG1,FF-MF1GLOB                                              
*                                                                               
* CHECK FOR XFER CONTROL ELEM                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK2,24,GLVXCTL                          
         TM    DMCB+8,X'10'                                                     
         BO    EXITOK              NO CONTROL ELEM                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    MISCFLG1,MF1GLOB                                                 
         LA    R0,*                                                             
         A     R0,=A(GLOBDWN-(*-4))                                             
         ST    R0,SENDROUT         GENERIC GLOBBER DOWNLOAD                     
*                                                                               
* CHECK FOR RETURN CONTRACT CON-ACT ELEMENT                                     
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK2,GLCONLNQ,GLRKACT                    
         TM    DMCB+8,X'10'                                                     
         BNZ   EXITL               NO ELEMENT                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLRKACT                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***      MVC   RTSRVC,TWARTSV      RESTORE RATING SERVICE FROM TWA              
*                                                                               
         LA    R1,WORK2                                                         
         USING GLCONNUM,R1                                                      
         MVC   CONERR,GLCONERR                                                  
         DROP  R1                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GETPROF - GET THE PROGRAM PROFILES                                            
*   INPUT:   P1  BYTE 1      PROGRAM #                                          
*                BYTE 2-4    A(PROFILE AREA) CL10                               
*                                                                               
*   OUPUT:   PROFILES IN PROFILE AREA                                           
*                                                                               
***********************************************************************         
GETPROF  NTR1                                                                   
         ZIC   R3,0(R1)                                                         
         L     R2,0(R1)                                                         
         XC    0(10,R2),0(R2)                                                   
*                                                                               
K        USING RREPKEY,KEY         GET PARENT REP CODE                          
         XC    K.RREPKEY,K.RREPKEY                                              
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD                                
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RREPELEM-RREPREC(R6)                                          
GPROF02  CLI   0(R6),0                                                          
         BE    GETPROFX            NO PROFILE ELEMENT                           
         CLI   0(R6),X'04'                                                      
         BE    GPROF04                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GPROF02                                                          
*                                                                               
GPROF04  DS    0H                                                               
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLM   R3,1,RREPPGM1       CORRECT PROGRAM?                             
         BE    GPROF20             YES                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GETPROFX            NOT FOUND. USE DEFAULTS.                     
*                                                                               
GPROF20  MVC   0(10,R2),RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GETPROFX B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
*-------------------------------------------------------------------            
VROUTS   NTR1  BASE=*,LABEL=*                                                   
         SRL   RF,24                                                            
         B     ROUTTAB(RF)                                                      
*                                                                               
ROUTTAB  B     READ                                                             
         B     SEQ                                                              
         B     HIGH                                                             
         B     ADD                                                              
         B     WRITE                                                            
         B     GETREC                                                           
         B     PUTREC                                                           
         B     ADDREC                                                           
NUMROUTS EQU   (*-ROUTTAB)/4                                                    
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         B     DIRCTRY                                                          
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
DIRCTRY  CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(DMINBTS,COMMAND),=C'REPDIR',KEYSAVE,KEY,0            
         B     DMCHECK                                                          
         EJECT                                                                  
*-------------------------------------------------------------------            
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
*-------------------------------------------------------------------            
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
FILE     CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   AIOREC,0(R1)                                                     
         LA    R0,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R0,KEY                                                           
         GOTO1 VDMGR,DMCB,(DMINBTS,COMMAND),=C'REPFILE',(R0),          X        
               AIOREC,(0,DMWORK)                                                
         EJECT                                                                  
*-------------------------------------------------------------------            
*                  DATA MANAGER ERRORS AND EXIT                                 
*-------------------------------------------------------------------            
DMCHECK  DS    0H                                                               
         MVI   DMINBTS,X'00'                                                    
         MVI   UPDATE,C'N'                                                      
         MVC   DMBYTE,DMCB+8                                                    
*                                                                               
         NC    DMBYTE,DMOUTBTS                                                  
         BZ    DMEXITOK                                                         
         B     DMEXITOK                                                         
*                                                                               
DMEXITL  DS    0H                  SET CC LOW & FAMSGNO                         
         CLI   *,FF                                                             
         B     *+6                                                              
DMEXITOK DS    0H                                                               
         CR    RB,RB               SET CC EQUAL                                 
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* XARSE- EXTENDED PARSE                                                         
*                                                                               
*   PARSE GENERIC RETURN FOR AND SEND TO PC BASED ON TABLE                      
*                                                                               
*    P1 - A(GENERIC OUTPUT STREAM)                                              
*    P2 - A(NULL TERMINATED TABLE)                                              
*                                                                               
***********************************************************************         
XARSE    NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            A(OUTPUT STREAM)                             
         L     R6,4(R1)            A(TABLE)                                     
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,2(R8)                                                       
         BZ    EXITL               NO SUBDIVISIONS                              
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,2(R8)                                                       
         MHI   R2,2                                                             
         LA    R2,3(R2,R8)         INDEX TO DATA                                
*                                                                               
XARSE002 DS    0H                                                               
         LA    R5,3(R8)                                                         
         ICM   R4,1,2(R8)                                                       
         NI    MISCFLG1,FF-MF1TMPB2                                             
XARSE010 DS    0H                                                               
         LA    R3,4(R6)            CHECK FOR DATA IN TABLE                      
XARSE012 DS    0H                                                               
         CLI   0(R3),0             END OF TYPES?                                
         BE    XARSE100            YES                                          
         CLC   0(1,R3),1(R5)       TYPE MATCH?                                  
         BE    XARSE020            YES                                          
         LA    R3,XTABLQ(R3)                                                    
         B     XARSE012                                                         
*                                                                               
XARSE020 DS    0H                                                               
         CLI   1(R3),2             GENERIC ELEMENT ADD?                         
         BH    XARSE100            NO                                           
*                                                                               
         CLI   1(R3),1             SPACES CHECK?                                
         BNE   XARSE022            NO                                           
*                                                                               
         ZIC   RE,0(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SPACES                                                   
         BNH   XARSE034                                                         
*                                                                               
XARSE022 DS    0H                                                               
         TM    MISCFLG1,MF1TMPB2   HEADER SENT?                                 
         BO    XARSE030            YES                                          
*                                                                               
         ICM   RF,15,0(R6)         GET ELEMENT HEADER ADDRESS                   
         BZ    XARSE028                                                         
*                                                                               
         A     RF,BASERELO                                                      
         GOTO1 ASETELEM,DMCB,AFABLK,(RF),0                                      
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
XARSE028 DS    0H                                                               
         OI    MISCFLG1,MF1TMPB2   HEADER SENT                                  
*                                                                               
XARSE030 DS    0H                                                               
         ZIC   R0,0(R5)            GET DATA LENGTH                              
         ICM   RF,15,4(R3)         GET MAPTABLE ADDRESS                         
         A     RF,BASERELO                                                      
*                                                                               
         CLI   1(R3),2             MAPCODE ONLY?                                
         BNE   XARSE032            NO                                           
*                                                                               
         CLI   0(R2),C'Y'                                                       
         BNE   XARSE034                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,(RF),0,0                                    
         B     XARSE034                                                         
*                                                                               
XARSE032 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,(RF),(R2),(R0)                              
*                                                                               
XARSE034 DS    0H                                                               
*                                                                               
XARSE100 DS    0H                                                               
         ZIC   R0,0(R5)            BUMP DISPLACEMENT INTO DATA                  
         AR    R2,R0                                                            
*                                                                               
         LA    R5,2(R5)            NEXT ENTRY                                   
         BCT   R4,XARSE010                                                      
*                                                                               
         CLI   0(R2),0                                                          
         BNE   XARSE002                                                         
*                                                                               
XARSEX   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
********************************************************************            
* LITERALS AND CONSTANTS                                                        
********************************************************************            
FF       EQU   X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
BREAK    NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
RESUME   NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
SEND     NTR1  BASE=*,LABEL=*                                                   
         OC    SENDROUT,SENDROUT                                                
         BZ    SENDX                                                            
*                                                                               
         MVC   ASETELEM,0(R1)      SAVE FALINK ROUTINE ADDRESSES                
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
         L     RF,SENDROUT                                                      
         BASR  RE,RF                                                            
         BL    EXITL                                                            
*                                                                               
         TM    MISCFLG1,MF1DATA    ANY DATA IN BUFFER?                          
         BZ    SENDX               NO - DON'T CLOSE                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
SENDX    DS    0H                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESSE                 
         XC    AFLDTAB,AFLDTAB                                                  
         XC    SENDROUT,SENDROUT                                                
*                                                                               
RCV000   DS    0H                                                               
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    EXIT                FALINK ERROR                                 
         BH    RCV100              END OF DATA                                  
*                                                                               
         CLI   FPARMS,0            HEADER?                                      
         BNE   RCV010              NO                                           
*                                                                               
         BAS   RE,PRCHDR           PROCESS HEADER                               
         BNL   RCV020                                                           
         B     EXITL                                                            
*                                                                               
RCV010   DS    0H                  FIELD DATA                                   
         BAS   RE,PRCFLD           PROCESS FIELD DATA                           
         BNL   RCV020                                                           
         B     EXITL                                                            
*                                                                               
RCV020   DS    0H                                                               
         B     RCV000                                                           
*                                                                               
RCV100   DS    0H                                                               
         B     EXITOK                                                           
*---------------------------------------------------------------------          
* PRCHDR - PROCESS HEADER ELEMENT                                               
*---------------------------------------------------------------------          
PRCHDR   NTR1                                                                   
         L     R6,FPARMS                                                        
         USING MHELD,R6            R6=A(HEADER ENTRY)                           
         TM    MHUFLG,MHUFRQ       REQUEST HEADER?                              
         BO    *+14                YES                                          
         MVC   ERROR,=Y(54)                                                     
         BL    EXITL                                                            
*                                                                               
         LA    RF,*                                                             
         A     RF,=A(REQHDRS-(*-4))                                             
         SR    R1,R1                                                            
PHDR0010 DS    0H                                                               
         CLC   0(2,RF),=X'0000'    E.O.T.                                       
         BNE   *+14                                                             
         MVC   ERROR,=Y(841)       UNKNOWN MAPCODE                              
         B     EXITL                                                            
*                                                                               
         CLC   MHCODE,0(RF)        R1 HOLDS MAPCODE                             
         BE    PHDR0012            MATCHED                                      
         LA    RF,ROUTABLQ(RF)                                                  
         B     PHDR0010            ITERATE THIS TABLE                           
*                                                                               
PHDR0012 DS    0H                                                               
         LH    R0,=Y(SVPARMBF-T83AFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF           CLEAR LAST FIELD SEEN                        
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         ICM   RE,15,4(RF)                                                      
         A     RE,BASERELO                                                      
         ST    RE,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
         ICM   RE,15,8(RF)                                                      
         A     RE,BASERELO                                                      
         ST    RE,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
*                                                                               
         ICM   RF,15,12(RF)        CHECK FOR EXTRA SETUP                        
         BZ    PHDR0020                                                         
*                                                                               
         A     RF,BASERELO         GOTO EXTRA SETUP                             
         BASR  RE,RF                                                            
*                                                                               
PHDR0020 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* FVERHDR - PROCESS VERSION INFO INDICATOR                                      
*.....................................................................          
FVERHDR  DS    0H                                                               
         NI    MISCFLG1,FF-MF1VERCK                                             
         CLC   MHCODE,=Y(FVERHDRQ)                                              
         BNE   *+8                                                              
         OI    MISCFLG1,MF1VERCK   CHECK REQUIRED                               
         B     EXITOK                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* PRCFLD - PROCESS FIELD ELEMENTS TO BUILD REQUEST PARAMETERS                   
*---------------------------------------------------------------------          
PRCFLD   NTR1                                                                   
         L     R6,FPARMS                                                        
         USING MDELD,R6            R6=A(FIELD ENTRY)                            
*                                                                               
         L     RF,AFLDTAB                                                       
         SR    R1,R1                                                            
         ICM   R1,3,MDCODE                                                      
         B     ITER                                                             
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VERSION INFO - VERSION FIELD                                                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
FVERVER  LR    RB,RF                                                            
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         L     RF,FPARMS+4                                                      
         MVC   VERSION,0(RF)        COPY VERSION                                
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* INIT REQUEST - SALESPERSON FIELD                                              
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
INITSAL  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSAL,0(RF)        COPY SALESPERSON                             
         OC    VHPSAL,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* LCON REQUEST - SALESPERSON FIELD                                              
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCSAL    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSAL,0(RF)        COPY SALESPERSON                             
         OC    VHPSAL,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - STATION FIELD                                                  
*.....................................................................          
         USING *,RB                                                             
LCSTA    LR    RB,RF                                                            
         CLC   =Y(LCSALQ),HALF     LAST FIELD THE SALESPERSON?                  
         BNE   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSTA,0(RF)        COPY SALESPERSON                             
         OC    VHPSTA,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - FLIGHT START FIELD                                             
*.....................................................................          
         USING *,RB                                                             
LCFLTST  LR    RB,RF                                                            
**HQ     CLC   =Y(LCSTAQ),HALF     LAST FIELD THE STATION?                      
**HQ     BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLS,0(RF)        COPY FLIGHT START                            
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* LCON REQUEST - FLIGHT END FIELD                                               
*.....................................................................          
         USING *,RB                                                             
LCFLTEN  LR    RB,RF                                                            
*HAN     CLC   =Y(LCFLTSTQ),HALF   LAST FIELD THE FLIGHT START?                 
*HAN     BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLE,0(RF)        COPY FLIGHT END                              
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* LCON REQUEST - AGENCY FIELD                                                   
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCAGY    LR    RB,RF                                                            
*HAN     CLC   =Y(LCFLTENQ),HALF   LAST FIELD THE FLIGHT END?                   
*HAN     BNE   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAGY,0(RF)        COPY AGENCY                                  
         OC    VHPAGY,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - AGENCY OFFICE FIELD                                            
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCAOF    LR    RB,RF                                                            
*HAN     CLC   =Y(LCAGYQ),HALF     LAST FIELD THE AGENCY?                       
*HAN     BNE   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'2'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAOF,0(RF)        COPY AGENCY OFFICE                           
         OC    VHPAOF,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - ADVERTISER FIELD                                               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCADV    LR    RB,RF                                                            
         CLC   =Y(LCFLTENQ),HALF   LAST FIELD THE FLIGHT END?                   
         BE    LCADV002            YES                                          
         CLC   =Y(LCAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BE    LCADV002            YES                                          
*HAN     CLC   =Y(LCAOFQ),HALF     LAST FIELD THE AGENCY OFFICE?                
*HAN     BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
LCADV002 DS    0H                                                               
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPADV,0(RF)        COPY ADVERTISER                              
         OC    VHPADV,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* BCON REQUEST - SEND TIME FILTER                                               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
BCSTIM   LR    RB,RF                                                            
         CLC   FPARMS+8,=F'6'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFFPRD+3(6),0(RF)        COPY TIME                             
         OC    VHPFFPRD+3(6),SPACES                                             
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* BCON REQUEST - SEND DATE FILTER                                               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
BCSDAT   LR    RB,RF                                                            
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFFPRD(3),0(RF)        COPY DATE                               
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* STAWEB REQUEST - SET STATION WEB MODE STATUS                                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
STAWEB   LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG1,MF1STWEB                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*.....................................................................          
* CONTRACT DETAIL IN CONTRACT LIST                                              
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
CONDET   LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,BCDETAL                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*.....................................................................          
* CONTRACT NUMBERS ONLY IN CONTRACT LIST                                        
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
CONONLY  LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,BC#ONLY                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*.....................................................................          
* NO DETAIL CONTRACT DETAIL IN STATION WEB DOWNLOAD                             
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
NODETAIL LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,DCNODET                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*.....................................................................          
* NO ORDER COMMENTS ON CONTRACT                                                 
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
NOORDCOM LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,DCNOCOM                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* RATING DOWLOAD REQUEST - RATING SERVICE FIELD                                 
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
RTRTSRV  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ                                                         
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   RTSRVC,0(RF)        COPY RATING SERVICE                          
         OC    RTSRVC,SPACES                                                    
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - BOOK FIELD(S)                                                
*.....................................................................          
         USING *,RB                                                             
RTBOOK   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(RTBOOKQ)    FIRST BOOK FIELD?                            
         BE    RTBK0010            NO                                           
         CLC   =Y(RTRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF UPGRADES                       
         LA    RE,1(RE)            NUMBER OF UPGRADES GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
RTBK0010 DS    0H                                                               
         MVC   HALF,=Y(RTBOOKQ)                                                 
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VBOOKVAL,DMCB,(RTSRVC,WORK),(1,WORK+16+8),              +        
               (C'B',VSCANNER),FULL                                             
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    EBADBOOK            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVI   3(RE),C'I'          SET DEMO FILE(INV IS DEFAULT)                
         CLC   MDCODE,=Y(RTTPBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(RTT4BKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(RTPVBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         MVC   4(1,RE),FULL        SET BOOK TYPE                                
         MVI   5(RE),0             MAKE IT NEW                                  
*                                                                               
         LA    RE,6(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - UPGRADE FIELD(S)                             
*.....................................................................          
         USING *,RB                                                             
RTUPG    LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(RTUPGQ)     FIRST UPGRADE FIELD?                         
         BE    RTUPG010            NO                                           
         CLC   =Y(RTBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    RTUPG004            YES                                          
         CLC   =Y(RTRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0                 - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
RTUPG004 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF UPGRADES                       
         LA    RE,1(RE)            NUMBER OF UPGRADES GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
RTUPG010 DS    0H                                                               
         MVC   HALF,=Y(RTUPGQ)                                                  
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,79+8           79 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         MVC   WORK+8(4),=C'UPT='                                               
         LA    R0,4(RE)                                                         
         STC   R0,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8+4(0),0(RF)                                                
*                                                                               
         MVC   WORK+100(4),VBOOKVAL                                             
         MVC   WORK+104(4),ACOMFACS                                             
         MVC   WORK+108(4),VUPVAL                                               
         GOTO1 =V(REPRPUPV),DMCB,WORK,WORK+120,WORK+100,RR=Y                    
         BE    *+14                                                             
         MVC   ERROR,WORK+120                                                   
         B     EXITL                                                            
*                                                                               
         OC    WORK+120+15(3),WORK+120+15                                       
         BZ    EINVUPG             REQUIRES A SHARE BOOK                        
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
*                                                                               
UE       USING RAVLNEL,WORK+120                                                 
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BNZ   EINVUPG             YES                                          
*                                                                               
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(RTT4UPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(RTPVUPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         MVC   UE.RAVLNBT,WORK+120+14      SPECIAL BOOK TYPE                    
         DROP  UE                                                               
*                                                                               
         MVC   0(3,RE),WORK+120+15         1ST BASE BOOKS                       
         MVC   4(1,RE),WORK+120+14         SPECIAL BOOK TYPE                    
         MVC   5(2,RE),WORK+120+18+1       2ND BASE BOOK(YM ONLY)               
         MVC   7(2,RE),WORK+120+21+1       3RD                                  
         MVC   9(2,RE),WORK+120+24+1       4TH                                  
         MVC   11(14,RE),WORK+120          UPGRADE ELEMENT                      
         MVI   11+14(RE),0                 MAKE IT NEW                          
         LA    RE,11+14+1(RE)                                                   
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - DEMO FIELD                                                   
*.....................................................................          
         USING *,RB                                                             
RTDEMO   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(RTDEMOQ)    FIRST DEMO FIELD?                            
         BE    RTDEM010            NO                                           
*                                                                               
         CLC   =Y(RTUPGQ),HALF     LAST FIELD AN UPGRADE?                       
         BE    RTDEM002            YES                                          
         CLC   =Y(RTBOOKQ),HALF    LAST FIELD A BOOK?                           
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO UPGRADES                        
         LA    RE,1(RE)                                                         
RTDEM002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
RTDEM010 DS    0H                                                               
         MVC   HALF,=Y(RTDEMOQ)                                                 
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8             16 BYTE FIELD                              
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         L     RE,AIO1                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 VDEMOVAL,DMCB,(1,WORK),(1,WORK+16+8),(0,AIO1),0                  
*                                                                               
         CLI   4(R1),0             GOOD DEMO?                                   
         BE    EBADDEMO            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         MVI   3(RE),0             MAKE IT NEW                                  
         LA    RE,4(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWNOAD - STATION FIELD                                                
*.....................................................................          
         USING *,RB                                                             
RTSTA    LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(RTETIMQ),HALF    LAST FIELD AN END TIME?                      
         BE    RTSTA012            YES                                          
         CLC   =Y(RTDATEQ),HALF    LAST FIELD A EFFECTIVE DATE?                 
         BE    RTSTA012            YES                                          
         CLC   =Y(RTSTIMQ),HALF    LAST FIELD A START TIME?                     
         BE    RTSTA010            YES                                          
         CLC   =Y(RTDEMOQ),HALF    LAST FIELD A DEMO?                           
         BNE   EPARMSEQ                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR             STATION GOES HERE                            
         B     RTSTA020                                                         
*                                                                               
RTSTA010 DS    0H                  SET MISSING END TIME TO ZERO                 
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         XC    0(2,RE),0(RE)                                                    
         MVI   2(RE),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    RE,3(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
RTSTA012 DS    0H                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVI   0(RE),0             SET END OF PREVIOUS STATION                  
         LA    RE,1(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
RTSTA020 DS    0H                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RF,5(RE)            BUMP TO NEXT ENTRY                           
         LR    R1,RE                                                            
         S     RF,ATWA             STORE NEW ENTRY                              
         ST    RF,ADDR                                                          
*                                                                               
         CHI   RF,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - DAY FIELD                                                    
*.....................................................................          
         USING *,RB                                                             
RTDAY    LR    RB,RF                                                            
         CLC   =Y(RTXTIMQ),HALF    LAST FIELD AN ORBIT END TIME?                
         BE    RTDAY012            YES                                          
         CLC   =Y(RTETIMQ),HALF    LAST FIELD AN END TIME?                      
         BE    RTDAY012            YES                                          
         CLC   =Y(RTDATEQ),HALF    LAST FIELD A EFFECTIVE DATE?                 
         BE    RTDAY012            YES                                          
         CLC   =Y(RTSTIMQ),HALF    LAST FIELD A START TIME?                     
         BE    RTDAY010            YES                                          
         CLC   =Y(RTSTAQ),HALF     LAST FIELD A STATION?                        
         BE    RTDAY012            YES                                          
         B     EPARMSEQ                                                         
*                                                                               
RTDAY010 DS    0H                  SET MISSING END TIME TO ZERO                 
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         XC    0(2,RE),0(RE)                                                    
         MVI   2(RE),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    RE,3(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
RTDAY012 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(1,RE),0(RF)       COPY DAY                                     
         LA    RE,1(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - START TIME FIELD                                             
*.....................................................................          
         USING *,RB                                                             
RTSTIM   LR    RB,RF                                                            
         CLC   =Y(RTDAYQ),HALF     LAST FIELD A STATION?                        
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(2,RE),0(RF)       COPY START TIME                              
         LA    RE,2(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - END TIME FIELD                                               
*.....................................................................          
         USING *,RB                                                             
RTETIM   LR    RB,RF                                                            
         CLC   =Y(RTSTIMQ),HALF     LAST FIELD THE START TIME?                  
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(2,RE),0(RF)       COPY END TIME                                
         LA    RE,2(RE)                                                         
*                                                                               
         CLC   HALF,=Y(RTETIMQ)    LAST END TIME?                               
         BNE   *+12                                                             
         MVI   0(RE),X'FF'         YES ADD MARKER                               
         LA    RE,1(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
*  RATING DOWLOAD - EFFECTIVE DATE FIELD (REPLACES END TIME)                    
*.....................................................................          
         USING *,RB                                                             
RTDATE   LR    RB,RF                                                            
         CLC   =Y(RTSTIMQ),HALF     LAST FIELD THE START TIME?                  
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     R2,ADDR                                                          
         A     R2,ATWA                                                          
         L     R3,FPARMS+4                                                      
         MVI   0(R2),X'FF'         SET END TIME IS DATE                         
         GOTO1 VDATCON,DMCB,(8,0(R3)),(2,1(R2))                                 
         MVI   3(R2),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    R2,4(R2)                                                         
*                                                                               
         S     R2,ATWA                                                          
         ST    R2,ADDR                                                          
*                                                                               
         CHI   R2,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
*  RATING DOWLOAD - END OF REQUEST MARKER                                       
*.....................................................................          
         USING *,RB                                                             
RTEMRK   LR    RB,RF                                                            
         CLC   =Y(RTSTIMQ),HALF    LAST FIELD A START TIME?                     
         BNE   RTEMRKX             NO                                           
*                                                                               
         L     RE,ADDR             SET MISSING END TIME TO ZERO                 
         A     RE,ATWA                                                          
         XC    0(2,RE),0(RE)                                                    
         MVI   2(RE),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    RE,3(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
RTEMRKX  DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADER REQUEST - STATION FIELD                                       
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
VHSTA    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSTA,0(RF)        COPY STATION CALL LETTERS                    
         OC    VHPSTA,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - ADVERTISER FIELD                                    
*.....................................................................          
         USING *,RB                                                             
VHADV    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPADV,0(RF)        COPY ADVERTISER                              
         OC    VHPADV,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - PRODUCT FIELD                                       
*.....................................................................          
         USING *,RB                                                             
VHPRD    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPPRD,0(RF)        COPY PRODUCT                                 
         OC    VHPPRD,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - AGENCY FIELD                                        
*.....................................................................          
         USING *,RB                                                             
VHAGY    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAGY,0(RF)        COPY AGENCY                                  
         OC    VHPAGY,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - AGENCY OFFICE FIELD                                 
*.....................................................................          
         USING *,RB                                                             
VHAOF    LR    RB,RF                                                            
         CLC   =Y(VHAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BNE   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'2'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAOF,0(RF)        COPY AGENCY OFFICE                           
         OC    VHPAOF,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - FLIGHT START FIELD                                  
*.....................................................................          
         USING *,RB                                                             
VHFLS    LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLS,0(RF)        COPY FLIGHT START                            
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - FLIGHT END FIELD                                    
*.....................................................................          
         USING *,RB                                                             
VHFLE    LR    RB,RF                                                            
         CLC   =Y(VHFLSQ),HALF     LAST FIELD THE FLIGHT END?                   
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLE,0(RF)        COPY FLIGHT END                              
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - CONTRACT TYPE FIELD (OPTIONAL)                      
*.....................................................................          
         USING *,RB                                                             
VHCTYP   LR    RB,RF                                                            
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPCTY,0(RF)        COPY CONTRACT TYPE                           
         OC    VHPCTY,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - DEV CONTRACT TYPE FIELD (OPTIONAL)                  
*.....................................................................          
         USING *,RB                                                             
VHDCT    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'2'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPDCT,0(RF)        COPY DEV CONTRACT TYPE                       
         DROP  RE                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE HEADER REQUEST - DEV CONTRACT TYPE FIELD (OPTIONAL)                  
*.....................................................................          
         USING *,RB                                                             
VHDSP    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPDSP,0(RF)        COPY DEV SALEPERSON                          
         DROP  RE                                                               
         B     EXITOK                                                           
         SPACE 3                                                                
*.....................................................................          
* VALIDATE HEADER REQUEST - SALESPERSON FIELD (OPTIONAL)                        
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
VHSAL    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSAL,0(RF)        COPY SALESPERSON                             
         OC    VHPSAL,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - RATING SERVICE FIELD                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
VBRTSRV  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   RTSRVC,0(RF)        COPY RATING SERVICE                          
         OC    RTSRVC,SPACES                                                    
*                                                                               
         L     R1,ADDR             SET POSITION FOR VALIDATED DATA              
         AHI   R1,VHPARMLQ                                                      
         ST    R1,ADDR2                                                         
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - BOOK FIELD(S)                         
*.....................................................................          
         USING *,RB                                                             
VBBOOK   LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG2,VHBKS                                                   
*                                                                               
         MVC   HALF,=Y(VBBOOKQ)                                                 
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VBOOKVAL,DMCB,(RTSRVC,WORK),(1,WORK+16+8),              +        
               (C'B',VSCANNER),FULL                                             
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    EBADBOOK            NO                                           
*                                                                               
         XC    WORK+60(20),WORK+60                                              
         MVC   WORK+60(2),=X'0B07'      PUT OUT BOOK TYPE                       
         MVC   WORK+60+2(1),FULL                                                
         GOTOX VUNBOOK,DMCB,(1,WORK+16+8),WORK,(C'L',WORK+60),         +        
               (C'+',=CL6' ')                                                   
*                                                                               
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         LA    RE,WORK(RE)                                                      
         CLI   0(RE),C' '          REMOVE SPACES                                
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   VBBK0020                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     VBBK0020                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
VBBK0020 DS    0H                                                               
         L     R1,ADDR2            COPY UNBOOKED NAME                           
         A     R1,ATWA                                                          
         MVC   0(2,R1),HALF        SAY ITS A BOOK                               
         TM    WORK+16+8,X'2E'     CHECK FOR E/P/S/T BOOKS                      
         BZ    *+10                                                             
         MVC   0(2,R1),=Y(VBINBKQ) SAY ITS AN INVENTORY BOOK                    
*                                                                               
         LA    RF,WORK+8                                                        
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),WORK+8      MOVE THE TEXT                                
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - UPGRADE FIELD                         
*.....................................................................          
         USING *,RB                                                             
VBUPGRD  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG2,VHBKS                                                   
*                                                                               
         MVC   HALF,=Y(VBUPGRDQ)                                                
*                                                                               
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,79+8           79 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         MVC   WORK+8(4),=C'UPT='                                               
         LA    R0,4(RE)                                                         
         STC   R0,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8+4(0),0(RF)                                                
*                                                                               
         MVC   WORK+100(4),VBOOKVAL                                             
         MVC   WORK+104(4),ACOMFACS                                             
         MVC   WORK+108(4),VUPVAL                                               
         GOTO1 =V(REPRPUPV),DMCB,WORK,WORK+120,WORK+100,RR=Y                    
         BE    *+14                                                             
         MVC   ERROR,WORK+120                                                   
         B     EXITL                                                            
*                                                                               
         OC    WORK+120+15(3),WORK+120+15                                       
         BZ    EINVUPG             REQUIRES A SHARE BOOK                        
*                                                                               
         L     R1,ADDR2                                                         
         A     R1,ATWA                                                          
*                                                                               
UE       USING RAVLNEL,WORK+120                                                 
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BNZ   EINVUPG             YES                                          
*                                                                               
         MVC   0(2,R1),=Y(VBNIUPGQ)   SAY ITS A NON INVENOTRY UPGRADE           
         DROP  UE                                                               
*                                                                               
VBUPG020 DS    0H                                                               
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),0(RF)                                                    
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - DEMO FIELD                            
*.....................................................................          
         USING *,RB                                                             
VBDEMO   LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG2,VHBKS                                                   
*                                                                               
         MVC   HALF,MDCODE                                                      
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8             16 BYTE FIELD                              
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         L     RE,AIO1                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 VDEMOVAL,DMCB,(1,WORK),(1,WORK+16+8),(0,AIO1),0                  
*                                                                               
         CLI   4(R1),0             GOOD DEMO?                                   
         BE    EBADDEMO            NO                                           
*                                                                               
         L     RE,AIO1                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),WORK+16+8                                             
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO1)                      
*                                                                               
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
*                                                                               
         L     R1,ADDR2            COPY UNDEMOED NAME                           
         A     R1,ATWA                                                          
         MVC   0(2,R1),HALF        SAY ITS A DEMO                               
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),WORK        MOVE THE TEXT                                
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - UPGRADE NAME                          
*.....................................................................          
         USING *,RB                                                             
VBUPGNM  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG2,VHBKS                                                   
*                                                                               
         MVC   HALF,=Y(VBUPGNMQ)                                                
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VBOOKVAL,DMCB,(RTSRVC,WORK),(1,WORK+16+8),              +        
               (C'B',VSCANNER),FULL                                             
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BNE   EBADUPNM            YES - BAD UPGRADE NAME                       
*                                                                               
         L     R1,ADDR2            COPY UNBOOKED NAME                           
         A     R1,ATWA                                                          
         MVC   0(2,R1),=Y(VBUPGNMQ) SAY ITS AN UPGRADE NAME                     
*                                                                               
         ZIC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),WORK+8      MOVE THE TEXT                                
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* UPDATE CONTRACT HEADER FIELDS REQUEST - CONTRACT NUMBER FIELD                 
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
UCONCON  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),0(5,RF)                                               
         MVO   WORK(5),WORK+10(5) CONTRACT NUM IN 9'S COMPLEMENT                
*                                                                               
         LA    R8,KEY                                                           
         XC    KEY,KEY                                                          
         USING RCONPTYP,R8                                                      
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,REPALPHA                                                
         MVC   RCONPCON,WORK                                                    
         DROP  R8                                                               
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   ERROR,=Y(82)                                                     
         B     EXITL                                                            
*                                                                               
         MVI   UPDATE,C'N'                                                      
         GOTO1 VGETREC,DMCB,AIO2                                                
*                                                                               
         MVI   ELCODE,X'17'        GET COMBO ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   *+14                NOT FOUND - OK TO CONTINUE                   
         MVC   ERROR,=Y(920)                                                    
         B     EXITL                                                            
*                                                                               
         MVI   MISCFLG2,0           INITIALIZE FLAGS FOR THIS K                 
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* UPDATE CONTRACT HEADER FIELDS REQUEST - ACTION CODES                          
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
UCONACT  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
*                                                                               
         L     RF,FPARMS+4                                                      
*                                                                               
         CLI   0(RF),C'S'                                                       
         BNE   *+8                                                              
         OI    MISCFLG2,UCFSEND                                                 
*                                                                               
         CLI   0(RF),C'C'                                                       
         BNE   *+8                                                              
         OI    MISCFLG2,UCFCF                                                   
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* UPDATE CONTRACT HEADER FIELDS REQUEST - VERIFY VERSION NUMBER                 
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
UCONVER  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
*                                                                               
         L     R8,AIO2             CONTRACT                                     
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   UCONVERR                                                         
         USING RCONSEND,R8                                                      
         LA    R2,RCONSRV                                                       
         CLC   RCONSRV,RCONSSV                                                  
         BNL   *+8                                                              
         LA    R2,RCONSSV                                                       
         DROP  R8                                                               
*                                                                               
         L     R8,AIOREC           SAVE AIOREC                                  
         MVC   AIOREC,AIO2         POINT TO CONTRACT                            
         GOTO1 =A(CKSIDE),RR=Y     CHECK REP/STATION SIDE                       
         ST    R8,AIOREC           RESTORE AIOREC                               
*                                                                               
         CLI   BYTE,C'R'                                                        
         BNE   UCONVER2                                                         
*                                                                               
         MVC   ERROR,=Y(168)                                                    
         B     EXITL                                                            
*                                                                               
UCONVER2 L     RF,FPARMS+4                                                      
         CLC   0(1,RF),0(R2)       CHECK VS HIGHES VER #                        
         BE    EXITOK              MATCH - OK                                   
*                                                                               
UCONVERR DS    0H                  FAILED MATCH                                 
         MVC   ERROR,=Y(184)                                                    
         B     EXITL                                                            
*.....................................................................          
* UPDATE CONTRACT HEADER FIELDS REQUEST - ORDER COMMENT LINES                   
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
UCONOCM  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R8,AIO2                                                          
         USING RCONREC,R8                                                       
*                                                                               
         TM    MISCFLG2,UCFOCM                                                  
         BO    UCOCM010                                                         
         OI    MISCFLG2,UCFOCM+UCFVER                                           
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'92',RCONREC),0,0               
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'82',RCONREC),0,0               
*                                                                               
UCOCM010 DS    0H                                                               
         L     RF,FPARMS+4         OCM TEXT                                     
         MVI   WORK,X'92'                                                       
         ICM   RE,15,FPARMS+8      OCM LEN                                      
         BZ    UCOCM020                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   WORK+2(0),0(RF)                                                  
         LA    RE,1(RE)                                                         
UCOCM020 LA    RE,2(RE)                                                         
         STC   RE,WORK+1                                                        
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,=C'ADD=CODE'         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXITOK                                                           
         DROP  R8                                                               
*.....................................................................          
* UPDATE CONTRACT HEADER FIELDS REQUEST - BUY LINE NUMBER                       
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
UCONLIN  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
*                                                                               
         LA    R8,KEY                                                           
         USING RBUYREC,R8                                                       
         L     R2,AIO2                                                          
         USING RCONREC,R2                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(3,RBUYKCON)               
         DROP  R2                                                               
         MVC   RBUYKPLN,=X'FFFFFF'                                              
         L     RF,FPARMS+4         LINE #                                       
         MVC   RBUYKMLN,0(RF)                                                   
         MVC   RBUYKLIN,0(RF)                                                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   ERROR,=Y(90)                                                     
         B     EXITL                                                            
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO1                                                
*                                                                               
         NI    MISCFLG2,X'FF'-UCFBOCM                                           
*                                                                               
         B     EXITOK                                                           
         DROP  R8                                                               
*.....................................................................          
* UPDATE CONTRACT HEADER FIELDS REQUEST - BUY ORD COMMENT LINE                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
UCONBOC  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R8,AIO1                                                          
         USING RBUYREC,R8                                                       
*                                                                               
         TM    MISCFLG2,UCFBOCM                                                 
         BO    UCBOC010                                                         
         OI    MISCFLG2,UCFBOCM+UCFVER                                          
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'84',RBUYREC),0,0               
*                                                                               
UCBOC010 DS    0H                                                               
         L     RF,FPARMS+4         BOC TEXT                                     
         MVI   WORK,X'84'                                                       
         MVI   WORK+2,0                                                         
         ICM   RE,15,FPARMS+8      BOC LEN                                      
         BZ    UCBOC020                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   WORK+3(0),0(RF)                                                  
         LA    RE,1(RE)                                                         
UCBOC020 LA    RE,3(RE)                                                         
         STC   RE,WORK+1                                                        
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,WORK,=C'ADD=CODE'         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXITOK                                                           
         DROP  R8                                                               
*.....................................................................          
* UPDATE CONTRACT HEADER FIELDS REQUEST - BUY END OF BUY MARKER                 
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
UCONEOB  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
*                                                                               
         L     R2,AIO1                                                          
         USING RBUYREC,R2                                                       
         L     R8,AIO2                                                          
         USING RCONREC,R8                                                       
*                                                                               
         TM    MISCFLG2,UCFBOCM                                                 
         BZ    UCEOB050                                                         
         MVC   RBUYCHGI,=C'O '                                                  
*                                                                               
UCEOB050 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,RBUYCHGD) BUY LAST CHGD DATE               
*                                                                               
         L     R8,AIO2                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R8                                                      
         ZIC   RE,RCONSSV     STATION VERSION                                   
         TM    RCONSENF,X'10' STA VERSION NOT ADVANCED                          
         BZ    UCEOB060                                                         
         LA    RE,2(RE)       STA VER WILL BE ADV LATER IN TRANSACTION          
UCEOB060 DS    0H                                                               
         STC   RE,RBUYVER                                                       
         DROP  R8                                                               
*                                                                               
UCEOB100 DS    0H                                                               
         GOTO1 VPUTREC,DMCB,AIO1   WRITE BUYREC                                 
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - CONTRACT NUMBER FIELD               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONCON  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         OC    HALF,HALF           FIRST CONTRACT?                              
         BNZ   DCCON010            NO                                           
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF CONTRACT                       
         LA    RE,2(RE)            NUMBER OF CONTRACTS GOES HERE                
         ST    RE,ADDR                                                          
*                                                                               
DCCON010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(L'VHPCON,RE),0(RF)       SAVE CONTRACT #                       
         LA    RE,L'VHPCON(RE)            BUMP TO NEXT ENTRY                    
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-300)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          NUMBER OF CONTRACTS                          
         LA    RF,1(RF)                                                         
         STCM  RF,3,0(RE)            UPDATE NUMBER OF ENTRIES                   
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - DOWNLOAD BUYS FLAG                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONBUY  LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,DCBUYS                                                  
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - POST START DATE                     
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONPSD  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R2,FPARMS+4                                                      
         GOTO1 VDATCON,DMCB,(8,0(R2)),(0,WORK)                                  
*                                  ORIGINAL START DATE -> EBCDIC                
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BE    DCPSD010                                                         
*                                                                               
         ZIC   R0,DMCB             MAKE IT A MONDAY                             
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
*                                                                               
DCPSD010 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,POSTST)                                 
         B     EXITOK                                                           
*.....................................................................          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - POST END DATE                       
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONPED  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R2,FPARMS+4                                                      
         GOTO1 VDATCON,DMCB,(8,0(R2)),(0,WORK)                                  
*                                  ORIGINAL END DATE -> EBCDIC                  
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,7              ORIGINAL END = SUNDAY?                       
         BE    DCPED010                                                         
*                                                                               
         ZIC   R0,DMCB             MAKE IT A SUNDAY                             
         LNR   R0,R0                                                            
         AHI   R0,7                                                             
*                                                                               
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
*                                                                               
DCPED010 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,POSTEN)                                 
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* INITIAL DOWNLOAD                                                              
**********************************************************************          
INITDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ATWA                                                          
         USING TWAD,R4                                                          
*                                                                               
         XC    KEY,KEY             READ STATION SIGN ON PASSIVE KEY             
K        USING RST9KEY,KEY                                                      
         MVI   K.RST9KTYP,X'83'    SET RECORD ID                                
         MVI   K.RST9KSTP,X'07'    SUB ID                                       
         MVC   K.RST9KREP,REPALPHA                                              
         MVC   K.RST9KSIG,TWAUSRID SIGN ON USER ID                              
         DROP  R4                                                               
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,STADATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,STAREPEL,REPALPHA,0                         
*                                                                               
         GOTO1 VHIGH                                                            
IDWN0010 DS    0H                                                               
         CLC   KEY(RST9KSIG-RST9KEY),KEYSAVE                                    
         BNE   IDWN0020                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,STASTAEL,K.RST9KSTA,0                       
*                                                                               
         GOTO1 VSEQ                                                             
         B     IDWN0010                                                         
         DROP  K                                                                
*                                                                               
IDWN0020 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,STAMASEL,INVSEQ,0                           
*                                                                               
* UNWIRED/NETWORK CONTYPES                                                      
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,CTYDATA,0                                   
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,=C'N',1                            
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,=C'X',1                            
*                                                                               
         CLC   =C'K3',INVSEQ                                                    
         BNE   IDWN0030                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,=C'D',1                            
*                                                                               
IDWN0030 DS    0H                                                               
         CLI   INVSEQ+2,0          PROFILE CONTYPE                              
         BE    IDWN0032                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,INVSEQ+2,1                         
*                                                                               
IDWN0032 DS    0H                                                               
*------------------------------------------------------------------             
* HARDCODED DDS MF->PC PROFILES                                                 
*                                                                               
* BYTE 0:                                                                       
*         X'80'        SEND EMAIL TO POINT PERSON FOR NETWORK                   
* BYTE 1:                                                                       
*                                                                               
* BYTE 2:                                                                       
*                                                                               
* BYTE 3:                                                                       
*                                                                               
*------------------------------------------------------------------             
         GOTO1 ASETELEM,DMCB,AFABLK,PRFDATA,0                                   
*                                                                               
         XC    FULL,FULL                                                        
         CLC   =C'K3',INVSEQ                                                    
         BNE   *+8                                                              
         OI    FULL+0,X'80'                                                     
*                                                                               
         CLC   =C'K9',INVSEQ                                                    
         BNE   *+8                                                              
         OI    FULL+0,X'80'                                                     
*                                                                               
         CLC   =C'B3',INVSEQ                                                    
         BNE   *+8                                                              
         OI    FULL+0,X'80'                                                     
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,PRFWCF1L,FULL,0                             
*                                                                               
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* LIST CONTRACT DOWNLOAD                                                        
**********************************************************************          
LCONDWN  NTR1  BASE=*,LABEL=*                                                   
         LH    R5,=Y(SVPARMBF-T83AFFD)                                          
         A     R5,ATWA                                                          
         USING VHPARMD,R5                                                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCON8ETP,KEY                                                     
KS       USING RCON8ETP,KEYSAVE                                                 
         MVI   K.RCON8ETP,X'8E'                                                 
         MVC   K.RCON8ERP,REPALPHA                                              
         MVC   K.RCON8EST,VHPSTA                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(8,VHPFLS),(2,FULL)                                 
         GOTO1 VDATCON,DMCB,(8,VHPFLE),(2,FULL+2)                               
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
LCD0010  DS    0H                                                               
         CLC   KEY(RCON8EFS-RCON8ETP),KEYSAVE                                   
         BNE   LCDX                                                             
*                                                                               
         CLI   K.RCON8EID,1                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    VHPAGY,VHPAGY       FILTER AGENCY?                               
         BZ    LCD0014                                                          
*                                                                               
         CLC   VHPAGY,K.RCON8EAG                                                
         BNE   LCD0100                                                          
*                                                                               
         OC    VHPAOF,VHPAOF       FILTER AGENCY OFFICE?                        
         BZ    LCD0014                                                          
*                                                                               
         CLC   VHPAOF,K.RCON8EAG+4                                              
         BNE   LCD0100                                                          
*                                                                               
LCD0014  DS    0H                                                               
         OC    VHPADV,VHPADV       FILTER AGENCY?                               
         BZ    LCD0016                                                          
*                                                                               
         CLC   VHPADV,K.RCON8EAV                                                
         BNE   LCD0100                                                          
*                                                                               
LCD0016  DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLI   K.RCON8EID,2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LCD0020  DS    0H                                                               
         CLC   K.RCON8EFE,FULL     CHECK FOR FLIGHT OVERLAP                     
         BL    LCD0100                                                          
         CLC   K.RCON8EFS,FULL+2                                                
         BH    LCD0100                                                          
*                                                                               
         CLC   K.RCON8ESP,VHPSAL                                                
         BNE   LCD0100                                                          
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
         MVI   ELCODE,X'03'        SEE IF THERE IS MONEY                        
         BAS   RE,GETEL                                                         
         BNE   LCD0100                                                          
*                                                                               
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,WORK,0                             
*                                                                               
         TM    MISCFLG2,BC#ONLY    ONLY WANT CON #                              
         BNZ   LCD0050                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,WORK)                            
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,RCONBUYR,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONAGYEL,RCONKAGY,0                         
         CLC   RCONKAOF,SPACES                                                  
         BNH   LCD0022                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONAOFEL,RCONKAOF,0                         
*                                                                               
LCD0022  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONADVEL,RCONKADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTAEL,RCONKSTA,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSALEL,RCONSAL,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONCTYEL,RCONTYPE,0                         
*                                                                               
         CLC   RCONPRD,SPACES                                                   
         BNH   LCD0030                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONPRD,L'RCONPRD                  
         B     LCD0032                                                          
*                                                                               
LCD0030  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LCD0032                                                          
*                                                                               
         USING RCONEXEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONEXPR,L'RCONEXPR                
         DROP  R8                                                               
*                                                                               
LCD0032  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'18'        DEVELOPMENT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   LCD0040                                                          
*                                                                               
         USING RCONDVEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONDCTEL,RCONDVCT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDSPEL,RCONDVSP,0                         
         DROP  R8                                                               
*                                                                               
LCD0040  DS    0H                                                               
         TM    MISCFLG1,MF1STWEB   DOWNLOAD SEND/CF STATUS?                     
         BZ    LCD0045             NO                                           
*                                                                               
         GOTO1 =A(CKSIDE),RR=Y     CHECK REP/STATION SIDE                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONSIDEL,BYTE,0                             
*                                                                               
         GOTO1 =A(CKSEND),RR=Y     GET REP/STATION LAST SENT/CF                 
*                                                                               
         GOTO1 =A(CKVER),RR=Y      GET LAST VERSION                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONVEREL,BYTE,0                             
*                                                                               
         GOTO1 =A(CKMOD),RR=Y      GET LAST MOD                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONMODEL,BYTE,0                             
*                                                                               
         GOTO1 =A(CKWIP),RR=Y      GET WIP STATUS                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONWIPEL,BYTE,0                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONREPEL,REPALPHA,0                         
*                                                                               
LCD0045  DS    0H                                                               
         MVI   K.RCON8EID,X'03'    READ DEMO KEY                                
         XC    K.RCON8EAG(L'KEY-(RCON8EAG-RCON8ETP)),K.RCON8EAG                 
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(RCON8EAG-RCON8ETP),KEYSAVE                                   
         BNE   LCD0050                                                          
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),K.RCON8EMO                                            
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
LCD0050  DS    0H                                                               
         MVC   KEY,KEYSAVE         RESET KEY                                    
*                                                                               
LCD0100  DS    0H                                                               
*                                                                               
         MVI   K.RCON8EID,X'FF'    READ NEXT RECORD SET                         
         XC    K.RCON8EAG(L'KEY-(RCON8EAG-RCON8ETP)),K.RCON8EAG                 
         GOTO1 VHIGH                                                            
         B     LCD0010                                                          
*                                                                               
LCDX     DS    0H                                                               
         B     EXITOK                                                           
         DROP  K,KS,R5                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* LIST CONTRACT DOWNLOAD FOR STATION WEB REQUESTS                               
**********************************************************************          
BCONDWN  NTR1  BASE=*,LABEL=*                                                   
         LH    R5,=Y(SVPARMBF-T83AFFD)                                          
         A     R5,ATWA                                                          
         USING VHPARMD,R5                                                       
*                                                                               
         XC    VHPSTA,VHPSTA                                                    
*                                                                               
BCD0005  DS    0H                                                               
         XC    KEY,KEY             READ STATION SIGN ON PASSIVE KEY             
K        USING RST9KEY,KEY                                                      
         MVI   K.RST9KTYP,X'83'    SET RECORD ID                                
         MVI   K.RST9KSTP,X'07'    SUB ID                                       
         MVC   K.RST9KREP,REPALPHA                                              
*                                                                               
         L     RE,ATWA             SIGN ON USER ID                              
         AHI   RE,TWAUSRID-TWAD                                                 
         MVC   K.RST9KSIG,0(RE)                                                 
*                                                                               
         MVC   K.RST9KSTA,VHPSTA   NEXT STATION, ADD BINARY 1                   
         ZIC   R0,K.RST9KSTA+4                                                  
         AHI   R0,1                                                             
         STC   R0,K.RST9KSTA+4                                                  
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(RST9KSTA-RST9KEY),KEYSAVE                                    
         BNE   BCDX                                                             
*                                                                               
         MVC   VHPSTA,K.RST9KSTA   SAVE THE STATION CALL LTR                    
         DROP  K                                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCON8ETP,KEY                                                     
KS       USING RCON8ETP,KEYSAVE                                                 
         MVI   K.RCON8ETP,X'8E'                                                 
         MVC   K.RCON8ERP,REPALPHA                                              
         MVC   K.RCON8EST,VHPSTA                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(8,VHPFLS),(2,FULL)                                 
         GOTO1 VDATCON,DMCB,(8,VHPFLE),(2,FULL+2)                               
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
BCD0010  DS    0H                                                               
         CLC   KEY(RCON8EFS-RCON8ETP),KEYSAVE                                   
         BNE   BCD0005                                                          
*                                                                               
         CLI   K.RCON8EID,1                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    VHPAGY,VHPAGY       FILTER AGENCY?                               
         BZ    BCD0014                                                          
*                                                                               
         CLC   VHPAGY,K.RCON8EAG                                                
         BNE   BCD0100                                                          
*                                                                               
         OC    VHPAOF,VHPAOF       FILTER AGENCY OFFICE?                        
         BZ    BCD0014                                                          
*                                                                               
         CLC   VHPAOF,K.RCON8EAG+4                                              
         BNE   BCD0100                                                          
*                                                                               
BCD0014  DS    0H                                                               
         OC    VHPADV,VHPADV       FILTER ADVERTISER?                           
         BZ    BCD0016                                                          
*                                                                               
         CLC   VHPADV,K.RCON8EAV                                                
         BNE   BCD0100                                                          
*                                                                               
BCD0016  DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLI   K.RCON8EID,2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BCD0020  DS    0H                                                               
         CLC   K.RCON8EFE,FULL     CHECK FOR FLIGHT OVERLAP                     
         BL    BCD0100                                                          
         CLC   K.RCON8EFS,FULL+2                                                
         BH    BCD0100                                                          
*                                                                               
*        CLC   K.RCON8ESP,VHPSAL   NO SALESPERSON FOR WEB REQ                   
*        BNE   BCD0100                                                          
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'17'        GET COMBO ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    BCD0100             DON'T LIST COMBOS                            
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'12'        GET SAR   ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   BCD0024             NOT FOUND                                    
         USING RSARXEL,R8          CHECK FOR FORECAST ORDER                     
         TM    RSARXFLG,X'08'      FORECAST FLAG SET?                           
         BO    BCD0100             YES - DON'T SHOW FORECAST ORDERS             
         DROP  R8                                                               
*                                                                               
BCD0024  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'20'        SEE IF THE ORDER IS NOT SENT                 
         BAS   RE,GETEL                                                         
         BNE   BCD0100                                                          
*                                                                               
         USING RCONSEND,R8         CHECK FIRST REP SEND DATE                    
         OC    RCONSRDF,RCONSRDF                                                
         BZ    BCD0100                                                          
*                                                                               
         OC    VHPFFPRD(3),VHPFFPRD                                             
         BZ    BCD0028                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(8,VHPFFPRD),(2,FULL)                               
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BZ    BCD0026             NO                                           
*                                                                               
         CLC   RCONSRDT,FULL       CHECK DATE                                   
         BL    BCD0100                                                          
         BH    BCD0028                                                          
*                                                                               
         CLC   RCONSRTI,VHPFFPRD+3 CHECK TIME                                   
         BL    BCD0100                                                          
         B     BCD0028                                                          
*                                                                               
BCD0026  DS    0H                                                               
         TM    RCONSENF,X'40'      LAST SENT BY STA?                            
         BZ    BCD0100             NO                                           
*                                                                               
         CLC   RCONSSDT,FULL       CHECK DATE                                   
         BL    BCD0100                                                          
         BH    BCD0028                                                          
*                                                                               
         CLC   RCONSSTI,VHPFFPRD+3 CHECK TIME                                   
         BL    BCD0100                                                          
         B     BCD0028                                                          
*                                                                               
         DROP  R8                                                               
*                                                                               
BCD0028  DS    0H                                                               
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,WORK)                              
*                                                                               
         TM    MISCFLG2,BC#ONLY    ONLY WANT CON #                              
         BNZ   BCD0050                                                          
*                                                                               
         TM    MISCFLG2,BCDETAL    WANT CONTRACT DETAIL?                        
         BZ    BCD0030             NO                                           
*                                                                               
         BAS   RE,BCONDET                                                       
         BL    EXITL                                                            
         B     BCD0050                                                          
*                                                                               
BCD0030  DS    0H                                                               
         BAS   RE,BCONLST                                                       
*                                                                               
BCD0050  DS    0H                                                               
         MVC   KEY,KEYSAVE         RESET KEY                                    
*                                                                               
BCD0100  DS    0H                                                               
*                                                                               
         MVI   K.RCON8EID,X'FF'    READ NEXT RECORD SET                         
         XC    K.RCON8EAG(L'KEY-(RCON8EAG-RCON8ETP)),K.RCON8EAG                 
         GOTO1 VHIGH                                                            
         B     BCD0010                                                          
*                                                                               
BCDX     DS    0H                                                               
         B     EXITOK                                                           
         DROP  K,KS,R5,R8                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
* LIST CONTRACT DOWNLOAD FOR STATION WEB REQUESTS                               
*      OUTPUT SIMPLE LIST                                                       
*--------------------------------------------------------------------*          
BCONLST  NTR1  BASE=*,LABEL=*                                                   
         USING RCONREC,R8                                                       
*                                                                               
         AHI   R5,VHPARMLQ                 SECOND PARAMTER BLOCK                
         USING VHPARMD,R5                                                       
*                                                                               
         MVC   VHPSTA,RCONKSTA                                                  
         MVC   VHPADV,RCONKADV                                                  
         MVC   VHPPRD,RCONPRD                                                   
         MVC   VHPAGY,RCONKAGY                                                  
         MVC   VHPAOF,RCONKAOF                                                  
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,VHPFLS)                            
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,VHPFLE)                          
         MVC   VHPSAL,RCONSAL                                                   
         MVC   VHPCTY,RCONTYPE                                                  
         MVC   VHPBUYER,RCONBUYR                                                
         DROP  R8                                                               
* FLIGHT                                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,VHPFLS,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,VHPFLE,0                           
* BUYER                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,VHPBUYER,0                         
* DEMOS                                                                         
         GOTO1 =A(DEMOS),RR=Y                                                   
* SIDE                                                                          
         GOTO1 =A(CKSIDE),RR=Y                                                  
         GOTO1 AADDDATA,DMCB,AFABLK,CONSIDEL,BYTE,0                             
* SEND                                                                          
         GOTO1 =A(CKSEND),RR=Y                                                  
* VER                                                                           
         GOTO1 =A(CKVER),RR=Y                                                   
         GOTO1 AADDDATA,DMCB,AFABLK,CONVEREL,BYTE,0                             
* MOD                                                                           
         GOTO1 =A(CKMOD),RR=Y      GET LAST MOD                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONMODEL,BYTE,0                             
* WIP                                                                           
         GOTO1 =A(CKWIP),RR=Y      GET WIP STATUS                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONWIPEL,BYTE,0                             
* ALPHA                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONREPEL,REPALPHA,0                         
* TOTAL$                                                                        
         GOTO1 =A(BUCKET),RR=Y                                                  
* PRD                                                                           
         CLC   VHPPRD,SPACES                                                    
         BNH   BCL0010                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONPCDEL,VHPPRD,L'VHPPRD                    
         B     BCL0012                                                          
*                                                                               
BCL0010  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   BCL0012                                                          
*                                                                               
         USING RCONEXEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONEXPR,L'RCONEXPR                
         DROP  R8                                                               
*                                                                               
BCL0012  DS    0H                                                               
*                                                                               
* CTY                                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONCTYEL,VHPCTY,0                           
* STA                                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTAEL,VHPSTA,0                           
* ADV                                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONADVEL,VHPADV,0                           
* AGY                                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONAGYEL,VHPAGY,0                           
*                                                                               
         CLC   VHPAOF,SPACES                                                    
         BNH   BCL0020                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONAOFEL,VHPAOF,0                           
*                                                                               
BCL0020  DS    0H                                                               
*                                                                               
* SAL                                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONSALEL,VHPSAL,0                           
* OFF                                                                           
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONOFFEL,RCONKOFF,0                         
         DROP  R8                                                               
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'18'        DEVELOPMENT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   BCL0030                                                          
*                                                                               
         USING RCONDVEL,R8                                                      
* DSP                                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONDSPEL,RCONDVSP,0                         
* DCT                                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONDCTEL,RCONDVCT,0                         
         DROP  R8                                                               
*                                                                               
BCL0030  DS    0H                                                               
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
* LIST CONTRACT DOWNLOAD FOR STATION WEB REQUESTS                               
*      OUTPUT DETAIL LIST                                                       
*--------------------------------------------------------------------*          
BCONDET  NTR1  BASE=*,LABEL=*                                                   
         USING RCONREC,R8                                                       
*                                                                               
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         AHI   R5,VHPARMLQ                 SECOND PARAMTER BLOCK                
         USING VHPARMD,R5                                                       
*                                                                               
         LA    RE,VHPARMLQ(R5)                                                  
         MVC   0(L'KEY,RE),KEYSAVE         SAVE THE KEY AFTER THAT              
         AHI   RE,L'KEY                                                         
         ST    RE,ADDR                     SAVE THE NEXT AREA                   
*                                                                               
         MVC   VHPSTA,RCONKSTA                                                  
         MVC   VHPADV,RCONKADV                                                  
         MVC   VHPPRD,RCONPRD                                                   
         MVC   VHPAGY,RCONKAGY                                                  
         MVC   VHPAOF,RCONKAOF                                                  
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,VHPFLS)                            
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,VHPFLE)                          
         MVC   VHPSAL,RCONSAL                                                   
         MVC   VHPCTY,RCONTYPE                                                  
         MVC   VHPBUYER,RCONBUYR                                                
         DROP  R8                                                               
*                                                                               
         CLC   VHPPRD,SPACES       PRODUCT GIVEN?                               
         BH    BCDET002            YES - DON'T LOOK FOR FREE FORM               
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   BCDET002                                                         
*                                                                               
         USING RCONEXEL,R8                                                      
         MVC   VHPFFPRD,RCONEXPR                                                
         DROP  R8                                                               
*                                                                               
BCDET002 DS    0H                                                               
*                                                                               
* FLIGHT                                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,VHPFLS,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,VHPFLE,0                           
* BUYER                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,VHPBUYER,0                         
* DEMOS                                                                         
         GOTO1 =A(DEMOS),RR=Y                                                   
* SIDE                                                                          
         GOTO1 =A(CKSIDE),RR=Y                                                  
         GOTO1 AADDDATA,DMCB,AFABLK,CONSIDEL,BYTE,0                             
* SEND                                                                          
         GOTO1 =A(CKSEND),RR=Y                                                  
* VER                                                                           
         GOTO1 =A(CKVER),RR=Y                                                   
         GOTO1 AADDDATA,DMCB,AFABLK,CONVEREL,BYTE,0                             
* MOD                                                                           
         GOTO1 =A(CKMOD),RR=Y      GET LAST MOD                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONMODEL,BYTE,0                             
* WIP                                                                           
         GOTO1 =A(CKWIP),RR=Y      GET WIP STATUS                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONWIPEL,BYTE,0                             
* ALPHA                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONREPEL,REPALPHA,0                         
* TOTAL$                                                                        
         GOTO1 =A(BUCKET),RR=Y                                                  
* PRD                                                                           
         CLC   VHPPRD,SPACES       PRODUCT GIVEN?                               
         BH    BCDET020            YES - USE VALPRD                             
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,PRDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R0,L'VHPFFPRD                                                    
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,VHPFFPRD,(R0)                      
         B     BCDET022                                                         
*                                                                               
BCDET020 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   BCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,PRDBCD-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
BCDET022 DS    0H                                                               
*                                                                               
* STA                                                                           
         GOTO1 ASETELEM,DMCB,AFABLK,STADATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,STASTAEL,VHPSTA,5                           
* ADV                                                                           
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   BCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,ADVBCD-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
* AGY                                                                           
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   BCONERR                                                          
* SAL                                                                           
         LA    R0,*                                                             
         AHI   R0,AGYBCD-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                          
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
*                                                                               
         LA    R0,*                                                             
         AHI   R0,SALBCD-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         AHI   R5,VHPARMLQ             RESTORE THE KEY                          
         MVC   KEYSAVE,0(R5)                                                    
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
*                                                                               
BCONERR  DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DEMOS - GET AND OUTPUT DEMOS FROM SAR OR BOP                                  
**********************************************************************          
DEMOS    NTR1  BASE=*,LABEL=*                                                   
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'12'        SAR ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   DEMO0020                                                         
*                                                                               
         LA    R2,7                NUMBER OF DEMOS                              
         LA    R8,RSARXDEM-RSARXEL(R8)                                          
*                                                                               
DEMO0010 DS    0H                                                               
         OC    0(3,R8),0(R8)                                                    
         BZ    DEMO0040                                                         
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
         MVI   WORK+60,0           NO PRIMARY                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
         LA    R8,3(R8)                                                         
         BCT   R2,DEMO0010                                                      
*                                                                               
DEMO0020 DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'10'        BOP ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   DEMO0040                                                         
*                                                                               
         USING RCONBPEL,R8                                                      
         CLI   RCONBPDM,X'FF'      SKIP OLD CONTRACTS                           
         BNE   DEMO0040                                                         
*                                                                               
         LA    R2,(L'RCONBPDM-1)/3 NUMBER OF DEMOS                              
         LA    R8,RCONBPDM+1                                                    
         DROP  R8                                                               
*                                                                               
DEMO0030 DS    0H                                                               
         OC    0(3,R8),0(R8)                                                    
         BZ    DEMO0040                                                         
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
         MVI   WORK+60,0           NO PRIMARY                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
         LA    R8,3(R8)                                                         
         BCT   R2,DEMO0030                                                      
*                                                                               
DEMO0040 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUCKET - ADD BUCKETS AND OUTPUT ORDER TOTAL ELEMENT                           
**********************************************************************          
BUCKET   NTR1  BASE=*,LABEL=*                                                   
         ZAP   WORK(10),=PL1'0'                                                 
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'03'        SEE IF THERE IS MONEY                        
         BAS   RE,GETEL                                                         
         BNE   BUCK020                                                          
*                                                                               
BUCK010  DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,15,RCONBKAM-RCONBKEL(R8)                                      
         CVD   RE,DUB                                                           
         AP    WORK(10),DUB                                                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    BUCK010                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONTOTEL,WORK,0                             
*                                                                               
BUCK020  DS    0H                                                               
         ZAP   WORK(10),=PL1'0'                                                 
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'63'        SEE IF THERE IS TRADE                        
         BAS   RE,GETEL                                                         
         BNE   BUCK040                                                          
*                                                                               
BUCK030  DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,15,RCONBKAM-RCONBKEL(R8)                                      
         CVD   RE,DUB                                                           
         AP    WORK(10),DUB                                                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    BUCK030                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONXOTEL,WORK,0                             
*                                                                               
BUCK040  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CKSIDE - CHECK IF CONTRACT IS ON REP/STATION/BOTH SIDE                        
*          RETURNS R/S/B IN 'BYTE'                                              
**********************************************************************          
CKSIDE   NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,C'R'           DEAFULT TO REP SIDE                          
         L     R8,AIOREC           CHECK SEND STATUS                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSIDEX             NO 20 ELEM, ON REP SIDE                      
         USING RCONSEND,R8                                                      
*                                                                               
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BZ    CKSIDE20            NO                                           
         MVI   BYTE,C'S'           STATION SIDE                                 
         TM    RCONSENF,X'10'      STA VERSION NOT ADVANCED?                    
         BZ    CKSIDEX             NO                                           
         MVI   BYTE,C'B'           REP COULD TOUCH IT AGAIN                     
CKSIDE20 DS    0H                                                               
         TM    RCONSENF,X'20'      REP VERSIO NOT ADVANCED?                     
         BZ    CKSIDEX                                                          
         MVI   BYTE,C'B'           STATION COULD TOUCH IT AGAIN                 
CKSIDEX  B     EXITOK                                                           
         DROP  R8                                                               
**********************************************************************          
* CKSEND - CHECK IF CONTRACT LAST SEND BY REP/STA OR IS CONFIRMED               
*          RETURNS R/S/C IN 'BYTE'                                              
**********************************************************************          
CKSEND   NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,C'N'                                                        
         L     R8,AIOREC           CHECK SEND STATUS                            
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND10                                                         
*                                                                               
         USING RCONXEL,R8                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    CKSEND10            NO                                           
*                                                                               
         MVI   BYTE,C'C'                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
         DROP  R8                                                               
         L     R8,AIOREC           GET CF DATE/TIME                             
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND30                                                         
*                                                                               
         USING RMODELEM,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RMODEL1D,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RMODEL1T,0                         
*                                                                               
         B     CKSEND30                                                         
         DROP  R8                                                               
*                                                                               
CKSEND10 DS    0H                                                               
         L     R8,AIOREC           CHECK SEND STATUS                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND30            NO 20 ELEM, ON REP SEND                      
*                                                                               
         USING RCONSEND,R8                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BZ    CKSEND20            NO                                           
*                                                                               
         MVI   BYTE,C'R'                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RCONSRDT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RCONSRTI,0                         
*                                                                               
         B     CKSEND30                                                         
*                                                                               
CKSEND20 DS    0H                                                               
         TM    RCONSENF,X'40'      LAST SENT BY STATION?                        
         BZ    CKSEND30                                                         
*                                                                               
         MVI   BYTE,C'S'           STATION COULD TOUCH IT AGAIN                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RCONSSDT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RCONSSTI,0                         
*                                                                               
CKSEND30 DS    0H                                                               
         CLI   BYTE,C'N'                                                        
         BNE   CKSEND32                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
*                                                                               
CKSEND32 DS    0H                                                               
         CLI   BYTE,C'C'                                                        
         B     CKSEND50            MO DID NOT IMPLEMENT STACF                   
         BE    CKSEND50                                                         
*                                                                               
         L     R8,AIOREC           CHECK FOR DATE                               
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND50                                                         
*                                                                               
         USING RCONDREL,R8                                                      
         TM    RCONDRFG,X'80'      LINKED?                                      
         BZ    CKSEND50            NO                                           
         DROP  R8                                                               
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         MVC   WORK(L'KEY),KEY                                                  
         GOTOX (RFGETDAR,VREPFACS),DMCB,AIOREC,KEY,0,DUB                        
         BL    CKSEND48            NO                                           
*                                                                               
         MVC   DUB(4),AIOREC                                                    
         GOTO1 VGETREC,AIO2                                                     
         MVC   AIOREC,DUB                                                       
*                                                                               
         L     R8,AIO2                                                          
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND48                                                         
*                                                                               
         USING RDARFLEM,R8         X                                            
         TM    RDARFLG1,X'10'      STA-CF?                                      
         BZ    CKSEND48            NO                                           
         DROP  R8                                                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONSCFEL,0,0                                
*                                                                               
         L     R8,AIO2                                                          
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND48                                                         
*                                                                               
         USING RDARSTEM,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONSCDEL,RDARSTDT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSCTEL,RDARSTTM,0                         
         DROP  R8                                                               
*                                                                               
CKSEND48 DS    0H                                                               
         MVC   KEY,WORK                                                         
         GOTO1 VHIGH                                                            
*                                                                               
CKSEND50 DS    0H                                                               
         B     EXITOK                                                           
**********************************************************************          
* CKVER - GET CURRENT VERSION NUMBER OF HEADER                                  
**********************************************************************          
CKVER    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,0                                                           
         L     R8,AIOREC           CHECK SEND STATUS                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKVERX                                                           
         USING RCONSEND,R8                                                      
         MVC   BYTE,RCONSRV        REP VER#                                     
         CLC   BYTE,RCONSSV        VS. STATION VER#                             
         BH    *+10                                                             
         MVC   BYTE,RCONSSV        STA VER#                                     
CKVERX   B     EXITOK                                                           
         DROP  R8                                                               
**********************************************************************          
* CKMOD - GET CURRENT MOD NUMBER OF HEADER                                      
**********************************************************************          
CKMOD    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,0                                                           
         L     R8,AIOREC           CHECK SEND STATUS                            
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKMODX                                                           
         USING RMODELEM,R8                                                      
         MVC   BYTE,RMODEL1M       MOD#                                         
CKMODX   B     EXITOK                                                           
         DROP  R8                                                               
**********************************************************************          
* CKWIP - GET CURRENT WIP STATUS                                                
**********************************************************************          
CKWIP    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,0                                                           
         L     R8,AIOREC           CHECK WIP STATUS                             
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKWIP10                                                          
         USING RCONXEL,R8                                                       
*                                                                               
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BO    CKWIPX              YES - NOT WIP                                
*                                                                               
CKWIP10  L     R8,AIOREC           CHECK WIP STATUS                             
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKWIPX                                                           
         USING RCONSEND,R8                                                      
*                                                                               
         TM    RCONSENF,X'30'      CHECK BOTH VERSION ADVANCED FLAGS            
         BO    CKWIPX                                                           
         MVI   BYTE,1                                                           
CKWIPX   B     EXITOK                                                           
         DROP  R8                                                               
**********************************************************************          
* RATINGS DOWNLOAD                                                              
**********************************************************************          
RTGSDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         AHI   R2,(SVPARMBF-T83AFFD)                                            
         ST    R2,ADDR                                                          
*                                                                               
         MVC   NUMBKS,0(R2)        NUMBER OF BOOKS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST BOOK                            
         ST    RF,FRSTBK                                                        
         ZIC   RF,NUMBKS                                                        
         MH    RF,=Y(BKLENQ)       BUMP TO UPGRADES                             
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMUPGS,0(R2)       NUMBER OF UPGRADES                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST UPGRADE                         
         ST    RF,FRSTUPG                                                       
         ZIC   RF,NUMUPGS                                                       
         MH    RF,=Y(UPGLENQ)      BUMP TO DEMOS                                
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=Y(DEMLENQ)      BUMP TO RATE CARDS                           
         AR    R2,RF                                                            
         EJECT                                                                  
*                                                                               
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         LA    R2,5(R2)            POINT TO STATIONS PROGRAMS                   
*                                                                               
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO INVENOTRY                           
         ST    RF,CURINV                                                        
*                                                                               
         MVC   CURSTA,FRSTSTA      SET CURRENT STATION                          
         ZAP   INVSEQ,=P'1'                                                     
*                                                                               
********************************************************                        
* LOOP THROUGH ALL THE STATIONS / INVENTORY                                     
********************************************************                        
XPSTA000 DS    0H                                                               
         LA    R0,FETCHBLK         CLEAR THE FETCH PARAMTER BLOCK               
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOMFACS    A(COMFACS)                                   
         MVC   RFTAIO1,AIO3        A(2K IO AREA)                                
         MVC   RFTAIO2,AIO4        A(2K IO AREA)                                
         LA    RE,FETCHWRK                                                      
         STCM  RE,15,RFTAWRK       A(6K WORK AREA)                              
         MVC   RFTCREP,REPALPHA    REP CODE                                     
         MVC   RFTCSRC,RTSRVC      RATING SERVICE                               
         MVI   RFTAMODE,RFTADIRQ   FETCH MODE                                   
         MVI   RFTCDCTL,RFTCDC1Q   FETCH METHOD                                 
         LA    RE,FTCHHOOK         HOOK ROUTINE                                 
         STCM  RE,15,RFTHOOKA                                                   
         OI    RFTCNTL,RFTCHDRQ    INCLUDE HEADER                               
         OI    RFTCNTL,RFTCDEMQ    INCLUDE DEMOS                                
         OI    RFTCNTL,RFTCSLVQ    INCLUDE SHARES AND LEVELS                    
         OI    RFTCNTL,RFTCFTNQ    INCLUDE FOOTNOTES                            
*                                                                               
XPSTA010 DS    0H                                                               
         L     RE,ADDR             POINT TO CURRENT STATIONS                    
         A     RE,CURSTA                                                        
         MVC   RFTCSTAT,0(RE)      STATION CALL LETTERS                         
         CLI   RFTCSTAT+4,C' '     NEED 'T' SET?                                
         BNE   *+8                 NO                                           
         MVI   RFTCSTAT+4,C'T'                                                  
*                                                                               
         L     RE,ADDR             POINT TO CURRENT INVENOTRY                   
         A     RE,CURINV                                                        
*                                                                               
         CLI   0(RE),0             ANY INVENTORY NUMBER?                        
         BE    XPSTA100            NO SKIP PLACE HOLDER                         
*                                                                               
         MVI   RFTCDTDP,C'D'       DAYPART                                      
         LA    R0,RFTCDTM+(8*RFTCDTLQ)     END OF LIST                          
         LA    RF,RFTCDTM                                                       
X        USING RFTCDTM,RF                                                       
*                                                                               
XPSTA020 DS    0H                                                               
         MVC   X.RFTCDTDY,0(RE)      DAY                                        
         MVC   X.RFTCDTST,1(RE)      START TIME                                 
*                                                                               
         CLI   3(RE),X'FF'           EFFECTIVE DATE W/NO END TIME?              
         BE    *+14                  YES                                        
         MVC   X.RFTCDTEN,3(RE)      END TIME                                   
         B     XPSTA022                                                         
*                                                                               
         XC    X.RFTCDTEN,X.RFTCDTEN                                            
         MVC   X.RFTCDTES,4(RE)                                                 
         LA    RE,1(RE)            FUDGE ADDRESS                                
         DROP  X                                                                
*                                                                               
XPSTA022 DS    0H                                                               
         CLI   5(RE),X'FF'         END OF PROGRAM?                              
         BE    XPSTA030                                                         
*                                                                               
         LA    RF,RFTCDTLQ(RF)     NO BUILD ORBIT                               
         LA    RE,5(RE)                                                         
*                                                                               
         CR    RF,R0               PAST END OF LIST?                            
         BL    XPSTA020            NO -  ADD TO REQUEST                         
         B     XPSTA022            YES - SKIP OVER                              
*                                                                               
XPSTA030 DS    0H                                                               
         S     RE,ADDR             SAVE CURRENT POINTER IN CASE OF              
         ST    RE,CURINV            ORBITS                                      
*                                                                               
         L     RF,AIO1                                                          
         ST    RF,ACURPARM         STORE ADDRESS OF CURRENT PARMS               
         EJECT                                                                  
*-----------------*                                                             
* BUILD BOOK LIST *                                                             
*-----------------*                                                             
         CLI   NUMDEMS,0                                                        
         BE    DEMSX99                                                          
*                                                                               
         CLI   NUMBKS,0                                                         
         BE    BKSX099                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCBKS,LONGPARM    SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCBKS+1                                                  
         ZIC   R1,NUMBKS           SET NUMBER OF REMAINING BOOKS                
         L     RE,ADDR             POINT TO CURRENT BOOK                        
         A     RE,FRSTBK                                                        
BKSX010  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCBKLQ*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCBKS,RF                                                       
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         MVC   0(RFTCBKLQ,RF),0(RE)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         LA    RE,BKLENQ(RE)                                                    
         DROP  RF                                                               
*                                                                               
         BCT   R1,BKSX010                                                       
*                                                                               
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
BKSX099  DS    0H                                                               
         EJECT                                                                  
*--------------------*                                                          
* BUILD UPGRADE LIST *                                                          
*--------------------*                                                          
         CLI   NUMUPGS,0                                                        
         BE    UPGSX99                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCUPGA                                                   
         ZIC   R1,NUMUPGS          SET NUMBER OF REMAINING BOOKS                
         L     RE,ADDR             POINT TO CURRENT BOOK                        
         A     RE,FRSTUPG                                                       
UPGSX10  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(UPGLENQ*2))                                         
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         MVC   0(UPGLENQ-1,RF),0(RE)                                            
         LA    RF,UPGLENQ-1(RF)                                                 
         LA    RE,UPGLENQ(RE)                                                   
         BCT   R1,UPGSX10                                                       
*                                                                               
         XC    0(UPGLENQ,RF),0(RF)                                              
         LA    RF,UPGLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
UPGSX99  DS    0H                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD DEMO LIST *                                                             
*-----------------*                                                             
         LA    RF,RFTCDEMS                                                      
         ZIC   R1,NUMDEMS          SET NUMBER OF REMAINING DEMOS                
         CLI   NUMDEMS,((RFTCBKS-RFTCDEMS)/3)-1                                 
         BNH   *+8                 NOT TOO MANY DEMOS                           
         LA    R1,((RFTCBKS-RFTCDEMS)/3)-1                                      
         L     RE,ADDR             POINT TO CURRENT DEMO                        
         A     RE,FRSTDEM                                                       
DEMSX10  DS    0H                  LOOP AND SET DEMOS                           
         MVC   0(3,RF),0(RE)                                                    
         LA    RF,3(RF)                                                         
         LA    RE,DEMLENQ(RE)                                                   
         BCT   R1,DEMSX10                                                       
*                                                                               
DEMSX99  DS    0H                                                               
         EJECT                                                                  
*------------*                                                                  
* FETCH CALL *                                                                  
*------------*                                                                  
         NI    MISCFLG1,FF-MF1TMPB2   SET NEW INVENTORY                         
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
         DROP  R4                                                               
*                                                                               
XPSTA100 DS    0H                  PROCESS NEXT INVENTORY                       
         L     RE,ADDR                                                          
         A     RE,CURINV                                                        
         LA    RE,6(RE)                                                         
         CLI   0(RE),0             END OF INVENOTRY FOR THIS STATION?           
         BNE   XPSTA110            YES                                          
*                                                                               
         LA    RE,1(RE)            PROCESS NEXT STATION                         
         CLI   0(RE),0             END OF STATIONS?                             
         BE    XPSTAX              YES                                          
*                                                                               
         LR    RF,RE                                                            
         S     RF,ADDR                                                          
         ST    RF,CURSTA                                                        
         LA    RE,5(RE)            BUMP TO INVENTORY                            
*                                                                               
XPSTA110 DS    0H                  PROCESS NEXT INVENTORY                       
         S     RE,ADDR                                                          
         ST    RE,CURINV                                                        
         AP    INVSEQ,=P'1'                                                     
         B     XPSTA000               FETCH IT                                  
*                                                                               
XPSTAX   DS    0H                  END OF FETCH LOOP                            
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HOOK FOR THE FETCH ROUTINE TO ADD NEW DETAIL CLUSTERS                         
***********************************************************************         
FTCHHOOK NTR1  BASE=*,LABEL=*                                                   
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RFTMODE,RFTNBKQ                                                  
         BE    INVBK                                                            
         B     EXITOK                                                           
         EJECT                                                                  
*=============================*                                                 
* PROCESS INVENTORY BOOK HOOK *                                                 
*=============================*                                                 
INVBK    DS    0H                                                               
         OC    RFTFDEMS(24*4),RFTFDEMS                                          
         BNZ   IBK00001                                                         
         OC    RFTFSHRS(24*4),RFTFSHRS                                          
         BNZ   IBK00001                                                         
         OC    RFTFLVLS(24*4),RFTFLVLS                                          
         BZ    EXITOK              NO INTERESTING DATA                          
*                                                                               
IBK00001 DS    0H                                                               
         OC    RFTFBK,RFTFBK       WAS IT A BOOK?                               
         BZ    INVUPG              NO - ITS AN UPGRADE                          
*                                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
         ZIC   RF,NUMBKS           GET BOOK SEQUENCE NUMBER                     
         L     RE,FRSTBK                                                        
         A     RE,ADDR                                                          
IBK0010  DS    0H                                                               
         CLC   RFTFBK,0(RE)        BOOK MATCH?                                  
         BE    IBK0020             YES                                          
         LA    RE,BKLENQ(RE)                                                    
         BCT   RF,IBK0010                                                       
         DC    H'0'                UM - THIS CAN'T HAPPEN                       
*                                                                               
IBK0020  DS    0H                                                               
         ZIC   RE,NUMBKS                                                        
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
*                                                                               
*                                  ADD NEW BOOK ELEMENT                         
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY SEQUENCE NUMBER?              
         BNZ   IBK0022             NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSPRGEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IBK0022  DS    0H                                                               
*                                  SET SEQUENCE NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSSEQEL,BYTE,0                             
*                                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IBK0030  CLI   0(RE),C' '                                                       
         BH    IBK0032                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IBK0030                                                          
         B     IBK0040             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IBK0032  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
*&&DO                                                                           
         MVI   BYTE,3              SET FLUFF LENGTH                             
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFOOEL,BYTE,0                             
*&&                                SET FOOTNOTE                                 
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IBK0040                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFTNEL,(RF),(R0)                          
*                                                                               
IBK0040  DS    0H                  DO DEMOS                                     
         B     INVDEM                                                           
*================================*                                              
* PROCESS INVENTORY UPGRADE HOOK *                                              
*================================*                                              
INVUPG   DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY SEQUENCE NUMBER?              
         BNZ   IUPG002             NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSPRGEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IUPG002  DS    0H                                                               
         SR    R0,R0               SET SEQUENCE NUMBER                          
         L     R1,RFTFUPGA                                                      
         S     R1,RFTCUPGA         DISPLACEMENT TO UPGRADE                      
         LTR   R1,R1                                                            
         BZ    IUPG010                                                          
*                                                                               
         LA    RE,UPGLENQ-1                                                     
         DR    R0,RE                                                            
IUPG010  DS    0H                                                               
         LA    R0,1(R1)            1 BASED SEQUENCE NUMBER                      
         STC   R0,BYTE                                                          
*                                                                               
         MHI   R1,UPGLENQ          ADDRESS INPUT UPGRADE                        
         A     R1,FRSTUPG                                                       
         A     R1,ADDR                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,BYTE,0                             
*                                                                               
IUPG040  DS    0H                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IUPG050  CLI   0(RE),C' '                                                       
         BH    IUPG052                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IUPG050                                                          
         B     IUPG060             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IUPG052  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
*&&DO                                                                           
         MVI   BYTE,3              SET FLUFF LENGTH                             
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFOOEL,BYTE,0                             
*&&                                SET FOOTNOTE                                 
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IUPG060                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFTNEL,(RF),(R0)                          
*                                                                               
IUPG060  DS    0H                  DO DEMOS                                     
         B     INVDEM                                                           
         EJECT                                                                  
*========================*                                                      
* PROCESS INVENTORY DEMO *                                                      
*========================*                                                      
INVDEM   DS    0H                                                               
         LA    R5,1                DEMO SEQUENCE NUMBER                         
         LA    R2,RFTFDEMS         RATING VALUES                                
         LA    R3,RFTFSHRS         SHARE VALUES                                 
         LA    R6,RFTFLVLS         HUT/PUT LEVEL VALUES                         
*                                                                               
IDEM010  DS    0H                                                               
         OC    0(4,R2),0(R2)       ANY RATING?                                  
         BNZ   IDEM020             YES                                          
         OC    0(4,R3),0(R3)       ANY SHARE?                                   
         BNZ   IDEM020             YES                                          
         OC    0(4,R6),0(R6)       ANY HUT/PUT LEVEL?                           
         BNZ   IDEM020             YES                                          
         B     IDEM050             DON'T SEND THIS DEMO                         
*                                                                               
IDEM020  DS    0H                                                               
*                                  ADD NEW DEMO ELEMENT                         
         GOTO1 ASETELEM,DMCB,AFABLK,DEMDATA,0                                   
*                                                                               
         STC   R5,BYTE             SET SEQUENCE NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,DEMSEQEL,BYTE,0                             
*                                                                               
         OC    0(4,R2),0(R2)       ANY RATING?                                  
         BZ    IDEM030             NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,DEMRTGEL,(R2),0                             
*                                                                               
IDEM030  DS    0H                                                               
         OC    0(4,R3),0(R3)       ANY SHARE?                                   
         BZ    IDEM040             NO                                           
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,DEMSHREL,(R3),0                             
*                                                                               
IDEM040  DS    0H                                                               
         OC    0(4,R6),0(R6)       ANY HUT/PUT LEVEL?                           
         BZ    IDEM050             NO                                           
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,DEMLVLEL,(R6),0                             
*                                                                               
IDEM050  DS    0H                                                               
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R6,4(R6)                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         CLM   R5,1,NUMDEMS                                                     
         BNH   IDEM010                                                          
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADER DOWNLOAD                                                      
**********************************************************************          
VHDRDWN  NTR1  BASE=*,LABEL=*                                                   
         TM    MISCFLG2,VHBKS                                                   
         BZ    VHDWN002                                                         
*                                                                               
         BAS   RE,VBKSDWN          OUTPUT VALIDATED BKS,DEMOS,UPGS              
*                                                                               
VHDWN002 DS    0H                                                               
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T83AFFD)                                            
         USING VHPARMD,R6                                                       
*                                                                               
         LA    R0,VHPARMLQ(R6)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         CLC   VHPSTA,SPACES                                                    
         BNH   VHDWN010                                                         
*                                                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,VHPSTA),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,STATBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
**       GOTO1 GSTABKS,DMCB,(0,VHPSTA),ADDR                                     
**       BNE   VHDWNERR                                                         
*                                                                               
VHDWN010 DS    0H                                                               
         CLC   VHPADV,SPACES                                                    
         BNH   VHDWN020                                                         
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,ADVTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN020 DS    0H                                                               
         CLC   VHPPRD,SPACES                                                    
         BNH   VHDWN030                                                         
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,PRDTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN030 DS    0H                                                               
         CLC   VHPAGY,SPACES                                                    
         BNH   VHDWN040                                                         
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGYTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         GOTOX (AGYADDRQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),       +        
               (0,VHPADV),(0,VHPCTY),ADDR                                       
         BNE   VHDWN040                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGADTBL-(*-4)                                                 
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN040                                                         
*                                                                               
VHDWN040 DS    0H                                                               
         OC    VHPFLS,VHPFLS                                                    
         BZ    VHDWN050                                                         
*                                                                               
         GOTOX (VFLIGHTQ,ADDR2),DMCB,(RC),(0,VHPFLS),(0,VHPFLE),       +        
               (0,VHPSTA),ADDR                                                  
         BNE   VHDWNERR                                                         
*                                                                               
VHDWN050 DS    0H                                                               
         CLC   VHPDSP,SPACES                                                    
         BNH   VHDWN060                                                         
*                                                                               
         GOTOX (VDEVSALQ,ADDR2),DMCB,(RC),(0,VHPDSP),ADDR                       
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DSPTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN060 DS    0H                                                               
         CLC   VHPDCT,SPACES                                                    
         BNH   VHDWN070                                                         
*                                                                               
         GOTOX (VDEVTYPQ,ADDR2),DMCB,(RC),(0,VHPDCT),ADDR                       
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DCTTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN070 DS    0H                                                               
         CLC   VHPSAL,SPACES                                                    
         BNH   VHDWN080                                                         
*                                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,SALTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN080 DS    0H                                                               
*                                                                               
VHDWNX   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
VHDWNERR DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GENERIC GLOBBER DOWNLOAD                                                      
**********************************************************************          
GLOBDWN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    CONERR,CONERR       ERROR?                                       
         BZ    GLDWN000            NO                                           
*                                                                               
         MVC   ERROR,CONERR        SET ERROR                                    
         B     EXITL                                                            
*                                                                               
GLDWN000 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* VALIDATE BOOKS/UPGRADES/DEMOS DOWNLOAD                                        
**********************************************************************          
VBKSDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADDR                                                          
         AHI   R6,VHPARMLQ                                                      
         A     R6,ATWA                                                          
*                                                                               
         SR    R2,R2               UPGRADE SEQ# COUNT                           
         SR    R4,R4               UPGRADE NAME COUNT                           
VBDWN010 DS    0H                                                               
         OC    0(3,R6),0(R6)       END OF DATA?                                 
         BZ    VBDWN200            YES                                          
*                                                                               
         CLC   =Y(VBINBKQ),0(R6)   INVENTORY BOOK?                              
         BE    *+14                YES                                          
         CLC   =Y(VBBOOKQ),0(R6)   BOOK?                                        
         BNE   VBDWN020            NO                                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         CLC   =Y(VBINBKQ),0(R6)   INVENTORY BOOK?                              
         BNE   VBDWN012            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSINVEL,0,0                                
*                                                                               
VBDWN012 DS    0H                                                               
         ZIC   RF,2(R6)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,BKSNAMEL,3(R6),(RF)                         
         B     VBDWN100                                                         
*                                                                               
VBDWN020 DS    0H                                                               
         CLC   =Y(VBDEMOQ),0(R6)   DEMO?                                        
         BNE   VBDWN030            NO                                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,DEMDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZIC   RF,2(R6)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,DEMNAMEL,3(R6),(RF)                         
         B     VBDWN100                                                         
*                                                                               
VBDWN030 DS    0H                                                               
         CLC   =Y(VBUPGRDQ),0(R6)  INVENTORY UPGRADE?                           
         BE    *+14                YES                                          
         CLC   =Y(VBNIUPGQ),0(R6)  UPGRADE?                                     
         BNE   VBDWN040            NO                                           
*                                                                               
         LA    R2,1(R2)                                                         
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         CLC   =Y(VBUPGRDQ),0(R6)  INVENTORY UPGRADE?                           
         BNE   VBDWN032            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSINVEL,0,0                                
*                                                                               
VBDWN032 DS    0H                                                               
         STC   R2,BYTE                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,BYTE,0                             
         B     VBDWN100                                                         
*                                                                               
VBDWN040 DS    0H                                                               
         CLC   =Y(VBUPGNMQ),0(R6)  UPGRADE NAME?                                
         BNE   VBDWN050            NO                                           
*                                                                               
         LA    R4,1(R4)                                                         
****     GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
****     OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         STC   R4,BYTE                                                          
****     GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,BYTE,0                             
*                                                                               
         ZIC   RF,2(R6)                                                         
****     GOTO1 AADDDATA,DMCB,AFABLK,BKSNAMEL,3(R6),(RF)                         
         B     VBDWN100                                                         
*                                                                               
VBDWN050 DS    0H                                                               
*                                                                               
VBDWN100 DS    0H                                                               
         ZIC   RF,2(R6)                                                         
         LA    R6,3(RF,R6)                                                      
         B     VBDWN010                                                         
*                                                                               
VBDWN200 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* REFRESH CONTRACT UPDATE                                                       
**********************************************************************          
UCONDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
         L     R8,AIO2                                                          
         USING RCONREC,R8                                                       
*                                                                               
         TM    MISCFLG2,UCFVER     NEED TO ADVANCE VERSION?                     
         BZ    *+8                                                              
         BAS   RE,GOUCVER          DO IT                                        
*                                                                               
         MVC   KEY(27),RCONKEY     PUT UPDATED CONTRACT RECORD                  
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO3                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
         TM    MISCFLG2,UCFCF      CONFIRM CONTRACT?                            
         BZ    *+10                                                             
         BAS   RE,GOUCCONF                                                      
         DC    H'0'                WE DON'T COME BACK                           
*                                                                               
         TM    MISCFLG2,UCFSEND    SEND CONTRACT?                               
         BZ    *+10                                                             
         BAS   RE,GOUCSEND                                                      
         DC    H'0'                WE DON'T COME BACK                           
*                                                                               
         B     EXITOK                                                           
         DROP  R8                                                               
*                                                                               
* ADVANCE CONTRACT VERSION                                                      
*                                                                               
GOUCVER  NTR1                                                                   
         L     R8,AIO2                                                          
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R8                                                       
         TM    RCONCONF,X'40'      CONFIRMED?                                   
         BZ    GOUCV040                                                         
         NI    RCONCONF,X'FF'-X'40' UNCONFIRM                                   
         OI    RCONCONF,X'20'+X'80' CONF PREVIOUSLY + NOT CONFIRMED             
         DROP  R8                                                               
*                                                                               
GOUCV040 DS    0H                                                               
         L     R8,AIO2                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R8                                                      
*                                                                               
         TM    RCONSENF,X'10'      STATION VERSION NOT ADVANCED                 
         BZ    GOUCV100                                                         
         DROP  R8                                                               
                                                                                
* ADVANCE STA VERSION AND UPDATE VERSION DATE                                   
         MVC   WORK(4),VHELLO                                                   
         MVC   WORK+4(4),VDATCON                                                
         GOTO1 =V(REGENVER),DMCB,(C'S',AIO2),(X'80',WORK),RR=Y                  
         BZ    GOUCV050                                                         
         STCM  R3,3,ERROR                                                       
         B     EXITL                                                            
GOUCV050 DS    0H                                                               
         TM    MISCFLG2,UCFOCM     ORD COMMENT EDIT IN PROCESS?                 
         BO    GOUCV100            YES - SKIP ORD CMT DELETE                    
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'92',AIO2),0,0                  
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'82',AIO2),0,0                  
GOUCV100 DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
* CONFIRM CONTRACT                                                              
*                                                                               
GOUCCONF NTR1                                                                   
         L     R8,AIO2                                                          
         USING RCONREC,R8                                                       
*                                  **BUILD XFER CONTROL ELEM**                  
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM WCF                                     
         MVC   GLVXFRPR,=C'WCF'                                                 
         MVC   GLVXTOSY,=C'REP'    TO CONTRACT                                  
         MVC   GLVXTOPR,=C'CON'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,24,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2       **BUILD CONTRACT ACTION ELEM**                 
         LA    R2,WORK2                                                         
         USING GLCONNUM,R2                                                      
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(5,GLCONNUM)               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R8                                                      
         MVC   GLCONCA(3),=C'CF='                                               
         EDIT  RCONSRV,(3,GLCONCA+3),ALIGN=LEFT                                 
         DROP  R8                                                               
         OI    GLCONFLG,GLCONRPQ  REQUEST REPLY                                 
         DROP  R2                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  **OFF TO CONTRACT**                          
*                                  FALINK TO EXIT AND RESUME                    
***>     GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB,0                           
         L     RD,BASERD                                                        
         B     EXITOK                                                           
*                                                                               
* SEND CONTRACT                                                                 
*                                                                               
GOUCSEND NTR1                                                                   
         L     R8,AIO2                                                          
         USING RCONREC,R8                                                       
*                                  **BUILD XFER CONTROL ELEM**                  
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM WCF                                     
         MVC   GLVXFRPR,=C'WCF'                                                 
         MVC   GLVXTOSY,=C'REP'    TO CONTRACT                                  
         MVC   GLVXTOPR,=C'CON'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,24,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2       **BUILD CONTRACT ACTION ELEM**                 
         LA    R2,WORK2                                                         
         USING GLCONNUM,R2                                                      
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(5,GLCONNUM)               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R8                                                      
         MVC   GLCONCA(5),=C'SEND='                                             
         EDIT  RCONSRV,(3,GLCONCA+5),ALIGN=LEFT                                 
         DROP  R8                                                               
         OI    GLCONFLG,GLCONRPQ  REQUEST REPLY                                 
         DROP  R2                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  **OFF TO CONTRACT**                          
*                                  FALINK TO EXIT AND RESUME                    
***>     GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB,0                           
         L     RD,BASERD                                                        
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
UCONX    DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
UCONERR  DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* REFRESH CONTRACT DOWNLOAD                                                     
**********************************************************************          
DCONDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T83AFFD)                                            
         SR    R2,R2                                                            
         ICM   R2,3,0(R6)          NUMBER OF CONTRACTS                          
         LA    R3,2(R6)            START OF CONTRACTS                           
         LR    R6,R2                                                            
         MHI   R6,L'VHPCON                                                      
         LA    R6,0(R6,R3)         BUMP PAST CONTRACTS                          
         LA    R0,VHPARMLQ(R6)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         USING VHPARMD,R6                                                       
DCDWN010 DS    0H                                                               
         XC    VHPARMD(VHPARMLQ),VHPARMD                                        
         MVC   VHPCON,0(R3)                                                     
*                                                                               
         BAS   RE,DCONFOO                                                       
         BNE   EXITL                                                            
*                                                                               
         LA    R3,L'VHPCON(R3)                                                  
         BCT   R2,DCDWN010                                                      
         DROP  R6                                                               
*                                                                               
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------          
* DO THE WORK OF DOWNLOADING A CONTRACT                                         
*    R6       POINTS TO VHPARMS WITH CON# FILLED IN                             
*    ADDR     POINTS TO USABLE OUTPUT AREA                                      
*    ADDR2    POINTS TO T83A10 - THE UTILITY ROUTINE                            
*---------------------------------------------------------------------          
DCONFOO  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         USING VHPARMD,R6                                                       
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5) CONTRACT NUM IN 9'S COMPLEMENT                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCONPTYP,KEY                                                     
         MVI   K.RCONPTYP,X'8C'                                                 
         MVC   K.RCONPREP,REPALPHA                                              
         MVC   K.RCONPCON,WORK                                                  
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   ERROR,=Y(82)                                                     
         B     EXITL                                                            
*                                                                               
         MVC   VHPKDA,KEY+28       SAVE HEADER D/A                              
*                                                                               
         TM    MISCFLG1,MF1STWEB                                                
         BO    *+8                 NO UPDATE FOR STATION WEB                    
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         MVI   UPDATE,C'N'                                                      
*                                                                               
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
*                                                                               
         LA    RF,*                                                             
         AHI   RF,STACHK-(*-4)                                                  
         BASR  RE,RF               VALIDATE CONTRACT STATION                    
         BNE   DCONERR                                                          
*                                                                               
         MVI   ELCODE,X'17'        GET COMBO ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   *+14                NOT FOUND - OK TO CONTINUE                   
         MVC   ERROR,=Y(920)                                                    
         B     EXITL                                                            
*                                                                               
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
*                                                                               
         MVC   VHPSTA,RCONKSTA                                                  
         MVC   VHPADV,RCONKADV                                                  
         MVC   VHPPRD,RCONPRD                                                   
         MVC   VHPAGY,RCONKAGY                                                  
         MVC   VHPAOF,RCONKAOF                                                  
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,VHPFLS)                            
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,VHPFLE)                          
         MVC   VHPSAL,RCONSAL                                                   
         MVC   VHPCTY,RCONTYPE                                                  
         MVC   VHPBUYER,RCONBUYR                                                
         DROP  R8                                                               
*                                                                               
         CLC   VHPPRD,SPACES       PRODUCT GIVEN?                               
         BH    DFOO0010            YES - DON'T LOOK FOR FREE FORM               
*                                                                               
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DFOO0010                                                         
*                                                                               
         USING RCONEXEL,R8                                                      
         MVC   VHPFFPRD,RCONEXPR                                                
         DROP  R8                                                               
*                                                                               
DFOO0010 DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'18'        DEVELOPMENT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DCON0012                                                         
*                                                                               
         USING RCONDVEL,R8                                                      
         MVC   VHPDCT,RCONDVCT                                                  
         MVC   VHPDSP,RCONDVSP                                                  
         DROP  R8                                                               
*                                                                               
DCON0012 DS    0H                                                               
*                                  ADD NEW CONTRACT ELEMENT                     
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,VHPCON,0                           
*                                                                               
         TM    MISCFLG1,MF1STWEB   STATION WEB REQUEST?                         
         BO    *+12                DOWNLOAD EVERYTHING                          
         TM    MISCFLG2,DCBUYS                                                  
         BNZ   DFOO0060                                                         
*                                                                               
         TM    MISCFLG2,DCNODET                                                 
         BNZ   DFOO0060                                                         
* FLIGHT                                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,VHPFLS,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,VHPFLE,0                           
* BUYER                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,VHPBUYER,0                         
* DEMOS                                                                         
         LA    RF,*                                                             
         AHI   RF,DEMOS-(*-4)                                                   
         BASR  RE,RF               GET DEMOS                                    
* SIDE                                                                          
         LA    RF,*                                                             
         AHI   RF,CKSIDE-(*-4)                                                  
         BASR  RE,RF               CHECK REP/STATION SIDE                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONSIDEL,BYTE,0                             
* SEND                                                                          
         LA    RF,*                                                             
         AHI   RF,CKSEND-(*-4)                                                  
         BASR  RE,RF               GET REP/STATION LAST SENT/CF                 
* VER                                                                           
         LA    RF,*                                                             
         AHI   RF,CKVER-(*-4)                                                   
         BASR  RE,RF               GET LAST VERSION                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONVEREL,BYTE,0                             
* MOD                                                                           
         LA    RF,*                                                             
         AHI   RF,CKMOD-(*-4)                                                   
         BASR  RE,RF               GET LAST MOD                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONMODEL,BYTE,0                             
* WIP                                                                           
         LA    RF,*                                                             
         AHI   RF,CKWIP-(*-4)                                                   
         BASR  RE,RF               GET WIP                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONWIPEL,BYTE,0                             
* ALPHA                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONREPEL,REPALPHA,0                         
* TOTAL$                                                                        
         LA    RF,*                                                             
         AHI   RF,BUCKET-(*-4)                                                  
         BASR  RE,RF               BUCKET CONTRACT                              
*                                                                               
* MON DOLLARS                                                                   
         BAS   RE,DCONDMON                                                      
* PRD                                                                           
         CLC   VHPPRD,SPACES       PRODUCT GIVEN?                               
         BH    DFOO0020            YES - USE VALPRD                             
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,PRDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R0,L'VHPFFPRD                                                    
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,VHPFFPRD,(R0)                      
         B     DFOO0022                                                         
*                                                                               
DFOO0020 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,PRDTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DFOO0022 DS    0H                                                               
         CLC   VHPCTY,SPACES                                                    
         BNH   DFOO0030                                                         
* CTY                                                                           
         GOTO1 ASETELEM,DMCB,AFABLK,CTYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R0,L'VHPCTY                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,VHPCTY,(R0)                        
*                                                                               
DFOO0030 DS    0H                                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,VHPSTA),ADDR                          
         BNE   DCONERR                                                          
* STA                                                                           
         LA    R0,*                                                             
         AHI   R0,STATBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
* STAREC IS STILL IN AIOREC FROM VSTA CALL.                                     
*  GET THE STATION EMAIL ADDRESS FROM IT.                                       
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'25'        SALES ASSISTANT ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DFOO0034                                                         
*                                                                               
DFOO0032 DS    0H                                                               
         ZIC   R0,1(R8)            LENGTH                                       
         SHI   R0,RSTAADD-RSTAEML                                               
         USING RSTAEML,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,STAEMLEL,RSTAADD,(R0)                       
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DFOO0032                                                         
*                                                                               
DFOO0034 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
* ADV                                                                           
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,ADVTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
* AGY                                                                           
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGYTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         GOTOX (AGYADDRQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),       +        
               (0,VHPADV),(0,VHPCTY),ADDR                                       
         BNE   DFOO0040                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGADTBL-(*-4)                                                 
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DFOO0040 DS    0H                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                          
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
* SAL                                                                           
         LA    R0,*                                                             
         AHI   R0,SALTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
* SAA                                                                           
*                                                                               
K        USING RSA2KEY,KEY                                                      
         XC    K.RSA2KEY,K.RSA2KEY                                              
         MVI   K.RSA2KTYP,X'46'                                                 
         MVC   K.RSA2KREP,REPALPHA                                              
         MVC   K.RSA2KSAL,VHPSAL                                                
         OC    K.RSA2KSAL,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSA2KEY),KEYSAVE                                           
         BNE   DFOO0044            MISSING PERSON 2 RECORD                      
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'21'        SALES ASSISTANT ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DFOO0044                                                         
*                                                                               
         USING RSASEMEM,R8                                                      
         GOTO1 ASETELEM,DMCB,AFABLK,SAADATA,0                                   
         GOTO1 AADDDATA,DMCB,AFABLK,SAANAMEL,RSASEMNM,0                         
         TM    RSASEMFL,X'80'                                                   
         BZ    DFOO0042                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,SAAEFLEL,0,0                                
*                                                                               
DFOO0042 DS    0H                                                               
         ZIC   R0,1(R8)            LENGTH                                       
         SHI   R0,RSASEAML-RSASEMEM                                             
         GOTO1 AADDDATA,DMCB,AFABLK,SAAEMLEL,RSASEAML,(R0)                      
         DROP  R8                                                               
*                                                                               
DFOO0044 DS    0H                                                               
         CLC   VHPDSP,SPACES                                                    
         BNH   DFOO0050                                                         
* DSP                                                                           
         GOTOX (VDEVSALQ,ADDR2),DMCB,(RC),(0,VHPDSP),ADDR                       
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DSPTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DFOO0050 DS    0H                                                               
         CLC   VHPDCT,SPACES                                                    
         BNH   DFOO0060                                                         
* DCT                                                                           
         GOTOX (VDEVTYPQ,ADDR2),DMCB,(RC),(0,VHPDCT),ADDR                       
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DCTTBL-(*-4)                                                  
         GOTO1 XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DFOO0060 DS    0H                                                               
         TM    MISCFLG2,DCNOCOM    NO COMMENTS?                                 
         BNZ   DFOO0100                                                         
* COMMENTS                                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),VHPKDA    HEADER D/A                                   
         GOTO1 VGETREC,AIOREC      REREAD HEADER REC                            
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'82'        REP ORDER COMMENT                            
         BAS   RE,GETEL                                                         
         BNE   DFOO0070                                                         
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,OCMDATA,0                                   
*                                                                               
DFOO0065 DS    0H                                                               
         ZIC   R2,1(R8)                                                         
         AHI   R2,-3                                                            
*                                                                               
         LTR   R2,R2                                                            
         BM    DFOO0066                                                         
* REP                                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    2(0,R8),SPACES                                                   
         LA    R2,1(R2)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,OCMREPEL,2(R8),(R2)                         
*                                                                               
DFOO0066 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DFOO0065                                                         
*                                                                               
DFOO0070 DS    0H                                                               
         ZIC   R2,0(R8)            SAVE THE LAST ELCODE                         
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'92'        STA ORDER COMMENT                            
         BAS   RE,GETEL                                                         
         BNE   DFOO0080                                                         
*                                                                               
         CHI   R2,X'82'            IF THE LAST ELCODE WAS REP ORDER             
         BE    DFOO0075             COMMENT ALREADY HAVE HEADER                 
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,OCMDATA,0                                   
*                                                                               
DFOO0075 DS    0H                                                               
         ZIC   R2,1(R8)                                                         
         AHI   R2,-3                                                            
*                                                                               
         LTR   R2,R2                                                            
         BM    DFOO0076                                                         
* STA                                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    2(0,R8),SPACES                                                   
         LA    R2,1(R2)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,OCMSTAEL,2(R8),(R2)                         
*                                                                               
DFOO0076 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DFOO0075                                                         
*                                                                               
DFOO0080 DS    0H                                                               
         EJECT                                                                  
**********************************************************************          
* READ BUYLINES FOR PC                                                          
**********************************************************************          
DFOO0100 DS    0H                                                               
         TM    MISCFLG2,DCBUYS                                                  
         BZ    DFOO0110                                                         
*                                                                               
* BUYS                                                                          
         BAS   RE,DCONDBUY                                                      
*                                                                               
DFOO0110 DS    0H                  GENERATE WORKSHEET DOWNLOAD                  
         GOTO1 VCOLY,DMCB,(X'11',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
* WKS                                                                           
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,VHPCON,(RC),FAMAP                                      
*                                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
DCONERR  DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*....................................................................           
*                                                                               
* PROCESS MON DOLLARS                                                           
*                                                                               
*    AIOREC -> RCONREC                                                          
*                                                                               
*....................................................................           
DCONDMON NTR1  BASE=*,LABEL=*                                                   
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
*                                                                               
         TM    RCONMODR+1,X'20'    MON DOLLARS?                                 
         BZ    DMON030             NO                                           
         DROP  R8                                                               
*                                                                               
         SR    R6,R6                                                            
         L     R2,AIOREC                                                        
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'03'        SEE IF THERE IS MONEY                        
         BAS   RE,GETEL                                                         
         BNE   DMON030                                                          
*                                                                               
         USING RCONBKEL,R8                                                      
DMON010  DS    0H                                                               
         CLC   0(RCONBKWK-RCONBKEL,R2),0(R8)                                    
         BE    DMON020                                                          
*                                                                               
         LTR   R6,R6               ANY MONEY?                                   
         BZ    DMON012             NO                                           
*                                                                               
X        USING RCONBKEL,R2                                                      
         XC    FULL,FULL                                                        
         MVC   FULL(1),X.RCONBKYR                                               
         MVC   FULL+1(1),X.RCONBKMN                                             
         MVI   FULL+2,1                                                         
         DROP  X                                                                
* MONTH                                                                         
         GOTO1 VDATCON,DMCB,(3,FULL),(19,WORK)                                  
         GOTO1 AADDDATA,DMCB,AFABLK,CONMONEL,WORK,0                             
* DOLLARS                                                                       
         ST    R6,FULL                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONDOLEL,FULL,0                             
*                                                                               
DMON012  DS    0H                                                               
         CLI   0(R8),X'03'         ELEMENT A BUCKET?                            
         BNE   DMON030             NO                                           
*                                                                               
         SR    R6,R6               RESET FOR NEXT BUCKET                        
         LR    R2,R8                                                            
*                                                                               
DMON020  DS    0H                                                               
         ICM   RE,15,RCONBKAM                                                   
         AR    R6,RE               ADD DOLLARS                                  
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     DMON010                                                          
         DROP  R8                                                               
*                                                                               
DMON030  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*....................................................................           
*                                                                               
* PROCESS BUYLINE                                                               
*                                                                               
*    R6 -> VHPARMS                                                              
*                                                                               
*....................................................................           
DCONDBUY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING VHPARMD,R6                                                       
*                                                                               
         L     R8,AIOREC                                                        
         ZAP   WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5) PWOS CONTRACT NUM                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING RBUYKEY,KEY                                                      
         MVI   K.RBUYKTYP,X'0B'                                                 
         MVC   K.RBUYKREP,REPALPHA                                              
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,WORK),(3,K.RBUYKCON)                 
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
DCON0101 DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   DCONDBYX                                                         
*                                                                               
         CLC   K.RBUYKPLN,=X'FFFFFF'                                            
         BNE   DCON0190            SKIP THIS BUY ITS A PLAN                     
         DROP  K                                                                
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC                                                        
         USING RBUYREC,R8                                                       
*                                                                               
**JRD    TM    RBUYFLG2,X'20'+X'10'+X'08'                                       
**JRD    BNZ   DCON0190            SKIP THIS BUY,ITS CANCELED                   
*                                                                               
**JRD    CLI   RBUYCHGI,C'C'                                                    
**JRD    BE    DCON0190            SKIP THIS BUY,ITS CANCELED                   
*                                                                               
         OC    POSTST,POSTST       START DATE FILTER?                           
         BNZ   *+14                YES                                          
         OC    POSTEN,POSTEN       END DATE FILTER?                             
         BZ    DCON0102            NO                                           
*                                                                               
**JRD    GOTO1 =A(CHOPBUY),DMCB,(R8),RR=Y                                       
**JRD    BNE   DCON0190            SKIP THIS BUY                                
*                                                                               
DCON0102 DS    0H                                                               
         MVC   BYTE2,RBUYNW        SAVE #/WK FOR 03 ELEMENTS                    
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BUYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYMLNEL,RBUYKMLN,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYLINEL,RBUYKLIN,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYVEREL,RBUYVER,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,BUYFLTEL,RBUYFLT,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,BUYCOSEL,RBUYCOS,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,BUYTSPEL,RBUYTSPT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYTCSEL,RBUYTCOS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYTWKEL,RBUYTWKS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYNPWEL,BYTE2,0                            
         ZIC   R1,RBUYSTED                                                      
         SRL   R1,4                                                             
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
         GOTO1 AADDDATA,DMCB,AFABLK,BUYSDYEL,(R0),0                             
         MVC   BYTE,RBUYSTED                                                    
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
         GOTO1 AADDDATA,DMCB,AFABLK,BUYEDYEL,(R0),0                             
*                                                                               
         CLI   RBUYDPT,C' '                                                     
         BNH   DCON0104                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYDPTEL,RBUYDPT,0                          
*                                                                               
DCON0104 DS    0H                                                               
         TM    RBUYFLG2,X'80'      DAILY?                                       
         BZ    DCON0106            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYDALEL,0,0                                
*                                                                               
DCON0106 DS    0H                                                               
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BNZ   DCON108A            YES                                          
*                                                                               
         MVC   HALF,RBUYDUR                                                     
         NI    HALF,X'FF'-X'80'                                                 
*                                                                               
         TM    RBUYDUR,X'80'       MINUTES?                                     
         BZ    DCON0108            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYMINEL,0,0                                
DCON0108 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYLENEL,HALF,0                             
*                                                                               
DCON108A DS    0H                                                               
         TM    MISCFLG1,MF1STWEB   STATION WEB REQUEST?                         
         BZ    DCON108B                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYMODEL,RBUYCHGI,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYREPEL,REPALPHA,0                         
         DROP  R8                                                               
*                                                                               
DCON108B DS    0H                                                               
         MVC   WORK(3),=X'FFFFFF'  GET ABSOLUTE START AND END                   
         XC    WORK+3(3),WORK+3                                                 
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'03'        EFFECTIVE DATE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   DCON01XA                                                         
*                                                                               
DCON0109 DS    0H                                                               
         USING RBUYDTEL,R8                                                      
         CLC   RBUYDTST,WORK                                                    
         BH    *+10                                                             
         MVC   WORK(3),RBUYDTST                                                 
*                                                                               
         CLC   RBUYDTED,WORK+3                                                  
         BL    *+10                                                             
         MVC   WORK+3(3),RBUYDTED                                               
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON0109                                                         
*                                                                               
DCON01XA DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYSDTEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,BUYEDTEL,WORK+3,0                           
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'04'        COMMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DCON01XC                                                         
*                                                                               
DCON01XB DS    0H                                                               
         ZIC   RF,1(R8)                                                         
         AHI   RF,-3                                                            
         EX    RF,*+4                                                           
         OC    2(0,R8),SPACES                                                   
         LA    RF,1(RF)                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYCOMEL,2(R8),(RF)                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON01XB                                                         
*                                                                               
DCON01XC DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'21'        PROGRAM ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DCON01XE                                                         
*                                                                               
DCON01XD DS    0H                                                               
         ZIC   RF,1(R8)                                                         
         AHI   RF,-3                                                            
         EX    RF,*+4                                                           
         OC    2(0,R8),SPACES                                                   
         LA    RF,1(RF)                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYPGMEL,2(R8),(RF)                         
*                                                                               
**JRD    BAS   RE,NEXTEL                                                        
**JRD    BE    DCON01XD                                                         
*                                                                               
DCON01XE DS    0H                                                               
         L     R8,AIOREC                                                        
**TEST                                                                          
***      USING RBUYREC,R8                                                       
***      CLI   RBUYKLIN,X'19'                                                   
***      BNE   *+6                                                              
***      DC    H'0'                                                             
***      DROP  R8                                                               
**TEST                                                                          
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'84'        ORD COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DCON01YC                                                         
*                                                                               
DCON01YB DS    0H                                                               
         ZIC   RF,1(R8)                                                         
         AHI   RF,-4                                                            
         EX    RF,*+4                                                           
         OC    3(0,R8),SPACES                                                   
         LA    RF,1(RF)                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYOCMEL,3(R8),(RF)                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON01YB                                                         
*                                                                               
DCON01YC DS    0H                                                               
         L     R8,AIOREC                                                        
         USING RBUYREC,R8                                                       
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BZ    DCON01ZC            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYTRDEL,0,0                                
*                                                                               
DCON01ZC DS    0H                                                               
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    DCON01ZX            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYSTYEL,RBUYSTYP,0                         
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'81'        SPORTS BUY COMMENT 1                         
         BAS   RE,GETEL                                                         
         BNE   DCON01ZH                                                         
*                                                                               
         ZIC   RF,1(R8)                                                         
         AHI   RF,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,BUYSC1EL,2(R8),(RF)                         
*                                                                               
DCON01ZH DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'82'        SPORTS BUY COMMENT 2                         
         BAS   RE,GETEL                                                         
         BNE   DCON01ZM                                                         
*                                                                               
         ZIC   RF,1(R8)                                                         
         AHI   RF,-2                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,BUYSC2EL,2(R8),(RF)                         
*                                                                               
DCON01ZM DS    0H                                                               
*                                                                               
         B     DCON0120            SKIP DAY/TIME ELEMENTS                       
*                                                                               
         DROP  R8                                                               
DCON01ZX DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'02'        DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DCON0120                                                         
*                                                                               
DCON0110 DS    0H                                                               
         USING RBUYDYEL,R8                                                      
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BDYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZIC   R1,RBUYDYIN                                                      
         SRL   R1,4                                                             
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
         GOTO1 AADDDATA,DMCB,AFABLK,BDYSDYEL,(R0),0                             
         MVC   BYTE,RBUYDYIN                                                    
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
         GOTO1 AADDDATA,DMCB,AFABLK,BDYEDYEL,(R0),0                             
         GOTO1 AADDDATA,DMCB,AFABLK,BDYDAYEL,RBUYDAYS,0                         
*                                                                               
         TM   RBUYDYT1,X'40'                                                    
         BZ   DCON0112                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDYTMXEL,RBUYDYT1,0                         
         B     DCON0116                                                         
*                                                                               
DCON0112 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDYSTMEL,RBUYDYT1,0                         
*                                                                               
         CLC   RBUYDYT2,=C'CC'                                                  
         BNE   DCON0114                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDYETMEL,=X'00C8',0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BDYECCEL,0,0                                
         B     DCON0116                                                         
*                                                                               
DCON0114 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDYETMEL,RBUYDYT2,0                         
*                                                                               
DCON0116 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDYDWTEL,RBUYDYWT,0                         
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON0110                                                         
*                                                                               
DCON0120 DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'03'        EFFECTIVE DATE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   DCON0140                                                         
*                                                                               
DCON0130 DS    0H                                                               
         USING RBUYDTEL,R8                                                      
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BDTDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RBUYDTST),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,BDTSDTEL,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RBUYDTED),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,BDTEDTEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,BDTWKSEL,RBUYDTWK,0                         
*                                                                               
         TM    RBUYDTIN,X'80'                                                   
         BZ    DCON0131                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTEWKEL,0,0                                
*                                                                               
DCON0131 DS    0H                                                               
         TM    RBUYDTIN,X'40'                                                   
         BZ    DCON0132                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTAWKEL,0,0                                
*                                                                               
DCON0132 DS    0H                                                               
         TM    RBUYDTIN,X'01'                                                   
         BZ    DCON0133                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTSPWEL,RBUYDTNW,0                         
         B     DCON0134                                                         
*                                                                               
DCON0133 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTSPWEL,BYTE2,0                            
*                                                                               
DCON0134 DS    0H                                                               
         DROP  R8,R6                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON0130                                                         
*                                                                               
DCON0140 DS    0H                                                               
*                                                                               
DCON0190 DS    0H                                                               
         GOTO1 VSEQ                                                             
         B     DCON0101                                                         
*                                                                               
DCONDBYX DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
DAYBITS  DC    X'40201008040201'   DAYVAL BITS M-SU                             
         LTORG                                                                  
         EJECT                                                                  
*....................................................................           
*                                                                               
* STACHK                                                                        
*    R8 -> CONTRACT RECORD                                                      
*                                                                               
*    CHECK TO SEE IF STATION IS VALID                                           
*....................................................................           
STACHK   NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R8                                                            
         USING RCONREC,R3                                                       
         L     R4,ATWA                                                          
         USING TWAD,R4                                                          
*                                                                               
         CLI   TWAACCS,C'$'        ONLY CHECK FOR STATION USER                  
         BNE   EXITOK                                                           
*                                                                               
         XC    KEY,KEY                                                          
K        USING RST9KEY,KEY                                                      
         MVI   K.RST9KTYP,X'83'    SET RECORD ID                                
         MVI   K.RST9KSTP,X'07'    SUB ID                                       
         MVC   K.RST9KREP,REPALPHA REP CODE                                     
         MVC   K.RST9KSIG,TWAUSRID SIGN ON USER ID                              
         MVC   K.RST9KSTA,RCONKSTA STATION CALL LETTERS                         
         DROP  R4                                                               
         DROP  K                                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     STATION EXIT?                                
         BE    EXITOK              YES, EXIT W/OUT ERROR                        
         L     R5,ADDR             INSERT ERROR CODE                            
         MVC   0(2,R5),=Y(150)     AND                                          
         B     EXITL               EXIT                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================*          
*                                                                               
* INTERESTING DATA STRINGS FOR XARSE                                            
*                                                                               
*  AL4(HEADER ELEMENT)                                                          
*    AL1(DATA EQUATE), XARSE MODE  0 - DEFAULT                                  
*                                  1 - DON'T SEND UNLESS > SPACE                
*                                  2 - MAPCODE ONLY                             
*    AL4(MAPTABLE ENTRY FOR DATA)                                               
*                                                                               
SALTBL   DC    A(SALDATA)                                                       
FOO      DC    AL1(QSALKSAL,0),A(SALCODEL)                                      
XTABLQ   EQU   *-FOO                                                            
         DC    AL1(QSALNAME,0),A(SALNAMEL)                                      
         DC    AL1(QSALTEL,0),A(SALTELEL)                                       
         DC    AL1(QSALFAX,0),A(SALFAXEL)                                       
         DC    AL1(QSALTEAM,0),A(SALTEMEL)                                      
         DC    AL1(QTEMDVNM,0),A(SALDIVEL)                                      
         DC    AL1(QTEMNAME,0),A(SALTNMEL)                                      
         DC    AL1(QSALEMAL,0),A(SALEMLEL)                                      
         DC    AL1(QSALOFF,0),A(SALOFFEL)                                       
         DC    AL1(QSALOFFN,0),A(SALOFNEL)                                      
         DC    AL1(QSALMGR,2),A(SALMGREL)                                       
         DC    AL1(QSALADD0,0),A(SALADDEL)                                      
         DC    AL1(QSALADD1,0),A(SALADDEL)                                      
         DC    AL1(0)                                                           
CTYTBL   DC    A(CTYDATA)                                                       
         DC    AL1(QCTYKCTY,0),A(CTYCTYEL)                                      
         DC    AL1(QCTYDESC,0),A(CTYDSCEL)                                      
         DC    AL1(0)                                                           
DCTTBL   DC    A(DCTDATA)                                                       
         DC    AL1(QDCTKCTY,0),A(DCTCTYEL)                                      
         DC    AL1(QDCTNAME,0),A(DCTNAMEL)                                      
         DC    AL1(0)                                                           
DSPTBL   DC    A(DSPDATA)                                                       
         DC    AL1(QDSPKSAL,0),A(DSPCODEL)                                      
         DC    AL1(QDSPNAME,0),A(DSPNAMEL)                                      
         DC    AL1(QDSPTEL,0),A(DSPTELEL)                                       
         DC    AL1(QDSPFAX,0),A(DSPFAXEL)                                       
         DC    AL1(0)                                                           
STATBL   DC    A(STADATA)                                                       
         DC    AL1(QSTAKSTA,0),A(STASTAEL)                                      
         DC    AL1(QSTAMKT,0),A(STAMKTEL)                                       
         DC    AL1(QSTAAFFL,0),A(STAAFFEL)                                      
         DC    AL1(QSTACHAN,0),A(STACHNEL)                                      
         DC    AL1(0)                                                           
ADVTBL   DC    A(ADVDATA)                                                       
         DC    AL1(QADVKADV,0),A(ADVCODEL)                                      
         DC    AL1(QADVNAME,0),A(ADVNAMEL)                                      
         DC    AL1(0)                                                           
AGYTBL   DC    A(AGYDATA)                                                       
         DC    AL1(QAGYKAGY,0),A(AGYCODEL)                                      
         DC    AL1(QAGYKAOF,1),A(AGYOFFEL)                                      
         DC    AL1(QAGYNAME,0),A(AGYNAMEL)                                      
         DC    AL1(0)                                                           
AGADTBL  DC    A(0)                                                             
         DC    AL1(QAGADLN0,0),A(AGYAD0EL)                                      
         DC    AL1(QAGADLN1,0),A(AGYAD1EL)                                      
         DC    AL1(QAGADLN2,0),A(AGYAD2EL)                                      
         DC    AL1(QAGADLN3,0),A(AGYAD3EL)                                      
         DC    AL1(QAGADNAM,0),A(AGYONMEL)                                      
         DC    AL1(0)                                                           
PRDTBL   DC    A(PRDDATA)                                                       
         DC    AL1(QPRDKPRD,1),A(PRDCODEL)                                      
         DC    AL1(QPRDNAME,0),A(PRDNAMEL)                                      
         DC    AL1(0)                                                           
DPTTBL   DC    A(DPTDATA)                                                       
         DC    AL1(QDPTCODE,0),A(DPTCODEL)                                      
         DC    AL1(QDPTSNAM,0),A(DPTSNMEL)                                      
         DC    AL1(QDPTLNAM,0),A(DPTLNMEL)                                      
         DC    AL1(0)                                                           
         SPACE 3                                                                
*---------------------------------------------                                  
* THESE ARE USED DURING BCONDET                                                 
*---------------------------------------------                                  
SALBCD   DC    A(SALDATA)                                                       
         DC    AL1(QSALNAME,0),A(SALNAMEL)                                      
         DC    AL1(QSALTEL,0),A(SALTELEL)                                       
         DC    AL1(QSALFAX,0),A(SALFAXEL)                                       
         DC    AL1(QSALEMAL,0),A(SALEMLEL)                                      
         DC    AL1(0)                                                           
ADVBCD   DC    A(ADVDATA)                                                       
         DC    AL1(QADVNAME,0),A(ADVNAMEL)                                      
         DC    AL1(0)                                                           
AGYBCD   DC    A(AGYDATA)                                                       
         DC    AL1(QAGYKAGY,0),A(AGYCODEL)                                      
         DC    AL1(QAGYKAOF,1),A(AGYOFFEL)                                      
         DC    AL1(QAGYNAME,0),A(AGYNAMEL)                                      
         DC    AL1(0)                                                           
PRDBCD   DC    A(PRDDATA)                                                       
         DC    AL1(QPRDNAME,0),A(PRDNAMEL)                                      
         DC    AL1(0)                                                           
         SPACE 3                                                                
*====================================================================*          
*                                                                               
* ROUTINE LIST FOR REQUEST HEADERS                                              
*                                                                               
*   AL2 - HEADER EQUATE NUMBER                                                  
*   AL4 - ADDRESS OF DOWNLOAD ROUTINE                                           
*   AL4 - ADDRESS OF FIELD LIST FOR REQUEST                                     
*   AL4 - ADDRESS OF OPTIONAL EXTRA SETUP ROUTINE                               
*                                                                               
*                                                                               
REQHDRS  DS    0A                                                               
         DC    AL2(INITHDRQ),A(INITDWN),A(INITFLDS),A(0)                        
ROUTABLQ EQU   *-REQHDRS                                                        
         DC    AL2(BCONHDRQ),A(BCONDWN),A(BCONFLDS),A(0)                        
         DC    AL2(LCONHDRQ),A(LCONDWN),A(LCONFLDS),A(0)                        
         DC    AL2(DCONHDRQ),A(DCONDWN),A(DCONFLDS),A(0)                        
         DC    AL2(UCONHDRQ),A(UCONDWN),A(UCONFLDS),A(0)                        
         DC    AL2(RTGSHDRQ),A(RTGSDWN),A(RTGSFLDS),A(0)                        
         DC    AL2(VHDRHDRQ),A(VHDRDWN),A(VHDRFLDS),A(0)                        
**       DC    AL2(VBKSHDRQ),A(VBKSDWN),A(VBKSFLDS),A(0)                        
         DC    AL2(FVERHDRQ),A(0),A(FVERFLDS),A(FVERHDR)                        
         DC    AL2(FVE2HDRQ),A(0),A(FVERFLDS),A(FVERHDR)                        
         DC    AL2(0000)                                                        
*====================================================================*          
*                                                                               
* FIELD ROUTINE LIST FOR VERSION INFO                                           
*                                                                               
FVERFLDS DS    0A                                                               
         DC    AL2(FVERVERQ),A(FVERVER)                                         
FLDTABLQ EQU   *-FVERFLDS                                                       
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR INIT REQUEST                                           
*                                                                               
INITFLDS DS    0A                                                               
         DC    AL2(INITSALQ),A(INITSAL)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR LIST CONTRACT REQUEST                                  
*                                                                               
LCONFLDS DS    0A                                                               
         DC    AL2(LCSALQ),A(LCSAL)                                             
         DC    AL2(LCSTAQ),A(LCSTA)                                             
         DC    AL2(LCFLTSTQ),A(LCFLTST)                                         
         DC    AL2(LCFLTENQ),A(LCFLTEN)                                         
         DC    AL2(LCAGYQ),A(LCAGY)                                             
         DC    AL2(LCAOFQ),A(LCAOF)                                             
         DC    AL2(LCADVQ),A(LCADV)                                             
         DC    AL2(LCSTWQ),A(STAWEB)                                            
         DC    AL2(LC#ONQ),A(CONONLY)                                           
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR STATION CONTRACT REQUEST                               
*                                                                               
BCONFLDS DS    0A                                                               
         DC    AL2(BCFLTSTQ),A(LCFLTST)                                         
         DC    AL2(BCFLTENQ),A(LCFLTEN)                                         
         DC    AL2(BCAGYQ),A(LCAGY)                                             
         DC    AL2(BCAOFQ),A(LCAOF)                                             
         DC    AL2(BCADVQ),A(LCADV)                                             
         DC    AL2(BCSTWQ),A(STAWEB)                                            
         DC    AL2(BC#ONQ),A(CONONLY)                                           
         DC    AL2(BCDETQ),A(CONDET)                                            
         DC    AL2(BCSDAQ),A(BCSDAT)                                            
         DC    AL2(BCSTMQ),A(BCSTIM)                                            
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR RATING DOWNLOAD REQUEST                                
*                                                                               
RTGSFLDS DS    0A                                                               
         DC    AL2(RTRTSRVQ),A(RTRTSRV)                                         
         DC    AL2(RTTPBKQ),A(RTBOOK)                                           
         DC    AL2(RTT4BKQ),A(RTBOOK)                                           
         DC    AL2(RTPVBKQ),A(RTBOOK)                                           
         DC    AL2(RTTPUPGQ),A(RTUPG)                                           
         DC    AL2(RTT4UPGQ),A(RTUPG)                                           
         DC    AL2(RTPVUPGQ),A(RTUPG)                                           
         DC    AL2(RTDEMOQ),A(RTDEMO)                                           
         DC    AL2(RTSTAQ),A(RTSTA)                                             
         DC    AL2(RTDAYQ),A(RTDAY)                                             
         DC    AL2(RTSTIMQ),A(RTSTIM)                                           
         DC    AL2(RTETIMQ),A(RTETIM)                                           
         DC    AL2(RTXTIMQ),A(RTETIM)                                           
         DC    AL2(RTDATEQ),A(RTDATE)                                           
         DC    AL2(RTEMRKQ),A(RTEMRK)                                           
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR VALIDATE HEADER                                        
*   AND BOOKS/UPGRADES/DEMOS                                                    
*                                                                               
VHDRFLDS DS    0A                                                               
         DC    AL2(VBRTSRVQ),A(VBRTSRV)                                         
         DC    AL2(VBBOOKQ),A(VBBOOK)                                           
         DC    AL2(VBUPGRDQ),A(VBUPGRD)                                         
         DC    AL2(VBDEMOQ),A(VBDEMO)                                           
         DC    AL2(VBNIUPGQ),A(VBUPGRD)                                         
         DC    AL2(VBUPGNMQ),A(VBUPGNM)                                         
         DC    AL2(VHSTAQ),A(VHSTA)                                             
         DC    AL2(VHADVQ),A(VHADV)                                             
         DC    AL2(VHPRDQ),A(VHPRD)                                             
         DC    AL2(VHAGYQ),A(VHAGY)                                             
         DC    AL2(VHAOFQ),A(VHAOF)                                             
         DC    AL2(VHFLSQ),A(VHFLS)                                             
         DC    AL2(VHFLEQ),A(VHFLE)                                             
         DC    AL2(VHCTYPQ),A(VHCTYP)                                           
         DC    AL2(VHDCTQ),A(VHDCT)                                             
         DC    AL2(VHDSPQ),A(VHDSP)                                             
         DC    AL2(VHSALQ),A(VHSAL)                                             
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR DOWNLOAD CONTRACT HEADER FIELDS                        
*                                                                               
DCONFLDS DS    0A                                                               
         DC    AL2(DCONCONQ),A(DCONCON)                                         
         DC    AL2(DCONBUYQ),A(DCONBUY)                                         
         DC    AL2(DCONPSDQ),A(DCONPSD)                                         
         DC    AL2(DCONPEDQ),A(DCONPED)                                         
         DC    AL2(DCONSTWQ),A(STAWEB)                                          
         DC    AL2(DCONNODQ),A(NODETAIL)                                        
         DC    AL2(DCONNOCQ),A(NOORDCOM)                                        
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR UPDATE CONTRACT FIELDS                                 
*                                                                               
UCONFLDS DS    0A                                                               
         DC    AL2(UCONCONQ),A(UCONCON)                                         
         DC    AL2(UCONACTQ),A(UCONACT)                                         
         DC    AL2(UCONVERQ),A(UCONVER)                                         
         DC    AL2(UCONOCMQ),A(UCONOCM)                                         
         DC    AL2(UCONLINQ),A(UCONLIN)                                         
         DC    AL2(UCONBOCQ),A(UCONBOC)                                         
         DC    AL2(UCONEOBQ),A(UCONEOB)                                         
         DC    AL2(0000)                                                        
*                                                                               
*                                                                               
*                                                                               
*                                                                               
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(0)              <=== TWABLD                                  
         DC    AL1(0)              <=== UNBOOK                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QBOOKVAL)       <=== BUT THIS EQUATE IS ALSO 0               
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QREFETCH)                                                    
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QFALINK)                                                     
         DC    AL1(QREPFACS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
PHASESN  EQU   *-PHASES                                                         
         EJECT                                                                  
         EJECT                                                                  
KEYSAV2  DS    CL48                TEMP KEY                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE REWCFWRK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENDAR                                                       
         SPACE 3                                                                
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                 BINARY MONTH START DATE                      
BRDEND   DS    XL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    XL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019REWCF00S  06/03/04'                                      
         END                                                                    
