*          DATA SET RETST00    AT LEVEL 107 AS OF 08/31/00                      
*          DATA SET RETST00    AT LEVEL 106 AS OF 07/05/00                      
*PHASE T81C00A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE UNBOOK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE REPRPUPV                                                               
         TITLE 'RETST00(T81C00) - REP MODULE TESTER'                            
*                                                                               
*                                                                               
* JUL05/00 (BU ) REMOVE REFERENCES TO GLV1GOTO PER MEL H.                       
*                                                                               
*                                                                               
* THIS VERSION IS FOR QUICK AND DIRTY STUB TESTING OF ANYTHING THAT             
*    COMES ALONG                                                                
*                                                                               
*                                                                               
*********************************************************************           
* ALL ROUTINES SHOULD FOLLOW USE THE FOLLOWING OUTPUT FORMAT:                   
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
*********************************************************************           
T81C00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T81C00,RR=R2,CLEAR=YES                                   
         LR    R7,RB                                                            
         AH    R7,=Y(COMMON-T81C00)                                             
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
         USING T81CFFD,RA                                                       
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
         AH    RE,=Y(VROUTS-T81C00)                                             
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
         DROP  R6                                                               
*                                                                               
         GOTO1 GETPROF,DMCB,('RREPQSEL',SELPROFS)                               
         GOTO1 GETPROF,DMCB,('RREPQCNT',CONPROFS)                               
*                                                                               
         BAS   RE,CHKGLOB                                                       
*                                                                               
         LA    RE,3000(RA)                                                      
         LH    RF,=Y(10000)                                                     
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0               CLEAR ALOT OF TWA SPACE                      
*                                                                               
*&&DO                                                                           
T        USING TWAD,RA                                                          
         CLI   T.TWAOFFC,C'*'      DDS TERMINAL?                                
         BE    MAIN                YES DON'T DO FALINK(STUB MODE)               
         DROP  T                                                                
*                                                                               
         BAS   RE,INIFALNK         INITIALIZE FALINK BLOCK                      
         GOTO1 VFALINK,DMCB,FABLK  GIVE FALINK CONTROL                          
         B     EXIT                                                             
*                                                                               
*---------------------------*                                                   
* STUB TESTING PROGRAM LOOP *                                                   
*---------------------------*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR,DMCB                                                        
*&&                                                                             
*                                                                               
MAIN     DS    0H                                                               
         USING PSBLOCK,WORK                                                     
*                                                                               
*********  PASS 1                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   PSBPGM,=C'CON'                                                   
         MVC   PSBACT,=X'000000'                                                
         MVI   PSBSEC,X'50'                                                     
         MVC   PSBXACT,=CL10'DIS    '                                           
         MVC   HALF,=C'B4'                                                      
         BAS   RE,TESTSEC                                                       
*                                                                               
*********  PASS 2                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   PSBPGM,=C'CON'                                                   
         MVC   PSBACT,=X'000000'                                                
         MVI   PSBSEC,X'20'                                                     
         MVC   PSBXACT,=CL10'CHA   '                                            
         MVC   HALF,=C'B4'                                                      
         BAS   RE,TESTSEC                                                       
*&&DO                                                                           
*********  PASS 2                                                               
*                                                                               
         XC    WORK2,WORK2                                                      
         MVI   WORK2,C'-'                                                       
         GOTO1 PRINTOUT,DMCB,WORK2                                              
         XC    WORK,WORK                                                        
         MVC   SBGROUP,=C'R     '                                               
         MVC   SBOFFICE,=C'      '                                              
         MVC   SBSTATN,=C'WCBS  '                                               
         MVC   SBOWNER,=C'      '                                               
         MVC   SBMARKET,=C'      '                                              
         MVC   SBREGION,=C'      '                                              
         MVC   SBSALES,=C'      '                                               
         MVC   SBBREAKS(4),=X'00000000'                                         
         MVC   HALF,=C'B4'                                                      
         BAS   RE,TESTSEC                                                       
*                                                                               
*********  PASS 3                                                               
*                                                                               
         XC    WORK2,WORK2                                                      
         MVI   WORK2,C'-'                                                       
         GOTO1 PRINTOUT,DMCB,WORK2                                              
         XC    WORK,WORK                                                        
         MVC   SBGROUP,=C'R     '                                               
         MVC   SBOFFICE,=C'CH    '                                              
         MVC   SBSTATN,=C'WABC  '                                               
         MVC   SBOWNER,=C'      '                                               
         MVC   SBMARKET,=C'      '                                              
         MVC   SBREGION,=C'      '                                              
         MVC   SBSALES,=C'      '                                               
         MVC   SBBREAKS(4),=X'20000000'                                         
         MVC   HALF,=C'B4'                                                      
         BAS   RE,TESTSEC                                                       
*                                                                               
*********  PASS 4                                                               
*                                                                               
         XC    WORK2,WORK2                                                      
         MVI   WORK2,C'-'                                                       
         GOTO1 PRINTOUT,DMCB,WORK2                                              
         XC    WORK,WORK                                                        
         MVC   SBGROUP,=C'      '                                               
         MVC   SBOFFICE,=C'CH    '                                              
         MVC   SBSTATN,=C'WABC  '                                               
         MVC   SBOWNER,=C'      '                                               
         MVC   SBMARKET,=C'      '                                              
         MVC   SBREGION,=C'      '                                              
         MVC   SBSALES,=C'      '                                               
         MVC   SBBREAKS(4),=X'20000000'                                         
         MVC   HALF,=C'B4'                                                      
         BAS   RE,TESTSEC                                                       
*&&                                                                             
ENDMAIN  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         EJECT                                                                  
********************************************************************            
COMMON   DS    0D                                                               
       ++INCLUDE REPRPMAP                                                       
         EJECT                                                                  
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    DS    0H                  SET CC LOW & FAMSGNO                         
         MVC   FAMSGBLK+(FAMSGNO-FAMSGD)(L'FAMSGNO),ERROR                       
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
         GETEL R8,=Y(RCONELEM-RCONREC),ELCODE                                   
*                                                                               
         LTORG                                                                  
***********************************************************************         
* FIND THE SPOT WHERE AND ELEMENT GOES                                          
***********************************************************************         
FINDSPOT DS    0H                                                               
         L     R1,AIOREC                                                        
         LA    R1,RCONELEM-RCONKEY(R1)               WHERE DOES IT GO?          
FINDSPT1 CLI   0(R1),0             END OF RECORD?                               
         BER   RE                  YES                                          
         CLC   ELCODE,0(R1)        IS THIS THE PLACE?                           
         BLR   RE                  YES                                          
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FINDSPT1                                                         
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
         LA    RF,ROUTABLQ(RF)                                                  
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,4(RF)         ROUTINE TO HANDLE THE VERB                   
         A     RF,BASERELO                                                      
         BR    RF                                                               
*                                                                               
ITER04   DS    0H                                                               
         MVC   ERROR,=Y(45)                                                     
         B     EXITL                                                            
*                                                                               
*                                                                               
*                                                                               
TESTSEC  NTR1                                                                   
         XC    WORK2,WORK2                                                      
         MVC   WORK2(55),=C'PG AC S ACTDESC                           1+        
                2 3 4   REP'                                                    
         GOTO1 PRINTOUT,DMCB,WORK2                                              
         XC    WORK2,WORK2                                                      
         MVC   WORK2(17),WORK                                                   
         GOTO1 VHEXOUT,DMCB,SBBREAKS,WORK2+(SBBREAKS-SBLOCK),4                  
         MVC   WORK2+(SBBREAKS-SBLOCK)+10(2),HALF                               
         GOTO1 PRINTOUT,DMCB,WORK2                                              
*                                                                               
         LA    R2,CONINPH          1ST OUTPU FIELD                              
TSEC004  DS    0H                                                               
         OC    8(L'CONINP,R2),8(R2) THIS LINE USED?                             
         BZ    TSEC005             NO - USE IT                                  
         ZIC   RF,0(R2)            YES - GET NEXT ONE                           
         AR    R2,RF                                                            
         B     TSEC004                                                          
*                                                                               
TSEC005  DS    0H                                                               
         XC    WORK2,WORK2                                                      
         MVC   WORK2(37),=C'RETURN P1:           RETURN CC: EQUAL'              
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),HALF                                                    
         GOTOX (RFCKPSEC,VREPFACS),DMCB,WORK,(R2),ATIOB,DUB                     
         BE    *+10                                                             
         MVC   WORK2+32(9),=C'NOT EQUAL'                                        
         MVC   FULL,0(R1)                                                       
         GOTO1 VHEXOUT,DMCB,FULL,WORK2+11,4                                     
         GOTO1 PRINTOUT,DMCB,WORK2                                              
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* PARSE GENERIC RETURN FOR DISPLACEMNTS TO GIVEN DATA TYPES                     
*                                                                               
*  P1 - A(GENERIC OUTPUT STREAM)                                                
*  P2 - A(NULL TERMINATED DATA TYPE STRING)                                     
*                                                                               
* OUTPUT:                                                                       
*  P1    - A(START OF DATA)                                                     
*  HALF  - LENGTH OF SINGLE ENTRY                                               
*  WORK2 - ORDERED LIST OF XL1 DATA LENGTH, XL2 DISPLACEMENT TO DATA            
*                                                                               
***********************************************************************         
PARSE    NTR1                                                                   
         L     R5,0(R1)            A(OUTPUT STREAM)                             
         L     R6,4(R1)            A(DATA TYPES)                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,2(R5)                                                       
         BZ    EXITL               NO SUBDIVISIONS                              
*                                                                               
         XC    WORK2,WORK2         KEEP DISPLACEMENTS TO DATA HERE              
         MVC   HALF,0(R5)          LENGTH OF SINGLE ENTRY                       
*                                                                               
         LA    R5,3(R5)                                                         
         SR    RF,RF                                                            
PARSE010 DS    0H                                                               
         LR    RE,R6               CHECK FOR DATA IN FOOTAB                     
PARSE012 DS    0H                                                               
         CLI   0(RE),0             END OF TYPES?                                
         BE    PARSE030            YES                                          
         CLC   0(1,RE),1(R5)       TYPE MATCH?                                  
         BE    PARSE020            YES                                          
         LA    RE,1(RE)                                                         
         B     PARSE012                                                         
*                                                                               
PARSE020 DS    0H                                                               
         SR    RE,R6               INDEX TO DATA TYPE                           
         MHI   RE,3                                                             
         LA    RE,WORK2(RE)        BUMP INTO WORK2                              
         MVC   0(1,RE),0(R5)       STORE DATA LENGTH                            
         STCM  RF,3,1(RE)          STORE DISP. TO DATA                          
*                                                                               
PARSE030 DS    0H                                                               
         ZIC   RE,0(R5)            INCREMENT DISPLACEMENT                       
         AR    RF,RE                                                            
         LA    R5,2(R5)            NEXT ENTRY                                   
         BCT   R0,PARSE010                                                      
*                                                                               
         ST    R5,0(R1)                                                         
*                                                                               
PARSEX   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
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
         AH    R1,=Y(SVFALINK-T81CFFD) A(FALINK SAVED STORAGE)                  
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
         BO    EXITL               NO CONTROL ELEM                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =C'CON',WORK2+(GLVXFRPR-GLVXFRSY)                                
         BNE   EXITL               NOT FROM CONTRACT                            
*                                                                               
* CHECK FOR RETURN CONTRACT AUTOGEN ELEMENT                                     
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK2,RCAUELLQ,GLRCAUTO                   
         TM    DMCB+8,X'10'                                                     
         BNZ   EXITL               NO AUTOGEN ELEMENT                           
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLRCAUTO                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    MISCFLG1,MF1GLOB                                                 
         LA    R0,*                                                             
         A     R0,=A(GLOBDWN-(*-4))                                             
         ST    R0,SENDROUT         GENERIC GLOBBER DOWNLOAD                     
*                                                                               
W        USING RCAUTOD,WORK2                                                    
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,W.RCAUCON#                                                   
         CVB   R0,DUB                                                           
         STCM  R0,15,CONNUM                                                     
         MVC   CONERR,W.RCAUERR#                                                
*                                                                               
         B     EXITOK                                                           
         DROP  W                                                                
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
         EJECT                                                                  
********************************************************************            
* PRINT ROUTINE OUPUT AREA                                                      
*                                                                               
*   P1 - A(OUTPUT AREA)                                                         
*                                                                               
********************************************************************            
PRINTOUT NTR1                                                                   
         L     R3,0(R1)                                                         
         LA    R2,CONINPH          1ST OUTPU FIELD                              
PRO0004  DS    0H                                                               
         OC    8(L'CONINP,R2),8(R2) THIS LINE USED?                             
         BZ    PRO0005             NO - USE IT                                  
         ZIC   RF,0(R2)            YES - GET NEXT ONE                           
         AR    R2,RF                                                            
         B     PRO0004                                                          
*                                                                               
PRO0005  DS    0H                                                               
         LA    R6,8(R2)                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   9(60,R2),0(R3)                                                   
         MVI   5(R2),60                                                         
         B     EXITOK                                                           
*&&DO                                                                           
PRO0007  DS    0H                                                               
         EDIT  (B2,0(R3)),(4,0(R6)),ZERO=NOBLANK                                
         LA    R6,5(R6)                                                         
         EDIT  (B1,2(R3)),(3,0(R6)),ZERO=NOBLANK                                
         LA    R6,4(R6)                                                         
*                                                                               
         LA    R5,3(R3)                                                         
         SR    R4,R4                                                            
         ICM   R4,1,2(R3)                                                       
         BZ    PRO0020             NO SUBDIVISIONS                              
         B     PRO0015                                                          
*                                                                               
PRO0010  DS    0H                                                               
         LA    RE,8(R2)                                                         
         LA    RE,74(RE)                                                        
         CR    R6,RE               ROOM LEFT ON LINE?                           
         BL    PRO0015             YES                                          
         ZIC   RE,0(R2)            NO - TIME FOR NEW LINE                       
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
         LA    R6,17(R2)                                                        
*                                                                               
PRO0015  DS    0H                                                               
         EDIT  (B1,0(R5)),(3,0(R6)),ZERO=NOBLANK                                
         MVC   4(1,R6),1(R5)                                                    
         LA    R6,5(R6)                                                         
         LA    R5,2(R5)                                                         
         BCT   R4,PRO0010                                                       
*                                                                               
PRO0020  DS    0H                                                               
         ZICM  RF,0(R3),2          LEN OF DATA                                  
PRO0021  DS    0H                                                               
         LA    RE,87(R2)                                                        
         SR    RE,R6               ROOM LEFT ON LINE                            
         CR    RF,RE                                                            
         BNH   PRO0025             IT ALL FITS                                  
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R5)       MOVE WHAT FITS                               
         LA    RE,1(RE)            UNDO BCTR                                    
         AR    R5,RE               POINT TO REST OF DATA TO OUTPUT              
         SR    RF,RE               NEW LENGTH OF DATA LEFT                      
*                                                                               
         ZIC   RE,0(R2)            NO - TIME FOR NEW LINE                       
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
         LA    R6,17(R2)                                                        
         B     PRO0021                                                          
*                                                                               
PRO0025  DS    0H                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R5)                                                    
         B     EXITOK                                                           
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
********************************************************************            
*                  COMMUNICAION WITH DATA MANAGER (DIRECTORY)                   
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
********************************************************************            
* LITERALS AND CONSTANTS                                                        
********************************************************************            
FF       EQU   X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
BREAK    NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
RESUME   NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
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
         EJECT                                                                  
***********************************************************************         
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESSE                 
         XC    AFLDTAB,AFLDTAB                                                  
         XC    SENDROUT,SENDROUT                                                
*                                                                               
         TM    MISCFLG1,MF1GLOB    FROM GLOBBER?                                
         BZ    *+6                                                              
         DC    H'0'                YES - SHOULD NOT GET HERE                    
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
         ICM   R1,3,MHCODE                                                      
         B     ITER                                                             
         EJECT                                                                  
*.....................................................................          
* INITHDR - PROCESS INITIAL DOWNLOAD REQUEST                                    
*.....................................................................          
INITHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(INITFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(INITDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF           CLEAR LAST FIELD SEEN                        
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* NINVHDR - PROCESS NEW INVENTORY REQUEST                                       
*.....................................................................          
NINVHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(NINVFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(NINVDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF           CLEAR LAST FIELD SEEN                        
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* NINVHDR - PROCESS NEW DATA FOR INVENTORY REQUEST                              
*.....................................................................          
NDATHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(NDATFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(NDATDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
         NI    MISCFLG1,FF-MF1TXT     GET BOOKS/DEMOS/RATES NOT TEXT            
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* NTXTHDR - PROCESS NEW MARKET STATION TEXT FOR STATION REQUEST                 
*.....................................................................          
NTXTHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(NTXTFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(NTXTDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
         NI    MISCFLG1,FF-MF1MKT     GET STATION TEXT BY DEFAULT               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* CSTAHDR - PROCESS COMPETITIVE STATION REQUEST                                 
*.....................................................................          
CSTAHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(CSTAFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(CSTADWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* VHDRHDR - PROCESS VALIDATE HEADER REQUEST                                     
*.....................................................................          
VHDRHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(VHDRFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(VHDRDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* VBUDHDR - PROCESS VALIDATE BOOK/UPGRADE/DEMO REQUEST                          
*.....................................................................          
VBUDHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(VBUDFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(VBUDDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* RCHFHDR - PROCESS REFRESH CONTRACT HEADER FIELDS REQUEST                      
*.....................................................................          
RCHFHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(RCHFFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(RCHFDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* ACONHDR - PROCESS ADD CONTRACT REQUEST                                        
*.....................................................................          
ACONHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(ACONFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(ACONDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* CCONHDR - PROCESS CHANGE CONTRACT REQUEST                                     
*.....................................................................          
CCONHDR  DS    0H                                                               
         LA    R0,*                                                             
         A     R0,=A(CCONFLDS-(*-4))                                            
         ST    R0,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
         LA    R0,*                                                             
         A     R0,=A(CCONDWN-(*-4))                                             
         ST    R0,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF              CLEAR LAST FIELD SEEN                     
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         DROP  R6                                                               
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
         EJECT                                                                  
         LTORG                                                                  
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
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* NEW INVENTORY REQUEST - RATING SERVICE FIELD                                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
NIRTSRV  LR    RB,RF                                                            
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
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - STATION FIELD                                         
*.....................................................................          
         USING *,RB                                                             
NISTA    LR    RB,RF                                                            
         CLC   HALF,MDCODE         FIRST STATION FIELD?                         
         BE    NISTA010            NO                                           
         CLC   =Y(NIRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF STATIONS                       
         LA    RE,1(RE)            NUMBER OF STATIONS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
NISTA010 DS    0H                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - DAYPART FIELD                                         
*.....................................................................          
         USING *,RB                                                             
NIDPT    LR    RB,RF                                                            
         CLC   HALF,MDCODE         FIRST DAYPART FIELD?                         
         BE    NIDPT010            NO                                           
         CLC   =Y(NISTAQ),HALF     LAST FIELD A STATION?                        
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF DAYPARTS                       
         LA    RE,1(RE)            NUMBER OF DAYPARTS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
NIDPT010 DS    0H                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(1,RE),0(RF)       COPY DAYPART                                 
         OC    0(1,RE),SPACES                                                   
         LA    RE,1(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - FLIGHT START FIELD                                    
*.....................................................................          
         USING *,RB                                                             
NIFLTST  LR    RB,RF                                                            
         CLC   =Y(NIFLTENQ),HALF   LAST FIELD AN END FLIGHT?                    
         BE    NIFLS010            YES                                          
         CLC   =Y(NIDPTQ),HALF     LAST FIELD A DAYPART?                        
         BNE   EPARMSEQ                                                         
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF FLIGHTS                        
         LA    RE,1(RE)            NUMBER OF FLIGHTS GOES HERE                  
         ST    RE,ADDR                                                          
*                                                                               
NIFLS010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(3,RE),0(RF)       COPY FLIGHT START                            
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - FLIGHT END FIELD                                      
*.....................................................................          
         USING *,RB                                                             
NIFLTEN  LR    RB,RF                                                            
         CLC   =Y(NIFLTSTQ),HALF   LAST FIELD A START FLIGHT?                   
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   3(3,RE),0(RF)       COPY FLIGHT END                              
         LA    RE,6(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - BOOK FIELD(S)                                         
*.....................................................................          
         USING *,RB                                                             
NIBOOK   LR    RB,RF                                                            
         CLC   HALF,=Y(NIBOOKQ)    FIRST BOOK FIELD?                            
         BE    NIBK0010            NO                                           
         CLC   =Y(NIFLTENQ),HALF   LAST FIELD AN END FLIGHT?                    
         BNE   EPARMSEQ                                                         
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF BOOKS                          
         LA    RE,1(RE)            NUMBER OF BOOKS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NIBK0010 DS    0H                                                               
         MVC   HALF,=Y(NIBOOKQ)                                                 
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(232)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVI   3(RE),C'I'          SET DEMO FILE(INV IS DEFAULT)                
         CLC   MDCODE,=Y(NITPBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(NIT4BKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(NIPVBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         MVC   4(1,RE),FULL        SET BOOK TYPE                                
*                                                                               
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - UPGRADE FIELD(S)                                      
*.....................................................................          
         USING *,RB                                                             
NIUPGRD  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(NIUPGRDQ)   FIRST UPGRADE FIELD?                         
         BE    NIUPG010            NO                                           
         CLC   =Y(NIBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    NIUPG002            YES                                          
         CLC   =Y(NIFLTENQ),HALF   LAST FIELD AN END FLIGHT?                    
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
NIUPG002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF UPGRADES                       
         LA    RE,1(RE)            NUMBER OF UPGRADES GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
NIUPG010 DS    0H                                                               
         MVC   HALF,=Y(NIUPGRDQ)                                                
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
         CLC   MDCODE,=Y(NIUPGRDQ)                                              
         BNE   NIUPG020                                                         
*                                                                               
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BZ    EINVUPG             NO                                           
*                                                                               
         MVI   3(RE),C'I'                                                       
         B     NIUPG022                                                         
*                                                                               
NIUPG020 DS    0H                                                               
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BNZ   EINVUPG             YES                                          
*                                                                               
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(NIT4UPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(NIPVUPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         DROP  UE                                                               
*                                                                               
NIUPG022 DS    0H                                                               
         MVC   0(3,RE),WORK+120+15         1ST BASE BOOKS                       
         MVC   4(1,RE),WORK+120+14         SPECIAL BOOK TYPE                    
         MVC   5(2,RE),WORK+120+18+1       2ND BASE BOOK(YM ONLY)               
         MVC   7(2,RE),WORK+120+21+1       3RD                                  
         MVC   9(2,RE),WORK+120+24+1       4TH                                  
         MVC   11(14,RE),WORK+120             UPGRADE ELEMENT                   
         LA    RE,11+14(RE)                                                     
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - DEMO FIELD                                            
*.....................................................................          
         USING *,RB                                                             
NIDEMO   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,MDCODE         FIRST DEMO FIELD?                            
         BE    NIDEM010            NO                                           
*                                                                               
         CLC   =Y(NIUPGRDQ),HALF   LAST FIELD AN UPGRADE?                       
         BE    NIDEM002            YES                                          
         CLC   =Y(NIBOOKQ),HALF    LAST FIELD A BOOK?                           
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO UPGRADES                        
         LA    RE,1(RE)                                                         
NIDEM002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NIDEM010 DS    0H                                                               
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(233)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         LA    RE,3(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW INVENTORY REQUEST - RATE CARD FIELD                                       
*.....................................................................          
         USING *,RB                                                             
NIRATEC  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,MDCODE         FIRST RATE CARD FIELD?                       
         BE    NIRCD010            NO                                           
*                                                                               
         CLC   =Y(NIDEMOQ),HALF    LAST FIELD A DEMO?                           
         BE    NIRCD002            YES                                          
         CLC   =Y(NIFLTENQ),HALF   LAST FIELD A FLIGHT END?                     
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
         MVI   0(RE),0                 - SET NO UPGRADES                        
         LA    RE,1(RE)                                                         
         MVI   0(RE),0                 - SET NO DEMOS                           
         LA    RE,1(RE)                                                         
NIRCD002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NIRCD010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* NEW DATA FOR INVENTORY REQUEST - RATING SERVICE FIELD                         
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
NDRTSRV  LR    RB,RF                                                            
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
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - FLIGHT START FIELD                           
*.....................................................................          
         USING *,RB                                                             
NDFLTST  LR    RB,RF                                                            
         CLC   =Y(NDFLTENQ),HALF   LAST FIELD AN END FLIGHT?                    
         BE    NDFLS010            YES                                          
         CLC   =Y(NDRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF FLIGHTS                        
         LA    RE,1(RE)            NUMBER OF FLIGHTS GOES HERE                  
         ST    RE,ADDR                                                          
*                                                                               
NDFLS010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(3,RE),0(RF)       COPY FLIGHT START                            
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - FLIGHT END FIELD                             
*.....................................................................          
         USING *,RB                                                             
NDFLTEN  LR    RB,RF                                                            
         CLC   =Y(NDFLTSTQ),HALF   LAST FIELD A START FLIGHT?                   
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   3(3,RE),0(RF)       COPY FLIGHT END                              
         LA    RE,6(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - BOOK FIELD(S)                                
*.....................................................................          
         USING *,RB                                                             
NDBOOK   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(NDBOOKQ)    FIRST BOOK FIELD?                            
         BE    NDBK0010            NO                                           
         CLC   =Y(NDFLTENQ),HALF   LAST FIELD A FLIGHT END?                     
         BE    NDBK0002            YES                                          
         CLC   =Y(NDRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0                 - SET NO FLIGHTS                         
         LA    RE,1(RE)                                                         
NDBK0002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF BOOKS                          
         LA    RE,1(RE)            NUMBER OF BOOKS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NDBK0010 DS    0H                                                               
         MVC   HALF,=Y(NDBOOKQ)                                                 
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(232)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVI   3(RE),C'I'          SET DEMO FILE(INV IS DEFAULT)                
         CLC   MDCODE,=Y(NDTPBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(NDT4BKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(NDPVBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         MVC   4(1,RE),FULL        SET BOOK TYPE                                
*                                                                               
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - UPGRADE FIELD(S)                             
*.....................................................................          
         USING *,RB                                                             
NDUPGRD  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(NDUPGRDQ)   FIRST UPGRADE FIELD?                         
         BE    NDUPG010            NO                                           
         CLC   =Y(NDBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    NDUPG004            YES                                          
         CLC   =Y(NDFLTENQ),HALF   LAST FIELD A FLIGHT END?                     
         BE    NDUPG002            YES                                          
         CLC   =Y(NDRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0                 - SET NO FLIGHTS                         
         LA    RE,1(RE)                                                         
NDUPG002 DS    0H                                                               
         MVI   0(RE),0                 - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
NDUPG004 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF UPGRADES                       
         LA    RE,1(RE)            NUMBER OF UPGRADES GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
NDUPG010 DS    0H                                                               
         MVC   HALF,=Y(NDUPGRDQ)                                                
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
         CLC   MDCODE,=Y(NDUPGRDQ)                                              
         BNE   NDUPG020                                                         
*                                                                               
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BZ    EINVUPG             NO                                           
*                                                                               
         MVI   3(RE),C'I'                                                       
         B     NDUPG022                                                         
*                                                                               
NDUPG020 DS    0H                                                               
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BNZ   EINVUPG             YES                                          
*                                                                               
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(NDT4UPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(NDPVUPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         DROP  UE                                                               
*                                                                               
NDUPG022 DS    0H                                                               
         MVC   0(3,RE),WORK+120+15         1ST BASE BOOKS                       
         MVC   4(1,RE),WORK+120+14         SPECIAL BOOK TYPE                    
         MVC   5(2,RE),WORK+120+18+1       2ND BASE BOOK(YM ONLY)               
         MVC   7(2,RE),WORK+120+21+1       3RD                                  
         MVC   9(2,RE),WORK+120+24+1       4TH                                  
         MVC   11(14,RE),WORK+120          UPGRADE ELEMENT                      
         LA    RE,11+14(RE)                                                     
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - DEMO FIELD                                   
*.....................................................................          
         USING *,RB                                                             
NDDEMO   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,MDCODE         FIRST DEMO FIELD?                            
         BE    NDDEM010            NO                                           
*                                                                               
         CLC   =Y(NDUPGRDQ),HALF   LAST FIELD AN UPGRADE?                       
         BE    NDDEM002            YES                                          
         CLC   =Y(NDBOOKQ),HALF    LAST FIELD A BOOK?                           
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO UPGRADES                        
         LA    RE,1(RE)                                                         
NDDEM002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NDDEM010 DS    0H                                                               
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(233)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         LA    RE,3(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - RATE CARD FIELD                              
*.....................................................................          
         USING *,RB                                                             
NDRATEC  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,MDCODE         FIRST RATE CARD FIELD?                       
         BE    NDRCD010            NO                                           
*                                                                               
         CLC   =Y(NDDEMOQ),HALF    LAST FIELD A DEMO?                           
         BE    NDRCD004            YES                                          
         CLC   =Y(NDFLTENQ),HALF   LAST FIELD A FLIGHT END?                     
         BE    NDRCD002            YES                                          
         CLC   =Y(NDRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0                 - SET NO FLIGHTS                         
         LA    RE,1(RE)                                                         
NDRCD002 DS    0H                                                               
         MVI   0(RE),0                 - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
         MVI   0(RE),0                 - SET NO UPGRADES                        
         LA    RE,1(RE)                                                         
         MVI   0(RE),0                 - SET NO DEMOS                           
         LA    RE,1(RE)                                                         
NDRCD004 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NDRCD010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - STATION FIELD                                
*.....................................................................          
         USING *,RB                                                             
NDSTA    LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(NDEFFSTQ),HALF   LAST FIELD AN EFFECTIVE START?               
         BE    NDSTA010            YES                                          
         CLC   =Y(NDRATECQ),HALF   LAST FIELD A RATE CARD?                      
         BE    NDSTA006            YES                                          
         CLC   =Y(NDDEMOQ),HALF    LAST FIELD A DEMO?                           
         BE    NDSTA004            YES                                          
         CLC   =Y(NDRATECQ),HALF   LAST FIELD A FLIGHT END?                     
         BE    NDSTA002            YES                                          
         CLC   =Y(NDRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0                 - SET NO FLIGHTS                         
         LA    RE,1(RE)                                                         
NDSTA002 DS    0H                                                               
         MVI   0(RE),0                 - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
         MVI   0(RE),0                 - SET NO UPGRADES                        
         LA    RE,1(RE)                                                         
         MVI   0(RE),0                 - SET NO DEMOS                           
         LA    RE,1(RE)                                                         
NDSTA004 DS    0H                                                               
         MVI   0(RE),0                 - SET NO RATE CARDS                      
         LA    RE,1(RE)                                                         
NDSTA006 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR             STATION GOES HERE                            
         B     NDSTA020                                                         
*                                                                               
NDSTA010 DS    0H                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVI   0(RE),0             SET END OF PREVIOUS STATION                  
         LA    RE,1(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
NDSTA020 DS    0H                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RE,5(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - INVENTORY NUMBER FIELD                       
*.....................................................................          
         USING *,RB                                                             
NDINV#   LR    RB,RF                                                            
         CLC   HALF,=Y(NDEFFSTQ)   LAST FIELD AN EFFECTIVE START?               
         BE    NDINV010            YES                                          
         CLC   =Y(NDSTAQ),HALF     LAST FIELD A STATION?                        
         BNE   EPARMSEQ                                                         
*                                                                               
NDINV010 DS    0H                                                               
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(4,RE),0(RF)       COPY INVENTORY NUMBER                        
         OC    0(4,RE),SPACES                                                   
         XC    4(6,RE),4(RE)       CLEAR DATES                                  
         LA    RE,4(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - INVENTORY EFFECTIVE END DATE FIELD           
*.....................................................................          
         USING *,RB                                                             
NDEFFEN  LR    RB,RF                                                            
         CLC   =Y(NDINV#Q),HALF    LAST FIELD AN INVENTORY NUMBER?              
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   3(3,RE),0(RF)       COPY FLIGHT END                              
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - INV. EFFECITVE START DATE FIELD              
*.....................................................................          
         USING *,RB                                                             
NDEFFST  LR    RB,RF                                                            
         CLC   =Y(NDEFFENQ),HALF   LAST FIELD AN EFFECTIVE END?                 
         BE    NDEST010                                                         
         CLC   =Y(NDINV#Q),HALF    LAST FIELD AN INVENTORY NUMBER?              
         BNE   EPARMSEQ                                                         
*                                                                               
NDEST010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(3,RE),0(RF)     COPY FLIGHT START                              
         LA    RE,6(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - GET TEXT NOT RATINGS/RATES FLAG              
*.....................................................................          
         USING *,RB                                                             
NDTXT    LR    RB,RF                                                            
         OI    MISCFLG1,MF1TXT                                                  
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* NEW MARKET/STATION TEXT REQUEST - RATING SERVICE FIELD                        
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
NTRTSRV  LR    RB,RF                                                            
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
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW MARKET/STATION TEXT REQUEST - STATION FIELD                               
*.....................................................................          
         USING *,RB                                                             
NTSTA    LR    RB,RF                                                            
         CLC   HALF,MDCODE         FIRST STATION FIELD?                         
         BE    NTSTA010            NO                                           
         CLC   =Y(NTRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF STATIONS                       
         LA    RE,1(RE)            NUMBER OF STATIONS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
NTSTA010 DS    0H                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW MARKET/STATION TEXT REQUEST - BOOK FIELD                                  
*.....................................................................          
         USING *,RB                                                             
NTBOOK   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(NTBOOKQ)    FIRST BOOK FIELD?                            
         BE    NTBK0010            NO                                           
         CLC   =Y(NTSTAQ),HALF     LAST FIELD A STATION?                        
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF BOOKS                          
         LA    RE,1(RE)            NUMBER OF BOOKS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NTBK0010 DS    0H                                                               
         MVC   HALF,=Y(NTBOOKQ)                                                 
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(232)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVI   3(RE),C'I'          SET DEMO FILE(INV IS DEFAULT)                
         CLC   MDCODE,=Y(NTTPBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(NTT4BKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(NTPVBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         MVC   4(1,RE),FULL        SET BOOK TYPE                                
*                                                                               
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW MARKET/STATION TEXT REQUEST - DEMO FIELD                                  
*.....................................................................          
         USING *,RB                                                             
NTDEMO   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,MDCODE         FIRST DEMO FIELD?                            
         BE    NTDEM010            NO                                           
         CLC   =Y(NTBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    NTDEM002            YES                                          
         CLC   =Y(NTSTAQ),HALF     LAST FIELD A STATION?                        
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVI   0(RE),0             SET NO BOOKS                                 
         LA    RE,1(RE)                                                         
NTDEM002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
NTDEM010 DS    0H                                                               
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(233)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         LA    RE,3(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* NEW MARKET/STATION TEXT REQUEST - MARKET NOT STATION TEXT FLAG                
*.....................................................................          
         USING *,RB                                                             
NTMKT    LR    RB,RF                                                            
         OI    MISCFLG1,MF1MKT                                                  
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* COMPETITVE STATION LIST REQUEST - STATION FIELD                               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
CSSTA    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADER REQUEST - STATION FIELD                                       
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
VHSTA    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO                                           
*                                                                               
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
         LTORG                                                                  
*.....................................................................          
* VALIDATE HEADER REQUEST - ADVERTISER FIELD                                    
*.....................................................................          
         USING *,RB                                                             
VHADV    LR    RB,RF                                                            
         CLC   =Y(VHSTAQ),HALF     LAST FIELD THE STATION?                      
         BNE   EPARMSEQ            NO                                           
*                                                                               
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
         LTORG                                                                  
*.....................................................................          
* VALIDATE HEADER REQUEST - PRODUCT FIELD                                       
*.....................................................................          
         USING *,RB                                                             
VHPRD    LR    RB,RF                                                            
         CLC   =Y(VHADVQ),HALF     LAST FIELD THE ADVERTISER?                   
         BNE   EPARMSEQ            NO                                           
*                                                                               
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
         LTORG                                                                  
*.....................................................................          
* VALIDATE HEADER REQUEST - AGENCY FIELD                                        
*.....................................................................          
         USING *,RB                                                             
VHAGY    LR    RB,RF                                                            
         CLC   =Y(VHADVQ),HALF     LAST FIELD THE ADVERTISER?                   
         BE    *+14                YES                                          
         CLC   =Y(VHPRDQ),HALF     LAST FIELD THE PRODUCT?                      
         BNE   EPARMSEQ            NO                                           
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
         B     EXITOK                                                           
         DROP  RE                                                               
         LTORG                                                                  
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
         LTORG                                                                  
*.....................................................................          
* VALIDATE HEADER REQUEST - FLIGHT START FIELD                                  
*.....................................................................          
         USING *,RB                                                             
VHFLS    LR    RB,RF                                                            
         CLC   =Y(VHAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BE    *+14                YES                                          
         CLC   =Y(VHAOFQ),HALF     LAST FIELD THE AGENCY OFFICE?                
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLS,0(RF)        COPY FLIGHT START                            
         B     EXITOK                                                           
         DROP  RE                                                               
         LTORG                                                                  
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
         LTORG                                                                  
*.....................................................................          
* VALIDATE HEADER REQUEST - CONTRACT TYPE FIELD (OPTIONAL)                      
*.....................................................................          
         USING *,RB                                                             
VHCTYP   LR    RB,RF                                                            
         CLC   =Y(VHFLEQ),HALF     LAST FIELD THE FLIGHT END?                   
         BNE   EPARMSEQ            NO                                           
*                                                                               
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
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - BOOK FIELD(S)                         
*.....................................................................          
         USING *,RB                                                             
VBBOOK   LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(232)                                                    
         B     EXITL                                                            
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
         L     R1,ADDR             COPY UNBOOKED NAME                           
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
         ST    R1,ADDR                                                          
*                                                                               
         CHI   R1,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - UPGRADE FIELD                         
*.....................................................................          
         USING *,RB                                                             
VBUPGRD  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
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
         L     R1,ADDR                                                          
         A     R1,ATWA                                                          
*                                                                               
UE       USING RAVLNEL,WORK+120                                                 
         CLC   MDCODE,=Y(VBUPGRDQ)                                              
         BNE   VBUPG010                                                         
*                                                                               
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BZ    EINVUPG             NO                                           
*                                                                               
         MVC   0(2,R1),=Y(VBUPGRDQ)   SAY ITS AN INVENTORY UPGRADE              
         B     VBUPG020                                                         
*                                                                               
VBUPG010 DS    0H                                                               
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
         ST    R1,ADDR                                                          
*                                                                               
         CHI   R1,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - DEMO FIELD                            
*.....................................................................          
         USING *,RB                                                             
VBDEMO   LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(233)                                                    
         B     EXITL                                                            
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
         L     R1,ADDR             COPY UNDEMOED NAME                           
         A     R1,ATWA                                                          
         MVC   0(2,R1),HALF        SAY ITS A DEMO                               
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),WORK+8      MOVE THE TEXT                                
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR                                                          
*                                                                               
         CHI   R1,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* REFRESH CONTRACT HEADER FIELDS REQUEST - CONTRACT NUMBER FIELD                
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
RCHFCON  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPCON,0(RF)        COPY CONTRACT #                              
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ADD CONTRACT REQUEST - SALESPERSON FIELD                                      
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
ACSAL    LR    RB,RF                                                            
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
*                                                                               
         B     EXITOK                                                           
         DROP  RE                                                               
         LTORG                                                                  
*.....................................................................          
* ADD CONTRACT REQUEST - STATION FIELD                                          
*.....................................................................          
         USING *,RB                                                             
ACSTA    LR    RB,RF                                                            
         CLC   =Y(ACSALQ),HALF     LAST FIELD THE STATION?                      
         BNZ   EPARMSEQ            NO                                           
*                                                                               
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
         LTORG                                                                  
*.....................................................................          
* ADD CONTRACT REQUEST - ADVERTISER FIELD                                       
*.....................................................................          
         USING *,RB                                                             
ACADV    LR    RB,RF                                                            
         CLC   =Y(ACSTAQ),HALF     LAST FIELD THE STATION?                      
         BNE   EPARMSEQ            NO                                           
*                                                                               
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
* ADD CONTRACT REQUEST - PRODUCT CODE FIELD                                     
*.....................................................................          
         USING *,RB                                                             
ACPRD    LR    RB,RF                                                            
         CLC   =Y(ACADVQ),HALF     LAST FIELD THE ADVERTISER?                   
         BNE   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,=Y(ACPRDQ)                                                  
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPPRD,0(RF)        COPY PRODUCT                                 
         OC    VHPPRD,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* ADD CONTRACT REQUEST - FREE FORM PRODUCT FIELD                                
*.....................................................................          
         USING *,RB                                                             
ACFFPRD  LR    RB,RF                                                            
         CLC   =Y(ACADVQ),HALF     LAST FIELD THE ADVERTISER?                   
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,=Y(ACPRDQ)                                                  
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFFPRD,0(RF)      COPY FREE FORM PRODUCT                       
         OC    VHPFFPRD,SPACES                                                  
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* ADD CONTRACT REQUEST - AGENCY FIELD                                           
*.....................................................................          
         USING *,RB                                                             
ACAGY    LR    RB,RF                                                            
         CLC   =Y(ACPRDQ),HALF     LAST FIELD THE PRODUCT?                      
         BNE   EPARMSEQ            NO                                           
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
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* ADD CONTRACT REQUEST - AGENCY OFFICE FIELD                                    
*.....................................................................          
         USING *,RB                                                             
ACAOF    LR    RB,RF                                                            
         CLC   =Y(ACAGYQ),HALF     LAST FIELD THE AGENCY?                       
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
* ADD CONTRACT REQUEST - BUYER FIELD (OPTIONAL)                                 
*.....................................................................          
         USING *,RB                                                             
ACBUYER  LR    RB,RF                                                            
         CLC   =Y(ACAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BE    *+14                YES                                          
         CLC   =Y(ACAOFQ),HALF     LAST FIELD THE AGENCY OFFICE?                
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPBUYER,0(RF)      COPY BUYER                                   
         OC    VHPBUYER,SPACES                                                  
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* ADD CONTRACT REQUEST - FLIGHT START FIELD                                     
*.....................................................................          
         USING *,RB                                                             
ACFLS    LR    RB,RF                                                            
         CLC   =Y(ACBUYERQ),HALF   LAST FIELD THE BUYER?                        
         BE    *+24                YES                                          
         CLC   =Y(ACAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BE    *+14                YES                                          
         CLC   =Y(ACAOFQ),HALF     LAST FIELD THE AGENCY OFFICE?                
         BNE   EPARMSEQ            NO                                           
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
* ADD CONTRACT REQUEST - FLIGHT END FIELD                                       
*.....................................................................          
         USING *,RB                                                             
ACFLE    LR    RB,RF                                                            
         CLC   =Y(ACFLSQ),HALF     LAST FIELD THE FLIGHT START                  
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
* ADD CONTRACT REQUEST - LENGTH FIELD                                           
*.....................................................................          
         USING *,RB                                                             
ACLEN    LR    RB,RF                                                            
         CLC   =Y(ACLENQ),HALF     LAST FIELD A LENGTH?                         
         BE    *+14                YES                                          
         CLC   =Y(ACFLEQ),HALF     LAST FIELD THE FLIGHT END?                   
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         LA    R1,VHPLENS          FIND AN EMPTY SLOT                           
         LA    R0,VHPLENS+L'VHPLENS                                             
ACLEN02  DS    0H                                                               
         OC    0(2,R1),0(R1)                                                    
         BZ    ACLEN04                                                          
         LA    R1,2(R1)                                                         
         CR    R1,R0                                                            
         BL    ACLEN02                                                          
*                                                                               
         MVC   ERROR,=Y(324)                                                    
         B     EXITL                                                            
*                                                                               
ACLEN04  DS    0H                                                               
         MVC   1(1,R1),0(RF)       COPY LENGTH                                  
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* ADD CONTRACT REQUEST - CONTRACT TYPE FIELD (OPTIONAL)                         
*.....................................................................          
         USING *,RB                                                             
ACCTYP   LR    RB,RF                                                            
         CLC   =Y(ACLENQ),HALF     LAST FIELD A LENGTH?                         
         BNE   EPARMSEQ            NO                                           
*                                                                               
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
* ADD CONTRACT REQUEST - BUSINIESS TYPE FIELD                                   
*.....................................................................          
         USING *,RB                                                             
ACBUTYP  LR    RB,RF                                                            
         CLC   =Y(ACLENQ),HALF     LAST FIELD A LENGTH?                         
         BE    *+14                YES                                          
         CLC   =Y(ACCTYPQ),HALF    LAST FIELD THE CONTRACT TYPE?E               
         BNE   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPBUTYP,0(RF)       COPY BUSINESS TYPE                          
         OC    VHPBUTYP,SPACES                                                  
         B     EXITOK                                                           
*.....................................................................          
* ADD CONTRACT REQUEST - MARKET BUDGET FIELD                                    
*.....................................................................          
         USING *,RB                                                             
ACMKBUD  LR    RB,RF                                                            
         CLC   =Y(ACBUTYPQ),HALF   LAST FIELD THE BUSINESS TYPE?                
         BE    *+24                YES                                          
         CLC   =Y(ACLENQ),HALF     LAST FIELD A LENGTH?                         
         BE    *+14                YES                                          
         CLC   =Y(ACCTYPQ),HALF    LAST FIELD THE CONTRACT TYPE?                
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPMKBUD,0(RF)       COPY MARKET BUDGET                          
*&&DO                                                                           
         OC    VHPMKBUD,VHPMKBUD                                                
         BNZ   *+8                                                              
         OI    VHPMKBUD,X'80'      SET ZERO ENTERED                             
*&&                                                                             
         B     EXITOK                                                           
*.....................................................................          
* ADD CONTRACT REQUEST - SHARE GOAL FIELD                                       
*.....................................................................          
         USING *,RB                                                             
ACSHRGL  LR    RB,RF                                                            
         CLC   =Y(ACMKBUDQ),HALF   LAST FIELD THE MARKET BUDGET?                
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSHRGL,0(RF)       COPY SHARE GOAL                             
*&&DO                                                                           
         OC    VHPSHRGL,VHPSHRGL                                                
         BNZ   *+8                                                              
         OI    VHPSHRGL,X'80'      SET ZERO ENTERED                             
*&&                                                                             
         B     EXITOK                                                           
*.....................................................................          
* ADD CONTRACT REQUEST - RATING SERVICE FIELD                                   
*.....................................................................          
         USING *,RB                                                             
ACRTSRV  LR    RB,RF                                                            
         CLC   =Y(ACLENQ),HALF     LAST FIELD A LENGTH?                         
         BE    ACRTSR10            YES                                          
         CLC   =Y(ACCTYPQ),HALF    LAST FIELD THE CONTRACT TYPE?                
         BE    ACRTSR10            YES                                          
         CLC   =Y(ACBUTYPQ),HALF   LAST FIELD THE BUSINESS TYPE?                
         BE    ACRTSR10            YES                                          
         CLC   =Y(ACSHRGLQ),HALF   LAST FIELD THE SHARE GOAL?                   
         BE    ACRTSR10            YES                                          
         B     EPARMSEQ                                                         
*                                                                               
ACRTSR10 DS    0H                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   RTSRVC,0(RF)        COPY RATING SERVICE                          
         OC    RTSRVC,SPACES                                                    
*                                                                               
         L     RE,ADDR             BUMP PAST CONTRACT FIELDS                    
         AHI   RE,VHPARMLQ                                                      
         ST    RE,ADDR                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* ADD CONTRACT REQUEST - BOOK FIELD(S)                                          
*.....................................................................          
         USING *,RB                                                             
ACBOOK   LR    RB,RF                                                            
         CLC   HALF,=Y(ACBOOKQ)    FIRST BOOK FIELD?                            
         BE    ACBK0010            NO                                           
         CLC   =Y(ACRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF BOOKS                          
         LA    RE,1(RE)            NUMBER OF BOOKS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
ACBK0010 DS    0H                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         CLI   0(RE),7             TOO MANY BOOKS?                              
         BNE   *+14                NO                                           
         MVC   ERROR,=Y(161)                                                    
         B     EXITL                                                            
*                                                                               
         MVC   HALF,=Y(ACBOOKQ)                                                 
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(232)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVC   3(1,RE),FULL        SET BOOK TYPE                                
*                                                                               
         LA    RE,4(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* ADD CONTRACT REQUEST - DEMO FIELD                                             
*.....................................................................          
         USING *,RB                                                             
ACDEMO   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,MDCODE         FIRST DEMO FIELD?                            
         BE    ACDEM010            NO                                           
*                                                                               
         CLC   =Y(ACBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    ACDEM002            YES                                          
         CLC   =Y(ACRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
ACDEM002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
ACDEM010 DS    0H                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         CLI   0(RE),7             TOO MANY DEMOS?                              
         BNE   *+14                NO                                           
         MVC   ERROR,=Y(160)                                                    
         B     EXITL                                                            
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(233)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         LA    RE,3(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* ADD CONTRACT REQUEST - BUYER CPP FIELD                                        
*.....................................................................          
         USING *,RB                                                             
ACBYCPP  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(ACBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    ACBCPP02            YES                                          
         CLC   =Y(ACDEMOQ),HALF    LAST FIELD A DEMO?                           
         BE    ACBCPP04            YES                                          
         CLC   =Y(ACDPTQ),HALF     LAST FIELD A DAYPART?                        
         BE    ACBCPP10            YES                                          
         CLC   =Y(ACRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
ACBCPP02 DS    0H                                                               
         MVI   0(RE),0             YES - SET NO DEMOS                           
         LA    RE,1(RE)                                                         
ACBCPP04 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DAYPARTS                       
         LA    RE,1(RE)            NUMBER OF DAYPARTS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
ACBCPP10 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         MVC   FULL,0(RF)                                                       
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* ADD CONTRACT REQUEST - DAYPART FIELD                                          
*.....................................................................          
         USING *,RB                                                             
ACDPT    LR    RB,RF                                                            
         CLC   =Y(ACBYCPPQ),HALF   LAST FIELD A BUYER CPP?                      
         BE    ACDPT010            YES                                          
*                                                                               
         XC    FULL,FULL           CLEAR BUYER CPP                              
*                                                                               
         CLC   HALF,MDCODE         FIRST DAYPART FIELD?                         
         BE    ACDPT010            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(ACBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    ACDPT002            YES                                          
         CLC   =Y(ACDEMOQ),HALF    LAST FIELD A DEMO?                           
         BE    ACDPT004            YES                                          
         CLC   =Y(ACRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
ACDPT002 DS    0H                                                               
         MVI   0(RE),0             YES - SET NO DEMOS                           
         LA    RE,1(RE)                                                         
ACDPT004 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DAYPARTS                       
         LA    RE,1(RE)            NUMBER OF DAYPARTS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
ACDPT010 DS    0H                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(1,RE),0(RF)       COPY DAYPART                                 
         OC    0(1,RE),SPACES                                                   
         MVC   1(4,RE),FULL        COPY BUYERS CPP                              
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* ADD CONTRACT REQUEST - PENDING COMMENT FIELD                                  
*.....................................................................          
         USING *,RB                                                             
ACPCOM   LR    RB,RF                                                            
         CLC   HALF,MDCODE         FIRST COMMENT FIELD?                         
         BE    ACPCOM10            NO                                           
         CLC   =Y(ACDPTQ),HALF     LAST FIELD A DAYPART?                        
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF COMMENTS                       
         LA    RE,1(RE)            NUMBER OF COMMENTS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
ACPCOM10 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         L     R1,FPARMS+8         GET THE LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       COPY COMMENT TEXT                            
         OC    0(60,RE),SPACES                                                  
         LA    RE,60(RE)            BUMP TO NEXT ENTRY                          
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* ADD CONTRACT REQUEST - COMPETITIVE STATION FIELD                              
*.....................................................................          
         USING *,RB                                                             
ACCSTA   LR    RB,RF                                                            
         CLC   HALF,MDCODE         FIRST STATION FIELD?                         
         BE    ACCSTA10            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(ACPCOMQ),HALF    LAST FIELD A PENDING COMMENT?                
         BE    ACCSTA02            YES                                          
         CLC   =Y(ACDPTQ),HALF     LAST FIELD A DAYPART?                        
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVI   0(RE),0             YES - SET NO COMMENTS                        
         LA    RE,1(RE)                                                         
ACCSTA02 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF STATIONS                       
         LA    RE,1(RE)            NUMBER OF STATIONS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
ACCSTA10 DS    0H                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* CHANGE CONTRACT REQUEST - CONTRACT NUMBER FIELD                               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
CCCON#   LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPCON,0(RF)        COPY CONTRACT #                              
*                                                                               
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* CHANGE CONTRACT REQUEST - SALESPERSON FIELD                                   
*.....................................................................          
         USING *,RB                                                             
CCSAL    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
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
*                                                                               
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* CHANGE CONTRACT REQUEST - STATION FIELD                                       
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
CCSTA    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
*                                                                               
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
* CHANGE CONTRACT REQUEST - ADVERTISER FIELD                                    
*.....................................................................          
         USING *,RB                                                             
CCADV    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
*                                                                               
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
* CHANGE CONTRACT REQUEST - PRODUCT CODE FIELD                                  
*.....................................................................          
         USING *,RB                                                             
CCPRD    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
*                                                                               
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,=Y(CCPRDQ)                                                  
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPPRD,0(RF)        COPY PRODUCT                                 
         OC    VHPPRD,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* CHANGE CONTRACT REQUEST - FREE FORM PRODUCT FIELD                             
*.....................................................................          
         USING *,RB                                                             
CCFFPRD  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
*                                                                               
         MVC   HALF,=Y(CCPRDQ)                                                  
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFFPRD,0(RF)      COPY FREE FORM PRODUCT                       
         OC    VHPFFPRD,SPACES                                                  
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* CHANGE CONTRACT REQUEST - AGENCY FIELD                                        
*.....................................................................          
         USING *,RB                                                             
CCAGY    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
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
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* CHANGE CONTRACT REQUEST - AGENCY OFFICE FIELD                                 
*.....................................................................          
         USING *,RB                                                             
CCAOF    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
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
* CHANGE CONTRACT REQUEST - BUYER FIELD (OPTIONAL)                              
*.....................................................................          
         USING *,RB                                                             
CCBUYER  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPBUYER,0(RF)      COPY BUYER                                   
         OC    VHPBUYER,SPACES                                                  
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* CHANGE CONTRACT REQUEST - FLIGHT START FIELD                                  
*.....................................................................          
         USING *,RB                                                             
CCFLS    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
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
* CHANGE CONTRACT REQUEST - FLIGHT END FIELD                                    
*.....................................................................          
         USING *,RB                                                             
CCFLE    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
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
* CHANGE CONTRACT REQUEST - LENGTH FIELD                                        
*.....................................................................          
         USING *,RB                                                             
CCLEN    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         LA    R1,VHPLENS          FIND AN EMPTY SLOT                           
         LA    R0,VHPLENS+L'VHPLENS                                             
CCLEN02  DS    0H                                                               
         OC    0(2,R1),0(R1)                                                    
         BZ    CCLEN04                                                          
         LA    R1,2(R1)                                                         
         CR    R1,R0                                                            
         BL    CCLEN02                                                          
*                                                                               
         MVC   ERROR,=Y(324)                                                    
         B     EXITL                                                            
*                                                                               
CCLEN04  DS    0H                                                               
         MVC   1(1,R1),0(RF)       COPY LENGTH                                  
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* CHANGE CONTRACT REQUEST - CONTRACT TYPE FIELD (OPTIONAL)                      
*.....................................................................          
         USING *,RB                                                             
CCCTYP   LR    RB,RF                                                            
         CLC   =Y(CCLENQ),HALF     LAST FIELD A LENGTH?                         
         BNE   EPARMSEQ            NO                                           
*                                                                               
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
* CHANGE CONTRACT REQUEST - BUSINIESS TYPE FIELD                                
*.....................................................................          
         USING *,RB                                                             
CCBUTYP  LR    RB,RF                                                            
         CLC   =Y(CCLENQ),HALF     LAST FIELD A LENGTH?                         
         BE    *+14                YES                                          
         CLC   =Y(CCCTYPQ),HALF    LAST FIELD THE CONTRACT TYPE?E               
         BNE   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPBUTYP,0(RF)       COPY BUSINESS TYPE                          
         OC    VHPBUTYP,SPACES                                                  
         B     EXITOK                                                           
*.....................................................................          
* CHANGE CONTRACT REQUEST - MARKET BUDGET FIELD                                 
*.....................................................................          
         USING *,RB                                                             
CCMKBUD  LR    RB,RF                                                            
         CLC   =Y(CCBUTYPQ),HALF   LAST FIELD THE BUSINESS TYPE?                
         BE    *+24                YES                                          
         CLC   =Y(CCLENQ),HALF     LAST FIELD A LENGTH?                         
         BE    *+14                YES                                          
         CLC   =Y(CCCTYPQ),HALF    LAST FIELD THE CONTRACT TYPE?                
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPMKBUD,0(RF)       COPY MARKET BUDGET                          
*&&DO                                                                           
         OC    VHPMKBUD,VHPMKBUD                                                
         BNZ   *+8                                                              
         OI    VHPMKBUD,X'80'      SET ZERO ENTERED                             
*&&                                                                             
         B     EXITOK                                                           
*.....................................................................          
* CHANGE CONTRACT REQUEST - SHARE GOAL FIELD                                    
*.....................................................................          
         USING *,RB                                                             
CCSHRGL  LR    RB,RF                                                            
         CLC   =Y(CCMKBUDQ),HALF   LAST FIELD THE MARKET BUDGET?                
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSHRGL,0(RF)       COPY SHARE GOAL                             
*&&DO                                                                           
         OC    VHPSHRGL,VHPSHRGL                                                
         BNZ   *+8                                                              
         OI    VHPSHRGL,X'80'      SET ZERO ENTERED                             
*&&                                                                             
         B     EXITOK                                                           
*.....................................................................          
* CHANGE CONTRACT REQUEST - RATING SERVICE FIELD                                
*.....................................................................          
         USING *,RB                                                             
CCRTSRV  LR    RB,RF                                                            
         CLC   =Y(CCLENQ),HALF     LAST FIELD A LENGTH?                         
         BE    CCRTSR10            YES                                          
         CLC   =Y(CCCTYPQ),HALF    LAST FIELD THE CONTRACT TYPE?                
         BE    CCRTSR10            YES                                          
         CLC   =Y(CCBUTYPQ),HALF   LAST FIELD THE BUSINESS TYPE?                
         BE    CCRTSR10            YES                                          
         CLC   =Y(CCSHRGLQ),HALF   LAST FIELD THE SHARE GOAL?                   
         BE    CCRTSR10            YES                                          
         B     EPARMSEQ                                                         
*                                                                               
CCRTSR10 DS    0H                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   RTSRVC,0(RF)        COPY RATING SERVICE                          
         OC    RTSRVC,SPACES                                                    
*                                                                               
         L     RE,ADDR             BUMP PAST CONTRACT FIELDS                    
         AHI   RE,VHPARMLQ                                                      
         ST    RE,ADDR                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* CHANGE CONTRACT REQUEST - BOOK FIELD(S)                                       
*.....................................................................          
         USING *,RB                                                             
CCBOOK   LR    RB,RF                                                            
         CLC   HALF,=Y(CCBOOKQ)    FIRST BOOK FIELD?                            
         BE    CCBK0010            NO                                           
         CLC   =Y(CCRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF BOOKS                          
         LA    RE,1(RE)            NUMBER OF BOOKS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
CCBK0010 DS    0H                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         CLI   0(RE),7             TOO MANY BOOKS?                              
         BNE   *+14                NO                                           
         MVC   ERROR,=Y(161)                                                    
         B     EXITL                                                            
*                                                                               
         MVC   HALF,=Y(CCBOOKQ)                                                 
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(232)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVC   3(1,RE),FULL        SET BOOK TYPE                                
*                                                                               
         LA    RE,4(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* CHANGE CONTRACT REQUEST - DEMO FIELD                                          
*.....................................................................          
         USING *,RB                                                             
CCDEMO   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,MDCODE         FIRST DEMO FIELD?                            
         BE    CCDEM010            NO                                           
*                                                                               
         CLC   =Y(CCBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    CCDEM002            YES                                          
         CLC   =Y(CCRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
CCDEM002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
CCDEM010 DS    0H                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         CLI   0(RE),7             TOO MANY DEMOS?                              
         BNE   *+14                NO                                           
         MVC   ERROR,=Y(160)                                                    
         B     EXITL                                                            
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
         BNE   *+14                YES                                          
         MVC   ERROR,=Y(233)                                                    
         B     EXITL                                                            
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         LA    RE,3(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* CHANGE CONTRACT REQUEST - BUYER CPP FIELD                                     
*.....................................................................          
         USING *,RB                                                             
CCBYCPP  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(CCBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    CCBCPP02            YES                                          
         CLC   =Y(CCDEMOQ),HALF    LAST FIELD A DEMO?                           
         BE    CCBCPP04            YES                                          
         CLC   =Y(CCDPTQ),HALF     LAST FIELD A DAYPART?                        
         BE    CCBCPP10            YES                                          
         CLC   =Y(CCRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
CCBCPP02 DS    0H                                                               
         MVI   0(RE),0             YES - SET NO DEMOS                           
         LA    RE,1(RE)                                                         
CCBCPP04 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DAYPARTS                       
         LA    RE,1(RE)            NUMBER OF DAYPARTS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
CCBCPP10 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         MVC   FULL,0(RF)                                                       
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* CHANGE CONTRACT REQUEST - DAYPART FIELD                                       
*.....................................................................          
         USING *,RB                                                             
CCDPT    LR    RB,RF                                                            
         CLC   =Y(CCBYCPPQ),HALF   LAST FIELD A BUYER CPP?                      
         BE    CCDPT010            YES                                          
*                                                                               
         XC    FULL,FULL           CLEAR BUYER CPP                              
*                                                                               
         CLC   HALF,MDCODE         FIRST DAYPART FIELD?                         
         BE    CCDPT010            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(CCBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    CCDPT002            YES                                          
         CLC   =Y(CCDEMOQ),HALF    LAST FIELD A DEMO?                           
         BE    CCDPT004            YES                                          
         CLC   =Y(CCRTSRVQ),HALF   LAST FIELD THE RATING SERIVCE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0             YES - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
CCDPT002 DS    0H                                                               
         MVI   0(RE),0             YES - SET NO DEMOS                           
         LA    RE,1(RE)                                                         
CCDPT004 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DAYPARTS                       
         LA    RE,1(RE)            NUMBER OF DAYPARTS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
CCDPT010 DS    0H                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(1,RE),0(RF)       COPY DAYPART                                 
         OC    0(1,RE),SPACES                                                   
         MVC   1(4,RE),FULL        COPY BUYERS CPP                              
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* CHANGE CONTRACT REQUEST - PENDING COMMENT FIELD                               
*.....................................................................          
         USING *,RB                                                             
CCPCOM   LR    RB,RF                                                            
         CLC   HALF,MDCODE         FIRST COMMENT FIELD?                         
         BE    CCPCOM10            NO                                           
         CLC   =Y(CCDPTQ),HALF     LAST FIELD A DAYPART?                        
         BNE   EPARMSEQ            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF COMMENTS                       
         LA    RE,1(RE)            NUMBER OF COMMENTS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
CCPCOM10 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         L     R1,FPARMS+8         GET THE LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       COPY COMMENT TEXT                            
         OC    0(60,RE),SPACES                                                  
         LA    RE,60(RE)            BUMP TO NEXT ENTRY                          
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
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
* CHANGE CONTRACT REQUEST - COMPETITIVE STATION FIELD                           
*.....................................................................          
         USING *,RB                                                             
CCCSTA   LR    RB,RF                                                            
         CLC   HALF,MDCODE         FIRST STATION FIELD?                         
         BE    CCCSTA10            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(CCPCOMQ),HALF    LAST FIELD A PENDING COMMENT?                
         BE    CCCSTA02            YES                                          
         CLC   =Y(CCDPTQ),HALF     LAST FIELD A DAYPART?                        
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVI   0(RE),0             YES - SET NO COMMENTS                        
         LA    RE,1(RE)                                                         
CCCSTA02 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF STATIONS                       
         LA    RE,1(RE)            NUMBER OF STATIONS GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
CCCSTA10 DS    0H                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWAMAX-100)                                                  
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* INITIAL DOWNLOAD                                                              
**********************************************************************          
INITDWN  NTR1  BASE=*,LABEL=*                                                   
*                                  ADD NEW REP COMPANY ELEMENT                  
         GOTO1 ASETELEM,DMCB,AFABLK,REPDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,REPNAMEL,REPNAME,0                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,REPADDEL,REPADDR,0                          
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(1,WORK)                                      
***NOP*  GOTO1 AADDDATA,DMCB,AFABLK,REPTODEL,WORK,0                             
*                                                                               
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR,DMCB                                                        
*                                                                               
         LH    R5,=Y(SVPARMBF-T81CFFD)                                          
         A     R5,ATWA                                                          
         USING VHPARMD,R5                                                       
         OC    VHPSAL,VHPSAL       VALIDATE SALESPERSON?                        
         BZ    IDWN0018            NO                                           
*                                                                               
         GOTOX (VSALQ,ADDR),DMCB,(RC),(0,VHPSAL),VHPARMLQ(R5)                   
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
         DROP  R5                                                               
*                                                                               
         LA    R0,*                                                             
         AHI   R0,SALFOO-(*-4)                                                  
         GOTO1 PARSE,DMCB,VHPARMLQ(R5),(R0)                                     
         BNE   IDWN0018                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW SALESPERSON ELEMENT                  
         GOTO1 ASETELEM,DMCB,AFABLK,SALDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R6,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0002                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALNAMEL,(R0),(RF)                          
*                                                                               
IDWN0002 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0004                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALTELEL,(R0),0                             
*                                                                               
IDWN0004 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0006                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALFAXEL,(R0),0                             
*                                                                               
IDWN0006 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0008                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALTEMEL,(R0),(RF)                          
*                                                                               
IDWN0008 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0010                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALDIVEL,(R0),(RF)                          
*                                                                               
IDWN0010 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0012                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALTNMEL,(R0),(RF)                          
*                                                                               
IDWN0012 DS    0H                                                               
         LH    R5,=Y(SVPARMBF-T81CFFD)                                          
         A     R5,ATWA                                                          
         USING VHPARMD,R5                                                       
         GOTOX (GSALSTXQ,ADDR),DMCB,(RC),(0,VHPSAL),VHPARMLQ(R5)                
         BNE   IDWN0018                                                         
         DROP  R5                                                               
*                                                                               
         LA    R0,*                                                             
         AHI   R0,STXFOO-(*-4)                                                  
         GOTO1 PARSE,DMCB,VHPARMLQ(R5),(R0)                                     
         BNE   IDWN0018                                                         
         L     R5,0(R1)                                                         
*                                                                               
IDWN0014 DS    0H                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       END OF TABLE?                                
         BZ    IDWN0018            YES                                          
*                                                                               
         LA    R6,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0016                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALSTXEL,(R0),0                             
*                                                                               
IDWN0016 DS    0H                                                               
         AH    R5,HALF                                                          
         B     IDWN0014                                                         
*                                                                               
IDWN0018 DS    0H                                                               
         LH    R5,=Y(SVPARMBF-T81CFFD)                                          
         A     R5,ATWA                                                          
         GOTOX (GDPLISTQ,ADDR),DMCB,(RC),(R5)                                   
         BNE   IDWN0030            NO DATA                                      
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DPTFOO-(*-4)                                                  
         GOTO1 PARSE,DMCB,(R5),(R0)                                             
         BNE   IDWN0030                                                         
         L     R5,0(R1)                                                         
*                                                                               
IDWN0020 DS    0H                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       END OF TABLE?                                
         BZ    IDWN0030            YES                                          
*                                  ADD NEW DAYPART ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,DPTDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R6,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0022                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,DPTCODEL,(R0),(RF)                          
*                                                                               
IDWN0022 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0024                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,DPTSNMEL,(R0),(RF)                          
*                                                                               
IDWN0024 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0026                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,DPTLNMEL,(R0),(RF)                          
*                                                                               
IDWN0026 DS    0H                                                               
         AH    R5,HALF                                                          
         B     IDWN0020                                                         
*                                                                               
IDWN0030 DS    0H                                                               
         L     R0,ATWA                                                          
         AHI   R0,SVPARMBF-T81CFFD                                              
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PARAMETER BUFFER                       
*                                                                               
         LH    R5,=Y(SVPARMBF-T81CFFD)                                          
         A     R5,ATWA                                                          
         GOTOX (GCTLISTQ,ADDR),DMCB,(RC),(R5)                                   
         BNE   IDWN0040            NO DATA                                      
*                                                                               
         LA    R0,*                                                             
         AHI   R0,CTYFOO-(*-4)                                                  
         GOTO1 PARSE,DMCB,(R5),(R0)                                             
         BNE   IDWN0040                                                         
         L     R5,0(R1)                                                         
*                                                                               
IDWN0032 DS    0H                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       END OF TABLE?                                
         BZ    IDWN0040            YES                                          
*                                  ADD NEW DAYPART ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,CTYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R6,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0034                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,(R0),0                             
*                                                                               
IDWN0034 DS    0H                                                               
         LA    R6,3(R6)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    IDWN0036                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,CTYDSCEL,(R0),(RF)                          
*                                                                               
IDWN0036 DS    0H                                                               
         AH    R5,HALF                                                          
         B     IDWN0032                                                         
*                                                                               
IDWN0040 DS    0H                                                               
         GOTO1 VCOLY,DMCB,(X'20',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR,DMCB                                                        
*                                                                               
         GOTOX (RCDLISTQ,ADDR),DMCB,(RC)                                        
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* NEW INVENTORY DOWNLOAD                                                        
**********************************************************************          
NINVDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'20',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,DMCB                                                        
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         A     R0,ATWA                                                          
         GOTOX (NEWINVQ,FULL),DMCB,(RC),(R0)                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* NEW DATA FOR INVENTORY DOWNLOAD                                               
**********************************************************************          
NDATDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'20',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,DMCB                                                        
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         XC    0(2,RE),0(RE)       SET END OF STATIONS/INVENTORY                
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         A     R0,ATWA                                                          
         GOTOX (NEWDATAQ,FULL),DMCB,(RC),(R0)                                   
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* NEW MARKET/STATION TEXT DOWNLOAD                                              
**********************************************************************          
NTXTDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'20',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,DMCB                                                        
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         A     R0,ATWA                                                          
         GOTOX (NEWTEXTQ,FULL),DMCB,(RC),(R0)                                   
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* COMPETITIVE STATION DOWNLOAD                                                  
**********************************************************************          
CSTADWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,DMCB                                                        
*                                                                               
         LH    R0,=Y(SVPARMBF-T81CFFD)                                          
         A     R0,ATWA                                                          
         L     R5,ADDR                                                          
         A     R5,ATWA                                                          
         GOTOX (GMSLISTQ,FULL),DMCB,(RC),(R0),(R5)                              
         BNE   CSDWN030            NO DATA                                      
*                                                                               
         LA    R0,*                                                             
         AHI   R0,CSTAFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,(R5),(R0)                                             
         BNE   CSDWN030                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW STATION ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,ISTDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
CSDWN020 DS    0H                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       END OF TABLE?                                
         BZ    CSDWN030            YES                                          
*                                                                               
         LA    R6,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R6)                                                       
         BZ    CSDWN022                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R6)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTCSTEL,(R0),0                             
*                                                                               
CSDWN022 DS    0H                                                               
         AH    R5,HALF                                                          
         B     CSDWN020                                                         
*                                                                               
CSDWN030 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADER DOWNLOAD                                                      
**********************************************************************          
VHDRDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T81CFFD)                                            
         USING VHPARMD,R6                                                       
*                                                                               
         LA    R0,VHPARMLQ(R6)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,VHPSTA),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,VSTAFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN020                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW STATION ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,ISTDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN002                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTMKTEL,(R0),(RF)                          
*                                                                               
VHDWN002 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN004                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTCHNEL,(R0),0                             
*                                                                               
VHDWN004 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN006                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTAFFEL,(R0),0                             
*                                                                               
VHDWN006 DS    0H                                                               
         GOTOX (GMSLISTQ,ADDR2),DMCB,(RC),VHPSTA,ADDR                           
         BNE   VHDWN020            NO DATA                                      
*                                                                               
         LA    R0,*                                                             
         AHI   R0,CSTAFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN020                                                         
         L     R5,0(R1)                                                         
*                                                                               
VHDWN010 DS    0H                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       END OF TABLE?                                
         BZ    VHDWN020            YES                                          
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN012                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTCSTEL,(R0),0                             
*                                                                               
VHDWN012 DS    0H                                                               
         AH    R5,HALF                                                          
         B     VHDWN010                                                         
*                                                                               
VHDWN020 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,VADVFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN030                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW ADVERTISER ELEMENT                   
         GOTO1 ASETELEM,DMCB,AFABLK,ADVDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN022                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ADVNAMEL,(R0),(RF)                          
*                                                                               
VHDWN022 DS    0H                                                               
*                                                                               
VHDWN030 DS    0H                                                               
         OC    VHPPRD,VHPPRD       PRODUCT GIVEN?                               
         BZ    VHDWN040            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,VPRDFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN040                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW PRODUCT ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,PRDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN032                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,(R0),(RF)                          
*                                                                               
VHDWN032 DS    0H                                                               
*                                                                               
VHDWN040 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,VAGYFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN050                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW AGENCY ELEMENT                       
         GOTO1 ASETELEM,DMCB,AFABLK,AGYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN042                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,AGYNAMEL,(R0),(RF)                          
*                                                                               
VHDWN042 DS    0H                                                               
         GOTOX (AGYADDRQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),       +        
               (0,VHPADV),(0,VHPCTY),ADDR                                       
         BNE   VHDWN050                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGADFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN050                                                         
         L     R5,0(R1)                                                         
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN044                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   VHDWN044                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD0EL,(R0),(RF)                          
*                                                                               
VHDWN044 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN045                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   VHDWN045                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD1EL,(R0),(RF)                          
*                                                                               
VHDWN045 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN046                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   VHDWN046                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD2EL,(R0),(RF)                          
*                                                                               
VHDWN046 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN047                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   VHDWN047                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD3EL,(R0),(RF)                          
*                                                                               
VHDWN047 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    VHDWN048                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   VHDWN048                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYONMEL,(R0),(RF)                          
*                                                                               
VHDWN048 DS    0H                                                               
*                                                                               
VHDWN050 DS    0H                                                               
         GOTOX (VFLIGHTQ,ADDR2),DMCB,(RC),(0,VHPFLS),(0,VHPFLE),       +        
               (0,VHPSTA),ADDR                                                  
         BNE   VHDWNERR                                                         
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
         EJECT                                                                  
**********************************************************************          
* VALIDATE BOOKS/UPGRADES/DEMOS DOWNLOAD                                        
**********************************************************************          
VBUDDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T81CFFD)                                            
*                                                                               
VBDWN010 DS    0H                                                               
         OC    0(3,R6),0(R6)       END OF DATA?                                 
         BZ    VBDWN100            YES                                          
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
         B     VBDWN040                                                         
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
         B     VBDWN040                                                         
*                                                                               
VBDWN030 DS    0H                                                               
         CLC   =Y(VBUPGRDQ),0(R6)  INVENTORY UPGRADE?                           
         BE    *+14                YES                                          
         CLC   =Y(VBNIUPGQ),0(R6)  UPGRADE?                                     
         BNE   VBDWN040            NO                                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         CLC   =Y(VBUPGRDQ),0(R6)  INVENTORY UPGRADE?                           
         BNE   VBDWN032            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSINVEL,0,0                                
*                                                                               
VBDWN032 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,0,0                                
*                                                                               
****     ZIC   RF,2(R6)                                                         
****     GOTO1 AADDDATA,DMCB,AFABLK,BKSNAMEL,3(R6),(RF)                         
*                                                                               
VBDWN040 DS    0H                                                               
         ZIC   RF,2(R6)                                                         
         LA    R6,3(RF,R6)                                                      
         B     VBDWN010                                                         
*                                                                               
VBDWN100 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* REFRESH CONTRACT DOWNLOAD                                                     
**********************************************************************          
RCHFDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T81CFFD)                                            
         USING VHPARMD,R6                                                       
*                                                                               
         LA    R0,VHPARMLQ(R6)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
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
         BNE   EXITINV                                                          
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
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
         BH    RCDWN002            YES - DON'T LOOK FOR FREE FORM               
*                                                                               
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RCDWN002                                                         
*                                                                               
         USING RCONEXEL,R8                                                      
         MVC   VHPFFPRD,RCONEXPR                                                
         DROP  R8                                                               
*                                                                               
RCDWN002 DS    0H                                                               
*                                  ADD NEW CONTRACT ELEMENT                     
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,VHPFLS,0                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,VHPFLE,0                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,VHPBUYER,0                         
*                                                                               
* READ COMPETITIVE STATION RECORD                                               
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSLWREC,KEY                                                      
         MVI   K.RSLWKTYP,RSLWKTYQ                                              
         MVI   K.RSLWKSTY,RSLWKSBQ                                              
         MVC   K.RSLWKREP,REPALPHA                                              
*                                                                               
         ZAP   WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5) CONTRACT NUM IN PWOS                          
         MVC   K.RSLWKCON,WORK                                                  
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RSLWKEY),KEYSAVE                                           
         BNE   RCDWN00X                                                         
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,RSWCSELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   RCDWN00X                                                         
*                                                                               
         USING RSWCSELD,R8                                                      
RCDWN00A DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONCSTEL,RSWCSSTA,0                         
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    RCDWN00A                                                         
*                                                                               
RCDWN00X DS    0H                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,PRDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         CLC   VHPPRD,SPACES       PRODUCT GIVEN?                               
         BH    RCDWN004            YES - USE VALPRD                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,PRDIFFEL,0,0                                
*                                                                               
         LA    R0,L'VHPFFPRD                                                    
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,VHPFFPRD,(R0)                      
         B     RCDWN010                                                         
*                                                                               
RCDWN004 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   RCDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,RPRDFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN010                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW PRODUCT ELEMENT                      
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN006                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,PRDCODEL,(R0),(RF)                          
*                                                                               
RCDWN006 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN008                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,(R0),(RF)                          
*                                                                               
RCDWN008 DS    0H                                                               
*                                                                               
RCDWN010 DS    0H                                                               
         CLC   VHPCTY,SPACES                                                    
         BNH   RCDWN011                                                         
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,CTYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R0,L'VHPCTY                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,VHPCTY,(R0)                        
*                                                                               
RCDWN011 DS    0H                                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,VHPSTA),ADDR                          
         BNE   RCDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,RSTAFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN030                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW STATION ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,ISTDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN012                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTSTAEL,(R0),(RF)                          
*                                                                               
RCDWN012 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN014                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTMKTEL,(R0),(RF)                          
*                                                                               
RCDWN014 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN016                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTCHNEL,(R0),0                             
*                                                                               
RCDWN016 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN018                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTAFFEL,(R0),0                             
*                                                                               
RCDWN018 DS    0H                                                               
         GOTOX (GMSLISTQ,ADDR2),DMCB,(RC),VHPSTA,ADDR                           
         BNE   RCDWN030            NO DATA                                      
*                                                                               
         LA    R0,*                                                             
         AHI   R0,CSTAFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN030                                                         
         L     R5,0(R1)                                                         
*                                                                               
RCDWN020 DS    0H                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       END OF TABLE?                                
         BZ    RCDWN030            YES                                          
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN022                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ISTCSTEL,(R0),0                             
*                                                                               
RCDWN022 DS    0H                                                               
         AH    R5,HALF                                                          
         B     RCDWN020                                                         
*                                                                               
RCDWN030 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   RCDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,RADVFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN040                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW ADVERTISER ELEMENT                   
         GOTO1 ASETELEM,DMCB,AFABLK,ADVDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN032                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ADVCODEL,(R0),(RF)                          
*                                                                               
RCDWN032 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN034                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,ADVNAMEL,(R0),(RF)                          
*                                                                               
RCDWN034 DS    0H                                                               
*                                                                               
RCDWN040 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   RCDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,RAGYFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN050                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW PRODUCT ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,AGYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN041                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,AGYCODEL,(R0),(RF)                          
*                                                                               
RCDWN041 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN042                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    RE,R0                                                            
         LR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(RE),SPACES        ANY OFFICE?                                  
         BNH   RCDWN042            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYOFFEL,(R0),(RF)                          
*                                                                               
RCDWN042 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN043                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,AGYNAMEL,(R0),(RF)                          
*                                                                               
RCDWN043 DS    0H                                                               
         GOTOX (AGYADDRQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),       +        
               (0,VHPADV),(0,VHPCTY),ADDR                                       
         BNE   RCDWN050                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGADFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN050                                                         
         L     R5,0(R1)                                                         
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN044                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   RCDWN044                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD0EL,(R0),(RF)                          
*                                                                               
RCDWN044 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN045                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   RCDWN045                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD1EL,(R0),(RF)                          
*                                                                               
RCDWN045 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN046                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   RCDWN046                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD2EL,(R0),(RF)                          
*                                                                               
RCDWN046 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN047                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   RCDWN047                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD3EL,(R0),(RF)                          
*                                                                               
RCDWN047 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN048                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
*                                                                               
         LR    R1,R0                                                            
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES                                                   
         BNH   RCDWN048                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYONMEL,(R0),(RF)                          
*                                                                               
RCDWN048 DS    0H                                                               
*                                                                               
RCDWN050 DS    0H                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                          
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
*                                                                               
         LA    R0,*                                                             
         AHI   R0,RSALFOO-(*-4)                                                 
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN068                                                         
         L     R5,0(R1)                                                         
*                                  ADD NEW SALESPERSON ELEMENT                  
         GOTO1 ASETELEM,DMCB,AFABLK,SALDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN052                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALCODEL,(R0),(RF)                          
*                                                                               
RCDWN052 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN053                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALNAMEL,(R0),(RF)                          
*                                                                               
RCDWN053 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN054                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALTELEL,(R0),0                             
*                                                                               
RCDWN054 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN056                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALFAXEL,(R0),0                             
*                                                                               
RCDWN056 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN058                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALTEMEL,(R0),(RF)                          
*                                                                               
RCDWN058 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN060                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALDIVEL,(R0),(RF)                          
*                                                                               
RCDWN060 DS    0H                                                               
         LA    R3,3(R3)                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN062                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALTNMEL,(R0),(RF)                          
*                                                                               
RCDWN062 DS    0H                                                               
         GOTOX (GSALSTXQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                       
         BNE   RCDWN068                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,STXFOO-(*-4)                                                  
         GOTO1 PARSE,DMCB,ADDR,(R0)                                             
         BNE   RCDWN068                                                         
         L     R5,0(R1)                                                         
*                                                                               
RCDWN064 DS    0H                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       END OF TABLE?                                
         BZ    RCDWN068            YES                                          
*                                                                               
         LA    R3,WORK2                                                         
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    RCDWN066                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         AR    R0,R5                                                            
         GOTO1 AADDDATA,DMCB,AFABLK,SALSTXEL,(R0),0                             
*                                                                               
RCDWN066 DS    0H                                                               
         AH    R5,HALF                                                          
         B     RCDWN064                                                         
*                                                                               
RCDWN068 DS    0H                                                               
*                                                                               
RCDWNX   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
RCDWNERR DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* ADD CONTRACT DOWNLOAD                                                         
**********************************************************************          
ACONDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T81CFFD)                                            
         USING VHPARMD,R6                                                       
*                                                                               
         XC    WORK2,WORK2                                                      
W        USING GLVXFRSY,WORK2                                                   
         MVC   W.GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                        
         MVC   W.GLVXFRPR,=C'SWP'    SELLERS WORKSHEET                          
         MVC   W.GLVXTOSY,=C'REP'    TO THE REP SYSTEM                          
         MVC   W.GLVXTOPR,=C'CON'    CONTRACT PROGRAM                           
***      OI    W.GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER             
         OI    W.GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                      
         DROP  W                                                                
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
W        USING RCAUTOD,WORK2                                                    
         MVC   W.RCAUSTAT,VHPSTA                                                
         MVC   W.RCAUAGY,VHPAGY                                                 
         MVC   W.RCAUAGOF,VHPAOF                                                
         MVC   W.RCAUBUYR,VHPBUYER                                              
         MVC   W.RCAUADV,VHPADV                                                 
*                                                                               
         OC    VHPPRD,VHPPRD                                                    
         BZ    *+16                                                             
         MVC   W.RCAUPRD(2),=C'C='                                              
         MVC   W.RCAUPRD+2(3),VHPPRD                                            
*                                                                               
         CLC   VHPFFPRD,SPACES                                                  
         BNH   *+10                                                             
         MVC   W.RCAUPRD,VHPFFPRD                                               
*                                                                               
         MVC   W.RCAUSAL,VHPSAL                                                 
         MVC   W.RCAUTYPE,VHPCTY                                                
         GOTO1 VDATCON,DMCB,(8,VHPFLS),(2,W.RCAUFLT)                            
         GOTO1 VDATCON,DMCB,(8,VHPFLE),(2,W.RCAUFLT+2)                          
         DROP  W                                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,RCAUELLQ,GLRCAUTO                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                  FALINK TO EXIT AND RESUME                    
         GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB,0                           
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* CHANGE CONTRACT DOWNLOAD                                                      
**********************************************************************          
CCONDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T81CFFD)                                            
         USING VHPARMD,R6                                                       
         XC    WORK2,WORK2                                                      
W        USING GLVXFRSY,WORK2                                                   
         MVC   W.GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                        
         MVC   W.GLVXFRPR,=C'SWP'    SELLERS WORKSHEET                          
         MVC   W.GLVXTOSY,=C'REP'    TO THE REP SYSTEM                          
         MVC   W.GLVXTOPR,=C'CON'    CONTRACT PROGRAM                           
***      OI    W.GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER             
         OI    W.GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                      
         DROP  W                                                                
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
W        USING RCAUTOD,WORK2                                                    
         OI    W.RCAUFLAG,X'20'    CHANGE MODE                                  
*                                                                               
         ZAP   WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5)  PWOS CONTRACT NUMBER                         
         MVC   W.RCAUSCON,WORK                                                  
*                                                                               
         MVC   W.RCAUSTAT,VHPSTA                                                
         MVC   W.RCAUAGY,VHPAGY                                                 
         MVC   W.RCAUAGOF,VHPAOF                                                
         MVC   W.RCAUBUYR,VHPBUYER                                              
         MVC   W.RCAUADV,VHPADV                                                 
*                                                                               
         OC    VHPPRD,VHPPRD                                                    
         BZ    *+16                                                             
         MVC   W.RCAUPRD(2),=C'C='                                              
         MVC   W.RCAUPRD+2(3),VHPPRD                                            
*                                                                               
         CLC   VHPFFPRD,SPACES                                                  
         BNH   *+10                                                             
         MVC   W.RCAUPRD,VHPFFPRD                                               
*                                                                               
         MVC   W.RCAUSAL,VHPSAL                                                 
         MVC   W.RCAUTYPE,VHPCTY                                                
         GOTO1 VDATCON,DMCB,(3,VHPFLS),(2,W.RCAUFLT)                            
         GOTO1 VDATCON,DMCB,(3,VHPFLE),(2,W.RCAUFLT+2)                          
         DROP  W                                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK2,RCAUELLQ,GLRCAUTO                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  FALINK TO EXIT AND RESUME                    
         GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB,0                           
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* GENERIC GLOBBER DOWNLOAD                                                      
**********************************************************************          
GLOBDWN  NTR1  BASE=*,LABEL=*                                                   
         OC    CONERR,CONERR       ERROR?                                       
         BZ    *+14                NO                                           
         MVC   ERROR,CONERR                                                     
         B     EXITL                                                            
*                                                                               
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T81CFFD)                                            
         USING VHPARMD,R6                                                       
*                                                                               
         LA    R0,VHPARMLQ(R6)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         ICM   R0,15,CONNUM                                                     
         CVD   R0,DUB                                                           
         ZAP   VHPCON,DUB                                                       
*                                                                               
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
         BNE   EXITINV                                                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
* UPDATE PENDING DATA                                                           
*                                                                               
         L     R8,AIOREC                                                        
         LA    R8,RCONELEM-RCONKEY(R8)                                          
         MVI   ELCODE,X'0B'        DELETE SPL-RENAME ELEMENTT                   
         BAS   RE,FIRSTEL                                                       
         BNE   GLDWN002                                                         
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),(R8)                                   
*                                                                               
GLDWN002 DS    0H                                                               
         L     R8,AIOREC                                                        
         LA    R8,RCONELEM-RCONKEY(R8)                                          
         MVI   ELCODE,X'01'        GET CONTRACT DESC. ELEMENT                   
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                REQ'D                                        
*                                                                               
         USING RCONELEM,R8                                                      
         MVC   RCONRTGS,RTSRVC                                                  
         DROP  R8                                                               
*                                                                               
         L     R8,AIOREC                                                        
         LA    R8,RCONELEM-RCONKEY(R8)                                          
         MVI   ELCODE,X'1F'        GET CONTRACT EXTENDED DESC. ELEMENT          
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                REQ'D                                        
*                                                                               
         USING RCONXEL,R8                                                       
         OI    RCONSTAT,X'08'      SET SELWIN HAS BEEN HERE                     
         DROP  R8                                                               
*                                                                               
         CLI   VHPBUTYP,C' '                                                    
         BNH   GLDWN190                                                         
*                                                                               
         L     R8,AIOREC                                                        
         LA    R8,RCONELEM-RCONKEY(R8)                                          
         MVI   ELCODE,X'12'        GET SAR ELEMENT                              
         BAS   RE,FIRSTEL                                                       
         BNE   GLDWN010                                                         
*                                                                               
         ZIC   RE,1(R8) ELEMENT LENGTH                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R8)       SAVE THE ELEMENT THEN DELETE IT              
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),(R8)                                   
         B     GLDWN012                                                         
*                                                                               
S        USING RSARXEL,WORK                                                     
GLDWN010 XC    WORK,WORK           BUILD ELEMENT FROM SCRATCH                   
         MVI   S.RSARXCO,X'12'     ELEMENT CODE                                 
         GOTO1 VDATCON,DMCB,(5,0),(3,S.RSARXEDT)                                
*                                                                               
GLDWN012 DS    0H                    DO THE ACTUAL CHANGES                      
         MVI   S.RSARXLEN,RSARXLTH   ELEMENT LENGTH                             
         MVC   S.RSARXSRC,RTSRVC     RATING SOURCE                              
         MVC   S.RSARXRFR,VHPLENS    LENGTHS                                    
         MVI   S.RSARXFLG,0                                                     
         MVI   S.RSARXFL2,0                                                     
         OI    S.RSARXFLG,X'04'      USE PROPOSAL EXPANSION ELEMENTS            
*                                                                               
         LA    RE,*                                                             
         A     RE,=A(BUSTYPE-(*-4))                                             
GLDWN014 DS    0H                                                               
         CLI   0(RE),0             END OF TABLE?                                
         BZ    GLDWN016            YES                                          
         CLC   VHPBUTYP,0(RE)      TYPE MATCH?                                  
         BE    *+12                YES                                          
         LA    RE,L'BUSTYPE(RE)                                                 
         B     GLDWN014                                                         
*                                                                               
         OC    S.RSARXFL2,1(RE)    SET FLAGS                                    
         B     GLDWN018                                                         
*                                                                               
GLDWN016 DS    0H                                                               
         OI    S.RSARXFLG,X'80'      MARKET=BUDGET                              
         MVC   S.RSARXBGT,VHPMKBUD   COPY MARKET BUDGET                         
         OC    S.RSARXBGT,S.RSARXBGT                                            
         BNZ   *+8                                                              
         OI    S.RSARXFLG,X'40'      SET ZERO ENTERED FLAG                      
         MVC   S.RSARXSHG,VHPSHRGL   COPY SHARE GOAL                            
         OC    S.RSARXBGT,S.RSARXBGT                                            
         BNZ   *+8                                                              
         OI    S.RSARXFLG,X'20'      SET ZERO ENTERED FLAG                      
*                                                                               
GLDWN018 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,S.RSARXLAD)                                
*                                                    LAST UPDATE TODAY          
         XC    S.RSARXBKS,S.RSARXBKS                                            
         XC    S.RSARXDEM,S.RSARXDEM                                            
         XC    S.RSARXDPT,S.RSARXDPT                                            
*                                                                               
* ADD BOOKS TO SAR AND EXPANSION ELEMENT                                        
*                                                                               
         L     R8,AIOREC                                                        
         LA    R8,RCONELEM-RCONKEY(R8)                                          
         MVI   ELCODE,RCPRBKEQ     BOOK EXPANSION ELEMENT                       
         BAS   RE,FIRSTEL          FIND AND DELETE                              
         BNE   GLDWN020                                                         
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),(R8)                                   
*                                                                               
GLDWN020 DS    0H                                                               
         L     R3,ADDR             START OF BOOKS                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          NUMBER OF BOOKS                              
         BZ    GLDWN030            NO BOOKS                                     
*                                                                               
         LA    R3,1(R3)            BUMP PAST NUMBER OF BOOKS                    
         SR    RF,RF               COUNT BOOKS                                  
*                                                                               
         XC    WORK2,WORK2                                                      
E        USING RCPRBKEL,WORK2                                                   
         MVI   E.RCPRBKCO,RCPRBKEQ                                              
GLDWN022 DS    0H                                                               
         CHI   RF,6                DONE WITH RSARXBKS?                          
         BNL   GLDWN024            YES                                          
*                                                                               
         LR    R2,RF                                                            
         MHI   R2,3                                                             
         LA    R2,S.RSARXBKS(R2)                                                
         MVC   0(3,R2),0(R3)                                                    
*                                                                               
GLDWN024 DS    0H                                                               
         LR    R2,RF                                                            
         MHI   R2,L'RCPRBKBK                                                    
         LA    R2,E.RCPRBKBK(R2)                                                
         MVC   1(1,R2),3(R3)       MOVE BOOK TYPE                               
         MVC   2(3,R2),0(R3)       MOVE BOOK                                    
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,GLDWN022         NEXT BOOK                                    
*                                                                               
         LR    R2,RF               GET EXPANSION ELEMENT LENGTH                 
         MHI   R2,L'RCPRBKBK                                                    
         AHI   R2,RCPRBKOQ                                                      
         STC   R2,E.RCPRBKLN                                                    
         DROP  E                                                                
*                                                                               
         MVI   ELCODE,RCPRBKEQ     FIND THE ELEMENT A HOME                      
         BAS   RE,FINDSPOT                                                      
         LR    R0,R1                                                            
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),WORK2,(R0)                             
*                                                                               
* ADD DEMOS TO SAR                                                              
*                                                                               
GLDWN030 DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          NUMBER OF DEMOS                              
         BZ    GLDWN040            NO DEMOS                                     
*                                                                               
         LA    R3,1(R3)            BUMP PAST NUMBER OF DEMOS                    
         LA    RF,S.RSARXDEM                                                    
GLDWN032 DS    0H                                                               
         MVC   0(3,RF),0(R3)                                                    
         LA    RF,3(RF)                                                         
         LA    R3,3(R3)                                                         
         BCT   R1,GLDWN032         NEXT DEMO                                    
*                                                                               
* ADD DAYPARTS TO SAR AND EXPANSION ELEMENT                                     
*                                                                               
GLDWN040 DS    0H                                                               
         L     R8,AIOREC                                                        
         LA    R8,RCONELEM-RCONKEY(R8)                                          
         MVI   ELCODE,RCPRDPEQ     DAYPART EXPANSION ELEMENT                    
         BAS   RE,FIRSTEL          FIND AND DELETE                              
         BNE   GLDWN042                                                         
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),(R8)                                   
*                                                                               
GLDWN042 DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          NUMBER OF DAYPARTS                           
         BZ    GLDWN050            NO DAYPARTS                                  
*                                                                               
         LA    R3,1(R3)            BUMP PAST NUMBER OF DAYPARTS                 
         SR    RF,RF               COUNT DAYPARTS                               
*                                                                               
         XC    WORK2,WORK2                                                      
E        USING RCPRDPEL,WORK2                                                   
         MVI   E.RCPRDPCO,RCPRDPEQ                                              
GLDWN044 DS    0H                                                               
         CHI   RF,6                DONE WITH RSARXDPT?                          
         BNL   GLDWN046            YES                                          
*                                                                               
         LR    R2,RF                                                            
         MHI   R2,3                                                             
         LA    R2,S.RSARXDPT(R2)                                                
         MVC   0(1,R2),0(R3)       COPY DAYPART CODE                            
         MVC   1(2,R2),3(R3)       COPY LAST HALFWORD OF CPP                    
*                                                                               
GLDWN046 DS    0H                                                               
         LR    R2,RF                                                            
         MHI   R2,L'RCPRDPDP                                                    
         LA    R2,E.RCPRDPDP(R2)                                                
         MVC   0(L'RCPRDPDP,R2),0(R3)                                           
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R3,5(R3)                                                         
         BCT   R1,GLDWN044         NEXT DAYPART                                 
*                                                                               
         LR    R2,RF               GET EXPANSION ELEMENT LENGTH                 
         MHI   R2,L'RCPRDPDP                                                    
         AHI   R2,RCPRDPOQ                                                      
         STC   R2,E.RCPRDPLN                                                    
         DROP  E                                                                
*                                                                               
         MVI   ELCODE,RCPRDPEQ     FIND THE ELEMENT A HOME                      
         BAS   RE,FINDSPOT                                                      
         LR    R0,R1                                                            
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),WORK2,(R0)                             
*                                                                               
* ADD THE PENDING ELEMENT                                                       
*                                                                               
GLDWN050 DS    0H                                                               
         MVI   ELCODE,X'12'        FIND THE ELEMENT A HOME                      
         BAS   RE,FINDSPOT                                                      
         LR    R0,R1                                                            
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),WORK,(R0)                              
         DROP  S                                                                
*                                                                               
* COMMENTS                                                                      
*                                                                               
         L     R8,AIOREC           DELETE COMMENTS                              
         LA    R8,RCONELEM-RCONKEY(R8)                                          
         MVI   ELCODE,X'11'                                                     
GLDWN110 BAS   RE,FIRSTEL                                                       
         BNE   GLDWN112                                                         
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),(R8)                                   
         B     GLDWN110                                                         
*                                                                               
GLDWN112 DS    0H                                                               
         SR    R4,R4                                                            
         ICM   R4,1,0(R3)          NUMBER OF COMMENTS                           
         BZ    GLDWN130            NO COMMENTS                                  
*                                                                               
         LA    R3,1(R3)            BUMP PAST NUMBER OF COMMENTS                 
*                                                                               
GLDWN120 DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R2,WORK2                                                         
         MVI   0(R2),X'11'         ELEMENT CODE                                 
         LA    RE,59(R3)           GET COMMENT LENGTH                           
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RE,R3                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R2),0(R3)                                                    
         LA    RE,3(RE)            ELEMENT LENGTH                               
         STC   RE,1(R2)                                                         
*                                                                               
         MVI   ELCODE,X'11'        FIND THE ELEMENT A HOME                      
         BAS   RE,FINDSPOT                                                      
         LR    R0,R1                                                            
         GOTO1 VRECUP,DMCB,(C'R',AIOREC),WORK2,(R0)                             
*                                                                               
         LA    R3,60(R3)                                                        
         BCT   R4,GLDWN120                                                      
*                                                                               
GLDWN130 DS    0H                                                               
         GOTO1 VPUTREC,AIOREC                                                   
*                                                                               
* UPDATE COMPETITIVE STATION RECORD                                             
*                                                                               
GLDWN190 DS    0H                                                               
         L     RE,AIO2                                                          
         LHI   RF,LENIO                                                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R8,AIO2                                                          
         USING RSLWREC,R8                                                       
         MVI   RSLWKTYP,RSLWKTYQ                                                
         MVI   RSLWKSTY,RSLWKSBQ                                                
         MVC   RSLWKREP,REPALPHA                                                
*                                                                               
         ZAP   WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5) CONTRACT NUM IN PWOS                          
         MVC   RSLWKCON,WORK                                                    
         MVC   RSLWRLEN,=Y(RSLWR1ST-RSLWREC)                                    
         DROP  R8                                                               
*                                                                               
         XC    WORK,WORK           ADD DESCRIPTION ELEMENT                      
E        USING RSWDSELD,WORK                                                    
         MVI   E.RSWDSEL,RSWDSELQ                                               
         MVI   E.RSWDSLEN,RSWDSLNQ                                              
         GOTO1 VDATCON,DMCB,(5,0),(19,E.RSWDSNEW)                               
         GOTO1 VDATCON,DMCB,(5,0),(19,E.RSWDSCHG)                               
         DROP  E                                                                
*                                                                               
         LA    R8,RSLWR1ST-RSLWREC(R8)                                          
         GOTO1 VRECUP,DMCB,(C'R',AIO2),WORK,(R8)                                
*                                                                               
         LA    R8,RSWDSLNQ(R8)                                                  
         SR    R4,R4                                                            
         ICM   R4,1,0(R3)          NUMBER OF COMPETITIVE STATIONS               
         BZ    GLDWN210            NONE                                         
*                                                                               
         LA    R3,1(R3)            BUMP PAST NUMBER OF COMPETITVE               
*                                                                               
GLDWN200 DS    0H                                                               
         XC    WORK,WORK                                                        
E        USING RSWCSELD,WORK                                                    
         MVI   E.RSWCSEL,RSWCSELQ                                               
         MVI   E.RSWCSLEN,RSWCSLNQ                                              
         MVC   E.RSWCSSTA,0(R3)                                                 
         DROP  E                                                                
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',AIO2),WORK,(R8)                                
         LA    R8,RSWCSLNQ(R8)                                                  
*                                                                               
         LA    R3,5(R3)                                                         
         BCT   R4,GLDWN200                                                      
*                                                                               
GLDWN210 DS    0H                  SEE IF RECORD EXISTS                         
         XC    KEY,KEY                                                          
         L     R1,AIO2                                                          
         MVC   KEY(L'RSLWKEY),0(R1)                                             
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSLWKEY),KEYSAVE                                           
         BNE   GLDWN220                                                         
*                                                                               
         MVC   AIOREC,AIO1                                                      
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC           GET CREATION DATE                            
         MVI   ELCODE,RSWDSELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R2,R8                                                            
         L     R8,AIO2                                                          
         MVI   ELCODE,RSWDSELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NEW      USING RSWDSEL,R8                                                       
OLD      USING RSWDSEL,R2                                                       
         MVC   NEW.RSWDSNEW,OLD.RSWDSNEW                                        
         DROP  OLD,NEW                                                          
*                                                                               
         MVC   AIOREC,AIO2                                                      
         GOTO1 VPUTREC,AIOREC                                                   
         B     GLDWN230                                                         
*                                                                               
GLDWN220 DS    0H                                                               
         MVC   AIOREC,AIO2                                                      
         GOTO1 VADDREC,AIOREC                                                   
*                                                                               
GLDWN230 DS    0H                                                               
         MVC   AIOREC,AIO1                                                      
*                                  ADD NEW CONTRACT ELEMENT                     
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                  ADD CONTRACT NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,VHPCON,0                           
*                                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
         EJECT                                                                  
*====================================================================*          
*                                                                               
* INTERESTING DATA STRINGS FOR PARSE                                            
*                                                                               
SALFOO   DC    AL1(QSALNAME,QSALTEL,QSALFAX,QSALTEAM,QTEMDVNM,QTEMNAME)         
         DC    X'00'                                                            
STXFOO   DC    AL1(QSALSTAX),X'00'                                              
DPTFOO   DC    AL1(QDPTCODE,QDPTSNAM,QDPTLNAM),X'00'                            
CTYFOO   DC    AL1(QCTYKCTY,QCTYDESC),X'00'                                     
CSTAFOO  DC    AL1(QMKTSTA),X'00'                                               
VSTAFOO  DC    AL1(QSTAMKT,QSTAAFFL,QSTACHAN),X'00'                             
VADVFOO  DC    AL1(QADVNAME),X'00'                                              
VPRDFOO  DC    AL1(QPRDNAME),X'00'                                              
VAGYFOO  DC    AL1(QAGYNAME),X'00'                                              
AGADFOO  DC    AL1(QAGADLN0,QAGADLN1,QAGADLN2,QAGADLN3,QAGADNAM),X'00'          
*                                                                               
RSTAFOO  DC    AL1(QSTAKSTA,QSTAMKT,QSTAAFFL,QSTACHAN),X'00'                    
RADVFOO  DC    AL1(QADVKADV,QADVNAME),X'00'                                     
RPRDFOO  DC    AL1(QPRDKPRD,QPRDNAME),X'00'                                     
RAGYFOO  DC    AL1(QAGYKAGY,QAGYKAOF,QAGYNAME),X'00'                            
RSALFOO  DC    AL1(QSALKSAL,QSALNAME,QSALTEL,QSALFAX,QSALTEAM,QTEMDVNM)         
         DC    AL1(QTEMNAME),X'00'                                              
*====================================================================*          
* BUSINESS TYPE CONVERSION TABLE                                                
BUSTYPE  DS    0CL2                                                             
         DC    C'B',X'80'          BONUS                                        
         DC    C'T',X'40'          TRADE                                        
         DC    C'D',X'20'          DIRECT RESPONSE                              
         DC    C'G',X'04'          GENERAL AVAIL                                
         DC    C'P',X'08'          PAID PROGRAMMING                             
         DC    X'0000'                                                          
*====================================================================*          
*                                                                               
* ROUTINE LIST FOR REQUEST HEADERS                                              
*                                                                               
REQHDRS  DS    0A                                                               
         DC    AL2(INITHDRQ),A(INITHDR)                                         
ROUTABLQ EQU   *-REQHDRS                                                        
         DC    AL2(NINVHDRQ),A(NINVHDR)                                         
         DC    AL2(NDATHDRQ),A(NDATHDR)                                         
         DC    AL2(NTXTHDRQ),A(NTXTHDR)                                         
         DC    AL2(CSTAHDRQ),A(CSTAHDR)                                         
         DC    AL2(VHDRHDRQ),A(VHDRHDR)                                         
         DC    AL2(VBUDHDRQ),A(VBUDHDR)                                         
         DC    AL2(RCHFHDRQ),A(RCHFHDR)                                         
         DC    AL2(ACONHDRQ),A(ACONHDR)                                         
         DC    AL2(CCONHDRQ),A(CCONHDR)                                         
         DC    AL1(0000)                                                        
*====================================================================*          
*                                                                               
* FIELD ROUTINE LIST FOR INIT REQUEST                                           
*                                                                               
INITFLDS DS    0A                                                               
         DC    AL2(INITSALQ),A(INITSAL)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR NEW INVENTORY REQUEST                                  
*                                                                               
NINVFLDS DS    0A                                                               
         DC    AL2(NIRTSRVQ),A(NIRTSRV)                                         
         DC    AL2(NISTAQ),A(NISTA)                                             
         DC    AL2(NIDPTQ),A(NIDPT)                                             
         DC    AL2(NIFLTSTQ),A(NIFLTST)                                         
         DC    AL2(NIFLTENQ),A(NIFLTEN)                                         
         DC    AL2(NIBOOKQ),A(NIBOOK)                                           
         DC    AL2(NITPBKQ),A(NIBOOK)                                           
         DC    AL2(NIT4BKQ),A(NIBOOK)                                           
         DC    AL2(NIPVBKQ),A(NIBOOK)                                           
         DC    AL2(NIUPGRDQ),A(NIUPGRD)                                         
         DC    AL2(NITPUPGQ),A(NIUPGRD)                                         
         DC    AL2(NIT4UPGQ),A(NIUPGRD)                                         
         DC    AL2(NIPVUPGQ),A(NIUPGRD)                                         
         DC    AL2(NIDEMOQ),A(NIDEMO)                                           
**JRD    DC    AL2(NIRATECQ),A(NIRATEC)                                         
**JRD    DC    AL2(NIRTLENQ),A(NIRTLEN)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR NEW DATA FOR INVENTORY REQUEST                         
*                                                                               
NDATFLDS DS    0A                                                               
         DC    AL2(NDRTSRVQ),A(NDRTSRV)                                         
         DC    AL2(NDFLTSTQ),A(NDFLTST)                                         
         DC    AL2(NDFLTENQ),A(NDFLTEN)                                         
         DC    AL2(NDBOOKQ),A(NDBOOK)                                           
         DC    AL2(NDTPBKQ),A(NDBOOK)                                           
         DC    AL2(NDT4BKQ),A(NDBOOK)                                           
         DC    AL2(NDPVBKQ),A(NDBOOK)                                           
         DC    AL2(NDUPGRDQ),A(NDUPGRD)                                         
         DC    AL2(NDTPUPGQ),A(NDUPGRD)                                         
         DC    AL2(NDT4UPGQ),A(NDUPGRD)                                         
         DC    AL2(NDPVUPGQ),A(NDUPGRD)                                         
         DC    AL2(NDDEMOQ),A(NDDEMO)                                           
**JRD    DC    AL2(NDRATECQ),A(NDRATEC)                                         
**JRD    DC    AL2(NDRTLENQ),A(NDRTLEN)                                         
         DC    AL2(NDSTAQ),A(NDSTA)                                             
         DC    AL2(NDINV#Q),A(NDINV#)                                           
         DC    AL2(NDEFFSTQ),A(NDEFFST)                                         
         DC    AL2(NDEFFENQ),A(NDEFFEN)                                         
         DC    AL2(NDTXTQ),A(NDTXT)                                             
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR NEW MARKET OR STATION TEXT FOR INVENTORY               
*                                                                               
NTXTFLDS DS    0A                                                               
         DC    AL2(NTRTSRVQ),A(NTRTSRV)                                         
         DC    AL2(NTSTAQ),A(NTSTA)                                             
         DC    AL2(NTBOOKQ),A(NTBOOK)                                           
         DC    AL2(NTTPBKQ),A(NTBOOK)                                           
         DC    AL2(NTT4BKQ),A(NTBOOK)                                           
         DC    AL2(NTPVBKQ),A(NTBOOK)                                           
         DC    AL2(NTDEMOQ),A(NTDEMO)                                           
         DC    AL2(NTMKTQ),A(NTMKT)                                             
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR COMPETITIVE STATION LIST                               
*                                                                               
CSTAFLDS DS    0A                                                               
         DC    AL2(CSSTAQ),A(CSSTA)                                             
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR VALIDATE HEADER                                        
*                                                                               
VHDRFLDS DS    0A                                                               
         DC    AL2(VHSTAQ),A(VHSTA)                                             
         DC    AL2(VHADVQ),A(VHADV)                                             
         DC    AL2(VHPRDQ),A(VHPRD)                                             
         DC    AL2(VHAGYQ),A(VHAGY)                                             
         DC    AL2(VHAOFQ),A(VHAOF)                                             
         DC    AL2(VHFLSQ),A(VHFLS)                                             
         DC    AL2(VHFLEQ),A(VHFLE)                                             
         DC    AL2(VHCTYPQ),A(VHCTYP)                                           
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR VALIDATE BOOKS/UPGRADES/DEMOS                          
*                                                                               
VBUDFLDS DS    0A                                                               
         DC    AL2(VBRTSRVQ),A(VBRTSRV)                                         
         DC    AL2(VBBOOKQ),A(VBBOOK)                                           
         DC    AL2(VBUPGRDQ),A(VBUPGRD)                                         
         DC    AL2(VBDEMOQ),A(VBDEMO)                                           
         DC    AL2(VBNIUPGQ),A(VBUPGRD)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR REFRESH CONTRACT HEADER FIELDS                         
*                                                                               
RCHFFLDS DS    0A                                                               
         DC    AL2(RCHFCONQ),A(RCHFCON)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR ADD CONTRACT                                           
*                                                                               
ACONFLDS DS    0A                                                               
         DC    AL2(ACSALQ),A(ACSAL)                                             
         DC    AL2(ACSTAQ),A(ACSTA)                                             
         DC    AL2(ACADVQ),A(ACADV)                                             
         DC    AL2(ACPRDQ),A(ACPRD)                                             
         DC    AL2(ACFFPRDQ),A(ACFFPRD)                                         
         DC    AL2(ACAGYQ),A(ACAGY)                                             
         DC    AL2(ACAOFQ),A(ACAOF)                                             
         DC    AL2(ACBUYERQ),A(ACBUYER)                                         
         DC    AL2(ACFLSQ),A(ACFLS)                                             
         DC    AL2(ACFLEQ),A(ACFLE)                                             
         DC    AL2(CCLENQ),A(CCLEN)                                             
         DC    AL2(ACCTYPQ),A(ACCTYP)                                           
         DC    AL2(ACRTSRVQ),A(ACRTSRV)                                         
         DC    AL2(ACBOOKQ),A(ACBOOK)                                           
         DC    AL2(ACDEMOQ),A(ACDEMO)                                           
         DC    AL2(ACDPTQ),A(ACDPT)                                             
         DC    AL2(ACBYCPPQ),A(ACBYCPP)                                         
         DC    AL2(ACBUTYPQ),A(ACBUTYP)                                         
         DC    AL2(ACMKBUDQ),A(ACMKBUD)                                         
         DC    AL2(ACSHRGLQ),A(ACSHRGL)                                         
         DC    AL2(ACPCOMQ),A(ACPCOM)                                           
         DC    AL2(ACCSTAQ),A(ACCSTA)                                           
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR CHANGE CONTRACT                                        
*                                                                               
CCONFLDS DS    0A                                                               
         DC    AL2(CCCON#Q),A(CCCON#)                                           
         DC    AL2(CCSALQ),A(CCSAL)                                             
         DC    AL2(CCSTAQ),A(CCSTA)                                             
         DC    AL2(CCADVQ),A(CCADV)                                             
         DC    AL2(CCPRDQ),A(CCPRD)                                             
         DC    AL2(CCFFPRDQ),A(CCFFPRD)                                         
         DC    AL2(CCAGYQ),A(CCAGY)                                             
         DC    AL2(CCAOFQ),A(CCAOF)                                             
         DC    AL2(CCBUYERQ),A(CCBUYER)                                         
         DC    AL2(CCFLSQ),A(CCFLS)                                             
         DC    AL2(CCFLEQ),A(CCFLE)                                             
         DC    AL2(CCLENQ),A(CCLEN)                                             
         DC    AL2(CCCTYPQ),A(CCCTYP)                                           
         DC    AL2(CCRTSRVQ),A(CCRTSRV)                                         
         DC    AL2(CCBOOKQ),A(CCBOOK)                                           
         DC    AL2(CCDEMOQ),A(CCDEMO)                                           
         DC    AL2(CCDPTQ),A(CCDPT)                                             
         DC    AL2(CCBYCPPQ),A(CCBYCPP)                                         
         DC    AL2(CCBUTYPQ),A(CCBUTYP)                                         
         DC    AL2(CCMKBUDQ),A(CCMKBUD)                                         
         DC    AL2(CCSHRGLQ),A(CCSHRGL)                                         
         DC    AL2(CCPCOMQ),A(CCPCOM)                                           
         DC    AL2(CCCSTAQ),A(CCCSTA)                                           
         DC    AL2(0000)                                                        
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
*====================================================================           
VHPARMD  DSECT                                                                  
VHPCON   DS    PL5                 CONTRACT #                                   
VHPSTA   DS    CL5                 STATION                                      
VHPADV   DS    CL4                 ADVERTISER                                   
VHPPRD   DS    CL3                 PRODUCT                                      
VHPFFPRD DS    CL20                FREE FORM PRODUCT                            
VHPAGY   DS    CL4                 AGENCY                                       
VHPAOF   DS    CL2                 AGENCY OFFICE                                
VHPBUYER DS    CL20                BUYER                                        
VHPFLS   DS    XL3                 FLIGHT START JULIAN                          
VHPFLE   DS    XL3                 FLIGHT END JULIAN                            
VHPSAL   DS    CL3                 SALESPERSON                                  
VHPCTY   DS    CL1                 CONTRACT TYPE                                
VHPBUTYP DS    CL1                 BUSINESS TYPE                                
VHPMKBUD DS    XL4                 MARKET BUDGET                                
VHPSHRGL DS    XL1                 SHARE GOAL                                   
VHPLENS  DS    XL(6*2)             LENGTHS                                      
VHPARMLQ EQU   *-VHPARMD                                                        
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE RETSTWORKD                                                     
         DSECT                                                                  
       ++INCLUDE REGENSBLK                                                      
       ++INCLUDE REGENPBLK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107RETST00   08/31/00'                                      
         END                                                                    
