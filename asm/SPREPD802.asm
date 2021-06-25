*          DATA SET SPREPD802  AT LEVEL 072 AS OF 11/19/19                      
*PHASE SPD802T                                                                  
*INCLUDE MEDBDESC                                                               
*INCLUDE SPRPFOOT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE REPUDESC                                                               
*INCLUDE REPCALOV                                                               
*INCLUDE COVAIL                                                                 
*===================================================================            
* 08MAY19 71 MHER 2-DECIMAL IMPS                                                
* 14APR10 67 AKAT DEMO PROGRAM NAME ONLY 15 CHARS LONG                          
* 13JUN08 64 AKAT DISPLAY 2-CHARACTER BOOK TYPES                                
* 30MAY03 50 PWES FIX PRINTING OF (RIDICULOUS) LARGE CCP VALUES                 
* 14FEB03  MHER  IGNORE MARKET 0 BUYS                                           
* 26DEC01  MHER  IF NEW PAGE FOR STATION, NEW PAGE FOR MKTTOTS TOO              
* 09NOV01  MHER  TO NEW PAGE FOR EACH STATION IN PROGPROF+11                    
* 27FEB98  MHER  OPTION TO SUPPRESS COSTS IN PROGPROF+9                         
*===================================================================            
         TITLE 'SPREPD802-SWEEP REPORT'                                         
         PRINT NOGEN                                                            
SPD802   CSECT                                                                  
         NMOD1 0,SPD802                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING MEDBLOCK,R2                                                      
         USING SUMDSECT,R3                                                      
         L     R2,MEDBUFF                                                       
         L     R7,=A(SP08WK)                                                    
         USING SP08WK,R7                                                        
         ST    R7,SP08R7                                                        
         STM   RA,RC,SP08RA                                                     
         LA    R3,MYBUFIO                                                       
         L     RF,=A(GETBUFC)                                                   
         ST    RF,GETBUF                                                        
         CLI   MODE,MKTLAST                                                     
         BL    BYPW                                                             
         GOTO1 MEDADDWT,DMCB,(RA)                                               
BYPW     DS    0H                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         MVC   SVMAXLIN,MAXLINES                                                
         L     RF,=V(SPRPFOOT)                                                  
         ST    RF,VFOOT                                                         
         L     RF,=A(PRTLIN)                                                    
         ST    RF,VREPORT                                                       
         L     RF,=V(REPCALOV)                                                  
         ST    RF,REPCALOV                                                      
         L     RF,=A(SUBPAREA)                                                  
         ST    RF,VSUBPARA                                                      
         L     RF,=A(SSPD804)                                                   
         ST    RF,ASPD804                                                       
         L     RF,=A(SSPD801)                                                   
         ST    RF,ASPD801                                                       
         OI    RQOPT2,RQOPT2_NETBUYS                                            
         MVI   FIRST,1                                                          
         J     EXIT                                                             
*                                                                               
M2       CLI   MODE,REQFRST                                                     
         BNE   M4                                                               
         SPACE 1                                                                
         NI    RCOPTS,X'FF'-RCOPT_NOCOST RESET FLAG                             
         XC    D8SVD0,D8SVD0                                                    
         MVI   FIRSTRB,C'Y'        RESET FIRST TIME REPL. BUY                   
         CLI   QOPT2,C'Y'          REPLACE BUY                                  
         BNE   M2A0                                                             
         MVI   FIRSTRB,C'Y'        SET FIRST TIME REPL. BUY                     
         CLI   QDEMOVRD,C'Y'       DEMO MENU                                    
         BNE   M2A0                                                             
         MVI   QDEMOVRD,C' '       DISALLOW DEMO MENU ON REP BUY                
         MVC   QDEMNOS(L'QDEMNOS),SPACES                                        
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
                                                                                
M2A0     LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         BRAS  RE,RFRSTC                                                        
*                                                                               
         MVI   MODE,RUNFRST                                                     
         BAS   R9,GOTOSUB                                                       
*                                                                               
         MVI   MODE,REQFRST                                                     
         GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
         BRAS  RE,BFLOAT                                                        
*                                                                               
M2A      BAS   R9,GOTOSUB                                                       
         MVI   FIRST,0                                                          
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
         MVC   VARIANCE,=F'15'                                                  
         CLI   PROGPROF+8,C' '                                                  
         BE    *+8                                                              
         CLI   PROGPROF+8,0                                                     
         BE    *+10                                                             
         MVC   VARIANCE+3(1),PROGPROF+8                                         
         CLI   QOPT4,C'Y'          ALLOW ZERO VARIANCE FOR                      
         BNE   *+10                SUPPRESS DETAIL FLAG                         
         XC    VARIANCE,VARIANCE                                                
         TM    QOPT5+1,X'F0'         OVERRIDE VARIANCE                          
         BNO   M2A1                                                             
         PACK  DUB,QOPT5+1(2)                                                   
         CVB   R0,DUB                                                           
         ST    R0,VARIANCE                                                      
M2A1     CLI   PROGPROF+3,C'Y'     DAYPART BREAKOUT                             
         JE    EXIT                                                             
         LA    R8,SUPDPT                                                        
         BAS   R9,SUPRPTS                                                       
         J     EXIT                                                             
         EJECT                                                                  
M4       CLI   MODE,ESTFRST                                                     
         BNE   M6                                                               
*                                                                               
         L     R1,ADEST                                                         
         LA    R1,EDEMOS-ESTHDR(R1)                                             
         OC    0(3,R1),0(R1)       MAKE SURE THERE ARE DEMOS                    
         BNZ   M5                                                               
         MVC   P(40),=C'NO DEMOS IN ESTIMATE - REQUEST CANCELLED'               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
M5       OI    RQOPTS,RQOPTS_POST    FLAG TO INDICATE SPOT POSTING              
*                                                                               
         LA    R0,20               SET FOR MAXDEMOS                             
         LA    R5,DEMTYPES                                                      
         XC    0(20,R5),0(R5)                                                   
*                                                                               
M5A      BRAS  RE,ISRATING         R1 POINTS TO 3-BYTE DEMO                     
         JNE   *+8                                                              
         MVI   0(R5),C'R'                                                       
*                                                                               
         LA    R1,3(R1)                                                         
         LA    R5,1(R5)                                                         
         JCT   R0,M5A                                                           
*                                                                               
         CLI   Q2OPT2,C' '           TEST REQUEST OVERRIDE                      
         BNH   *+10                                                             
         MVC   PROGPROF+9(1),Q2OPT2  USE REQUEST OVERRIDE                       
*                                                                               
         CLI   PROGPROF+9,C'Y'       TEST SUPPRESS COSTS                        
         BNE   *+8                                                              
         OI    RCOPTS,RCOPT_NOCOST SET FLAG FOR GETBUY/GETGOAL                  
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
         CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         B     *+8                 TESTING ONLY                                 
         MVI   SPOTPROF+8,3        FORCE A DAY                                  
         DROP  RE                                                               
*                                                                               
         MVC   MSSPPROF,SPOTPROF                                                
         BAS   R9,GOTOSUB                                                       
*                                                                               
         BRAS  RE,EFRSTC                                                        
         J     EXIT                                                             
         EJECT                                                                  
M6       CLI   MODE,STAFRST                                                     
         BNE   M8                                                               
         BAS   R9,GOTOSUB                                                       
*                                                                               
         CLI   PROGPROF+11,C'Y'                                                 
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   MID1(7),=C'STATION' SET UP STATION LINE                          
         MVC   MID1+8(9),BIGSTA                                                 
         MVC   SVMID,MID1                                                       
         XC    SPBUFSTA,SPBUFSTA                                                
         MVI   SPLPRINT,1                                                       
         CLI   FORCEHED,C'Y'                                                    
         JE    EXIT                                                             
         MVI   FORCEMID,C'Y'                                                    
         J     EXIT                                                             
         EJECT                                                                  
M8       CLI   MODE,PROCBUY                                                     
         BNE   M10                                                              
         MVI   MEDEXTAV,C'Y'                                                    
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
*                                                                               
         CLI   Q2OPT4,C'Y'         TEST LKUP=ALL                                
         JNE   *+8                                                              
         BRAS  RE,SETNOPBD         CHANGE ELCODES ON POSTBUY DEMOS              
*                                                                               
         CLC   =C'0000',QMKT       IF MARKET 0 REQUESTEDD                       
         JE    *+14                SKIPPING MARKET 0 BUYS IS IDIOTIC            
         OC    BUYKMKTN,BUYKMKTN   TEST MARKET 0 BUY                            
         JZ    EXIT                YES- IGNORE                                  
*                                                                               
         CLI   QPRGTYPE,C' '                                                    
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         JNE   EXIT                                                             
         DROP  R6                                                               
*                                                                               
M8WIMX   LA    RF,SVBUYKEY+4                                                    
         TM    SVBUYKEY,X'0C'      TEST ID POINTER                              
         BNO   *+8                                                              
         LA    RF,SVBUYKEY+6                                                    
         CLC   4(2,R6),0(RF)       TEST SPILL                                   
         BNE   *+12                YES                                          
         CLI   Q2OPT1,C'S'         TEST SPILL ONLY                              
         JE    EXIT                YES - SKIP ORIGINATING MKT                   
         LA    R6,24(R6)                                                        
         USING BDELEM,R6                                                        
*                                                                               
         CLI   SPCONLY,C'Y'        SPECIALS ONLY REPORT                         
         BNE   *+16                                                             
         CLI   BDPROGT-1,X'00'     IS THIS A SPECIAL                            
         JNE   EXIT                NO EXIT                                      
         MVI   BDPROGT-1,X'40'     RESET SPECIAL FLAG                           
*                                                                               
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDSPTLN,BDSEC                                                   
         XC    PSLIST,PSLIST                                                    
         XC    PSLADDR,PSLADDR                                                  
         CLI   BPRD,X'FF'          GET SPOT LENGTH FOR BRAND                    
         BE    M8SLOK                                                           
*                                                                               
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
*                                                                               
         LA    RE,PSLIST                                                        
*                                                                               
M8CKPRD  CLI   0(RE),0             END OF LIST                                  
         JE    EXIT                                                             
         CLI   1(RE),X'FE'                                                      
         BE    M8CKPRD2                                                         
         CLC   0(1,RE),BPRD        EQUAL TO REQUSTED PRODUCT                    
         BE    M8CKPRD4                                                         
*                                                                               
M8CKPRD2 LA    RE,2(RE)                                                         
         B     M8CKPRD                                                          
*                                                                               
M8CKPRD4 ST    RE,PSLADDR          SAVE ENTRY ADDRESS                           
         MVC   MEDSPTLN,1(RE)      SET CURRENT SLN                              
*                                                                               
M8SLOK   CLI   QOPT2,C'Y'          SPECIAL REPLACE BUY STUFF                    
         BNE   M8D0OK                                                           
         CLI   FIRSTRB,C'Y'        FIRST REPL. BUY MEDGETBY                     
         BNE   M8D0OK                                                           
*                                                                               
         GOTO1 MEDGETBY,DMCB,(RA),3 FORCE A LOOKUP TO GET D0 PROF               
         L     RE,ASVD0            OVERRIDE SD0 PROFILE                         
         LTR   RE,RE                                                            
         BZ    M8D0NF                                                           
         MVC   D8SVD0(2),0(RE)                                                  
         MVC   0(2,RE),=C'NN'                                                   
*                                                                               
M8D0NF   MVI   FIRSTRB,C'N'        OK FOR REST OF REQUEST                       
*                                                                               
M8D0OK   GOTO1 MEDGETBY,DMCB,(RA),2                                             
*                                                                               
         BAS   RE,ANYDATA          CHECK FOR ACTIVITY                           
         MVC   SVESTPNT(56),MEDVPEST   SAVE ESTIMATES                           
         CLI   QOPT2,C'Y'                                                       
         BNE   M8NORB                                                           
         MVC   SVBDATA1,MEDBY1                                                  
M8NORB   DS    0H                                                               
         BAS   R9,GOTOSUB                                                       
         CLC   MEDBRAND,BPRD       SAME BRAND EXTRACTED                         
         BNE   *+8                                                              
         CLI   MEDSPILL,C'Y'       EXIT IF SPILL                                
         BNE   *+8                                                              
         BAS   RE,ANYDATA          AND NO DATA                                  
         MVC   MEDVPEST(56),SVESTPNT   RESTORE ESTIMATES                        
         SPACE 2                                                                
*        MVI   ACTSW,1             SET FOR ACTIVITY                             
         CLI   MEDSPILL,C'Y'                                                    
         BNE   M8CAN                                                            
         CLI   QOPT2,C'Y'          IF THIS IS A REPLACE BUY                     
         BE    M8IGNOPT            REQUEST THEN IGNORE SPILL REPORTING          
*                                  OPTIONS. DO REPLACE BUY FOR ALL              
*                                  ORIGINATING AND SPILL BUYS                   
         CLI   SPOTPROF+5,C'0' BYPASS IF SPILL REPORTING NOT                    
         JE    EXIT            REQUIRED.                                        
         CLI   SPOTPROF+5,0                                                     
         JE    EXIT                                                             
M8IGNOPT CLI   SPLPRINT,1          SAVE SPILL IF SPILL PRINT ACTIVE             
         BNE   M8CAN                                                            
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',SPBUFSTA),0                        
         GOTO1 (RF),DMCB,(RA),(X'01',SPBUFMKT),0                                
         MVI   SPLPRINT,2                                                       
         SPACE 2                                                                
M8CAN    DS    0H                                                               
         MVI   ACTSW,1             SET FOR ACTIVITY                             
* IF THIS IS A REPLACE BUY                                                      
* REQUEST THEN IGNORE SPILL REPORTING                                           
* OPTIONS. DO REPLACE BUY FOR ALL                                               
* ORIGINATING AND SPILL BUYS.                                                   
         CLI   QOPT2,C'Y'                                                       
         BNE   M8NORB1                                                          
         L     RE,ADBUY                                                         
         CLI   3(RE),X'FF'                                                      
         BNE   M8NORB1                                                          
         MVC   RBMEDWK,MEDNUMWK                                                 
         XC    MEDNUMWK,MEDNUMWK                                                
         MVC   RBSVPRD,MEDBRAND    SET UP TO REPLACE ALL DEMOS                  
         MVC   RBLCHNK,MEDLCHNK                                                 
         MVC   MEDLCHNK,=F'200'                                                 
         MVI   MEDBRAND,X'FF'                                                   
         MVC   AFRSTSV,MEDAFRST                                                 
         LA    RE,MEDPERD                                                       
         ST    RE,MEDAFRST                                                      
         MVC   ALASTSV,MEDALAST                                                 
         ST    RE,MEDALAST                                                      
         MVC   EXTDMSV,MEDEXTDM                                                 
         MVI   MEDEXTDM,14                                                      
         MVI   SVRB,1                                                           
         B     M8NORB2                                                          
M8NORB1  CLC   MEDBRAND,BPRD       MEDBLOCK OK                                  
         BE    M8HAVBUF             YES - PROCESS DATA                          
         MVC   MEDBRAND,BPRD                                                    
M8NORB2  MVC   MEDSPTLN,BDSEC                                                   
         DROP  R6                                                               
M8NORB3  LA    R9,2                 NO - GET MEDBLOCK                           
         CLI   QRERATE,C' '        SET LOOKUP TYPE                              
         BE    M8GB2                                                            
         CLI   QRERATE,C'A'        ADJUST ONLY                                  
         BNE   M8GB1                                                            
         LA    R9,5                                                             
         B     M8GB2                                                            
M8GB1    LA    R9,3                PURCHASED RERATED                            
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R9,1(R9)                                                         
         CLI   QRERATE,C'I'        AFFID REQUEST                                
         BNE   *+8                                                              
         LA    R9,3(R9)                                                         
M8GB2    GOTO1 MEDGETBY,DMCB,(RA),(R9)                                          
*                                                                               
         CLI   MEDSPILL,C'Y'       SPILL DATA                                   
         BNE   *+8                                                              
         BAS   RE,ANYDATA                                                       
*                                                                               
M8GBNSP  DS    0H                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         CLI   SVRB,1                                                           
         BNE   M8HAVBUF                                                         
         MVC   MEDNUMWK,RBMEDWK                                                 
         MVC   MEDEXTDM,EXTDMSV                                                 
         MVC   MEDAFRST,AFRSTSV                                                 
         MVI   SVRB,0                                                           
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDLCHNK,RBLCHNK                                                 
         MVC   MEDALAST,ALASTSV                                                 
         SPACE 2                                                                
M8HAVBUF DS    0H                                                               
         CLI   SPLPRINT,2          PRINT SPILL CAPTION FIRST TIME               
         BNE   M8NOSPL                                                          
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFSTA),SPSTPL                   
         MVC   MID1+16(17),=C'***SPILLS FROM** '                                
         MVC   MID1+33(24),SPSTPL+17                                            
         MVC   SVMID,MID1                                                       
         MVI   SPLPRINT,0                                                       
         SPACE 2                                                                
M8NOSPL  GOTO1 =A(PRTSWP)                                                       
**NOP**  CLI   QOPT2,C'Y'                                                       
**NOP**  J     EXIT                                                             
         CLI   BPRD,X'FF'          WILL ALWAYS GET BDSEC                        
         JE    EXIT                SO JUST EXIT                                 
M8NXTPRD L     RE,PSLADDR          TRY ANOTHER SPOT LENGTH                      
         B     M8CKPRD2                                                         
         EJECT                                                                  
M10      CLI   MODE,STALAST                                                     
         BNE   M12                                                              
         CLI   SPLPRINT,2          IS THIS A SPILL STAION                       
         BNE   *+12                                                             
         CLI   SPOTPROF+5,1        BYPASS IF TYPE 1 REPORT                      
         BE    M10B                                                             
         MVC   MCOUNT,=F'1'                                                     
         MVI   BUFCDE,X'91'        GET STATION TOTAL RECORDS                    
         MVI   BUFHI,1                                                          
         MVI   BUFLV,1                                                          
*                                                                               
M10A     GOTO1 GETBUF                                                           
         OC    SWBKEY,SWBKEY                                                    
         BZ    M10B                                                             
         CLI   SWBSLN,X'FD'        SPILL                                        
         BE    M10A                DONT PRINT ON STATION TOTAL                  
         CLI   SWBSLN,X'FE'        ORIG                                         
         BE    M10A                DONT PRINT ON STATION TOTAL                  
         BAS   R9,PRTDPT                                                        
         MVC   P1+10(9),BIGSTA                                                  
         MVC   P1+20(5),=C'TOTAL'                                               
         GOTO1 =A(SWPRINT)                                                      
         CLI   SWBDTYP,2           ACHIEVED                                     
         BNE   M10A                                                             
         GOTO1 VREPORT               YES - PRINT                                
         B     M10A                                                             
*                                                                               
M10B     L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(20),LVCNTRL                                               
         TM    LVCNTRL,X'80'                                                    
         BO    M10C                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'91',(R8))                                
*                                                                               
M10C     GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'91',(R8)),(X'80',1)                    
         MVC   SVMID,SPACES                                                     
         BAS   R9,GOTOSUB                                                       
         J     EXIT                                                             
         EJECT                                                                  
M12      CLI   MODE,MKTLAST                                                     
         BNE   M14                                                              
         CLI   PROGPROF+11,C'Y'    IF NEW PAGE FOR STATION                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'       THEN FOR MARKET TOO !                        
         XC    MID1,MID1                                                        
         XC    MID2,MID2                                                        
         LA    RE,14                                                            
         LA    RF,P1                                                            
         MVC   1(L'P1-1,RF),SPACES                                              
         LA    RF,L'P1(RF)                                                      
         BCT   RE,*-10                                                          
         MVI   FORCEMID,C'N'                                                    
         MVC   MCOUNT,=F'1'                                                     
         MVI   BUFCDE,X'91'                                                     
         MVI   BUFHI,1                                                          
         MVI   BUFLV,2                                                          
         CLI   ACTSW,1                                                          
         BNE   M14B                                                             
*                                                                               
M12A     GOTO1 GETBUF                                                           
         OC    SWBKEY,SWBKEY                                                    
         BZ    M12B                                                             
         BAS   R9,PRTDPT                                                        
         MVC   P+10(6),=C'MARKET'                                               
         MVC   P+17(5),=C'TOTAL'                                                
         GOTO1 =A(SWPRINT)                                                      
         CLI   SWBDTYP,2                                                        
         BNE   M12A                                                             
         GOTO1 VREPORT                                                          
         B     M12A                                                             
*                                                                               
M12B     OC    SPBUFMKT(2),SPBUFMKT  PRINT ORIGINATING MARKETS                  
         BZ    M14B                  IF THERE WAS ANY SPILL                     
         MVC   P1(12),=C'****SPILL***'                                          
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFMKT),P1                       
         GOTO1 VREPORT                                                          
         B     M14B                                                             
*                                                                               
M14      CLI   MODE,MGR1LAST                                                    
         BNE   M16                                                              
         MVC   P1+4(12),MGR1BK                                                  
         MVC   P1+17(5),=C'TOTAL'                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   BUFHI,1                                                          
         MVI   BUFLV,3                                                          
*                                                                               
M14A     CLI   SPDUPTOT,C'Y'                                                    
         BE    M14B                                                             
         GOTO1 GETBUF                                                           
         OC    SWBKEY,SWBKEY                                                    
         BZ    M14B                                                             
         BAS   R9,PRTDPT                                                        
         GOTO1 =A(SWPRINT)                                                      
         CLI   SWBDTYP,2                                                        
         BNE   M14A                                                             
         GOTO1 VREPORT                                                          
         B     M14A                                                             
*                                                                               
M14B     SR    R9,R9                                                            
         IC    R9,BUFLV                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'91',(R8)),(X'80',(R9))                 
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         MVI   FORCEHED,C'Y'                                                    
         J     EXIT                                                             
*                                                                               
M16      CLI   MODE,MGR2LAST                                                    
         BNE   M18                                                              
         MVC   P1+4(12),MGR2BK                                                  
         MVC   P1+17(5),=C'TOTAL'                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   BUFHI,1                                                          
         MVI   BUFLV,4                                                          
         B     M14A                                                             
*                                                                               
M18      CLI   MODE,MGR3LAST                                                    
         BNE   M20                                                              
         MVC   P1+4(12),MGR3BK                                                  
         MVC   P1+17(5),=C'TOTAL'                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   BUFHI,1                                                          
         MVI   BUFLV,5                                                          
         B     M14A                                                             
*                                                                               
M20      CLI   MODE,PRDLAST                                                     
         MVC   P1,SPACES                                                        
         BNE   M22                                                              
         TM    LVCNTRL,X'80'                                                    
         JO    EXIT                                                             
         TM    QMKT,X'F0'                                                       
         JO    EXIT                                                             
         MVI   BUFCDE,X'92'                                                     
         MVI   BUFHI,1                                                          
         MVI   BUFLV,1                                                          
*                                                                               
M20A     CLI   SPDUPTOT,C'Y'                                                    
         BE    M20B                                                             
         GOTO1 GETBUF                                                           
         OC    SWBKEY,SWBKEY                                                    
         BZ    M20B                                                             
         BAS   R9,PRTDPT                                                        
         MVC   P1+10(13),=C'PRODUCT TOTAL'                                      
         GOTO1 =A(SWPRINT)                                                      
         CLI   SWBDTYP,2                                                        
         BNE   M20A                                                             
         GOTO1 VREPORT                                                          
         B     M20A                                                             
*                                                                               
M20B     L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(20),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'92',(R8))                                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'92',(R8)),(X'80',1)                    
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         J     EXIT                                                             
*                                                                               
M22      CLI   MODE,PGR1LAST                                                    
         BNE   M24                                                              
         MVC   P1+4(12),PGR1BK                                                  
         MVC   P1+17(5),=C'TOTAL'                                               
         MVI   BUFLV,3                                                          
*                                                                               
M22COM   MVI   BUFHI,1             COMMON ENTRY POINT FOR CLT-PGR TOTS          
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
M22A     CLC   WEIGHT+4(4),=F'1'                                                
         BE    M22B                                                             
         GOTO1 GETBUF                                                           
         OC    SWBKEY,SWBKEY                                                    
         BZ    M22B                                                             
         BAS   R9,PRTDPT                                                        
         GOTO1 =A(SWPRINT)                                                      
         CLI   SWBDTYP,2                                                        
         BNE   M22A                                                             
         GOTO1 VREPORT                                                          
         B     M22A                                                             
*                                                                               
M22B     SR    R9,R9                                                            
         IC    R9,BUFLV                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'92',(R8)),(X'80',(R9))                 
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         J     EXIT                                                             
*                                                                               
M24      CLI   MODE,PGR2LAST                                                    
         BNE   M26                                                              
         MVC   P1+4(12),PGR2BK                                                  
         MVC   P1+17(5),=C'TOTAL'                                               
         MVI   BUFLV,4                                                          
         B     M22COM                                                           
*                                                                               
M26      CLI   MODE,PGR2LAST                                                    
         BNE   M28                                                              
         MVC   P1+4(12),PGR3BK                                                  
         MVC   P1+17(5),=C'TOTAL'                                               
         MVI   BUFLV,5                                                          
         B     M22COM                                                           
*                                                                               
M28      CLI   MODE,CLTLAST                                                     
         BNE   M30                                                              
         TM    LVCNTRL,X'80'                                                    
         JO    EXIT                                                             
         TM    QMKT,X'F0'                                                       
         MVC   P1+10(12),=C'REPORT TOTAL'                                       
         MVI   BUFLV,2                                                          
         B     M22COM                                                           
*                                                                               
M30      CLI   MODE,MKTFRST                                                     
         BNE   M32                                                              
         XC    SPBUFMKT,SPBUFMKT                                                
         XC    SPBUFMK2,SPBUFMK2                                                
         MVI   ACTSW,0                                                          
*                                                                               
M30A     BAS   R9,GOTOSUB                                                       
         J     EXIT                                                             
*                                                                               
M32      CLI   MODE,REQLAST                                                     
         BNE   M34                                                              
         BAS   R9,GOTOSUB                                                       
         BRAS  RE,RLASTC                                                        
*                                                                               
         CLI   FIRSTRB,C'Y'        ONLY IF REPLACE BUY REQUEST                  
         JE    EXIT                                                             
         L     RE,ASVD0            RESTORE THE D0 PROF                          
         LTR   RE,RE                                                            
         JZ    EXIT                                                             
         MVC   0(2,RE),D8SVD0                                                   
         J     EXIT                                                             
*                                                                               
M34      CLI   MODE,PROCGOAL                                                    
         BE    *+8                                                              
         CLI   MODE,CLTFRST                                                     
         JNE   EXIT                                                             
         BAS   R9,GOTOSUB                                                       
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
GOTOSUB  CLI   MSACTIVE,1                                                       
         BE    GOTOSUB2                                                         
GOTOSUB1 CLI   MODE,PROCBUY        BYPASS MS IF HIGH                            
         BHR   R9                                                               
*                                                                               
GOTOSUB2 BRAS  RE,GOSUBCST                                                      
         BR    R9                                                               
         SPACE 2                                                                
PRTDPT   CLI   SWBDTYP,1                                                        
         BNER  R9                                                               
         GOTO1 VREPORT                                                          
         CLI   SWBSLN,X'FD'        CHECK FOR SPILL TOTAL                        
         BNE   *+12                                                             
         MVC   P(7),=C'*SPILL*'                                                 
         BR    R9                                                               
         CLI   SWBSLN,X'FE'        CHECK FOR ORIG TOTAL                         
         BNE   *+12                                                             
         MVC   P(6),=C'*ORIG*'                                                  
         BR    R9                                                               
         CLI   SWBDPC2,X'FF'                                                    
         BE    *+10                                                             
         MVC   P(3),SWBDPN2                                                     
         EDIT  (1,SWBSLN),(3,P1+5)                                              
         CLI   SWBSLN,X'FF'                                                     
         BNE   *+10                                                             
         MVC   P1+5(3),SPACES                                                   
         BR    R9                                                               
         EJECT                                                                  
*=============================================================                  
*SUPPRESS REPORT - A(SUPPRESSION TABLE) IN R8                                   
*=============================================================                  
                                                                                
SUPRPTS  L     RE,MEDTABLE                                                      
         SR    RF,RF                                                            
         IC    RF,0(R8)                                                         
         BCTR  RF,0                                                             
         MHI   RF,4                                                             
         LA    RF,0(RE,RF)                                                      
         OI    0(RF),X'80'         DELETE REPORT                                
         LA    R8,1(R8)                                                         
         CLI   0(R8),0                                                          
         BNE   SUPRPTS                                                          
         BR    R9                                                               
         EJECT                                                                  
*============================================================                   
* REFORMAT MEDPOST RECORDS INTO CURRENT BUFFALO FORMAT                          
*============================================================                   
                                                                                
RFMTBUFF NTR1  BASE=*,LABEL=*                                                   
         L     RA,SP08RA                                                        
         L     RC,SP08RC                                                        
         L     R7,SP08R7                                                        
         L     R2,MEDBUFF                                                       
         L     R3,0(R1)            A(INPUT RECORD)                              
         L     RF,4(R1)            A(BUFFALOC)                                  
         USING BUFFALOD,RF                                                      
         LR    R9,R3               SET TO DATA                                  
         A     R9,BUFFLKEY                                                      
*                                                                               
         CLI   MEDSPILL,C'Y'       IS THIS SPILL                                
         BNE   RFBUFF1                                                          
         CLI   SWBDPC1,X'FF'       IS THIS A TOTAL LINE                         
         BE    RFBUFF1                                                          
         CLI   SPOTPROF+5,1                                                     
         BNE   RFBUFF1                                                          
         XC    0(48,R9),0(R9)     SUPPRESS SPILL IN DETAILS                     
         B     ORIGX                                                            
*                                                                               
RFBUFF1  DS    0H'0'                                                            
         CLI   SWBSLN,X'FD'        CHECK FOR SPILL                              
         BNE   RFSPILX                                                          
         CLI   SPOTPROF+5,0                                                     
         BE    *+8                                                              
         CLI   SPOTPROF+5,2                                                     
         BE    *+12                                                             
         CLI   MEDSPILL,C'Y'                                                    
         BE    RFSPILX                                                          
         XC    0(48,R9),0(R9)      CLEAR IF NOT A SPILL STATION                 
*                                                                               
RFSPILX  DS    0H'0'                                                            
         CLI   SWBSLN,X'FE'        CHECK FOR ORIG                               
         BNE   ORIGX                                                            
         CLI   MEDSPILL,C'Y'       KILL IF SPILL DATA                           
         BE    RFSPILX2                                                         
         CLI   SPOTPROF+5,1        SUPPRESS ORIG LINE                           
         BE    ORIGX                                                            
         CLI   SPOTPROF+5,3                                                     
         BE    ORIGX                                                            
*                                                                               
RFSPILX2 XC    0(48,R9),0(R9)                                                   
*                                                                               
ORIGX    XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
ANYDATA  LA    R4,MEDPERD          EXIT IF NO DATA                              
         L     R4,4(R4)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         JZ    M8NXTPRD            GO BACK FOR NEXT PSLIST ENTRY                
         BR    RE                  RETURN TO CALLER                             
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
MYHEAD   NTR1  BASE=*,LABEL=*                                                   
         L     RA,SP08RA                                                        
         L     RC,SP08RC                                                        
         L     R7,SP08R7                                                        
         BRAS  RE,MYHEADC                                                       
         J     EXIT                                                             
SP08R7   DC    F'0'                                                             
SP08RA   DC    F'0'                                                             
SP08RB   DC    F'0'                                                             
SP08RC   DC    F'0'                                                             
         LTORG                                                                  
*=========================================================                      
* CHANGE ELEMENT CODES ON POSTBUY DEMOS TO SUPPRESS VALUES                      
* AND REMOVE -S FROM BDPROGT                                                    
*=========================================================                      
                                                                                
SETNOPBD NTR1  BASE=*,LABEL=*                                                   
         L     RE,ADBUY                                                         
         MVI   BDPROGT-1-BUYREC(RE),C' '      SUPPRESS -S                       
         LA    RE,24(RE)                                                        
*                                                                               
SETNOPB2 LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         JE    EXIT                                                             
         CLI   0(RE),X'22'                                                      
         JE    *+12                                                             
         CLI   0(RE),X'23'                                                      
         JNE   SETNOPB2                                                         
         OI    0(RE),X'80'                                                      
         J     SETNOPB2                                                         
         LTORG                                                                  
         EJECT                                                                  
GTCOM    NTR1  BASE=*,LABEL=*                                                   
         USING SP08WK,R7                                                        
         L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING COMELEM,R5                                                       
*                                                                               
         L     R0,=A(COMAREA)                                                   
         LHI   R1,400                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   NUMCOM,0                                                         
         CLI   PROGPROF+6,C'0'     MUST BE ACTIVE TO PRINT                      
         BL    GETCOMX                                                          
*                                                                               
GETCOM2  CLI   CMCODE,0                                                         
         BE    GETCOMX                                                          
         CLI   CMCODE,X'66'                                                     
         BE    GETCOM4                                                          
*                                                                               
GETCOM3  SR    R0,R0                                                            
         IC    R0,CMLEN                                                         
         AR    R5,R0                                                            
         B     GETCOM2                                                          
*                                                                               
GETCOM4  CLI   PROGPROF+6,C'1'                                                  
         BNE   GETCOM5                                                          
         CLI   CMDATA,C'$'         ACCEPT COMMENT IF $,4, OR 5                  
         BE    *+8                                                              
         CLI   CMNUM,4                                                          
         BE    *+8                                                              
         CLI   CMNUM,5                                                          
         BNE   GETCOM3                                                          
*                                                                               
GETCOM5  CLI   PROGPROF+6,C'2'                                                  
         BNE   *+14                                                             
         CLC   CMDATA(8),=C'COMMENT-'                                           
         BNE   GETCOM3                                                          
         L     R4,=A(COMAREA)                                                   
         CLI   CMNUM,5             GET COMMENT SLOT                             
         BH    GETCOM3                                                          
         SR    R9,R9                                                            
         IC    R9,CMNUM                                                         
         BCTR  R9,0                                                             
         MH    R9,=H'80'                                                        
         AR    R4,R9                                                            
         SR    R9,R9                                                            
         IC    R9,CMLEN                                                         
         SH    R9,=H'4'                                                         
         LTR   R9,R9                                                            
         BM    GETCOM3                                                          
         ZIC   RE,NUMCOM           BUMP COMMENT COUNTER                         
         LA    RE,1(RE)                                                         
         STC   RE,NUMCOM                                                        
         EX    R9,*+8                                                           
         B     GETCOM3                                                          
         MVC   0(0,R4),CMDATA                                                   
*                                                                               
GETCOMX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
PRTSWP   NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         USING SP08WK,R7                                                        
         MVI   ACTSW,1                                                          
         MVC   P1,SPACES           CLEAR OUT JUNK FROM SUPPRESSION              
         GOTO1 =V(VMDBDESC),DMCB,(RA),MYBUFIO                                   
         LA    RE,MYBUFIO                                                       
         USING BDEXTD,RE                                                        
         MVC   P1(3),BDPEST                                                     
         MVI   P1+3,C'-'                                                        
         MVC   P1+4(3),BDPLIN                                                   
         MVC   P1+8(L'BDPBDATE),BDPBDATE                                        
         MVC   P1+21(L'BDPWKS),BDPWKS                                           
         MVC   P1+24(L'BDPDAY),BDPDAY                                           
         MVC   P1+33(L'BDPNPWK),BDPNPWK                                         
         MVC   P1+37(L'BDPTIME),BDPTIME                                         
         MVC   P2+7(L'BDPSLN),BDPSLN                                            
         MVC   P2+12(L'BDPROG),BDPPROG                                          
         CLI   PROGPROF+9,C'Y'     SUPPRESS COSTS                               
         BE    *+10                                                             
         MVC   P2+30(9),BDPCOST+3                                               
         MVC   P2+6(1),BDPDPT                                                   
         CLI   QMED,C'C'           CHECK FOR COMBINED MEDIA                     
         BNE   NOTCOM                                                           
         L     RF,ADBUY                                                         
         MVC   HALF(1),0(RF)                                                    
         NI    HALF,X'0F'                                                       
         MVC   P2(4),=C'SPOT'                                                   
         CLI   HALF,1                                                           
         BE    *+10                                                             
         MVC   P2(4),=C'NTWK'                                                   
NOTCOM   DS    0H                                                               
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   P2+30(9),=C'**SPILL**'                                           
*                                                                               
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RF),C'C'                                        
         BNE   *+8                                                              
         BRAS  RE,GETDMCAP         GET CANADIAN DEMO CAPTION                    
         DROP  RE                                                               
         EJECT                                                                  
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         USING ORBELEM,R6                                                       
GETORB   CLI   0(R6),0                                                          
         BE    GETORBX                                                          
         CLI   0(R6),X'67'                                                      
         BE    GETORB1                                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     GETORB                                                           
GETORB1  LA    R5,ORBDAY                                                        
         USING ORBDAY,R5                                                        
         ZIC   R9,1(R6)                                                         
         CHI   R9,133              CAN ONLY HANDLE 8 ORBS                       
         BL    *+8                                                              
         LA    R9,132                                                           
         AR    R9,R6                                                            
         MVC   P3(8),=C'ORBIT '                                                 
         LA    R4,P4                                                            
GETORB3  CR    R5,R9                                                            
         BNL   GETORBX                                                          
         GOTO1 CODAY,DMCB,ORBDAY,(R4)                                           
         GOTO1 UNTIME,DMCB,ORBTIME,(0,9(R4))                                    
         MVC   21(7,R4),ORBDESC                                                 
*                                                                               
         ICM   R1,3,ORBDEM                                                      
         N     R1,=X'00003FFF'                                                  
         TM    ORBDEM,X'40'        TEST 2-DECIMAL                               
         BO    GETORB10                                                         
*                                                                               
GETORB8  EDIT  (R1),(4,29(R4)),1                                                
         B     GETORB22                                                         
*                                                                               
GETORB10 CLI   QMED,C'R'           RADIO DOES NOT DO 2-DEC                      
         JE    GETORB11                                                         
         CLI   DEMTYPES,C'R'       TEST TARGET IS RTG                           
         JNE   GETORB20                                                         
         CHI   R1,999              TEST 2-DEC FITS IN 4 SPACES                  
         BNH   GETORB20                                                         
*                                                                               
GETORB11 M     R0,=F'2'            NO - ROUND TO 1 DECIMAL                      
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         B     GETORB8                                                          
*                                                                               
GETORB20 EDIT  (R1),(4,29(R4)),2                                                
*                                                                               
GETORB22 LA    R4,132(R4)                                                       
         LA    R5,16(R5)                                                        
         B     GETORB3                                                          
         EJECT                                                                  
GETORBX  DS    0H                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         USING NDELEM,R6                                                        
         XC    BUYSDEMO,BUYSDEMO                                                
         CLI   MEDSPILL,C'Y'                                                    
         BNE   M80                                                              
M8SP1    CLI   NDCODE,3            GET SPILL ELEMENT ADDRESS                    
         BNE   M8SP2                                                            
         CLC   SVBUYKEY+4(2),NDCODE+4                                           
         BNE   M8SP2                                                            
         ST    R6,BUYSDEMO                                                      
         B     M80                                                              
M8SP2    SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    R6,R0                                                            
         CLI   NDCODE,0                                                         
         BNE   M8SP1                                                            
         SPACE 2                                                                
M80      L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         USING NDELEM,R6                                                        
M81      CLI   NDCODE,2            FIND ORIGINAL DEMO ELEMENT                   
         BE    M82                                                              
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    R6,R0                                                            
         B     M81                                                              
M82      GOTO1 DATCON,DMCB,(X'03',NDBOOK),(X'09',P2+40)                         
         CLI   NDBOOK,0                                                         
         BNE   *+10                                                             
         XC    P2+40(6),P2+40                                                   
         ST    R6,BUYADEMO                                                      
*                                                                               
M82A     L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         USING NDELEM,R6                                                        
M82B     CLI   NDCODE,X'24'        FIND BOOK TYPE ELEMENT                       
         BE    M82C                                                             
         CLI   NDCODE,0                                                         
         BE    M82D                                                             
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    R6,R0                                                            
         B     M82B                                                             
M82C     MVC   BYTE,2(R6)          BOOK TYPE                                    
         BAS   RE,GETBOOK          GET BOOK                                     
         MVC   P2+46(2),HALF       MOVE IN BOOK TYPE                            
*                                                                               
M82D     L     RE,MEDADEMO         POINT TO OLD/NEW TABLE                       
         L     R6,4(RE)            POINT TO NEW DEMO ELEMENT                    
*                                                                               
         CLI   MEDSPILL,C'Y'       TEST SPILL                                   
         BNE   M82E                                                             
*                                                                               
         MVC   P3+12(10),NDPROG+5  SET UP SPILL RPOGRAM NAME                    
         MVC   BYTE,NDBOOK+4                                                    
         BAS   RE,GETBOOK                                                       
         L     RE,MEDADEMO         RESET RE!!                                   
         MVC   P2+46(2),HALF       SET BOOK TYPE                                
         B     *+10                                                             
M82E     MVC   P3+12(L'NDPROG-1),NDPROG MOVE IN REGULAR PROG NAME               
         EJECT                                                                  
         CLI   QOPT2,C'Y'                                                       
         BNE   M8A                                                              
         CLI   COMSCORE,C'1'       TEST COMSCORE PASS 1                         
         JE    M8A                 YES - DO REPLACE BUY ON PASS 2               
         BRAS  RE,REPBUY                                                        
         L     RE,MEDADEMO                                                      
*                                                                               
* PRINT DETAIL DEMOS AND ADD TO TOTALS                                          
*                                                                               
M8A      LR    R9,RE                                                            
         L     R6,0(R9)            GET ORIGINAL ELEMENT                         
         LA    R8,OVRFLAG          SET OVERRIDE FLAGS                           
         BAS   RE,SETOVR                                                        
         L     R6,4(R9)            GET RERATE ELEMENT                           
         LA    R8,OVRFLAGR         SET OVERRIDE FLAGS FOR RERATE DEMOS          
         BAS   RE,SETOVR                                                        
         B     M82X                                                             
*                                                                               
GETBOOK  NTR1                                                                   
*                                                                               
         XC    HALF,HALF                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
GETBK00  CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    GETBKXIT            YES - JUST EXIT                              
         CLC   SPBKTYPN,BYTE       MATCH ON 1 CHAR BOOK TYPE?                   
         BE    *+10                YES                                          
         AR    RF,RE               NO - BUMP TABLE                              
         B     GETBK00                                                          
         MVC   HALF,SPBKTYPA       2-CHAR BOOK TYPE                             
         DROP  RF                                                               
*                                                                               
GETBKXIT J     EXIT                                                             
*                                                                               
SETOVR   NTR1                                                                   
         XC    0(L'OVRFLAG,R8),0(R8)                                            
         LA    RF,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               SET END ADDRESS                              
SETOVR2  CR    RF,R6                                                            
         BNL   SETOVRX                                                          
         CLC   PRDBUFLN,=H'56'                                                  
         BE    *+8                                                              
         LA    RF,4(RF)                                                         
         TM    0(RF),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R8),C'*'                                                       
         LA    RF,4(RF)                                                         
         LA    R8,1(R8)                                                         
         B     SETOVR2                                                          
SETOVRX  XIT1                                                                   
         SPACE 2                                                                
M82X     DS    0H                                                               
         LA    RE,P1+75            MOVE DEMO NAMES TO PRINT LINE                
         LA    RF,DNAMES                                                        
         LA    R9,14                                                            
*                                                                               
M83      CLI   0(RF),0                                                          
         BE    M84                                                              
         MVC   0(7,RE),0(RF)                                                    
         LA    RE,132(RE)                                                       
         LA    RF,7(RF)                                                         
         BCT   R9,M83                                                           
*                                                                               
M84      LA    R4,MEDPERD          PRINT DEMOS AND ADJUSTMENTS                  
         L     R4,4(R4)                                                         
         USING MEDDATA,R4                                                       
         LA    R3,MYBUFIO          REFORMAT DATA                                
         XC    MYBUFIO,MYBUFIO                                                  
         MVI   SWBRCDE,X'91'                                                    
         MVC   SWBDPC1(8),MEDDPGNO                                              
         MVC   SWBSLN,MEDSPTLN                                                  
         MVC   SWBADJ(56),MEDVAEST                                              
         MVC   SVBDATA,MEDBY1      SAVE ACHIEVED TOTALS                         
         CLI   QOPT2,C'Y'                                                       
         BNE   *+10                                                             
         MVC   SVBDATA,SVBDATA1                                                 
         MVC   SWBDL,MEDBYD                                                     
         MVC   SWBDLE,MEDBYDEQ                                                  
         MVC   SWBSPT,MEDBYSPT                                                  
*                                                                               
         LA    R8,SWBD1                                                         
         LA    R9,MEDVPEST                                                      
         LHI   R0,14                                                            
*                                                                               
RFMT1    L     RF,0(R9)            ADJUST FOR NUMBER OF SPOTS                   
         MH    RF,MEDBYSPT+2                                                    
         ST    RF,0(R8)                                                         
         LA    R8,8(R8)                                                         
         LA    R9,4(R9)                                                         
         BCT   R0,RFMT1                                                         
*                                                                               
         LHI   R0,14                                                            
         LA    R8,SWBD1                                                         
*                                                                               
RFMT2    ICM   RF,15,0(R8)                                                      
         BZ    RFMT3                                                            
         MH    RF,MEDEQFAC                                                      
         M     RE,=F'2'                                                         
         D     RE,=F'1000'                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
RFMT3    ST    RF,4(R8)                                                         
         LA    R8,8(R8)                                                         
         BCT   R0,RFMT2                                                         
         DROP  R4                                                               
*                                                                               
         L     R1,BUFFBUFF                                                      
         USING BUFFALOD,R1                                                      
         L     RF,=A(RFMTBUFF)                                                  
         ST    RF,BUFFHOOK                                                      
         MVC   WORK(2),=X'9101'                                                 
* MOVE DATA TO MEDBLOCK                                                         
         MVC   MEDBYD(12),SWBDL                                                 
         CLI   PROGPROF+9,C'Y'     TEST SUPPRESS COST DATA                      
         BNE   *+10                                                             
         XC    MEDBYD(8),MEDBYD    CLEAR OUT COST FIELDS                        
         MVC   MEDBY1(112),SWBD1                                                
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         GOTO1 MEDPOST,DMCB,(RA)   POST TO BUFFALO                              
         MVC   WORK(2),=X'9201'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         MVI   SWBDTYP,1                                                        
         GOTO1 =A(SWPRINT)         SET UP PRINT LINES                           
*REFORMAT ACHIEVED                                                              
         MVC   SWBADJ(56),MEDVAACH                                              
         LA    R8,SWBD1                                                         
         LA    R4,MEDPERD                                                       
         L     R4,4(R4)                                                         
         USING MEDDATA,R4                                                       
         MVC   SWBD1(112),SVBDATA                                               
         DROP  R4                                                               
*                                                                               
         MVC   WORK(2),=X'9102'                                                 
         MVC   MEDBYD(12),SWBDL                                                 
         MVC   MEDBY1(112),SWBD1                                                
         CLI   QOPT2,C'Y'                                                       
         BNE   FRMT4                                                            
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   SWBD1(112),MEDBY1                                                
*                                                                               
FRMT4    DS    0H                                                               
         GOTO1 MEDPOST,DMCB,(RA)        POST TO BUFFALO                         
         MVC   WORK(2),=X'9202'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         MVI   SWBDTYP,2                                                        
         GOTO1 =A(SWPRINT)                                                      
         L     R1,BUFFBUFF                                                      
         XC    BUFFHOOK,BUFFHOOK                                                
         DROP  R1                                                               
*                                                                               
         CLI   SUPPRESS,2          SPILL SUPPRESS SWITCH                        
         BE    FRMT5                                                            
         CLI   QOPT4,C'Y'          EXCEPTION REPORT                             
         BNE   *+12                                                             
         CLI   SUPPRESS,0                                                       
         BNE   FRMT5                                                            
         GOTO1 VREPORT                                                          
*                                                                               
         BRAS  RE,GTCOM            GET COMMENTS                                 
         CLI   NUMCOM,0                                                         
         BE    NOCOM                                                            
*                                                                               
         L     RE,=A(COMAREA)      NOW PRINT SELECTED ONES                      
         LA    R1,P1+33                                                         
         LA    R9,5                                                             
*                                                                               
FRMTCOM  OC    0(20,RE),0(RE)      ONLY CHECK A LITTLE BIT                      
         BZ    *+14                                                             
         MVC   0(80,R1),0(RE)      MOVE TO PRINT LINE                           
         LA    R1,132(R1)          NEXT PL                                      
         LA    RE,80(RE)           NEXT COMMENT                                 
         BCT   R9,FRMTCOM                                                       
         GOTO1 REPORT                                                           
*                                                                               
NOCOM    MVI   P1,0                SEND LINE FOR SEPARATION                     
         GOTO1 VREPORT                                                          
         B     FRMTX                                                            
         SPACE 2                                                                
FRMT5    LA    RF,14               CLEAR THE PRINT LINES                        
         LA    RE,P1                                                            
         MVC   0(132,RE),SPACES                                                 
         LA    RE,132(RE)                                                       
         BCT   RF,*-10                                                          
*                                                                               
FRMTX    BRAS  RE,PRPIG                                                         
         OC    P1(70),P1                                                        
         BZ    REPU                                                             
         GOTO1 VREPORT             PRINT PIGGYBACKS                             
*                                                                               
REPU     CLI   PROGPROF+7,C'Y'     SUPPRESS UPGRADE EXPRESSION                  
         BE    REPUX               YES - JUST EXIT                              
         GOTO1 =V(REPUDESC),DMCB,(RA),P1                                        
         GOTO1 VREPORT             PRINT UPGRADE                                
*                                                                               
REPUX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRPIG    NTR1  BASE=*,LABEL=*                                                   
         L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         L     R5,ADBUY                                                         
         CLI   3(R5),X'FF'         POL BUY                                      
         BNE   GPGPRNX              NO - EXIT                                   
*                                                                               
         LA    R0,COVRHLD                                                       
         LHI   R1,396                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   OVRCNT,0                                                         
         L     R5,MEDAFRST                                                      
         MVC   STRDTE,0(R5)                                                     
         L     R5,MEDALAST                                                      
         MVC   ENDDTE,2(R5)                                                     
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING REGELEM,R5                                                       
*                                                                               
GPGNXT   ZIC   RE,RLEN                                                          
         AR    R5,RE                                                            
         CLI   0(R5),0             END                                          
         BE    GPGPRN               YES - PRINT PIGGYBACKS                      
         CLI   RCODE,10                                                         
         BL    GPGNXT                                                           
         CLI   RCODE,13                                                         
         BH    GPGNXT                                                           
         CLI   RLEN,18                                                          
         BL    GPGNXT                                                           
         SR    R0,R0                                                            
         IC    R0,RLEN                                                          
         AHI   R0,-10                                                           
         SRL   R0,2                                                             
         LA    RE,RPPRD                                                         
         CLI   MEDBRAND,X'FF'                                                   
         BE    GPG1B                                                            
         ICM   RF,15,PSLADDR                                                    
         BZ    GPG1B                                                            
*                                                                               
GPG1     CLC   RPPRD(2),0(RF)      SAME PRD/SLN                                 
         BE    GPG1B               NO                                           
         LA    RE,4(RE)             NO - CHECK NEXT                             
         BCT   R0,GPG1                                                          
         B     GPGNXT                                                           
*                                                                               
GPG1B    SR    R0,R0                                                            
         IC    R0,RLEN                                                          
         AHI   R0,-10                                                           
         SRL   R0,2                                                             
         LA    RE,RPPRD                                                         
         LA    RF,WORK                                                          
         XC    WORK,WORK                                                        
*                                                                               
GPG1C    MVC   0(2,RF),0(RE)                                                    
         LA    RF,2(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,GPG1C                                                         
*                                                                               
         MVI   WORK+8,1                                                         
         LA    RE,COVRHLD                                                       
*                                                                               
GPG2     CLI   0(RE),0                                                          
         BE    GPG4                                                             
         CLC   WORK(8),0(RE)                                                    
         BE    GPG5                                                             
         LA    RE,9(RE)                                                         
         B     GPG2                                                             
         SPACE 2                                                                
GPG4     MVC   0(9,RE),WORK        INSERT INTO LIST                             
         B     GPGNXT                                                           
*                                                                               
GPG5     ZIC   RF,8(RE)            BUMP SPOT COUNT                              
         AHI   RF,1                                                             
         STC   RF,8(RE)                                                         
         B     GPGNXT                                                           
         DROP  R5                                                               
         EJECT                                                                  
*===============================================================                
* PRINT OUT POOL PIGGYBACKS                                                     
*===============================================================                
                                                                                
GPGPRN   LA    R4,P                                                             
         MVI   COVRFRST,1                                                       
         MVI   OVRCNT,0                                                         
         LA    R5,COVRHLD          POINT TO PIGGYBACKS                          
         CLI   0(R5),0             AND EXIT IF NONE THERE                       
         BE    GPGPRNX                                                          
         ST    R4,APL                                                           
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
*                                                                               
GPGPRN1  CLC   1(131,R4),0(R4)     FIRST UNUSED LINE                            
         BE    GPGPRN2                                                          
         LA    R4,132(R4)                                                       
         CLI   0(R4),0                                                          
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         ST    R4,APL                                                           
         B     GPGPRN1                                                          
*                                                                               
GPGPRN2  CLI   0(R5),0                                                          
         BE    GPGPRNX                                                          
         CLI   COVRFRST,1                                                       
         BNE   *+14                                                             
         MVC   0(20,R4),=C'*****PIGGYBACKS*****'                                
         MVI   COVRFRST,0                                                       
         CLI   OVRCNT,0                                                         
         BNE   *+8                                                              
         LA    R4,25(R4)                                                        
         MVI   132(R4),0                                                        
         LR    R1,R4               PRINT PARTNERS                               
         LR    R3,R5                                                            
GPGPRN3  CLI   0(R3),0                                                          
         BE    GPGPRN4                                                          
         LA    RF,CLIST                                                         
GPGPRN3A CLC   0(1,R3),3(RF)       GET BRAND                                    
         BE    GPGPRN3B                                                         
         LA    RF,4(RF)                                                         
         B     GPGPRN3A                                                         
GPGPRN3B MVC   0(3,R1),0(RF)                                                    
         MVI   3(R1),C'-'                                                       
         ZIC   RE,1(R3)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R1),DUB+6(2)                                                 
         MVI   6(R1),C'/'                                                       
         LA    R1,7(R1)                                                         
         LA    R3,2(R3)                                                         
         B     GPGPRN3                                                          
         SPACE 2                                                                
GPGPRN4  BCTR  R1,0                                                             
         MVI   0(R1),C' '                                                       
         LA    R4,25(R4)                                                        
         LA    R5,9(R5)                                                         
         ZIC   RE,OVRCNT                                                        
         LA    RE,1(RE)                                                         
         STC   RE,OVRCNT                                                        
         CLI   OVRCNT,3                                                         
         BNH   GPGPRN2                                                          
         MVI   OVRCNT,0                                                         
         L     R4,APL                                                           
         LA    R4,132(R4)                                                       
         ST    R4,APL                                                           
         B     GPGPRN2                                                          
         SPACE 2                                                                
GPGPRNX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RLASTC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,CURPH01                                                       
         LHI   R9,3                                                             
*                                                                               
RESPRGM1 L     R0,8(R6)                                                         
         L     R1,4(R6)                                                         
         L     RE,0(R6)                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R6,12(R6)                                                        
         BCT   R9,RESPRGM1                                                      
*                                                                               
         L     R0,MEDTABLE                                                      
         L     R1,LSPD804                                                       
         L     RE,ASPD804                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,SPECS                                                         
         L     R1,LSPD801                                                       
         L     RE,ASPD801                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
RLASTX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RFRSTC   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R2                                                      
         USING SUMDSECT,R3                                                      
         MVI   SPCONLY,C'N'                                                     
         CLC   QUESTOR(2),=C'-S'                                                
         BNE   *+12                                                             
         MVI   SPCONLY,C'Y'                                                     
         MVI   QOPT2,C'N'          INHIBIT REPLACE BUY                          
*                                                                               
         MVI   SVRB,0                                                           
         MVC   MAXLINES,SVMAXLIN                                                
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   FOOT1,C' '                                                       
         BE    M2NOFT                                                           
         ZIC   R0,MAXLINES                                                      
         AHI   R0,-3                                                            
         STC   R0,MAXLINES                                                      
         MVC   FOOT1,SPACES                                                     
M2NOFT   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   MSACTIVE,0                                                       
         MVC   MSOPT,SPACES        INITIALIZE MS OPTIONS                        
         MVC   MSOPT+4(1),QOPT5    SPILL REPORTING OPT APPLIES TO BOTH          
         MVC   SUBPROG1,=C'M2'     SET DEFAULT MEDIA SUMMARY                    
         CLI   QOPT2,C'Y'          IS IT A REPLACE BUY                          
         BNE   *+10                                                             
         MVC   PROGPROF(3),=C'M22' YES FORCE M2                                 
         SPACE 2                                                                
         CLI   PROGPROF,C' '                                                    
         BE    M2AA                                                             
         CLI   PROGPROF,0                                                       
         BE    M2AA                                                             
         CLI   PROGPROF,C'0'                                                    
         BE    M2AA                                                             
         CLC   QPRD,=C'POL'                                                     
         BE    M2AB                                                             
         CLI   PROGPROF+1,C'0'                                                  
         BE    M2AA                                                             
         MVI   MSACTIVE,1                                                       
         MVC   SUBPROG1,PROGPROF   SET BRAND MEDIA SUMMARY                      
         B     M2AA                                                             
M2AB     CLI   PROGPROF+2,C'0'                                                  
         BE    M2AA                                                             
         MVC   SUBPROG1+1(1),PROGPROF+2  SET POL MEDIA SUMMARY                  
         MVI   MSACTIVE,1                                                       
M2AA     DS    0H                                                               
         CLC   =C'SUB02=T',QUESTOR                                              
         BE    M2WTPX                                                           
* READ DO PROFILE TO GET LPMWK AND OVERNIGHTS OPTION                            
         MVC   WORK(10),=CL10'S0D0'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         GOTO1 GETPROF,DMCB,WORK,D0PROF,DATAMGR                                 
*                                                                               
*                                                                               
         CLI   Q2LPMWK,C'N'                                                     
         BNE   *+8                                                              
         MVI   D0PROF+10,C'N'                                                   
         CLI   Q2OVNITE,C'N'                                                    
         BNE   *+8                                                              
         MVI   D0PROF+11,C'N'                                                   
*                                                                               
         CLI   D0PROF+10,C'Y'      FORCE M3 FOR LPM WEEKLY                      
         BE    *+8                                                              
         CLI   D0PROF+11,C'Y'      FORCE M3 FOR OVERNIGHTS                      
         BE    *+8                                                              
         CLI   D0PROF+11,C'M'      FORCE M3 FOR OVERNIGHTS                      
         BE    *+8                                                              
         CLI   Q2LPMWK,C'Y'        FORCE M3 FOR LPM WEEKLY                      
         BE    *+8                                                              
         CLI   Q2OVNITE,C'M'       FORCE M3 FOR OVERNIGHTS                      
         BE    *+8                                                              
         CLI   Q2OVNITE,C'Y'       FORCE M3 FOR OVERNIGHTS                      
         BE    *+8                                                              
         CLI   Q2USER+2,C'W'       FORCE M3 FOR WEEKLY POSTS                    
         BNE   *+10                                                             
         MVC   SUBPROG1(2),=C'M3'                                               
M2WTPX   DS    0C                                                               
         SPACE 2                                                                
* READ MEDIA SUMMARY PROFILE                                                    
         MVC   WORK(10),=CL10'S000'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVC   WORK+2(2),SUBPROG1                                               
         GOTO1 GETPROF,DMCB,WORK,MSPROF,DATAMGR                                 
                                                                                
* SYNC THE SPILL OPTIONS OTHERWISE IT'S DISASTER IF M. EXCLUDES SPILL           
* AND D8 ATTEMPTS TO INCLUDE SPILL                                              
         CLC   =C'M2',SUBPROG1                                                  
         BNE   *+10                                                             
         MVC   MSPROF+4(1),PROGPROF+5                                           
         CLC   =C'M3',SUBPROG1                                                  
         BNE   *+10                                                             
         MVC   MSPROF+5(1),PROGPROF+5                                           
         CLC   =C'M4',SUBPROG1                                                  
         BNE   *+10                                                             
         MVC   MSPROF+6(1),PROGPROF+5                                           
* ****************************************************************              
                                                                                
* PURCH RERATE                                                                  
         MVI   MSPROF,C'A'         SET TO GOL V ACH                             
         CLI   PROGPROF+10,C'P'    TEST D8 OPT TO FORCE GOL TO PUR              
         BNE   *+8                                                              
         MVI   MSPROF,C'C'         SET TO PUR V ACH                             
*                                                                               
         CLI   QRERATE,C'I'        TEST AFFID RERATE                            
         BNE   COMPOK              YES                                          
         IC    R0,MSPROF                                                        
         AHI   R0,1                                                             
         STC   R0,MSPROF                                                        
         B     COMPOK1                                                          
*                                                                               
COMPOK   DS    0H                                                               
         CLI   QRERATE,C'U'                                                     
         BE    *+12                                                             
         CLI   SPCONLY,C'Y'                                                     
         BNE   COMPOK1                                                          
         LA    R6,C'C'             SET FOR PURCH VS ACH                         
         CLI   QRERATE,C'I'                                                     
         BNE   *+8                                                              
         LA    R6,1(R6)            MAKE INTO PURCH VS. AFFID                    
         STC   R6,MSPROF                                                        
COMPOK1  MVC   QCOMPARE(1),MSPROF                                               
*                                                                               
COMPOK2  XC    MSBFHOOK,MSBFHOOK                                                
         SPACE 2                                                                
         L     R6,VSUBPARA                                                      
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2(3),=C'01 '                                              
         BAS   R9,LOADPROG                                                      
         L     RE,DMCB+4                                                        
         ST    RE,SVPH01                                                        
         MVC   CURPH01(4),CURPGPTR                                              
         MVC   CURPH01+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2(3),=C'02 '                                              
         CLC   =C'SUB02=T',QUESTOR SUBPROGRAM TESTING                           
         BNE   *+8                                                              
         MVI   SUBPROG2+2,C'T'                                                  
         BAS   R9,LOADPROG                                                      
         L     RE,DMCB+4                                                        
         ST    RE,SVPH02                                                        
         MVC   CURPH02(4),CURPGPTR                                              
         MVC   CURPH02+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2(3),=C'04 '                                              
         BAS   R9,LOADPROG                                                      
         L     RE,DMCB+4                                                        
         ST    RE,SVPH04                                                        
         MVC   CURPH04(4),CURPGPTR                                              
         MVC   CURPH04+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         LA    R9,3                                                             
         LA    R6,CURPH01                                                       
*                                                                               
SAVPRGM  L     R0,0(R6)                                                         
         L     R1,4(R6)                                                         
         L     RE,8(R6)                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R6,12(R6)                                                        
         BCT   R9,SAVPRGM                                                       
*                                                                               
         MVI   MEDEXTAV,C'Y'                                                    
         MVI   MEDEXTDM,14                                                      
         CLC   SUBPROG1,=C'M3'     WEEKLIES DO 4 DEMOS ONLY                     
         BNE   M2NOM3                                                           
         CLI   MSPROF+3,C'N'                                                    
         BE    M2NOM3                                                           
         MVI   MEDEXTDM,4          WEEKLIES GET ONLY 4 DEMOS                    
*                                                                               
M2NOM3   DS    0H                                                               
         CLC   QUESTOR(4),=C'D=2,'                                              
         BNE   *+8                                                              
         MVI   MEDEXTDM,2                                                       
         MVC   SVSPECS,SPECS                                                    
         MVC   WORK(8),=C'SPD804  '                                             
         GOTO1 REPCALOV,DMCB,(RA),WORK                                          
         MVC   MEDTABLE,DMCB+4                                                  
         L     RF,ASPD804                                                       
         MVC   LSPD804,DMCB                                                     
         L     RE,MEDTABLE                                                      
         L     R1,LSPD804                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   WORK(8),=C'SPD801  '                                             
         GOTO1 REPCALOV,DMCB,(RA),WORK                                          
         MVC   SPECS,DMCB+4                                                     
         L     RF,ASPD801                                                       
         MVC   LSPD801,DMCB                                                     
         L     RE,SPECS                                                         
         L     R1,LSPD801                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
M2NOM3X  MVC   SVSPECS,SPECS       SAVE SWEEP SPECS                             
         XIT1                                                                   
*                                                                               
LOADPROG GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
BFLOAT   NTR1  BASE=*,LABEL=*                                                   
         L     R6,BUFFBUFF         ORIGINAL BUFFER                              
         USING BUFFALOD,R6                                                      
         MVC   4(8,R1),ABUFF       SET PREV. BUFFERS                            
         OC    ABUFF,ABUFF                                                      
         BNZ   BFLOAT2                                                          
         MVC   LNBUFF,=F'50000'    MINIMUM BUFFER                               
         GOTO1 =V(COVAIL),DMCB,C'LOOK'                                          
         L     R9,8(R1)            GET AMOUNT OF CORE LEFT                      
         C     R9,=F'1050000'                                                   
         BL    *+12                                                             
         S     R9,=F'1000000'     LEAVE ENOUGH FOR SUBPROGRAMS                  
         ST    R9,LNBUFF                                                        
         MVC   ABUFF,=F'50000'                                                  
         GOTO1 =V(COVAIL),DMCB,C'GET',ABUFF,LNBUFF                              
*                                                                               
BFLOAT2  OC    4(8,R1),4(R1)       ALLOCATE OK                                  
         BNZ   *+6                                                              
         DC    H'0'                NOT ENOUGH CORE                              
         MVC   ABUFF,4(R1)                                                      
         L     R9,4(R1)            SHIFT BUFFALO TO NEW AREA                    
         MVC   0(255,R9),0(R6)                                                  
         ST    R9,BUFFBUFF                                                      
         LR    R6,R9               SWITCH DSECT TO NEW BUFFER                   
         MVC   BUFFADDR,4(R1)      SET BUFFER ADDRESS                           
         L     R9,8(R1)            GET LENGTH OF BUFFER                         
         SR    R8,R8                                                            
         D     R8,BUFFLALL                                                      
         ST    R9,BUFFCRMX                                                      
         XIT1                                                                   
         DROP  R6                                                               
ABUFF    DC    A(0)                ADDRESS OF BUFFER                            
LNBUFF   DC    A(0)                LENGTH OF BUFFER                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* ESTIMATE FIRST ROUTINE                                                        
*================================================================               
                                                                                
EFRSTC   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R2                                                      
         USING SUMDSECT,R3                                                      
*                                                                               
         CLI   QOPT5,C' '          OVERRIDE SPILL OPTION                        
         BE    *+10                IF IT IS IN THE REQUEST                      
         MVC   PROGPROF+5(1),QOPT5                                              
         CLI   PROGPROF+5,C'N'                                                  
         BNE   *+8                                                              
         MVI   PROGPROF+5,0                                                     
         CLI   PROGPROF+5,0                                                     
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+5                                         
         NI    SPOTPROF+5,X'0F'                                                 
* SET NUMBER OF LEVELS                                                          
         LA    RF,LVCNTRL          CLEAR STOP CHARACTER                         
         LA    RE,5                                                             
         NI    0(RF),X'7F'                                                      
         LA    RF,1(RF)                                                         
         BCT   RE,*-8                                                           
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         L     RE,BUFFROWS                                                      
         BCTR  RE,0                                                             
         MHI   RE,4                                                             
         LA    RE,LVCNTRL(RE)                                                   
         OI    0(RE),X'80'                                                      
         DROP  RF                                                               
*                                                                               
         XC    DNAMES,DNAMES                                                    
         L     RE,PRDBUFF          FIND CURRENT PRODUCT                         
         LA    RF,220               IN PRODUCT BUFFER                           
*                                                                               
M4A      CLC   BPRD,0(RE)                                                       
         BE    M4B                                                              
         AH    RE,PRDBUFLN                                                      
         BCT   RF,M4A                                                           
         MVI   MODE,PRDLAST        CURRENT PROD NOT IN BUFFER                   
         J     EXIT                 FORCE PRODUCT LAST                          
M4B      DS    0H'0'                                                            
*                                                                               
* THIS ROUTINE WILL GET DEMO NAMES FOR NEW FORMAT                               
NEWM4B   LA    RE,28(RE)                                                        
         ST    RE,FULL             SAVE START OF LINE                           
         LA    R9,0                                                             
         LA    RF,14                                                            
         CLC   SUBPROG1,=C'M3'                                                  
         BNE   *+8                                                              
         LA    RF,4                                                             
         CLC   QUESTOR(4),=C'D=2,'                                              
         BNE   *+8                                                              
         LA    RF,2                                                             
NEWM4C   CLI   1(RE),0             COUNT THE DEMOS                              
         BE    NEWM4D                                                           
         LA    R9,1(R9)                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,NEWM4C                                                        
NEWM4D   L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         XC    0(255,R6),0(R6)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
* CANADA USES A DIFFERENT DEMO LIST                                             
         L     RF,ADCLT            SET UP FOR PARAMETER LIST                    
         USING CLTHDR,RF                                                        
         CLI   CEXTRA,C'U'         TEST US DEMOS                                
         BE    NEWDCDX                                                          
         DROP  RF                                                               
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF+7-AGYHDR(RF)                                          
         CLI   0(RF),C'C'          TEST CANADIAN AGY                            
         BNE   NEWDCDX                                                          
         MVI   DBSELMED,C'C'       SET CANADIAN MEDIA                           
NEWDCDX  DS    0C                                                               
         DROP  R6                                                               
         L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
*                                                                               
         L     R5,FULL                                                          
         GOTO1 DEMOCON,DMCB,((R9),(R5)),(2,DNAMES),(C'S',ADBLOCK),     *        
               EUSRNMS,VNONTNMS                                                 
         STC   R9,NODEM                                                         
         STC   R9,MEDEXTDM                                                      
         DROP  R6                                                               
*                                                                               
         CLC   QPRD,=C'POL'                                                     
         JE    EXIT                                                             
         CLI   QOPT2,C'Y'                                                       
         JNE   EXIT                                                             
         MVC   SVQPRD,QPRD                                                      
         MVC   QPRD,=C'POL'                                                     
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         MVC   QPRD,SVQPRD                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
* CALL IN SUBPROGRAM AND SWAP PARAMETERS                                        
GOSUBCST NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R2                                                      
         USING SUMDSECT,R3                                                      
         MVC   SVSPECS,SPECS       GO TO SUB - PROGRAM                          
         MVC   SVOPT,QOPT1                                                      
         MVC   SVSPPROF,SPOTPROF                                                
         MVC   SPOTPROF,MSSPPROF                                                
         MVC   QOPT1(7),MSOPT                                                   
         MVC   SVMDTAB,MEDTABLE                                                 
         MVC   SVPROF,PROGPROF                                                  
         MVC   PROGPROF,MSPROF                                                  
         MVC   SPECS,SVPH01                                                     
         MVC   MEDTABLE,SVPH04                                                  
         OC    MSBFHOOK,MSBFHOOK                                                
         BZ    GOSUB01                                                          
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   BUFFHOOK,MSBFHOOK                                                
         DROP  RF                                                               
GOSUB01  DS    0C                                                               
         MVC   SVSUPMKT,SPSUPMKT                                                
         MVC   SPSUPMKT,MSSUPMKT                                                
         MVC   SVRCSUB,RCSUBPRG                                                 
         MVC   SVHDHOOK,HEADHOOK                                                
         MVC   RCSUBPRG,MSRCSUB                                                 
         MVC   HEADHOOK,MSHDHOOK                                                
         L     RF,SVPH02                                                        
         GOTO1 (RF),DMCB,(RA)                                                   
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   MSBFHOOK,BUFFHOOK                                                
         XC    BUFFHOOK,BUFFHOOK                                                
         DROP  RF                                                               
         MVC   SVPH01,SPECS                                                     
         MVC   SVPH04,MEDTABLE                                                  
         MVC   MSSUPMKT,SPSUPMKT                                                
         MVC   SPSUPMKT,SVSUPMKT                                                
         MVC   MSOPT,QOPT1                                                      
         MVC   QOPT1(7),SVOPT                                                   
         MVC   SPECS,SVSPECS                                                    
         MVC   MSPROF,PROGPROF                                                  
         MVC   PROGPROF,SVPROF                                                  
         MVC   MSSPPROF,SPOTPROF                                                
         MVC   SPOTPROF,SVSPPROF                                                
         MVC   MEDTABLE,SVMDTAB                                                 
         MVC   MSRCSUB,RCSUBPRG                                                 
         MVC   MSHDHOOK,HEADHOOK                                                
         MVC   RCSUBPRG,SVRCSUB                                                 
         MVC   HEADHOOK,SVHDHOOK                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
MYHEADC  NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R2                                                      
         USING SUMDSECT,R3                                                      
         CLI   SPCONLY,C'Y'                                                     
         BNE   *+16                                                             
         MVC   H1+71(16),=C' (SPECIALS ONLY)'                                   
         MVC   H2+71(16),=C'----------------'                                   
*                                                                               
         CLI   QFILTER,C'F'        FILM NUMBER FILTER FOR COKE                  
         BNE   MYFILMX                                                          
         MVC   H8(8),=C'**FILM**'                                               
         MVC   H8+9(1),QFILTER+1                                                
MYFILMX  DS    0H'0'                                                            
         CLI   Q2OPT4,C'Y'         TEST IGNORE POSTBUY DEMOS                    
         BNE   *+10                                                             
         MVC   H8(8),=C'LKUP=ALL'                                               
*                                                                               
         EDIT  VARIANCE,(2,H10+118)                                             
         MVC   MID1(L'SVMID),SVMID                                              
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
         MVC   H7+50(33),=C'******** PRODUCT SUMMARY ********'                  
         CLI   MODE,CLTLAST                                                     
         BNE   *+10                                                             
         MVC   H7+50(33),=C'********* CLIENT SUMMARY ********'                  
         CLI   QOPT2,C'Y'                                                       
         BNE   MYHEAD1                                                          
         MVC   H13+55(13),=C'NEW  VS.  OLD'                                     
         MVC   H13+92(11),=C'NEW  V  OLD'                                       
         MVC   H13+106(9),=C'NEW   OLD'                                         
         MVC   H1+73(13),=C'(REPLACE BUY)'                                      
MYHEAD1  DS    0H                                                               
         CLI   PROGPROF+9,C'Y'     TEST SUPPRESS COSTS                          
         BNE   MYHEADX                                                          
         XC    H12+57(9),H12+57    GET RID OF 'COST/DEMO'                       
         XC    H13+55(13),H13+55   GET RID OF EST VS. ACH                       
         XC    H14+55(13),H14+55   GET RID OF ---   ---                         
         XC    H14+30(4),H14+30    GET RID OF 'COST'                            
MYHEADX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETBUFC  NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R2                                                      
         USING SUMDSECT,R3                                                      
         USING SP08WK,R7                                                        
         XC    MYBUFIO,MYBUFIO                                                  
         MVC   PRVBKEY,MYBUFIO2                                                 
         XC    MEDBYD(132),MEDBYD                                               
         LA    R3,MYBUFIO2                                                      
         CLI   BUFHI,1                                                          
         BNE   GBP1                                                             
         MVI   BUFHI,0                                                          
         XC    MYBUFIO2(20),MYBUFIO2                                            
         SR    R6,R6                                                            
         IC    R6,BUFLV                                                         
         MVC   MYBUFIO2(1),BUFCDE                                               
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BUFCDE,(R8)),MYBUFIO2,(R6)                
         MVC   PRVBKEY,MYBUFIO2                                                 
         B     GBHVREC                                                          
GBSEQ    SR    R6,R6                                                            
         IC    R6,BUFLV                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFCDE,(R8)),MYBUFIO2,(R6)                 
         SPACE 2                                                                
GBHVREC  TM    DMCB+8,X'80'                                                     
         BZ    GBP1                                                             
         XC    MYBUFIO2,MYBUFIO2   CLEAR RECORD AT EOF                          
         B     GBEXIT                                                           
         SPACE 2                                                                
GBP1     CLI   MYBUFIO2,0          EOF ON PREV READ                             
         BE    GBEXIT               YES - EXIT                                  
         CLC   MYBUFIO2(11),PRVBKEY SAME PRIMARY KEY                            
         BNE   GBEXIT                NO - EXIT                                  
         LA    R6,MYBUFIO2                                                      
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     R6,BUFFLKEY                                                      
         DROP  RF                                                               
         USING SWBDATA,R6                                                       
         CLI   SWBRPT,1            REFORMAT RECORD 1                            
         BNE   GBP2                                                             
         MVC   MEDBYD(12),SWBDL                                                 
         MVC   MEDBY1(32),SWBD1                                                 
         B     GBSEQ                                                            
         SPACE 2                                                                
GBP2     CLI   SWBRPT,2            REFORMAT RECORD 2                            
         BNE   GBP3                                                             
         MVC   MEDBY5(32),SWBD1                                                 
         B     GBSEQ                                                            
         SPACE 2                                                                
GBP3     CLI   SWBRPT,3            REFORMAT RECORD 3                            
         BNE   GB4                                                              
         MVC   MEDBY9(32),SWBD1                                                 
         B     GBSEQ                                                            
         SPACE 2                                                                
GB4      MVC   MEDBY13(16),SWBD1                                                
         B     GBSEQ                                                            
         DROP  R6                                                               
         SPACE 2                                                                
GBEXIT   LA    R3,MYBUFIO                                                       
         MVC   SWBKEY,PRVBKEY                                                   
         MVC   SWBDL(12),MEDBYD                                                 
         MVC   SWBD1(112),MEDBY1                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRTLIN   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* SET UP SWEEP REPORT PRINT LINES                                               
*=================================================================              
                                                                                
SWPRINT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,RC                                                    
         USING MEDBLOCK,R2                                                      
         USING SUMDSECT,R3                                                      
         USING SP08WK,R7                                                        
*                                                                               
         CLI   NODEM,0                                                          
         BE    SWPRINTX                                                         
         CLI   SPOTPROF+1,C'N'     ANY WEIGHTS                                  
         BE    SWRP                 NO                                          
         CLI   SWBDTYP,X'02'                                                    
         BE    *+12                                                             
         CLI   MODE,PROCBUY        DETAIL RECORD                                
         BE    SWRP                 YES                                         
*                                                                               
         MVC   DUB,SPWEIGHT                                                     
         LA    R4,DNAMES           SET UP FOR UNWEIGHTING                       
         LHI   R5,14                                                            
         LA    R6,SWBD1                                                         
*                                                                               
SWUNWGT  CLI   SPOTPROF+1,C'D'     UNWEIGHT TOTALS                              
         BE    SWUNW                                                            
         CLI   0(R4),C'E'                                                       
         BE    *+8                                                              
         CLI   0(R4),C'R'                                                       
         BNE   SWUNWGT2                                                         
*                                                                               
SWUNW    OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   SWUNWGT1                                                         
         XC    0(8,R6),0(R6)                                                    
         B     SWUNWGT2                                                         
*                                                                               
SWUNWGT1 DS    0H                                                               
         L     RF,0(R6)                                                         
         M     RE,=F'2'                                                         
         D     RE,DUB                                                           
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
*                                                                               
         L     RF,4(R6)                                                         
         M     RE,=F'2'                                                         
         D     RE,DUB                                                           
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
*                                                                               
SWUNWGT2 LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,SWUNWGT                                                       
*                                                                               
SWRP     DS    0H                                                               
         CLI   SWBDTYP,X'02'       DATA CODE 2 = ACHIEVED                       
         BE    SWRACH                                                           
         LA    RE,P1+75            MOVE DEMO NAMES TO PRINT LINES               
         LA    RF,DNAMES                                                        
         LHI   R9,14                                                            
*                                                                               
SWREST2  CLI   0(RF),0                                                          
         BE    SWREST4                                                          
         MVC   0(7,RE),0(RF)                                                    
         LA    RE,132(RE)                                                       
         LA    RF,7(RF)                                                         
         BCT   R9,SWREST2                                                       
*                                                                               
SWREST4  DS    0H                                                               
         OC    SWBADJ,SWBADJ       ANY ADJUSTMENTS                              
         BZ    SWREST20             NO - BYPASS ADJ PRINT                       
*                                                                               
         LA    R4,SWBADJ                                                        
         SR    R1,R1                                                            
         IC    R1,NODEM                                                         
         MVC   ALLOWLIN,NODEM                                                   
         LA    R8,P1+106                                                        
*                                                                               
SWREST10 ICM   RF,15,0(R4)         PRINT EST SVI                                
         BZ    SWREST12                                                         
         EDIT  (RF),(3,(R8))                                                    
*                                                                               
SWREST12 LA    R4,4(R4)                                                         
         LA    R8,132(R8)                                                       
         BCT   R1,SWREST10                                                      
*                                                                               
SWREST20 SR    R1,R1               PRINT ESTIMATED POINTS                       
         IC    R1,NODEM                                                         
         LA    R4,SWBD1                                                         
         LA    R9,OVRFLAG                                                       
         LA    R8,P1+87                                                         
         MVI   DEMINDX,0           DEMO SEQUENCE NUMBER                         
*                                                                               
SWREST22 L     RF,0(R4)            GET POINTS                                   
         CLI   MODE,PROCBUY                                                     
         BH    SWREST32                                                         
*                                                                               
         M     RE,=F'2'                                                         
         ICM   R0,15,SWBSPT                                                     
         BNZ   *+8                                                              
         LHI   R0,1                                                             
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
SWREST32 DS    0H                                                               
         LTR   RF,RF                                                            
         BZ    SWREST36                                                         
*                                                                               
         SR    RE,RE               SEE IF DEMO IS A RATING                      
         IC    RE,DEMINDX                                                       
         MHI   RE,7                                                             
         LA    RE,DNAMES(RE)                                                    
         CLI   0(RE),C'R'          TEST RATING                                  
         BE    *+12                                                             
         CLI   0(RE),C'E'          OR EXTENDED RATING                           
         BNE   SWREST33                                                         
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC RATINGS                           
         BZ    SWREST36            NO                                           
         B     SWREST34                                                         
*                                                                               
SWREST33 TM    RQOPTS,RQOPTS_2DECIMP   TEST 2-DEC IMPS                          
         BZ    SWREST36                                                         
*                                                                               
SWREST34 CLI   MODE,PROCBUY        IF NOT BUY DETAIL,                           
         BH    SWREST35            PRINT 1 DECIMAL ONLY                         
*                                                                               
         C     RF,=F'99999'        MAX VALUE WITH 2 DEC                         
         BH    SWREST35                                                         
         EDIT  (RF),(7,(R8)),2                                                  
         B     SWREST40                                                         
*                                                                               
SWREST35 M     RE,=F'2'            ROUND TO 1 DECIMAL                           
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
SWREST36 C     RF,=F'999999'                                                    
         BH    SWREST38                                                         
         EDIT  (RF),(7,(R8)),1                                                  
         B     SWREST40                                                         
*                                                                               
SWREST38 M     RE,=F'2'                                                         
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         EDIT  (RF),(7,(R8))                                                    
*                                                                               
SWREST40 MVC   7(1,R8),0(R9)       FLAG OVERRIDES                               
         LA    R9,1(R9)                                                         
         LA    R8,132(R8)                                                       
         LA    R4,8(R4)                                                         
         SR    RE,RE                                                            
         IC    RE,DEMINDX                                                       
         AHI   RE,1                                                             
         STC   RE,DEMINDX                                                       
         BCT   R1,SWREST22                                                      
                                                                                
* CALCULATE CPP                                                                 
                                                                                
         XC    OVRFLAG,OVRFLAG                                                  
         CLI   PROGPROF+9,C'Y'     TEST SUPPRESS COSTS                          
         BE    SWREST60                                                         
         BAS   RE,CALCCPP                                                       
*                                                                               
         SR    R1,R1               PRINT ESTIMATE CPP                           
         IC    R1,NODEM                                                         
         LA    R4,SWBD1E                                                        
         LA    R8,P1+52                                                         
*                                                                               
SWREST50 ICM   RF,15,0(R4)                                                      
         BZ    SWREST54                                                         
         C     RF,=F'999999'                                                    
         BH    SWREST52                                                         
         EDIT  (RF),(7,(R8)),2                                                  
         B     SWREST54                                                         
*                                                                               
SWREST52 M     RE,=F'2'                                                         
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         EDIT  (RF),(7,(R8)),0                                                  
*                                                                               
SWREST54 LA    R8,132(R8)                                                       
         LA    R4,8(R4)                                                         
         BCT   R1,SWREST50                                                      
         EJECT                                                                  
* SET ESTIMATE DEMO VALUES                                                      
                                                                                
SWREST60 DS    0H                                                               
         XC    MEDVAEST(56),MEDVAEST                                            
         LA    R1,14                                                            
         LA    RE,MEDVPEST                                                      
         LA    RF,SWBD1                                                         
*                                                                               
SWREST62 MVC   0(4,RE),0(RF)                                                    
         LA    RE,4(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R1,SWREST62                                                      
*                                                                               
         EDIT  SWBSPT,(4,P+47)                                                  
         CLI   MODE,PROCBUY                                                     
         BE    SWPRINTX                                                         
         EDIT  SWBSPT,(5,P+46)                                                  
         CLI   PROGPROF+9,C'Y'     TEST SUPPRESS COSTS                          
         BE    SWPRINTX                                                         
         EDIT  SWBDL,(13,P1+26),2,MINUS=YES,COMMAS=YES                          
         B     SWPRINTX                                                         
         EJECT                                                                  
*==============================================================                 
* GET ACHIEVED PRINT LINES                                                      
*==============================================================                 
                                                                                
SWRACH   OC    SWBADJ,SWBADJ       ANY ADJUSTMENTS                              
         BZ    SWRACH30             NO - BYPASS ADJ PRINT                       
*                                                                               
         LA    R4,SWBADJ                                                        
         LA    R1,14                                                            
         LA    R8,P1+112                                                        
*                                                                               
SWRACH10 ICM   RF,15,0(R4)         PRINT ACH SVI                                
         BZ    SWRACH12                                                         
         EDIT  (RF),(3,(R8))                                                    
*                                                                               
SWRACH12 LA    R4,4(R4)                                                         
         LA    R8,132(R8)                                                       
         BCT   R1,SWRACH10                                                      
*                                                                               
SWRACH30 LLC   R1,NODEM                                                         
         LA    R4,SWBD1                                                         
         LA    R8,P1+97                                                         
         LA    R9,OVRFLAGR                                                      
         MVI   DEMINDX,0                                                        
*                                                                               
SWRACH40 ICM   RF,15,0(R4)                                                      
         BZ    SWRACH46                                                         
         CLI   MODE,PROCBUY                                                     
         BH    SWRACH42                                                         
*                                                                               
         M     RE,=F'2'                                                         
         ICM   R0,15,SWBSPT                                                     
         BNZ   *+8                                                              
         LHI   R0,1                                                             
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
SWRACH42 LLC   RE,DEMINDX                                                       
         MHI   RE,7                                                             
         LA    RE,DNAMES(RE)                                                    
         CLI   0(RE),C'R'          TEST RATING                                  
         BE    *+12                                                             
         CLI   0(RE),C'E'          OR EXTENDED RATING                           
         BNE   SWRACH43                                                         
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMAL RATINGS                       
         BZ    SWRACH43                                                         
         B     SWRACH44                                                         
*                                                                               
SWRACH43 TM    RQOPTS,RQOPTS_2DECIMP  DEMO NOT A RTG - TEST 2-DEC IMPS          
         BZ    SWRACH46                                                         
                                                                                
* WE'VE GOT A 2-DEC VALUE - PRINT 2-DEC IN BUY DETAIL ONLY                      
                                                                                
SWRACH44 CLI   MODE,PROCBUY        TEST BUY DETAIL                              
         BH    SWRACH45            PRINT 1 DECIMAL ONLY                         
*                                                                               
         C     RF,=F'99999'        MAX VALUE WITH 2 DEC                         
         BH    SWRACH45                                                         
         EDIT  (RF),(7,(R8)),2                                                  
         B     SWRACH50                                                         
*                                                                               
SWRACH45 M     RE,=F'2'            ROUND TO 1 DECIMAL                           
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
SWRACH46 C     RF,=F'999999'       MAX VALUE WITH 1 DECIMAL                     
         BH    SWRACH48                                                         
         EDIT  (RF),(7,(R8)),1                                                  
         B     SWRACH50                                                         
*                                                                               
SWRACH48 M     RE,=F'2'                                                         
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         EDIT  (RF),(7,(R8))                                                    
*                                                                               
SWRACH50 MVC   7(1,R8),0(R9)       FLAG OVERRIDES                               
         LA    R9,1(R9)                                                         
         LA    R8,132(R8)                                                       
         LA    R4,8(R4)                                                         
         SR    RE,RE                                                            
         IC    RE,DEMINDX                                                       
         AHI   RE,1                                                             
         STC   RE,DEMINDX                                                       
         BCT   R1,SWRACH40                                                      
*                                                                               
         XC    OVRFLAGR,OVRFLAGR                                                
         CLI   PROGPROF+9,C'Y'     TEST SUPPRESS COSTS                          
         BE    SWRACH70                                                         
*                                                                               
         BAS   RE,CALCCPP          CALCULATE CPP                                
*                                                                               
         SR    R1,R1               PRINT ACHIEVED CPP                           
         IC    R1,NODEM                                                         
         LA    R4,SWBD1E                                                        
         LA    R8,P1+62                                                         
*                                                                               
SWRACH60 ICM   RF,15,0(R4)                                                      
         BZ    SWRACH64                                                         
         C     RF,=F'999999'                                                    
         BH    SWRACH62                                                         
         EDIT  (RF),(7,(R8)),2                                                  
         B     SWRACH64                                                         
*                                                                               
SWRACH62 M     RE,=F'2'                                                         
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         EDIT  (RF),(7,(R8)),0                                                  
*                                                                               
SWRACH64 LA    R8,132(R8)                                                       
         LA    R4,8(R4)                                                         
         BCT   R1,SWRACH60                                                      
*                                                                               
SWRACH70 DS    0H                                                               
         SR    R1,R1               SET ACHIEVED DEMOS                           
         IC    R1,NODEM                                                         
         XC    MEDVAACH(56),MEDVAACH                                            
         LA    RE,MEDVPACH                                                      
         LA    RF,SWBD1                                                         
*                                                                               
SWRACH72 MVC   0(4,RE),0(RF)                                                    
         LA    RE,4(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R1,SWRACH72                                                      
         EJECT                                                                  
*===============================================================                
* CALCULATE GROSS AND PERCENTAGE CHANGES                                        
*===============================================================                
                                                                                
SWRDIF6  MVI   SUPPRESS,1          RESET SUPPRESS SWITCH                        
         LA    R4,MEDVPEST                                                      
         LA    R5,MEDVPACH                                                      
         SR    R1,R1                                                            
         IC    R1,NODEM                                                         
         LA    R8,P1+117                                                        
         MVI   DEMINDX,0                                                        
*                                                                               
SWRDIF8  L     RE,0(R4)            ESTIMATED                                    
         L     RF,0(R5)            ACTUAL                                       
         SR    RF,RE                                                            
         ST    RF,DMCB             SAVE RAW CHANGE                              
*                                                                               
         MVC   DMCB+4(4),=X'FFFF0000'  SET TO PRINT 'HIGH'                      
         ICM   R0,15,0(R4)             ACT/EST - 100 = PCT CHG                  
         BZ    SWRDIF10                                                         
*                                                                               
         L     RF,0(R5)          ACTUAL                                         
         M     RE,=F'200'                                                       
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         AHI   RF,-100                                                          
         ST    RF,DMCB+4                                                        
         LTR   RE,RF                                                            
         BP    SWRDIF10                                                         
         LPR   RE,RE                                                            
         OC    VARIANCE,VARIANCE                                                
         BZ    SWRDIF10                                                         
         C     RE,VARIANCE                                                      
         BL    SWRDIF10                                                         
         MVI   SUPPRESS,0          RESET SUPPRESS SWITCH                        
         MVI   13(R8),C'X'                                                      
*                                                                               
SWRDIF10 OC    DMCB(4),DMCB                                                     
         BZ    SWRDIF22                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DEMINDX                                                       
         MHI   RE,7                                                             
         LA    RE,DNAMES(RE)                                                    
         CLI   0(RE),C'R'                                                       
         BE    *+12                                                             
         CLI   0(RE),C'E'                                                       
         BNE   SWRDIF12                                                         
         TM    RQOPTS,RQOPTS_2DEC   TEST 2-DEC RTGS                             
         BZ    SWRDIF18                                                         
         B     SWRDIF14                                                         
*                                                                               
SWRDIF12 TM    RQOPTS,RQOPTS_2DECIMP TEST 2-DEC IMPS                            
         BZ    SWRDIF18                                                         
*                                                                               
SWRDIF14 CLI   MODE,PROCBUY        IF BUY DETAIL, PRINT 2 DEC                   
         BNH   SWRDIF16            THEN PRINT 2 DEC                             
*                                                                               
         L     RF,DMCB             ELSE ADJUST RAW VAR TO 1 DEC                 
         M     RE,=F'2'                                                         
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,DMCB                                                          
         B     SWRDIF18                                                         
*                                                                               
SWRDIF16 L     R0,DMCB                                                          
         EDIT  (R0),(7,(R8)),2,MINUS=YES                                        
         B     SWRDIF20                                                         
*                                                                               
SWRDIF18 L     R0,DMCB                                                          
* NOTE - EDIT MACRO DOES NOT WORK FOR 1 DECIMAL,MINUS=Y                         
******   EDIT  (R0),(7,(R8)),1,MINUS=YES                                        
         CVD   R0,DUB                                                           
         MVC   WORK(17),=X'40404040202020202020202020214B2060'                  
         ED    WORK(17),DUB+2                                                   
         MVC   0(7,R8),WORK+17-(7)                                              
*                                                                               
SWRDIF20 L     R0,DMCB+4                                                        
         EDIT  (R0),(4,8(R8)),MINUS=YES                                         
         C     R0,=X'FFFF0000'                                                  
         BNE   *+10                                                             
         MVC   7(5,R8),=C' HIGH'                                                
*                                                                               
SWRDIF22 LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         LA    R8,132(R8)                                                       
         SR    RE,RE                                                            
         IC    RE,DEMINDX                                                       
         AHI   RE,1                                                             
         STC   RE,DEMINDX                                                       
         BCT   R1,SWRDIF8                                                       
*                                                                               
SWPRINTX CLI   MEDSPILL,C'Y'       IS THIS SPILL                                
         BNE   *+8                                                              
         CLI   SPOTPROF+5,1        SUPPRESS SPILL DETAILS                       
         BNE   *+8                                                              
         MVI   SUPPRESS,2                                                       
SWRDIFX  XIT1                                                                   
         EJECT                                                                  
CALCCPP  NTR1                                                                   
         L     RE,SWBDL                                                         
         CVD   RE,DUB                                                           
         ZAP   PDOLS,DUB                                                        
         MP    PDOLS,=P'20'        PACKED DOLLARS X 10 X 2                      
*                                                                               
         LLC   R1,NODEM                                                         
         LA    R4,SWBD1                                                         
         LA    R5,DEMTYPES                                                      
*                                                                               
         CLI   SPOTPROF+4,C'Y'     EQUIVALENCE DETAIL LINES                     
         BE    CPP1                                                             
         CLI   SWBSLN,X'FF'                                                     
         BE    CPP1                                                             
*                                                                               
         LHI   RE,14                                                            
         LA    RF,SWBD1                                                         
         MVC   4(4,RF),0(RF)                                                    
         LA    RF,8(RF)                                                         
         BCT   RE,*-10                                                          
*                                                                               
CPP1     ICM   RF,15,4(R4)         GET POINTS                                   
         BZ    CPP10                                                            
         CVD   RF,DUB                                                           
*                                                                               
         ZAP   P16,PDOLS                                                        
         CLI   0(R5),C'R'          TEST RATING                                  
         BNE   CPP02                                                            
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMAL RATINGS                       
         JO    CPP02X                                                           
         J     CPP04                                                            
*                                                                               
CPP02    TM    RQOPTS,RQOPTS_2DECIMP   TEST 2-DEC IMPS                          
         JZ    CPP04                                                            
*                                                                               
CPP02X   MP    P16,=P'10'          ADJUST FOR EXTRA DECIMAL                     
*                                                                               
CPP04    DP    P16,DUB                                                          
         ZAP   DUB,P16(8)                                                       
         CP    DUB,=P'2147483647'  X'7FFFFFFF' LARGEST # IN A REG               
         BH    CPP06                                                            
         CP    DUB,=P'-2147483648' X'80000000' LARGEST -# IN A REG              
         BNL   CPP08                                                            
*                                                                               
CPP06    XR    RF,RF                                                            
         B     CPP10                                                            
*                                                                               
CPP08    CVB   RF,DUB                                                           
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
CPP10    ST    RF,4(R4)                                                         
         LA    R4,8(R4)                                                         
         LA    R5,1(R5)            NEXT DEMO TYPE                               
         BCT   R1,CPP1                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REPBUY   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R2                                                      
         USING SP08WK,R7                                                        
         L     R5,BUYADEMO         R5 --> BUY DEMOS                             
         L     R6,MEDADEMO         R6 --> NEW DEMOS                             
         LTR   R6,R6                                                            
         BZ    REPBUYX                                                          
         L     R6,4(R6)                                                         
         LTR   R6,R6                                                            
         BZ    REPBUYX                                                          
         CLI   QOPT3,C'Y'          REPLACE OVERRIDES                            
         BE    REPBUY6              YES                                         
*                                                                               
REPBUYN  SR    R4,R4                                                            
         IC    R4,1(R5)                                                         
         AR    R4,R5                                                            
         LA    R6,24(R6)                                                        
REPBUY1N L     R5,BUYADEMO                                                      
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+8                                                              
         L     R5,BUYSDEMO                                                      
         LA    R5,24(R5)                                                        
REPBUY2N TM    4(R5),X'80'                                                      
         BZ    REPBUY3N                                                         
         CLC   0(3,R5),0(R6)                                                    
         BE    REPBUY5N                                                         
REPBUY3N LA    R5,8(R5)                                                         
         CR    R5,R4                                                            
         BL    REPBUY2N                                                         
REPBUY4N LA    R6,8(R6)                                                         
         CLI   1(R6),0                                                          
         BNE   REPBUY1N                                                         
         B     REPBUY6                                                          
REPBUY5N MVC   3(5,R6),3(R5)       INSERT OVERRIDE VALUES                       
         B     REPBUY4N                                                         
*                                                                               
REPBUY6  L     R4,ADBUY                                                         
         L     R5,BUYADEMO                                                      
         L     R6,MEDADEMO                                                      
         LTR   R6,R6                                                            
         BZ    REPBUYX                                                          
         L     R6,4(R6)                                                         
         LTR   R6,R6                                                            
         BZ    REPBUYX                                                          
         MVI   0(R6),2                                                          
         CLI   MEDSPILL,C'Y'                                                    
         BNE   REPBUY7                                                          
         SPACE 2                                                                
         L     R5,BUYSDEMO         SET UP SPILL ELEMENT                         
         MVI   0(R6),3                                                          
         MVC   4(14,R6),4(R5)                                                   
         XC    DEMSPEL,DEMSPEL     CONDENSE SPILL ELEMENT                       
         MVC   DEMSPEL(32),0(R6)   HEADER AND ONE DEMO                          
         MVI   DEMSPEL+1,24                                                     
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-24                                                           
         SRL   RE,3                DIVIDE BY EIGHT                              
         LA    R1,24(R6)           POINT TO OLD DEMOS                           
         LA    RF,DEMSPEL+24       POINT TO NEW DEMOS                           
         LHI   R0,24               ORIG. LENGTH                                 
*                                                                               
REPBUY6A CLI   1(R1),C'E'          ALWAYS SEED RATINGS                          
         BE    *+8                                                              
         CLI   1(R1),C'R'                                                       
         BE    *+14                                                             
         OC    4(4,R1),4(R1)       BYPASS DEMO IF NO VALUE                      
         BZ    REPBUY6B                                                         
         MVC   0(8,RF),0(R1)       SET DEMO CODE AND VALUE                      
         LA    RF,8(RF)                                                         
         AHI   R0,8                BUMP EL LENGTH                               
REPBUY6B LA    R1,8(R1)            GET NEXT DEMO                                
         BCT   RE,REPBUY6A                                                      
*                                                                               
         STC   R0,DEMSPEL+1        SET NEW LENGTH                               
         CLI   DEMSPEL+1,32                                                     
         BNL   *+8                                                              
         MVI   DEMSPEL+1,32                                                     
         LA    R6,DEMSPEL          POINT TO NEW ELEMENT                         
         EJECT                                                                  
REPBUY7  DS    0H                                                               
         GOTO1 RECUP,DMCB,(R4),(R5)                                             
         GOTO1 RECUP,DMCB,(R4),(R6),(R5)                                        
         MVC   MEDVPACH(56),MEDVPEST    SAVE OLD DEMOS                          
         MVC   MEDVAACH(56),MEDVAEST                                            
         GOTO1 MEDGETBY,DMCB,(RA),2     GET NEW BUYREC DEMOS                    
         MVC   AREC,ADBUY                                                       
         XC    DMCB(24),DMCB                                                    
         GOTO1 PUT                                                              
REPBUYX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* PRINT DEMO SOURCE CAPTION FOR CANADIAN AGENCIES                               
* FROM DLUELEM OR 03 DEMO ELEMENT FOR SPILL                                     
*================================================================               
                                                                                
GETDMCAP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   P3+30(7),=C'SRC=???'                                             
         L     R6,ADBUY                                                         
         AHI   R6,BDELEM-BUYREC                                                 
         SR    R0,R0                                                            
*                                                                               
GETDM2   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GETDMX                                                           
*                                                                               
         CLI   MEDSPILL,C'Y'                                                    
         BE    GETDM10                                                          
         CLI   0(R6),X'24'                                                      
         BNE   GETDM2                                                           
         USING DLUELEM,R6                                                       
*                                                                               
         CLI   DLUBAMKT,C'A'                                                    
         BL    GETDMX                                                           
*                                                                               
         MVC   P3+34(3),=C'BBM'     SET UP ELEMENT PRINTING                     
         TM    DLUBFLGS,X'01'                                                   
         BO    GETDM4                                                           
         MVC   P3+34(3),=C'NSI'                                                 
         TM    DLUBFLGS,X'02'                                                   
         BO    GETDM4                                                           
         MVC   P3+34(3),=C'...'                                                 
*                                                                               
GETDM4   MVI   P3+37,C'/'                                                       
         MVC   P3+38(3),DLUBAMKT                                                
         MVI   P3+41,C'/'                                                       
         MVC   P3+42(4),DLUBSTOV                                                
         OC    P3+42(4),SPACES                                                  
         B     GETDMX                                                           
         DROP  R6                                                               
*                                                                               
GETDM10  CLI   0(R6),3                                                          
         BNE   GETDM2                                                           
         USING NDELEM,R6                                                        
*                                                                               
         CLC   KEY+4(2),NDAGYMKT   MATCH DIR KEY TO MARKET                      
         BNE   GETDM2                                                           
*                                                                               
         CLI   NDMKTALF,C'A'                                                    
         BL    GETDMX                                                           
*                                                                               
         MVC   P3+34(3),=C'BBM'     SET UP ELEMENT PRINTING                     
         CLI   NDRTGSVC,C'1'                                                    
         BE    GETDM12                                                          
         MVC   P3+34(3),=C'NSI'                                                 
         CLI   NDRTGSVC,C'0'                                                    
         BE    GETDM12                                                          
         MVC   P3+34(3),=C'...'                                                 
*                                                                               
GETDM12  MVI   P3+37,C'/'                                                       
         MVC   P3+38(3),NDMKTALF                                                
         MVI   P3+41,C'/'                                                       
         MVC   P3+42(4),NDSTA                                                   
         OC    P3+42(4),SPACES                                                  
*                                                                               
GETDMX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SUBROUTINE RETURNS WITH CC EQ IF DEMO AT 0(R1) IS A RATING                    
* R1 POINTS TO 3-BYTE DEMO CODE ON ENTRY                                        
*==============================================================                 
                                                                                
ISRATING NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R1),0             TEST NORMALIZED NON-T DEMO                   
         JE    ISRTG2              NO                                           
         CLI   0(R1),C'R'                                                       
         JE    ISRTGYES                                                         
         CLI   0(R1),C'E'                                                       
         JE    ISRTGYES                                                         
         J     ISRTGNO                                                          
*                                                                               
ISRTG2   CLI   2(R1),0             NON-TRAD DEMO CATEGORY?                      
         JE    ISRTG4              YES                                          
*                                                                               
         CLI   1(R1),C'R'          SEE IF NORMAL DEMO IS A RATING               
         JE    ISRTGYES                                                         
         CLI   1(R1),C'E'                                                       
         JE    ISRTGYES                                                         
         J     ISRTGNO                                                          
*                                                                               
ISRTG4   LLC   RF,1(R1)            YES, GET THE NON-TRAD INDEX                  
         BCTR  RF,0                                                             
         SLL   RF,3                8 BYTE/DEMO                                  
         A     RF,VNONTNMS                                                      
         CLI   0(RF),C'R'          SEE IF NON-TRAD CTGY IS RATING               
         JE    ISRTGYES              OR EXTENDED RATING                         
         CLI   0(RF),C'E'                                                       
         JNE   ISRTGNO                                                          
*                                                                               
ISRTGYES MVI   RATING,C'Y'                                                      
         J     ISRTGX                                                           
*                                                                               
ISRTGNO  MVI   RATING,C'N'                                                      
*                                                                               
ISRTGX   CLI   RATING,C'Y'                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*SP08WK*'                                                    
SP08WK   DS    0D                                                               
SVSPECS  DS    F                   SAVE SPEC ADDRESS                            
SVMDTAB  DS    F                   SAVE MEDTABLE ADDRESS                        
SUBPROG  DS    0CL8                                                             
         DC    C'SP'                                                            
SUBPROG1 DC    C'12'                                                            
SUBPROG2 DC    C'01'                                                            
         DC    C'  '                                                            
SPCONLY  DS    CL1                 SPECIALS ONLY SWITCH                         
ACTSW    DS    CL1                                                              
WEIGHT   DC    F'0'                                                             
MCOUNT   DC    F'0'                                                             
APL      DC    F'0'                                                             
SVPH01   DS    F                                                                
SVPH02   DS    F                                                                
SVPH04   DC    F'0'                                                             
REPCALOV DC    F'0'                                                             
SVRCSUB  DC    F'0'                                                             
MSBFHOOK DC    F'0'                                                             
SVHDHOOK DC    F'0'                                                             
BUYADEMO DS    F                                                                
BUYSDEMO DS    F                                                                
VFOOT    DS    F                                                                
P16      DS    PL16                                                             
DEMTYPES DS    XL20                                                             
PDOLS    DS    PL8                                                              
NODEM    DS    X'00'                                                            
OVRFLAG  DS    CL14                                                             
OVRFLAGR DS    CL14                                                             
VARIANCE DC    F'15'                                                            
SUPPRESS DS    X'00'                                                            
FIRST    DC    X'01'                                                            
FIRSTRB  DC    C'Y'                                                             
MSACTIVE DS    CL1                                                              
MSHDHOOK DS    F                                                                
CURPGPTR DS    F                                                                
VSUBPARA DC    F'0'                                                             
ASPD801  DC    F'0'                                                             
LSPD801  DC    F'0'                                                             
ASPD804  DC    F'0'                                                             
LSPD804  DC    F'0'                                                             
GETBUF   DC    F'0'                =V(GETBUFC)                                  
VREPORT  DS    A                                                                
CURPH01  DS    CL12                                                             
CURPH02  DS    CL12                                                             
CURPH04  DS    CL12                                                             
MSRCSUB  DS    C                                                                
BUFCDE   DS    C                   BUFFALO KEY                                  
BUFHI    DS    C                   BUFFALO HIGH SWITCH                          
BUFLV    DS    C                   BUFFALO LEVEL SWITCH                         
D8SVD0   DS    CL2                                                              
NUMCOM   DS    X                                                                
LVCNTRL  DC    A(1,2,3,4,5)        LEVEL CONTROL                                
SUPDPT   DC    AL1(1,3,5,7,0)                                                   
PRVBKEY  DS    CL20                PREVIOUS BUFFALO KEY                         
SVPROF   DS    CL16                SWEEP PROFILE SAVE AREA                      
SVSPPROF DS    CL16                SPOT PROFILE SAVE AREA                       
MSSPPROF DS    CL16                                                             
MSPROF   DS    CL16                MEDIA SUMMARY PROFILE AREA                   
D0PROF   DS    CL16                D0 PROFILE                                   
MSOPT    DS    CL7                                                              
SVOPT    DS    CL7                                                              
RATING   DS    CL1                                                              
MSSUPMKT DS    C                                                                
SVSUPMKT DS    C                                                                
SVMAXLIN DS    C                                                                
EXTDMSV  DS    CL1                                                              
AFRSTSV  DS    F                                                                
SVRB     DS    C                                                                
RBSVPRD  DS    C                                                                
RBMEDWK  DS    F                                                                
RBLCHNK  DS    F                                                                
ALASTSV  DS    F                                                                
PSLADDR  DS    A                                                                
SVMID    DS    CL60                                                             
DNAMES   DS    CL98                                                             
DEMSPEL  DS    CL220                                                            
PSLIST   DS    CL150                                                            
         DS    0F                                                               
MYBUFIO  DS    CL200                                                            
MYBUFIO2 DS    CL200                                                            
SVBDATA  DS    CL112                                                            
SVQPRD   DS    CL3                                                              
SPLPRINT DS    C                   PRINT ORIGINATING SWITCH                     
SPBUFSTA DS    CL150               SAVE AREA FOR ORIG. STATIONS                 
SPBUFMKT DS    CL240               SAVE AREA FOR ORIG. MARKETS                  
SPBUFMK2 DS    CL240                                                            
SPSTPL   DS    CL132                                                            
SPSTPL2  DS    CL132                                                            
SVBDATA1 DS    CL112                                                            
SVESTPNT DS    CL56                                                             
STRDTE   DS    CL2                                                              
ENDDTE   DS    CL2                                                              
OVRCNT   DC    X'00'                                                            
COVRFRST DC    X'00'                                                            
COVRHLD  DS    396C                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'DBLKAREA'                                                    
DBLKAREA DS    CL256                                                            
         DS    0D                                                               
COMAREA  DS    CL400                                                            
         DS    0D                                                               
         DC    CL8'*SPD801*'                                                    
SSPD801  DS    3000C                                                            
         DS    0D                                                               
         DC    CL8'*SPD804*'                                                    
SSPD804  DS    8000C                                                            
         DS    0D                                                               
         DS    CL8'SUBPAREA'                                                    
SUBPAREA DS    0D                                                               
         DS    90000C                                                           
         EJECT                                                                  
SUMDSECT DSECT                                                                  
SWBKEY   DS    0CL12               SWEEP REPORT BUFFER                          
SWBRCDE  DS    CL1                 RECORD CODE                                  
SWBDPC1  DS    CL1                 DAYPART NUMBER                               
SWBDPN1  DS    CL3                 DAYPART CODE                                 
SWBDPC2  DS    CL1                 SUB-DAYPART NUMBER                           
SWBDPN2  DS    CL3                 SUB-DAYPART CODE                             
SWBSLN   DS    CL1                 SPOT LENGT                                   
SWBDTYP  DS    C                   DATA TYPE                                    
SWBRPT   DS    CL1                 REPORT NUMBER                                
SWBDATA  DS    0C                                                               
SWBDL    DS    F                   DOLLARS                                      
SWBDLE   DS    F                   EQUIVALENCE DOLLARS                          
SWBSPT   DS    F                   SPOTS                                        
SWBD1    DS    F                   DEMO                                         
SWBD1E   DS    F                   EQUIVALENCED DEMO 1                          
SWBD2    DS    F                                                                
SWBD2E   DS    F                                                                
SWBD3    DS    F                                                                
SWBD3E   DS    F                                                                
SWBD4    DS    F                                                                
SWBD4E   DS    F                                                                
SWBD5    DS    F                                                                
SWBD5E   DS    F                                                                
SWBD6    DS    F                                                                
SWBD6E   DS    F                                                                
SWBD7    DS    F                                                                
SWBD7E   DS    F                                                                
SWBD8    DS    F                                                                
SWBD8E   DS    F                                                                
SWBD9    DS    F                                                                
SWBD9E   DS    F                                                                
SWBD10   DS    F                                                                
SWBD10E  DS    F                                                                
SWBD11   DS    F                                                                
SWBD11E  DS    F                                                                
SWBD12   DS    F                                                                
SWBD12E  DS    F                                                                
SWBD13   DS    F                                                                
SWBD13E  DS    F                                                                
SWBD14   DS    F                                                                
SWBD14E  DS    F                                                                
SWBADJ   DS    14F                                                              
         LTORG                                                                  
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   Q2USER                                                           
Q2OPT1   DS    C       21          C'S'= REPLACE SPILL ONLY                     
Q2OPT2   DS    C       22          C'N'= DO NOT SHOW COSTS                      
Q2OPT3   DS    C       23          C'W'= USE WTP DATA                           
Q2OPT4   DS    C       24          C'Y'= SUPPRESS POSTBUY DEMOS                 
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPMEDBDESD                                                     
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072SPREPD802 11/19/19'                                      
         END                                                                    
