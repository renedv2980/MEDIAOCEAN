*          DATA SET RECNT01    AT LEVEL 007 AS OF 03/23/17                      
*PHASE T80201A,*                                                                
         TITLE 'T80201 - RECNT01 - CONTROL CONTRACT ACTION'                     
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT01 --- CONTRACT PROGRAM - CONTROL CONTRACT ACTION       *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* SEE RECNTHIST FOR PAST HISTORY                                      *         
*                                                                     *         
* 10MAR17 KUI SPEC-11082 REDIRECT TO FORMAT O IF CBSWO                *         
*             STATION ADDITIONAL OPTION 8: USE WIDE ORBIT FORMAT      *         
* 15MAR13 KWA ADD SCHA ACTION TO CONTROL SALESPERSON CHANGES          *         
* 13MAR13 KUI SKIP STATION CHECK IF GLOBBER CALLER IS STK             *         
* 26JAN11 KUI FIX EC BUG FOR UNIVISION                                *         
* 20JAN11 KUI REPPAK ROLLBACK. STAPRO HARD CODE FOR UNIVISION         *         
* 13DEC10 KUI ALLOW MON$ EVEN IF INVOICE BUCKETS EXIST                *         
* 19OCT10 KUI DO NOT ALLOW BUYS IF XML AND NOT OPENED                 *         
* 30JUL10 KUI MON$ SCRIPT UPLOAD SUPPORT                              *         
* 28JAN10 KUI REPPAK ROLLBACK. STAPRO HARD CODE FOR FOX STATION SALES *         
* 09SEP09 KUI REPPAK ROLLBACK. STAPRO HARD CODE FOR EAGLE             *         
* 08JUL09 KUI ESPERANTO SUPPORT                                       *         
* 10JUL08 KUI ALLOW BUY ACTION FOR XML ORDERS                         *         
* 09AUG07 BU  PROPOSER CUTOFF: PERMIT ADD'L BUYS IF FIRST BUY ADDED   *         
* 07AUG07 BU  NO BUYS FOR PROPOSER ORDERS AFTER CUTOFF DATE           *         
* 30JUL07 BU  GLOBBER CALL FROM SWP: SKIP PAR CHECK                   *         
* 30MAY07 SKU CLEAR KEY IN ROUTINE CHKSECU                            *         
* 19APR06 BU  SOFTEN 'CBS HOME MARKET' EC TESTING                     *         
* 05MAR06 BU  TRAP MOD ELEMENT DISPLAY IN EXPANDED BUY DISPLAY        *         
* 17MAY05 BU  WIDE ORBIT / COLUMBINE WORKSHEET EC. FORMAT 'E'         *         
* 19APR05 HQ  DISABLE KCHG ACTION                                     *         
* 28MAR05 BU  VCI/W.O. EC WKSHEET 'U' FORMAT                          *         
* 02FEB05 BU  W.O. EC:  ALTERNATE WORKSHEET                           *         
* 19AUG04 SKU HARD CODE EC FUNCTIONALITY FOR KPIXL                    *         
* 13AUG04 HQ  PREVENT STATION FROM TOUCHING CONTRACT FOR OTHER STATION&         
* 13JUL04 BU  CBS/VSS EC: CROSS TO NEW MODULES                        *         
* 10JUN04 BU  EC FROM REP SIDE:  FORCE TRAFFIC CODE                   *         
* 07MAY04 BU  PERMIT EC FROM THE REP SIDE, BASED ON PROFILE           *         
* 09MAR04 BU  BUY ACTIONS BCD/BCC FOR BUYCODE MAINTENANCE             *         
* 10FEB04 BU  TRAFFIC CODE 'Z' FOR VSS                                *         
* 06JUN03 BU  TRAFFIC CODE 'I' FOR OSI                                *         
* 08OCT02 BU  TRAFFIC CODE 'L' FOR MARKETRON                          *         
* 14AUG02 SKU SUPPRESS RNA FOR COLUMBINE EC CHANGES                   *         
* 09JUL02 BU  TRAFFIC CODE 'T' AND 'U'  FOR VCI BDE                   *         
* 29MAY02 SKU SUPPORT FOR REPLACEMENT OFFERS FOR NA SPOTS AND         *         
*             LINE CORRECTION                                         *         
* MAR1402 RHV SUPPRESS AGY COPY FOR CFX                               *         
* FEB0402 BU  PERMIT E3 TEST FOR SJR/STA=WORB                         *         
* FEB0102 BU  REROUTE E3 TEST TO MODULE 59                            *         
* 04DEC01 BU  ADD PAYD/PAYC ACTION CODES                              *         
* 03DEC01 BU  ADD E3 AS VALID EC CODE (WIDE ORBIT TESTS)              *         
* 12OCT01 BU  TURN ON 'BUY CODE' FIELD ON PROFILE                     *         
* 24AUG01 SKU IGNORE STATION OPTION 8 (SPL) FOR TAKEOVER CONTRACTS    *         
* 22AUG01 SKU PREVENT ACTION MON FOR TRADE CONTRACTS                  *         
* 07AUG01 RHV BUYTMP SUPPORT                                          *         
* 24JUL01 BU  NEW SCREENS: ADD BUY CODE                               *         
* 17JUL01 BU  ESG EC CHANGES:  PROFILE FOR T8027E                     *         
* 12JUL01 SKU REMOVED SCREEN REFERENCES FROM ISTKOV ROUTINE           *         
* 05JUL01 BU  TD/TC FOR JDS/2000                                      *         
* 17MAY01 RHV 'ADD TR' ACTION REFRESH BUG                             *         
* 28FEB01 BU  RVAN'S LAST CHANGES                                     *         
* 27JAN01 BU  TD/TC MAJOR SCREEN HANDLING                             *         
* 29NOV00 RHV REVISED BUY SCRN PATTERN/NOTATION                       *         
* 17NOV00 RHV TRAFFIC TYPE S EC SUPPORT                               *         
* 09AUG00 RHV SPORTS SPECTRUM SCREEN HANDLING                         *         
* 28JUN00 BU  REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG*          *         
* 15JUN00 RHV PAR APPLICATION SECURITY                                *         
* 24MAY00 BU  BUYT ACTION REMOVED.  TRADE FLAG WILL BE ON $$.         *         
* 19MAY00 BU  ADD BUYT ACTION (BUY TRADE)                             *         
* 19MAY00 SKU CANNOT EC/E2 FORECAST CONTRACTS                         *         
* 29DEC99 SKU NEW MAKEGOOD OFFER SCROLLER                             *         
*         BU  PROFILE TO SELECT NEW EC CHANGES MODULES                *         
* 04NOV99 SKU SPECIAL FOR SELTEL                                      *         
* 23SEP99 RHV ACTION RSND ENABLE STA PROF#16 XTRA FAX WORKSHEET       *         
* 07JUN99 SKU FIX TAKEOVER BUG                                        *         
* 03JUN99 BU  PERMIT PFM ACTION FOR FORECAST ORDERS                   *         
* 31MAR99 JRD IF PROF 41 ON WHEN EC SET SONNET BOXID TO 'ECD'         *         
* 22FEB99 RHV NBC HOME MARKET                                         *         
* 09DEC98 RHV HOME MKT PROF#38 STA/REP INTERCHANGEABLE                *         
* 07DEC98 RHV STAREC PROF #16 FAX XTRA WORKSHEET TO STATION           *         
* 18NOV98 JRD SINGLE WORKSHEET FOR HOME MARKET(TREAT AS RSND)         *         
* 13NOV98 BU  EC FOR HOME MARKET FROM REP SIDE OPTIONAL ON PROFILE    *         
* 28SEP98 AST 'SPCP' BUYACT TO COPY SPL DATA FROM ONE K TO ANOTHER    *         
* 13JUL98 AST 'XRER' BUYACT TO EXCLUDE CONTRACT FROM RER TAPES        *         
* 03JUN98 RHV AUTOGEN CHANGE                                          *         
* 30MAR98 RHV COVERSHEET INTERFACE                                    *         
* 13JAN98 RHV 'MOVED' ACTION                                          *         
* 19NOV97 BU  CF* ACTION FOR TAKEOVER PAPERWORK                       *         
* 04NOV97 RHV BYPASS PROPOSAL CHK FOR KTV REPS ON MOVE ACTION         *         
* 15SEP97 RHV SUPPORT MCI MODULE REWRITE                              *         
*         SKU ALLOW REVISION CONFIRMATION                             *         
* 16JUL97 SKU MAKEGOOD GLOBBER BUG FIX                                *         
* 27MAR97 SKU SUPPORT VARIOUS/DARE ORDERS                             *         
* 27MAR97 RHV RESTRICT MOVE TO NON-COMBO ORDERS & ALLOW KRG REPS      *         
* 19MAR97 RHV SUPPORT CFC (CONFIRMATION COMMENT) ACTION               *         
* 17MAR97 RHV REVISE CF LOGIC REMOVE OLD CONF/CNF ACTIONS             *         
* 15MAR97 RHV REVISE SCREEN OVERLAY CALLING                           *         
* 04MAR97 RHV SUPPORT FULL SIZE ORD SCREEN                            *         
* 28FEB97 DBU CD ACT FOR STA / CLR ACT FOR REP                        *         
* 19FEB97 DBU PACE ACTION                                             *         
* 09JAN97 SKU MAKEGOOD FOR MAKEGOOD                                   *         
* 11DEC96 SKU REMOVE RETURN SESSION GLOBBER BRANCH AROUND             *         
* 05DEC96 RHV NEW CONFIRMATION PAPERWORK GENERATION LOGIC             *         
* 03OCT96 SKU SUPPORT LOW POWER STATION                               *         
* 19AUG96 RHV SUPPORT & VALIDATION FOR MOVE                           *         
* 31MAY96 SKU ADD HIATUS BUY ACTION                                   *         
* 29APR96 RHV FIX BUG PARSING BUYACT FROM CONACT FIELD                *         
* 01PAR96 SKU ALLOW CFX FOR COMBOS                                    *         
* 09MAR96 SKU STEREO SUPPORT                                          *         
* 07MAR96 RHV AUTO FILLIN OF ORD TRAFFIC # FROM CONACT FIELD          *         
* 07MAR96 SKU NEW PENDING SUPPORT                                     *         
* 29FEB96 SKU PROFILE 25 BUG FIX                                      *         
*         RHV ADD 'CF' ACTION TO EMULATE CONF&CNF ACTIONS             *         
* 22FEB96 SKU SFM BUYCOPY SWAP                                        *         
* 15FEB96 SKU PROFILE 25 TO SEND WORKSHEET INSTEAD OF CONF FOR TYPE D *         
* 26JAN96 SKU AUTO-APPLY FROM DARE MAKEGOODS                          *         
* 02JAN95 SKU AUTO HEADER GENERATION. TOMBSTONE MOVED TO RECNTHIST    *         
* 13DEC95 SKU 2K CONTRACT SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80201   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80201,R9,R4,RR=R5                                             
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         ST    R5,TWARELO1         SAVE RELO ADDR FOR RECNT01 ONLY!!            
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
CONA0000 EQU   *                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,55               ERROR CODE                                   
         GOTO1 =A(CHKSECU),DMCB,(RC),RR=TWARELO1                                
         BNE   ERROR               NOT RIGHT TO ACCESS THIS CONTRACT            
*                                                                               
         TM    TWASTREO,X'40'      DSM STEREO IN PROGRESS?                      
         BO    DSMST                                                            
*                                                                               
*  CHECK FOR PROF #37 SEND WARNING FLAG                                         
         CLC   =C'SEND',CONCACT    ACTION SEND?                                 
         BE    *+8                 YES - SKIP FLAG                              
         NI    TWASNDFL,X'FF'-X'80' NO -RESET FLAG                              
*                                                                               
*              SCAN THE ACTION FIELD                                            
*                                                                               
CONA0005 DS    0H                                                               
         CLC   LASTCACT,=C'MCIR'   AFTER MCIR HAS BEEN INITIATED,               
         BNE   *+14                ALWAYS GO TO IT DIRECTLY                     
         MVC   CONACT,LASTCACT                                                  
         B     CONA0160            USE TO SET R5 TO TABLE                       
         MVI   BYTE3,0             INITIALIZE FIRST TIME SWITCH                 
         CLC   LASTCACT,=C'EPLR'   AND TREAT EPLR JUST THE SAME                 
         BNE   *+18                                                             
         MVC   CONACT,LASTCACT                                                  
         MVI   BYTE3,C'Y'                                                       
         B     CONA0160            USE TO SET R5 TO TABLE                       
*                                                                               
         TM    CONCACTH+4,X'20'                                                 
         BZ    CONA0010                                                         
         MVC   CONACT,LASTCACT                                                  
         CLC   CONACT,=C'ADDR'                                                  
         BNE   CONA0090                                                         
         CLI   CONBACTH+5,0                                                     
         BE    ADDR10                                                           
         CLC   =C'BOP',CONBACT                                                  
         BE    ADDR10                                                           
         MVC   CONACT,=C'DIS '     IF THEY CHANGE BUY ACTION AFTER              
         MVC   LASTCACT,=C'DIS '   ADDR GET NEW SCREEN                          
         B     CONA0090                                                         
*                                                                               
CONA0010 DS    0H                                                               
         LA    R2,CONCACTH                                                      
         CLI   5(R2),0                                                          
         BE    CONA0090                                                         
*                                                                               
         GOTO1 VANY                                                             
         MVC   CONACT,MYSPACES                                                  
         XC    TWAXCON,TWAXCON                                                  
         MVI   TWATRAFL,C'N'            INIT TRAFFIC# IN CONACT FLAG            
*                                                                               
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         GOTO1 SCANNER,DMCB,(R2),(3,WORK2),0                                    
         LA    R3,2                INVALID INPUT FLD                            
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         MVC   BYTE,DMCB+4         SAVE OFF NUMBER OF SCANNER LINES             
         MVC   CONACT,WORK2+12                                                  
*                                                                               
         CLI   BYTE,1            FIX SCANNER BUG                                
         BE    CONA0015            ONE LINE, OK                                 
         CLI   WORK2+32,0          ANYTHING ON 2ND LINE                         
         BNE   CONA0015            YES - OK                                     
         MVI   BYTE,1            NO- THEN WE ONLY REALLY HAVE 1 LINE            
*                                                                               
CONA0015 DS    0H                                                               
         CLC   =C'DIS',CONACT    INITIATING FRESH DSM CALL                      
         BNE   CONA0017                                                         
         CLI   TWASCRN,X'FD'                                                    
         BE    *+12                                                             
         CLI   TWASCRN,X'F9'                                                    
         BNE   CONA0017                                                         
         CLC   =C'NEXT',CONBNUM                                                 
         BNE   CONA0017                                                         
         XC    CONBNUM,CONBNUM                                                  
         MVI   CONBNUMH+5,0                                                     
         OI    CONBNUMH+6,X'80'                                                 
*                                                                               
CONA0017 DS    0H                                                               
*                                                                               
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET?                                 
         BZ    CONA0019            NO                                           
*                                                                               
         OC    CONACT,MYSPACES                                                  
*                                                                               
         LA    RF,*                CONTRACT ACTION TABLE                        
         A     RF,=A(CONACTS-(*-4))                                             
         B     *+8                                                              
*                                                                               
         LA    RF,L'CONACTS(RF)                                                 
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    CONA0019            GET OUT, WORRY ABOUT THIS LATER              
         CLC   CONACT,0(RF)        CON ACTION FOUND IN TABLE?                   
         BNE   *-18                NO  - KEEP LOOKING                           
*                                                                               
         TM    15(RF),X'08'        FORCE STATION LIMIT ACCESS CODE?             
         BZ    CONA0019            NO                                           
*                                                                               
         MVC   TWAACCS,=C'$   '    NOW WE'RE A STATION                          
*                                                                               
CONA0019 DS    0H                                                               
*  FOR ACE SEND/CONFIRM                                                         
*                                                                               
         CLC   CONACT,=C'SEND'                                                  
         BE    CONA0020                                                         
         CLC   CONACT,=C'CF  '     FOR ACTION CF, STATION MUST INPUT            
         BNE   CONA0040            CF=X, WHERE X IS THE LATEST REP              
         LA    R3,189              ACT. CF=X, X IS LATEST REP VER               
*                                                                               
CONA0020 XC    STLNUM,STLNUM                                                    
         CLI   WORK2+1,0           VERSION#. REP/STA CAN INPUT SEND OR          
         BE    CONA0040            SEND=X, WHERE X IS LATEST VER.               
         SPACE 1                                                                
CONA0030 TM    WORK2+3,X'80'       MAKE SURE 2ND HALF OF FIELD                  
         BNO   ERROR               IS NUMERIC                                   
         MVC   STLNUM,WORK2+11     SAVE BINARY REP VERSION NUMBER               
         SPACE 1                                                                
CONA0040 EQU   *                                                                
         CLI   BYTE,1            ANY BUY ACTION IN CONCACT?                     
         BNE   CONA0045            YES - MORE THAN ONE FIELD                    
*                                                                               
         TM    TWAPROST,X'02'      EXTEND BUYS SCREEN LOADED?                   
         BZ    *+8                 NO                                           
         BAS   R8,FFDIS            YES - RELOAD SO WE HAVE BUYNUM FIELD         
*                                                                               
         CLC   =C'DIS',CONACT      DISPLAY CONTRACT?                            
         BNE   CONA0070            NO  - TREAT AS SINGLE FIELD                  
*                                                                               
         TM    TWAPROST,X'01'      FULL SIZE SUBSCREEN LOADED?                  
         BO    CONA0043            YES - NO BUYACT FIELD                        
CONA0042 EQU   *                                                                
         L     R2,ABUYFH           NO  - CHECK BUY ACTION FIELD                 
         AR    R2,RA               SET ADDRESSABILITY                           
         CLI   5(R2),0             ANY DATA IN FIELD?                           
         BNE   CONA0070            YES - PROCESS IT WHEN READY                  
*                                                                               
         MVC   8(4,R2),=C'HIST'    NO  - FORCE 'HIST' BUYACTION                 
         MVI   5(R2),4             INSERT LENGTH OF '4'                         
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT FIELD                           
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT FIELD (BUY NUMBER)              
         OC    8(8,R2),8(R2)       CLEAR FIELD                                  
         MVI   5(R2),0             SET LENGTH TO ZERO                           
         FOUT  (R2)                                                             
CONA0043 EQU   *                                                                
         MVC   BUYACT,=C'HIST'     SAVE BUY ACTION                              
         B     CONA0070            FINISHED                                     
CONA0045 EQU   *                                                                
         TM    TWAPROST,X'01'+X'02'                                             
***                                 FULL SIZE SUBSCREEN LOADED?                 
         BZ    *+8                 OR EXTENDED BUY SCREEN IN USE?               
         BAS   R8,FFDIS            YES - RELOAD SO WE HAVE BUYNUM FIELD         
*                                                                               
         LA    R6,WORK2+32         SECOND SCAN FILED                            
         L     R2,ABUYFH                                                        
         AR    R2,RA               BUY ACTION FIELD                             
*                                                                               
         CLI   TWAACCS,C'$'        STATION?                                     
         BNE   CONA0048            ONLY STATION CAN CHANGE TRAFFIC#             
         CLC   =C'CF ',CONACT      CF ACTION?                                   
         BNE   CONA0048            THIS IS ONLY FOR CF                          
         MVC   CONACT,=C'DIS '     MAKE CON ACTION DISPLAY                      
         MVC   8(4,R2),=C'ORD '    MAKE BUY ACTION ORD                          
         MVI   5(R2),3                                                          
         MVC   TWATRAF,12(R6)      SAVE OFF TRAFFIC# FROM CONACT FIELD          
         MVI   TWATRAFL,C'Y'       SET FLAG BYTE FOR ALL THIS STUFF             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         CLC   RCONSRV,STLNUM                                                   
         BE    CONA0049                                                         
         LA    R2,CONCACTH                                                      
         LA    R3,186                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
CONA0048 DS    0H                                                               
*MN                                                                             
         CLC   12(4,R6),=C'CHIS'                                                
         BNE   *+10                                                             
         MVC   12(4,R6),=C'HIST'                                                
*MN                                                                             
         MVC   8(4,R2),12(R6)      BUY ACTION TO FIELD                          
         MVC   5(1,R2),0(R6)       LENGTH                                       
CONA0049 MVC   BUYACT,12(R6)       SAVE BUY ACTION                              
         FOUT  (R2)                                                             
         SPACE 1                                                                
         CLI   BYTE,2                                                           
         BE    CONA0070            NO BUY NUMBER IN CONCACT FIELD               
CONA0050 ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    1(R2),X'20'                                                      
         BO    CONA0050            FIND BUY NUMBER FIELD                        
         LA    R6,32(R6)           THIRD SCAN FIELD                             
         CLI   0(R6),3             MAXIMUM OF 3 CHARS?                          
         BNH   CONA0060            YES - 3 OR LESS                              
         CLI   0(R6),4             SPECIAL TEST FOR 'TOT$'                      
         BNE   CONA0080            CAN'T BE 'TOT$'                              
         CLC   =C'TOT$',12(R6)     IS IT 'TOT$'?                                
         BNE   CONA0080                                                         
CONA0060 EQU   *                                                                
         ZIC   R1,0(R2)                                                         
         TM    TWAPROST,X'01'      FULL SIZE SUBSCREEN LOADED?                  
         BZ    CONA0065            YES - NO BUYACT FIELD                        
CONA0063 EQU   *                                                                
         LA    R1,24               YES - FORCE DUMMY SCREEN LENGTH              
CONA0065 EQU   *                                                                
         SH    R1,=H'8'            FIELD LENGTH                                 
         ZIC   RE,0(R6)            DATA LENGTH                                  
         CR    R1,RE                                                            
         BL    *+6                                                              
         LR    R1,RE                                                            
         STC   R1,5(R2)                                                         
         STC   R1,BYNMLN           SAVE BUY NUMBER LENGTH                       
         LTR   R1,R1                                                            
         BNP   CONA0068                                                         
         BCTR  R1,0                                                             
CONA0068 EQU   *                                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R6)      BUY NUMBER TO SCREEN                         
         MVC   BUYNUM,8(R2)        SAVE BUY NUMBER                              
         MVI   BYNMST,X'CA'        SET  STATUS OF FIELD                         
         NI    4(R2),X'F7'                                                      
         TM    2(R6),X'80'                                                      
         BZ    *+8                                                              
         OI    4(R2),X'08'         VALID NUMERIC                                
         FOUT  (R2)                                                             
         B     CONA0075                                                         
CONA0070 MVI   BYNMLN,0                                                         
         MVI   BYNMST,0                                                         
CONA0075 EQU   *                                                                
         MVI   BYFLT,0                                                          
         B     CONA0090                                                         
         SPACE 1                                                                
CONA0080 LA    R3,INVINP                                                        
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
         EJECT                                                                  
*              BUY ACTION FIELD                                                 
         SPACE 1                                                                
CONA0090 DC    0H'0'                                                            
         TM    TWAPROST,X'01'      FULL SIZE SUBSCREEN LOADED?                  
         BO    CONA0120            YES - NO BUY FIELD                           
CONA0092 DC    0H'0'                                                            
         MVC   BUYACT,MYSPACES                                                  
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         CLI   5(R2),0                                                          
         BE    CONA0110            NO BUY ACTION                                
         MVC   BUYACT,8(R2)                                                     
         OC    BUYACT,MYSPACES     SPACE PAD INCASE ACTION IS 2 LETTERS         
*                                                                               
         NI    TWAFLAGS,X'FF'-TWABUYFQ                                          
         CLC   =C'BUYF',8(R2)      CHECK IF USER INVOKED BUYF ACTION            
         BNE   *+8                                                              
         OI    TWAFLAGS,TWABUYFQ                                                
*                                                                               
***      NI    TWAFLAGS,X'FF'-TWABUYTQ                                          
***      CLC   =C'BUY*',8(R2)      CHECK IF USER INVOKED BUY* ACTION            
***      BNE   *+8                                                              
***      OI    TWAFLAGS,TWABUYTQ                                                
*                                                                               
CONA0100 ZIC   R1,0(R2)            LOOK FOR BUYNUM FIELD                        
         AR    R2,R1                                                            
         TM    1(R2),X'20'                                                      
         BO    CONA0100            SKIP PROTECTED                               
         SPACE 1                                                                
         MVC   BUYNUM,MYSPACES                                                  
         CLI   5(R2),0                                                          
         BE    CONA0110                                                         
         MVC   BUYNUM,8(R2)        SAVE BUY NUMBER                              
         MVC   BYNMLN,5(R2)        LENGTH                                       
         MVC   BYNMST,4(R2)        STATUS                                       
*                                                                               
CONA0110 DC    0H'0'                                                            
         TM    CONCACTH+4,X'20'                                                 
         BNO   CONA0120                                                         
         CLC   =C'MG',CONCACT      MAKEGOOD ACTION CODE?                        
         BNE   BUY                 NO  - LOOK FOR BUY ACTION                    
         EJECT                                                                  
*              FIND PROPER ROUTINE FOR THIS ACTION                              
         SPACE 1                                                                
CONA0120 DC    0H'0'                                                            
         LA    R2,CONCACTH                                                      
         SPACE 1                                                                
         LA    R3,ACTERR                                                        
*                                                                               
*   STATION CAN ONLY DO ACTION DISPLAY, SEND, RESEND, CF                        
*        EC, E2, E3, OR MGS                                                     
*                                                                               
CONA0140 EQU   *                                                                
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   CONA0150                                                         
*                                                                               
         LA    R1,OKSTLST                                                       
*                                                                               
CONA0145 EQU   *                                                                
         CLC   0(L'OKSTLST,R1),CONACT                                           
         BE    CONA0160                                                         
         AHI   R1,L'OKSTLST                                                     
         CLI   0(R1),X'FF'                                                      
         BNE   CONA0145                                                         
         B     ERROR                                                            
* LIST OF VALID STATION ACTIONS                                                 
OKSTLST  DS    0CL4                                                             
         DC    C'DIS '                                                          
         DC    C'SEND'                                                          
         DC    C'RSND'                                                          
         DC    C'CF  '                                                          
         DC    C'LAST'                                                          
         DC    C'EC  '                                                          
         DC    C'E2  '                                                          
         DC    C'E3  '                                                          
         DC    C'PFM '                                                          
         DC    C'CD  '                                                          
         DC    C'ORD '                                                          
         DC    C'MGS '                                                          
         DC    C'MGL '                                                          
         DC    C'MGC '                                                          
         DC    C'MGO '                                                          
         DC    C'MGM '                                                          
         DC    C'MGX '                                                          
         DC    C'MGB '                                                          
         DC    C'MGP '                                                          
         DC    C'MLR '                                                          
         DC    C'MLB '                                                          
         DC    C'RNA '                                                          
         DC    C'AUL '                                                          
         DC    C'AULR'                                                          
         DC    C'AULS'                                                          
         DC    C'AUD '                                                          
         DC    X'FF'                                                            
*                                                                               
CONA0150 CLC   CONACT,=C'CF  '     REP CAN'T CONFIRM ACE CONTRACT               
         BNE   *+12                                                             
         TM    RCONMODR+1,X'40'    GRAPHNET?                                    
         BZ    ERROR                                                            
*                                                                               
**>>     CLC   CONACT,=C'MGX '     MAKEGOOD DELETE:  REP CAN'T DO IT            
**>>     BE    ERROR                                                            
**>>     CLC   CONACT,=C'MGO '     MAKEGOOD OFFER:   REP CAN'T DO IT            
**>>     BE    ERROR                                                            
         SPACE 1                                                                
*                                                                               
* FORCE ADDS FOR TV, ADDB FOR RADIO, OR ADD IF NEITHER IS SET                   
CONA0160 DS    0H                                                               
*                                                                               
         LA    RF,*                BE A LITTLE QUICKER ABOUT IT                 
         A     RF,=A(CONACTS-(*-4))                                             
*                                                                               
         OC    CONACT,MYSPACES     IN CASE 2 OR 3 LONG                          
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTOGEN ADD?                                 
         BO    CONA0170            YES, GO STRAIGHT TO ADD                      
*                                                                               
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    CONA0165            YES - CAN'T REQUIRE SPL                      
         CLC   =C'SEND',CONACT     ACTION SEND?                                 
         BNE   CONA0165            NO                                           
         TM    TWAPRFK,X'20'       CHECK CONTYPE K OPTION#3 ON?                 
         BO    CONA0165            YES - DON'T REQUIRE SPL                      
*                                                                               
         CLI   CONTYPE,C'N'        YES - CONTRACT TYPE = 'N'?                   
         BE    CONA0165            YES - DON'T CHECK STATION PROF               
CONA0162 EQU   *                                                                
         ST    RF,FULL                                                          
*                                  SKIP SPL IF TAKEOVER CONTRACT                
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=TWARELO1                                
         BZ    CONA0163                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,6            CHECK IF SPL ELEMENT EXISTS                  
         BAS   RE,GETEL                                                         
         BE    CONA0163            SKIP IF YES                                  
                                                                                
         XC    RSTAREC(32),RSTAREC NO, CHECK OPTION 8                           
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                  RETRIEVE THE STATION RECORD                  
         LA    R6,RSTAREC                                                       
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   CONA0163                                                         
         CLI   RSTAOPT8,C'N'                                                    
         BE    CONA0163                                                         
         MVC   CONACT(4),=C'DIS '  IF OPTION 8=Y, FORCE SPL ENTRY SINCE         
         MVC   BUYACT(3),=C'SPL'   NO SPL WAS FOUND                             
         DROP  R6                                                               
                                                                                
CONA0163 DS    0H                  RESTORE POINTER                              
         L     RF,FULL                                                          
                                                                                
CONA0165 DS    0H                                                               
* ADDS CHANGE TO ADD FOR TAKEOVER CONTRACTS                                     
         CLC   =C'ADDS',CONACT                                                  
         BNE   CONA0168                                                         
         ST    RF,FULL                                                          
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=TWARELO1                                
         L     RF,FULL                                                          
         BNZ   CONA0168            IF TAKEOVER AND ADDS                         
         MVC   CONACT,=C'ADD '     FORCE ADDS TO ADD                            
                                                                                
CONA0168 DS    0H                                                               
         CLC   =C'ADD ',CONACT     ACTION ADD?                                  
         BNE   CONA0170                                                         
         MVI   SASPADDS,C'N'       SET ADDS/SASP USER FLAG TO 'NO'              
         CLC   =C'ACC-',CONBUY     ACCOUNTING CONTRACT?                         
         BE    CONA0170            YES - DON'T FORCE SAR                        
         ST    RF,FULL                                                          
*                                  SKIP SAR IF TAKEOVER CONTRACT                
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=TWARELO1                                
         L     RF,FULL                                                          
         BZ    CONA0170                                                         
         TM    PROFILES+CNTMEDRB,CNTMEDRA                                       
         BZ    *+14                                                             
         MVC   CONACT(4),=C'ADDB'  RADIO - FORCE BOP                            
         B     CONA0170                                                         
         TM    PROFILES+CNTMEDTB,CNTMEDTA                                       
         BZ    *+10                                                             
         MVC   CONACT(4),=C'ADDS'  TV - FORCE SAR                               
         SPACE 1                                                                
CONA0170 CLI   0(RF),X'FF'                                                      
         BE    ERROR               INVALID ACTION                               
         CLC   CONACT,0(RF)        CON ACTION FOUND IN TABLE?                   
         BNE   CONA0220            NO  - KEEP LOOKING                           
*                                                                               
* CHECK FORECAST USE FLAG, SCREEN REQUEST                                       
*                                                                               
         TM    PROFILES+CNT4CASB,CNT4CASA                                       
         BZ    CONA0173                                                         
         ST    RF,FULL                                                          
         BAS   RE,IS4CAST          IS THIS A FORECAST CONTRACT?                 
         L     RF,FULL             RESTORE POINTER TO TABLE BEFORE              
         BNZ   CONA0173             CHECKING IF FORECAST                        
         CLC   =C'PFM',CONACT       YES, ONLY THESE ACTIONS ALLOWED             
         BE    CONA0230                                                         
         CLC   =C'DIS',CONACT       YES, ONLY THESE ACTIONS ALLOWED             
         BE    CONA0230                                                         
         CLC   =C'DEL',CONACT       YES, ONLY THESE ACTIONS ALLOWED             
         BE    CONA0230                                                         
         CLC   =C'FD',CONACT       YES, ONLY THESE ACTIONS ALLOWED              
         BE    CONA0230                                                         
         CLC   =C'FC',CONACT                                                    
         BE    CONA0230                                                         
         LA    R3,296              ACTION INVALID FOR A FORECAST K              
         B     ERROR                                                            
*                                                                               
*   CHECK SASP USE FLAGS, SCREEN REQUEST                                        
*                                                                               
CONA0173 DS    0H                                                               
         TM    PROFILES+CNTSASPB,CNTSASPA                                       
*                                  SASP USER?                                   
         BNO   CONA0180            NO  - CHECK NO FURTHER                       
         LA    R3,369              SASP USER/ACCESS DENIED ERROR                
*                                     ERROR MSG NOT USED AT THIS TIME           
         TM    15(RF),X'04'        DON'T USE FOR SASP USER?                     
         BNO   CONA0175            NO  -                                        
         CLC   =C'ADDS',CONACT     YES - 'ADDS' CONTRACT ACTION?                
         BNE   CONA0175            NO  - TREAT AS 'NOT FOUND'                   
                                                                                
         ST    RF,FULL                                                          
*                                  SKIP SASP IF TAKEOVER CONTRACT               
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=TWARELO1                                
         L     RF,FULL             RESTORE POINTER TO TABLE BEFORE              
         BNZ   CONA0174                                                         
         MVC   CONACT,=C'ADD '     FORCE ADDS TO ADD                            
         B     CONA0180                                                         
                                                                                
CONA0174 DS    0H                                                               
         MVI   SASPADDS,C'Y'       YES - SET 'ADDS/SASP USER' FLAG              
         B     CONA0220            TREAT AS 'NOT FOUND'                         
CONA0175 EQU   *                                                                
         TM    15(RF),X'02'        FORCE SASP SCREEN?                           
         BNO   CONA0180            NO                                           
                                                                                
         ST    RF,FULL                                                          
*                                  SKIP SASP IF TAKEOVER CONTRACT               
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=TWARELO1                                
         L     RF,FULL             RESTORE POINTER TO TABLE BEFORE              
         BZ    CONA0180                                                         
*                                                                               
         LA    RF,*                                                             
         A     RF,=A(SASPACT-(*-4))                                             
CONA0180 EQU   *                                                                
*                                                                               
*  COMBO USE/NON-USE:  WHEN ACTION IS FOUND IN CON-TABLE, THE FLAG              
*     FOR COMBO USE/NO-USE IS CHECKED FIRST.  IF SET ON, AN ALTERNATE           
*     OVERLAY/SCREEN EXISTS FOR THIS ACTION CODE WHICH IS TO BE                 
*     CALLED IF - AND ONLY IF - THE CONTRACT BEING PROCESSED IS A               
*     COMBO ORDER, INDICATED BY FLAG IN TWA                                     
*                                                                               
         TM    15(RF),X'10'        TEST FOR COMBO USE/NON-USE?                  
         BNO   CONA0230            NO  - LOAD AND GO ON 'FOUND'                 
*                                                                               
*   IF ACTION IS 'EPLR', A NEW CONTRACT IS NOT BEING ADDED, AND                 
*     WHICH SCREEN IS SHOWN WILL DEPEND ON WHETHER THE CONTRACT                 
*     ITSELF, WHICH HAS ALREADY BEEN READ, IS COMBO OR NOT.                     
*                                                                               
         CLC   CONACT,=C'EPLR'     ACTION CODE 'EPLR'?                          
         BNE   CONA0185            NO                                           
         ST    RF,FULL             SAVE RF                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        IS ORDER A COMBO?                            
         BAS   RE,GETEL            FIND THE X'17' ELEMENT                       
         L     RF,FULL             RESTORE RF                                   
         BE    CONA0210            YES - CHECK FOR COMBO USE                    
         B     CONA0200            NO  -                                        
*                                                                               
CONA0185 EQU   *                                                                
         CLC   =C'ORD',CONACT      ACTION CODE 'ORD'?                           
         BNE   CONA0190            NO                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        IS ORDER A COMBO?                            
         ST    RF,FULL                                                          
         BAS   RE,GETEL            FIND THE X'17' ELEMENT                       
         L     RF,FULL                                                          
         BE    CONA0210                                                         
         B     CONA0200                                                         
CONA0190 EQU   *                                                                
*                                                                               
         CLC   =C'-C',CONSTA+3     YES - IS STATION COMBO STATION?              
         BE    CONA0210            YES - CHECK FOR COMBO USE                    
         CLC   =C'-C',CONSTA+4                                                  
         BE    CONA0210            YES - CHECK FOR COMBO USE                    
*                                                                               
*  *** NOT A COMBO ORDER ***                                                    
*                                                                               
CONA0200 EQU   *                                                                
         TM    15(RF),X'01'        COMBO SCREEN/OVERLAY?                        
         BO    CONA0220            YES - KEEP LOOKING                           
         B     CONA0230            NO  - LOAD AND GO                            
*                                                                               
* *** YES A COMBO ORDER ***                                                     
*                                                                               
CONA0210 EQU   *                                                                
         TM    15(RF),X'01'        COMBO SCREEN OVERLAY?                        
         BO    CONA0230            YES - LOAD AND GO                            
*                                                                               
CONA0220 EQU   *                                                                
         LA    RF,L'CONACTS(RF)                                                 
         B     CONA0170                                                         
         SPACE 1                                                                
CONA0230 DS    0H                                                               
         LR    R0,RF               SAVE RF                                      
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         XC    WORK,WORK           PAR APPLICATION SECURITY CHECK               
         LA    R1,WORK                                                          
         USING ABLOCK,R1                                                        
         MVC   ABPGM,=C'CON'                                                    
         MVC   ABREC,=CL8'CONACT'                                               
         MVC   ABACT(4),0(RF)                                                   
         OC    ABACT,MYSPACES                                                   
         DROP  R1                                                               
*                                                                               
*   FOLLOWING CHECK IS DONE BEFORE PAR CALL, TO IGNORE AN                       
*        ERROR RETURN IF THE CALL HAS BEEN MADE FOR A 'PROPOSER'                
*        AUTO ASSIGN.                                                           
*                                                                               
         LA    R3,TWAGLVX                                                       
SWP      USING GLVXFRSY,R3                                                      
         CLC   =C'SWP',SWP.GLVXFRPR      FROM PROPOSER PROGRAM?                 
         BE    CONA0232            YES - IGNORE THE ERROR RETURN                
*                                                                               
         DROP  SWP                                                              
*                                                                               
         L     R1,AFACILS                                                       
         L     R3,0(R1)            ATIO                                         
         GOTO1 (RFCKASEC,VREPFACS),DMCB,WORK,CONMSGH,(R3),DUB,0                 
         BE    CONA0232            OK                                           
         LA    R2,CONCACTH                                                      
         OI    6(R2),X'80'+X'40'                                                
         L     RD,BASERD           INVALID - GET ALL THE WAY OUT                
         B     EXXMOD                                                           
*                                                                               
CONA0232 DS    0H                                                               
         LR    RF,R0               RESTORE RF                                   
         TM    TWAPROST,X'01'      FULL SUBSCREEN LOADED?                       
         BZ    CONA0235            NO - GO AHEAD                                
         TM    9(RF),X'01'         NEW SUBSCREEN FULL SCREEN?                   
         BO    CONA0235            YES - GO AHEAD                               
CONA0233 DS    0H                                                               
         ST    RF,FULL                                                          
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
         L     RF,FULL                                                          
CONA0235 DS    0H                                                               
         MVC   LASTCACT,CONACT     SAVE THE ACTION                              
         LR    R5,RF                                                            
         L     RF,4(RF)                                                         
         RELOC                                                                  
         AR    RF,RE                                                            
         BR    RF                  ROUTINE FOR THIS ACTION                      
         EJECT                                                                  
********************************************************************            
* ADD FORECAST CONTRACT                                                         
********************************************************************            
ADDF     DS    0H                                                               
         GOTO1 =A(ADDF00),RR=TWARELO1                                           
         B     EXXMOD                                                           
********************************************************************            
* FORECAST CONTRACT CHANGE                                                      
********************************************************************            
CHAF     GOTO1 =A(CHAF00),RR=TWARELO1                                           
         B     EXXMOD                                                           
********************************************************************            
* FORECAST CONTRACT DISPLAY                                                     
********************************************************************            
DISF     DS    0H                                                               
         GOTO1 =A(DISF00),RR=TWARELO1                                           
         B     EXXMOD                                                           
*                                                                               
PCHA     GOTO1 =A(PAYC00),RR=TWARELO1                                           
         B     EXXMOD                                                           
*                                                                               
*              ADD CONTRACT AND BOP 'ADDR'                                      
         SPACE 1                                                                
ADDR     DC    0H'0'                                                            
         LA    R2,CONCACTH                                                      
         LA    R3,ACTERR                                                        
         CLC   =C'-T',CONSTA+4     DO NOT ALLOW BOP FOR TV                      
         BE    ERROR                                                            
         CLC   =C'-T',CONSTA+3                                                  
         BE    ERROR                                                            
         CLC   =C'-L',CONSTA+4     DO NOT ALLOW BOP FOR LOW POWER TV            
         BE    ERROR                                                            
         CLC   =C'-L',CONSTA+3                                                  
         BE    ERROR                                                            
         CLI   TWASCRN,X'FB'                                                    
         BNE   ADDR01                                                           
         TM    CONAGYH+1,X'20'                                                  
         BO    ADDR01                                                           
         TM    BOPORDH+1,X'20'                                                  
         BZ    ADDR10                                                           
         SPACE 1                                                                
ADDR01   MVI   TWASCRN,X'FF'       FORCE FB TO BE LOADED                        
         BAS   R8,LOADSCRN                                                      
         TM    TWAPROST,X'20'      CHANGED?                                     
         BZ    ADDR10                                                           
         BAS   R8,DIS1                                                          
         GOTO1 VCLEAR,DMCB,0                                                    
         FOUT  CONCNUMH,MYSPACES,8                                              
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R2,CONAGYH                                                       
         B     EXIT                                                             
         SPACE 1                                                                
ADDR10   DS    0H                                                               
**JRD    L     R5,=A(CONACTS)      IN CASE WE CAME HERE DIRECTLY                
**JRD    CNOP  0,4                 ESTABLISH ADDRESSABILITY FOR TABLE           
**JRD    B     *+8                                                              
**JRD    DC    A(*)                                                             
**JRD    LA    RE,*-4                                                           
**JRD    S     RE,*-8                                                           
**JRD    AR    R5,RE                                                            
         LA    R5,*                BE A LITTLE QUICKER ABOUT IT                 
         A     R5,=A(CONACTS-(*-4))                                             
*                                                                               
         BAS   R8,INVAL            TURN OFF VALID BITS                          
         BAS   R8,CONEDIT          EDIT CONTRACT HEADLINE                       
         TM    CONAGYH+1,X'20'                                                  
         BO    ADDR11                                                           
         TM    TWAPROST,X'01'                                                   
         BZ    ADDR10A                                                          
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
ADDR10A  GOTO1 VLDSCRN,DMCB,(X'FB',CONLAST),(X'C0',0)                           
         NI    TWAPROST,X'FF'-X'01'                                             
*                                                                               
ADDR11   BAS   R8,CONDIS           DISPLAY CONTRACT                             
         BAS   R8,LOADDIS                                                       
         SPACE 1                                                                
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         BAS   R8,DIS2                                                          
         GOTO1 VCKTAREQ                                                         
         SPACE 1                                                                
         LA    R2,CONBUYH                                                       
         NI    CONSTAH+1,X'DF'     STATION                                      
         NI    CONBUYH+1,X'DF'     AND BUYER NOT PROTECTED                      
         OI    CONSTAH+6,X'80'     XMIT                                         
         OI    CONBUYH+6,X'80'     XMIT                                         
         SPACE 1                                                                
         LA    R3,BOPTABH                                                       
         LA    R2,BOPORDH                                                       
         SPACE 1                                                                
ADDR12   OI    1(R2),X'20'                                                      
         OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HDR?                            
         BZ    *+8                 NO                                           
         SH    RF,=H'8'                                                         
         STC   RF,7(R2)                                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3                                                            
         BL    ADDR12                                                           
         SPACE 1                                                                
ADDR15   LA    R2,CONBUYH                                                       
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTOGEN ADD?                                 
         BO    GOBACK                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*   ADD CONTRACT AND XXX INFORMATION  'ADDX'                                    
         SPACE 1                                                                
ADDC     DC    0H'0'                                                            
         CLC   =C'ADDB',0(R5)                                                   
         BNE   ADDC20                                                           
         LA    R2,CONCACTH                                                      
         LA    R3,ACTERR                                                        
         CLC   =C'-T',CONSTA+4     DO NOT ALLOW BOP FOR TV                      
         BE    ERROR                                                            
         CLC   =C'-T',CONSTA+3                                                  
         BE    ERROR                                                            
         CLC   =C'-L',CONSTA+4     DO NOT ALLOW BOP FOR TV                      
         BE    ERROR                                                            
         CLC   =C'-L',CONSTA+3                                                  
         BE    ERROR                                                            
ADDC20   DS    0H                                                               
         BAS   R8,LOADSCRN                                                      
         CLC   =C'ADDS',CONACT     DIFF ACTIONS FOR ADD SAR                     
         BNE   ADDC30                                                           
*                                                                               
* SCRIPT IN PROGRESS? AND CONTRACT NOT FOUND? ABOUT TO PERFORM ADDS             
*                                                                               
         TM    TWASTREO,TWARCUQ+TWARCUEQ                                        
         BNO   ADDC30                                                           
* RESET ERROR                                                                   
         NI    TWASTREO,X'FF'-TWARCUEQ                                          
         GOTO1 VCLEAR,DMCB,0       CLEAR CONTRACT HEADLINE FIELDS               
         FOUT  CONCNUMH,MYSPACES,8                                              
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         MVC   CONSTA(4),TWARCUST  YES, FILL IN STATION                         
         MVI   CONSTAH+5,4                                                      
         CLI   CONSTA+3,C' '                                                    
         BNE   *+8                                                              
         MVI   CONSTAH+5,3                                                      
         NI    CONSTAH+4,X'FF'-X'20' TURN OFF PREVALID BIT                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+3,2                                                         
         OC    TWARCUP#,TWARCUP#   PENDING NUMBER PROVIDED                      
         BZ    ADDC25                BUT NOT FOUND?                             
*                                                                               
         MVI   CONTYPE,C'Y'                                                     
         MVI   CONTYPEH+5,1                                                     
         NI    CONTYPEH+4,X'FF'-X'20' TURN OFF PREVALID BIT                     
         MVC   CONSAL(3),=C'WOS'                                                
         MVI   CONSALH+5,3                                                      
         NI    CONSALH+4,X'FF'-X'20' TURN OFF PREVALID BIT                      
         MVC   CONADV(4),=C'WOAD'                                               
         MVI   CONADVH+5,4                                                      
         NI    CONADVH+4,X'FF'-X'20' TURN OFF PREVALID BIT                      
         MVC   CONAGY(4),=C'WOAG'                                               
         MVI   CONAGYH+5,4                                                      
         NI    CONAGYH+4,X'FF'-X'20' TURN OFF PREVALID BIT                      
         MVI   DMCB+3,174          SPECIAL INF MSG FOR SCRIPT                   
*                                                                               
ADDC25   DS    0H                                                               
         GOTO1 VDISMSG,DMCB                                                     
         MVC   ADSLAST+1(2),=C'0101' RETRANSMIT ENTIRE SCREEN                   
         LA    R2,CONTYPEH                                                      
         B     EXIT                                                             
****>    BAS   RE,CKALLOWD                                                      
****>    CLI   WORK,C'Y'                                                        
****>    BNE   ADDC30                                                           
****>    NI    SARCPPH+1,X'DF'     MAKE UNPROTECTED                             
****>    NI    SARBGTH+1,X'DF'                                                  
****>    NI    SARGTOTH+1,X'DF'                                                 
****>    NI    SARCTOTH+1,X'DF'                                                 
ADDC30   DS    0H                                                               
         CLC   =C'ADD TR',CONCACT                                               
         BE    ADDC32                                                           
         CLC   =C'ADD ',CONCACT                                                 
         BNE   *+12                                                             
         TM    TWAPROST,X'24'      20=CHANGED, 04=NEW SCREEN                    
         B     *+8                                                              
ADDC32   TM    TWAPROST,X'20'      20=STATUS CHANGED                            
         BZ    ADDC40                                                           
         BAS   R8,DIS1                                                          
         GOTO1 VCLEAR,DMCB,0       CLEAR CONTRACT HEADLINE FIELDS               
         FOUT  CONCNUMH,MYSPACES,8                                              
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R2,CONAGYH                                                       
         B     EXIT                                                             
         SPACE 1                                                                
ADDC40   DS    0H                                                               
         TM    TWAGENFG,TWQGOGEN   AUTOGEN ADD?                                 
         BZ    ADDC90                                                           
         CLC   =C'DAR',TWAGENCP    CALLER IS DARE?                              
         BNE   ADDC90                                                           
*                                                                               
AUTOD    USING RCAUTOD,TWAGENBK    AUTO FILL IN DEMO                            
         OC    AUTOD.RCAUDEMO,AUTOD.RCAUDEMO                                    
         BZ    ADDC90                                                           
*                                                                               
         CLI   SASPADDS,C'Y'       SAR OR PENDING?                              
         BE    ADDC50                                                           
         OC    SARDEM,SARDEM       SOMETHING THERE ALREADY?                     
         BNZ   ADDC90                                                           
         MVC   SARDEM(L'RCAUDEMO),AUTOD.RCAUDEMO                                
*                                                                               
         TM    PROFILES+CNTPDEMB,CNTPDEMA                                       
         BZ    ADDC90              PRIMARY DEMO REQUIRED?                       
         MVI   SARDEM+L'RCAUDEMO,C' '                                           
         LA    RF,SARDEM           IF YES, AUTOMATICALLY STICK A 'P'            
ADDC43   CLI   0(RF),C' '          AT THE END                                   
         BE    ADDC45                                                           
         LA    RF,1(RF)                                                         
         B     ADDC43                                                           
ADDC45   BCTR  RF,0                                                             
         CLI   0(RF),C'P'          SKIP IF ALREADY A PRIMARY                    
         BE    ADDC90                                                           
         MVI   1(RF),C'P'                                                       
         B     ADDC90                                                           
*                                                                               
ADDC50   DS    0H                                                               
         OC    ADSSDEM,ADSSDEM     SOMETHING THERE ALREADY?                     
         BNZ   ADDC90                                                           
         MVC   ADSSDEM(L'RCAUDEMO),AUTOD.RCAUDEMO                               
*                                                                               
         TM    PROFILES+CNTPDEMB,CNTPDEMA                                       
         BZ    ADDC90              PRIMARY DEMO REQUIRED?                       
         MVI   ADSSDEM+L'RCAUDEMO,C' '                                          
         LA    RF,ADSSDEM          IF YES, AUTOMATICALLY STICK A 'P'            
ADDC53   CLI   0(RF),C' '          AT THE END                                   
         BE    ADDC55                                                           
         LA    RF,1(RF)                                                         
         B     ADDC53                                                           
ADDC55   BCTR  RF,0                                                             
         CLI   0(RF),C'P'          SKIP IF ALREADY A PRIMARY                    
         BE    ADDC90                                                           
         MVI   1(RF),C'P'                                                       
         DROP  AUTOD                                                            
*                                                                               
ADDC90   DS    0H                                                               
         BAS   R8,INVAL            TURN OFF VALID BITS                          
         BAS   R8,CONEDIT          EDIT CONTRACT HEADLINE                       
         CLI   8(R5),X'00'         ACTION ADDN                                  
         BE    ADDC100                                                          
         BAS   R8,CONDIS           DISPLAY CONTRACT                             
         BAS   R8,LOADDIS                                                       
         OI    CONCNUMH+4,X'20'                                                 
         BAS   R8,DIS2                                                          
         GOTO1 VCKTAREQ            CHECK FOR T/A REQUEST                        
         LA    R2,CONCACTH                                                      
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTOGEN ADD?                                 
         BO    GOBACK                                                           
*                                                                               
         B     EXIT                                                             
ADDC100  DS    0H                                                               
         BAS   R8,CONDIS           DISPLAY CONTRACT                             
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'                                                 
         OI    CONCNUMH+4,X'20'                                                 
         BAS   R8,DIS2                                                          
         GOTO1 VCKTAREQ                                                         
         TM    TWAPROST,X'01'                                                   
         BZ    ADDC105                                                          
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
*                                                                               
ADDC105  DS    0H                                                               
         TM    PROFILES+CNTXBUYB,CNTXBUYA                                       
         BO    ADDC110             USE EXTENDED BUY SCREEN?                     
         GOTO1 VLDSCRN,DMCB,(X'FE',CONLAST),(X'C0',0)                           
*                                                                               
ADDC110  DS    0H                                                               
         NI    TWAPROST,X'FF'-X'01'                                             
         LA    R2,CONBACTH                                                      
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTOGEN ADD?                                 
         BO    GOBACK                                                           
*                                                                               
*        CLC   1,MYSPACES          << HUH????                                   
*        BE    EXIT                                                             
         B     BUY                                                              
         EJECT                                                                  
*                                                                               
* AUTO CONTRACT GENERATION RETURN ROUTINE                                       
* RETURNS CONTRACT NUMBER TO CALLER                                             
*                                                                               
GOBACK   DS    0H                                                               
         XC    WORK2,WORK2                                                      
*                                                                               
         TM    TWADARE,X'04'                                                    
         BO    GOBACK10                                                         
         TM    TWAGENFG,TWQGOGEN                                                
         BO    GOBACK30                                                         
         DC    H'0'                                                             
*                                                                               
GOBACK10 DS    0H                                                               
         NI    TWADARE,X'FF'-X'02'-X'04'                                        
**************                                                                  
* DUB HAS ERROR MESSAGE IF ANY                                                  
**************                                                                  
         B     GOBACK40                                                         
*                                                                               
GOBACK30 DS    0H                                                               
         NI    TWAGENFG,X'FF'-TWQGOGEN    SET AUTOGEN MODE OFF                  
*                                                                               
         LA    R1,WORK2            PASS BACK CONTRACT NUMBER                    
         USING RCAUTOD,R1                                                       
         MVC   RCAUCON#,RCONKCON                                                
         DROP  R1                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,RCAUELRQ,GLRCAUTO                       
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOBACK40 DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,TWAGENCS   TO THE CALLER'S SYSTEM                       
         MVC   GLVXTOPR,TWAGENCP   TO THE CALLER'S PROGRAM                      
* THIS IS A RETURN CALL, USES SEPARATE SESSION, CALL BASE ON TRANSFER           
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
*                                                                               
         TM    TWADARE,X'04'       MAKEGOOD DARE APPLY. DON'T BOTHER            
         BO    GOBACK50            WITH SESSION RETURN, JUST RETURN             
*                                                                               
         TM    TWAGENGF,GLV1SIDR   CALLER'S SESSION DEFINED?                    
         BZ    GOBACK50                                                         
         MVC   GLVXSESR,TWAGENSS   YES, RETURN TO CALLER'S SESSION              
***>>>   OI    GLVXFLG1,GLV1SIDR+GLV1GOTO                                       
         OI    GLVXFLG1,GLV1SIDR                                                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
GOBACK50 DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* SWAPS TO SFM AND INVOKES CONBCOPY                                             
COPY     DS    0H                                                               
         GOTO1 =A(DOCOPY),RR=TWARELO1                                           
         DC    H'0'                SHOULDN'T COME BACK HERE                     
*                                                                               
*              CHANGE A CONTRACT                                                
*                                                                               
*                                                                               
CHAC     DC    0H'0'                                                            
         BAS   R8,LOADSCRN                                                      
         TM    TWAPROST,X'20'                                                   
         BO    CHAC10              SCREEN CHANGE MUST DISPLAY                   
         TM    CONCNUMH+4,X'20'                                                 
         BO    CHAC20              NUMBER IS NOT CHANGED                        
         SPACE 1                                                                
CHAC10   BAS   R8,CONDIS                                                        
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'                                                 
         LA    R2,CONCACTH                                                      
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTOGEN CHANGE?                              
         BZ    EXIT                NO - DONE HERE                               
         GOTO1 AAUTOGEN            YES - FILL IN SCREEN & CONTINUE              
*                                                                               
CHAC20   DS    0H                                                               
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    CHAC30                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBCON',0),RR=TWARELO1                
         B     CHAC40                                                           
*                                                                               
CHAC30   BAS   R8,CONEDIT                                                       
*                                                                               
CHAC40   BAS   R8,CONDIS                                                        
         TM    TWAPROST,X'01'                                                   
         BZ    CHAC45                                                           
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
CHAC45   GOTO1 VLDSCRN,DMCB,(X'FE',CONLAST),(X'C0',0)                           
         NI    TWAPROST,X'FF'-X'01'                                             
*                                                                               
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBDIS',0),RR=TWARELO1                
*                                  PROTECT/UNPROTECT COMBO RATE FIELDS          
*                                                                               
         BAS   R8,DIS2                                                          
         GOTO1 VCKTAREQ                                                         
         SPACE 1                                                                
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONBACTH                                                      
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTOGEN CHANGE?                              
         BO    GOBACK              YES - EXIT VIA GO BACK ROUTUNE               
*                                                                               
         B     EXIT                NO - PLAIN VANILLA EXIT                      
         EJECT                                                                  
*              DISPLAY CONTRACT AND XXX INFO 'DISC'                             
         SPACE 1                                                                
DISC     DC    0H'0'                                                            
         CLC   =C'DISB',0(R5)                                                   
         BNE   DISC1                                                            
         CLI   TWASCRN,X'FB'                                                    
         BNE   DISC1                                                            
         TM    BOPORDH+1,X'20'                                                  
         BZ    DISC1                                                            
         MVI   TWASCRN,X'FF'                                                    
DISC1    DS    0H                                                               
         BAS   R8,LOADSCRN                                                      
*                                                                               
         CLC   =C'DISS',0(R5)                                                   
         BNE   DISC2                                                            
         BAS   RE,CKALLOWD                                                      
         CLI   WORK,C'Y'                                                        
         BNE   DISC2                                                            
         NI    SARCPPH+1,X'DF'     MAKE UNPROTECTED                             
         NI    SARBGTH+1,X'DF'                                                  
         NI    SARGTOTH+1,X'DF'                                                 
         NI    SARCTOTH+1,X'DF'                                                 
*                                                                               
DISC2    BAS   R8,CONDIS           CONTRACT DISPLAY                             
         CLI   8(R5),X'FE'         DISPLAY CONTRACT ONLY?                       
         BNE   DISC10              NO                                           
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONBACTH                                                      
         NI    CONBACTH+4,X'DF'    FOR CHANGED CONTRACT NUMBER,                 
         NI    CONBNUMH+4,X'DF'    FORCE RE-VALIDATION OF BUY ACT/NUM           
         CLC   BUYACT,MYSPACES                                                  
         BNE   BUY                                                              
         BAS   R8,DIS1                                                          
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBDIS',0),RR=TWARELO1                
         B     EXIT                                                             
DISC10   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONCACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              DISPLAY CONTRACT AND HANDLE MULTIPLE BUY CHANGE                  
**********************************************************************          
MCIR     GOTO1 =A(MCIR00),RR=TWARELO1                                           
         B     EXXMOD                                                           
*                                                                               
**********************************************************************          
*              DISPLAY CONTRACT AND ADD EPL 'EPLR'                              
**********************************************************************          
EPLR     GOTO1 =A(EPLR00),RR=TWARELO1                                           
         B     EXXMOD                                                           
*                                                                               
* DELETE/RESTORE A CONTRACT                                                     
         SPACE 1                                                                
DELC     DC    0H'0'                                                            
RESC     DC    0H'0'                                                            
         GOTO1 =A(DELC00),RR=TWARELO1                                           
         B     EXXMOD                                                           
         EJECT                                                                  
*              PRINT CONTRACT                                                   
         SPACE 1                                                                
PRIC     DC    0H'0'                                                            
         BAS   R8,LOADSCRN                                                      
         BAS   R8,CONDIS                                                        
         MVI   TWACFFLG,X'80'      PRINT K & NOTHING ELSE                       
         BAS   R8,LOADDIS                                                       
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONCACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*      EC (ELECTRONICALLY SEND) AN ORDER TO TRAFFIC                             
*                                                                               
ECON     DC    0H'0'                                                            
         LA    R3,865                                                           
         BAS   RE,IS4CAST                                                       
         BZ    ERROR               CONTRACT IS A FORECAST CONTRACT              
*******                                                                         
******* SPECIAL HARD CODE FOR KPIXL (CB) SIGNON TO USE EC FORMAT X              
******* KUI (8/19/04)                                                           
*******                                                                         
*******  CLC   =AL2(10304),TWAUSRID                                             
*******  BE    ECON0010                                                         
*                                                                               
*   ABOVE HARDCODED TEST IS REMOVED IN FAVOR OF TESTING A BIT SET IN            
*        'TWASTREO (X'20') THAT WAS SET IN RECNT00 FOR CBS / NON-CBS            
*        SIGNON IDS.  THIS IS PER CBS, APRIL 19, 2006 (BU)                      
*                                                                               
         MVI   BYTE2,0             INIT STATION TRAFFIC SYSTEM                  
*                                                                               
         TM    TWASTREO,X'20'      SPECIAL CBS CONDITION ENCOUNTERED?           
         BO    ECON0010            YES -                                        
*                                                                               
         TM    TWAFLAGS,TWAFLHMQ   HOME MKT: PERMIT EC FROM REP SIDE?           
         BO    ECON0020            YES                                          
*                                                                               
*   RETRIEVE THE STATION RECORD IN ALL CASES.  PROFILE IS NEEDED                
*        TO DETERMINE IF REP CAN INITIATE THIS ACTION.                          
*                                                                               
ECON0010 DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                  RETRIEVE THE STATION RECORD                  
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'        FIND EXTRA DESCRIP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   ECON0280            NOT FOUND:  NOT EC STATION                   
         USING RSTAXXEL,R6                                                      
*                                                                               
*   EC'ING FROM REP SIDE:  START                                                
*                                                                               
         MVC   BYTE2,RSTATRAF      SAVE STATION TRAFFIC SYSTEM                  
         TM    RSTAXOPT,X'80'      FOUND - EC STATION?                          
         BNO   ECON0280            NO  -                                        
         TM    RSTAOPTC,X'80'      YES - CAN REP EC?                            
         BNO   ECON0015            NO  - DO A STATION CHECK                     
         CLI   TWAACCS,C'$'        YES - IS STATION MAKING REQUEST?             
         BE    ECON0013            YES - PERMIT IT - STA CHECK DONE             
         CLC   =C'CB',RSTAKREP     NO  - IS REP 'CBS'?                          
         BNE   ECON0020            NO  - FORCE INTO STATION CHECK               
         MVI   BYTE2,C'X'          YES - FORCE TRAFFIC FORMAT 'X'               
         DROP  R6                                                               
*                                                                               
ECON0013 EQU   *                                                                
         CLI   RSTAP8,C'Y'                                                      
         BNE   ECON0020                                                         
         MVI   BYTE2,C'O'          YES - FORCE TRAFFIC FORMAT 'O'               
         B     ECON0020                                                         
*                                                                               
*   STATION RECORD GETS REREAD A LITTLE FURTHER ON.  NEED TO TRAP               
*        TRAFFIC CODE 'X' TO FORCE INTO RECORD                                  
*        THIS HAS BEEN CHANGED FROM  'Z'                                        
*                                                                               
ECON0015 EQU   *                                                                
*                                                                               
*   EC'ING FROM REP SIDE: END                                                   
*                                                                               
         CLI   TWAACCS,C'$'        NO  - STATION MAKING REQUEST?                
         BNE   ECON0300            NO  - DON'T PERMIT IT                        
ECON0020 EQU   *                                                                
         BAS   R8,LOADSCRN         LOAD THE CONTRACT SCREEN                     
         BAS   R8,CONDIS           DISPLAY THE CONTRACT                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'        LOOK FOR SEND INFO ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   ECON0310            NOT FOUND - CANT BE AT STATION               
         TM    4(R6),X'80'         RCONSENF: LAST SENT BY REP?                  
****>>>  BNO   ECON0310            NO  - DON'T PERMIT                           
*                                     LAST SENT TEST INACTIVATED                
*                                        14 JULY 95 - BU PER KH                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'15'        LOOK FOR EC CONTROL ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    ECON0040            FOUND                                        
         CLC   =C'EC',CONACT       NOT FOUND: MUST BE 'EC' ACTION               
         BNE   ECON0200            NO  - E2/E3 NOT ALLOWED YET                  
         B     ECON0120                                                         
ECON0040 EQU   *                                                                
         USING RCONECEL,R6                                                      
         OC    RCONECCT,RCONECCT   HAS CONTRACT BEEN EC'D?                      
         BNZ   ECON0080            YES                                          
         DROP  R6                                                               
         CLC   =C'E2',CONACT       NO  - IS IT AN E2?                           
         BE    ECON0200            YES - CAN'T BE                               
         CLC   =C'E3',CONACT       NO  - IS IT AN E3?                           
         BE    ECON0200            YES - CAN'T BE                               
         B     ECON0120                                                         
ECON0080 EQU   *                                                                
         CLC   =C'EC',CONACT       ALREADY EC'D - IS IT AN EC?                  
         BE    ECON0240            YES - CAN'T BE                               
         CLC   =C'E3',CONACT       NOT AN EC: VALIDATE FOR E3                   
         BNE   ECON0120            NOT E3: PROCEED                              
*                                                                               
*   SPECIAL CODE TO LIMIT E3 TO A REP AND STATION                               
*        EITHER EAGLE TV/WTKR OR SJR/WORB                                       
*                                                                               
         CLC   =C'AM',RCONKREP     MUST BE THIS REP: EAGLE TV                   
         BNE   ECON0100            NOT:  CHECK FOR SJR                          
         CLC   =C'WTKR',RCONKSTA   MUST BE THIS STATION                         
         BNE   ECON0330                                                         
         B     ECON0120            EAGLE + WTKR: OKAY                           
*                                                                               
ECON0100 EQU   *                                                                
         CLC   =C'SJ',RCONKREP     MUST BE THIS REP: SJR TEST                   
         BNE   ECON0330            NOT:  ERROR                                  
         CLC   =C'WORB',RCONKSTA   MUST BE THIS STATION                         
         BNE   ECON0330                                                         
*                                  SJR + WORB: OKAY                             
ECON0120 EQU   *                                                                
*                                                                               
*        THIS CODE HAS BEEN DUPED EARLIER, TO PERMIT A REP TO EC                
*        BASED ON A PROFILE FLAG.  UNFORTUNATELY, INTERVENING                   
*        ACTIONS REQUIRE THAT IT BE READ AGAIN, AS THE IO AREA                  
*        HAS BEEN OVERLAID BY SUBSEQUENT READS.                                 
*                                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                  RETRIEVE THE STATION RECORD                  
*                                                                               
*   FOR EC'ING FROM REP SIDE - START                                            
*                                                                               
         CLI   BYTE2,0             OVERRIDE PRESENT?                            
         BE    *+10                                                             
         MVC   RSTATRAF,BYTE2      RESET STATION TRAFFIC SYSTEM                 
*                                  RETRIEVE THE STATION RECORD                  
*   FOR EC'ING FROM REP SIDE - END                                              
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'        FIND EXTRA DESCRIP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   ECON0280            NOT FOUND:  NOT EC STATION                   
         USING RSTAXXEL,R6                                                      
         TM    RSTAXOPT,X'80'      FOUND - EC STATION?                          
         BNO   ECON0280            NO  -                                        
*                                                                               
**********************************************************************          
*                                                                               
*                                                                               
*   AT THIS TIME (AUG/93) THERE ARE FOUR PROJECTED TRAFFIC SYSTEMS.             
*      B=BIAS,J=JDS/2000,K=KAMAN(ENTERPRISE),C=COLUMBINE.  DIRECT               
*      COMPARES ARE PLACED HERE, RATHER THAN SETTING UP A TABLE.                
*      TRAFFIC CODE OF W ALSO GOES TO BIAS                                      
*      VCI ADDED MAY03/95.                                                      
*   FOR COLUMBINE/RADIO, IF EC STA RECOGNIZED, WITH MEDIA = A OR F,             
*        RSTAOPTA X'01' MUST BE ON (LAST FLAG IN OPTIONS FIELD ON               
*        STATION FILE MAINTENANCE SCREEN.                                       
*                                                                               
*                                                                               
*   SPECIAL CODE FOR WIDE ORBIT:  ACTION CODE 'E3' WILL FORCE THE               
*        TRANSFER TO THE WIDE ORBIT TRAFFIC FORMAT/BDE VERSION.                 
*                                                                               
         MVI   10(R5),X'59'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     WIDE ORBIT OVERLAY (RECNT59).             
*                                  RECNT59 = BDE VERSION                        
*                                  RECNT7F = PIANO VERSION                      
         CLC   =C'E3',CONACT       ACTION VALIDATED AS E3: E3                   
         BE    ECON0160               FORCE WIDE ORBIT                          
*                                                                               
*                                                                               
         MVI   10(R5),X'64'        SET TRAFFIC SYSTEM TO LOAD                   
         TM    RSTAOPTB,X'20'      USE NEW EC CHANGES VERSION?                  
         BNO   ECON0122            NO                                           
         MVI   10(R5),X'77'        SET TRAFFIC SYSTEM TO LOAD                   
*                                    BIAS EC CHANGES OVERLAY (RECNT77).         
ECON0122 EQU   *                                                                
         CLI   RSTATRAF,C'B'       BIAS TRAFFIC SYSTEM?                         
         BE    ECON0160            YES                                          
         CLI   RSTATRAF,C'W'       BIAS TRAFFIC SYSTEM?                         
         BE    ECON0160            YES                                          
         MVI   10(R5),X'65'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     THE JDS/2000 OVERLAY (RECNT65).           
         TM    RSTAOPTB,X'20'      USE NEW EC CHANGES VERSION?                  
         BNO   ECON0124            NO                                           
         MVI   10(R5),X'78'        SET TRAFFIC SYSTEM TO LOAD                   
*                                    2000 EC CHANGES OVERLAY (RECNT78).         
ECON0124 EQU   *                                                                
         CLI   RSTATRAF,C'J'       JDS/2000 TRAFFIC SYSTEM?                     
         BE    ECON0160            YES                                          
         MVI   10(R5),X'6E'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     THE ENTERPRISE OVERLAY (RECNT6E).         
         TM    RSTAOPTB,X'20'      USE NEW EC CHANGES VERSION?                  
         BNO   ECON0126            NO                                           
         MVI   10(R5),X'7E'        SET TRAFFIC SYSTEM TO LOAD                   
*                                    ESG EC CHANGES OVERLAY (RECNT7E).          
ECON0126 EQU   *                                                                
         CLI   RSTATRAF,C'K'       ENTERPRISE TRAFFIC SYSTEM?                   
         BE    ECON0160            YES                                          
         CLI   RSTATRAF,C'H'       ENTERPRISE II TRAFFIC SYSTEM?                
         BE    ECON0160            YES                                          
         CLI   RSTATRAF,C'Z'       VSS (ENTERPRISE) TRAFFIC SYSTEM?             
         BE    ECON0160            YES                                          
*                                                                               
****<<<<                                                                        
         MVI   10(R5),X'5E'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     CBS/VSS ESG OVERLAY (RECNT5E)             
         TM    RSTAOPTB,X'20'      USE NEW EC CHANGES VERSION?                  
         BNO   ECON0128            NO                                           
         MVI   10(R5),X'5F'        SET TRAFFIC SYSTEM TO LOAD                   
*                                    CBS/VSS ESG CHGS O'LAY (RECNT5F)           
ECON0128 EQU   *                                                                
         CLI   RSTATRAF,C'X'       CBS/VSS (ESG) TRAFFIC SYSTEM?                
         BE    ECON0160            YES                                          
*                                                                               
****<<<<                                                                        
         MVI   10(R5),X'6F'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     THE COLUMBINE OVERLAY (RECNT6F).          
         TM    RSTAOPTB,X'20'      USE NEW EC CHANGES VERSION?                  
         BNO   ECON0130            NO                                           
         MVI   10(R5),X'76'        SET TRAFFIC SYSTEM TO LOAD                   
*                                  COLUMB EC CHANGES OVERLAY (RECNT76).         
ECON0130 EQU   *                                                                
         CLI   RSTATRAF,C'C'       COLUMBINE TRAFFIC SYSTEM?                    
         BE    ECON0160            YES                                          
         MVI   10(R5),X'6B'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     THE VCI OVERLAY (RECNT6B).                
         CLI   RSTATRAF,C'V'       VCI TRAFFIC SYSTEM?                          
         BE    ECON0160            YES                                          
         CLI   RSTATRAF,C'S'       VCI STAR II TRAFFIC SYSTEM?                  
         BE    ECON0160            YES                                          
*                                                                               
         MVI   10(R5),X'5B'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     THE VCI OVERLAY (RECNT5B).                
         CLI   RSTATRAF,C'T'       VCI/BDE TRAFFIC SYSTEM?                      
         BE    ECON0160            YES                                          
         CLI   RSTATRAF,C'U'       VCI/BDE STAR II TRAFFIC SYSTEM?              
         BE    ECON0160            YES                                          
*                                                                               
         MVI   10(R5),X'7F'        SET TRAFFIC SYSTEM TO LOAD                   
*                                     GENERIC TRAFFIC OVERLAY (RECNT7F)         
         CLI   RSTATRAF,C'O'       WIDE ORBIT TRAFFIC SYSTEM?                   
         BE    ECON0160            YES                                          
*                                                                               
         CLI   RSTATRAF,C'E'       WO / COLUMBINE WKSHT TRAFFIC SYSTEM?         
         BE    ECON0160            YES                                          
*                                                                               
         CLI   RSTATRAF,C'D'       WIDE ORBIT/VCI TRAFFIC SYSTEM?               
         BE    ECON0160            YES                                          
*                                                                               
         CLI   RSTATRAF,C'N'       WIDE ORBIT/ALTERNATE TRFC SYSTEM?            
         BE    ECON0160            YES                                          
*                                                                               
         CLI   RSTATRAF,C'L'       MARKETRON (NEW) TRAFFIC SYSTEM?              
         BE    ECON0160            YES                                          
*                                                                               
         CLI   RSTATRAF,C'I'       OSI       (NEW) TRAFFIC SYSTEM?              
         BE    ECON0160            YES                                          
*                                                                               
*   AT THIS POINT, NOT TV TRAFFIC - CHECK FOR RADIO COLUMBINE                   
*                                                                               
         CLI   RSTAKSTA+4,C'A'     AM STATION?                                  
         BE    ECON0140            YES                                          
         CLI   RSTAKSTA+4,C'F'     FM STATION?                                  
         BNE   ECON0150            NO  - NOT RADIO                              
ECON0140 EQU   *                                                                
         MVI   10(R5),X'6F'        SET TRAFFIC SYSTEM TO LOAD                   
         TM    RSTAOPTA,X'01'      COLUMBINE RADIO TRAFFIC?                     
         BO    ECON0160            YES                                          
*                                                                               
         DROP  R6                                                               
ECON0150 EQU   *                                                                
         DC    H'0'                UNRECOGNIZED TRAFFIC SYSTEM                  
*                                                                               
**********************************************************************          
*                                                                               
ECON0160 EQU   *                                                                
         BAS   R8,LOADDIS          GO TO THE TRAFFIC OVERLAY                    
         B     ECON0320            EXIT                                         
*                                                                               
MUSTBEEC EQU   394                 MUST BE AN EC ACTION CODE                    
MUSTBEE2 EQU   395                 MUST BE AN E2 ACTION CODE                    
E3NOGOOD EQU   907                 E3 RESTRICTED TO REP/STATION                 
NOTECSTA EQU   396                 NOT AN EC STATION                            
NOTASTAT EQU   404                 NOT STATION MAKING REQUEST                   
NOTATSTA EQU   412                 NOT SENT TO STATION BY REP                   
*                                                                               
ECON0200 EQU   *                                                                
         LA    R3,MUSTBEEC         ACTION CODE MUST BE EC                       
         B     ERROR                                                            
ECON0240 EQU   *                                                                
         LA    R3,MUSTBEE2         ACTION CODE MUST BE E2                       
         B     ERROR                                                            
ECON0280 EQU   *                                                                
         LA    R3,NOTECSTA         NOT AN EC STATION.                           
         B     ERROR                                                            
ECON0300 EQU   *                                                                
         LA    R3,NOTASTAT         NOT A STATION REQUEST.                       
         B     ERROR                                                            
ECON0310 EQU   *                                                                
         LA    R3,NOTATSTA         NOT SENT TO STATION.                         
         B     ERROR                                                            
ECON0320 EQU   *                                                                
         GOTO1 =A(ECSONNET),RR=TWARELO1                                         
         BAS   R8,DIS2             DISPLAY MESSAGE                              
         B     EXIT                                                             
ECON0330 EQU   *                                                                
         LA    R3,E3NOGOOD         E3 NOT PERMITTED                             
         B     ERROR                                                            
         EJECT                                                                  
**********************************************************************          
* SEND A WORKSHEET                                                              
**********************************************************************          
SEND     DS    0H                                                               
         GOTO1 =A(SEND00),RR=TWARELO1                                           
         B     EXXMOD                                                           
*                                                                               
*             CONFIRM A CONTRACT (CF)                                           
*                                                                               
CONF     DC    0H'0'                                                            
         BAS   R8,LOADSCRN                                                      
         TM    TWAPROST,X'20'                                                   
         BO    CONF10              SCREEN CHANGE MUST DISPLAY                   
         TM    CONCNUMH+4,X'20'                                                 
         BO    CONF20              NUMBER IS NOT CHANGED                        
         SPACE 1                                                                
CONF10   BAS   R8,CONDIS                                                        
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'                                                 
         LA    R2,CONCACTH                                                      
         FOUT  (R2),CONACT,4                                                    
         OI    1(R2),X'01'                                                      
*                                                                               
CONF20   DS    0H                                                               
         CLC   =C'CF*',CONACT      TAKEOVER CONFIRMATION                        
         BE    CONF50                                                           
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    CONF25                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBCON',0),RR=TWARELO1                
         B     CONF30                                                           
*                                                                               
CONF25   BAS   R8,LOADEDT                                                       
CONF30   MVI   INTTYPE,C'O'            INDICATE ORDER WORKSHEET                 
*                                                                               
*** DARE CHECK                                                                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        ONLY APPROVED DARE ORDER DO THIS             
         BAS   RE,GETEL                                                         
         BNE   CONF50                                                           
                                                                                
         USING RCONDREL,R6                                                      
*        TM    RCONDRFG,X'04'      SKIP DARE PROCESSING FOR EDI                 
*        BO    CONF50                                                           
         TM    RCONDRF2,X'01'      XML ORDER?                                   
         BZ    CONF33                                                           
         OC    RCONDRDA,RCONDRDA   APPROVED AT LEAST ONCE?                      
         BNZ   CONF34                                                           
*                                                                               
CONF33   DS    0H                                                               
         TM    RCONDRFG,X'40'                                                   
         BZ    CONF50                                                           
*                                                                               
CONF34   DS    0H                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CONF35                                                           
         TM    RCONDRF2,X'80'      SEND AGENCY CONFIRMATION FOR                 
         BO    CONF40              VARIOUS ORDERS IN LATER MODS AS WELL         
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BO    CONF40                                                           
         OC    RCONDRRV,RCONDRRV   ALLOW REVISION CONFIRMATION                  
         BNZ   CONF40                                                           
         DROP  R6                                                               
                                                                                
CONF35   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'        AND UN-CONFIRMED ORDERS ONLY                 
         BAS   RE,GETEL                                                         
         BNE   CONF40                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'20'      CONFIRMED PREVIOUSLY?                        
         BNZ   CONF50                                                           
         DROP  R6                                                               
                                                                                
CONF40   DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'62',0) SEND DARE CONFIRMATION AND DELETE           
*** DARE CHECK                        CORRESPONDING AGENCY ORDER/BUYS           
                                                                                
*- PROCESS BACKWARDS                                                            
*  CHECK FOR 3RD COPY BEFORE DOING GRAPHNET TEST                                
*  ON 'CF' ACTION, MUST CALL 60 OVLY WITH PASS=1 TO POINT                       
*     SEND ID TO REP IF GRAPHNET                                                
*                                                                               
CONF50   DS    0H                                                               
         MVI   TWACFFLG,X'80'      DEFAULT FLAG: PRINT CONFIRMATION             
         GOTO1 =A(CHKCFC),DMCB,(RC),RR=TWARELO1 CHECK FOR CFC RECORD            
*                                                                               
         CLC   =C'CF*',CONACT      TAKEOVER CONFIRMATION                        
         BE    CONF60                                                           
*                                                                               
         CLC   =C'CFX',CONACT      REP ONLY ACTION. ALL PAPERWORK, IF           
         BE    CONF60              ANY, GOES TO THE REP                         
*                                                                               
         MVI   SENDPASS,3          3RD PASS = TO DESTINATION ID.                
         GOTO1 VLOAD,DMCB,(X'60',0)                                             
*                                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET-NO WORKSHEET TO STATION             
         BO    CONF60                                                           
*                                                                               
         MVI   SENDPASS,1          1ST PASS = TO SENDER                         
         GOTO1 VLOAD,DMCB,(X'60',0)    WORKSHEET HEADLINES TO STATION           
                                                                                
*                                                                               
* CHECKING OF VARIOUS BIT SETTINGS FROM A WHOLE MESS OF RECORDS TO              
* DETERMINE WHAT IN THE WAY OF CONTRACTS, TURNAROUNDS, ETC, IS                  
* GENERATED IS DONE HERE                                                        
*                                                                               
* WARNING: ROUTINE 'TKOV' FOR ACTION 'DONE' BRANCHES IN AT THIS POINT           
* TO GENERATE CONF PAPERWORK. KEEP THIS IN MIND WHEN MAKING CHANGES!!           
*                                                                               
CONF60   DS    0H                                                               
*                                                                               
         GOTO1 =A(CONFLIST),DMCB,(RC),RR=TWARELO1 STRING OF SETTINGS            
*                                     RELOCATED FOR ADDRESSABILITY              
*                                                                               
*                                                                               
CONF100  DS    0H                  ** GENERATE REP ORDER WORKSHEET **           
         MVI   INTTYPE,C'O'            INDICATE ORDER WORKSHEET                 
         MVI   SENDPASS,2          2ND PASS = JUST POINT SEND ID TO REP         
         GOTO1 VLOAD,DMCB,(X'60',0)  GENS WORKSHEET ONLY WHEN NECESSARY         
*                                                                               
CONF120  DS    0H                  ** PRINT CONTRACTS **                        
         TM    TWACFFLG,X'80'      COPY FOR REP?                                
         BO    *+12                                                             
         TM    TWACFFLG,X'10'      COPY FOR AGENCY?                             
         BZ    CONF140                                                          
         GOTO1 VLOAD,DMCB,(X'17',0)                                             
*                                                                               
CONF140  DS    0H                  ** GENERATE TURNAROUND REQUESTS **           
         TM    TWACFFLG,X'40'                                                   
         BO    CONF145                                                          
         CLC   =C'DONE',CONACT                                                  
         BE    CONF150                                                          
         B     CONF200                                                          
*                                                                               
CONF145  DS    0H                                                               
         OI    TAREQ,1                                                          
         GOTO1 VCKTAREQ                                                         
         CLC   =C'DONE',CONACT                                                  
         BE    CONF150                                                          
         CLC   =C'CFX',CONACT      EXCEPT FOR CFX CONTRACTS                     
         BE    CONF150                                                          
         CLC   =C'CF*',CONACT      TAKEOVER CONFIRMATION                        
         BE    CONF150                                                          
         CLI   TWAACCS,C'$'        STATION                                      
         BE    CONF200                                                          
         CLC   =C'CF ',CONACT                                                   
         BE    CONF150                                                          
         TM    PROFILES+CNTPORGB,CNTPORGA CONTRACT PROFILE 23 SET               
         BO    CONF200             SKIP MESSAGE                                 
*                                                                               
CONF150  DS    0H                                                               
         BAS   R8,DIS2                                                          
*                                                                               
CONF200  FOUT  CONCACTH,MYSPACES,15                                             
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONCACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
* 'DONE' CONFIRMS A TAKEOVER CONTRACT                                           
         SPACE 1                                                                
TKOV     DC    0H'0'                                                            
         BAS   R8,LOADSCRN                                                      
         TM    TWAPROST,X'20'                                                   
         BO    TKOV10              SCREEN CHANGE MUST DISPLAY                   
         TM    CONCNUMH+4,X'20'                                                 
         BO    TKOV20              NUMBER IS NOT CHANGED                        
         SPACE 1                                                                
TKOV10   BAS   R8,CONDIS                                                        
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'                                                 
         LA    R2,CONCACTH                                                      
         FOUT  (R2),CONACT,4                                                    
         OI    1(R2),X'01'                                                      
         B     EXIT                                                             
         SPACE 1                                                                
TKOV20   DS    0H                                                               
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    TKOV30                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMTKOV',0),RR=TWARELO1                
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBCON',0),RR=TWARELO1                
         B     TKOV40                                                           
                                                                                
TKOV30   DS    0H                                                               
         BAS   R8,LOADEDT          MARK CONTRACT CONFIRMED                      
                                                                                
TKOV40   DS    0H                                                               
         MVI   TWACFFLG,X'80'      DEFAULT FLAG: PRINT CONFIRMATION             
         B     CONF60              GEN CONFIRMATION PAPERWORK                   
         EJECT                                                                  
* AUTO SWITCH TO PFM                                                            
PFMC     DS    0H                  IF INVOKED FROM CONTRACT ACTION FLD          
         CLI   TWAOFFC,C'*'        DDS TERMINALS ONLY                           
         BNE   ERROR                                                            
         FOUT  CONCACTH,MYSPACES,15                                             
         OC    TWAKADDR,TWAKADDR                                                
         BZ    PFMX                                                             
         XC    WORK3,WORK3                                                      
         LA    R1,WORK3                                                         
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMFIL(6),=C'REPFIL'                                           
         MVC   GLPFMDA,TWAKADDR                                                 
         B     PFM10                                                            
                                                                                
PFMB     DS    0H                  IF INVOKED FROM BUY ACTION FIELD             
         CLI   TWAOFFC,C'*'        DDS TERMINALS ONLY                           
         BNE   ERROR                                                            
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OC    TWABADDR,TWABADDR                                                
         BZ    PFMX                                                             
         XC    WORK3,WORK3                                                      
         LA    R1,WORK3                                                         
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMFIL(6),=C'REPFIL'                                           
         MVC   GLPFMDA,TWABADDR                                                 
                                                                                
PFM10    DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK3,54,GLPFMCDQ                             
         DROP  R1                                                               
*                                                                               
         XC    WORK3,WORK3                                                      
         LA    R1,WORK3                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'                                                 
         MVC   GLVXFRPR,=C'CON'                                                 
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
***>>>   OI    GLVXFLG1,GLV1GOTO                                                
         OI    GLVXFLG1,GLV1SEPS                                                
                                                                                
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK3,14,GLVXCTL                              
         DROP  R1                                                               
PFMX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
LAST     GOTO1 =A(LAST00),RR=TWARELO1                                           
         B     EXIT                                                             
*                                                                               
*                                                                               
* DISPLAY COMBINED SAR/SPL SCREEN, TRANSFER TO PROCESSING ROUTINE,              
*                                                                               
SASP     DS    0H                                                               
         LA    R3,368              INVALID ACTION CODE MESSAGE                  
         LA    R2,CONCACTH                                                      
*        TM    PROFILES+CNTSASPB,CNTSASPA DOES CLIENT HAVE ACCESS?              
*        BNO   ERROR               NO  - EXIT WITH MESSAGE                      
         CLI   TWASCRN,X'F7'       SCREEN ALREADY UP?                           
         BNE   SASP0010            NO  - DISPLAY AND LOAD IT                    
         TM    CONCNUMH+4,X'20'    YES - CONTRACT PREVIOUSLY VALID?             
         BNO   SASP0020            NO  - CONTRACT CHANGED                       
         CLC   =C'SASP',CONCACT    YES - ANOTHER DISPLAY REQUEST?               
         BE    SASP0020            YES - DO IT AGAIN                            
         CLC   =C'PD',CONCACT      YES - ANOTHER DISPLAY REQUEST?               
         BE    SASP0020            YES - DO IT AGAIN                            
         B     SASP0030            NO  - REQUEST FOR CHANGE                     
*                                                                               
SASP0010 EQU   *                                                                
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         MVI   TWASCRN,X'F7'                                                    
         BAS   R8,FULLSCRN                                                      
SASP0020 EQU   *                                                                
         BAS   R8,LOADDIS          LOAD NEWLY DISPLAYED SCREEN                  
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'    TURN ON PREVALID BIT                         
         B     EXIT                                                             
*                                                                               
*  COMBINED SAR/SPL SCREEN OVERLAYS ENTIRE TWENTY-FOUR LINES                    
*                                                                               
SASP0030 EQU   *                                                                
         BAS   R8,LOADEDT          EDIT/UPDATE FROM SAR/SPL SCRN                
         BAS   R8,DIS2                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* THIS PROCEDURE READS THE PROFILE RECORDS(IF ANY) SET                          
* UP FOR A REP - TO SEE IF THEY HAVE ACCESS TO THE GRP AND CPP                  
* TOTAL FIELDS AND TO THE BUDGET FIELD - IF YES WORK(1) WILL BE A 'Y'           
         SPACE                                                                  
CKALLOWD NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'R0CO'                                              
         MVC   WORK+20(2),REPALPHA                                              
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
EXXIT    XIT1                                                                   
         EJECT                                                                  
*              TURN OFF VALID BITS FOR ALL FIELDS                               
         SPACE 1                                                                
INVAL    DC    0H'0'                                                            
         LA    RE,CONAGYH          START                                        
         L     RF,ALSTFH           END                                          
         AR    RF,RA                                                            
         SPACE 1                                                                
INVAL2   CR    RE,RF                                                            
         BER   R8                                                               
         NI    4(RE),X'DF'                                                      
         ZIC   R1,0(RE)                                                         
         AR    RE,R1                                                            
         LTR   R1,R1               SO WE WON'T LOOP                             
         BZR   R8                                                               
         B     INVAL2                                                           
         EJECT                                                                  
*                                                                               
*          DATA SET RECNT02    AT LEVEL 078 AS OF 08/03/90                      
*                                                                               
BUY      DS    0H                                                               
         GOTO1 =A(AVNHOOK),RR=TWARELO1 HANDLE AVN REQUESTS                      
*                                                                               
* TEMPORARY CODING TO ALLOW NEW EXPANDED BUY SCREEN TO BE INVOKED               
* USING BUM, DIM AND CHM                                                        
*                                                                               
         TM    TWAFLAGS,TWAFLAV2   AVN LUID?                                    
         BO    BUYA0002            EXPANDED SCREEN NOT ALLOWED                  
*                                                                               
         NI    TWAMGFLG,X'FF'-X'20'                                             
         TM    PROFILES+CNTXBUYB,CNTXBUYA                                       
         BO    BUYA0002                                                         
*                                                                               
         CLC   =C'BUM',BUYACT                                                   
         BNE   *+14                                                             
         MVC   BUYACT,=C'BUY'                                                   
         B     BUYA0001                                                         
         CLC   =C'DIM',BUYACT                                                   
         BNE   *+14                                                             
         MVC   BUYACT,=C'DIS'                                                   
         B     BUYA0001                                                         
         CLC   =C'CHM',BUYACT                                                   
         BNE   *+14                                                             
         MVC   BUYACT,=C'CHA'                                                   
         B     BUYA0001                                                         
*                                                                               
         CLC   =C'BUY',BUYACT                                                   
         BE    BUYA0000                                                         
         CLC   =C'CHA',BUYACT                                                   
         BE    BUYA0000                                                         
         CLC   =C'DEL',BUYACT                                                   
         BE    BUYA0000                                                         
         CLC   =C'CAN',BUYACT                                                   
         BNE   BUYA0002                                                         
*                                                                               
BUYA0000 DS    0H                  FORCE PROFILE TO USE EXPANDED SCREEN         
         CLI   TWASCRN,X'DE'       EXPANDED BUY SCREEN IN USE FROM LAST         
         BNE   BUYA0002            TRANSACTION?                                 
         TM    TWAFLAGS,TWAFLAVN   EXCEPT IF STILL AVN ACTION                   
         BO    BUYA0002                                                         
*                                                                               
BUYA0001 DS    0H                  FORCE PROFILE TO USE EXPANDED SCREEN         
         OI    PROFILES+CNTXBUYB,CNTXBUYA                                       
         OI    TWAMGFLG,X'20'      SO WE KNOW WHO TURNED ON THE FLAG            
*                                                                               
BUYA0002 DS    0H                                                               
         CLC   BUYACT,MYSPACES     EXIT IF NO BUY ACTION                        
         BE    EXIT                                                             
         CLC   =C'ACC-',CONBUY     ACCOUNTING CONTRACT?                         
         BE    BUYA0030            YES - DON'T FORCE SAR                        
*                                  SKIP SAR IF TAKEOVER CONTRACT                
         ST    RF,FULL                                                          
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=TWARELO1                                
         L     RF,FULL                                                          
         BZ    BUYA0030                                                         
                                                                                
         CLC   =C'FD',BUYACT       IF FORECAST ACTIONS, PROCESS                 
         BE    BUYA0030                                                         
         CLC   =C'FC',BUYACT                                                    
         BE    BUYA0030                                                         
         LA    R3,BACERR                                                        
         OC    BUYACT,BUYACT       IF NO BUY ACTION, ERROR EXIT                 
         BZ    ERROR                                                            
         LA    R6,RCONREC          YES - HAS SAR DATA BEEN ENTERED?             
         MVI   ELCODE,X'12'        LOOK FOR SAR ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   BUYA0003            NOT FOUND                                    
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH                                                
         BNE   BUYA0003                                                         
         TM    RSARXFLG,X'18'      IF FORECAST CONTRACT, ALLOW ONLY             
         BZ    BUYA0003            CERTAIN BUY DISPLAY ACTIONS                  
         DROP  R6                  ELSE - CONTINUE PROCESSING                   
*                                                                               
* CHECK IF FORECAST USER. IF CONTRACT IS FORECAST, ONLY HIS IS ALLOWED          
* FOR BUY ACTION                                                                
         TM    PROFILES+CNT4CASB,CNT4CASA                                       
         BZ    BUYA0010                                                         
         CLC   =C'HIS',BUYACT                                                   
         BE    BUYA0030                                                         
         LA    R3,296              INVALID ACTION FOR FORECAST K                
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         B     ERROR                                                            
*                                                                               
BUYA0003 DS    0H                                                               
         TM    PROFILES+CNTMEDTB,CNTMEDTA                                       
*                                  SAR REQUIRED?                                
         BNO   BUYA0030            NO                                           
*                                                                               
         CLC   =C'GEN ',RCONKADV   SKIP SAR REQUIREMENT FOR GEN/ZZ              
         BNE   BUYA0004                                                         
         CLC   =C'ZZ',RCONCTGY                                                  
         BE    BUYA0030                                                         
*                                                                               
BUYA0004 DS    0H                                                               
*        CLC   =C'SZ',REPALPHA                                                  
*        BE    BUYA0005                                                         
*        CLC   =C'CQ',REPALPHA                                                  
*        BNE   BUYA0008                                                         
BUYA0005 DS    0H                                                               
*        LA    R6,RCONREC          SPECIAL FOR SELTEL TAKEOVER                  
*        MVI   ELCODE,X'1C'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   BUYA0008            SKIP PENDING CHECK                           
*        USING RCONTKEL,R6                                                      
*        CLC   =C'AM',RCONTKRP                                                  
*        BE    BUYA0030                                                         
*        DROP  R6                                                               
*                                                                               
BUYA0008 DS    0H                                                               
         LA    R6,RCONREC          YES - HAS SAR DATA BEEN ENTERED?             
         MVI   ELCODE,X'12'        LOOK FOR SAR ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    BUYA0030            NOT FOUND                                    
*                                                                               
*   SAR DATA NOT FOUND.  CHECK: IF SPL FOUND, PERMIT BUYACTS OF                 
*       DIS,DSM,DRT,RTS.                                                        
*                                                                               
         LA    R6,RCONREC          YES - HAS SPL DATA BEEN ENTERED?             
         MVI   ELCODE,X'06'        LOOK FOR SPL ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   BUYA0020            NOT FOUND - FORCE ACTION TO SASP             
                                                                                
BUYA0010 DS    0H                                                               
         CLC   BUYACT,=C'DIS'      SPL FOUND - ALLOWABLE ACTION?                
         BE    BUYA0030            YES                                          
         CLC   BUYACT,=C'DSM'                                                   
         BE    BUYA0030                                                         
         CLC   BUYACT,=C'DRT'                                                   
         BE    BUYA0030                                                         
         CLC   BUYACT,=C'RTS'                                                   
         BE    BUYA0030                                                         
         CLC   BUYACT,=C'HIS'                                                   
         BE    BUYA0030                                                         
BUYA0020 EQU   *                                                                
         LA    RF,*                                                             
         A     RF,=A(SASPAC2-(*-4))                                             
         B     CONA0230            TRANSFER TO CONACTS ROUTINE                  
BUYA0030 L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         SPACE 1                                                                
         LA    R3,BACERR                                                        
*                                                                               
*   STATION CAN ONLY DO BUY ACTIONS DISPLAY, DISPLAY MULTIPLE,                  
*   CHANGE, ORDER, UNI ,CX/DX AND HISTORY                                       
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   BUYA0035                                                         
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBBUY1',0),RR=TWARELO1                 
         BNZ   ERROR               INVALID ACTION FOUND                         
         SPACE 1                                                                
BUYA0035 DS    0H                  IF ADV=GEN AND CAT=ZZ ALLOW ONLY             
         CLC   =C'GEN ',RCONKADV   SAR/EPL/BOP/SPL/SASP-C/PD-C                  
         BNE   BUYA0040                                                         
         CLC   =C'ZZ',RCONCTGY                                                  
         BNE   BUYA0040                                                         
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBBUY2',0),RR=TWARELO1                 
         BNZ   ERROR               INVALID ACTION FOUND                         
                                                                                
***********************************************************************         
* IF CONTRACT IS A DARE ORDER (X'1D' ELEMENT EXISTS) AND CONTRACT HAS           
* NOT BEEN CONFIRMED, BUY ACTIONS 'BUY' AND 'MBI' ARE NOT ALLOWED               
***********************************************************************         
BUYA0040 DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QIFDARE',0),RR=TWARELO1                  
*        BNZ   ERROR                                                            
         EJECT                                                                  
***********************************************************************         
* MON RULES: ONCE MON IS USED, ONLY BUY ACTION ALLOWED AFTER IS MON             
*            AND DSM.  IF ANY OTHER BUY ACTION HAS BEEN USED, MON IS            
*            NOT ALLOWED.  IF THERE ARE 03 ELEMS IN THE CONREC AND              
*            WERE NEVER ANY BUYS, THEN ACTN MON HAS BEEN USED.  IF              
*            IF ARE NO 03'S, THEN ANY BUY ACTION IS VALID.  THESE               
*            RESTRICTIONS ONLY APPLY TO ACTIONS WHICH AFFECT BUCKETS.           
*                                                                               
* NOTE: EXCEPTIONS HAVE EXPANDED TO VARIOUS FLAVORS OF BUCKETS                  
*                                                                               
***********************************************************************         
BUYA0045 DS    0H                                                               
         TM    RCONMODR+1,X'20'    HAS A MON BEEN DONE?                         
         BZ    BUYA0050            NO                                           
         LA    R3,253              ERR - NOT ALLOWED AFTER MON DONE             
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBBUY3',0),RR=TWARELO1                 
         BNZ   ERROR               INVALID ACTION FOUND                         
         B     BUYA0060                                                         
*                                                                               
* MON ACTION NOT ALLOWED FOR BUCKETS AND INVOICE BUCKETS OF:                    
* REGULAR, TRADE, ALTERNATE CALENDER AND ALTERNATE TRADE CALENDER               
*                                                                               
BUYA0050 DS    0H                                                               
         CLC   =C'MON',BUYACT      ARE WE DOING A MON?                          
         BNE   BUYA0060                                                         
*                                                                               
BUYA0052 DS    0H                                                               
         LA    R3,BUCTYPES                                                      
*                                                                               
BUYA0053 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVC   ELCODE,0(R3)                                                     
         BAS   RE,GETEL                                                         
         BNE   BUYA0055                                                         
         LA    R3,254              ERR - NOT ALLOWED IF BUYS                    
         B     ERROR                                                            
*                                                                               
BUYA0055 DS    0H                                                               
         AHI   R3,1                                                             
         CLI   0(R3),X'FF'                                                      
         BE    BUYA0060                                                         
         B     BUYA0053                                                         
*                                                                               
BUCTYPES DC    X'03',X'04',X'53',X'54',X'63',X'64',X'83',X'84'                  
         DC    X'FF'                                                            
*                                                                               
BUYA0060 DS    0H                                                               
         XC    FULL,FULL           SET FULL AS FLAG                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        LOOK FOR COMBO CONTROL ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   BUYA0070            NOT COMBO ORDER -                            
         ST    R6,FULL             COMBO ORDER - SET FLAG                       
*                                                                               
BUYA0070 EQU   *                                                                
         LA    RF,*                                                             
         A     RF,=A(BUYACTS-(*-4))                                             
*                                                                               
         TM    PROFILES+CNTXBUYB,CNTXBUYA                                       
         BZ    BUYA0072            CHECK EXTEND BUY SCREEN IN USE               
         CLI   TWACOMBO,0          COMBO CONTRACT NOT SUPPORTED                 
         BNE   BUYA0072                                                         
         LA    RF,*                                                             
         A     RF,=A(BUYACTSX-(*-4))                                            
*                                                                               
BUYA0072 EQU   *                                                                
         CLC   =C'SAS',BUYACT      SASP ACTION?                                 
         BE    BUYA0075            YES - SKIP TABLE LOOKUP                      
         CLC   =C'PD',BUYACT       SASP ACTION?                                 
         BE    BUYA0075                                                         
         CLC   =C'CD',BUYACT       COMPETITIVE SPL ACTION?                      
         BE    BUYA0093                                                         
                                                                                
         CLC   =C'FD',BUYACT       FD/FC ACTION?                                
         BE    BUYA0073                                                         
         CLC   =C'FC',BUYACT       FD/FC ACTION?                                
         BE    BUYA0073            YES - GOTO FORECAST                          
                                                                                
         OC    BUYACT,BUYACT       SASP/FORECAST DON'T HAVE BUY ACTION          
         BZ    ERROR               IF WE GET HERE, WE SHOULD ERROR EXIT         
                                                                                
         B     BUYA0080            ELSE - CONTINUE NORMAL TABLE CHECK           
*                                                                               
BUYA0073 DS    0H                                                               
         LA    RF,*                                                             
         A     RF,=A(FCSTCHAF-(*-4))                                            
         CLC   =C'FC',BUYACT                                                    
         BE    CONA0230                                                         
         LA    RF,*                                                             
         A     RF,=A(FCSTDISF-(*-4))                                            
         B     CONA0230            TRANSFER TO CONACTS ROUTINE                  
*                                                                               
BUYA0075 EQU   *                                                                
         TM    PROFILES+CNTSASPB,CNTSASPA                                       
*                                  SASP USER?                                   
         BO    BUYA0090            YES - SKIP TABLE LOOKUP AND                  
*                                     FORCE ACTION TO SASP                      
*                                                                               
BUYA0080 CLI   0(RF),X'FF'         END OF TABLE REACHED?                        
         BE    BUYA0150            YES - ACTION NOT FOUND - ERROR               
         CLC   BUYACT,0(RF)        BUY ACTION FOUND IN TABLE?                   
         BNE   BUYA0120            NO  - KEEP LOOKING                           
*                                                                               
*   IF SASP USER, AND 'FORCE SASP' IS SET, FORCE ACTION INTO                    
*      CONACT LOGIC, WITH POINTER TO SASP REQUEST LINE.                         
*                                                                               
         TM    PROFILES+CNTSASPB,CNTSASPA                                       
*                                  SASP USER?                                   
         BNO   BUYA0100            NO                                           
         TM    9(RF),X'02'         FORCE SASP?                                  
         BNO   BUYA0100            NO                                           
*                                  SKIP SASP IF TAKEOVER CONTRACT               
         ST    RF,FULL                                                          
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=TWARELO1                                
         L     RF,FULL                                                          
         BZ    BUYA0100                                                         
BUYA0090 EQU   *                                                                
         L     RF,=A(SASPACT)      YES - FORCE ACTION TO SASP                   
         B     BUYA0095                                                         
BUYA0093 EQU   *                                                                
         L     RF,=A(COMPACT)      YES - FORCE ACTION TO CD (SPL)               
*                                                                               
BUYA0095 EQU   *                                                                
         CNOP  0,4                 ESTABLISH ADDRESSABILITY FOR TABLE           
         B     *+8                                                              
         DC    A(*)                                                             
         LA    RE,*-4                                                           
         S     RE,*-8                                                           
         AR    RF,RE                                                            
         B     CONA0230            TRANSFER TO CONACTS ROUTINE                  
*                                                                               
* COMBO USE/NON-USE:  WHEN ACTION IS FOUND IN BUY-TABLE, THE                    
*    FLAG FOR COMBO USE/NON-USE IS CHECKED FIRST.  IF SET ON, AN                
*    ALTERNATE OVERLAY/SCREEN EXISTS FOR THIS ACTION CODE WHICH                 
*    IS TO BE CALLED IF - AND ONLY IF - THE CONTRACT BEING PROCESSED            
*    IS A COMBO ORDER, INDICATED BY PRESENCE OF AN X'17' ELEMENT.               
*                                                                               
BUYA0100 EQU   *                                                                
         TM    9(RF),X'10'         TEST FOR COMBO USE/NON-USE?                  
         BNO   BUYA0130            NO  - LOAD AND GO ON 'FOUND'                 
*                                                                               
         OC    FULL,FULL           YES - ANY VALUE IN FULL?                     
         BNZ   BUYA0110            YES - CHECK FOR COMBO USE                    
*                                                                               
*   *** NOT A COMBO ORDER ***                                                   
*                                                                               
         TM    9(RF),X'01'         COMBO SCREEN/OVERLAY?                        
         BO    BUYA0120            YES - KEEP LOOKING                           
         B     BUYA0130            NO  - LOAD AND GO                            
*                                                                               
*   *** YES A COMBO ORDER ***                                                   
*                                                                               
BUYA0110 EQU   *                                                                
         TM    9(RF),X'01'         COMBO SCREEN/OVERLAY?                        
         BO    BUYA0130            YES - LOAD AND GO                            
*                                  NO  - KEEP LOOKING                           
BUYA0120 EQU   *                                                                
         LA    RF,L'BUYACTS(RF)                                                 
         B     BUYA0080                                                         
         SPACE 1                                                                
BUYA0130 DS    0H                                                               
         CLI   10(RF),X'36'        BUY ACTION = COMBO DSM?                      
         BE    BUYA0140            YES                                          
         CLI   10(RF),X'26'        BUY ACTION = COMBO DSM?                      
         BE    BUYA0140            YES                                          
*                                                                               
*   NATURE OF TOTALS IN DSM/COMBO REQUIRES THAT THESE TWA FIELDS                
*        BE INITIALIZED EXTERNAL TO THE T80236 MODULE.                          
*                                                                               
         XC    TWALSTTO,TWALSTTO   NO  - RESET TOTAL INDICATORS                 
*                                     FOR DSM COMBO DISPLAY                     
         XC    TWATTLAR,TWATTLAR                                                
*                                                                               
BUYA0140 EQU   *                                                                
         LR    R5,RF                                                            
*                                                                               
         LR    R0,RF               SAVE RF                                      
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         XC    WORK,WORK           PAR APPLICATION SECURITY CHECK               
         LA    R1,WORK                                                          
         USING ABLOCK,R1                                                        
         MVC   ABPGM,=C'CON'                                                    
         MVC   ABREC,=CL8'BUYACT'                                               
         MVC   ABACT(4),0(R5)                                                   
         OC    ABACT,MYSPACES                                                   
         DROP  R1                                                               
         L     R1,AFACILS                                                       
         L     R3,0(R1)            ATIO                                         
         GOTO1 (RFCKASEC,VREPFACS),DMCB,WORK,CONMSGH,(R3),DUB,0                 
         BE    BUYA0143            OK                                           
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         OI    6(R2),X'80'+X'40'                                                
         L     RD,BASERD           INVALID - GET ALL THE WAY OUT                
         B     EXXMOD                                                           
*                                                                               
BUYA0143 DS    0H                                                               
         LR    RF,R0               RESTORE RF                                   
         TM    TWAPROST,X'02'      EXTENDED BUY SCREEN IN USE?                  
         BZ    BUYA0145                                                         
         TM    9(R5),X'04'         NEXT ACTION USE SAME SCREEN?                 
         BO    BUYA0145                                                         
         BAS   R8,FFDIS            NO,RELOAD BASE AND RE-DISPLAY HEADER         
         BAS   R8,CONDIS                                                        
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
*                                                                               
BUYA0145 EQU   *                                                                
         L     RF,4(R5)                                                         
         RELOC                                                                  
         AR    RF,RE                                                            
         BR    RF                  ROUTINE FOR THIS ACTION                      
         SPACE 1                                                                
BUYA0150 MVI   TWASTAT,0                                                        
         B     ERROR                                                            
         EJECT                                                                  
***********************************************************************         
* NOTE THAT WE CANNOT ADDRESS FIELDS IN THE BUY SCREEN DIRECTLY SINCE           
* THE INTRODUCTION OF THE EXTENDED BUY SCREEN (RECNTDE). EACH FIELD             
* IN THE EXTENDED BUY SCREEN SHOULD HAVE THE EXACT OFFSETS AS ITS               
* CORRESPONDING FIELD IN RECNTFE. THIS WAS DONE SO THERE WOULD BE               
* MINIMAL AMOUNT OF RECODING FOR THE NEW SCREEN. TO ADDRESS INDIVIDUAL          
* FIELDS, CALULATE THE OFFSET FROM ABUYFH WHICH SHOULD HAVE THE ADDRESS         
* OF THE 'BUY ACT' FIELD.                                                       
*                                                                               
* FOR EXAMPLE, TO POINT R2 TO BUYDAYSH:                                         
*                                                                               
*       L     R2,ABUYFH                                                         
*       AR    R2,RA                                                             
*       AHI   R2,BUYDAYSH-CONBACTH                                              
*                                                                               
***********************************************************************         
*              ACTION IS BUY                                                    
         SPACE 1                                                                
BUYB     DC    0H'0'                                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUYB10              NOT FOUND                                    
DARED    USING RCONDREL,R6                                                      
         TM    DARED.RCONDRF2,X'01' XML?                                        
         BZ    BUYB10                                                           
         TM    DARED.RCONDRFG,X'80' LINKED?                                     
         BZ    BUYB10                                                           
         TM    DARED.RCONDRFG,X'40' OPENED?                                     
         BNZ   BUYB10                                                           
         LA    R3,1022             LINKED XML ORDER NOT OPENED                  
         B     ERROR                                                            
         DROP  DARED                                                            
*                                                                               
BUYB10   EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        GET RANDOM FLAG ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   BUYB20              NOT FOUND                                    
RFLG     USING RCONRFEL,R6                                                      
         TM    RFLG.RCONRF1,X'40'  PROPOSER ORDER?                              
         BNO   BUYB20              NO  - DON'T CHECK CUTOFF DATE                
         DROP  RFLG                                                             
*                                                                               
         TM    RCONMODR,X'10'      BUYLINE ADDED?                               
         BO    BUYB20              YES - CONSIDER A REGULAR ORDER,              
*                                     PERMIT ADDITIONAL BUYS                    
         GOTO1 =A(STAPROCK),DMCB,(RC),RR=TWARELO1                               
         BZ    BUYB20              BUYS PERMITTED                               
         LA    R3,1007             SET 'PROPOSER ORDER: NO BUYS' ERROR          
         B     ERROR                                                            
*                                                                               
BUYB20   EQU   *                                                                
*                                                                               
         CLC   TWASCRN,8(R5)                                                    
         BE    BUYB70                                                           
*                                                                               
BUYB30   DS    0H                                                               
         MVC   BYTE,8(R5)                                                       
         BAS   R8,NEWSCRN          LOAD BUY SCREEN                              
*                                                                               
         TM    TWAPROST,X'02'      EXTENDED BY SCREEN IN USE?                   
         BZ    BUYB50              YES, NEED TO DISPLAY CONTRACT HEADER         
**                                                                              
*   ALSO NEED TO TURN ON BUYLINE CODE FIELD, IF PROFILED FOR IT                 
*                                                                               
         TM    PROFILES+CNTFOXSB,CNTFOXSA                                       
*                                  BUYLINE CODING IN USE?                       
         BNO   BUYB40              NO                                           
         L     RF,ABUYFH                                                        
         AR    RF,RA                                                            
         AHI   RF,BUYBCODH-CONBACTH                                             
         NI    1(RF),X'FF'-X'0C'                                                
*                                  YES - DISPLAY LABEL FIELD                    
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         L     RF,ABUYFH                                                        
         AR    RF,RA                                                            
         AHI   RF,BUYSCODH-CONBACTH                                             
         NI    1(RF),X'FF'-X'FC'                                                
*                                  DISPLAY CODE FIELD, PERMIT I/P               
         OI    6(RF),X'80'         TRANSMIT FIELD                               
BUYB40   EQU   *                                                                
**                                                                              
         GOTO1 VLOAD,DMCB,(X'2E',0),=C'HEAD'                                    
*                                                                               
BUYB50   DS    0H                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(3,R2),=C'BUY'                                                  
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         TM    TWAFLAGS,TWABUYFQ                                                
         BZ    *+8                                                              
         MVI   11(R2),C'F'         BUY ACTION IS BUYF                           
*                                                                               
         TM    TWAFLAGS,TWAFLAVN                                                
         BZ    *+8                                                              
         MVI   10(R2),C'#'         BUY ACTION IS BU#                            
*                                                                               
BUYB60   DS    0H                                                               
***      TM    TWAFLAGS,TWABUYTQ                                                
***      BZ    *+8                                                              
***      MVI   11(R2),C'*'         BUY ACTION IS BUYT                           
*                                                                               
         AHI   R2,BUYDAYSH-CONBACTH                                             
         BAS   R8,DIS1                                                          
         B     BUYBX                                                            
         SPACE 1                                                                
BUYB70   DS    0H                                                               
         CLI   TWACOMBO,0          SPECIAL IF COMBO                             
         BE    BUYB80                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBBUY',0),RR=TWARELO1                
         B     BUYB100                                                          
*                                                                               
BUYB80   DS    0H                                                               
         NI    TWAMGFLG,X'FF'-X'40'                                             
*                                                                               
         TM    TWAMGFLG,X'20'      RESET EXPANDED BUY SCREEN IN USE             
         BZ    *+8                 FOR THIS TRANSACTION ONLY                    
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
*                                                                               
BUYB90   DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
*                                  DISPLAY NEW BUY NUMBER                       
         LA    R8,CONBNUMH-CONBACTH(R2)                                         
         MVC   8(L'CONBNUM,R8),MYSPACES                                         
         EDIT  (1,RBUYKLIN),(3,8(R8)),ALIGN=LEFT                                
         OI    6(R8),X'80'                                                      
*                                                                               
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBDIS',0),RR=TWARELO1                
         BAS   R8,BUYDIS                                                        
*                                                                               
         TM    TWAMGFLG,X'40'      GO MY OWN BUCKET UPDATE?                     
         BO    BUYB100                                                          
         BAS   R8,BUCKUP                                                        
*                                                                               
BUYB100  DS    0H                                                               
         BAS   R8,DIS2                                                          
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 VCKTAREQ                                                         
*                                                                               
         OI    CONBNUMH-CONBACTH+4(R2),X'20'                                    
*                                                                               
BUYBX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
TMPC     DS    0H                  TMP BUYACTION HANDLING CALL                  
         GOTO1 =A(TMP00),RR=TWARELO1                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SBUY     DS    0H                  SPORTS BUY HANDLING CALL                     
         GOTO1 =A(SBUY00),RR=TWARELO1                                           
         B     EXIT                                                             
         EJECT                                                                  
*              DELETE BUY                                                       
         SPACE 1                                                                
*&&DO                                                                           
DELB     DC    0H'0'                                                            
         CLC   TWASCRN,8(R5)                                                    
         BNE   DELB10                                                           
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         TM    CONBNUMH-CONBACTH+4(R2),X'20'                                    
         BO    DELB20                                                           
*                                                                               
DELB10   DS    0H                                                               
         MVC   BYTE,8(R5)                                                       
         BAS   R8,NEWSCRN                                                       
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         NI    CONBNUMH-CONBACTH+4(R2),X'FF'-X'20'                              
*                                                                               
         BAS   R8,BUYDIS                                                        
         BAS   R8,DIS1                                                          
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OI    6(R2),X'80'         XMIT                                         
         OI    CONBNUMH-CONBACTH+4(R2),X'20'                                    
         B     DELBX                                                            
*                                                                               
DELB20   DS    0H                                                               
*                                                                               
         TM    TWAMGFLG,X'20'      RESET EXPANDED BUY SCREEN IN USE             
         BZ    *+8                 FOR THIS TRANSACTION ONLY                    
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
*                                                                               
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    DELB30                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBBUY',0),RR=TWARELO1                
         B     DELB40                                                           
*                                                                               
DELB30   BAS   R8,LOADEDT                                                       
         BAS   R8,BUCKUP                                                        
*                                                                               
DELB40   BAS   R8,DIS2                                                          
         GOTO1 VCKTAREQ                                                         
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OI    6(R2),X'80'                                                      
         NI    CONBNUMH-CONBACTH+4(R2),X'FF'-X'20'                              
*                                                                               
DELBX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
*              CHANGE BUY                                                       
         SPACE 1                                                                
CHAB     DC    0H'0'                                                            
         GOTO1 =A(RDBUY),RR=TWARELO1 PREREAD BUY                                
         TM    TWAFLAGS,TWABYRDQ   GOT BUY                                      
         BZ    CHAB08              NO                                           
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    CHAB06                                                           
         CLI   TWASCRN,X'C7'       SCREEN LOADED?                               
         BE    CHAB09              YES                                          
         MVI   BYTE,X'C7'          NO - FORCE SPORTS SCREEN                     
         B     CHAB11                   AND CONTINUE                            
*                                                                               
CHAB06   DS    0H                                                               
CHAB08   CLC   TWASCRN,8(R5)                                                    
         BNE   CHAB10                                                           
*                                                                               
CHAB09   L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         TM    CONBNUMH-CONBACTH+4(R2),X'20'                                    
         BO    CHAB20                                                           
         CLI   TWASCRN,X'C7'                                                    
         BNE   CHAB10                                                           
         MVI   BYTE,X'C7'                                                       
         B     CHAB11                                                           
*                                                                               
CHAB10   MVC   BYTE,8(R5)                                                       
CHAB11   DS    0H                                                               
         TM    TWAFLAGS,TWABYRDQ        HAVE BUY PREREAD?                       
         BZ    CHAB12                   NO                                      
         LA    R6,RBUYREC               ALREADY PATTERN BUY?                    
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    TWAFLAGS,TWAFLPTQ                                                
*                                                                               
CHAB12   DS    0H                                                               
         CLI   BYTE,X'C7'                                                       
         BNE   CHAB13                                                           
         TM    TWAPROST,X'02'      EXPANDED SCREEN IN USE?                      
         BZ    CHAB13              NO                                           
         BAS   R8,FFDIS            YES - REPAINT WHOLE SCREEN                   
         BAS   R8,CONDIS                                                        
         MVI   BYTE,X'C7'                                                       
CHAB13   BAS   R8,NEWSCRN                                                       
*                                                                               
         TM    TWAMGFLG,X'20'      RESET EXPANDED BUY SCREEN IN USE             
         BZ    *+8                 FOR THIS TRANSACTION ONLY                    
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         NI    CONBNUMH-CONBACTH+4(R2),X'FF'-X'20'                              
*                                                                               
         MVC   HALF2,=H'0'                                                      
*                                                                               
*                                                                               
         MVC   BUYACT,=C'DIS'                                                   
         BAS   R8,BUYDIS                                                        
         BAS   R8,DIS1                                                          
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(3,R2),0(R5)                                                    
         MVC   BUYACT,0(R5)                                                     
         CLC   BUYACT,=C'CAN'                                                   
         BE    *+14                                                             
         CLC   BUYACT,=C'DEL'                                                   
         BNE   *+14                                                             
         MVC   8(L'CONBACT,R2),MYSPACES     DEL/CAN MUST RE-ENTER ACT           
         B     CHAB18                                                           
*                                                                               
         TM    TWAFLAGS,TWAFLAVN                                                
         BZ    *+8                                                              
         MVI   10(R2),C'#'         AVN ACTION                                   
*                                                                               
CHAB18   DS    0H                                                               
         OI    6(R2),X'80'                                                      
***>     NI    CONBNUMH-CONBACTH+4(R2),X'FF'-X'20'                              
         OI    CONBNUMH-CONBACTH+4(R2),X'20'                                    
*                                                                               
         B     CHABX                                                            
         SPACE 1                                                                
CHAB20   DS    0H                                                               
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    CHAB30                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBBUY',0),RR=TWARELO1                
         B     CHAB40                                                           
*                                                                               
CHAB30   DS    0H                                                               
         CLI   TWASCRN,X'C7'       SPORTS BUY IN PROCESS?                       
         BNE   CHAB35              NO                                           
         GOTO1 VLOAD,DMCB,(X'7C',0),=C'EDIT'                                    
         B     CHAB38                                                           
*                                                                               
CHAB35   DS    0H                                                               
         NI    TWAMGFLG,X'FF'-X'40'                                             
*                                                                               
         BAS   R8,LOADEDT                                                       
CHAB38   DS    0H                                                               
         CLC   BUYACT,=C'CAN'                                                   
         BE    CHAB39                                                           
         CLC   BUYACT,=C'DEL'                                                   
         BE    CHAB39                                                           
         MVC   BYTE,TWASCRN                                                     
         BAS   R8,NEWSCRN          REFRESH SCREEN                               
         BAS   R8,BUYDIS                                                        
*                                                                               
CHAB39   DS    0H                                                               
         TM    TWAMGFLG,X'20'      RESET EXPANDED BUY SCREEN IN USE             
         BZ    *+8                 FOR THIS TRANSACTION ONLY                    
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
*                                                                               
         TM    TWAMGFLG,X'40'      GO MY OWN BUCKET UPDATE?                     
         BO    CHAB40                                                           
         BAS   R8,BUCKUP                                                        
*                                                                               
CHAB40   BAS   R8,DIS2                                                          
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OI    6(R2),X'80'         XMIT                                         
         GOTO1 VCKTAREQ                                                         
*                                                                               
CHABX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*              CANCEL BUY                                                       
         SPACE 1                                                                
*&&DO                                                                           
CANB     DC    0H'0'                                                            
         CLC   TWASCRN,8(R5)                                                    
         BNE   CANB10                                                           
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         TM    CONBNUMH-CONBACTH+4(R2),X'20'                                    
         BO    CANB20                                                           
*                                                                               
CANB10   MVC   BYTE,8(R5)                                                       
         BAS   R8,NEWSCRN                                                       
         SPACE 1                                                                
         BAS   R8,BUYDIS                                                        
         BAS   R8,DIS1                                                          
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OI    6(R2),X'80'         XMIT                                         
         OI    CONBNUMH-CONBACTH+4(R2),X'20'                                    
         B     CANBX                                                            
         SPACE 1                                                                
CANB20   DC    0H'0'                                                            
*                                                                               
         TM    TWAMGFLG,X'20'      RESET EXPANDED BUY SCREEN IN USE             
         BZ    *+8                 FOR THIS TRANSACTION ONLY                    
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
*                                                                               
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    CANB30                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBBUY',0),RR=TWARELO1                
         B     CANB40                                                           
*                                                                               
CANB30   BAS   R8,LOADEDT                                                       
         BAS   R8,BUCKUP                                                        
*                                                                               
CANB40   GOTO1 VCKTAREQ                                                         
         BAS   R8,DIS2                                                          
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OI    6(R2),X'80'         XMIT                                         
         OI    CONBNUMH-CONBACTH+4(R2),X'20'                                    
*                                                                               
CANBX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
*              DISPLAY BUY                                                      
         SPACE 1                                                                
DISB     DC    0H'0'                                                            
         GOTO1 =A(RDBUY),RR=TWARELO1 PREREAD BUYREC                             
         TM    TWAFLAGS,TWABYRDQ                                                
         BZ    DISB12              UNABLE TO PREREAD                            
*                                                                               
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    DISB10                                                           
         MVI   BYTE,X'C7'                                                       
         TM    TWAPROST,X'02'      EXPANDED SCREEN IN USE?                      
         BZ    *+12                NO                                           
         BAS   R8,FFDIS            YES - REPAINT WHOLE SCREEN                   
         BAS   R8,CONDIS                                                        
*                                                                               
         BAS   R8,NEWSCRN                                                       
         BAS   R8,BUYDIS                                                        
         BAS   R8,DIS1                                                          
         B     EXIT                                                             
*                                                                               
DISB10   DS    0H                                                               
         TM    TWAFLAGS,TWABYRDQ                                                
         BZ    DISB12                                                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,RBUYPTEQ     PATTERN ELEM                                 
         BAS   RE,GETEL                                                         
         BNE   DISB12                                                           
         OI    TWAFLAGS,TWAFLPTQ                                                
DISB12   DS    0H                                                               
         MVC   BYTE,8(R5)                                                       
         BAS   R8,NEWSCRN                                                       
*                                                                               
DISB20   DS    0H                                                               
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBDIS',0),RR=TWARELO1                
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         OI    CONBNUMH-CONBACTH+4(R2),X'20'                                    
*                                                                               
         TM    TWAMGFLG,X'20'      EXPANDED BUY SCREEN IN USE                   
         BZ    *+8                 FOR THIS TRANSACTION ONLY                    
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*              DSM            DISPLAY MULTIPLE                                  
         SPACE 1                                                                
DSMB     DC    0H'0'                                                            
         TM    TWASTREO,X'80'      STEREO IN USE?                               
         BZ    DSMB05                                                           
*                                                                               
         CLI   CONBNUMH+5,0        DSM ONLY ACTION?                             
         BNE   DSMB05                                                           
         LA    RF,L'TWASTRSV       YES                                          
         XCEF  TWASTRSV,(RF)       CLEAR STEREO BLOCK                           
         OI    TWASTREO,X'40'      SET TO CALL DSM STEREO VERSION               
         B     DSMST                                                            
*                                                                               
DSMB05   DS    0H                                                               
         CLI   TWASCRN,X'FD'                                                    
         BE    DSMB10                                                           
         MVI   BYTE,X'FD'                                                       
         BAS   R8,NEWSCRN                                                       
         SPACE 1                                                                
DSMB10   DC    0H'0'                                                            
         BAS   R8,LOADDIS                                                       
         B     EXXMOD                                                           
*                                                                               
DSMST    DS    0H                  DSM STEREO                                   
         CLI   TWASCRN,X'D3'                                                    
         BE    DSMST20                                                          
*                                                                               
DSMST10  DS    0H                                                               
         LA    R3,CONHACTH                                                      
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVI   DMCB+7,X'D3'        SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # D3                             
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         MVI   TWASCRN,X'D3'       LOAD NEW SCREEN NUMBER                       
*                                                                               
DSMST20  DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'22',0),=C'DISP'                                    
         B     EXXMOD                                                           
*                                                                               
*              DSM            DISPLAY MULTIPLE (COMBO ORDER)                    
*                                                                               
DSMC     DC    0H'0'                                                            
         TM    TWASTREO,X'80'      STEREO IN USE?                               
         BZ    DSMC05                                                           
         CLI   CONBNUMH+5,0        DSM ONLY ACTION?                             
         BNE   DSMC05                                                           
         LA    RF,L'TWASTRSV                                                    
         XCEF  TWASTRSV,(RF)       CLEAR STEREO BLOCK                           
         OI    TWASTREO,X'40'      SET TO CALL DSM STEREO VERSION               
         B     DSMST                                                            
*                                                                               
DSMC05   DC    0H'0'                                                            
         CLI   TWASCRN,X'F9'                                                    
         BE    DSMC10                                                           
         MVI   BYTE,X'F9'                                                       
         BAS   R8,NEWSCRN                                                       
         SPACE 1                                                                
DSMC10   DC    0H'0'                                                            
         BAS   R8,LOADDIS                                                       
         B     EXXMOD                                                           
*              DRT            DISPLAY MULTIPLE - REP TO SPOT                    
         SPACE 1                                                                
DSMD     DC    0H'0'                                                            
         CLI   TWASCRN,X'E7'                                                    
         BE    DSMD10                                                           
         MVI   BYTE,X'E7'                                                       
         BAS   R8,NEWSCRN                                                       
         SPACE 1                                                                
DSMD10   DC    0H'0'                                                            
         BAS   R8,LOADDIS                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
*              MKGL           MAKEGOOD LISTING FACILITY                         
*                                     WILL CYCLE THRU MG CONTROL MOD            
*                                     UNTIL A SELECTION (S/N) IS MADE           
*                                                                               
*                                  UPON SELECTION, WILL FORCE TRANSFER          
*                                     TO MAKEGOOD DISPLAY MOD                   
         SPACE 1                                                                
MKGL     EQU   *                                                                
         NI    TWAMKGFG,X'FF'-X'08' CLEAR BONUS FLAG                            
*        BAS   RE,CHECKMG          CHECK IF CONFIRMED AT LEAST ONCE             
*        GOTO1 =A(CHECKMG),RR=TWARELO1                                          
         CLI   TWASCRN,X'D1'       SCREEN ALREADY UP?                           
         BNE   MKGL0010            NO  - DISPLAY AND LOAD IT                    
*        TM    CONCNUMH+4,X'20'    YES - CONTRACT PREVIOUSLY VALID?             
*        BNO   EXIT                NO  - CONTRACT CHANGED - DON'T               
*                                     DO ANYTHING                               
         B     MKGL0120            REQUEST FOR CHANGE                           
*                                                                               
MKGL0010 EQU   *                                                                
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR MKGOOD  SCREEN)             
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVI   DMCB+7,X'D1'        SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # D1 (MAKEGOOD CONTROL)          
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
         MVI   TWASCRN,X'D1'       LOAD NEW SCREEN NUMBER                       
MKGL0120 EQU   *                                                                
         LA    R8,CONCACTH         SET UP MAIN ACTION FIELD                     
*                                  A 'DIS,MGL' ACTION WILL HAVE                 
*                                     CONCACT FILLED WITH SPACES AT             
*                                     THIS POINT                                
         CLC   8(4,R8),MYSPACES    CONTRACT FIELD SPACE-FILLED?                 
         BE    MKGL0130            YES - FORCE 'MGL' INTO ACTION FIELD          
         CLI   5(R8),0             NO  - ANYTHING IN FIELD?                     
         BNE   MKGL0140            YES - LEAVE IT ALONE                         
MKGL0130 EQU   *                                                                
         MVC   8(3,R8),=C'MGL'     NO  - INSERT MAKEGOOD CONTROL ACTION         
         MVC   LASTCACT,=C'MGL '   INSERT INTO LAST ACTION ALSO                 
         MVI   5(R8),3             INSERT A LENGTH OF 3                         
         NI    CONCACTH+4,X'FF'-X'20'   TURN OFF PREVALID BIT                   
MKGL0140 EQU   *                                                                
         BAS   R8,LOADDIS          PERFORM DISPLAY OF CONTROL INFO              
         CLI   DUB,X'FF'           SPECIAL CASE OF MGF PRINTED?                 
         BE    *+8                 YES - MSG HAS BEEN HANDLED                   
         BAS   R8,DIS1             DISPLAY INFO MESSAGE # 1                     
         TM    TWAMKGFG,X'04'      TOTAL DISPLAY??                              
         BO    MKGL0165                                                         
         OC    TWAMKGDS,TWAMKGDS   DISPLAY REQUIRED?                            
         BNZ   MKGL0160            YES - GO LOAD/DISPLAY OFFER                  
         TM    TWAMKGFG,X'08'      OR BONUS                                     
         BO    MKGL0160                                                         
*****    BAS   R8,DIS2             NO  - DISPLAY INFO MESSAGE # 2               
         TM    TWADARE,X'02'       AUTO-APPLY?                                  
         BZ    MKGL0150                                                         
         TM    TWADARE,X'04'       AUTO-APPLY RETURN CALL                       
         BO    GOBACK                                                           
MKGL0150 EQU   *                                                                
         CLI   DUB,X'FF'           SPECIAL CASE OF MGF PRINTED?                 
         BE    EXXMOD              YES - MSG HAS BEEN HANDLED                   
         OC    DUB(4),DUB                                                       
         BZ    EXXMOD                                                           
*        L     R2,DUB+4            POSITION CURSOR                              
*        OI    6(R2),X'40'         INSERT CURSOR                                
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),DUB       SET MESSAGE FOR DISPLAY                      
         OC    DMCB+2(2),DMCB+2                                                 
         BZ    EXXMOD                                                           
         GOTO1 VDISMSG,DMCB                                                     
         B     EXXMOD              EXIT ROUTINE                                 
*                                                                               
MKGL0160 EQU   *                                                                
         OC    TWAMKGD2,TWAMKGD2                                                
         BZ    MKGL0162                                                         
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         LA    R5,*                FORCE ACTION TO NEW MAKEGOOD OFFER           
         A     R5,=A(MGOFFDIS-(*-4))                                            
         CLI   RMKGKRTY,0          OLD STYLE SINGLE OFFER MAKEGOOD?             
         BE    MKGL0162                                                         
         CLI   RMKGKMLN,0          NEW MAKEGOOD?                                
         BE    MKGL0163                                                         
*                                                                               
MKGL0162 EQU   *                                                                
         LA    R5,*                NO, USE OLD MAKEGOOD OFFER                   
         A     R5,=A(MGOFFDI#-(*-4))                                            
*                                                                               
MKGL0163 EQU   *                                                                
*                                  LOAD 'E5' SCREEN FOR MAKEGOOD OFFER          
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR SAR/SPL SCREEN)             
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVC   DMCB+7(1),8(R5)     SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # E5 (MAKEGOOD SCREEN)           
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
         MVC   TWASCRN,8(R5)       LOAD NEW SCREEN NUMBER                       
         B     MKGL0170                                                         
*                                                                               
MKGL0165 EQU   *                                                                
*                                  LOAD 'DC' SCREEN FOR MAKEGOOD TOTAL          
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR SCREEN)                     
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVI   DMCB+7,X'DC'        SCREEN NUMBER                                
         MVI   TWASCRN,X'DC'       LOAD NEW SCREEN NUMBER                       
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # DC (MAKEGOOD SCREEN)           
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
*                                                                               
         MVI   BYTE,X'E5'                                                       
         LA    R5,*                FORCE ACTION TO MAKEGOOD TOTAL               
         A     R5,=A(MGOFFTOT-(*-4))                                            
*                                                                               
MKGL0170 DS    0H                                                               
         BAS   R8,LOADDIS          PERFORM DISPLAY OF SELECTED ITEM             
*                                                                               
*                                  MODULE WILL RETURN APPROPRIATE               
*                                     ACTION MESSAGE                            
MKGL0180 DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),DUB       SET MESSAGE FOR DISPLAY                      
         GOTO1 VDISMSG,DMCB                                                     
         LA    R2,CONCACTH         POSITION CURSOR                              
         OI    6(R2),X'40'         INSERT CURSOR                                
         B     EXXMOD                                                           
*                                                                               
*              MKGD           MAKEGOOD DISPLAY                                  
*                                     WILL DISPLAY A VIRGIN SCREEN              
*                                     IF TRANSFERRED TO DIRECTLY                
*                                                                               
         SPACE 1                                                                
MKGD     EQU   *                                                                
         CLI   TWASCRN,X'C0'                                                    
         BE    MKGD10                                                           
         LA    R3,490              MISSING MGO GROUP CODE                       
         LA    R2,CONBNUMH         SET CURSOR ADDRESS                           
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         MVI   BYTE,X'C0'                                                       
         BAS   R8,NEWSCRN          LOAD MAKEGOOD DISPLAY SCREEN                 
         SPACE 1                                                                
MKGD10   EQU   *                                                                
         BAS   R8,DIS1             DISPLAY INFO MESSAGE # 1                     
         B     EXXMOD              EXIT ROUTINE                                 
*                                                                               
*              MKGO           MAKEGOOD OFFER                                    
*                                     WILL UPDATE THE DISPLAYED MG              
*                                     OFFER                                     
*                                  IF ACCESSED DIRECTLY, WILL ADD A             
*                                     NEW MAKEGOOD OFFER                        
*                                                                               
*                                                                               
MKGO     EQU   *                                                                
         CLC   =C'MGO',CONBACT                                                  
         BNE   MKGO0010                                                         
         CLC   =C'MGO',CONBNUM                                                  
         BE    MKGO0010                                                         
         CLI   CONBNUM,C'M'        STEREO USER WILL PUT OFFER TYPE HERE         
         BNE   MKGO0010            ANYTHING STARTS WITH M IS OKAY               
         MVC   CONBACT(3),CONBNUM  MGM/MGP/MGB/MLR/MLB                          
         B     CONA0000            START OVER                                   
*                                                                               
MKGO0010 EQU   *                                                                
         GOTO1 =A(CHECKMG),RR=TWARELO1 CHECK IF CONFIRMED AT LEAST ONCE         
         MVI   BYTE,X'C0'          DEFAULT NEW MAKEGOOD SCREEN                  
         CLI   CONACT,C'M'                                                      
         BE    MKGO0020                                                         
         CLC   =C'RNA',CONACT                                                   
         BE    MKGO0020                                                         
         CLC   =C'LCO',CONACT                                                   
         BE    MKGO0020                                                         
         MVC   CONACT,BUYACT                                                    
         B     MKGO0030                                                         
*                                                                               
MKGO0020 EQU   *                                                                
         MVC   BUYACT,CONACT                                                    
*                                                                               
MKGO0030 EQU   *                                                                
         CLC   =C'MGX',CONACT                                                   
         BNE   MKGO0035                                                         
         CLI   TWASCRN,X'E5'       SCREEN ALREADY UP?                           
         BNE   MKGO0110            CHECK IF OLD STYLE MAKEGOOD                  
         LA    R5,*                                                             
         A     R5,=A(MGACTS#-(*-4))                                             
         B     MKGO0110                                                         
*                                                                               
MKGO0035 EQU   *                                                                
         CLC   =C'MGC',CONACT                                                   
         BE    MKGO0040                                                         
         CLC   =C'MGO',CONACT                                                   
         BE    MKGO0045                                                         
         B     MKGO0065                                                         
*                                                                               
MKGO0040 EQU   *                                                                
         CLI   TWASCRN,X'CC'       SCREEN ALREADY UP?                           
         BE    MKGO0110                                                         
         CLI   TWASCRN,X'C1'       SCREEN ALREADY UP?                           
         BL    MKGO0045                                                         
         CLI   TWASCRN,X'C5'       SCREEN ALREADY UP?                           
         BNH   MKGO0110                                                         
*                                                                               
MKGO0045 EQU   *                                                                
         CLI   TWASCRN,X'E5'       SCREEN ALREADY UP?                           
         BNE   MKGO0050                                                         
         LA    R5,*                                                             
         A     R5,=A(MGACTS#-(*-4))                                             
         B     MKGO0110                                                         
*                                                                               
MKGO0050 EQU   *                                                                
         GOTO1 =A(MGOPROC),DMCB,(RC),RR=TWARELO1                                
         BNZ   MKGO0060            NEW MG OFFER DO NOT SPECIFY BUY#             
         LA    R5,*                                                             
         A     R5,=A(MGOFFDI#-(*-4))                                            
         B     MKGO0140                                                         
*                                                                               
* CHECK IF NEW MAKEGOOD OFFER SCREENS LOADED                                    
*                                                                               
MKGO0060 EQU   *                                                                
         CLC   =C'MGO',BUYACT      NO TARGET, INVOKE MAKEGOOD MENU              
         BNE   MKGO0065                                                         
         CLC   =C'MGO',CONCACT     IF INVOKE AT CON ACT, NEED                   
         BNE   MKGO0063            TO RELOAD BASE SCREEN                        
         MVC   CONCACT(7),=C'DIS,MGO'                                           
         MVI   CONCACTH+5,7                                                     
         NI    CONCACTH+4,X'FF'-X'20'                                           
         XC    CONBACT,CONBACT                                                  
         MVI   CONBACTH+5,0                                                     
         B     CONA0000                                                         
*                                                                               
MKGO0063 DS    0H                                                               
         LA    R5,*                                                             
         A     R5,=A(MGACTS-(*-4)) FAKE MGM AND GO TO RECNT3A                   
         B     MKGO0110                                                         
*                                                                               
MKGO0065 EQU   *                                                                
         CLC   =C'MGM',BUYACT                                                   
         BNE   MKGO0070                                                         
         CLI   TWASCRN,X'C1'       SCREEN ALREADY UP?                           
         BNE   MKGO0130            NO  - DISPLAY AND LOAD IT                    
         B     MKGO0110                                                         
*                                                                               
MKGO0070 EQU   *                                                                
         CLC   =C'MGP',BUYACT                                                   
         BNE   MKGO0080                                                         
         CLI   TWASCRN,X'C2'       SCREEN ALREADY UP?                           
         BNE   MKGO0130            NO  - DISPLAY AND LOAD IT                    
         B     MKGO0110                                                         
*                                                                               
MKGO0080 EQU   *                                                                
         CLC   =C'MGB',BUYACT                                                   
         BNE   MKGO0090                                                         
         CLI   TWASCRN,X'C3'       SCREEN ALREADY UP?                           
         BNE   MKGO0130            NO  - DISPLAY AND LOAD IT                    
         B     MKGO0110                                                         
*                                                                               
MKGO0090 EQU   *                                                                
         CLC   =C'MLR',BUYACT                                                   
         BNE   MKGO0100                                                         
         CLI   TWASCRN,X'C4'       SCREEN ALREADY UP?                           
         BNE   MKGO0130            NO  - DISPLAY AND LOAD IT                    
         B     MKGO0110                                                         
*                                                                               
MKGO0100 EQU   *                                                                
         CLC   =C'MLB',BUYACT                                                   
         BNE   MKGO0105                                                         
         CLI   TWASCRN,X'C5'       SCREEN ALREADY UP?                           
         BNE   MKGO0130            NO  - DISPLAY AND LOAD IT                    
         B     MKGO0110                                                         
*                                                                               
MKGO0105 EQU   *                                                                
         CLC   =C'RNA',BUYACT                                                   
         BNE   EXXMOD                                                           
         CLI   TWASCRN,X'CC'       SCREEN ALREADY UP?                           
         BNE   MKGO0130            NO  - DISPLAY AND LOAD IT                    
*                                                                               
MKGO0110 EQU   *                                                                
         BAS   R8,LOADEDT                                                       
         BAS   R8,DIS2                                                          
*                                                                               
         CLC   =C'MGO',CONACT      REDISPLAY FOR SINGLE TARGET                  
         BE    MKGO0120            ADD/CHANGE                                   
         CLC   =C'MGC',CONACT                                                   
         BNE   EXIT                                                             
*                                                                               
MKGO0120 EQU   *                                                                
         CLI   TWASCRN,X'C6'       MENU SELECTION?                              
         BE    CONA0000                                                         
         CLI   TWASCRN,X'E5'       SCREEN ALREADY UP?                           
         BNE   EXIT                                                             
         LA    R5,*                                                             
         A     R5,=A(MGOFFDI#-(*-4))                                            
         BAS   R8,LOADDIS                                                       
         B     EXIT                                                             
*                                                                               
MKGO0130 EQU   *                                                                
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR MKGOOD  SCREEN)             
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVC   DMCB+7(1),BYTE      SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # D1 (MAKEGOOD CONTROL)          
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
         MVC   TWASCRN,BYTE        LOAD NEW SCREEN NUMBER                       
*                                                                               
MKGO0140 EQU   *                                                                
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
*                                                                               
         LA    R2,CONCACTH         POSITION CURSOR                              
         OI    6(R2),X'40'         INSERT CURSOR                                
         B     EXXMOD              EXIT ROUTINE                                 
*                                                                               
         EJECT                                                                  
*              AUDL           AUDIT COMMENTS LISTING FACILITY                   
*                                     WILL CYCLE THRU AUD CONTROL MOD           
*                                     UNTIL A SELECTION (S/N) IS MADE           
*                                                                               
*                                  UPON SELECTION, WILL FORCE TRANSFER          
*                                     TO AUDIT COMMENT DISPLAY MOD              
*                                                                               
AUDL     DS    0H                                                               
         BRAS  RE,AUDLCOM          HANDLE LIST SCREEN                           
         B     EXXMOD                                                           
*                                                                               
*        AUD            AUDIT COMMENTS DISPLAY FACILITY                         
*                                                                               
AUD      DS    0H                                                               
         BRAS  RE,AUDCOM           HANDLE DISPLAY SCREEN                        
         B     EXXMOD                                                           
*                                                                               
*              MULTIPLE BUY                                                     
         SPACE                                                                  
MBIB     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        GET RANDOM FLAG ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   MBIB0003            NOT FOUND                                    
RFLG     USING RCONRFEL,R6                                                      
         TM    RFLG.RCONRF1,X'40'  PROPOSER ORDER?                              
         BNO   MBIB0003            NO  - DON'T CHECK CUTOFF DATE                
         DROP  RFLG                                                             
*                                                                               
         TM    RCONMODR,X'10'      BUYLINE ADDED?                               
         BO    MBIB0003            YES - CONSIDER A REGULAR ORDER,              
*                                     PERMIT ADDITIONAL BUYS                    
         GOTO1 =A(STAPROCK),DMCB,(RC),RR=TWARELO1                               
         BZ    MBIB0003            BUYS PERMITTED                               
         LA    R3,1007             SET 'PROPOSER ORDER: NO BUYS' ERROR          
         B     ERROR                                                            
*                                                                               
MBIB0003 DS    0H                                                               
         CLI   TWASCRN,X'F3'                                                    
         BE    MBIB0040                                                         
         SPACE                                                                  
         TM    TWAPROST,X'01'                                                   
         BZ    MBIB0005                                                         
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
MBIB0005 GOTO1 VLDSCRN,DMCB,(X'F3',CONSHRTH),(X'80',0)                          
         BAS   R8,DIS1                                                          
         LA    R2,MBYFLNH          POSITION AT FLIGHT NUM FIELD                 
         CLI   RCONKSTA+4,C' '     TV?                                          
         BE    MBIB0010            NO                                           
         CLI   RCONKSTA+4,C'L'     TV?                                          
         BNE   MBIB0020            NO                                           
MBIB0010 EQU   *                                                                
         OI    1(R2),X'2C'         YES - SET FIELD TO PROTECTED                 
*                                     LOW INTENSITY                             
         OI    6(R2),X'80'         XMIT                                         
         LA    R2,MBYFNMH          TURN OFF FIELD DESCRIPTION                   
         OI    1(R2),X'0C'         SET FIELD TO LOW INTENSITY                   
         OI    6(R2),X'80'         XMIT                                         
         LA    R2,MBYFLTH           POSITION AT FLIGHT FIELD                    
MBIB0020 EQU   *                                                                
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
         SPACE                                                                  
MBIB0040 DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         GOTO1 VCKTAREQ                                                         
         LA    R2,MBYACTH                                                       
         MVC   MBYACT,MYSPACES                                                  
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
*                                                                               
*              MULTIPLE BUY (COMBO ORDER)                                       
         SPACE                                                                  
MBIC     CLI   TWASCRN,X'F8'                                                    
         BE    MBIC10                                                           
*                                                                               
         TM    TWAPROST,X'01'                                                   
         BZ    MBIC05                                                           
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
MBIC05   GOTO1 VLDSCRN,DMCB,(X'F8',CONSHRTH),(X'80',0)                          
         BAS   R8,DIS1                                                          
         LA    R2,MBYFLNH           POSITION AT FLIGHT NUM FIELD                
         OI    6(R2),X'80'         XMIT                                         
         BAS   R8,LOADDIS          FORCE LOAD OF SUBSCREEN STATION              
*                                     CALL LETTERS                              
         B     EXIT                                                             
         SPACE                                                                  
MBIC10   DS    0H                                                               
         BAS   R8,LOADDIS          FORCE LOAD OF SUBSCREEN STATION              
*                                     CALL LETTERS                              
         BAS   R8,LOADEDT                                                       
         GOTO1 VCKTAREQ                                                         
         LA    R2,MBYACTH                                                       
         MVC   MBYACT,MYSPACES                                                  
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* MULTIPLE CHANGE INPUT (MCI)                                                   
*                                                                               
MCIB     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBMCI',0),RR=TWARELO1                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* BUYCODE MAINTENANCE (BCD/BCC)                                                 
BCDA     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBBCD',0),RR=TWARELO1                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*           ORDER INFO (ORD)                                                    
         SPACE 1                                                                
ORDC     DC    0H'0'                                                            
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBORD',0),RR=TWARELO1                  
         B     EXIT                                                             
         EJECT                                                                  
*           COVERSHEET COMMENT (CMT)                                            
         SPACE 1                                                                
COVR     DC    0H'0'                                                            
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBCOV',0),RR=TWARELO1                  
         B     EXIT                                                             
         EJECT                                                                  
*           CONFIRMATION COMMENT (CFC)                                          
         SPACE 1                                                                
CCMT     DC    0H'0'                                                            
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBCFC',0),RR=TWARELO1                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SPL                                                                           
*                                                                               
SPLC     GOTO1 =A(SPLC00),RR=TWARELO1                                           
         B     EXIT                                                             
*                                                                               
*&&DO                                                                           
         CLC   CONSTA+4(2),=C'-T'                                               
         BE    SPLC5                                                            
         CLC   CONSTA+3(2),=C'-T'                                               
         BE    SPLC5                                                            
         LA    R2,CONBACTH                                                      
         LA    R3,BACERR                                                        
         B     BUYA0150                                                         
         SPACE 1                                                                
SPLC5    MVC   LOWBIT,SPLDATA                                                   
         MVC   ELCODE,SPLDATA+1                                                 
         B     SUPPL                                                            
*&&                                                                             
         EJECT                                                                  
*              BOP                                                              
         SPACE 1                                                                
BOPC     MVC   LOWBIT,BOPDATA                                                   
         MVC   ELCODE,BOPDATA+1                                                 
         LA    R2,CONBACTH                                                      
         LA    R3,BACERR                                                        
         CLC   CONSTA+4(2),=C'-T'                                               
         BE    BUYA0150                                                         
         CLC   CONSTA+3(2),=C'-T'                                               
         BE    BUYA0150                                                         
         B     SUPPL                                                            
         EJECT                                                                  
*              EPL                                                              
         SPACE 1                                                                
EPLC     LA    R2,CONBACTH                                                      
         LA    R3,BACERR                                                        
         CLC   CONSTA+4(2),=C'-T'                                               
         BE    BUYA0150                                                         
         CLC   CONSTA+3(2),=C'-T'                                               
         BE    BUYA0150                                                         
         SPACE 1                                                                
EPLC5    MVC   LOWBIT,EPLDATA                                                   
         MVC   ELCODE,EPLDATA+1                                                 
         B     SUPPL                                                            
         EJECT                                                                  
*              SAR                                                              
         SPACE 1                                                                
SARC     DS    0H                                                               
         LA    R3,368              INVALID ACTION CODE MESSAGE                  
         LA    R2,CONCACTH                                                      
         CLI   TWASCRN,X'F7'       SCREEN ALREADY UP?                           
         BNE   SAR10               NO  - DISPLAY AND LOAD IT                    
         TM    CONCNUMH+4,X'20'    YES - CONTRACT PREVIOUSLY VALID?             
         BNO   SAR20               NO  - CONTRACT CHANGED                       
         CLC   =C'SAR',CONACT      YES - ANOTHER DISPLAY REQUEST?               
         BE    SAR20               YES - DO IT AGAIN                            
         CLC   =C'PD',CONACT       YES - ANOTHER DISPLAY REQUEST?               
         BE    SAR20               YES - DO IT AGAIN                            
         B     SAR30               NO  - REQUEST FOR CHANGE                     
*                                                                               
SAR10    EQU   *                                                                
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         MVI   TWASCRN,X'F7'       LOAD NEW SCREEN NUMBER                       
         BAS   R8,FULLSCRN         FULL SCREEN CALL                             
*                                                                               
SAR20    EQU   *                                                                
         BAS   R8,LOADDIS          LOAD NEWLY DISPLAYED SCREEN                  
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'    TURN ON PREVALID BIT                         
         B     EXIT                                                             
*                                                                               
*  SAR SCREEN OVERLAYS ENTIRE TWENTY-FOUR LINES                                 
*                                                                               
SAR30    EQU   *                                                                
         BAS   R8,LOADEDT          EDIT/UPDATE FROM SAR SCRN                    
         BAS   R8,DIS2                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*&&DO                                                                           
         MVC   LOWBIT,SARDATA                                                   
         MVC   ELCODE,SARDATA+1                                                 
         B     SUPPL                                                            
         EJECT                                                                  
*&&                                                                             
*              UNI                                                              
         SPACE                                                                  
UNIC     MVC   LOWBIT,UNIDATA                                                   
         MVC   ELCODE,UNIDATA+1                                                 
         LA    R3,BACERR                                                        
         CLI   TWAUNIST,C'Y'                                                    
         BNE   ERROR                                                            
         B     SUPPL                                                            
         EJECT                                                                  
*              COV                                                              
         SPACE                                                                  
COVC     CLI   TWASCRN,X'EF'                                                    
         BE    COV10                                                            
*                                                                               
         MVI   BYTE,X'EF'                                                       
         BAS   R8,NEWSCRN                                                       
         BAS   R8,DIS1                                                          
         LA    R2,COVVALH                                                       
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
*                                                                               
COV10    DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         LA    R2,CONBACTH                                                      
         MVC   CONBACT,MYSPACES                                                 
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
         EJECT                                                                  
*              HIAT(US)                                                         
         SPACE                                                                  
QIAT     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBHIAT',0),RR=TWARELO1                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* TAKEOVER                                                                      
*                                                                               
QTKO     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBKTKO',0),RR=TWARELO1                 
         B     EXIT                                                             
         EJECT                                                                  
* PACING                                                                        
         SPACE                                                                  
PACE     DS    0H                                                               
         CLI   TWASCRN,X'D8'                                                    
         BE    PACE20                                                           
         MVI   BYTE,X'D8'                                                       
         BAS   R8,NEWSCRN                                                       
*                                                                               
PACE20   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         LA    R2,PCEESINH                                                      
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
*              MON                                                              
         SPACE                                                                  
MONB     DS    0H                                                               
         GOTO1 =A(MONB00),RR=TWARELO1                                           
         B     EXIT                                                             
MON#     DS    0H                  COMBO MON SUPPORT                            
         GOTO1 =A(MON#00),RR=TWARELO1                                           
         B     EXIT                                                             
MON$     DS    0H                  SCRIPT CONTRACT UPLOAD SUPPORT               
         GOTO1 =A(MON$00),RR=TWARELO1                                           
         B     EXIT                                                             
********************************************************************            
* CHECK IF CONTRACT IS A FORECAST CONTRACT                                      
********************************************************************            
IS4CAST  NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        LOOK FOR SAR ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   NO                  NOT FOUND                                    
                                                                                
         USING RSARXEL,R6          FOUND. IS THERE A FORECAST FLAG?             
         CLI   RSARXLEN,RSARXLTH   NO ELEMENT IS OLD SAR ELEMENT                
         BNE   NO                                                               
         TM    RSARXFLG,X'08'                                                   
         BO    YES                                                              
         DROP  R6                                                               
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* MOVE CONTRACT                                                                 
*                                                                               
MOVE     DS    0H                                                               
         GOTO1 =A(DOMOVE),DMCB,(RC),RR=TWARELO1                                 
         B     EXIT                                                             
*                                                                               
*              HIST                                                             
*                                                                               
HIST     DS    0H                                                               
         CLI   TWASCRN,X'EC'                                                    
         BE    HIST20                                                           
         MVI   BYTE,X'EC'                                                       
         BAS   R8,NEWSCRN                                                       
         SPACE 1                                                                
HIST20   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         LA    R2,CONBACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
KCHG     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBKCHG',0),RR=TWARELO1                 
         B     EXIT                                                             
         EJECT                                                                  
QRER     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBXRER',0),RR=TWARELO1                 
         B     EXIT                                                             
         EJECT                                                                  
QSPC     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBSPCP',0),RR=TWARELO1                 
         B     EXIT                                                             
         EJECT                                                                  
EQCD     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBEQCD',0),RR=TWARELO1                 
         B     EXIT                                                             
         EJECT                                                                  
EQTD     DS    0H                                                               
         GOTO1 =A(SUBACTS),DMCB,(RC),('QSUBEQTD',0),RR=TWARELO1                 
         LA    R2,CONCACTH                                                      
         OI    6(R2),X'40'         INSERT CURSOR AND RETURN SCREEN              
         B     EXIT                                                             
         EJECT                                                                  
*              BOP, EPL, SPL, SAR                                               
         SPACE 1                                                                
SUPPL    DC    0H'0'                                                            
         GOTO1 =A(SUPPLAB),RR=TWARELO1 PROCESS BOP,EPL,SPL,SAR                  
         DC    H'0'                SHOULD NEVER RETURN HERE!                    
         SPACE 1                                                                
*                                                                               
* LOAD FULL SCREEN AT CONTRACT NUMBER FIELD                                     
* INPUT: TWASCRN IS SET WITH NEW SCREEN NUMBER                                  
*                                                                               
FULLSCRN DS    0H                                                               
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVC   DMCB+7(1),TWASCRN   SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,CONCNUMX+8                                           
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
         BR    R8                                                               
         EJECT                                                                  
*                                                                               
* LOAD FULL SCREEN AT CONTRACT NUMBER FIELD                                     
* INPUT: TWASCRN IS SET WITH NEW SCREEN NUMBER                                  
*                                                                               
ORDSCRN  DS    0H                                                               
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVC   DMCB+7(1),TWASCRN   SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,CONMOD+L'CONMOD                                      
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
         BR    R8                                                               
         EJECT                                                                  
*                                                                               
NEWSCRN  DC    0H'0'                                                            
*                                                                               
         CLI   BYTE,X'C7'          SPORTS BUY SCREEN?                           
         BE    NEW10               YES                                          
         TM    PROFILES+CNTXBUYB,CNTXBUYA                                       
         BZ    NEW10                                                            
         TM    9(R5),X'04'         EXTENDED BUY SCREEN?                         
         BO    NEW20                                                            
*                                                                               
NEW10    DS    0H                                                               
         GOTO1 VLDSCRN,DMCB,(BYTE,CONLAST),(X'E0',0)                            
         B     NEW30                                                            
*                                                                               
NEW20    DS    0H                                                               
         OI    TWAPROST,X'02'                                                   
         GOTO1 VLDSCRN,DMCB,(BYTE,CONCNUMX+8),0                                 
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBDIS',0),RR=TWARELO1                
         B     NEWX                                                             
*                                                                               
NEW30    DS    0H                                                               
         CLI   TWACOMBO,0          IF COMBO ORDER, CHECK IF SPECIAL             
         BE    NEW40               COMBO DISPLAY LINE CLEARED.                  
         OC    CONCMBS,CONCMBS     IF SO, REDISPLAY LINE                        
         BNZ   NEW40                                                            
         GOTO1 =A(COMBORTN),DMCB,(RC),('QLINECMB',0),RR=TWARELO1                
*                                                                               
NEW40    CLI   BYTE,X'F5'          IS THIS SAR SCREEN?                          
         BE    NEW60               YES, UNPROTECT FLDS IF NECESSARY             
         CLI   BYTE,X'FE'          IS THIS BUY SCREEN?                          
         BE    NEW50               YES, CHECK IF COMBO CONTRACT                 
         B     NEWX                                                             
*                                                                               
NEW50    DS    0H                                                               
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBDIS',0),RR=TWARELO1                
         B     NEWX                                                             
*                                                                               
NEW60    DS    0H                                                               
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'R0CO'                                              
         MVC   WORK+20(2),REPALPHA                                              
         GOTO1 CGETPROF,DMCB,WORK+16,WORK,CDATAMGR                              
         DROP  R3                                                               
         CLI   WORK,C'Y'                                                        
         BE    NEW70                                                            
         B     NEWX                                                             
NEW70    NI    SARCPPH+1,X'DF'     MAKE UNPROTECTED                             
         NI    SARBGTH+1,X'DF'     MAKE UNPROTECTED                             
         NI    SARGTOTH+1,X'DF'    MAKE UNPROTECTED                             
         NI    SARCTOTH+1,X'DF'    MAKE UNPROTECTED                             
*                                                                               
NEWX     DS    0H                                                               
         TM    TWAMGFLG,X'20'      RESET EXPANDED BUY SCREEN IN USE             
         BZ    *+8                 FOR THIS TRANSACTION ONLY                    
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
*                                                                               
         BR    R8                                                               
         EJECT                                                                  
* THIS PROCEDURE CALLS VLDSCRN (JUST TO SAVE THE OVERHEAD OF 1 MILLION          
* GOTO1'S).  R5 MUST POINT TO THE APPROPRIATE ACTION TABLE ENTRY.               
* DITTO FOR LOADEDT (LOAD EDIT OVERLAY) AND LOADDIS (LOAD DISP OVL).            
* AND EVERYTHING ELSE THAT FOLLOWS...                                           
         SPACE                                                                  
LOADSCRN DS    0H                                                               
         GOTO1 VLDSCRN,DMCB,(8(R5),CONLAST),(9(R5),0)                           
*** COMBO, IF COMBO DISPLAY COMBO COMPONENT STATIONS                            
         CLI   8(R5),X'FE'                                                      
         BE    *+6                                                              
         BR    R8                                                               
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBDIS',0),RR=TWARELO1                
         BR    R8                                                               
*** COMBO                                                                       
         SPACE 2                                                                
LOADCLR  DS    0H                                                               
         GOTO1 VLOAD,DMCB,(10(R5),0),=C'CLRC'                                   
         BR    R8                                                               
         SPACE 2                                                                
LOADEDT  DS    0H                                                               
         GOTO1 VLOAD,DMCB,(10(R5),0),=C'EDIT'                                   
         BR    R8                                                               
         SPACE 2                                                                
LOADDIS  DS    0H                                                               
         GOTO1 VLOAD,DMCB,(10(R5),0),=C'DISP'                                   
         BR    R8                                                               
         SPACE 2                                                                
DIS1     DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),11(R5)                                                 
         GOTO1 VDISMSG,DMCB                                                     
         BR    R8                                                               
         SPACE 2                                                                
DIS2     DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),13(R5)                                                 
         GOTO1 VDISMSG,DMCB                                                     
         BR    R8                                                               
         SPACE 2                                                                
BUCKUP   DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'30',0)                                             
         BR    R8                                                               
         SPACE 2                                                                
CONEDIT  DC    0H'0'                                                            
         GOTO1 VLOAD,DMCB,(X'10',0)     EDIT OVERLAY                            
         BR    R8                                                               
         SPACE 2                                                                
CONDIS   DC    0H'0'                                                            
         GOTO1 VLOAD,DMCB,(X'20',0)     DISPLAY CONTRACT                        
         NI    CONBACTH+4,X'FF'-X'20'   TURN PRE-VALID BIT OFF                  
         NI    CONBNUMH+4,X'FF'-X'20'   TURN PRE-VALID BIT OFF                  
         BR    R8                                                               
         SPACE 2                                                                
BUYDIS   DC    0H'0'                                                            
         GOTO1 =A(BUYDNMOD),DMCB,(RC),RR=TWARELO1 BUYDIS ROUTINE                
         BR    R8                                                               
FFDIS    DS    0H                                                               
         GOTO1 =A(FFSCREEN),DMCB,(RC),RR=TWARELO1 RELOAD BASE SCREEN            
         BR    R8                                                               
         EJECT                                                                  
SPLDATA  DC    X'10',X'06'                                                      
BOPDATA  DC    X'08',X'10'                                                      
EPLDATA  DC    X'04',X'06'                                                      
SARDATA  DC    X'02',X'10'                                                      
UNIDATA  DC    X'20',X'A1'                                                      
LOWBIT   DS    X                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTEAD                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTEDD                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTC9D                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTD8D                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFBD                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTF2D                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTEFD                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTF5D                                                       
         ORG   CONLAST                                                          
       ++INCLUDE RECNTE4D                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTD6D                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTC7D                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTDBD          FOR SPL COPY                                 
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTCBD          FOR COMPENSATION S/P                         
         EJECT                                                                  
         ORG   CONSHRTH                                                         
       ++INCLUDE RECNTF3D                                                       
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTE5D                                                       
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTC0D                                                       
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTC8D                                                       
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTBFD                                                       
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTBED                                                       
*        ORG   MGHTAGH                                                          
*      ++INCLUDE RECNTC1D                                                       
         ORG   CONCACTH                                                         
       ++INCLUDE RECNTD3D                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
       ++INCLUDE REGENPRO                                                       
       ++INCLUDE REGENCFC                                                       
       ++INCLUDE REGLCOV                                                        
       ++INCLUDE REGENSBLK                                                      
       ++INCLUDE REGENABLK                                                      
T80201   CSECT                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*              PRINT WORKSHEET  'LAST'                                          
LAST00   NTR1  BASE=*,LABEL=*                                                   
         BAS   R8,LOADSCRN                                                      
         BAS   R8,CONDIS                                                        
         BAS   R8,LOADEDT                                                       
         MVI   INTTYPE,C'O'        INDICATE ORDER WORKSHEET                     
*                                                                               
         MVI   SENDPASS,1          ALWAYS ORIGINATOR                            
*                                                                               
         GOTO1 VLOAD,DMCB,(X'60',0)    PRODUCE WORKSHEET                        
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONCACTH                                                      
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SUPPLAB  NTR1   LABEL=*,BASE=*                                                  
         CLC   TWASCRN,8(R5)                                                    
         BE    *+8                                                              
         MVI   TWASTAT,0                                                        
         CLC   TWASTAT,LOWBIT                                                   
         BE    SUPPL50             DATA IS DISPLAYED                            
         MVC   BYTE,8(R5)                                                       
         BAS   R8,NEWSCRN          PUT OUT SCREEN                               
         SPACE 1                                                                
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         SPACE 1                                                                
SUPPL20  DC    0H'0'                                                            
         MVC   TWASTAT,LOWBIT                                                   
         LA    R2,CONLAST                                                       
         SPACE 1                                                                
SUPPL25  TM    1(R2),X'20'                                                      
         BNO   EXIT                CURSOR TO FIRST UNPROTECTED                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     SUPPL25                                                          
         SPACE 1                                                                
SUPPL50  DC    0H'0'                                                            
         BAS   R8,LOADEDT                                                       
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS2                                                          
         MVC   TWASTAT,LOWBIT                                                   
         LA    R2,CONBACTH                                                      
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*        HANDLE AUDIT COMMENTS LIST SCREEN                                      
*                                                                               
AUDLCOM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWASCRN,X'BE'       SKIP IF SCREEN ALREADY DISPLAYED             
         BE    AUDLSCRX                                                         
*                                                                               
*        LOAD LIST SCREEN                                                       
*                                                                               
         NI    CONCNUMH+4,X'FF'-X'20'    TURN OFF PREVALID BIT                  
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR COMMENT  SCREEN)            
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVI   DMCB+7,X'BE'        LIST SCREEN NUMBER                           
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # BE (AUDIT LIST)                
*                                                                               
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
*                                                                               
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
         MVI   TWASCRN,X'BE'       LOAD NEW SCREEN NUMBER                       
         XC    TWAMKGLA,TWAMKGLA   NO LAST COMMENT ON SCREEN                    
         XC    TWAMKGDA,TWAMKGDA   INIT RECORD LIST                             
         XC    TWAMKGDS,TWAMKGDS   NO CURRENT COMMENTS ON SCREEN                
         XC    TWAMKGD2,TWAMKGD2   NO SELECTED COMMENTS ON SCREEN               
*                                                                               
         B     AUDLLST                                                          
*                                                                               
AUDLSCRX EQU   *                                                                
*                                                                               
*        LIST SCREEN DISPLAYED                                                  
*                                                                               
         LA    R8,CONCACTH         SET UP MAIN ACTION FIELD                     
*                                  A 'DIS,AUDL' ACTION WILL HAVE                
*                                     CONCACT FILLED WITH SPACES AT             
*                                     THIS POINT                                
         CLC   8(4,R8),MYSPACES    CONTRACT FIELD SPACE-FILLED?                 
         BE    AUDL0130            YES - FORCE 'AUDL' INTO ACTION FIELD         
*                                                                               
         CLI   5(R8),0             NO  - ANYTHING IN FIELD?                     
         BNE   AUDL0140            YES - LEAVE IT ALONE                         
*                                                                               
AUDL0130 EQU   *                                                                
*                                                                               
         MVC   8(4,R8),=C'AUL '    INSERT AUDIT LIST ACTION                     
         MVC   LASTCACT,=C'AUL '   INSERT INTO LAST ACTION ALSO                 
         MVI   5(R8),4             INSERT A LENGTH OF 4                         
         NI    CONCACTH+4,X'FF'-X'20'   TURN OFF PREVALID BIT                   
*                                                                               
AUDL0140 EQU   *                                                                
*                                                                               
         LA    R8,CONCNUMH         POINT TO CONTRACT NUMBER                     
*                                                                               
         TM    4(R8),X'20'         IF CHANGED THIS TIME                         
         BO    AUDL145                                                          
*                                                                               
         XC    TWAMKGLA,TWAMKGLA      NO LAST COMMENT ON SCREEN                 
         XC    TWAMKGDA,TWAMKGDA      INIT RECORD LIST                          
*                                                                               
         B     AUDLLST             GO LIST COMMENTS                             
*                                                                               
AUDL145  DS    0H                                                               
*                                                                               
*        CHECK IF ANY COMMENT SELECTED                                          
*                                                                               
         LA    R0,(ADLCLSTH-ADLCSELH)/(ADLCSE2H-ADLCSELH)+1  # OF LINES         
         LA    R2,ADLCSELH         POINT TO FIRST SEL FIELD                     
         LA    R8,TWAMKGDA         POINT TO LIST OF DISK ADDR                   
*                                                                               
AUDLSLLP DS    0H                                                               
*                                                                               
         CLI   5(R2),0             SKIP IF NO ENTRY IN SELECT FIELD             
         BE    AUDLSLCN                                                         
*                                                                               
         CLI   8(R2),C'S'          CHECK FOR ITEM SELECTION                     
         BE    AUDLSLFD                                                         
*                                                                               
AUDLSLCN DS    0H                                                               
*                                                                               
         LA    R2,(ADLCSE2H-ADLCSELH)(R2) BUMP TO NEXT FIELD                    
         LA    R8,4(R8)            NEXT DISK ADDRESS                            
*                                                                               
         BCT   R0,AUDLSLLP                                                      
*                                                                               
AUDLSLDN DS    0H                  NOTHING SELECTED                             
         B     AUDLLST             GO LIST NEXT SCREEN                          
*                                                                               
AUDLSLFD DS    0H                  COMMENT SELECTED                             
*                                                                               
         MVC   TWAMKGD2,0(R8)      SAVE SELECTED RECORD                         
*                                                                               
         MVC   CONACT,=CL4'AUD '   CHANGE ACTION                                
*                                                                               
         BRAS  RE,AUDCOM           DISPLAY COMMENTS                             
*                                                                               
         B     AUDLCOMX                                                         
*                                                                               
*                                                                               
AUDLLST  DS    0H                  GO LIST COMMENTS                             
                                                                                
         BAS   R8,LOADDIS          PERFORM DISPLAY OF SELECTED ITEM             
*                                                                               
*                                  MODULE WILL RETURN APPROPRIATE               
*                                     ACTION MESSAGE                            
AUDL0180 DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),DUB       SET MESSAGE FOR DISPLAY                      
         GOTO1 VDISMSG,DMCB                                                     
         LA    R2,CONCACTH         POSITION CURSOR                              
         OI    6(R2),X'40'         INSERT CURSOR                                
         B     EXXMOD                                                           
*                                                                               
AUDLCOMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*        HANDLE AUDIT COMMENTS DISPLAY SCREEN                                   
*                                                                               
AUDCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWASCRN,X'BF'       SKIP IF SCREEN ALREADY DISPLAYED             
         BE    AUDSCRX                                                          
*                                                                               
         CLI   TWASCRN,X'BE'       SKIP IF LIST SCREEN DISPLAYED                
         BE    *+16                                                             
         XC    TWAMKGD2,TWAMKGD2   NO SELECTED COMMENTS ON SCREEN               
         XC    TWAMKGDS,TWAMKGDS   NO CURRENT  COMMENTS ON SCREEN               
*                                                                               
*        LOAD DETAIL SCREEN                                                     
*                                                                               
         NI    CONCNUMH+4,X'FF'-X'20'    TURN OFF PREVALID BIT                  
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR COMMENT  SCREEN)            
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVI   DMCB+7,X'BF'        DISPLAY SCREEN NUMBER                        
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # BF (AUDIT DISPLAY)             
*                                                                               
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
*                                                                               
         OI    TWAPROST,X'01'      FULL SIZE SUBSCREEN                          
         MVI   TWASCRN,X'BF'       LOAD NEW SCREEN NUMBER                       
*                                                                               
         LA    R8,CONCACTH         SET UP MAIN ACTION FIELD                     
*                                                                               
         XC    TWAMKGLA,TWAMKGLA   NO LAST COMMENT ON SCREEN                    
         XC    TWAMKGDA,TWAMKGDA   INIT RECORD LIST                             
         XC    TWAMKGDS,TWAMKGDS   NO CURRENT COMMENTS ON SCREEN                
*                                                                               
         B     AUD0130                                                          
*                                                                               
AUDSCRX  EQU   *                                                                
*                                                                               
*        DISPLAY SCREEN DISPLAYED                                               
*                                                                               
         LA    R8,CONCACTH         SET UP MAIN ACTION FIELD                     
*                                                                               
         CLC   8(4,R8),MYSPACES    CONTRACT FIELD SPACE-FILLED?                 
         BE    AUD0130             YES - FORCE 'AUD' INTO ACTION FIELD          
*                                                                               
         CLI   5(R8),0             NO  - ANYTHING IN FIELD?                     
         BNE   AUD0140             YES - LEAVE IT ALONE                         
*                                                                               
AUD0130  EQU   *                                                                
*                                                                               
         MVC   8(4,R8),=C'AUD '    INSERT AUDIT DISPLAY ACTION                  
         MVC   LASTCACT,=C'AUD '   INSERT INTO LAST ACTION ALSO                 
         MVI   5(R8),4             INSERT A LENGTH OF 4                         
         NI    CONCACTH+4,X'FF'-X'20'   TURN OFF PREVALID BIT                   
*                                                                               
AUD0140  EQU   *                                                                
*                                                                               
         BAS   R8,LOADDIS          PERFORM DISPLAY OF SELECTED ITEM             
*                                                                               
*                                  MODULE WILL RETURN APPROPRIATE               
*                                     ACTION MESSAGE                            
AUD0180  DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),DUB       SET MESSAGE FOR DISPLAY                      
         GOTO1 VDISMSG,DMCB                                                     
         LA    R2,CONCACTH         POSITION CURSOR                              
         OI    6(R2),X'40'         INSERT CURSOR                                
         B     EXXMOD                                                           
*                                                                               
AUDCOMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* AVNHOOK - HOOK TO INTERCEPT & HANDLE SPECIAL AVN BUY ACTION REQUESTS          
***********************************************************************         
AVNHOOK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    TWAFLAGS,X'FF'-TWAFLAVN DEFAULT FLAG OFF                         
*                                                                               
* CHECK FOR SPECIAL AVN ACTION                                                  
*                                                                               
         CLC   =C'BU#',BUYACT      BU#                                          
         BNE   *+14                                                             
         MVC   BUYACT,=C'BUY'                                                   
         B     AVN050                                                           
         CLC   =C'CH#',BUYACT      CH#                                          
         BNE   *+14                                                             
         MVC   BUYACT,=C'CHA'                                                   
         B     AVN050                                                           
         CLC   =C'DI#',BUYACT      DI#                                          
         BNE   *+14                                                             
         MVC   BUYACT,=C'DIS'                                                   
         B     AVN050                                                           
         CLC   =C'CA#',BUYACT      CA#                                          
         BNE   *+14                                                             
         MVC   BUYACT,=C'CAN'                                                   
         B     AVN050                                                           
         CLC   =C'DE#',BUYACT      DE#                                          
         BNE   *+14                                                             
         MVC   BUYACT,=C'DEL'                                                   
         B     AVN050                                                           
*                                                                               
         B     EXXMOD              NO AVN ACTION                                
*                                                                               
AVN050   DS    0H                                                               
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA                                 
         MVI   PROFEQU,0           FORCE REREAD OF PROFILES NEXT TX             
         OI    TWAFLAGS,TWAFLAVN   AVN IN PROGRESS                              
         B     EXXMOD                                                           
         LTORG                                                                  
***********************************************************************         
* MAKEGOOD OFFER ACTIONS ARE VALID ONLY IF CONTRACT HAS BEEN CONFIRMED          
* AT LEAST ONCE. ALSO, COMBO CONTRACTS ARE NOT SUPPORT HERE                     
*                                                                               
* DARE REVISION ORDER MUST BE APPROVED/REJECTED/CANCELLED                       
***********************************************************************         
CHECKMG  NTR1  BASE=*,LABEL=*      MUST BE CONFIRMED AT LEAST ONCE              
*                                                                               
         LA    R3,511              COMBO NOT SUPPORTED                          
         CLI   TWACOMBO,0                                                       
         BNE   ERROR                                                            
         CLC   =C'MGX',CONACT                                                   
         BE    CHKMGX                                                           
*                                                                               
         LA    R6,RCONREC          DARE ORDER                                   
         MVI   ELCODE,X'1D'        MAKEGOOD ACTIONS ARE INVALID                 
         BAS   RE,GETEL            FOR CORPORATE OR VARIOUS DARE ORDERS         
         BNE   CHKMG01A                                                         
         USING RCONDREL,R6                                                      
         LA    R3,692                                                           
         TM    RCONDRF2,X'80'+X'10'                                             
         BNZ   ERROR                                                            
* REPLACEMENT OFFERS CAN BE DONE ANYTIME (ONLY IF NON-DARE FOR NOW)             
* LINE CORRECTION CAN ONLY BE DONE WHEN CONTRACT IS NEVER CONFIRMED             
*                                                                               
* RNA MUST NOW BE DONE ONLY AFTER CONFIRMATION (SKUI 7/23/02)                   
*                                                                               
CHKMG01A DS    0H                                                               
*                                                                               
* DISALLOW RNA FOR COLUMBINE EC CHANGES CONTRACTS                               
*                                                                               
         CLC   =C'RNA',CONACT                                                   
         BE    CHKMG01B                                                         
         CLC   =C'RNA',BUYACT                                                   
         BNE   CHKMG02                                                          
                                                                                
CHKMG01B DS    0H                                                               
*                                                                               
* DISALLOW RNA FOR SZ, AM AND CQ                                                
*                                                                               
         LA    R3,INVACT                                                        
         CLC   =C'SZ',REPALPHA                                                  
         BE    ERROR                                                            
*        CLC   =C'AM',REPALPHA                                                  
*        BE    ERROR                                                            
         CLC   =C'CQ',REPALPHA                                                  
         BE    ERROR                                                            
*                                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                  RETRIEVE THE STATION RECORD                  
         LA    R6,RSTAREC                                                       
         USING RSTAREC,R6                                                       
         CLI   RSTATRAF,C'C'       COLUMBINE                                    
         BE    CHKMG01C                                                         
         CLI   RSTATRAF,C'J'       JDS 2000                                     
         BE    CHKMG01C                                                         
         CLI   RSTATRAF,C'K'       ENTERPRISE                                   
         BNE   CHKMG02                                                          
*                                                                               
CHKMG01C DS    0H                                                               
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKMG02                                                          
         USING RSTAXXEL,R6                                                      
         LA    R3,922                                                           
         TM    RSTAOPTB,X'20'      EC CHANGES?                                  
         BO    ERROR                                                            
         DROP  R6                                                               
*                                                                               
*&&DO                                                                           
         CLC   =C'RNA',CONACT                                                   
         BE    CHKMG01                                                          
         CLC   =C'RNA',BUYACT                                                   
         BE    CHKMG01                                                          
         CLC   =C'MGO',BUYACT                                                   
         BE    CHKMG01                                                          
         CLC   =C'MGO',BUYACT                                                   
         BNE   CHKMG02                                                          
*                                                                               
CHKMG01  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKMGX                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BNZ   CHKMGX                                                           
*                                                                               
* IF UNCONFIRMED, CHECK IF THIS IS A DARE ORDER                                 
*                                                                               
         LA    R3,912              CANNOT CREATE OFFER FOR UNCONFIRMED          
         LA    R6,RCONREC          DARE ORDER                                   
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKMGX                                                           
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'                                                   
         BO    ERROR                                                            
         B     CHKMGX                                                           
         DROP  R6                                                               
*&&                                                                             
CHKMG02  DS    0H                                                               
         CLC   =C'MGC',CONACT                                                   
         BE    CHKMGX                                                           
         CLC   =C'MGC',BUYACT                                                   
         BE    CHKMGX                                                           
*                                                                               
         LA    R3,479                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    ERROR                                                            
*                                                                               
         CLC   =C'MGO',CONACT                                                   
         BE    CHKMG10                                                          
         CLC   =C'MGO',BUYACT                                                   
         BNE   CHKMGX                                                           
*                                                                               
CHKMG10  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        DARE ORDER??                                 
         BAS   RE,GETEL                                                         
         BNE   CHKMGX                                                           
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q   EXTENDED ELEMENT??                           
         BL    CHKMGX              ONLY FOR REVISION                            
         OC    RCONDRRV,RCONDRRV   CONFIRMED/CANCELLED/NOT REVISION?            
         BZ    CHKMGX                                                           
         LA    R3,734              MUST PROCESS DARE REVISION ORDER             
         OC    RCONDRDD(4),RCONDRDD                                             
         BZ    ERROR               MUST HAVE DELIVERY DATE                      
         TM    RCONDRFG,X'40'+X'20'+X'10'                                       
         BZ    ERROR               MUST BE APPROVED/REJECTED/RECALLED           
*        CLC   RCONDRDA(4),RCONDRDD                                             
*        BH    CHKMGX                                                           
*        CLC   RCONDRDR(4),RCONDRDD                                             
*        BL    ERROR                                                            
*                                                                               
CHKMGX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************                                  
*              SPL                                                              
**********************************************                                  
SPLC00   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,368              INVALID ACTION CODE MESSAGE                  
         LA    R2,CONCACTH                                                      
         CLI   TWASCRN,X'D4'       SCREEN ALREADY UP?                           
         BNE   SPL10               NO  - DISPLAY AND LOAD IT                    
         TM    CONCNUMH+4,X'20'    YES - CONTRACT PREVIOUSLY VALID?             
         BNO   SPL20               NO  - CONTRACT CHANGED                       
         CLC   =C'SPL',CONACT      YES - ANOTHER DISPLAY REQUEST?               
         BE    SPL20               YES - DO IT AGAIN                            
         CLC   =C'CD',CONACT       YES - ANOTHER DISPLAY REQUEST?               
         BE    SPL20               YES - DO IT AGAIN                            
         CLC   =C'CLR',CONACT      YES - CLEAR REQUEST?                         
         BE    SPL25                                                            
         B     SPL30               NO  - REQUEST FOR CHANGE                     
*                                                                               
SPL10    EQU   *                                                                
         CLC   =C'CLR',CONACT      CLEAR REQUEST?                               
         BNE   SPL15                                                            
         LA    R2,CONCACTH                                                      
         LA    R3,670                                                           
         B     ERROR                                                            
SPL15    EQU   *                                                                
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         MVI   TWASCRN,X'D4'       LOAD NEW SCREEN NUMBER                       
         BAS   R8,FULLSCRN         FULL SCREEN CALL                             
*                                                                               
SPL20    EQU   *                                                                
         BAS   R8,LOADDIS          LOAD NEWLY DISPLAYED SCREEN                  
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'    TURN ON PREVALID BIT                         
         B     EXIT                                                             
SPL25    EQU   *                                                                
         BAS   R8,LOADCLR                                                       
         B     EXIT                                                             
*                                                                               
*  SPL SCREEN OVERLAYS ENTIRE TWENTY-FOUR LINES                                 
*                                                                               
SPL30    EQU   *                                                                
         BAS   R8,LOADEDT          EDIT/UPDATE FROM SAR/SPL SCRN                
         BAS   R8,DIS2                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
********************************************                                    
*              MON                                                              
********************************************                                    
MONB00   NTR1  BASE=*,LABEL=*                                                   
         CLI   TWASCRN,X'ED'                                                    
         BE    MONB20                                                           
         MVI   BYTE,X'ED'                                                       
         BAS   R8,NEWSCRN                                                       
         SPACE 1                                                                
MONB20   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         GOTO1 VCKTAREQ                                                         
         LA    R2,MONSTRTH                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               CUR TO $ FLD                                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
MON#00   NTR1  BASE=*,LABEL=*      SPECIAL MON# SUPPORT                         
         LA    R3,1025                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BE    MON#10                                                           
*                                                                               
         TM    RCONMODR+1,X'20'    MON DONE?                                    
         BO    ERROR                                                            
*                                                                               
MON#10   DS    0H                                                               
         CLI   TWASCRN,X'C9'                                                    
         BE    MON#20                                                           
         MVI   BYTE,X'C9'                                                       
         BAS   R8,NEWSCRN                                                       
         SPACE 1                                                                
MON#20   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         LA    R2,RC$ACDTH                                                      
         XIT1                                                                   
         EJECT                                                                  
MON$00   NTR1  BASE=*,LABEL=*      SCRIPT MON SUPPORT                           
         LA    R3,BACERR                                                        
         CLI   TWAOFFC,C'*'        DDS TERMINALS ONLY                           
         BE    MON$10              OK TO INVOKE                                 
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    ERROR                                                            
*                                                                               
MON$10   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
*                                                                               
         CLI   TWASCRN,X'EA'                                                    
         BE    MON$20                                                           
         MVI   BYTE,X'EA'                                                       
         BAS   R8,NEWSCRN                                                       
         BAS   R8,DIS1                                                          
         LA    R2,MO$MON1H                                                      
         B     EXXMOD                                                           
MON$20   DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         BAS   R8,DIS2                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* TMP BUYACTION ROUTINE                                                         
***********************************************************************         
TMP00    NTR1  BASE=*,LABEL=*                                                   
         CLC   TWASCRN,8(R5)                                                    
         BE    TMP20                                                            
         MVI   BYTE,X'DD'                                                       
         OI    TWAPROST,X'02'                                                   
         GOTO1 VLDSCRN,DMCB,(BYTE,CONCNUMX+8),0                                 
***>>>   BAS   R8,NEWSCRN                                                       
         BAS   R8,DIS1                                                          
         BAS   R8,LOADDIS                                                       
         B     TMPX                                                             
*                                                                               
TMP20    DS    0H                                                               
         BAS   R8,DIS2                                                          
         BAS   R8,LOADEDT                                                       
         B     TMPX                                                             
*                                                                               
TMPX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SPORTS BUY PROCESSING                                                         
**********************************************************************          
SBUY00   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,76                                                            
         CLI   TWACOMBO,0                                                       
         BNE   ERROR                                                            
*                                                                               
         LA    R3,253                                                           
         TM    RCONMODR+1,X'20'    HAS A MON BEEN DONE?                         
         BO    ERROR               NO                                           
*                                                                               
         CLC   TWASCRN,8(R5)                                                    
         BNE   SBUY0005                                                         
         CLC   BUYACT,SBSACT                                                    
         BE    SBUY0010                                                         
*                                                                               
SBUY0005 DS    0H                                                               
         MVC   BYTE,8(R5)                                                       
         BAS   R8,NEWSCRN          LOAD BUY SCREEN                              
*                                                                               
         GOTO1 =A(SBSCRN),RR=TWARELO1                                           
         MVC   SBSACT,BUYACT                                                    
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(3,R2),BUYACT                                                   
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         AHI   R2,SBSDATEH-CONBACTH                                             
         BAS   R8,DIS1                                                          
         B     EXIT                                                             
*                                                                               
SBUY0010 DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
*                                  DISPLAY NEW BUY NUMBER                       
         LA    R8,CONBNUMH-CONBACTH(R2)                                         
         MVC   8(L'CONBNUM,R8),MYSPACES                                         
         EDIT  (1,RBUYKLIN),(3,8(R8)),ALIGN=LEFT                                
         OI    6(R8),X'80'                                                      
*                                                                               
***>     BAS   R8,BUYDIS                                                        
         TM    TWAMGFLG,X'40'      GO MY OWN BUCKET UPDATE?                     
         BO    SBUY0030                                                         
         BAS   R8,BUCKUP                                                        
*                                                                               
SBUY0030 DS    0H                                                               
         BAS   R8,DIS2                                                          
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         MVC   8(L'CONBACT,R2),MYSPACES                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 VCKTAREQ                                                         
*                                                                               
         OI    CONBNUMH-CONBACTH+4(R2),X'20'                                    
*                                                                               
         B     EXIT                                                             
**********************************************************************          
* SPORTS BUY SCREEN FORMATTING                                                  
**********************************************************************          
SBSCRN   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 (RFGETTAB,VREPFACS),DMCB,('RTSPORTS',0)                          
         BE    *+6                                                              
         DC    H'0'                          MUST BE THERE                      
         L     R5,0(R1)                                                         
         MVC   HALF,0(R5)                    TABLE ENTRY LENGTH                 
         LA    R5,4(R5)                      START OF TABLE                     
SBSCRN10 DS    0H                                                               
         CLI   0(R5),X'FF'                   BUYACT NOT MATCHED                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BUYACT,0(R5)                                                     
         BE    SBSCRN20                                                         
         AH    R5,HALF                                                          
         B     SBSCRN10                                                         
*                                                                               
SBSCRN20 DS    0H                                                               
         MVC   SBSDCL1,4(R5)       DESC#1 LABEL                                 
         OI    SBSDCL1H+6,X'80'                                                 
         MVC   SBSDCL2,28(R5)      DESC#2 LABEL                                 
         OI    SBSDCL2H+6,X'80'                                                 
*                                                                               
         MVC   SBSTYP(3),=C'***'                                                
         MVC   SBSTYP+5(15),52(R5)   SPORTS BUY TYPE LABEL                      
         LA    R5,SBSTYP+20                                                     
         BCTR  R5,0                                                             
         CLI   0(R5),C' '                                                       
         BE    *-6                                                              
         MVC   3(3,R5),=C'***'                                                  
         OI    SBSTYPH+6,X'80'                                                  
*                                                                               
         XIT1                                                                   
**********************************************************************          
* SEND A WORKSHEET                                                              
**********************************************************************          
SEND00   NTR1  BASE=*,LABEL=*                                                   
         BAS   R8,LOADSCRN                                                      
         BAS   R8,CONDIS                                                        
*                                                                               
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    SEND03                                                           
*                                  IN CASE OF COMBO, MGO IS A SINGULAR          
         CLC   =C'MGS',CONACT      ACTION WITHIN A COMBO. SO DON'T              
         BE    SEND03              'DISTRIBUTE' THE MGS ACTION TO ALL           
*                                                                               
         XC    TWASPREF,TWASPREF   SET FLAG SO WE'LL CHECK FOR COMBO            
*                                  STATION SEND PREFERENCE IN RECNT63           
*                                  CHECK ORDER TOTAL                            
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBCON',0),RR=TWARELO1                
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBSND',0),RR=TWARELO1                
         XC    TWASPREF,TWASPREF   RESET FLAG                                   
         B     SEND15                                                           
*                                                                               
SEND03   BAS   R8,LOADEDT                                                       
SEND05   MVI   INTTYPE,C'O'           INDICATE ORDER WORKSHEET                  
*                                                                               
*- PROCESS BACKWARDS -- PUTS SENDER REPORT IN TWA AREA.                         
*                                                                               
         TM    TWAFLAGS,TWAFLHMQ                                                
         BO    SEND10              HOME MKT: JUST GEN STATION COPY              
         CLC   =C'MGS',CONCACT     ONLY RECEIVER GETS MAKEGOOD SEND             
         BE    SEND10                                                           
         CLC   =C'RSND',CONCACT    ONLY RECEIVER GETS RESEND                    
         BE    SEND08                                                           
         MVI   SENDPASS,3          3RD PASS = TO DESTINATION ID.                
         GOTO1 VLOAD,DMCB,(X'60',0)                                             
*                                                                               
SEND08   DS    0H                                                               
         TM    TWASTAOP,X'02'      PROF #16?                                    
         BZ    SEND10                                                           
         TM    TWASTAOB,X'04'      WEB? DON'T SEND XTRA FAX                     
         BO    SEND10                                                           
         MVI   SENDPASS,4          GEN EXTRA FAX COPY TO STATION                
         GOTO1 VLOAD,DMCB,(X'60',0)                                             
*                                                                               
SEND10   DS    0H                                                               
         MVI   SENDPASS,2          2ND PASS = TO RECEIVER                       
         GOTO1 VLOAD,DMCB,(X'60',0)   PRODUCE REPORT TO RECEIVER                
*                                                                               
         CLC   =C'SEND',CONCACT                                                 
         BE    SEND13                                                           
         CLC   =C'RSND',CONCACT                                                 
         BNE   SEND15                                                           
*                                                                               
SEND13   DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'7A',0) CHECK IF NEED TO EMAIL NOTICES              
*                                                                               
SEND15   DS    0H                                                               
         CLC   =C'MGS',CONCACT     SPECIAL CASE                                 
         BE    SEND20                                                           
         CLC   =C'RSND',CONCACT    ONLY RECEIVER GETS RESEND                    
         BNE   SEND20                                                           
*                                                                               
SEND18   DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,62                                                  
         B     SEND30                                                           
*                                                                               
SEND20   DS    0H                                                               
         TM    TWAFLAGS,TWAFLHMQ                                                
         BO    SEND25              HOME MKT: SKIP SENDER COPY                   
         MVI   SENDPASS,1          1ST PASS = TO SENDER                         
         GOTO1 VLOAD,DMCB,(X'60',0)   PRODUCE REPORT TO SENDER                  
*                                                                               
SEND25   DS    0H                                                               
         CLC   =C'MGS',CONCACT                                                  
         BNE   SEND30                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,136    MAKEGOOD OFFER SENT!                         
*                                                                               
SEND30   DS    0H                                                               
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONCACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*              DISPLAY CONTRACT AND HANDLE MULTIPLE BUY CHANGE                  
**********************************************************************          
MCIR00   NTR1  BASE=*,LABEL=*                                                   
         BAS   R8,LOADSCRN                                                      
         TM    CONCNUMH+4,X'20'    HAS CONTRACT BEEN PREVIOUSLY DISPLD          
         BO    MCIR5               YES                                          
         BAS   R8,CONDIS           NO                                           
         OI    CONCNUMH+4,X'20'                                                 
         OI    CONCNUMH+6,X'80'    XMIT                                         
*                                                                               
MCIR5    MVC   CONCACT,MYSPACES    CLEAR CONTRACT ACTION                        
         OI    CONCACTH+4,X'20'                                                 
         OI    CONCACTH+6,X'80'    XMIT                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   MCIR5A                                                           
         LA    R2,CONCNUMH                                                      
         LA    R3,769                                                           
         B     ERROR                                                            
*                                                                               
MCIR5A   DS    0H                                                               
         CLI   RCONKSTA+4,C' '     TV?                                          
         BE    MCIR6                                                            
         CLI   RCONKSTA+4,C'L'     TV?                                          
         BNE   MCIR7               NO                                           
*                                                                               
MCIR6    DS    0H                                                               
         LA    R2,MCIOFLH          OLD FLIGHT LABEL                             
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         LA    R2,MCIORLH          'OR' LABEL                                   
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         LA    R2,MCIOLDFH         OLD FLIGHT FIELD                             
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
         LA    R2,MCIFLTLH         NEW FLIGHT LABEL                             
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         LA    R2,MCIFLTH          NEW FLIGHT FIELD                             
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
         B     MCIR8                                                            
*                                                                               
MCIR7    DS    0H                                                               
         TM    MCIOLDFH+1,X'20'    OLD FLT FIELD PROT?                          
         BZ    MCIR8               NO - OK                                      
         LA    R2,MCIOFLH          OLD FLIGHT LABEL                             
         NI    1(R2),X'FF'-X'0C'   ZERO INTENSITY OFF                           
         OI    6(R2),X'80'                                                      
         LA    R2,MCIORLH          'OR' LABEL                                   
         NI    1(R2),X'FF'-X'0C'   ZERO INTENSITY OFF                           
         OI    6(R2),X'80'                                                      
         LA    R2,MCIOLDFH         OLD FLIGHT FIELD                             
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         OI    6(R2),X'80'                                                      
         LA    R2,MCIFLTLH         NEW FLIGHT LABEL                             
         NI    1(R2),X'FF'-X'0C'   ZERO INTENSITY OFF                           
         OI    6(R2),X'80'                                                      
         LA    R2,MCIFLTH          NEW FLIGHT FIELD                             
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         OI    6(R2),X'80'                                                      
*                                                                               
MCIR8    DS    0H                                                               
         TM    TWAPROST,X'24'      WAS SCREEN JUST LOADED                       
         BZ    MCIR10              NO                                           
         BAS   R8,DIS1                                                          
         MVC   CONBACT,MYSPACES                                                 
         OI    CONBACTH+6,X'80'    XMIT                                         
         LA    R2,MCIOLDBH         POSITION CURSOR                              
         B     EXIT                                                             
*                                                                               
MCIR10   DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         GOTO1 VCKTAREQ                                                         
         LA    R2,CONCNUMH         POSITION CURSOR AT CON NUMBER FIELD          
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*              DISPLAY CONTRACT AND ADD EPL 'EPLR'                              
**********************************************************************          
EPLR00   NTR1  BASE=*,LABEL=*                                                   
         BAS   R8,LOADSCRN                                                      
         CLI   BYTE3,0             AFTER FIRST TIME THROUGH, MAKE SURE          
         BE    EPLR2               CON ACT IS EMPTY                             
         LA    R2,CONCACTH                                                      
         CLI   5(R2),0                                                          
         BE    EPLR2                                                            
         LA    R3,347              RE-CONNECT TO CHANGE ACTIONS                 
         B     ERROR                                                            
         SPACE 1                                                                
EPLR2    TM    CONCNUMH+4,X'20'    IS CONTRACT DISPLAYED                        
         BO    EPLR5               YES                                          
         BAS   R8,CONDIS           NO-PUT IT OUT                                
         OI    CONCNUMH+4,X'20'                                                 
         OI    CONCNUMH+6,X'80'    SEND NUMBER FIELD                            
         SPACE                                                                  
EPLR5    EQU   *                                                                
         MVC   CONCACT,MYSPACES    CLEAR AND SEND ACTION FIELD                  
         OI    CONCACTH+4,X'20'                                                 
         OI    CONCACTH+6,X'80'    TRANSMIT                                     
         TM    TWAPROST,X'24'      SCREEN JUST LOADED                           
         BZ    EPLR10              NO                                           
         SPACE                                                                  
         BAS   R8,DIS1                                                          
         MVC   CONBACT,MYSPACES                                                 
         OI    CONBACTH+6,X'80'                                                 
         LA    R2,CONBNUMH         POSITION CURSOR AT FIRST                     
         SR    R1,R1               UNPROTECTED FIELD AFTER BUY NUMBER           
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                FIELD IS PROTECTED-TRY NEXT                  
         OI    6(R2),X'80'         SEND BACK CURSOR FIELD                       
         B     EXIT                                                             
         SPACE                                                                  
EPLR10   EQU   *                                                                
         LA    R2,CONCNUMH         POSITION CURSOR AT CONTRACT NUMBER           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL            ONLY ALLOW ACTION IF CONTRACT                
         BNE   EPLR15              DOES NOT HAVE EPL                            
         LA    R3,INVACT                                                        
         OI    6(R2),X'80'         SEND BACK NUMBER FIELD                       
         B     ERROR                                                            
         SPACE                                                                  
INVACT   EQU   12                                                               
         SPACE                                                                  
EPLR15   EQU   *                                                                
         BAS   R8,LOADEDT                                                       
         BAS   R8,DIS2                                                          
         OI    6(R2),X'80'         SEND BACK NUMBER FIELD                       
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              DELETE A CONTRACT                                                
***********************************************************************         
         SPACE 1                                                                
DELC00   NTR1  BASE=*,LABEL=*                                                   
         BAS   R8,LOADSCRN                                                      
         TM    TWAPROST,X'20'                                                   
         BO    DELC10              SCREEN CHANGE MUST DISPLAY                   
         TM    CONCNUMH+4,X'20'                                                 
         BO    DELC20              NUMBER IS NOT CHANGED                        
         SPACE 1                                                                
DELC10   BAS   R8,CONDIS                                                        
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'                                                 
         LA    R2,CONCACTH                                                      
         FOUT  (R2),CONACT,3                                                    
         OI    1(R2),X'01'                                                      
         B     EXIT                                                             
*                                                                               
DELC20   DS    0H                                                               
         CLI   TWACOMBO,0          SPECIAL ROUTINE FOR COMBO CONTRACTS          
         BE    DELC25                                                           
         GOTO1 =A(COMBORTN),DMCB,(RC),('QCOMBCON',0),RR=TWARELO1                
         B     DELC28                                                           
*                                                                               
DELC25   BAS   R8,LOADDIS                                                       
*                                                                               
DELC28   CLC   =C'RES',CONACT                                                   
         BE    RESC30                                                           
         CLC   =C'DEL',CONACT                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   R8,DIS2                                                          
         NI    CONCNUMH+4,X'DF'                                                 
         FOUT  CONCACTH,MYSPACES,15                                             
         LA    R2,CONCACTH                                                      
         B     EXIT                                                             
         SPACE 1                                                                
RESC30   BAS   R8,DIS2                                                          
         BAS   R8,CONDIS                                                        
         GOTO1 VCKTAREQ                                                         
*                                                                               
         TM    TWAPROST,X'01'                                                   
         BZ    RESC35                                                           
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
RESC35   GOTO1 VLDSCRN,DMCB,(X'FE',CONLAST),(X'C0',0)                           
         NI    TWAPROST,X'FF'-X'01'                                             
         FOUT  CONCACTH,MYSPACES,15                                             
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         LA    R2,CONBACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* SET SONNET BOXID TO EC                                                        
********************************************************************            
ECSONNET NTR1  BASE=*,LABEL=*                                                   
         TM    PROFILES+CNTSECDB,CNTSECDA                                       
         BZ    ECSX                                                             
*                                                                               
         MVC   KEY+28(4),TWAKADDR     CONTRACT ADDRESS                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC   READ CONTRACT FOR UPDATE                  
*                                                                               
*                                  DELETE EXISTING SONNET INFO                  
         GOTO1 VDELELEM,DMCB,(X'A3',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'A5',RCONREC)                                    
*                                                                               
         LA    R6,RCONREC          NO                                           
         MVI   ELCODE,X'20'        SENT ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         MVC   DUB,RCONSRV                                                      
         DROP  R6                                                               
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONSON,R6                                                       
         MVI   WORK,X'A3'                                                       
         MVI   WORK+1,RCONSOVQ+L'RCONSONM      40=LENGTH OF BASIC ELEM          
         MVC   RCONSONM(3),=C'ECD'                                              
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSDTE)  TODAY'S DATE                     
         MVC   RCONSRVR,DUB                    SENT BY REP' VER #               
         MVI   RCONSONS,X'03'                  SET STATUS                       
         DROP  R6                                                               
*                                                                               
* ADD THE ELEMENT                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R6),WORK,0                       
*                                                                               
* CREATE AUTO COMMENT                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONSCEL,R6                                                      
         MVI   WORK,X'A5'                                                       
         MVC   RCONSCCM(20),=C'EC ON XXXYY AT 00:00'                            
         GOTO1 DATCON,DMCB,(5,0),(12,RCONSCCM+6)  TODAY'S DATE                  
         ZAP   FULL,=P'0'                                                       
         THMS  DDSTIME=YES                        TIME                          
         ST    R0,DUB                                                           
         ST    R1,DUB+4                                                         
         AP    DUB(4),DUB+4(4)                                                  
         MVO   FULL,DUB(4)                                                      
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,FULL(1)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  RCONSCCM+15(2),DUB                                               
         MVI   RCONSCCM+17,C':'                                                 
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,FULL+1(1)                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  RCONSCCM+18(2),DUB                                               
*                                                                               
         MVI   RCONSCLN,RCONSCOV+20                                             
         DROP  R6                                                               
*                                                                               
* ADD THE ELEMENT AND WRITE THE RECORD                                          
*                                                                               
         LA    R6,RCONREC                                                       
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R6),WORK,0                       
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
*DELETE ANY SONNET EXISTING SONNET KEYS                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,RCONREC          SCANS ALL 8F'S FOR CONTRACT                  
K        USING RCON8FTP,KEY                                                     
         MVI   K.RCON8FTP,X'8F'                                                 
         MVC   K.RCON8FRP,RCONKREP                                              
         MVC   K.RCON8FSA,RCONKSTA                                              
*                                                                               
         TM    PROFILES+CNTRPEAB,CNTRPEAA   PROF 46 (CHECK FOR L)               
         BZ    ECS005                                                           
*                                                                               
         MVI   ELCODE,X'1E'        RANDOM FLAGS ELEM                            
         BAS   RE,GETEL                                                         
         BNE   ECS005                                                           
         TM    RCONRF1-RCONRFEL(R6),X'20' LOCAL ORDER?                          
         BZ    ECS005              NO                                           
         MVI   K.RCON8FTP,X'9F'    YES - USE NBC LOCAL 9F KEY                   
*                                                                               
ECS005   DS    0H                                                               
         LA    R6,RCONREC                                                       
         GOTO1 VHIGH                                                            
*                                                                               
ECS010   CLC   KEY(RCON8FDT-RCON8FTP),KEYSAVE                                   
         BNE   ECS030                                                           
         CLC   K.RCON8FCN,RCONKCON   CONTRACT NUMBER                            
         BE    ECS020                                                           
         DROP  K                                                                
*                                                                               
         GOTO1 VSEQ                                                             
         B     ECS010                                                           
*                                                                               
ECS020   DS    0H                  SHOULD ONLY BE ONE ACTIVE KEY                
         OI    KEY+27,X'80'        MARK DELETED - DON'T LOOK FOR MORE           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
*                                                                               
ECS030   DS    0H                                                               
         LA    R6,RCONREC                                                       
         XC    KEY,KEY                                                          
K        USING RCON8FTP,KEY                                                     
         MVI   K.RCON8FTP,X'8F'                                                 
         MVC   K.RCON8FRP,RCONKREP                                              
         MVC   K.RCON8FSA,RCONKSTA                                              
         MVC   K.RCON8FIN,=C'ECD'       'ECD' ID                                
         MVC   K.RCON8FCN,RCONKCON       CONTRACT NUMBER                        
         GOTO1 DATCON,DMCB,(5,0),(2,K.RCON8FDT)                                 
*                                                                               
         TM    PROFILES+CNTRPEAB,CNTRPEAA   PROF 46 (CHECK FOR L)               
         BZ    ECS035                                                           
         MVI   ELCODE,X'1E'        RANDOM FLAGS ELEM                            
         BAS   RE,GETEL                                                         
         BNE   ECS035                                                           
         TM    RCONRF1-RCONRFEL(R6),X'20' LOCAL ORDER?                          
         BZ    ECS035              NO                                           
         MVI   K.RCON8FTP,X'9F'    YES - USE NBC LOCAL 9F KEY                   
         DROP  K                                                                
*                                                                               
ECS035   DS    0H                                                               
         OI    DMINBTS,X'08'       READ FOR DELETED KEY                         
         GOTO1 VHIGH                                                            
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
         CLC   KEY(27),KEYSAVE     IS IT ON FILE ?                              
         BNE   ECS040              NO                                           
*                                                                               
         NI    KEY+27,X'FF'-X'80'  YES/TURN OFF DELETE                          
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         B     ECSX                                                             
*                                                                               
ECS040   DS    0H                                                               
         MVC   KEYSAVE+28,TWAKADDR     DISK ADDRESS                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEYSAVE,KEYSAVE              
*                                                                               
ECSX     B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* ADD FORECAST CONTRACT                                                         
********************************************************************            
ADDF00   NTR1  BASE=*,LABEL=*                                                   
         NI    TWAPROST,X'FF'-X'40' NO COMMENTS                                 
         CLC   TWASCRN,8(R5)                                                    
         BE    ADDF10                                                           
         TM    TWAPROST,X'01'                                                   
         BZ    ADDF05                                                           
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
ADDF05   GOTO1 VLDSCRN,DMCB,(8(R5),CONSHRTH),0                                  
         OI    TWAPROST,X'01'                                                   
                                                                                
ADDF10   DS    0H                                                               
         TM    TWAPROST,X'20'      20=CHANGED, 04=NEW SCREEN                    
         BZ    ADDF20                                                           
         BAS   R8,DIS1                                                          
         GOTO1 VLOAD,DMCB,(10(R5),0),=C'CLRS' FOUT SPACES                       
         MVC   CONCNUM,MYSPACES                                                 
         OI    CONCNUMH+6,X'80'                                                 
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R2,CONAGYH                                                       
         B     EXIT                                                             
                                                                                
ADDF20   DS    0H                                                               
         GOTO1 VLOAD,DMCB,(10(R5),0),=C'CLRN' FOUT NON-SPACES                   
         BAS   R8,INVAL            TURN OFF VALID BITS                          
         BAS   R8,LOADEDT                                                       
         BAS   R8,LOADDIS                                                       
         OI    CONCACTH+4,X'20'                                                 
         OI    CONCNUMH+4,X'20'                                                 
         BAS   R8,DIS2                                                          
         GOTO1 VCKTAREQ                                                         
         LA    R2,CONCACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
***>>>   PAY ROUTINE                                                            
********************************************************************            
* PAY S/P CODE CHANGE                                                           
********************************************************************            
PAYC00   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CONCACTH                                                      
         CLC   TWASCRN,8(R5)       SCREEN ALREADY UP?                           
         BE    PAYC10              YES - DON'T RELOAD IT                        
         TM    TWAPROST,X'01'                                                   
         BZ    PAYC05                                                           
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
PAYC05   GOTO1 VLDSCRN,DMCB,(8(R5),CONLAST),(X'E0',0)                           
         OI    PSPPSPH+4,X'20'     SET PREVIOUSLY VALID, WHICH WILL             
*                                     FORCE A DISPLAY BEFORE UPDATE             
PAYC10   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'                                                 
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
***>>>   PAY ROUTINE                                                            
********************************************************************            
* FORECAST CONTRACT CHANGE                                                      
********************************************************************            
CHAF00   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CONCACTH                                                      
         LA    R3,293                                                           
         BAS   RE,IS4CAST          IS THIS A FORECAST CONTRACT?                 
         BNZ   ERROR               CONTRACT IS NOT A FORECAST CONTRACT          
                                                                                
         NI    TWAPROST,X'FF'-X'40' NO COMMENTS                                 
         CLC   TWASCRN,8(R5)                                                    
         BE    CHAF05                                                           
         TM    TWAPROST,X'01'                                                   
         BZ    CHAF05                                                           
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
CHAF05   GOTO1 VLDSCRN,DMCB,(8(R5),CONSHRTH),0                                  
         OI    TWAPROST,X'01'                                                   
                                                                                
         TM    TWAPROST,X'20'                                                   
         BO    CHAF10              SCREEN CHANGE MUST DISPLAY                   
         TM    CONCNUMH+4,X'20'                                                 
         BO    CHAF20              NUMBER IS NOT CHANGED                        
                                                                                
CHAF10   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         OI    CONCNUMH+4,X'20'                                                 
         B     EXIT                                                             
                                                                                
CHAF20   DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         BAS   R8,LOADDIS                                                       
                                                                                
         BAS   R8,DIS2                                                          
         GOTO1 VCKTAREQ                                                         
                                                                                
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'                                                 
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
* FORECAST CONTRACT DISPLAY                                                     
********************************************************************            
DISF00   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CONCACTH                                                      
         LA    R3,293                                                           
         BAS   RE,IS4CAST          IS THIS A FORECAST CONTRACT?                 
         BNZ   ERROR               CONTRACT IS NOT A FORECAST CONTRACT          
                                                                                
         NI    TWAPROST,X'FF'-X'40' NO COMMENTS                                 
         CLC   TWASCRN,8(R5)                                                    
         BE    DISF010                                                          
         TM    TWAPROST,X'01'                                                   
         BZ    DISF010                                                          
         BAS   R8,FFDIS            RELOAD BASE SCREEN                           
DISF010  GOTO1 VLDSCRN,DMCB,(8(R5),CONSHRTH),(X'80',0)                          
         OI    TWAPROST,X'01'                                                   
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         MVC   CONCACT,MYSPACES                                                 
         OI    CONCACTH+6,X'80'                                                 
         OI    CONCACTH+4,X'20'    ACTION                                       
         OI    CONCNUMH+4,X'20'    AND NUMBER ARE VALID                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SWAPS TO SFM AND INVOKES CONBCOPY                                             
***********************************************************************         
DOCOPY   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,=C'REP'    TO THE CALLEE'S SYSTEM                       
         MVC   GLVXTOPR,=C'SFM'    TO THE CALLEE'S PROGRAM                      
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTF',CONCNUMH,,GLRBCOPY                            
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         LTORG                                                                  
***********************************************************************         
* DOMOVE- BUYACT MOVE ROUTINE                                                   
***********************************************************************         
DOMOVE   NMOD1 0,DOMOVE                                                         
         L     RC,0(R1)                                                         
*                                                                               
         LA    R3,12                                                            
         LA    RF,MOVEREPS        AUTHORIZED REPS                               
MOVE10   CLI   0(RF),X'FF'        END OF TABLE?                                 
         BE    ERROR              YES - REP NOT IN TABLE - INVALID ACT          
         CLC   REPALPHA,0(RF)     MATCH ON REP CODE                             
         BE    MOVE15             YES - OK                                      
         LA    RF,L'MOVEREPS(RF)  NO - NEXT IN TABLE                            
         B     MOVE10                                                           
*                                                                               
MOVE15   DS    0H                                                               
         LA    R3,607                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'             DOES DARE ELEM EXIST ON K?              
         BAS   RE,GETEL                                                         
         BNE   MOVE20                   NO - OK FOR MOVE                        
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'           DOES DARE LINK EXIST?                   
         BZ    MOVE20                                                           
         CLC   =C'MOVED',CONBACT   'MOVED' ACTION?                              
         BNE   ERROR                    YES - MOVE NOT ALLOWED                  
         LA    R3,764                                                           
         TM    RCONDRFG,X'20'+X'10' REJECTED OR RECALLED?                       
         BNZ   MOVE20               YES - OK TO PROCESS                         
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'             IS K PART OF COMBO?                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                    YES - MOVE NOT ALLOWED                  
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    ERROR               NO-MOVE NOT ALLOWED                          
         DROP  R6                                                               
*                                                                               
MOVE20   DS    0H                                                               
         LA    R3,688                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'             IS K PART OF COMBO?                     
         BAS   RE,GETEL                                                         
         BE    ERROR                    YES - MOVE NOT ALLOWED                  
*                                                                               
         LA    R3,PROPREPS         CHECK REP AGAINST LIST TO SEE IF             
MOVE22   CLI   0(R3),X'FF'         WE SKIP PROPOSAL REC CHECK                   
         BE    MOVE23              NO - DO CHECK                                
         CLC   RCONKREP,0(R3)                                                   
         BE    MOVE25              YES - SKIP CHECK                             
         LA    R3,2(R3)                                                         
         B     MOVE22                                                           
*                                                                               
MOVE23   LA    R3,563                                                           
         GOTO1 =A(PROCHK),DMCB,(RC),RR=TWARELO1                                 
         BE    ERROR                                                            
*                                                                               
MOVE25   DS    0H                                                               
         CLI   TWASCRN,X'D6'            SCRN LOADED ALREADY?                    
         BE    MOVE30                   YES - SKIP LOADING SCRN                 
         MVI   BYTE,X'D6'               SCRN NUM                                
         BAS   R8,NEWSCRN               LOAD SCRN                               
         BAS   R8,DIS1                  FIRST MSG/PROMPT                        
         MVC   MVKSREP,REPALPHA                                                 
         OI    MVKDREPH+6,X'40'         CUSROR TO DESTINATION FLD               
*                                                                               
         B     MOVEX                                                            
MOVE30   DS    0H                                                               
         BAS   R8,LOADDIS               CALL MOVE MODULE                        
MOVEX    XIT1                                                                   
*                                                                               
PROPREPS DC    C'CQ',C'AM',C'B3',C'B4',C'QT',C'OT',X'FF' SKIP CK                
*                                                                               
MOVEREPS DC    0H'0'  REP CODES AUTHORIZED FOR MOVE ACTION                      
         DC    C'SJ'                                                            
         DC    C'B3'                                                            
         DC    C'B4'                                                            
         DC    C'CQ'                                                            
         DC    C'AM'                                                            
         DC    C'BF'                                                            
         DC    C'KU'                                                            
         DC    C'KF'                                                            
         DC    C'EA'                                                            
         DC    C'CR'                                                            
         DC    C'K4'                                                            
         DC    C'RS'                                                            
         DC    C'S3'                                                            
         DC    C'QT'                                                            
         DC    C'OT'                                                            
*                                                                               
         DC    C'AQ'   INTEREP SUBSIDS                                          
         DC    C'MG'                                                            
         DC    C'IF'                                                            
         DC    C'I2'                                                            
         DC    C'I8'                                                            
         DC    C'I9'                                                            
         DC    C'RM'                                                            
         DC    C'D4'                                                            
         DC    C'S1'                                                            
         DC    C'CN'                                                            
         DC    X'FF'  END                                                       
***********************************************************************         
* BUY CONTRACT ROUTINES                                                         
* WILL BRANCH TO CORRESPONDING BUY ROUTINE                                      
***********************************************************************         
SUBACTS  NMOD1 0,*SACT*                                                         
         L     RC,0(R1)                                                         
                                                                                
QSUBORD  EQU   1                                                                
QSUBEQCD EQU   2                                                                
QSUBKCHG EQU   3                                                                
QSUBBUY1 EQU   4                                                                
QSUBBUY2 EQU   5                                                                
QSUBBUY3 EQU   6                                                                
QIFDARE  EQU   7                                                                
QSUBCFC  EQU   8                                                                
QSUBMCI  EQU   9                                                                
QSUBCOV  EQU   10                                                               
QSUBXRER EQU   11                                                               
QSUBSPCP EQU   12                                                               
QSUBKTKO EQU   13                                                               
QSUBHIAT EQU   14                                                               
QSUBEQTD EQU   15                                                               
QSUBBCD  EQU   16                                                               
                                                                                
         CLI   4(R1),QSUBCOV       COVERSHEET COMMENTS                          
         BE    COVR000                                                          
         CLI   4(R1),QSUBMCI       MCI                                          
         BE    MCIB00                                                           
         CLI   4(R1),QSUBORD       ORD                                          
         BE    ORDC00                                                           
         CLI   4(R1),QSUBEQCD      DX/CX                                        
         BE    EQCD00                                                           
         CLI   4(R1),QSUBKCHG      KCHG                                         
         BE    KCHG00                                                           
         CLI   4(R1),QSUBBUY1      BUY ACTION: 1ST COMPARES                     
         BE    BUYTEST1                                                         
         CLI   4(R1),QSUBBUY2      BUY ACTION: 2ND COMPARES                     
         BE    BUYTEST2                                                         
         CLI   4(R1),QSUBBUY3      BUY ACTION: 3RD COMPARES                     
         BE    BUYTEST3                                                         
         CLI   4(R1),QIFDARE       CHECK IF ORDER IS DARE LOCKED                
         BE    IFDARE                                                           
         CLI   4(R1),QSUBCFC       CFC                                          
         BE    CCMT00                                                           
         CLI   4(R1),QSUBXRER      BUY ACTION = XRER                            
         BE    XRER                                                             
         CLI   4(R1),QSUBSPCP      BUY ACTION = SPCP                            
         BE    SPCP                                                             
         CLI   4(R1),QSUBKTKO      TKO                                          
         BE    KTKO                                                             
         CLI   4(R1),QSUBHIAT      BUY ACTION = HIAT                            
         BE    HIAT                                                             
         CLI   4(R1),QSUBEQTD      TD/TC                                        
         BE    EQTD00                                                           
         CLI   4(R1),QSUBBCD       BCD/BCC                                      
         BE    BCD00                                                            
         DC    H'0'                                                             
                                                                                
SUBANO   DS    0H                                                               
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
         B     SUBACTSX                                                         
SUBAYES  DS    0H                                                               
         SR    R0,R0               SET CC = ZERO                                
SUBACTSX DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
MCIB00   DS    0H                                                               
         LA    R3,190                                                           
         CLI   RCONTYPE,C'B'       NO BUYS ON TYPE B CONTRACTS                  
         BE    ERROR                                                            
*                                                                               
         TM    RCONMODR+1,X'80'    ACE CONTRACT?                                
         BZ    MCIB0001            NO -SKIP CHECKS                              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         CLI   TWAACCS,C'$'        STATION                                      
         BE    MCIB000A                                                         
*                                                                               
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         TM    RCONSENF,X'10'      X'10'=STA. VERS. NOT ADVANCED                
         BZ    ERROR                                                            
         B     MCIB0001                                                         
MCIB000A DS    0H                                                               
         LA    R3,168              LATEST REP VERSION NOT YET SENT              
         TM    RCONSENF,X'20'      X'20'=REP VERS. NOT ADVANCED                 
         BZ    ERROR                                                            
         DROP  R6                                                               
*                                                                               
MCIB0001 DS    0H                                                               
         TM    CONCNUMH+4,X'20'    VLIDATED PREVIOUSLY?                         
         BZ    MCIB0002            NO FORCE SCREEN REPAINT                      
         CLI   TWASCRN,X'F2'                                                    
         BNE   MCIB0002                                                         
         BAS   R8,LOADEDT                                                       
         MVC   CONBACT,MYSPACES                                                 
         LA    R2,CONBACTH                                                      
         OI    6(R2),X'80'         XMIT                                         
         B     SUBACTSX                                                         
*                                                                               
MCIB0002 DS    0H                                                               
         MVI   BYTE,X'F2'          SET SCREEN NUMBER                            
         BAS   R8,NEWSCRN          LOAD NEW SCREEN                              
         BAS   R8,DIS1                                                          
         CLI   RCONKSTA+4,C' '     TV?                                          
         BE    MCIB0005                                                         
         CLI   RCONKSTA+4,C'L'     TV?                                          
         BNE   MCIB0010            NO                                           
*                                                                               
MCIB0005 DS    0H                                                               
         LA    R2,MCIOFLH          OLD FLIGHT LABEL                             
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MCIOLDFH         OLD FLIGHT FIELD                             
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MCIFLTLH         NEW FLIGHT LABEL                             
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MCIFLTH          NEW FLIGHT FIELD                             
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
*                                                                               
MCIB0010 DS    0H                                                               
         GOTOX (RFCMBNUM,VREPFACS),DMCB,RCONREC                                 
         CLI   0(R1),1                                                          
         BE    MCIB0020                                                         
         ZIC   R4,0(R1)                                                         
         ZICM  R6,1(R1),3                                                       
         LA    R6,2(R6)                                                         
         LA    R2,MCISTA1H                                                      
         LA    R3,MCIRAT1H                                                      
MCIB0015 DS    0H                                                               
         GOTOX (RFSTAOUT,VREPFACS),DMCB,0(R6),(1,8(R2))                         
         MVC   5(1,R2),4(R1)                                                    
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               NEXT STATION FIELD                           
         NI    1(R3),X'FF'-X'20'                                                
         OI    6(R3),X'80'                                                      
         ZIC   R0,0(R3)                                                         
         AR    R3,R0               NEXT RATE FIELD                              
         LA    R6,9(R6)                                                         
         BCT   R4,MCIB0015                                                      
*                                                                               
MCIB0020 EQU   *                                                                
         LA    R2,MCIOLDBH         OLD BUY NUMBER FIELD                         
         OI    6(R2),X'80'+X'40'   CURSOR HERE                                  
         B     SUBACTSX                                                         
         EJECT                                                                  
*                                                                               
COVR000  DS    0H                  COVERSHEET COMMENT                           
         CLC   =C'CVD',BUYACT      DISPLAY?                                     
         BE    COVR200                                                          
         CLC   =C'CVX',BUYACT      DELETE?                                      
         BE    COVR250                                                          
         CLC   =C'CVC',BUYACT      CHANGE?                                      
         BE    *+6                                                              
         DC    H'0'                NO OTHER POSSIBILITIES                       
*                                                                               
COVR100  DS    0H                  CHANGE/ADD COVERSHEET                        
         TM    RCONMODR+1,X'80'    ACE CONTRACT?                                
         BZ    COVR120             NO -SKIP CHECKS                              
         LA    R3,76               INVALID ACTION                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    ERROR                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         TM    RCONSENF,X'10'      X'10'=STA. VERS. NOT ADVANCED                
         BZ    ERROR                                                            
         DROP  R6                                                               
*                                                                               
COVR120  DS    0H                  BUILD COVER ELEM FOR ADD/CHANGE              
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING GLCOVNAM,R3                                                      
         MVC   GLCOVCON,RCONKCON                                                
         OI    GLCOVFLG,X'80'                                                   
*                                                                               
         GOTOX (RFCONLOW,VREPFACS),DMCB,RCONREC                                 
         MVC   GLCOVNAM+4(4),0(R1) DEFAULT NAME                                 
         MVI   GLCOVNAM,X'FF'                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   COVR300                                                          
         USING RCONCVEL,R6                                                      
         MVC   GLCOVNAM,RCONCVNM                                                
         B     COVR300                                                          
         DROP  R3,R6                                                            
*                                                                               
COVR200  DS    0H                  BUILD COVER ELEM FOR DISPLAY                 
         LA    R6,RCONREC                                                       
         LA    R3,796                                                           
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
*                                                                               
         USING RCONCVEL,R6                                                      
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING GLCOVNAM,R3                                                      
         MVC   GLCOVNAM,RCONCVNM                                                
         MVC   GLCOVCON,RCONKCON                                                
         B     COVR300                                                          
         DROP  R3,R6                                                            
*                                                                               
COVR250  DS    0H                  BUILD COVER ELEM FOR DELETE                  
         LA    R6,RCONREC                                                       
         LA    R3,796                                                           
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
*                                                                               
         USING RCONCVEL,R6                                                      
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING GLCOVNAM,R3                                                      
         MVC   GLCOVNAM,RCONCVNM                                                
         MVC   GLCOVCON,RCONKCON                                                
         OI    GLCOVFLG,X'40'      DELETE                                       
         DROP  R3,R6                                                            
*                                                                               
COVR300  DS    0H                  SWAP TO COVERSHEET PROGRAM                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*              WRITE COVER ELEMENT                                              
         GOTO1 (RF),DMCB,=C'PUTD',WORK,GLCOVLNQ,GLRCOVER                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK         BUILD CONTROL ELEM                             
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,=C'REP'    TO REP                                       
         MVC   GLVXTOPR,=C'COV'    TO COVERSHEET!                               
         MVI   GLVXFLG1,GLV1SEPS   SEPARATE SESSION                             
*              WRITE CONTROL ELEM                                               
         GOTO1 (RF),DMCB,=C'PUTD',WORK,24,GLVXCTL                               
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
ORDC00   DS    0H                                                               
         TM    CONCNUMH+4,X'20'    CONTRACT PREVIOUSLY VALID?                   
         BZ    ORDC10                                                           
         CLC   TWASCRN,8(R5)       APPROPRIATE ORD SCREEN DISPLAYED?            
         BE    ORDC20              YES                                          
ORDC10   DS    0H                  NO  - LOAD APPROPRIATE ORD SCREEN            
         TM    TWAPROST,X'01'      FULL SUBSCREEN LOADED?                       
         BZ    ORDC15              NO - LOAD OVER BASE                          
         GOTO1 =A(FFSCREEN),DMCB,(RC),RR=TWARELO1 RELOAD BASE                   
         BAS   R8,CONDIS                                                        
ORDC15   DS    0H                                                               
         MVC   TWASCRN,8(R5)                                                    
         BAS   R8,ORDSCRN          LOAD FULL SIZE SCREEN                        
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         OI    CONHTYPH+1,X'0C'    HIDE TYPE FIELD                              
         OI    CONHTYPH+6,X'80'                                                 
         OI    CONTYPEH+1,X'20'+X'0C'                                           
         OI    CONTYPEH+6,X'80'                                                 
         LA    R2,CONCACTH                                                      
         MVC   8(4,R2),=C'ORD '                                                 
         NI    4(R2),X'FF'-X'20'   TURN OFF PREV VAL CONACT                     
         OI    6(R2),X'80'+X'40'   XMIT + CURSOR                                
         OI    CONCNUMH+4,X'20'    SET CONTRACT PREVIOUSLY VALID                
         B     ORDCX                                                            
*                                                                               
ORDC20   DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS2                                                          
         LA    R2,CONCACTH                                                      
         MVC   8(4,R2),=C'ORD '                                                 
         NI    4(R2),X'FF'-X'20'   TURN OFF PREV VAL CONACT                     
         OI    6(R2),X'80'+X'40'   XMIT + CURSOR                                
         GOTO1 VCKTAREQ                                                         
         LA    R2,CONCACTH                                                      
         B     ORDCX                                                            
*                                                                               
ORDCX    DS    0H                                                               
         B     SUBACTSX                                                         
         EJECT                                                                  
EQCD00   DS    0H                                                               
         OC    TWAECON,TWAECON                                                  
         BNZ   EQCD10              NOT FOUND: NOT EC STATION                    
         LA    R3,396                                                           
         B     ERROR                                                            
                                                                                
EQCD10   DS    0H                                                               
         CLI   TWAECON,C'C'        COLUMBINE DX/CX                              
         BE    EQCD15                                                           
         CLI   TWAECON,C'J'                                                     
         BNE   EQCD30                                                           
                                                                                
EQCD15   DS    0H                                                               
         CLI   TWAACCS,C'$'        INVALID ACTION IF REP SIGNON                 
         BE    EQCD20                                                           
         LA    R3,BACERR                                                        
         B     ERROR                                                            
                                                                                
EQCD20   DS    0H                                                               
         MVI   10(R5),X'47'        JDS/2000 EDIT/DIS                            
         CLI   TWASCRN,X'E9'       JDS/2000 SCREEN                              
         BE    EQCD50                                                           
         MVI   BYTE,X'E9'          JDS/2000 SCREEN                              
         BAS   R8,NEWSCRN                                                       
         B     EQCD40                                                           
                                                                                
EQCD30   DS    0H                  BIAS DX/CX                                   
         CLI   TWAECON,C'B'                                                     
         BE    EQCD35                                                           
         LA    R3,397              INVALID TRAFFIC  FORMAT FOR THIS EC          
         B     ERROR                                                            
                                                                                
EQCD35   DS    0H                                                               
         MVI   10(R5),X'46'        BIAS EDIT/DIS                                
         CLI   TWASCRN,X'E8'                                                    
         BE    EQCD50                                                           
         MVI   BYTE,X'E8'                                                       
         BAS   R8,NEWSCRN                                                       
                                                                                
EQCD40   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         LA    R2,CONBACTH                                                      
         BAS   R8,DIS1                                                          
         B     EQCDX                                                            
                                                                                
EQCD50   DS    0H                                                               
         CLC   =C'CX',BUYACT                                                    
         BE    EQCD60                                                           
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         MVC   CONBACT(2),=C'DX'                                                
         B     EQCD70                                                           
                                                                                
EQCD60   DS    0H                                                               
         BAS   R8,LOADEDT          FOR CX                                       
         BAS   R8,LOADDIS          FOR CX                                       
         BAS   R8,DIS2                                                          
         MVC   CONBACT(2),=C'CX'                                                
                                                                                
EQCD70   DS    0H                                                               
         LA    R2,CONBACTH                                                      
         OI    6(R2),X'80'         XMIT                                         
         B     EQCDX                                                            
                                                                                
EQCDX    DS    0H                                                               
         B     SUBACTSX                                                         
         EJECT                                                                  
EQTD00   DS    0H                                                               
         OC    TWAECON,TWAECON                                                  
         BNZ   EQTD10              NOT FOUND: NOT EC STATION                    
         LA    R3,396                                                           
         B     ERROR                                                            
                                                                                
EQTD10   DS    0H                                                               
         MVI   WORK+46,X'C8'       SET COLUMBINE SCREEN NUMBER                  
         MVI   WORK+47,X'3C'       SET COLUMBINE OVERLAY NUMBER                 
         CLI   TWAECON,C'C'        COLUMBINE TD/TC                              
         BE    EQTD15                                                           
         CLI   TWAECON,C'J'        JDS/2000  TD/TC                              
         BE    EQTD12                                                           
         LA    R3,397              INVALID TRAFFIC  FORMAT FOR THIS TD          
         B     ERROR                                                            
EQTD12   EQU   *                                                                
         MVI   WORK+46,X'CA'       SET JDS/2000 SCREEN NUMBER                   
         MVI   WORK+47,X'3E'       SET JDS/2000 OVERLAY NUMBER                  
*                                                                               
                                                                                
EQTD15   DS    0H                                                               
**                                                                              
*    IS THIS SUPPOSED TO BE FROM THE STATION SIDE ONLY?                         
*                                                                               
         CLI   TWAACCS,C'$'        INVALID ACTION IF REP SIGNON                 
         BE    EQTD20                                                           
         LA    R3,BACERR                                                        
*        MVI   TEMP,8                                                           
         B     ERROR                                                            
                                                                                
EQTD20   DS    0H                                                               
         L     R6,ABUYFH                                                        
         AR    R6,RA               SET A(BUYACT SCREEN FIELD)                   
*                                                                               
         CLC   =C'TC',8(R6)        CHANGE?  SCREEN ALREADY UP                   
         BE    EQTD60                                                           
         XC    DUB,DUB                                                          
         CLI   TWAECON,C'C'        COLUMBINE TD/TC                              
         BNE   EQTD24              NO  - CHECK JDS/2000 SCREEN                  
         CLI   TWASCRN,X'C8'       SCREEN ALREADY UP?                           
         BE    EQTD40              YES - DON'T RELOAD SCREEN                    
         B     EQTD28              NO  - RELOAD C8 SCREEN                       
EQTD24   EQU   *                                                                
         CLI   TWASCRN,X'CA'       JDS/2000 SCREEN ALREADY UP?                  
         BE    EQTD40              YES - DON'T RELOAD SCREEN                    
         B     EQTD28              NO  - RELOAD CA SCREEN                       
EQTD28   EQU   *                                                                
         LA    R2,CONBNUMH                                                      
         CLI   5(R2),0             ANYTHING IN CON# FIELD?                      
         BZ    EQTD30              NO                                           
         ZIC   RF,5(R2)            YES - SAVE IT OFF                            
         STC   RF,HALF2            SAVE LENGTH OF FIELD                         
         BCTR  RF,0                BACK OFF 1 CHAR                              
         EX    RF,EQTDMOVE         SAVE FIELD IN DUB                            
         B     EQTD30              GO AND LOAD SCREEN                           
EQTDMOVE MVC   DUB(0),8(R2)        MOVE FROM CONBNUM                            
*                                                                               
EQTD30   EQU   *                                                                
         GOTO1 VLDSCRN,DMCB,(WORK+46,CONCNUMX+8),0                              
*                                  LOAD NEW SCREEN AFTER CON #                  
         OC    DUB,DUB             ANY VALUE IN DUB TO RESTORE?                 
         BZ    EQTD40              NO                                           
         LA    R2,COLBNUMH         SET A(BUY# FLD IN NEW SCREEN)                
         ZIC   RF,HALF2            RESET LENGTH OF FIELD                        
         STC   RF,5(R2)            INSERT NEW LENGTH                            
         OI    4(R2),X'80'         SET 'FIELD INPUT THIS TIME'                  
         BCTR  RF,0                BACK OFF 1 CHAR                              
         EX    RF,EQTDMOV2         LOAD DATA TO SCREEN                          
         B     EQTD40                                                           
EQTDMOV2 MVC   COLBNUM(0),DUB      MOVE TO COLBNUM                              
EQTD40   DS    0H                                                               
         GOTO1 VLOAD,DMCB,(WORK+47,0),=C'DISP'                                  
         OI    TWAPROST,X'02'                                                   
*                                  SET FULL-SIZE SUBSCREEN                      
*                                                                               
****>>>  BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         B     EQTDX                                                            
                                                                                
EQTD60   DS    0H                                                               
         GOTO1 VLOAD,DMCB,(WORK+47,0),=C'EDIT'                                  
***      BAS   R8,LOADEDT          FOR TC                                       
***      BAS   R8,LOADDIS          FOR TC                                       
         BAS   R8,DIS1                                                          
         MVC   8(2,R6),=C'TD'                                                   
         FOUT  (R2)                                                             
                                                                                
EQTD70   DS    0H                                                               
***      LA    R2,CONBACTH                                                      
***      OI    6(R2),X'80'         XMIT                                         
         B     EQTDX                                                            
                                                                                
EQTDX    DS    0H                                                               
         B     SUBACTSX                                                         
         EJECT                                                                  
***>>>>  BUYCODE                                                                
*                                                                               
* TAKEOVER                                                                      
*                                                                               
BCD00    DS    0H                                                               
         LA    R2,CONBACTH                                                      
         CLI   TWASCRN,X'CE'       SCREEN ALREADY UP?                           
         BNE   BCDO20              NO                                           
         BAS   R8,LOADEDT          YES -                                        
         BAS   R8,DIS2                                                          
         B     EXIT                                                             
*                                                                               
BCDO20   DS    0H                                                               
         MVI   BYTE,X'CE'          SET SCREEN FLAG                              
         BAS   R8,NEWSCRN          LOAD SCREEN                                  
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         B     EXIT                                                             
         EJECT                                                                  
***>>>>  BUYCODE                                                                
KCHG00   DS    0H                                                               
         CLI   TWASCRN,X'FE'                                                    
         BE    KCHG20                                                           
         MVI   BYTE,X'FE'                                                       
         BAS   R8,NEWSCRN                                                       
                                                                                
KCHG20   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
         LA    R2,CONBACTH                                                      
         B     SUBACTSX                                                         
*                                                                               
BUYTEST1 DS    0H                                                               
         LA    R1,OKSTLST2                                                      
*                                                                               
BUYT0005 EQU   *                                                                
         CLC   0(L'OKSTLST2,R1),BUYACT                                          
         BE    SUBAYES                                                          
         AHI   R1,L'OKSTLST2                                                    
         CLI   0(R1),X'FF'                                                      
         BNE   BUYT0005                                                         
         B     SUBANO                                                           
*                                                                               
* LIST OF VALID STATION ACTIONS                                                 
*                                                                               
OKSTLST2 DS    0CL3                                                             
         DC    C'DIS'                                                           
         DC    C'DSM'                                                           
         DC    C'DSS'                                                           
         DC    C'CHA'                                                           
         DC    C'UNI'                                                           
         DC    C'ORD'                                                           
         DC    C'DX '                                                           
         DC    C'CX '                                                           
         DC    C'TD '                                                           
         DC    C'TC '                                                           
         DC    C'HIS'                                                           
         DC    C'MGL'                                                           
         DC    C'MGO'                                                           
         DC    C'PFM'                                                           
         DC    C'CD '                                                           
         DC    C'PCF'                                                           
         DC    C'CVD'                                                           
         DC    C'MGC'                                                           
         DC    C'MGO'                                                           
         DC    C'MGM'                                                           
         DC    C'MGX'                                                           
         DC    C'MGB'                                                           
         DC    C'MGP'                                                           
         DC    C'MLR'                                                           
         DC    C'MLB'                                                           
         DC    C'RNA'                                                           
         DC    X'FF'                                                            
*                                                                               
BUYTEST2 DS    0H                                                               
         CLC   =C'SAR',BUYACT      ACCEPTABLE ACTION?                           
         BE    BUY20010            YES                                          
         CLC   =C'EPL',BUYACT                                                   
         BE    BUY20010                                                         
         CLC   =C'BOP',BUYACT                                                   
         BE    BUY20010                                                         
         CLC   =C'SPL',BUYACT                                                   
         BE    BUY20010                                                         
         CLC   =C'SASP',BUYACT                                                  
         BE    BUY20010                                                         
         CLC   =C'SASC',BUYACT                                                  
         BE    BUY20010                                                         
         CLC   =C'PD',BUYACT                                                    
         BE    BUY20010                                                         
         CLC   =C'PC',BUYACT                                                    
         BE    BUY20010                                                         
         CLC   =C'ORD',BUYACT                                                   
         BE    BUY20010                                                         
         CLC   =C'HIS',BUYACT                                                   
         BE    BUY20010                                                         
         CLC   =C'CD',BUYACT                                                    
         BE    BUY20010                                                         
         CLC   =C'PCF',BUYACT                                                   
         BE    BUY20010                                                         
         CLC   =C'CVD',BUYACT                                                   
         BE    BUY20010                                                         
*MN                                                                             
         CLC   =C'CHI',BUYACT                                                   
         BE    BUY20010                                                         
*MN                                                                             
         B     SUBANO              NO  - SET CC NOT = ZERO                      
BUY20010 EQU   *                                                                
         B     SUBAYES             SET CC = ZERO                                
*                                                                               
BUYTEST3 DS    0H                                                               
         CLC   =C'BUY',BUYACT      INVALID ACTION FOUND?                        
         BE    BUY30010            YES                                          
         CLC   =C'DIS',BUYACT                                                   
         BE    BUY30010                                                         
         CLC   =C'CAN',BUYACT                                                   
         BE    BUY30010                                                         
         CLC   =C'DEL',BUYACT                                                   
         BE    BUY30010                                                         
         CLC   =C'MBI',BUYACT                                                   
         BE    BUY30010                                                         
         CLC   =C'MCI',BUYACT                                                   
         BE    BUY30010                                                         
         CLC   =C'CD',BUYACT                                                    
         BNE   BUY30020                                                         
BUY30010 EQU   *                                                                
         B     SUBANO              NO  - SET CC NOT = ZERO                      
BUY30020 EQU   *                                                                
         B     SUBAYES             SET CC = ZERO                                
         EJECT                                                                  
**********************************************************************          
* IF CONTRACT IS A DARE ORDER (X'1D' ELEMENT EXISTS) AND CONTRACT HAS           
* NOT BEEN CONFIRMED, UPDATIVE BUY ACTIONS ARE NOT ALLOWED                      
**********************************************************************          
IFDARE   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IF ORDER IS DARE                             
         BAS   RE,GETEL                                                         
         BNE   SUBAYES                                                          
                                                                                
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      UPDATIVE ACTIONS ALLOWED IF UNLINKED         
         BZ    SUBAYES                                                          
         TM    RCONDRFG,X'01'      UPDATIVE ACTIONS ALLOWED IF TKO              
         BO    SUBAYES             (TAKEOVER)                                   
         TM    RCONDRFG,X'04'      KATZ EDI ORDER CAN DO EVERTHING              
         BO    SUBAYES                                                          
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    IFDARE10                                                         
*                                                                               
         TM    RCONDRF2,X'01'      UPDATIVE ALLOWED IF XML                      
         BO    SUBAYES                                                          
         TM    RCONDRF2,X'08'      UPDATIVE ACTIONS ALLOWED IF REMOVED          
         BO    SUBAYES                                                          
*                                                                               
         TM    RCONDRF2,X'10'      IF TRUE POOL DARE ORDER, USER                
         BZ    IFDARE05            CANNOT DO MAKEGOODS                          
         LA    R3,692                                                           
         CLC   =C'MG',BUYACT                                                    
         BE    ERROR                                                            
         CLC   =C'MG',CONACT                                                    
         BE    ERROR                                                            
*                                                                               
IFDARE05 DS    0H                                                               
         OC    RCONDRRV,RCONDRRV   OKAY TO UPDATE IF REVISION                   
         BZ    IFDARE07            UNLESS RECALL IN PROGRESS                    
         TM    RCONDRFG,X'10'+X'40'   OR APPROVED                               
         BNZ   IFDARE30                                                         
         B     SUBAYES                                                          
*                                                                               
IFDARE07 DS    0H                                                               
         TM    RCONDRF2,X'20'      IF CONFIRMED VARIOUS, USER                   
         BZ    IFDARE08            CANNOT UPDATE CONTRACT                       
         LA    R3,692                                                           
         CLC   =C'CHA',BUYACT      THESE ACTIONS ARE NOT ALLOWED                
         BE    ERROR                                                            
         B     IFDARE20            CANNOT UPDATE CONTRACT                       
                                                                                
IFDARE08 DS    0H                  NO UPDATIVE ACTIONS FOR VAR ORDERS           
         TM    RCONDRF2,X'80'                                                   
         BZ    IFDARE10                                                         
         LA    R3,692                                                           
         B     IFDARE20                                                         
         DROP  R6                                                               
                                                                                
IFDARE10 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   IFDARE20                                                         
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20' AND ORDER IS NOT CONFIRMED OR               
         BNZ   SUBAYES             CONFIRMED PREVIOUSLY                         
         DROP  R6                                                               
                                                                                
IFDARE20 DS    0H                  NO UPDATEIVE ACTION ALLOWED                  
*        CLC   =C'MG',BUYACT                                                    
*        BE    ERROR                                                            
*        CLC   =C'MG',CONACT                                                    
*        BE    ERROR                                                            
IFDARE30 CLC   =C'BUY',BUYACT      THESE ACTIONS ARE NOT ALLOWED                
         BE    ERROR                                                            
         CLC   =C'DEL',BUYACT                                                   
         BE    ERROR                                                            
         CLC   =C'CAN',BUYACT                                                   
         BE    ERROR                                                            
         CLC   =C'MBI',BUYACT                                                   
         BE    ERROR                                                            
         CLC   =C'MCI',BUYACT                                                   
         BE    ERROR                                                            
         CLC   =C'TMP',BUYACT                                                   
         BE    ERROR                                                            
         B     SUBAYES                                                          
         EJECT                                                                  
**********************************************************************          
* CONFIRMATION COMMENT (CFC)                                                    
**********************************************************************          
CCMT00   DS    0H                                                               
         TM    CONBACTH+4,X'20'    CONTRACT PREVIOUSLY VALID?                   
         BZ    CCMT10                                                           
         CLC   TWASCRN,8(R5)       APPROPRIATE ORD SCREEN DISPLAYED?            
         BE    CCMT20              YES                                          
CCMT10   DS    0H                                                               
         LA    R6,RCONREC          CHECK FOR DARE POL/VAR ORDER                 
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   CCMT15              NOT DARE ORDER, OK                           
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDRLQ   OLD LEN DARE ELEM?                           
         BE    CCMT15              YES, CAN'T BE POL/VAR                        
         TM    RCONDRF2,X'80'+X'10'   POL/VAR?                                  
         BZ    CCMT15                 NO - OK                                   
         LA    R3,703                 INVALID ACTION FOR THIS ORDER             
         B     ERROR                  OUTTA HERE                                
         DROP  R6                                                               
*                                                                               
CCMT15   BAS   R8,CONDIS           LOAD APPROPRIATE ORD SCREEN                  
         MVC   BYTE,8(R5)                                                       
         BAS   R8,NEWSCRN          LOAD CF COMMENT SCREEN                       
         BAS   R8,LOADDIS                                                       
         OI    CONCNUMH+4,X'20'    SET CONTRACT PREVIOUSLY VALID                
         OI    CONCNUMH+6,X'80'                                                 
         B     CCMTX                                                            
*                                                                               
CCMT20   DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         OI    CONCNUMH+4,X'20'    SET CONTRACT PREVIOUSLY VALID                
         OI    CONCNUMH+6,X'80'                                                 
         LA    R2,CONBACTH                                                      
CCMTX    DS    0H                                                               
         B     SUBACTSX                                                         
*                                                                               
* EXCLUDE RER                                                                   
XRER     DS    0H                                                               
         MVC   KEY+28(4),TWAKADDR     CONTRACT ADDRESS                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC   READ CONTRACT FOR UPDATE                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'2F'        GET NEW RER ELEM                             
         BAS   RE,GETEL                                                         
         BNE   XRER20              NOT THERE, MUST ADD ONE                      
         USING RCONRER,R6                                                       
         XI    RCONFRER,X'01'      EXCLUDE/UNEXCLUDE RER                        
*                                                                               
         TM    RCONFRER,X'01'         EXCLUDE RER?                              
         BZ    XRER15               NO, DATE STAMP AS UNEXCLUDED                
         GOTO1 DATCON,DMCB,(5,WORK),(2,RCONXRER)                                
*                                   USE TODAY'S DATE                            
         B     XRER25                                                           
*                                                                               
XRER15   DS    0H                  UNEXCLUDE RER                                
         GOTO1 DATCON,DMCB,(5,WORK),(2,RCONURER)                                
         B     XRER25                                                           
         DROP  R6                                                               
*                                                                               
XRER20   DS    0H                  ADD RER ELEM AND FLIP BIT                    
         USING RCONRER,R6                                                       
         LA    R6,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK,X'2F'          ELCODE                                       
         MVI   WORK+1,18           ELEM LENGTH                                  
         MVI   RCONFRER,1          EXCLUDE RER                                  
         GOTO1 DATCON,DMCB,(5,WORK),(2,RCONXRER)                                
*                                                                               
         LA    R6,RCONREC                                                       
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
XRER25   DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         BAS   R8,DIS1                                                          
*                                                                               
XRERX    LA    R2,CONBACTH                                                      
         B     SUBACTSX                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* SPL COPY                                                                      
*                                                                               
SPCP     DS    0H                                                               
         CLI   TWASCRN,X'DB'                                                    
         BE    SPCP01                                                           
         MVI   BYTE,X'DB'                                                       
         BAS   R8,NEWSCRN                                                       
*                                                                               
         MVC   KEY+28(4),TWAKADDR     CONTRACT ADDRESS                          
         GOTO1 VGETREC,DMCB,RCONREC   READ CONTRACT                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'        SPL PRESENT?                                 
         LA    R3,819              NO SPL                                       
         BAS   RE,GETEL                                                         
         BNE   ERROR               NO SPL, ERROR                                
*                                                                               
         L     R2,ABUYFH                                                        
         LA    R2,CONBACTH                                                      
         MVC   8(4,R2),=C'SPCP'                                                 
         OI    6(R2),X'80'                                                      
         BAS   R8,DIS1             ENTER CONTRACT DATA                          
         LA    R2,SCOCPO1H                                                      
         B     SPCPX                                                            
*                                                                               
* CODE TO CHECK FOR CURRENT K'S SPL ELEM                                        
SPCP01   DS    0H                                                               
         MVC   KEY+28(4),TWAKADDR     CONTRACT ADDRESS                          
         GOTO1 VGETREC,DMCB,RCONREC   READ CONTRACT                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'        SPL PRESENT?                                 
         LA    R3,819              NO SPL                                       
         BAS   RE,GETEL                                                         
         BNE   ERROR               NO SPL, ERROR                                
*                                                                               
SPCP10   BAS   R8,LOADEDT                                                       
         BAS   R8,DIS2                                                          
         LA    R2,CONBACTH                                                      
SPCPX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* TAKEOVER                                                                      
*                                                                               
KTKO     DS    0H                                                               
         LA    R2,CONBACTH                                                      
         LA    R3,453              CONTRACT ALREADY LINKED                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   KTKO10                                                           
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'01'                                                   
         BZ    KTKO20                                                           
         DROP  R6                                                               
*                                                                               
KTKO10   DS    0H                                                               
         CLI   TWASCRN,X'D7'                                                    
         BNE   KTKO20                                                           
         BAS   R8,LOADEDT                                                       
         BAS   R8,DIS2                                                          
         B     EXIT                                                             
*                                                                               
KTKO20   DS    0H                                                               
         MVI   BYTE,X'D7'                                                       
         BAS   R8,NEWSCRN                                                       
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS1                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              HIAT(US)                                                         
         SPACE                                                                  
HIAT     DS    0H                                                               
         CLI   TWASCRN,X'D5'                                                    
         BNE   HIAT05                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS DARE AGY ORDER ELEM THERE?                
         BAS   RE,GETEL                                                         
         BNE   HIAT10              NO, CONTINUE                                 
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      YES, IS CONTR LINKED TO AGY ORDER?           
         BO    HIAT08              YES, DON'T DO ANYTHING                       
         B     HIAT10                                                           
         DROP  R6                                                               
*                                                                               
HIAT05   DS    0H                                                               
         MVI   BYTE,X'D5'                                                       
         BAS   R8,NEWSCRN                                                       
*                                                                               
HIAT08   DS    0H                                                               
         BAS   R8,LOADDIS                                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS DARE AGY ORDER ELEM THERE?                
         BAS   RE,GETEL                                                         
         BNE   HIAT09              NO, CONTINUE                                 
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      YES, IS CONTR LINKED TO AGY ORDER?           
         BZ    HIAT09              YES, DON'T DO ANYTHING                       
         DROP  R6                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),=H'80'                                                 
         GOTO1 VDISMSG,DMCB                                                     
         B     HIAT20                                                           
*                                                                               
HIAT09   DS    0H                                                               
         BAS   R8,DIS1                                                          
         B     HIAT20                                                           
*                                                                               
HIAT10   DS    0H                                                               
         BAS   R8,LOADEDT                                                       
         BAS   R8,LOADDIS                                                       
         BAS   R8,DIS2                                                          
*                                                                               
HIAT20   DS    0H                                                               
         LA    R2,CONBACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MGOPROC ROUTINE                                                               
*    TRANSFER TO MAKEGOOD PROCESSING SCREEN DIRECTLY FROM BASE                  
***********************************************************************         
MGOPROC  NMOD1 0,*MGPR*                                                         
         L     RC,0(R1)                                                         
         XC    TWAMKGDS,TWAMKGDS   CLEAR ADDR OF PRIMARY RECORD                 
         XC    TWAMKGD2,TWAMKGD2   CLEAR ADDR OF SELECTION                      
         MVI   TWAMKGL#,0          CLEAR LINE NUMBER                            
         NI    TWAMKGFG,X'FF'-X'E8'                                             
*                                  TURN OFF M/G FLAGS                           
*                                  CHECK FOR NEW BUY OFFER FIRST                
         LA    R3,BUYERR           SET ERROR MESSAGE                            
         TM    TWAPROST,X'01'      FULL-SIZE SUBSCREEN LOADED?                  
         BO    MGOP0200            YES - NO BUYACT FIELD ON SCREEN              
         LA    R2,CONBNUMH         BUY # FOR NEW OFFER?                         
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BZ    MGOP0200            NO  - MUST BE PRESENT                        
         CLI   8(R2),C'B'          BONUS MAKEGOOD OFFER?                        
         BNE   MGOP0090                                                         
         OI    TWAMKGFG,X'08'      FLAG BONUS                                   
         B     MGOP0100                                                         
*                                                                               
MGOP0090 EQU   *                                                                
         GOTO1 VPACK               PACK/VALIDATE BUY NUMBER                     
         LTR   R0,R0               ERROR?                                       
         BZ    MGOP0200            YES                                          
         ST    R0,TWAMKGDS         LOAD LINE NUMBER                             
         GOTO1 GETBUYDA            RETRIEVE BUY DISK ADDRESS                    
         LA    R3,BUYERR                                                        
         BNZ   ERROR               ERROR - NO BUY RECORD WITH THAT #            
*                                                                               
*                                  LOAD MAKEGOOD OFFER SCREEN                   
MGOP0100 EQU   *                                                                
         NI    CONCNUMH+4,X'DF'    TURN OFF PREVALID BIT                        
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR MKGOOD SCREEN)              
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVI   DMCB+7,X'E5'        SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # E5 (MAKEGOOD SCREEN)           
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         OI    TWAPROST,X'01'      FULL-SIZE SUBCREEN                           
         MVI   TWASCRN,X'E5'       LOAD NEW SCREEN NUMBER                       
         SR    R0,R0               RESET CC TO ZERO                             
         B     MGOP0240            EXIT W/CC = ZERO: OKAY                       
MGOP0200 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO: ERROR                     
MGOP0240 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  GETBUYDA:  RETRIEVE DISK ADDRESS FOR THIS RECORD, INSERT INTO                
*      TWAMKGDS FIELD, SET ERROR FLAG IF NOT FOUND.                             
*                                                                               
GETBUYDA NTR1                                                                   
         XC    RBUYREC(32),RBUYREC                                              
         MVI   RBUYKTYP,X'0B'      SET KEY FOR BUY RECORD                       
         MVC   RBUYKREP,TWAAGY     INSERT POWER CODE                            
*                                                                               
         OI    CONCNUMH+4,X'08'    FORCE NUMERIC                                
         GOTOX (RFCONNUM,VREPFACS),DMCB,(4,CONCNUMH),(3,RBUYKCON)               
*                                                                               
         MVC   KEY,RBUYREC         LOAD KEY FROM RECORD                         
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
GETB0010 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY - THROUGH CON#                      
         BE    GETB0020            YES - CHECK FOR BUY LINE #                   
         LTR   RB,RB               NO KEY FOUND - SET CC NOT ZERO               
         B     GETB0080                                                         
GETB0020 EQU   *                                                                
         CLC   TWAMKGDS+3(1),KEY+RBUYKLIN-RBUYKEY                               
*                                  LINE NUMBER FOUND?                           
*        BNE   GETB0040            NO  - GO FOR NEXT LINE                       
*        CLC   KEY+RBUYKLIN-RBUYKEY(1),KEY+RBUYKMLN-RBUYKEY                     
*                                  YES - MASTER LINE = LINE #?                  
         BE    GETB0060            YES - LINE FOUND                             
GETB0040 EQU   *                                                                
         GOTO1 VSEQ                GET NEXT RECORD                              
         B     GETB0010            GO BACK FOR COMPARISON                       
GETB0060 EQU   *                                                                
         MVC   TWAMKGDS,KEY+28     SET DISK ADDRESS                             
         MVC   TWAMKGL#,KEY+RBUYKLIN-RBUYKEY                                    
*                                  SAVE LINE NUMBER                             
         OI    TWAMKGFG,X'40'      INSERT 'ADD' FLAG                            
         OI    TWAMKGFG,X'20'      INDICATE 'FROM BUYLINE'                      
*                                                                               
         SR    R0,R0               SET CC = ZERO                                
GETB0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* RDBUY - PREREAD BUYREC TO DETERMINE IF SPORTS BUY                             
*                                                                               
RDBUY    NTR1  BASE=*,LABEL=*                                                   
         NI    TWAFLAGS,X'FF'-TWABYRDQ DEFAULT FLAG OFF                         
         MVI   RBUYKTYP,11         BUY REC TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
***>     LA    R2,CONBNUMH-CONBACTH(R2)                                         
         ZIC   R0,0(R2)       BUYNUM LABEL                                      
         AR    R2,R0                                                            
         ZIC   R0,0(R2)       BUYNUM FIELD                                      
         AR    R2,R0                                                            
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    RDBUYX1                                                          
         CH    R0,=H'254'                                                       
         BH    RDBUYX1                                                          
         STC   R0,RBUYKLIN         LINE NUMBER                                  
         STC   R0,RBUYKMLN         MASTER LINE NUMBER                           
         MVC   RBUYKPLN,=3X'FF'                                                 
         MVC   KEY,RBUYREC                                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDBUYX1                                                          
         MVC   TWABADDR,KEY+28     SAVE DISK ADDR                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         OI    TWAFLAGS,TWABYRDQ   BUY HAS BEEN PREREAD                         
RDBUYX0  CR    RB,RB                                                            
         B     *+6                                                              
RDBUYX1  LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
***>>>   CONFLIST:  RELOCATED CODE FOR ADDRESSABILITY                           
CONFLIST NMOD1 0,*CONFLI*                                                       
         L     RC,0(R1)                                                         
*                                                                               
*                  ** FIRST CHECK: CONTRACT PROF 23 **                          
         TM    PROFILES+CNTPORGB,CNTPORGA                                       
         BZ    CLIS0020                                                         
         OI    TWACFFLG,X'40'        YES TURNAROUND REQUEST                     
         NI    TWACFFLG,X'FF'-X'80'  NO REP CONFIRMATION                        
         NI    TWACFFLG,X'FF'-X'20'  NO CONFIRMED ORD WRKSHT                    
*                                                                               
CLIS0020 DS    0H  ** NEXT CHECK: CONTRACT PROF 8 **                            
         CLC   RCONKOFF,=C'NY'                                                  
         BNE   CLIS0040                                                         
         TM    PROFILES+CNTCONFB,CNTCONFA                                       
         BZ    CLIS0040                                                         
         OI    TWACFFLG,X'40'        YES TURNAROUND REQUEST                     
         NI    TWACFFLG,X'FF'-X'80'  NO REP CONFIRMATION                        
         NI    TWACFFLG,X'FF'-X'20'  NO CONFIRMED ORD WRKSHT                    
*                                                                               
CLIS0040 DS    0H  ** NEXT CHECK: CONTYPE OPTION #3 **                          
         TM    TWAPRFA,X'20'       CONTYPE OPTION TO CONF ORD WRKSHT            
         BZ    CLIS0060                                                         
         OI    TWACFFLG,X'20'        YES CONFIRMED ORD WRKSHT                   
         NI    TWACFFLG,X'FF'-X'80'  NO REP CONFIRMATION                        
         NI    TWACFFLG,X'FF'-X'40'  NO TURNAROUND REQUEST                      
*                                                                               
CLIS0060 DS    0H  ** NEXT CHECK: OFFICE PROF #3 **                             
         CLI   TWAOCFTA,C'Y'       OFFICE PROFILE SET TO BYPASS T/A?            
         BNE   CLIS0080                                                         
         OI    TWACFFLG,X'80'        YES REP CONFIRMATION                       
         NI    TWACFFLG,X'FF'-X'20'  NO CONFIRMED ORD WRKSHT                    
         NI    TWACFFLG,X'FF'-X'40'  NO TURNAROUND REQUEST                      
                                                                                
CLIS0080 DS    0H  ** NEXT CHECK: AGY OPTION #3 **                              
         CLI   TWAACFPT,C'Y'       AGY OPT#3 (WRKSHT INSTEAD OF K)?             
         BNE   CLIS0100                                                         
         OI    TWACFFLG,X'20'        YES CONFIRMED ORD WRKSHT                   
         NI    TWACFFLG,X'FF'-X'80'  NO REP CONFIRMATION                        
         NI    TWACFFLG,X'FF'-X'40'  NO TURNAROUND REQUEST                      
*                                                                               
CLIS0100 DS    0H  ** NEXT CHECK: AGY OPTION #2 **                              
         CLI   TWAAEASY,C'Y'                                                    
         BNE   CLIS0120                                                         
         CLC   =C'CFX',CONACT      CFX ACTION?                                  
         BE    *+8                 NO AGY COPY FOR CFX                          
         OI    TWACFFLG,X'10'      YES AGENCY FAXED CONTRACT COPY               
*                                                                               
CLIS0120 DS    0H  ** NEXT CHECK: CFC RECORD PRESENT? **                        
         TM    TWACFFLG,X'08'      CFC RECORD PRESENT?                          
         BZ    CLIS0140                                                         
         OI    TWACFFLG,X'20'        YES CONFIRMED ORD WRKSHT                   
         NI    TWACFFLG,X'FF'-X'80'  NO REP CONFIRMATION                        
         NI    TWACFFLG,X'FF'-X'40'  NO TURNAROUND REQUEST                      
         NI    TWACFFLG,X'FF'-X'10'  NO AGY FAXED COPY                          
*                                                                               
CLIS0140 DS    0H  ** PUT FURTHER CHECKS HERE **                                
         XIT1                                                                   
         LTORG                                                                  
***>>>   CONFLIST END                                                           
***********************************************************************         
* CHKCFC - CHECKS IF THERE IS A CFC RECORD FOR CONTRACT                         
*          SETS X'08' IN TWACFFLG IF CFC PRESENT                                
***********************************************************************         
CHKCFC   NMOD1 0,*CHKCFC*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         BAS   RE,SETCOMBO                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,RCFCKTYQ                                                     
         MVC   KEY+21,RCONKREP                                                  
         MVC   KEY+23,FULL                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXXMOD                                                           
         OI    TWACFFLG,X'08'      CFC COMMENT PRESENT                          
         B     EXXMOD                                                           
*                                                                               
*  IF COMBO ORDER, RETURNS LOWEST K NUMBER IN COMBO IN 'FULL' ELSE              
*  RETURNS K NUMBER                                                             
*                                                                               
SETCOMBO NTR1                                                                   
         MVC   FULL,RCONKCON                                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   SCMBX                                                            
*                                                                               
         ZIC   R3,1(R6)            17 ELEM LEN                                  
         SH    R3,=H'2'            - ELCODE & LEN                               
         SR    R2,R2                                                            
         D     R2,=F'9'            LEN OF MINI ELEM                             
         LTR   R2,R2               DIVISION SHOULD BE EVEN                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,7(R6)            FIRST K NUMBER IN 17 ELEM                    
SCMB20   DS    0H                                                               
         CLC   FULL,0(R5)          FULL VS. CURRENT K?                          
         BL    *+10                FULL IS LOWER - SKIP                         
         MVC   FULL,0(R5)          FULL IS HIGHER - REPLACE W/CURRENT           
         LA    R5,9(R5)            NEXT MINI ELEM IN 17 ELEM                    
         BCT   R3,SCMB20                                                        
*                                                                               
SCMBX    DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
***********************************************************************         
* CHECKS IF THERE ARE PROPOSALS AGAINST A CONTRACT                              
*                                                                               
* CC ON RETURN                                                                  
*    EQUAL - PROPOSALS                                                          
*    NE    - NO PROPOSALS                                                       
***********************************************************************         
PROCHK   NMOD1 0,*PROCHK*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R6,KEY                                                           
         USING RPROKEY,R6                                                       
         XC    RPROKEY,RPROKEY                                                  
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,REPALPHA                                                
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   RPROKCON,WORK                                                    
         DROP  R6                                                               
         GOTO1 VHIGH                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(RPROKPRO-RPROKEY),KEYSAVE                                    
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   BUYDIS ROUTINE NMOD TO REGAIN SOME ADDRESSABILITY                           
*                                                                               
BUYDNMOD NMOD1 0,*BUYD*                                                         
         L     RC,0(R1)                                                         
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    BUYDIS05                                                         
         GOTO1 VLOAD,DMCB,(X'7D',0)     SPORTS BUY DISPLAY MOD                  
         B     BUYDIS99                                                         
BUYDIS05 DS    0H                                                               
         TM    TWAPROST,X'02'      EXTENDED BUY SCREEN IN USE?                  
         BZ    BUYDIS10                                                         
         GOTO1 VLOAD,DMCB,(X'2E',0)     DISPLAY BUY                             
         B     BUYDIS99                                                         
BUYDIS10 DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'25',0)     DISPLAY BUY                             
BUYDIS99 DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*   PREVIOUS SCREEN WAS FULL-SCREEN                                             
*   NEW REQUESTS REQUIRE THAT CONTRACT BASE SCREEN BE RELOADED                  
*                                                                               
FFSCREEN NMOD1 0,*FFSCRN*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R3,CONBACTH-TWATASK                                              
         ST    R3,ABUYFH           A(BUY ACTION FIELD)                          
*                                                                               
         MVI   TWASCRN,X'FF'       SET WHICH SCREEN LOADED                      
         OI    TWAPROST,X'20'                                                   
         NI    TWAPROST,X'FF'-X'01'  NO MORE FULL SIZE SUBSCREEN                
         NI    TWAPROST,X'FF'-X'02'  NO MORE EXTEND BUY SCREEN                  
         LA    R3,CONCNUMX+8       A(LOAD POINT FOR CONTRACT                    
*                                     BASE SCREEN RECONSTITUTION                
         MVC   DMCB+4(3),=X'D90802'                                             
*                                  PROGRAM NUMBER                               
         MVI   DMCB+7,X'E3'        'SHORT' SCREEN                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # E3                             
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
         B     EXXMOD                                                           
*                                                                               
********************************************************************            
* CHECK IF CONTRACT IS A TAKEOVER CONTRACT                                      
********************************************************************            
T80201   CSECT                                                                  
ISTAKOV  NMOD1 0,*ISTAKOV*                                                      
         L     RC,0(R1)                                                         
*                                                                               
         XC    WORK2,WORK2                                                      
         XC    WORK,WORK                                                        
         CLC   =C'ADD',CONCACT                                                  
         BE    ISTKOV03                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ISTKNO                                                           
         USING RCONCMEL,R6                                                      
         ZIC   R1,RCONCMLN                                                      
         SH    R1,=H'3'                                                         
         BM    ISTKNO                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONCMNT                                                
         DROP  R6                                                               
                                                                                
         MVC   WORK+10(4),RCONKSTA                                              
         CLI   RCONKSTA+4,C' '                                                  
         BE    ISTKOV04                                                         
         CLI   WORK+13,C' '                                                     
         BE    ISTKOV02                                                         
         MVI   WORK+13,C'-'                                                     
         MVC   WORK+14(1),RCONKSTA+4                                            
         B     ISTKOV04                                                         
*                                                                               
ISTKOV02 DS    0H                                                               
         MVI   WORK+14,C'-'                                                     
         MVC   WORK+15(1),RCONKSTA+4                                            
         B     ISTKOV04                                                         
*                                                                               
ISTKOV03 DS    0H                                                               
         MVC   WORK2(L'CONCOM1),CONCOM1                                         
         MVC   WORK+10(L'CONSTA),CONSTA                                         
                                                                                
ISTKOV04 DS    0H                                                               
         OC    WORK+10(L'CONSTA),MYSPACES                                       
         OC    WORK2(L'MYSPACES),MYSPACES                                       
         CLC   =C'C=TO',WORK2      COMMET MUST BE C=TOXXXX-X                    
         BNE   ISTKNO              WHERE XXXX-X IS THE STATION                  
         MVC   WORK(6),WORK2+4                                                  
                                                                                
* ON COMBO ADD, CHECK STATION DISPLAY FIELD VS COMMENT FIELD                    
* AFTER COMBO IS ADDED, THE STATION DISPLAY FIELD IS CHANGED. HENCE,            
* WE'LL NEED TO LOOK UP THE STATION REC FOR THE PARENT CALL LETTER              
                                                                                
         CLC   =C'-C',WORK+14      COMBO, AT ACTION ADD?                        
         BE    ISTKOV05                                                         
         CLC   =C'-C',WORK+13      CHECK INCASE OF 3 LETTER CALLS               
         BE    ISTKOV05                                                         
         CLC   =C'-C',WORK2+7      COMBO, AFTER ACTION ADD?                     
         BE    ISTKOV50                                                         
         CLC   =C'-C',WORK2+8                                                   
         BE    ISTKOV50                                                         
                                                                                
ISTKOV05 DS    0H                                                               
         OC    WORK(6),MYSPACES                                                 
                                                                                
         CLC   =C'-T',WORK+14                                                   
         BE    ISTKOV20                                                         
         CLC   =C'-T',WORK+13      CHECK INCASE OF 3 LETTER CALLS               
         BE    ISTKOV30                                                         
         CLI   WORK+13,C' '                                                     
         BE    ISTKOV30                                                         
         CLI   WORK+14,C' '                                                     
         BE    ISTKOV20                                                         
                                                                                
* COMPARE FOR RADIO                                                             
         CLI   WORK+5,C' '         CHECK INCASE OF 3 LETTER CALLS               
         BNE   ISTKOV10                                                         
         CLC   WORK(5),WORK+10                                                  
         BE    ISTKYES                                                          
         B     ISTKNO                                                           
                                                                                
ISTKOV10 DS    0H                                                               
         CLC   WORK(6),WORK+10                                                  
         BE    ISTKYES                                                          
         B     ISTKNO                                                           
                                                                                
* COMPARE FOR TV                                                                
ISTKOV20 DS    0H                  TV CAN BE SPECIFIED AS                       
         CLC   WORK(4),WORK+10     XXXX OR XXXX-T OR XXXX-L                     
         BNE   ISTKNO                                                           
         CLC   WORK+4(2),MYSPACES                                               
         BE    ISTKYES                                                          
         CLC   =C'-T',WORK+4                                                    
         BE    ISTKYES                                                          
         CLC   =C'-L',WORK+4                                                    
         BE    ISTKYES                                                          
         B     ISTKNO                                                           
                                                                                
* AND 3 LETTER CALLS                                                            
ISTKOV30 DS    0H                                                               
         CLC   WORK(3),WORK+10                                                  
         BNE   ISTKNO                                                           
         CLC   WORK+3(3),MYSPACES                                               
         BE    ISTKYES                                                          
         CLC   =C'-T',WORK+3                                                    
         BE    ISTKYES                                                          
         CLC   =C'-L',WORK+3                                                    
         BE    ISTKYES                                                          
         B     ISTKNO                                                           
* CHECK FOR COMBO TAKEOVER, ALL ACTIONS OTHER THAN ADD                          
ISTKOV50 DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
                                                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ISTKNO                                                           
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   ISTKNO                                                           
         CLI   WORK+3,C'-'         TAKEOVER COMMENT STATION MUST MATCH          
         BE    ISTKOV60            PARENT STATION                               
         CLC   RSTACS(4),WORK                                                   
         BNE   ISTKNO                                                           
                                                                                
ISTKOV60 DS    0H                  INCASE PARENT'S CALL IS 3 LETTERS            
         CLC   RSTACS(3),WORK                                                   
         BNE   ISTKNO                                                           
         B     ISTKYES                                                          
         DROP  R6                                                               
*                                                                               
ISTKNO   LA    R1,1                                                             
         B     *+6                                                              
ISTKYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CHECK STATION SIGN ON TO SEE IF THEY ARE ALLOW TO ACCESS THIS                 
* CONTRACT                                                                      
* CC EQUAL: SIGN ON = CONTRACT STATION                                          
* CC NOT EQUAL: SIGN ON != CONTRACT STATION                                     
**********************************************************************          
CHKSECU  NMOD1 0,*CHKSECU*                                                      
         L     RC,0(R1)                                                         
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   SECUYES             NOT STATION - SKIP                           
*                                                                               
         CLC   =C'LIN',TWAGENCP    STATION TOOL KIT? SKIP                       
         BE    SECUYES                                                          
*                                                                               
         XC    RSTAKEY(27),RSTAKEY                                              
         MVI   RSTAKTYP,2          READ STATION RECORD                          
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   SECU0360                                                         
         USING RSTASOEL,R6         STATION'S CONTRACTS                          
*                                                                               
SECU0340 CLC   TWAUSRID,RSTASID    SAME ID?                                     
         BE    SECUYES                                                          
         BAS   RE,NEXTEL                                                        
         BE    SECU0340                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
SECU0360 MVC   CONAGY,MYSPACES     BLANK OUT THE OTHER FIELDS                   
         MVC   CONAGYN,MYSPACES                                                 
         MVC   CONBUY,MYSPACES                                                  
         MVC   CONADV,MYSPACES                                                  
         MVC   CONADVN,MYSPACES                                                 
         MVC   CONSTA,MYSPACES                                                  
         MVC   CONSTAM,MYSPACES                                                 
         B     SECUNO                                                           
*                                                                               
SECUYES  SR    RC,RC                                                            
SECUNO   LTR   RC,RC                                                            
SECUX    DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
T80201   CSECT                                                                  
*                                                                               
***********************************************************************         
* STATION PROPOSER OFFICE CHECK ROUTINE                                         
*        1.  THIS IS A PROPOSER ORDER                                           
*        2.  RETRIEVE ORDER'S STATION RECORD                                    
*        3.  ACCESS X'3A' ELEMENT IN RECORD                                     
*        4.  SEEK OFFICE OF ORDER WITHIN X'3A' ELEMENT                          
*        5.  IF FOUND, LOOK FOR A CUTOFF DATE                                   
*        6.  IF FOUND, COMPARE CUTOFF DATE TO ORDER CREATE DATE                 
*        7.  CREATE DATE EARLIER THAN CUTOFF:  PERMIT BUYS, ELSE                
*               ERROR MESSAGE, PROHIBIT BUYS                                    
***********************************************************************         
STAPROCK NMOD1 0,*STAPRO*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         XC    RSTAKEY(27),RSTAKEY                                              
         MVI   RSTAKTYP,2          READ STATION RECORD                          
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'3A'                                                     
         BAS   RE,GETEL                                                         
         BNE   SPRO0800                                                         
         LR    R1,R6               CALCULATE R1 = A(NEXT ELEMENT)               
         ZIC   RF,1(R6)                                                         
         AR    R1,RF                                                            
         USING RSTCPFEL,R6         STATION PROPOSER OFFICE ELEMENT              
         LA    RF,RSTCPFFC         SET A(1ST OFFICE/DATE)                       
*                                                                               
         DROP  R6                                                               
*                                                                               
SPRO0020 EQU   *                                                                
         CR    RF,R1               A(NEXT ELEMENT) REACHED?                     
         BNL   SPRO0800            YES - NO ENTRY FOUND                         
         CLC   RCONKOFF,0(RF)      ORDER OFFICE = ELEMENT OFFICE?               
         BE    SPRO0040            YES                                          
         LA    RF,5(RF)            NO  - BUMP TO NEXT OFFICE IN ELEMENT         
         B     SPRO0020            GO BACK FOR NEXT OFFICE                      
SPRO0040 EQU   *                                                                
         OC    2(3,RF),2(RF)       ANY DATE ENTERED?                            
         BZ    SPRO0800            NO  - NO DATE TEST: PERMIT                   
         CLC   RCONHDRD,2(RF)      CONTRACT CREATE VS CUTOFF DATE               
         BL    SPRO0800            CREATED BEFORE: PERMIT BUYS                  
*                                                                               
* ROLLBACK CODING FOR EAGLE 9/10/09                                             
*                                                                               
*        CLC   =C'B4',REPALPHA     ROB TEST REP                                 
*        BNE   SPRO0045                                                         
*        CLC   RCONHDRD,=X'6D0909' CONTRACT CREATE VS CUTOFF END DATE           
*        BH    SPRO0800            CREATED BEFORE: PERMIT BUYS                  
*        B     SPRO0050                                                         
*                                                                               
SPRO0045 EQU   *                                                                
         CLC   =C'AM',REPALPHA                                                  
         BNE   SPRO0046                                                         
         CLC   RCONHDRD,=X'6D090A' CONTRACT CREATE VS CUTOFF END DATE           
         BH    SPRO0800            CREATED BEFORE: PERMIT BUYS                  
         B     SPRO0050                                                         
*                                                                               
SPRO0046 EQU   *                                                                
         CLC   =C'FB',REPALPHA                                                  
         BNE   SPRO0047                                                         
         CLC   RCONHDRD,=X'6E011F' CONTRACT CREATE VS CUTOFF END DATE           
         BH    SPRO0800            CREATED BEFORE: PERMIT BUYS                  
         B     SPRO0050                                                         
*                                                                               
SPRO0047 EQU   *                                                                
         CLC   =C'UV',REPALPHA                                                  
         BNE   SPRO0050                                                         
         CLC   RCONHDRD,=X'6F0115' CONTRACT CREATE VS CUTOFF END DATE           
         BH    SPRO0800            CREATED BEFORE: PERMIT BUYS                  
         B     SPRO0050                                                         
*                                                                               
* INSERT FUTURE ROLLBACK CLIENTS HERE                                           
*                                                                               
SPRO0050 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     SPRO0900            EXIT CC NOT ZERO                             
*                                                                               
SPRO0800 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
*                                                                               
SPRO0900 EQU   *                                                                
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* COMBO CONTRACT ROUTINES                                                       
* WILL BRANCH TO CORRESPONDING COMBO ROUTINE                                    
***********************************************************************         
COMBORTN NMOD1 0,*CSND*                                                         
         L     RC,0(R1)                                                         
                                                                                
QCOMBSND EQU   1                                                                
QCOMBDIS EQU   2                                                                
QLINECMB EQU   3                                                                
QCOMBCON EQU   4                                                                
QCOMBBUY EQU   5                                                                
QCOMTKOV EQU   6                                                                
                                                                                
         CLI   4(R1),QCOMBSND                                                   
         BE    COMBOSND                                                         
         CLI   4(R1),QCOMBDIS                                                   
         BE    COMBODIS                                                         
         CLI   4(R1),QLINECMB                                                   
         BE    LINECOMB                                                         
         CLI   4(R1),QCOMBCON                                                   
         BE    COMBOCON                                                         
         CLI   4(R1),QCOMBBUY                                                   
         BE    COMBOBUY                                                         
         CLI   4(R1),QCOMTKOV                                                   
         BE    COMBOTKV                                                         
         DC    H'0'                                                             
COMBOXIT XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE - COMBO CONTRACT SEND ROUTINE                                         
* THIS WILL FIRST CHECK THE ORDER TOTALS AGAINST THE CONTRACT TOTALS            
* BEFORE SENDING.  THE ORDER IN WHICH THE BUYLINES ARE PRINTED IS               
* THE SAME AS THE ORDER FOUND IN THE X'17' COMBO ELEMENT IN THE K REC.          
***********************************************************************         
COMBOSND DS    0H                                                               
         MVC   SVCONKEY,RCONREC    SAVE FIRST COMBO KEY                         
         MVC   SVCNUM,TWACNUM                                                   
         MVI   TWACMBPT,1          INDEX, SET TO FIRST COMPONENT K              
*                                                                               
         LA    R6,RCONREC          SET TO FIRST COMPONENT K#                    
         USING RCONCBEL,R6                                                      
         MVI   ELCODE,X'17'        AS FOUND IN THE COMBO ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVCOMBO,SVCOMBO     SAVE OFF COMPONENTS, CONTRACT IO             
         ZIC   R1,RCONCBLN         AREA GETS USED BY OTHER CONTRACTS            
         SH    R1,=H'3'            OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCOMBO(0),RCONCBST                                              
*                                                                               
         LA    R6,SVCOMBO+5        GET NEXT COMPONENT K#                        
         DROP  R6                                                               
*                                                                               
COMBOS10 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),0(4,R6)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+5(5)   CHANGE TO PWOS                               
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,WORK       NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28                                                  
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)      REVERSE THE COMPLIMENT                 
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'F2'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COMBOS40 DS    0H                                                               
         CLC   =C'SEND',CONACT     MUST BE ACTION SEND OR RSND                  
         BE    COMBOS56                                                         
         CLC   =C'RSND',CONACT                                                  
         BE    COMBOS56                                                         
         CLC   =C'MGS',CONACT                                                   
         BE    COMBOS56                                                         
         DC    H'0'                                                             
*                                                                               
COMBOS56 BAS   R8,LOADEDT          LOOP FOR SEND/RESEND                         
*                                                                               
         TM    TWASPREF,PREFSENT   IF WE'VE ALREADY SEND TO PREFERRED           
         BO    COMBOS60            STATION, SKIP REST OF PRINTING               
*                                                                               
COMBOS57 MVI   INTTYPE,C'O'        INDICATE ORDER WORKSHEET                     
         TM    TWAFLAGS,TWAFLHMQ                                                
         BO    COMBOS58            HOME MKT: JUST GEN STATION COPY              
         CLC   =C'RSND',CONCACT    ONLY RECEIVER GETS RESEND                    
         BE    COMBOS58                                                         
         CLC   =C'MGS',CONCACT     ONLY RECEIVER GETS MAKEGOOD SEND             
         BE    COMBOS58                                                         
         MVI   SENDPASS,3          3RD PASS = TO DESTINATION ID.                
         GOTO1 VLOAD,DMCB,(X'60',0)                                             
*                                                                               
         TM    TWASTAOP,X'02'      PROF #16?                                    
         BZ    COMBOS58                                                         
         MVI   SENDPASS,4          GEN EXTRA FAX COPY TO STATION                
         GOTO1 VLOAD,DMCB,(X'60',0)                                             
*                                                                               
COMBOS58 DS    0H                                                               
         MVI   INTTYPE,C'O'        INDICATE ORDER WORKSHEET                     
         MVI   SENDPASS,2          2ND PASS = TO RECEIVER                       
         GOTO1 VLOAD,DMCB,(X'60',0)   PRODUCE REPORT TO RECEIVER                
*                                                                               
COMBOS60 ZIC   RF,TWACMBPT         BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,TWACMBPT                                                      
*                                                                               
         CLC   TWACMBPT,TWACOMBO   WE HAVE THIS MANY COMBO K'S TO DO            
         BH    COMBOS70                                                         
         LA    R6,9(R6)            GET NEXT COMPONENT K#                        
         B     COMBOS10                                                         
*                                                                               
COMBOS70 DS    0H                                                               
*                                  ALL DONE, RESTORE ORIGINAL COMBO K           
*                                  TO RCONREC AREA                              
         MVC   KEY(L'RCONKEY),SVCONKEY                                          
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'F2'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28     RESTORE K INFO IN TWA, TOO                   
         MVC   TWACNUM,SVCNUM                                                   
*                                                                               
COMBOSX  DS    0H                                                               
         B     COMBOXIT                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE - DISPLAY COMBO STATIONS                                              
* THIS ROUTINE WILL CLEAR AND MAKE COMBO RATE FIELDS PROTECTED IF THE           
* CONTRACT IS NOT A COMBO ORDER.  OTHERWISE, COMBO COMPONENT STATIONS           
* WILL BE DISPLAYED ON TOP OF COMBO RATE FIELDS WITH THE REGULAR RATE           
* FIELD PROTECTED.                                                              
***********************************************************************         
COMBODIS DS    0H                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
*                                  CLEAR LOWER SCREEN                           
         TWAXC BUYDAYSH-CONBACTH(R2)                                            
*                                                                               
         CLI   TWACOMBO,0          COMBO CONTRACT?                              
         BNE   COMBOD10            YES                                          
*                                                                               
         AHI   R2,BUYPLNLH-CONBACTH                                             
         NI    1(R2),X'FF'-X'0C'   DISPLAY PLAN LABEL                           
         OI    6(R2),X'80'                                                      
*                                                                               
         AHI   R2,BUYPLNH-BUYPLNLH                                              
         NI    1(R2),X'FF'-X'20'   UNPROTECT PLAN FIELD                         
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R6,4                                                             
         AHI   R2,BUYCMBSH-BUYPLNH                                              
         LA    R3,BUYCBC1H-BUYCMBSH(R2)                                         
*                                                                               
COMBOD05 MVC   8(L'BUYCMBS,R2),MYSPACES  CLEAR COMBO STATION FIELDS             
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         OI    6(R3),X'20'+X'80'   PROTECT COMBO BUY FIELDS                     
         ZIC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         BCT   R6,COMBOD05                                                      
         B     COMBODX                                                          
*                                                                               
COMBOD10 DS    0H                  COMBO BUY,                                   
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
*                                                                               
         AHI   R2,BUYPLNLH-CONBACTH                                             
         OI    1(R2),X'0C'         HIDE PLAN LABEL                              
         OI    6(R2),X'80'                                                      
*                                                                               
         AHI   R2,BUYPLNH-BUYPLNLH                                              
         OI    1(R2),X'20'         PROTECT PLAN FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
*                                                                               
         LA    R6,4                                                             
         AHI   R2,CONCMBSH-CONBACTH      COMBO DISPLAY LINE                     
         LA    R3,BUYCMBSH-CONCMBSH(R2)  COMBO RATE STATION                     
         LA    R5,BUYCBC1H-CONCMBSH(R2)  COMBO RATE                             
*                                                                               
         OC    8(L'CONCMBS,R2),8(R2)     CHECK IF RATE LINE DISPLAYED           
         BNZ   COMBOD20            IT SHOULD BE AT THIS POINT                   
         BAS   RE,LINECOMB         IF NOT, REFRESH THE LINE                     
*                                                                               
COMBOD20 DS    0H                                                               
         MVC   8(6,R3),8(R2)                                                    
         OI    6(R3),X'08'+X'80'   XMIT/INTENSIFY                               
*                                                                               
         NI    6(R5),X'FF'-X'20'   UNPROTECT RATE FIELD AS DEFAULT              
         OC    8(6,R3),MYSPACES                                                 
         CLC   8(6,R3),MYSPACES                                                 
         BNZ   COMBOD30                                                         
         OI    6(R5),X'20'         PROTECT RATE FIELD IF NO STATION             
*                                                                               
COMBOD30 DS    0H                                                               
*                                                                               
         ZIC   R0,0(R2)            BUMP THRU COMBO DISPLAY LINE                 
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         ZIC   R0,0(R3)            BUMP THRU COMBO BUY RATE FIELDS              
         AR    R3,R0                                                            
         ZIC   R0,0(R5)                                                         
         AR    R5,R0                                                            
*                                                                               
         BCT   R6,COMBOD20                                                      
*                                                                               
         L     R2,ABUYFH           PROTECT NON-COMBO RATE FIELD                 
         AR    R2,RA                                                            
         OI    BUYRATEH-CONBACTH+6(R2),X'20'                                    
*                                                                               
COMBODX  DS    0H                                                               
         B     COMBOXIT                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE - DISPLAY SPECIAL COMBO LINE                                          
* THIS ROUTINE DISPLAYS THE COMBO LINE LOCATED AFTER THE BUY ACTION             
* FIELD AND BEFORE THE LOWER SCREEN.                                            
***********************************************************************         
LINECOMB DS    0H                                                               
         L     R2,ABUYFH                                                        
         AR    R2,RA                                                            
         LA    RE,CONCMBSH-CONBACTH(R2) IF ERASED, REDISPLAY LINE               
         LA    RF,TWACMBS1                                                      
         LA    R3,4                MAX 4 STATIONS TO DISPLAY                    
*                                                                               
LCOMBO10 DS    0H                                                               
         OC    0(5,RF),0(RF)       IF LESS THAN 4 STATIONS, PROTECT             
         BNZ   LCOMBO20            THE REST OF THE BLANK FIELDS                 
         ZIC   R0,0(RE)            BUMP TO CONTRACT #                           
         AR    RE,R0                                                            
         B     LCOMBO30                                                         
*                                                                               
LCOMBO20 MVC   8(4,RE),0(RF)       CALL LETTERS                                 
         MVI   12(RE),C'-'                                                      
         MVC   13(1,RE),4(RF)                                                   
*                                                                               
         ZIC   R0,0(RE)            BUMP TO CONTRACT #                           
         AR    RE,R0                                                            
*                                                                               
         ZAP   WORK(5),=P'0'       DISPLAY CONTRACT #                           
         MVO   WORK(5),5(4,RF)                                                  
         EDIT  (P5,WORK),(8,8(RE)),ALIGN=LEFT                                   
*                                                                               
LCOMBO30 DS    0H                                                               
         OI    1(RE),X'20'         MAKE PROTECTED IN CASE ACTION ADD            
*                                  HAS UNPROTECTED IT                           
         ZIC   R0,0(RE)            BUMP TO NEXT CALL LETTER                     
         AR    RE,R0                                                            
         LA    RF,9(RF)                                                         
         BCT   R3,LCOMBO10                                                      
*                                                                               
         B     COMBOXIT                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE - COMBO CONTRACT ACTION ROUTINE                                       
* THIS ROUTINE WILL LOOP THRU THE K SCREEN APPLYING THE K ACTION                
* TO EACH COMBO K INDIVIDUALLY.  THIS WILL BE DONE N TIMES,                     
* DEPENDING ON THE NUMBER OF COMBO CONTRACTS.                                   
***********************************************************************         
COMBOCON DS    0H                                                               
         MVC   SVCONKEY,RCONREC    SAVE FIRST COMBO KEY                         
         MVC   SVCNUM,TWACNUM                                                   
         MVI   TWACMBPT,1          INDEX, SET TO FIRST COMPONENT K              
         LA    R2,TWACMBC1         SET TO FIRST COMPONENT K#                    
         B     COMBOC40                                                         
*                                                                               
COMBOC10 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
         LA    R2,9(R2)            GET NEXT COMPONENT K#                        
*                                                                               
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),0(4,R2)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+5(5)   CHANGE TO PWOS                               
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,WORK       NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28                                                  
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)      REVERSE THE COMPLIMENT                 
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COMBOC40 DS    0H                                                               
         CLC   =C'CHA',CONACT                                                   
         BE    COMBOC50                                                         
         CLC   =C'SEND',CONACT                                                  
         BE    COMBOC56                                                         
         CLC   =C'RSND',CONACT                                                  
         BE    COMBOC56                                                         
         CLC   =C'MGS',CONACT                                                   
         BE    COMBOC56                                                         
         CLC   =C'CF ',CONACT                                                   
         BE    COMBOC52                                                         
         CLC   =C'CF*',CONACT                                                   
         BE    COMBOC52                                                         
         CLC   =C'CFX',CONACT                                                   
         BE    COMBOC52                                                         
         CLC   =C'DONE',CONACT                                                  
         BE    COMBOC52                                                         
         CLC   =C'DEL',CONACT                                                   
         BE    COMBOC54                                                         
         CLC   =C'RES',CONACT                                                   
         BE    COMBOC54                                                         
         DC    H'0'                                                             
*                                                                               
COMBOC50 BAS   R8,CONEDIT          CALL K EDIT ROUTINE                          
         B     COMBOC60                                                         
*                                                                               
COMBOC52 BAS   R8,LOADEDT                                                       
         B     COMBOC60                                                         
*                                                                               
COMBOC54 BAS   R8,LOADDIS                                                       
         B     COMBOC60                                                         
*                                                                               
COMBOC56 DS    0H                  CHECK ORDER TOTALS AGAINST K TOTALS          
         OI    TWASPREF,SNDNOUPD   DON'T WANT UPDATE TO ANY RECORDS             
         BAS   R8,LOADEDT          LOOP FOR SEND/RESEND                         
         NI    TWASPREF,X'FF'-SNDNOUPD  RESET                                   
*                                                                               
COMBOC60 ZIC   RF,TWACMBPT         BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,TWACMBPT                                                      
*                                                                               
         CLC   TWACMBPT,TWACOMBO   WE HAVE THIS MANY COMBO K'S TO DO            
         BNH   COMBOC10                                                         
*                                  ALL DONE, RESTORE ORIGINAL COMBO K           
*                                  TO RCONREC AREA                              
         MVC   KEY(L'RCONKEY),SVCONKEY                                          
         OI    DMINBTS,X'08'       NEED THIS IN CASE ACTION IS DEL              
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    DMINBTS,X'08'       NEED THIS IN CASE ACTION IS DEL              
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         MVC   TWAKADDR,KEY+28     RESTORE CONTRACT INFO IN TWA, TOO            
         MVC   TWACNUM,SVCNUM                                                   
*                                                                               
         CLC   =C'DEL',CONACT      IF ACTION DELETE, SKIP CHECK FOR             
         BE    COMBOCX             DELETED BIT                                  
         TM    RCONCNTL,X'80'                                                   
         BZ    COMBOCX                                                          
         DC    H'0'                                                             
*                                                                               
COMBOCX  DS    0H                                                               
         B     COMBOXIT                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE - COMBO BUY ACTION ROUTINE                                            
* THIS ROUTINE WILL LOOP THRU THE BUY SCREEN APPLYING THE BUY ACTION            
* TO EACH RATE FIELD INDIVIDUALLY.  THIS WILL BE DONE N TIMES,                  
* DEPENDING ON THE NUMBER OF COMBO CONTRACTS.  IF A CHANGE IS MADE TO           
* THE REGULAR BUY ACTION ROUTINES ABOVE, THE SAME CHANGE MAY HAVE TO            
* BE MADE TO THE ROUTINE BELOW AS WELL.                                         
***********************************************************************         
COMBOBUY DS    0H                                                               
         XC    CBSTAT,CBSTAT       CLEAR STATUS                                 
*                                                                               
         CLC   =C'CAN',BUYACT      ACTION CAN GETS CHANGED TO DEL               
         BNE   *+8                 AFTER EDIT.  SET FLAG SO WE KNOW             
         OI    CBSTAT,CANACT       BUY ACTION IS CANCEL                         
*                                                                               
         MVC   SVCONKEY,RCONREC    SAVE FIRST COMBO K KEY                       
         MVC   SVCNUM,TWACNUM      SAVE OFF TWACNUM                             
         MVI   TWACMBPT,1          INDEX, SET TO FIRST COMPONENT K              
         LA    R2,TWACMBC1         SET TO FIRST COMPONENT K#                    
         CLC   =C'BUY',BUYACT      ACTION BUY HAS NO BUY TO GET                 
         BE    COMBOB40                                                         
*                                                                               
         MVC   KEY+28(4),TWABADDR  BUY DISK ADDR                                
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         MVC   SVBUYKEY,RBUYREC                                                 
         B     COMBOB30            FETCH CORRESPONDING BUY RECORD               
*                                                                               
COMBOB10 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
         LA    R2,9(R2)            GET NEXT COMPONENT K#                        
*                                                                               
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),0(4,R2)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+5(5)   CHANGE TO PWOS                               
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,WORK       NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28                                                  
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)      REVERSE THE COMPLIMENT                 
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'F2'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =C'BUY',BUYACT      ACTION BUY HAS NO BUY TO GET                 
         BE    COMBOB40                                                         
*                                                                               
COMBOB30 DS    0H                                                               
         MVC   RBUYREC(L'SVBUYKEY),SVBUYKEY                                     
         MVC   RBUYKCON,TWACNUM    GET CORRESPONDING COMBO BUY RECORD           
         MVC   KEY,RBUYREC         FOR EACH COMBO CONTRACT                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWABADDR,KEY+28                                                  
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         TM    RCONCNTL,X'F2'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COMBOB40 DS    0H                                                               
         BAS   R8,LOADEDT          CALL BUY EDIT ROUTINE                        
*                                                                               
         CLC   =C'DEL',BUYACT      SKIP BUY DIS ROUTINE FOR ACTIONS             
         BE    COMBOB50            DELETE AND CANCEL                            
         CLC   =C'CAN',BUYACT                                                   
         BE    COMBOB50                                                         
         CLC   =C'BUY',BUYACT      FOR ACTION BUY, DISPLAY NEW LINE #           
         BNE   COMBOB50                                                         
*                                                                               
         MVC   CONBNUM,MYSPACES                                                 
         EDIT  (1,RBUYKLIN),(3,CONBNUM),ALIGN=LEFT                              
         OI    CONBNUMH+6,X'80'    XMIT                                         
*                                                                               
         OI    RBUYRTS,X'40'       FLAG BUY AS COMBO ORDER                      
*                                                                               
COMBOB50 BAS   R8,BUCKUP                                                        
*                                                                               
         ZIC   RF,TWACMBPT         BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,TWACMBPT                                                      
*                                                                               
         CLC   TWACMBPT,TWACOMBO   WE HAVE THIS MANY COMBO K'S TO DO            
         BH    COMBOB70                                                         
         TM    CBSTAT,CANACT       WAS ACTION CANCEL?                           
         BZ    COMBOB10                                                         
         MVC   BUYACT,=C'CAN'      EDIT CHANGES CAN TO DEL FOR BUCKUP           
         B     COMBOB10            WE NEED TO RESTORE IT TO CAN                 
         EJECT                                                                  
COMBOB70 DS    0H                  ALL DONE, RESTORE FIRST COMBO K              
*                                  TO RCONREC AREA                              
*                                                                               
         CLC   =C'DEL',BUYACT      SKIP SCREEN RE-DISPLAY FOR                   
         BE    COMBOB80            ACTIONS DEL AND CAN                          
         CLC   =C'CAN',BUYACT                                                   
         BE    COMBOB80                                                         
         BAS   R8,BUYDIS                                                        
*                                                                               
COMBOB80 DS    0H                                                               
         MVC   KEY(L'RCONKEY),SVCONKEY                                          
         OI    DMINBTS,X'08'       IN CASE ACTION WAS DEL                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    DMINBTS,X'08'       IN CASE ACTION WAS DEL                       
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28     RESTORE K INFO IN TWA, TOO                   
         MVC   TWACNUM,SVCNUM                                                   
*                                  ALL DONE, RESTORE ORIGINAL COMBO BUY         
*                                  TO RBUYREC AREA                              
         CLC   =C'BUY',BUYACT      ACTION BUY DID NOT USE SVBUYKEY              
         BE    COMBOB90                                                         
         MVC   RBUYREC(L'SVBUYKEY),SVBUYKEY                                     
*                                                                               
COMBOB90 DS    0H                                                               
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY(L'RBUYKEY),RBUYREC                                           
         OI    DMINBTS,X'08'       IN CASE ACTION WAS DEL                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    DMINBTS,X'08'       IN CASE ACTION WAS DEL                       
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         TM    RCONCNTL,X'F2'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWABADDR,KEY+28     RESTORE BUY INFO IN TWA, TOO                 
*                                                                               
COMBOBX  DS    0H                                                               
         OI    BUYRATEH+6,X'20'    PROTECT NON-COMBO RATE FIELD                 
         B     COMBOXIT                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE - COMBO CONTRACT DONE TAKEOVER CHECK ROUTINE                          
* THIS WILL CHECK EACH OF THE COMPONENT STATIONS TO MAKE SURE THAT THE          
* PARENT STATION MATCHES THE COMMENT TAKEOVER CODE                              
***********************************************************************         
COMBOTKV DS    0H                                                               
         LA    R3,473              COMBO PARENT TAKEOVER CMT MISMATCH           
         MVI   TWACMBPT,1          INDEX, SET TO FIRST COMPONENT K              
*                                                                               
         LA    R6,RCONREC          SET TO FIRST COMPONENT K#                    
         MVI   ELCODE,X'17'        AS FOUND IN THE COMBO ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,2(R6)                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         USING RCONCMEL,R6                                                      
                                                                                
         MVC   WORK2(L'MYSPACES),MYSPACES                                       
                                                                                
         ZIC   R1,RCONCMLN                                                      
         SH    R1,=H'3'                                                         
         BM    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONCMNT                                                
*                                                                               
COMBOT10 DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,0(R5)                                                   
                                                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         CLI   WORK2+7,C'-'        TAKEOVER COMMENT STATION MUST MATCH          
         BE    COMBOT20            PARENT STATION                               
         CLC   RSTACS(4),WORK2+4   ASSUMES BAND IS -C                           
         BNE   ERROR                                                            
                                                                                
COMBOT20 DS    0H                  INCASE PARENT'S CALL IS 3 LETTERS            
         CLC   RSTACS(3),WORK2+4                                                
         BNE   ERROR                                                            
         DROP  R6                                                               
*                                                                               
         ZIC   RF,TWACMBPT         BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,TWACMBPT                                                      
*                                                                               
         CLC   TWACMBPT,TWACOMBO   WE HAVE THIS MANY COMBO K'S TO DO            
         BH    COMBOTX                                                          
         LA    R5,9(R5)            GET NEXT COMPONENT K#                        
         B     COMBOT10                                                         
*                                                                               
COMBOTX  DS    0H                                                               
         B     COMBOXIT                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONTRACT ACTIONS                                                 
*  ACTION            DS  CL4                                                    
*  A(ACTN ROUTINE)   DS  AL4                                                    
*  SCREEN            DS  X             *** X'00' = NO LOAD ***                  
*  LDSCREEN BITS     DS  X                                                      
*                                      *** X'80' = PROTECTED CONTRACT           
*                                      *** X'40' = COMMENTS ON SCREEN           
*                                      *** X'01' = FULL SIZE SCREEN             
*  EDIT/DISP OVL     DS  X             *** X'00' = NA ***                       
*  TEXT 1 MSG NUM    DS  XL2           *** X'0000' = NA ***                     
*  TEXT 2 MSG NUM    DS  XL2           *** X'0000' = NA ***                     
*  ACTION FLAGS      DS  X             *** X'10' CHECK FOR COMBO                
*                                      *** X'01' USE FOR COMBO                  
*                                      *** X'02' FORCE SASP SCREEN              
*                                      *** X'04' DON'T USE FOR SASP             
*                                      *** X'08' MAKE REP STA, PROF#38          
*                                                                               
* WARNING: ADDR MUST BE FIRST ACTION IN TABLE!!!                                
         SPACE 1                                                                
         DS    0F                                                               
CONACTS  DS    0CL16                                                            
         DC    CL4'ADDR',AL4(ADDR),X'FB4027',X'0001',X'0005',X'10'              
         DC    CL4'ADDR',AL4(ADDR),X'FB404B',X'0001',X'0005',X'11'              
         DC    CL4'ADD ',AL4(ADDC),X'004000',X'0004',X'0008',X'00'              
         DC    CL4'ADDB',AL4(ADDC),X'FB4027',X'0001',X'0005',X'10'              
         DC    CL4'ADDB',AL4(ADDC),X'FB404B',X'0001',X'0005',X'11'              
*        DC    CL4'ADDS',AL4(ADDC),X'F54045',X'0002',X'0006',X'04'              
         DC    CL4'ADDS',AL4(ADDC),X'E44071',X'0002',X'0006',X'00'              
         DC    CL4'ADDF',AL4(ADDF),X'EB0111',X'0047',X'0008',X'00'              
FCSTDISF DC    CL4'FD  ',AL4(DISF),X'EB8111',X'0009',X'0000',X'00'              
FCSTCHAF DC    CL4'FC  ',AL4(CHAF),X'EB0111',X'000D',X'000E',X'00'              
         DC    CL4'CHA ',AL4(CHAC),X'004000',X'000D',X'000E',X'00'              
         DC    CL4'SCHA',AL4(CHAC),X'004000',X'000D',X'000E',X'00'              
         DC    CL4'DIS ',AL4(DISC),X'FEC000',X'0009',X'0000',X'00'              
         DC    CL4'DISB',AL4(DISC),X'FB4027',X'000A',X'0000',X'00'              
****>>   DC    CL4'DISS',AL4(DISC),X'F54045',X'000B',X'0000',X'02'              
         DC    CL4'MCIR',AL4(MCIR),X'F2C039',X'000F',X'0000',X'00'              
         DC    CL4'EPLR',AL4(EPLR),X'FAC051',X'0010',X'0011',X'10'              
         DC    CL4'EPLR',AL4(EPLR),X'E1C04C',X'0010',X'0011',X'11'              
         DC    CL4'DEL ',AL4(DELC),X'00C010',X'0009',X'0012',X'00'              
         DC    CL4'RES ',AL4(RESC),X'00C010',X'0009',X'0013',X'00'              
         DC    CL4'PRI ',AL4(PRIC),X'FEC017',X'0000',X'0000',X'00'              
         DC    CL4'SEND',AL4(SEND),X'FEC06A',X'0000',X'0000',X'00'              
         DC    CL4'MGS ',AL4(SEND),X'FEC06A',X'0000',X'0000',X'00'              
         DC    CL4'RSND',AL4(SEND),X'FEC06A',X'0000',X'0000',X'00'              
         DC    CL4'CF  ',AL4(CONF),X'FEC06A',X'0009',X'0040',X'08'              
         DC    CL4'CFX ',AL4(CONF),X'FEC06A',X'0009',X'0040',X'00'              
         DC    CL4'CF* ',AL4(CONF),X'FEC06A',X'0009',X'0040',X'00'              
         DC    CL4'LAST',AL4(LAST),X'FEC06A',X'0000',X'0000',X'00'              
         DC    CL4'ORD ',AL4(ORDC),X'F00149',X'001F',X'0020',X'10'              
         DC    CL4'ORD ',AL4(ORDC),X'E0014A',X'001F',X'0020',X'11'              
SASPACT  DC    CL4'SASP',AL4(SASP),X'F70170',X'004A',X'0000',X'00'              
         DC    CL4'SASC',AL4(SASP),X'F70170',X'004A',X'004B',X'00'              
         DC    CL4'PD  ',AL4(SASP),X'F70170',X'004A',X'0000',X'00'              
         DC    CL4'PC  ',AL4(SASP),X'F70170',X'004A',X'004B',X'00'              
COMPACT  DC    CL4'CD  ',AL4(SPLC),X'D40172',X'0021',X'0000',X'00'              
         DC    CL4'CC  ',AL4(SPLC),X'D40172',X'0021',X'0022',X'00'              
         DC    CL4'CLR ',AL4(SPLC),X'D40172',X'0000',X'0000',X'00'              
MGACTS   DC    CL4'MGM ',AL4(MKGO),X'C0013A',X'0074',X'0000',X'00'              
MGACTS#  DC    CL4'MGO ',AL4(MKGO),X'E5012A',X'0074',X'0000',X'00'              
         DC    CL4'MGP ',AL4(MKGO),X'C0013A',X'00B7',X'0000',X'00'              
         DC    CL4'MGB ',AL4(MKGO),X'C0013A',X'00B8',X'0000',X'00'              
         DC    CL4'MLR ',AL4(MKGO),X'C0013A',X'00B9',X'0000',X'00'              
         DC    CL4'MLB ',AL4(MKGO),X'C0013A',X'00BA',X'0000',X'00'              
         DC    CL4'RNA ',AL4(MKGO),X'C0013A',X'00C4',X'0000',X'00'              
*        DC    CL4'LCO ',AL4(MKGO),X'E5012A',X'00C5',X'0000',X'00'              
         DC    CL4'MGL ',AL4(MKGL),X'D1012B',X'0073',X'0000',X'00'              
         DC    CL4'MGX ',AL4(MKGO),X'E5013A',X'0073',X'0000',X'00'              
*                                  DELETE GOES TO 'OFFER' SCREEN                
         DC    CL4'MGC ',AL4(MKGO),X'C0013A',X'0073',X'0000',X'00'              
*                                  CHANGE GOES TO 'OFFER' SCREEN                
*                                                                               
*   EC/E2/E3 TRANSFER TO INDIVIDUAL TRAFFIC OVERLAYS.  THE CODE IN              
*        'ECON' PLUGS THE APPROPRIATE OVERLAY INTO THE TABLE                    
*        BASED ON THE TRAFFIC SYSTEM FOUND IN THE STATION RECORD.               
*                                                                               
         DC    CL4'EC  ',AL4(ECON),X'00C000',X'0000',X'0069',X'00'              
         DC    CL4'E2  ',AL4(ECON),X'00C000',X'0000',X'0069',X'00'              
         DC    CL4'E3  ',AL4(ECON),X'00C000',X'0000',X'0069',X'00'              
*                                                                               
* ACTION 'DONE' IS TO SIGNIFY THE COMPLETION OF INPUT FOR A TAKEOVER            
* CONTRACT. THE CONTRACT WILL BE MARKED CONFIRMED AND A CONFIRMATION            
* WILL BE GENERATED WITHOUT HAVING TO SEND OR FOLLOWING WIP RULES               
*                                                                               
         DC    CL4'DONE',AL4(TKOV),X'FEC06A',X'0009',X'0083',X'00'              
* INSTANT SWAP TO PFM                                                           
         DC    CL4'PFM ',AL4(PFMC),X'00C000',X'0000',X'0000',X'00'              
         DC    CL4'COPY',AL4(COPY),X'000000',X'0000',X'0000',X'00'              
*                                                                               
* SPECIAL SCRIPT REPPAK CONTRACT UPLOAD ACTION                                  
         DC    CL4'R#  ',AL4(CHAC),X'FE4000',X'000D',X'000E',X'00'              
*                                                                               
*        AUDIT COMMENTS ACTIONS                                                 
*                                                                               
         DC    CL4'AUL ',AL4(AUDL),X'BE014D',X'0038',X'0000',X'00'              
         DC    CL4'AULR',AL4(AUDL),X'BE014D',X'0038',X'0000',X'00'              
         DC    CL4'AULS',AL4(AUDL),X'BE014D',X'0038',X'0000',X'00'              
         DC    CL4'AUD ',AL4(AUD),X'BF014D',X'00CF',X'00D0',X'00'               
         DC    X'FF'                                                            
*                                                                               
*   NOTE:  SASPAC2 IS ACTUALLY OUTSIDE THE TABLE, AND IS ONLY                   
*       REFERENCED EXPLICITLY WHEN SAR/SPL SCREEN IS NEEDED WITH                
*       A SPECIAL MESSAGE TO THE USER.                                          
*                                                                               
SASPAC2  DC    CL4'SASP',AL4(SASP),X'F70170',X'0043',X'0000',X'00'              
         EJECT                                                                  
*              BUY ACTIONS                                                      
*  ACTION                DS CL4                                                 
*  A(ACTN ROUTINE)       DS CL4                                                 
*  SCREEN                DS X           *** X'00' = NO LOAD ***                 
*  COMBO USE/NON-USE     DS X           *** X'10' = COMBO USE/NON-USE           
*                                       *** X'01' = COMBO O'LAY/SCRN            
*     + MISCELLANEOUS FLAGS             *** X'02' = FORCE SASP SCREEN           
*                                                   IF SASP USER....            
*     + MISCELLANEOUS FLAGS             *** X'04' = EXTENDED BUY SCRN           
*  DISP/EDIT OVL         DS X           *** X'00' = NA ***                      
*  TEXT 1 MSG NUM        DS XL2         *** X'0000' = NA ***                    
*  TEXT 2 MSG NUM        DS XL2         *** X'0000' = NA ***                    
*********************************************************************           
*  NOTE:  TWO ENTRIES FOR COMBO/NON-COMBO SCREEN/OVERLAY PROCESSING             
*    SCREEN FIELD 'F?', COMBO/NON-COMBO BYTE, OVERLAY # ARE USED                
*       IN THIS MANNER:                                                         
*    SCREEN FIELD:  ACTION MAY REQUIRE DIFFERENT SCREENS DEPENDING              
*                   UPON WHETHER CONTRACT IS A COMBO OR NOT                     
*    COMBO/NON-COMBO BYTE:  BIT X'10' SET INDICATES THIS FIELD                  
*        TO BE TESTED FOR COMBO/NON-COMBO ORDERS, USING THE                     
*        TABLE ENTRY WHERE BIT X'01' IS SET FOR COMBO ORDERS, AND               
*        TABLE ENTRY WHERE BIT X'01' IS NOT SET FOR NON-COMBO.                  
*                                                                               
*********************************************************************           
         SPACE 1                                                                
         DS    0F                                                               
BUYACTSX DS    0CL15                                                            
*                                                                               
         DC    CL4'BUY ',AL4(BUYB),X'DE041E',X'0014',X'0015'                    
         DC    CL4'DEL ',AL4(CHAB),X'DE041E',X'0017',X'0018'                    
         DC    CL4'CHA ',AL4(CHAB),X'DE041E',X'0019',X'001A'                    
         DC    CL4'CAN ',AL4(CHAB),X'DE041E',X'0017',X'001C'                    
         DC    CL4'DIS ',AL4(DISB),X'DE042E',X'0017',X'0000'                    
         DC    CL4'DD0 ',AL4(DISB),X'DE042E',X'0017',X'0000'                    
*                                                                               
BUYACTS  DS    0CL15                                                            
         DC    CL4'BUY ',AL4(BUYB),X'FE0015',X'0014',X'0015'                    
         DC    CL4'DEL ',AL4(CHAB),X'FE0015',X'0017',X'0018'                    
         DC    CL4'CHA ',AL4(CHAB),X'FE0015',X'0019',X'001A'                    
         DC    CL4'CAN ',AL4(CHAB),X'FE0015',X'0017',X'001C'                    
         DC    CL4'DIS ',AL4(DISB),X'FE0025',X'0017',X'0000'                    
*                                                                               
         DC    CL4'RTS ',AL4(DISB),X'FE002F',X'0067',X'0000'                    
         DC    CL4'DSS ',AL4(DSMB),X'FD0026',X'0000',X'0000'                    
         DC    CL4'DSM ',AL4(DSMB),X'FD1026',X'0000',X'0000'                    
         DC    CL4'DSM ',AL4(DSMC),X'F91136',X'0000',X'0000'                    
*                                                                               
         DC    CL4'BCD ',AL4(BCDA),X'CE0031',X'0000',X'0000'                    
         DC    CL4'BCC ',AL4(BCDA),X'CE0031',X'0000',X'0000'                    
*                                                                               
         DC    CL4'DRT ',AL4(DSMD),X'E70028',X'0000',X'0000'                    
         DC    CL4'MBI ',AL4(MBIB),X'F31018',X'001D',X'0000'                    
         DC    CL4'MBI ',AL4(MBIC),X'F81138',X'001D',X'0000'                    
         DC    CL4'MCI ',AL4(MCIB),X'F20039',X'001E',X'0000'                    
         DC    CL4'ORD ',AL4(ORDC),X'F01049',X'001F',X'0020'                    
         DC    CL4'ORD ',AL4(ORDC),X'E0114A',X'001F',X'0020'                    
         DC    CL4'CVC ',AL4(COVR),X'000000',X'0000',X'0000'                    
         DC    CL4'CVD ',AL4(COVR),X'000000',X'0000',X'0000'                    
         DC    CL4'CVX ',AL4(COVR),X'000000',X'0000',X'0000'                    
         DC    CL4'XRER',AL4(QRER),X'EC0053',X'005F',X'0060'                    
         DC    CL4'PCF ',AL4(CCMT),X'D90048',X'00A4',X'00A6'                    
*        DC    CL4'SPL ',AL4(SPLC),X'FC0241',X'0021',X'0022'                    
         DC    CL4'SPL ',AL4(SPLC),X'D40072',X'0021',X'0022'                    
         DC    CL4'SPCP',AL4(QSPC),X'DB0074',X'0061',X'0063'                    
         DC    CL4'BOP ',AL4(BOPC),X'FB1027',X'0023',X'0024'                    
         DC    CL4'BOP ',AL4(BOPC),X'FB114B',X'0023',X'0024'                    
         DC    CL4'EPL ',AL4(EPLC),X'FA1051',X'0025',X'0026'                    
         DC    CL4'EPL ',AL4(EPLC),X'E1114C',X'0025',X'0026'                    
*        DC    CL4'SAR ',AL4(SARC),X'F50245',X'0027',X'0028'                    
         DC    CL4'SAR ',AL4(SARC),X'F70270',X'0027',X'0028'                    
         DC    CL4'UNI ',AL4(UNIC),X'EE0043',X'0029',X'002A'                    
         DC    CL4'COV ',AL4(COVC),X'000014',X'002B',X'0000'                    
         DC    CL4'MON ',AL4(MONB),X'ED1052',X'0000',X'0000'                    
*        DC    CL4'MON ',AL4(MONC),X'E21155',X'0000',X'0000'                    
         DC    CL4'MO$ ',AL4(MON$),X'EA005A',X'003C',X'003D'                    
         DC    CL4'#MON',AL4(MON#),X'C9005C',X'0000',X'0000'                    
         DC    CL4'MOVE',AL4(MOVE),X'D60033',X'009F',X'0000'                    
         DC    CL4'HIST',AL4(HIST),X'EC0053',X'003F',X'0000'                    
*MN                                                                             
         DC    CL4'CHIS',AL4(HIST),X'EC0053',X'003F',X'0000'                    
*MN                                                                             
         DC    CL4'PAY ',AL4(PCHA),X'CB003F',X'00C2',X'00C3'     '              
         DC    CL4'PACE',AL4(PACE),X'D80057',X'0000',X'0000'                    
*        DC    CL4'KCHG',AL4(KCHG),X'FE002D',X'0000',X'0000'                    
         DC    CL4'DX  ',AL4(EQCD),X'E80046',X'0044',X'0000'                    
         DC    CL4'CX  ',AL4(EQCD),X'E80046',X'0046',X'0045'                    
         DC    CL4'TD  ',AL4(EQTD),X'C8043C',X'00C1',X'0000'                    
         DC    CL4'TC  ',AL4(EQTD),X'C8043C',X'00C0',X'0000'                    
         DC    CL4'PFM ',AL4(PFMB),X'000400',X'0000',X'0000'                    
         DC    CL4'HIAT',AL4(QIAT),X'D50054',X'004E',X'004F'                    
         DC    CL4'TKO ',AL4(QTKO),X'D70056',X'0055',X'0056'                    
         DC    CL4'MGL ',AL4(MKGL),X'D1002B',X'0073',X'0000'                    
         DC    CL4'MGM ',AL4(MKGO),X'C0003A',X'0074',X'0000'                    
         DC    CL4'MGO ',AL4(MKGO),X'C0003A',X'0074',X'0000'                    
         DC    CL4'MGP ',AL4(MKGO),X'C0003A',X'00B7',X'0000'                    
         DC    CL4'MGB ',AL4(MKGO),X'C0003A',X'00B8',X'0000'                    
         DC    CL4'MLR ',AL4(MKGO),X'C0013A',X'00B9',X'0000'                    
         DC    CL4'MLB ',AL4(MKGO),X'C0013A',X'00BA',X'0000'                    
         DC    CL4'RNA ',AL4(MKGO),X'C0013A',X'00C4',X'0000'                    
*        DC    CL4'LCO ',AL4(MKGO),X'E5012A',X'00C5',X'0000'                    
         DC    CL4'ESIG',AL4(SBUY),X'C7007C',X'0014',X'0015'                    
         DC    CL4'HSIG',AL4(SBUY),X'C7007C',X'0014',X'0015'                    
         DC    CL4'VIRT',AL4(SBUY),X'C7007C',X'0014',X'0015'                    
         DC    CL4'ENDO',AL4(SBUY),X'C7007C',X'0014',X'0015'                    
         DC    CL4'PROM',AL4(SBUY),X'C7007C',X'0014',X'0015'                    
         DC    CL4'TMP ',AL4(TMPC),X'DD041F',X'0092',X'0093'                    
****     DC    CL4'AUL ',AL4(AUDL),X'BE014D',X'0073',X'0000'                    
****     DC    CL4'AULR',AL4(AUDL),X'BE014D',X'0073',X'0000'                    
****     DC    CL4'AULS',AL4(AUDL),X'BE014D',X'0073',X'0000'                    
****     DC    CL4'AUD ',AL4(AUD),X'BF014D',X'0073',X'0000'                     
*        DC    CL4'MGX ',AL4(MKGO),X'E5007C',X'0073',X'0000'                    
*                                  DELETE GOES TO 'OFFER' SCREEN                
*        DC    CL4'MGC ',AL4(MKGO),X'E5002A',X'0073',X'0000'                    
*                                  CHANGE GOES TO 'OFFER' SCREEN                
         DC    X'FF'                                                            
* CALLABLE ONLY INTERNALLY                                                      
MGOFFDIS DC    CL4'MGD ',AL4(MKGD),X'C0003A',X'0074',X'0000'                    
MGOFFDI# DC    CL4'MGD ',AL4(MKGD),X'E50029',X'0074',X'0000'                    
MGOFFTOT DC    CL4'MGT ',AL4(MKGD),X'DC002C',X'0074',X'0000'                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007RECNT01   03/23/17'                                      
         END                                                                    
