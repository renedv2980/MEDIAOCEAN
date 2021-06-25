*          DATA SET RECNT15    AT LEVEL 162 AS OF 09/18/09                      
*PHASE T80215A                                                                  
*INCLUDE RETIMVAL                                                               
*INCLUDE REDAYVAL                                                               
         TITLE 'T80215 - REPPAK BUY EDIT'                                       
*                                                                               
***********************************************************************         
* IMPORTANT NOTE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!            
* ALL CHANGES MADE TO THIS MODULE MUST BE REFLECTED IN RECNT1E.                 
* RECNT1E IS A COPY OF RECNT15 PLUS ADDITIONAL CODE TO SUPPORT NEW              
* MAKEGOOD ENTRIES. WE HAVE DONE AWAY WITH MG= FORMULA IN THE COMMENT           
* FIELD AND EXPANDED THE BUY SCREEN TO ADD MISSED TARGETS AT THE BOTTOM         
* OF THE SCREEN. NOTE THAT RECNT1E DOES NOT CALL RECNT30 FOR BUCKET             
* UPDATES. ALL BUCKET UPDATES ARE DONE WITHIN RECNT1E. BEWARE!!!!!!!            
* THIS IS A TRANSITIONAL PERIOD, WE WILL DO AWAY WITH THIS MODULE               
* EVENTUALLY, AND REPLACE IT WITH RECNT1E. (SKUI 4/19/2000)                     
***********************************************************************         
*                                                                     *         
*     RECNT15 (T80215) --- BUY, DAY/TIME, BUY LEN, EFF DATE EDIT      *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
* 18SEP09 SKU ON-THE-FLY FIX FOR OPENED EPORT ORDERS W/ 0 SPOT STARTS *         
* 14NOV03 SKU DISALLOW MAKEGOOD TO TARGET CANCELLED BUYS              *         
* 25JUL01 BU  NEW FIELDS ON SCREEN                                    *         
* 30JAN01 SKU FIX MAKEGOOD VALIDATION BUG                             *         
* 18JAN01 RHV PROGRAM FIELD                                           *         
* 03OCT00 SKU UPDATE X'03' ELEMENT REBUILD                            *         
* 24MAY00 BU  NO BUYT:  ALL TRADE WILL BE FLAGGED ON THE $$           *         
* 19MAY00 BU  BUYT (BUYTRADE) ACTION                                  *         
* 08MAY00 SKU MULTI-MAKEGOOD USAGE CHECK                              *         
* 03JAN99 SKU REMOVE REP TO TRANSFER CHECK WHEN DELETING SENT BUYS    *         
* 28DEC99 SKU I MIGHT BE NUTS, BUT ROB WEARS FUNNY SHOES. CLASS FIX.  *         
* 22SEP99 RHV SONNY'S CRAZY, MAX BUYS BACK TO 254                     *         
* 25AUG99 SKU FIX BUG OF MAX BUY NUMBER TO 255                        *         
* 29APR99 RHV REVISED START/END DATES                                 *         
* 28APR99 SKU ALLOW DDS TO DELETE BUYLINES ANYTIME                    *         
* 25JAN99 SKU FIX MAKEGOOD VERSION BUG                                *         
* 18DEC98 RHV DON'T DELETE CANCELLED LINES                            *         
* 06MAY98 SKU NEW CREDIT CR= PROCESSING                               *         
* 31MAR98 SKU DON'T SET MANUAL CHANGE FOR REVISION MAKEGOOD APPLY     *         
* 17FEB98 SKU ALLOW DDS ONLY TO ALTER DARE LINK                       *         
* 23JAN98 RHV PROF#35 & STA OPT#14 CONTROL BUY ORD CMT REQ.           *         
* 07JAN97 SKU FIX FUNNY GOTO1 MACRO CALL                              *         
* 26AUG97 SKU RESET ELEMENT POINTERS AFTER CALL TO REGENVER           *         
* 18JUN97 SKU CAN CREDIT AGAINST ZERO RATE BUY                        *         
* 24APR97 BU  OPTIONAL CONTRACT TYPE FOR REP TO SPOT TRANSFER         *         
* 22NOV96 SKU SUPPORT MAKEGOOD FOR MAKEGOOD                           *         
* 03OCT96 SKU SUPPORT LOW POWER STATION                               *         
* 09SEP96 SKU FIX CC END TIME BUG                                     *         
* 06SEP96 SKU FIX COMBO SPOTS PER WEEK BUG                            *         
* 27MAR96 JRD MOVE PENDING COMMENTS TO SPL COMMENTS                   *         
* 05APR96 BU  ADD SELF-CORRECT FOR PETRY BUY SPOT # CHANGING PROBLEM  *         
* 03APR96 SKU ADD CONTRACT MOD# AT BUY CREATION TO CORRECT PRINTING   *         
*             FIX BUG OF CR=XXX WHERE XXX IS > 99                     *         
* 09MAR96 SKU STEREO SUPPORT                                          *         
* 26FEB96 SKU MAKEGOOD BUG FIX                                        *         
* 23FEB96 SKU GOTO TIMVAL WITH 6A-559A FLAG IF PROFILE SET            *         
* 03JAN95 SKU FIX BUG OF CONTRACT USING WRONG REGISTER                *         
* 28DEC95 SKU BUG FIX IN MG SPLIT ROUTINE                             *         
* 13DEC95 SKU 2K CONTRACT SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80215   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80215,R9,R8                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         OI    BUYFLTH+1,X'20'     PROTECT FLIGHT FIELD                         
         TM    TWASTREO,X'80'      IS THIS STEREO?                              
         BZ    *+8                 NO                                           
         NI    BUYFLTH+1,X'FF'-X'20'    YES, UNPROTECT FLIGHT                   
*                                                                               
DIE2     EQU   *                                                                
         MVI   STAT2,0             CLEAR OUT STATUS BYTE                        
         LA    R2,CONBACTH                                                      
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  K DISK ADDR                                  
         MVI   UPDATE,C'Y'                                                      
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         SPACE 1                                                                
         LA    R2,CONBACTH                                                      
         LA    R3,BACERR                                                        
         TM    RCONCNTL,X'01'      COMPRESSED CONTRACT                          
         BO    ERROR                                                            
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE CONTRACT?                              
         BNO   BUED0010            NO                                           
         OI    BUYFLAGS,X'80'      YES - TURN ON INDICATOR                      
         B     BUED0015                                                         
         DROP  R6                                                               
BUED0010 EQU   *                                                                
*                                  CASH ORDERS:                                 
*                                                                               
         CLI   RCONKSTA+4,C'A'     AM MEDIA/RADIO ORDER?                        
         BE    BUED0015            YES - CAN BE CASH+TRADE                      
         CLI   RCONKSTA+4,C'F'     FM MEDIA/RADIO ORDER?                        
         BE    BUED0015            YES - CAN BE CASH+TRADE                      
         TM    RCONMODR,X'20'      NO  - TV: FIRST BUYLINE ENTERED?             
         BNO   BUED0015            NO  - CAN BE EITHER CASH OR TRADE            
*                                     NOT SET YET                               
         OI    BUYFLAGS,X'40'      CASH ORDER FOR TV FOUND                      
BUED0015 EQU   *                                                                
         TM    RCONMODR+1,X'80'    IF 'ACE' THERE ARE SOME                      
         BZ    BUED0040            SPECIAL TESTS TO BE DONE                     
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING RCONSEND,R6                                                      
*                                                                               
* CAN'T MAKE CHANGES IF OTHER SIDE IS IN PROCESS OF MAKING CHANGES              
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    BUED0020                                                         
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         TM    RCONSENF,X'10'      X'10'=STA. VERS. NOT ADVANCED                
         BZ    ERROR                                                            
         B     BUED0040                                                         
         DROP  R6                                                               
*                                                                               
BUED0020 EQU   *                                                                
         CLC   =C'CHA',BUYACT                                                   
         BNE   BUED0030            FOR STATION, PERMIT CHANGE TO THE            
*                                  ELSE DON'T BOTHER IF ONLY SECTION            
         TM    BUYSECH+4,X'20'     WAS CHANGED                                  
         BZ    BUED0022                                                         
*                                  ELSE DON'T BOTHER IF ONLY PATTERN            
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    BUED0030                                                         
         TM    BUYCLSH+4,X'20'     PATTERN CHANGED?                             
         BZ    BUED0022            YES                                          
         TM    BUYNOTH+4,X'20'     NOTATION CHANGED?                            
         BZ    BUED0022            YES                                          
         TM    BUYUPTH+4,X'20'     'USE PATTERN TIMES' CHANGED?                 
         BO    BUED0030            NO                                           
*                                                                               
BUED0022 DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWABADDR  BUY DISK ADDR                                
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         L     R6,AIO2                                                          
         GOTO1 VMOVEREC,DMCB,IOAREA,(R6)                                        
         OI    STAT2,X'40'         UPDATE IN PROGRESS                           
*                                                                               
         GOTO1 =A(ELEM20),DMCB,(RC),RR=Y                                        
*                                                                               
         GOTO1 =A(CHECKMG),RR=Y    CHECK IF MULTI-MAKEGOOD IN USE               
*                                                                               
         GOTO1 =A(SECEDIT),DMCB,(RC),RR=Y                                       
*                                  VALIDATE SECTION                             
         GOTO1 =A(PTNEDIT),DMCB,(RC),RR=Y                                       
*                                  VALIDATE PATTERN/NOTATION                    
         GOTO1 =A(UPTEDIT),DMCB,(RC),RR=Y                                       
*                                  VALIDATE 'USE PATTERN TIMES'                 
*                                                                               
         TM    BUYORDCH+4,X'20'    ORD CMT CHANGED?                             
         BZ    BUED0030            YES - CONTINUE                               
         TM    BUYORD2H+4,X'20'    ORD CMT CHANGED                              
         BO    EXXMOD              NO - DONE                                    
                                                                                
BUED0030 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING RCONSEND,R6                                                      
         LA    R3,168              LATEST REP VERSION NOT YET SENT              
         TM    RCONSENF,X'20'      X'20'=REP VERS. NOT ADVANCED                 
         BZ    ERROR                                                            
         DROP  R6                                                               
*                                                                               
BUED0040 DS    0H                                                               
         CLC   BUYACT(3),=C'DEL'  DELETE?                                       
         BE    BUED0050                                                         
         CLC   BUYACT,=C'CAN'      CANCEL?                                      
         BNE   BUED0055                                                         
*                                                                               
BUED0050 DS    0H                                                               
         CLC   =C'DELDDS',CONBACT                                               
         BNE   BUED0053                                                         
         LA    R3,12               INVALID ACTION                               
         CLI   TWAOFFC,C'*'        DDS ONLY                                     
         BNE   ERROR                                                            
*                                                                               
BUED0053 DS    0H                                                               
         GOTO1 =A(DELBUY),DMCB,(RC),RR=Y                                        
         B     EXXMOD                                                           
*                                                                               
BUED0055 DS    0H                                                               
         CLC   CONADV(3),=C'GEN'   DON'T ALLOW BUYS WHEN ADV=GEN &              
         BNE   BUED0060                                                         
         CLC   CONCAT,=C'ZZ'       CATEGORY=ZZ (I.E. GENERAL AVAILS)            
         BNE   BUED0060                                                         
         LA    R3,192                                                           
         B     ERROR                                                            
         SPACE 1                                                                
*              MUST BE NEW BUY                                                  
         SPACE 1                                                                
BUED0060 EQU   *                                                                
         MVI   ECFORMAT,C' '       CLEAR EC FORMAT                              
         CLI   RCONKSTA+4,C' '     TV STATION?                                  
         BE    BUED0160            YES                                          
         CLI   RCONKSTA+4,C'L'     TV STATION?                                  
         BE    BUED0160            YES                                          
         SPACE                                                                  
BUED0080 EQU   *                   FORCE BOP BEFORE BUYS ON NEW CONT.           
         CLC   BUYACT(3),=C'BUX'   SPECIAL NO BUCKET ACTION                     
         BE    BUED0100                                                         
         CLC   BUYACT(3),=C'BUY'                                                
         BNE   BUED0140            ONLY CHECK FOR ACTION BUY                    
         CLC   CONBACT(4),=C'BUYX' SKIP BOP CHECK FOR SPECIAL                   
         BE    BUED0140            OVERRIDE ACTION                              
BUED0100 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    BUED0140                                                         
         BAS   RE,GETSTAT                                                       
         B     BUED0120                                                         
         EJECT                                                                  
*                                                                               
GETSTAT  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           BUILD STATION KEY AND GET RECORD             
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         SPACE                                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GEST0100                                                         
         DC    H'0'                                                             
GEST0100 GOTO1 VGETREC,DMCB,IOAREA                                              
         XIT1                                                                   
         EJECT                                                                  
BUED0120 EQU   *                                                                
         TM    RSTASTAT,X'80'      BOP CHECK OVERRIDE                           
         BO    BUED0140            YES                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    BUED0140            FOUND BOP                                    
         GOTO1 =A(ISTAKOV),RR=Y                                                 
         BZ    BUED0140                                                         
         LA    R3,BOPERR                                                        
         B     ERROR                                                            
         SPACE                                                                  
BOPERR   EQU   173                 MISSING BOP DATA                             
         SPACE 2                                                                
BUED0140 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0200            NO EPL ELEMENT                               
         USING RCONSPEL,R6                                                      
         CLC   RCONSPAM(3),=C'CAN' NO BUYS ALLOWED ON CANCELLED                 
         BNE   BUED0200            CONTRACTS                                    
         LA    R3,190                                                           
         B     ERROR                                                            
         DROP  R6                                                               
         SPACE 1                                                                
BUED0160 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0180            NO SAR ELEMENT                               
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         OC    RSARRDT,RSARRDT     CONTRACT RESOLVED?                           
         BNZ   BUED0180            YES - FIELD CONTAINS DATE                    
         SPACE 1                                                                
         MVC   RSARRDT,TODAY       NO  - RESOLVE CONTRACT                       
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
***>>    GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     BUED0180                                                         
         DROP  R6                                                               
         SPACE 1                                                                
BUED0180 DC    0H'0'                                                            
         BAS   RE,GETSTAT          GET STATION FOR EC FORMAT                    
         MVC   ECFORMAT,RSTATRAF   SAVE EC FORMAT                               
         CLI   TWAACCS,C'$'        IF STATION, GO DIRECTLY TO GET               
         BE    CHGBUY              BUY RECORD, THEN TO T80216                   
         SPACE 1                                                                
BUED0200 CLC   BUYACT(3),=C'CHA'  CHANGE?                                       
         BE    CHGBUY                                                           
         CLC   BUYACT(3),=C'CHX'   SPECIAL NO BUCKET ACTION                     
         BE    CHGBUY                                                           
         EJECT                                                                  
         MVI   RBUYLEN+1,77        ELEM LENGTH (34 + 43)                        
         MVC   RBUYCODE(2),=X'012B'     BUY DESC ELEM CODE + LEN                
*              SET OFF PREVIOUSLY VALID BITS FOR ADD (IN CASE ERROR)            
         LA    R2,BUYDAYSH                                                      
         LA    R3,BUYLAST                                                       
         SR    RE,RE                                                            
*                                                                               
         NI    4(R2),X'DF'                                                      
         IC    RE,0(R2)            LEN                                          
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3                                                            
         BL    *-14                                                             
         FOUT  CONBNUMH,MYSPACES,8                                              
         NI    CONBNUMH+4,X'DF'    TURN OFF VALID BITS IN CASE OF ERROR         
         B     DAYED                                                            
         EJECT                                                                  
*              CHANGE BUY                                                       
CHGBUY   DC    0H'0'                                                            
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWABADDR  BUY DISK ADDR                                
         DROP  RF                                                               
*                                                                               
         TM    STAT2,X'40'         UPDATE ALREADY IN PROGRESS?                  
         BO    CHGB0025            YES - SKIP RE-READ                           
*                                                                               
         LA    R2,BUYDAYSH                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*&&DO                                                                           
         LA    RF,IOAREA                                                        
         CLC   =C'PV',RBUYKREP-RBUYREC(RF)                                      
*                                  PETRY ORDER?                                 
         BE    CHGB0010            YES - SCAN BUY EFF DATE ELTS                 
         CLC   =C'PR',RBUYKREP-RBUYREC(RF)                                      
*                                  PETRY LATINO?                                
         BE    CHGB0010            YES - SCAN BUY EFF DATE ELTS                 
         CLC   =C'B3',RBUYKREP-RBUYREC(RF)                                      
*                                  FOR TESTING?                                 
         BNE   CHGB0020            NO  - SKIP NEXT TEST                         
CHGB0010 EQU   *                                                                
         TM    RCONMODR+1,X'10'    CONVERTED ORDERS ONLY                        
         BZ    CHGB0020                                                         
         GOTO1 =A(CORRSPOT),DMCB,(RC),RR=Y                                      
*&&                                                                             
*                                                                               
CHGB0020 EQU   *                                                                
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC  MOVE TO BUY AREA                   
*        LA    R6,RBUYREC                                                       
*        LA    R6,1000(R6)                                                      
         L     R6,AIO2                                                          
         GOTO1 VMOVEREC,DMCB,IOAREA,(R6)                                        
*                                                                               
*   IF STATION, ALL FIELDS ARE PROTECTED EXCEPT ORDER COMMENT FIELD,            
*   AND SECTION. THEREFORE, EDIT ONLY SECTION AND ORDER COMMENT FIELDS          
*                                                                               
CHGB0025 DS    0H                                                               
         GOTO1 =A(CHECKMG),RR=Y    CHECK IF MULTI-MAKEGOOD IN USE               
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    BUYED2                                                           
*                                                                               
*              IF EITHER DAYS, TIMES OR DATES CHANGE, REVALIDATE ALL            
         TM    BUYDAYSH+4,X'20'    DAYS CHANGE?                                 
         BO    *+12                                                             
         NI    BUYTIMSH+4,X'DF'    TIMES                                        
         NI    BUYDTESH+4,X'DF'    DATES                                        
         TM    BUYTIMSH+4,X'20'    TIMES CHANGE?                                
         BO    *+12                                                             
         NI    BUYDAYSH+4,X'DF'    DAYS                                         
         NI    BUYDTESH+4,X'DF'    DATES                                        
         TM    BUYNPWH+4,X'20'                                                  
         BO    *+8                                                              
         NI    BUYDTESH+4,X'DF'                                                 
*              IF EITHER COMMENT CHANGES REVALIDATE BOTH                        
         TM    BUYCOM1H+4,X'20'    COMMENT 1 CHANGE?                            
         BO    *+12                                                             
         NI    BUYCOM2H+4,X'DF'    COMMENT 2                                    
         NI    BUYPGMH+4,X'DF'     AND PGM FIELD                                
*                                                                               
         TM    BUYCOM2H+4,X'20'    COMMENT 2 CHANGE?                            
         BO    *+12                                                             
         NI    BUYCOM1H+4,X'DF'    COMMENT 1                                    
         NI    BUYPGMH+4,X'DF'     AND PGM FIELD                                
*                                                                               
         TM    BUYPGMH+4,X'20'     PGM CHANGE?                                  
         BO    *+12                                                             
         NI    BUYCOM1H+4,X'DF'    COMMENT 1                                    
         NI    BUYCOM2H+4,X'DF'    COMMENT 1                                    
         TITLE 'T80215 - REPPAK DAY-TIME FIELDS EDIT'                           
*              EDIT DAYS AND TIMES                                              
DAYED    LA    R2,BUYDAYSH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALID? (CHANGE)                   
         BO    LENEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
*              DELETE ALL DAY-TIME ELEMENTS                                     
         GOTO1 VDELELEM,DMCB,(2,RBUYREC)                                        
*                                                                               
         MVC   WORK2(2),=X'0209'   ELEM CODE + LENGTH                           
         MVI   WORK2+8,1           WEIGHTING FACTOR                             
*                                                                               
*              PREPARE TO EDIT STRING OF DAY-TIME FIELDS IN PARALLEL            
         LA    R4,BUYDAYS-1                                                     
         LA    R5,BUYDAYSH                                                      
         LA    R6,BUYTIMS-1                                                     
         LA    R7,BUYTIMSH                                                      
*                                                                               
         STM   R4,R7,DMCB+8        PARAMETERS FOR SCAN                          
         SR    R6,R6                                                            
         SR    R3,R3               START DAY FOR ALL 02 ELEMENTS                
         SR    R7,R7               END DAY                                      
DAYTIMED LA    R2,BUYDAYSH                                                      
         GOTO1 SCAN,DMCB+8         SCAN DAY FIELD TO GET LENGTH                 
         XC    WORK2+2(6),WORK2+2                                               
         CLI   DMCB+8,0            NO DAY ENTRY?                                
         BNE   DAYTIM50                                                         
*              NO DAY LENGTH                                                    
         LTR   R6,R6               ANY DAYS?                                    
         BNZ   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
         CLI   DMCB+20,C'*'        ANOTHER TIME ENTRY?                          
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
* GET START AND END DAYS FOR ALL 02 ELEMENTS                                    
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         STC   R3,RBUYSTED                                                      
*                                                                               
         B     LENEDIT             EDIT NEXT FIELD                              
DAYTIM50 MVC   DMCB2(4),DMCB+8      DAY FIELD ADDR + LEN                        
*                                                                               
*              EDIT DAY FIELD                                                   
         GOTO1 =V(REDAYVAL),DMCB2,,WORK2+3,WORK2+2,RR=YES                       
*                                                                               
         CLI   WORK2+3,0           VALID DAY?                                   
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
*                                                                               
         LA    R6,1(R6)            COUNTER                                      
*                                                                               
*              GET FIRST START DAY AND LAST END DAY                             
         SR    R4,R4               START                                        
         SR    R5,R5               END                                          
         IC    R4,WORK2+2          START-END                                    
         SRDL  R4,4                                                             
         SRL   R5,28               END                                          
         LTR   R3,R3               FIRST 02 ELEMENT?                            
         BNZ   *+6                                                              
         LR    R3,R4               FIRST START DAY IS KEY                       
         CR    R4,R5               START V END                                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CR    R3,R4               CHECK AGAINST 1ST START DAY                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CH    R5,=H'8'            END DAY                                      
         BL    DAY100                                                           
* MAKE SURE NO MORE THAN 7 DAYS COVERED                                         
         LA    R1,7(R3)                                                         
         CR    R1,R5                                                            
         BH    *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR               MORE THAN 7 DAYS                             
DAY100   CR    R5,R7               END                                          
         BNH   *+6                                                              
         LR    R7,R5               NEW HIGH END                                 
*                                                                               
*              EDIT TIME FIELD                                                  
         LA    R2,BUYTIMSH                                                      
*                                                                               
         GOTO1 SCAN,DMCB+16        SCAN NEXT TIME FIELD FOR LENGTH              
*                                                                               
         CLI   DMCB+16,0           NO TIME ENTRY?                               
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
*                                                                               
         ZIC   R4,DMCB+16          ALLOW INPUT OF 'VARIOUS'                     
         L     R5,DMCB+16                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'VARIOUS'                                              
         BNE   DAY140                                                           
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4(4),=C'VARY'                                              
         B     DAY200                                                           
*                                                                               
DAY140   EX    R4,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   0(0,R5),=C'NONE'                                                 
         BNE   DAY150              OR GO TO RETIMVAL                            
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4(4),=C'NONE'                                              
         B     DAY200                                                           
*                                                                               
DAY150   MVC   DMCB(4),DMCB+16     TIME LEN + ADDR                              
         LA    RF,WORK2+4                                                       
         ST    RF,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY155              6AM - 559AM                                  
         MVI   DMCB+4,X'80'                                                     
         DROP  RF                                                               
*                                                                               
DAY155   DS    0H                                                               
         GOTO1 =V(RETIMVAL),DMCB,,,RR=YES  EDIT TIME                            
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
*                                                                               
         OC    WORK2+6(2),WORK2+6  IS THERE AN END TIME?                        
         BZ    DAY200              NO                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY160              6AM - 559AM                                  
         DROP  RF                                                               
*                                                                               
         CLC   WORK2+4(2),=H'0600' START TIME LT 6AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0600' END TIME GT = 6AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
DAY160   DS    0H                                                               
         CLC   WORK2+4(2),=H'0500' START TIME LT 5AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0500' END TIME GT = 5AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*              ADD DAY-TIME ELEMENT TO BUYREC                                   
DAY200   DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        CHECK IF ELECTRONIC CONTRACT                 
         BNE   DAY230                                                           
         DROP  RF                                                               
                                                                                
DAY210   DS    0H                                                               
         BAS   RE,VCROSS           VALIDATE CROSS DAY                           
                                                                                
DAY230   DS    0H                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         B     DAYTIMED            EDIT NEXT DAY TIME FIELD COMBO               
         EJECT                                                                  
********************************************************************            
* FOR BIAS ELECTRONIC CONTRACT, VALIDATE CROSS DAY.  CROSS DAY MUST             
*   HAVE AT LEAST ONE DAY OPEN                                                  
********************************************************************            
VCROSS   NTR1                                                                   
         ZIC   RF,WORK2+2          START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,WORK2+3                                                       
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
                                                                                
         LA    R6,RCONREC          NOW CHECK IF BIAS ELEMENT EXISTS             
         MVI   ELCODE,X'13'        IN CONTRACT                                  
         BAS   RE,GETEL                                                         
         BNE   VCROSS30            ELEMENT HASN'T BEEN ADDED YET                
         USING RCONCCEL,R6                                                      
                                                                                
         LA    R3,399              CANNOT CROSS CROSS DAY DEFAULT               
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    VCROSS10                                                         
         LA    R2,BUYDAYSH                                                      
         B     ERROR               CANNOT CROSS CROSS DAY DEFAULT               
                                                                                
VCROSS10 DS    0H                                                               
         OC    WORK(1),RCONCCCD    COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK,X'7F'          ERROR IF ALL ON                              
         BNO   VCROSS20                                                         
         LA    R2,BUYDAYSH                                                      
         B     ERROR                                                            
                                                                                
VCROSS20 DS    0H                                                               
         MVC   RCONCCCD,WORK       UPDATE NEW CROSS DAY                         
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     VCROSSX                                                          
         DROP  R6                                                               
                                                                                
VCROSS30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
         ZIC   RF,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL2                                                
         STC   RF,RCONCCCD         DAYS IN CROSS DAY FOR THIS BUY               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
VCROSSX  DS    0H                                                               
         B     EXXMOD                                                           
         TITLE 'T80115 - REPPAK BUY LENGTH EDIT'                                
*              EDIT LENGTH                                                      
LENEDIT  LA    R2,BUYLENH                                                       
         LA    R3,LENERR                                                        
         TM    4(R2),X'20'                                                      
         BO    FLTEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VPACK               LENGTH                                       
         LTR   R0,R0                                                            
         BZ    LENE0060                                                         
* VALID SECONDS                                                                 
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0020            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0030            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0020 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0040                                                         
LENE0030 EQU   *                                                                
         CH    R0,=H'120'                                                       
         BNH   LENE0040                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     ERROR                                                            
*                                                                               
LENE0040 STH   R0,HALF                                                          
         MVC   RBUYDUR,HALF                                                     
         B     FLTEDIT                                                          
* TEST FOR MINUTES                                                              
LENE0060 LA    R4,4                                                             
         LA    R5,BUYLEN                                                        
*                                                                               
LENE0080 CLI   0(R5),C'M'          MINUTES?                                     
         BE    LENE0100                                                         
         CLI   0(R5),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(R5),X'F9'                                                      
         BH    ERROR                                                            
         LA    R5,1(R5)                                                         
         BCT   R4,LENE0080                                                      
         B     ERROR                                                            
* PACK MINUTES (MINUTES NOT ALLOWED FOR SPOTPAK XFER)                           
LENE0100 DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0110            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0115            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0110 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0120                                                         
LENE0115 EQU   *                                                                
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     ERROR                                                            
*                                                                               
LENE0120 DS    0H                                                               
         LA    R6,4                                                             
         SR    R6,R4                                                            
         BNP   ERROR                                                            
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,BUYLEN(0)                                                    
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   RBUYDUR,HALF                                                     
         OI    RBUYDUR,X'80'       MINUTES IND                                  
*                                                                               
         TITLE 'T80115 - REPPAK FLIGHT CODE EDIT'                               
*              EDIT FLIGHT CODE                                                 
FLTEDIT  TM    TWASTREO,X'80'      IS STEREO IN USE?                            
         BZ    DATEDIT             NO, SKIP AROUND                              
*                                                                               
         LA    R2,BUYFLTH                                                       
         LA    R3,INVINP                                                        
*                                                                               
         ZIC   R6,BUYFLTH+5        NUM OF INPUT CHARS                           
         LTR   R6,R6               IS IT 0?                                     
         BZ    FLT100              YES                                          
*                                                                               
         TM    BUYFLTH+4,X'08'     IS IT VALID NUMERIC?                         
         BZ    ERROR               NO                                           
*                                                                               
         BCTR  R6,0                GET BINARY OF NUMBER                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,BUYFLT(0)                                                    
         CVB   R6,DUB                                                           
*                                                                               
FLT100   STC   R6,RBUYFLT          PUT NUMBER INTO RECORD                       
*                                                                               
         TITLE 'T80115 - REPPAK EFFECTIVE DATES EDIT'                           
*              VALIDATE DATES                                                   
DATEDIT  LA    R2,BUYDTESH                                                      
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BZ    DATE25                                                           
         TM    BUYDTE2H+4,X'20'    2D DATE FIELD                                
         BO    BUYED2                                                           
DATE25   OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
* SET UP 2 DATE INPUT FIELDS IN WORK3(PRETEND 1 FIELD)                          
         MVC   WORK3(L'BUYDTES+8),BUYDTESH                                      
         ZIC   RE,BUYDTESH+5       LEN OF FIELD 1                               
         ZIC   RF,BUYDTE2H+5       LEN OF FIELD 2                               
         LA    R4,WORK3+8(RE)      END OF FIELD 1                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    DATE30                                                           
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),BUYDTE2     FIELD 2                                      
         LA    RE,1(RF,RE)         NEW LEN                                      
         STC   RE,WORK3+5                                                       
*              DELETE ALL PREVIOUS DATE ELEMENTS (CHANGE)                       
DATE30   DS    0H                                                               
         CLC   =C'CR=',WORK3+8     LOOK FOR SPECIAL CHARS                       
         BNE   DATE40                                                           
         GOTO1 =A(CREDIT),DMCB,(RC),RR=Y                                        
         B     BUYED2                                                           
*                                                                               
* +/- WEEK(S) TO CURRENT DATES?                                                 
DATE40   DS    0H                                                               
         GOTO1 =A(DTADDSUB),DMCB,(RC),RR=Y                                      
*                                                                               
         GOTO1 =A(SAVEMG03),DMCB,(RC),RR=Y                                      
*                                                                               
         GOTO1 VDELELEM,DMCB,(3,RBUYREC)                                        
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
*                                                                               
         MVC   WORK2(2),=X'030B'   DATE ELEM CODE + LEN                         
*              GET LOWEST START DAY AND HIGHEST END DAY IN DAY ELEMENTS         
         SR    R5,R5                                                            
         ZIC   R4,RBUYSTED                                                      
         SRDL  R4,4                                                             
         SRL   R5,28                                                            
         STM   R4,R5,DMCB+16                                                    
*                                                                               
         XC    WORK+24(6),WORK+24  FOR CONSECUTIVE TEST                         
         LA    R7,WORK3+7        FOR SCAN                                       
         ST    R7,DMCB+12                                                       
*              EDIT START DATE                                                  
STARTED  MVC   DMCB(4),DMCB+12                                                  
         LA    R3,SDTERR                                                        
         GOTO1 SCAN,DMCB,,WORK3     SCAN FOR NEXT DATE FIELD                    
*                                                                               
         CLI   DMCB,0              NONE?                                        
         BNE   DATE50                                                           
*              NO DATE                                                          
         OC    WORK+24(6),WORK+24  NO DATES GIVEN?                              
         BZ    ERROR                                                            
         B     DATE240                                                          
DATE50   L     R5,DMCB             FIELD ADDR                                   
         MVC   DMCB+12(4),DMCB     SAVE LEN + ADDR FOR SCAN                     
         CLC   0(2,R5),=C'S-'      K START INDICATOR                            
         BNE   DATE75                                                           
*                                                                               
*              DETERMINE DATE IN FIRST WEEK OF CONTRACT                         
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
*                                                                               
         CLC   FULL(3),MYSPACES    VALID K START DATE?                          
         BNE   *+6                                                              
         DC    H'0'                K ERROR                                      
*                                                                               
         ZIC   RE,DMCB             DAY OF WEEK                                  
         L     R4,DMCB+16          BUY START DAY                                
         SR    R4,RE                                                            
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            NEXT WEEK                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4) GET 4ST DATE                        
*                                                                               
         LA    R5,2(R5)            NEXT FIELD                                   
*                                                                               
         B     DATE150                                                          
DATE75   EQU   *                                                                
*                                                                               
*              EDIT START DATE                                                  
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+12                                     
*                                                                               
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
*                                                                               
         LA    R5,1(R7,R5)         NEXT FIELD                                   
         MVC   WORK+12(2),WORK     K START YEAR                                 
         CLC   WORK+14(4),WORK+2   BUY MMDD V K MMDD                            
         BNL   DATE100                                                          
*              BUY MMDD LOW                                                     
DATE90   CLC   WORK(2),WORK+6      K START AND END YEARS SAME?                  
         BE    ERROR                                                            
         MVC   WORK+12(2),WORK+6   USE K END YEAR                               
DATE100  GOTO1 GETDAY,DMCB,WORK+12,FULL VALIDATE START DATE                     
*                                                                               
         CLC   FULL(3),MYSPACES                                                 
         BE    ERROR                                                            
         LA    R3,SDYERR                                                        
         ZIC   R4,DMCB             START DAY                                    
         C     R4,DMCB+16          SAME AS 1ST DAY?                             
         BE    DATE150                                                          
* TEST FOR FLIGHT BUY WHERE WRONG START DAY OK TO EASE INPUT                    
         CLC   CONBACT(4),=C'BUYF'                                              
         BE    DATE110                                                          
         SPACE 1                                                                
         CLC   WORK+12(2),WORK+6   BUY YEAR SAME AS END YEAR                    
         BE    ERROR                                                            
         B     DATE90              FOR CONTRACTS MORE THAN 1 CALENDER           
* GET CORRECT START DAY                                                         
DATE110  L     R7,DMCB+16          DAY FIELD DAY NO.                            
*                                  YEAR - BUY DATE COULD BE SECOND YEAR         
* GET K START DAY - DEPENDS ON WHETHER K HAS OUT-OF-WEEK DATES                  
         GOTO1 GETDAY,(R1),WORK,FULL                                            
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB             K START DAY                                  
*                                                                               
*              DETERMINE END DAY FROM K END DATE                                
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES    ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB,5              IF END DAY IS A FRIDAY                       
         BE    *+12                                                             
         CLI   DMCB,7              OR IF IT IS A SUNDAY                         
         BNE   *+8                                                              
         LA    R6,1                MAKE WEEK NORMAL FLIGHT WEEK (HAVE           
*                                     IT START ON MONDAY)                       
         LR    R3,R7                                                            
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
         CR    R3,R6               DAY V K START DAY                            
         BL    DATE125                                                          
         CR    R4,R6               DATE DAY V K START DAY                       
         BNL   DATE140                                                          
         SH    R7,=H'7'            PREVIOUS WEEK                                
         B     DATE140                                                          
*                                                                               
DATE125  CR    R4,R6               DATE DAY V K START DAY                       
         BL    DATE140                                                          
         AH    R7,=H'7'            NEXT WEEK                                    
* GET PROPER DATE IN WEEK                                                       
DATE140  GOTO1 ADDAY,(R1),WORK+12,DUB,(R7)                                      
         MVC   WORK+12(6),DUB                                                   
         CLC   WORK+12(6),WORK     DATE V K START DATE                          
         BNL   *+12                                                             
         LA    R3,SDYERR                                                        
         B     ERROR               JUST IN CASE                                 
DATE150  XC    WORK2+8(2),WORK2+8                                               
         CLC   WORK+12(6),WORK+6   BUY START V K END                            
         BNH   *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
         CLC   WORK+12(6),WORK+24  BUY START DATE V LAST ELEM END DATE          
         BH    *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
*              EDIT END DATE                                                    
*              END DATE                                                         
         CLI   0(R5),C'E'          -E INDICATOR?                                
         BNE   DATE175                                                          
*              DETERMINE END DATE FROM K END DATE                               
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES    ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,DMCB             K END DAY                                    
*                                                                               
         S     R4,DMCB+20          BUY END DAY                                  
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            PREVIOUS WEEK                                
         LNR   R4,R4                                                            
*              BACK UP K END DATE TO LAST BUY DATE                              
         GOTO1 ADDAY,DMCB,WORK+6,WORK+18,(R4)                                   
*                                                                               
         LA    R5,1(R5)                                                         
         B     DATE200                                                          
DATE175  EQU   *                   EDIT END DATE                                
         CLI   0(R5),C'*'          NO END DATE?                                 
         BE    DATE177                                                          
         LR    R6,R5                                                            
         BCTR  R6,R0                                                            
         CLI   0(R6),C'*'                                                       
         BE    DATE176                                                          
         CLI   0(R6),C'('                                                       
         BE    DATE176                                                          
         CLI   0(R6),0                                                          
         BE    DATE176                                                          
         CLI   0(R6),C' '                                                       
         BNE   DATE180                                                          
* NO END DATE GIVEN                                                             
DATE176  LR    R5,R6                                                            
DATE177  LM    R6,R7,DMCB+16       START AND END DAYS                           
         CR    R6,R7               END IN NEXT WEEK?                            
         BNH   *+8                                                              
         LA    R7,7(R7)                                                         
         SR    R7,R6                                                            
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(R7)                                  
         B     DATE210                                                          
*                                                                               
DATE180  GOTO1 DATVAL,DMCB,(1,(R5)),WORK+18 END DATE                            
*                                                                               
         L     RE,DMCB             LENGTH                                       
         LTR   RE,RE                                                            
         BNZ   DATE199A                                                         
*                                                                               
* CHECK FOR END WEEKS OPTION                                                    
         LA    R4,1                                                             
         CLI   1(R5),C'W'          WEEKS IND?                                   
         BE    DATE193                                                          
         LA    R4,2                                                             
         CLI   2(R5),C'W'                                                       
         BE    *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
* W HAS BEEN ENTERED - PACK WEEKS                                               
DATE193  LR    R7,R5                                                            
         LR    R3,R5                                                            
         LA    R5,1(R4,R5)         END OF FIELD                                 
         LR    R6,R4                                                            
*                                                                               
DATE195  CLI   0(R7),X'F0'         NUMERIC?                                     
         BNL   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         CLI   0(R7),X'F9'                                                      
         BNH   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         LA    R7,1(R7)                                                         
         BCT   R4,DATE195                                                       
* NUMERIC WEEKS ENTERED                                                         
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R3)                                                      
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         MVC   WORK+30(6),WORK+12   START DATE                                  
         MVC   WORK+18(6),WORK+12  START TO END                                 
         OI    WORK2+8,X'80'       EVERY WEEK                                   
         LA    R3,7                                                             
         BCTR  R4,R0               NUMBER OF WEEKS                              
         LTR   R4,R4                                                            
         BZ    DATE199                                                          
*                                                                               
* TEST FOR ALTERNATE WEEKS                                                      
         CLI   0(R5),C'A'                                                       
         BNE   DATE198                                                          
         OI    WORK2+8,X'40'                                                    
         NI    WORK2+8,X'7F'                                                    
         LA    R5,1(R5)                                                         
         LA    R3,14                                                            
* GET NEXT WEEK                                                                 
DATE198  GOTO1 ADDAY,DMCB,WORK+30,WORK+18,(R3)                                  
         MVC   WORK+30(6),WORK+18                                               
         BCT   R4,DATE198          GET NUMBER OF WEEKS-1                        
*                                                                               
* GET DAY SPAN FOR WEEK                                                         
DATE199  L     R6,DMCB+20          END DAY OF WEEK                              
         C     R6,DMCB+16          END V START DAY                              
         BNL   *+8                                                              
         LA    R6,7(R6)            OUT OF WEEK ROTATOR                          
         S     R6,DMCB+16          GET DAY SPAN                                 
         GOTO1 ADDAY,(R1),WORK+30,WORK+18,(R6)                                  
         B     DATE215                                                          
*                                                                               
* END DATE IS VALID MONTH-DAY                                                   
DATE199A MVC   WORK+18(2),WORK+6   K END YEAR                                   
         CLC   WORK+20(4),WORK+8   BUY END MMDD V K END MMDD                    
         BNH   *+10                                                             
         MVC   WORK+18(2),WORK     MOVE K START YEAR                            
*                                                                               
         LA    R5,0(RE,R5)         FIELD END                                    
*                                                                               
*              VALIDATE END DATE                                                
         GOTO1 GETDAY,DMCB,WORK+18,FULL                                         
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
*                                                                               
         ZIC   R4,DMCB             DAY OF WEEK                                  
*                                                                               
         C     R4,DMCB+20          SAME DAY AS END DAY?                         
         BE    DATE200                                                          
         CLC   CONBACT(4),=C'BUYF'                                              
         BE    *+12                                                             
         LA    R3,EDYERR                                                        
         B     ERROR                                                            
* FLIGHT BUY NEED NOT HAVE PROPER END DATE - FIND WEEK AND ADJUST DATE          
         L     R7,DMCB+20          END DAY                                      
* GET K END DAY                                                                 
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB                                                          
         LR    R3,R7                                                            
*                                                                               
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
DATE199G GOTO1 ADDAY,(R1),WORK+18,DUB,(R7)                                      
         MVC   WORK+18(6),DUB                                                   
*                                                                               
         CLC   WORK+18(6),WORK+6   DATE V K END                                 
         BNH   *+12                                                             
         LA    R3,EDYERR                                                        
         B     ERROR               JUST IN CASE                                 
*                                                                               
DATE200  CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DATE210                                                          
         LA    R5,1(R5)                                                         
         OI    WORK2+8,X'40'                                                    
         B     *+8                                                              
DATE210  OI    WORK2+8,X'80'       EVERY WEEK                                   
DATE215  LA    R3,EDTERR                                                        
         CLC   WORK+18(6),WORK+6   BUY END V K END DATE                         
         BH    ERROR                                                            
*                                                                               
         CLC   WORK+18(6),WORK+12  BY END V BUY START                           
         BL    ERROR                                                            
*                                                                               
         MVC   WORK+24(6),WORK+18  SAVE BUY END DATE FOR CONSECUTIVE            
*                                  TEST                                         
* SEE IF NO. PER WEEK GIVEN                                                     
         CLI   0(R5),C'('                                                       
         BNE   DATE230                                                          
* GET NPW OVERRIDE - FORMAT IS JAN15(4) JAN15-JAN26(5) JAN5-E(6)                
*                           OR JAN15-8W(3)                                      
         LA    R3,NPWERR                                                        
         SR    R1,R1                                                            
         LA    R5,1(R5)                                                         
         LR    RF,R5                                                            
* CHECK NUMBER PER WEEK                                                         
DATE220  CLI   0(R5),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(R5),X'F9'                                                      
         BH    ERROR                                                            
*                                                                               
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=H'3'                                                         
         BH    ERROR                                                            
         CLI   0(R5),C')'                                                       
         BNE   DATE220                                                          
*                                                                               
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'                                                       
         BH    ERROR                                                            
*                                                                               
         OI    WORK2+8,X'01'       NPW OVERRIDE INDICATOR                       
         STC   R1,WORK2+9          NPW                                          
* NOW GET TOTAL WEEKS                                                           
DATE230  SR    R7,R7               CTR                                          
         MVC   WORK+30(6),WORK+12  START                                        
*                                                                               
         LA    R3,7                                                             
         TM    WORK2+8,X'40'       ALT?                                         
         BZ    *+8                                                              
         LA    R3,14                                                            
DATE235  LA    R7,1(R7)                                                         
         GOTO1 ADDAY,DMCB,WORK+30,WORK+36,(R3)                                  
         MVC   WORK+30(6),WORK+36                                               
*                                                                               
         CLC   WORK+30(6),WORK+18  PAST END?                                    
         BNH   DATE235                                                          
*                                                                               
         STC   R7,WORK2+10                                                      
*              CONVERT DATES FOR BUYREC                                         
         GOTO1 DATCON,DMCB,WORK+12,(3,WORK2+2)    START DATE                    
*                                                                               
         GOTO1 (RF),(R1),WORK+18,(3,WORK2+5)      END DATE                      
*                                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2  ADD DATE ELEM TO BUYREC             
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         B     STARTED             SEE IF ANOTHER DATE ENTRY                    
DATE240  EQU   *                                                                
         GOTO1 =A(CHKMGDS),DMCB,(RC),RR=Y                                       
         BZ    BUYED2              ALL MAKEGOODS OKAY                           
         LA    R3,MG1ERR           MAKEGOOD ERROR: SEND MESSAGE                 
         B     ERROR                                                            
         EJECT                                                                  
BUYED2   DS    0H                  THIS WAS CNT16                               
         GOTO1 =A(ELEM20),DMCB,(RC),RR=Y  ADD X'20' ELEM IF NEEDED              
*                                                                               
         MVI   BSTATUS,0           CLEAR OUT STATUS BYTE                        
*                                                                               
*  FOR STATION, DEAL WITH ORDER COMMMENT                                        
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   ADDSPOT                                                          
         SPACE 1                                                                
         LA    R4,2                BCT THROUGH 2 COMMENT LINES                  
         LA    R2,BUYORDCH                                                      
         TM    4(R2),X'20'         HAS 1ST LINE CHANGED                         
         BZ    STA5                                                             
         LA    R2,BUYORD2H                                                      
         TM    4(R2),X'20'         HAS 2ND LINE CHANGED                         
         BO    EXXMOD              NEITHER CHANGED, GET OUT                     
         LA    R2,BUYORDCH         OTHERWISE, REDO BOTH LINES                   
         SPACE 1                                                                
STA5     OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)  YES, DELETE OLD                   
         SPACE 1                                                                
         CLI   5(R2),0            IF EITHER COMMENT LINE IS                     
         BNE   STA7                BLANK, DON'T ADD BLANK ELEMENT               
         LA    R2,BUYORD2H                                                      
         LA    R4,1                ONLY BCT ONCE                                
         CLI   5(R2),0                                                          
         BE    STA10                                                            
         SPACE 1                                                                
STA7     CLC   8(3,R2),=C'#DS'     STATION NOT ALLOWED TO USE                   
         BNE   *+12                'DON'T SEND' FEATURE                         
         LA    R3,2                INVALID INPUT FLD                            
         B     ERROR                                                            
         XC    WORK2(100),WORK2    AND REBUILD NEW ELEMENT                      
         MVI   WORK2,X'84'                                                      
*   WORK2+2 IS ZERO TO DESIGNATE STATION BUY ORDER COMMENT                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+3(0),8(R2)                                                 
         LA    RE,4(RE)            TOTAL LENGTH                                 
         STC   RE,WORK2+1                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         SPACE 1                                                                
         LA    R2,BUYORD2H         LOOK AT 2ND COMMENT LINE                     
         CLI   5(R2),0                                                          
         BE    STA10                                                            
         BCT   R4,STA7                                                          
         SPACE 1                                                                
* UNCONFIRM CONTRACT AND UPDATE VERSION NUMBER IF NECESSARY                     
*                                                                               
STA10    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THERE SHOULD ALREADY BE A '1F'               
                                                                                
         USING RCONXEL,R6                                                       
         OI    RCONCONF,X'80'      TURN ON NOT CONFIRMED                        
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    STA11                                                            
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'       TURN ON CONFIRMED PREVIOUSLY                
         DROP  R6                                                               
                                                                                
STA11    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    STA12                                                            
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
                                                                                
STA12    DS    0H                                                               
         TM    RCONSENF,X'10'      ON MEANS STA VERSION NOT ADVANCED            
         BZ    STA20                                                            
*                                                                               
* ADVANCE STATION VERSION AND UPDATE VERSION DATES                              
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    STA15                                                            
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
STA15    DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'S',RCONREC)                                     
         BNZ   ERROR                                                            
                                                                                
STA20    DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSSV          STORE VERSION NUMBER IN BUY                  
         DROP  R6                                                               
         STC   R1,RBUYVER                                                       
         OI    TAREQ,X'01'         T/A REQ INDICATOR- TO PUT CONTRACT           
         MVI   BYTE,C'O'           UPDATE BUY CHANGE INDICATOR                  
         BAS   RE,ADDCODE                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
ADDSPOT  DS    0H                                                               
*                                                                               
         LA    R3,265                                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    ADDSP000            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    ADDSP010            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
ADDSP000 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   TYPEDIT                                                          
*                                                                               
ADDSP010 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'        X'08' ELEMENT MUST BE UNIQUE                 
         BAS   RE,GETEL                                                         
         BE    TYPEDIT                                                          
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
*                                                                               
         CLI   TWASPES,0                                                        
         BNE   ADDSP10                                                          
                                                                                
* PROFILE TO ALLOW SPOTPAK INTERFACE DATA                                       
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    TYPEDIT             IF OFF, SKIP SPOTPAK INTERFACE ADD           
         B     ERROR                                                            
                                                                                
ADDSP10  DS    0H                                                               
         XC    WORK2,WORK2                                                      
WK2      USING RBUYSPEL,WORK2                                                   
         MVC   WK2.RBUYSPCD(2),=X'0830'                                         
         MVC   WK2.RBUYSPAG,TWASPAG    SPOTPAK AGENCY POWER CODE                
         MVC   WK2.RBUYSPMD,TWASPMD    SPOTPAK MEDIA CODE                       
         MVC   WK2.RBUYSPCL,TWASPCL    SPOTPAK CLIENT CODE                      
         MVC   WK2.RBUYSPPD,TWASPPD    SPOTPAK PRODUCT CODE                     
         MVC   WK2.RBUYSPES,TWASPES    SPOTPAK ESTIMATE NUMBER                  
         MVC   WK2.RBUYSPPP,TWASPPP    SPOTPAK PIGGY PRODUCT CODE               
         MVC   WK2.RBUYSPP1,TWASPP1    SPOTPAK PRODUCT 1 SPLIT                  
         MVC   WK2.RBUYSPP2,TWASPP2    SPOTPAK PRODUCT 2 SPLIT                  
         MVC   WK2.RBUYSPST,RCONKSTA   STATION CALL LETTERS                     
         MVC   WK2.RBUYSADV,RCONKADV   REPPAK ADVERTISER CODE                   
         MVC   WK2.RBUYSPRD,RCONPRD    REPPAK PRODUCT CODE                      
         DROP  WK2                                                              
         DROP  RF                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2   ADD SPOTPAK INTERFACE ELEM         
         B     TYPEDIT                                                          
*                                                                               
TYPEDIT  LA    R2,BUYTYPH          PROGRAM TYPE                                 
         TM    4(R2),X'20'                                                      
         BO    DPTEDIT                                                          
***********************************************************************         
* DDS ONLY, ALLOW UPDATE TO DARE AGENCY LINK - REVISION ONLY                    
*                                                                               
         CLI   TWAOFFC,C'*'        DDS                                          
         BNE   TEDIT20                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        FOR DARE ORDER ONLY                          
         BAS   RE,GETEL                                                         
         BNE   TEDIT20                                                          
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      MUST BE LINKED TO A DARE ORDER               
         BZ    TEDIT20                                                          
         OC    RCONDRRV,RCONDRRV                                                
         BZ    TEDIT20             AND MUST BE A REVISION                       
         DROP  R6                                                               
*                                                                               
         CLI   5(R2),0             ANY LINK??                                   
         BE    TEDIT20                                                          
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'1D'        ORIGINAL SAVED LINK ALREADY EXISTS           
         BAS   RE,GETEL                                                         
         BE    TEDIT10                                                          
*                                                                               
         XC    WORK,WORK                                                        
DARELKD  USING RBUYDREL,WORK                                                    
         MVI   DARELKD.RBUYDRCD,RBUYDRCQ                                        
         MVI   DARELKD.RBUYDRLN,RBUYDRLQ                                        
         MVC   DARELKD.RBUYDRLK,RBUYAGBL                                        
         MVC   DARELKD.RBUYDRDT,TODAY                                           
         DROP  DARELKD                                                          
*                                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK                                       
*                                                                               
TEDIT10  DS    0H                                                               
         GOTO1 VPACK                                                            
         STC   R0,RBUYAGBL         UPDATE DARE LINK                             
         B     DPTEDIT             SKIP TYPE UPDATE                             
***********************************************************************         
*                                                                               
*   NOTE - CHANGING PROGRAM TYPE AND DAYPART ARE CHANGES THAT DO NOT            
*     UP THE VERSION OR MOD NUMBER, OR REQUIRE AN ORDER COMMENT                 
*                                                                               
TEDIT20  DS    0H                                                               
         MVI   RBUYTYP,0                                                        
         CLI   5(R2),0             TYPE FIELD IS OPTIONAL                       
         BE    DPTEDIT                                                          
         SPACE 2                                                                
         LA    R3,58               TYPE MUST BE 1 CHARACTER                     
         CLI   5(R2),1                                                          
         BH    ERROR                                                            
         SPACE 1                                                                
         XC    KEY,KEY             VERIFY PROGRAM TYPE EXISTS                   
         MVI   KEY,X'25'                                                        
         MVC   KEY+24(2),REPALPHA                                               
         MVC   KEY+26(1),8(R2)                                                  
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+12                                                             
         LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
         SPACE 1                                                                
         GOTO1 VMOVE                                                            
         MVC   RBUYTYP,WORK                                                     
*              VALIDATE DAYPART                                                 
DPTEDIT  LA    R2,BUYDPTH          DAYPART                                      
         CLI   5(R2),0             FIELD IS OPTIONAL                            
         BE    DPTEDIT5                                                         
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
*                                                                               
         LA    RE,TWADAYPT                                                      
         LA    R3,L'TWADAYPT       MAX 36 CODES TO CHECK                        
         DROP  RF                                                               
         CLI   0(RE),0                                                          
         BNE   DPTEDIT3                                                         
         LA    RE,DPTTAB                                                        
         LA    R3,L'DPTTAB                                                      
         B     DPTEDIT3                                                         
DPTTAB   DC    CL19'MDERATLWKFNPVSJOXYZ'                                        
*                                                                               
DPTEDIT3 CLC   8(1,R2),0(RE)                                                    
         BE    DPTEDIT5                                                         
         LA    RE,1(RE)                                                         
         BCT   R3,DPTEDIT3                                                      
         LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
*                                                                               
DPTEDIT5 DS    0H                                                               
         GOTO1 VMOVE                                                            
         MVC   RBUYDPT,WORK                                                     
*                                                                               
         TM    TWAFLAGS,TWAFLPTQ      PROFILE FOR PATTERN?                      
         BO    CLSEDIT2               YES- CLASS FIELD DONE                     
*                                                                               
*              VALIDATE CLASS                                                   
CLSEDIT  DS    0H                                                               
         LA    R2,BUYCLSH          CLASS                                        
         TM    4(R2),X'20'                                                      
         BO    PLNEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VMOVE                                                            
         MVC   RBUYCLS,WORK                                                     
         B     PLNEDIT                                                          
*                                                                               
CLSEDIT2 DS    0H                                                               
         GOTO1 =A(PTNEDIT),DMCB,(RC),RR=Y                                       
*                                  VALIDATE PATTERN/NOTATION                    
         GOTO1 =A(UPTEDIT),DMCB,(RC),RR=Y                                       
*                                  VALIDATE 'USE PATTERN TIMES'                 
*                                                                               
PLNEDIT  DS    0H                                                               
         GOTO1 =A(SECEDIT),DMCB,(RC),RR=Y   VALIDATE SECTION                    
*                                                                               
         LA    R2,BUYPLNH          PLAN                                         
         TM    4(R2),X'20'                                                      
         BO    NPWEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R3,PLNERR           PLAN ERROR                                   
         TM    4(R2),X'0C'         MUST BE ALL ALPHA OR ALL NUMERIC             
         BC    0,ERROR                                                          
         CLI   5(R2),3             LEN                                          
         BH    ERROR                                                            
         GOTO1 VMOVE                                                            
         MVC   RBUYKPLN,WORK                                                    
         CLC   RBUYKPLN,=3C' '     ANY PACKAGE?                                 
         BNE   *+10                                                             
         MVC   RBUYKPLN,=3X'FF'    FOR SORT                                     
         SPACE 3                                                                
NPWEDIT  LA    R2,BUYNPWH          NUMBER PER WEEK                              
         CLC   BUYCOM1(3),=C'CR='                                               
         BNE   *+8                                                              
         NI    4(R2),X'DF'                                                      
         SPACE 1                                                                
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    RATEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R3,NPWERR                                                        
*                                                                               
         GOTO1 VPACK                                                            
         BNZ   *+12                                                             
         TM    BUYNPWH+4,X'08'                                                  
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,RBUYNW                                                        
*                                                                               
* UPDATE NPW FOR NA RATE COMBO ALSO                                             
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    RATEDIT                                                          
         DROP  RF                                                               
*                                                                               
         TM    RBUYCOMB,X'80'                                                   
         BZ    RATEDIT                                                          
         MVC   RBUYCOMB,RBUYNW                                                  
         TM    RBUYCOMB,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    RBUYCOMB,X'80'                                                   
         EJECT                                                                  
RATEDIT  LA    R2,BUYRATEH         RATE EDIT                                    
*                                                                               
         LR    RF,RA               SPECIAL IF COMBO K                           
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    REDI0140                                                         
         CLI   TWACMBPT,1                                                       
         BH    REDI0100                                                         
*                                                                               
         ZIC   R4,TWACOMBO         CHECK ALL THE COMBO RATES IN FIRST           
         DROP  RF                                                               
         LA    R2,BUYCBC1H         PASS TO MAKE SURE ALL FIELDS ARE             
         LA    R3,RATERR           VALID BEFORE WE UPDATE THE RECORDS           
*                                                                               
REDI0020 DS    0H                                                               
         CLI   5(R2),0             CHECK IF NO INPUT                            
         BE    REDI0080            -OR-                                         
         CLC   =C'NA',8(R2)        CHECK IF NA RATE                             
         BE    REDI0080            -OR-                                         
         XC    DMCB+4(3),DMCB+4    CHECK IF VALID CASH VALUE                    
         ZIC   RF,5(R2)            CHECK FOR 'TRADE' FLAG                       
         LA    RE,8(R2)            SET A(CASH VALUE FIELD)                      
         AR    RE,RF               ADD L(CASH VALUE INPUT)                      
         BCTR  RE,0                BACK UP 1 CHARACTER                          
         CLI   0(RE),C'T'          'TRADE' INDICATOR?                           
         BE    REDI0030            YES                                          
         TM    BUYFLAGS,X'80'      TRADE CONTRACT?                              
         BO    REDI0035            YES                                          
         B     REDI0040            NO                                           
REDI0030 DS    0H                                                               
         BCTR  RF,0                YES - DECREMENT LENGTH BY 1                  
REDI0035 DS    0H                                                               
         STC   RF,DMCB+7           INSERT INTO PARA LIST                        
         LR    RE,RA                                                            
         AH    RE,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RE                                                       
         OI    TWACMTRD,X'80'      SET 'COMBO TRADE FOUND' FLAG                 
*                                                                               
         DROP  RE                                                               
*                                                                               
         B     REDI0060                                                         
REDI0040 EQU   *                                                                
         MVC   DMCB+7(1),5(R2)                                                  
REDI0060 EQU   *                                                                
         GOTO1 CASHVAL,DMCB,8(R2)                                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
REDI0080 DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R4,REDI0020                                                      
*                                                                               
REDI0100 DS    0H                                                               
*                                                                               
         LA    R2,BUYCBC1H                                                      
*                                                                               
         LR    RF,RA               SPECIAL IF COMBO K                           
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACMBPT,1                                                       
         BE    REDI0140                                                         
*                                                                               
         ZIC   R1,TWACMBPT         BUMP TO DESIRED COMPONENT K                  
         BCTR  R1,0                                                             
         DROP  RF                                                               
*                                                                               
REDI0120 ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,REDI0120                                                      
*                                                                               
REDI0140 DS    0H                                                               
         LR    R5,R2               SAVE ADDR OF CURRENT COMBO RATE FLD          
         CLC   BUYCOM1(3),=C'CR='                                               
         BNE   *+8                                                              
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    COMEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R3,RATERR                                                        
*                                                                               
         TM    RBUYCOMB,X'80'      IF BUYLINE HAS NA PREVIOUSLY AND             
         BZ    REDI0160            HAS $ NOW, MAKE SURE WE MOVE SPT/WK          
         CLC   =C'NA',8(R2)        INTO RBUYNW                                  
         BE    REDI0160                                                         
*                                                                               
         LA    R3,NPWERR                                                        
         LA    R2,BUYNPWH          POINT TO SPT/WK                              
         GOTO1 VPACK               GET SPT/WK                                   
         BNZ   *+12                                                             
         TM    BUYNPWH+4,X'08'                                                  
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,RBUYNW                                                        
         LR    R2,R5               RESTORE R2                                   
*                                                                               
REDI0160 DS    0H                                                               
         XC    RBUYCOMB,RBUYCOMB   DEFAULT BUY TO NON-COMBO                     
         XC    RBUYCOS,RBUYCOS                                                  
         CLI   5(R2),0                                                          
         BE    ADD10                                                            
*                                                                               
         CLC   =C'NA',8(R2)        FOR COMBO BUYS,"NA" MEANS TO ADD THE         
         BNE   REDI0180            BUYLINE WITH 0 SPOTS AND 0 DOLLARS           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          ONLY COMBO BUY CAN HAVE N/A RATE             
         BE    ERROR                                                            
         DROP  RF                                                               
*                                                                               
         LA    R2,BUYNPWH          POINT TO SPOT PER WEEK                       
         GOTO1 VPACK               AND GET THE NUMBER                           
         STC   R0,RBUYCOMB                                                      
         LR    R2,R5               RESTORE R2                                   
*                                                                               
         TM    RBUYCOMB,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                DIE IF #SPOTS OVERWRITES N/A FLAG            
         OI    RBUYCOMB,X'80'      UP COMBO BUY N/A FLAG                        
*                                                                               
         XC    RBUYNW,RBUYNW       0 SPOTS                                      
         XC    RBUYCOS,RBUYCOS     0 DOLLARS                                    
         B     ADD10                                                            
*                                                                               
REDI0180 DS    0H                                                               
*              EDIT RATE                                                        
         XC    DMCB+4(3),DMCB+4                                                 
         ZIC   RF,5(R2)            CHECK FOR 'TRADE' FLAG                       
         LA    RE,8(R2)            SET A(CASH VALUE FIELD)                      
         AR    RE,RF               ADD L(CASH VALUE INPUT)                      
         BCTR  RE,0                BACK UP 1 CHARACTER                          
         CLC   =C'BUY',BUYACT      BUY ACTION?                                  
         BNE   REDI0200            NO  - DON'T RESET TRADE FLAG                 
         NI    RBUYFLG2,X'FF'-X'02'                                             
*                                  TURN OFF POSSIBLE TRADE FLAG                 
REDI0200 EQU   *                                                                
         CLI   0(RE),C'T'          'TRADE' INDICATOR?                           
         BE    REDI0210            YES                                          
         TM    BUYFLAGS,X'80'      TRADE CONTRACT?                              
         BNO   REDI0300            NO                                           
         B     REDI0215            YES                                          
REDI0210 EQU   *                                                                
         TM    BUYFLAGS,X'40'      CASH ORDER IN PROCESS?                       
         BNO   REDI0212            NO  - OKAY TO BOOK TRADE                     
         LA    R3,TRDINCSH         YES - NO TRADE IN CASH                       
         B     ERROR                                                            
REDI0212 EQU   *                                                                
         BCTR  RF,0                YES - DECREMENT LENGTH BY 1                  
REDI0215 EQU   *                                                                
         STC   RF,DMCB+7           INSERT INTO PARA LIST                        
         CLC   =C'BUY',BUYACT      BUY ACTION?                                  
         BNE   REDI0220            NO  - DON'T SET TRADE FLAG                   
         OI    RBUYFLG2,X'02'      YES - SET TRADE BUY FLAG                     
         CLI   RCONKSTA+4,C'A'     AM MEDIA/RADIO ORDER?                        
         BE    REDI0220            YES - CAN BE CASH+TRADE                      
         CLI   RCONKSTA+4,C'F'     FM MEDIA/RADIO ORDER?                        
         BE    REDI0220            YES - CAN BE CASH+TRADE                      
*                                  ONLY SET FOR TV ORDERS                       
         GOTO1 =A(SETRADE),DMCB,(RC),RR=Y                                       
*                                                                               
REDI0220 EQU   *                                                                
         LA    RE,BUYCOMUH         SET COMMENT # 2 TITLE                        
         MVC   BUYCOMU(07),=C'       '                                          
         NI    1(RE),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
         LR    RF,RA               SPECIAL IF COMBO K                           
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          COMBO BUY?                                   
         BE    REDI0240            NO  - SKIP NEXT TEST                         
         TM    TWACMTRD,X'80'      ANY 'TRADE' COMBO BUYS?                      
*                                                                               
         DROP  RF                                                               
*                                                                               
         BNO   REDI0280            NO                                           
         B     REDI0260                                                         
REDI0240 EQU   *                                                                
         TM    RBUYFLG2,X'02'      TRADE ORDER?                                 
         BNO   REDI0280            NO                                           
REDI0260 EQU   *                                                                
         CLC   =C'BUY',BUYACT      BUY ACTION?                                  
         BNE   REDI0270            NO  - LEAVE WHAT'S ON SCREEN                 
         MVC   BUYCOMU(07),=C'*TRADE*'                                          
REDI0270 EQU   *                                                                
DUMP     EQU   *                                                                
         OI    1(RE),X'08'         TURN ON HIGH INTENSITY                       
REDI0280 EQU   *                                                                
         OI    BUYCOMUH+6,X'80'    SET FIELD TO TRANSMIT                        
         B     REDI0320                                                         
REDI0300 EQU   *                                                                
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
REDI0320 EQU   *                                                                
*                                                                               
*   TEST                                                                        
***      MVC   DUMP(2),=X'0000'                                                 
*   TEST END                                                                    
*                                                                               
         GOTO1 CASHVAL,DMCB,8(R2)                                               
*                                                                               
         CLI   DMCB,X'FF'          ERROR?                                       
         BE    ERROR                                                            
*                                                                               
         MVC   RBUYCOS,DMCB+4                                                   
*                                                                               
ADD10    DS    0H                                                               
         LR    RF,RA               SPECIAL DISPLAY FOR COMBO RATES              
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          DISPLAY REST OF COMBO RATE FIELDS            
         BE    ADD15                                                            
         DROP  RF                                                               
*                                                                               
         TM    RBUYCOMB,X'80'      IF COMBO BUY PLACE HOLDER                    
         BZ    ADD12                 DISPLAY "NA"                               
         MVC   8(2,R2),=C'NA'                                                   
         B     ADD13                                                            
*                                                                               
ADD12    DS    0H                                                               
         EDIT  (4,RBUYCOS),(9,8(R2)),2,ALIGN=LEFT,FLOAT=-      RATE             
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   ADD13               NO                                           
         LA    RF,8(R2)                                                         
         AR    RF,R0               ADD LENGTH OF OUTPUT                         
         MVI   0(RF),C'T'          INSERT 'TRADE' INDICATOR                     
ADD13    OI    6(R2),X'80'         XMIT                                         
*                                                                               
ADD15    CLC   BUYACT,=C'BUX'      SPECIAL NO BUCKET ACTION                     
         BE    ADD20                                                            
         CLC   BUYACT,=C'BUY'                                                   
         BNE   COMEDIT                                                          
* GET LINE NUMBER FOR NEW BUY                                                   
ADD20    MVI   RBUYKTYP,X'0B'      BUY KEY TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  RF                                                               
         OI    DMINBTS,X'08'       GET DELETED RECORDS                          
*              FIND NEXT LINE NUMBER                                            
         XC    HALF,HALF           LINE NUMBER                                  
         MVC   KEY(22),RBUYREC                                                  
         XC    KEY+22(10),KEY+22                                                
         GOTO1 VHIGH                                                            
*                                                                               
ADD50    CLC   KEY(22),KEYSAVE                                                  
         BNE   ADD100                                                           
         CLI   KEY+26,255          PLANREC?                                     
         BE    ADD75                                                            
         CLC   HALF+1(1),KEY+26                                                 
         BNL   *+10                                                             
         MVC   HALF+1(1),KEY+26    HIGHEST LINE NUMBER SO FAR                   
         SPACE 1                                                                
ADD75    OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         B     ADD50                                                            
*                                                                               
ADD100   LH    RE,HALF             LAST LINE NUMBER                             
         LA    RE,1(RE)                                                         
         CH    RE,=H'254'                                                       
         BNH   *+16                                                             
         LA    R2,CONBACTH                                                      
         LA    R3,MAXERR                                                        
         B     ERROR                                                            
*                                                                               
         STC   RE,RBUYKLIN         BUY LINE NUMBER                              
         STC   RE,RBUYKMLN         IN CASE NO MAKE-GOOD                         
         NI    DMINBTS,X'F7'       TURN OFF DELETE PASS                         
         EJECT                                                                  
*              VALIDATE BUY COMMENTS                                            
COMEDIT  EQU   *                                                                
         LA    R2,BUYCOM1H                                                      
         TM    4(R2),X'20'         WERE COMMENTS INPUT THIS TIME                
         BO    COED0040            NO                                           
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
*                                                                               
         LA    R3,881              'P=' FORMAT NO LONGER ALLOWED                
         CLC   =C'P=',BUYCOM1                                                   
         BE    ERROR                                                            
         CLC   =C'P=',BUYCOM2                                                   
         BE    ERROR                                                            
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   COED0040            NO COMMENTS ELEMENT                          
         USING RBUYCMEL,R6                                                      
         CLC   RBUYCMNT(3),=C'CR=' DOES COMMENT START WITH CR NOTATION          
         BNE   COED0040            NO                                           
         ZIC   R1,RBUYCMLN         YES-MOVE IT TO SCREEN                        
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     COED0040                                                         
         MVC   8(0,R2),RBUYCMNT                                                 
         DROP  R6                                                               
         SPACE                                                                  
COED0040 CLC   BUYCOM1(3),=C'CR='                                               
         BNE   *+8                                                              
         NI    4(R2),X'DF'                                                      
         SPACE 1                                                                
         TM    4(R2),X'20'                                                      
         BO    ORDCOM                                                           
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
*                                                                               
         XC    IOAREA(32),IOAREA   NO OLD MG LINE                               
*                                  ELIMINATE OLD MG REFERENCE,IF ANY            
*                                  GET OLD MG ELEM                              
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   COED0320            NOT FOUND                                    
*                                  GET OLD MISSED LINE                          
         MVC   KEY,RBUYKEY                                                      
         XC    KEY+22(10),KEY+22                                                
         GOTO1 VHIGH                                                            
         B     COED0120                                                         
COED0080 EQU   *                                                                
         GOTO1 VSEQ                                                             
COED0120 EQU   *                                                                
         CLC   KEY(22),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                OLD MISSED LINE NOT THERE                    
*                                                                               
         CLC   KEY+26(1),2(R6)                                                  
         BNE   COED0080                                                         
*                                  OLD MISSED RECORD FOUND                      
*                                     ELIMINATE MISSED ELEM                     
         GOTO1 VGETREC,DMCB,IOAREA                                              
COED0140 EQU   *                                                                
         LA    R3,IOAREA+34                                                     
         SR    R4,R4                                                            
COED0160 EQU   *                                                                
         CLI   0(R3),0             LAST?                                        
         BE    COED0280                                                         
         CLI   0(R3),6             MISSED?                                      
         BE    COED0240                                                         
         CLI   0(R3),7             CREDIT FOR MISSED                            
         BE    COED0240                                                         
         CLI   0(R3),X'56'         MISSED (NEW SPLIT MG FORMAT)?                
         BE    COED0240                                                         
COED0200 EQU   *                                                                
         IC    R4,1(R3)            LEN                                          
         AR    R3,R4               NEXT ELEM                                    
         B     COED0160                                                         
*                                  CHECK FOR THIS MG LINE NUMBER                
COED0240 EQU   *                                                                
         CLC   5(1,R3),RBUYKLIN                                                 
         BNE   COED0200                                                         
         MVC   SAV56ORI(7),0(R3)   SAVE ELEMENT ABOUT TO BE                     
*                                     DELETED - ONLY USED IF X'56'              
*                                  DELETE OLD                                   
         GOTO1 VRECUP,DMCB,(2,IOAREA),(R3),(R3)                                 
         CLI   SAV56ORI,X'56'      MISSED (NEW SPLIT MG FORMAT)?                
         BNE   COED0280            NO  -                                        
*                                  YES - NEED TO REINSERT SPOT                  
         GOTO1 =A(MGPROC),DMCB,(RC),RR=Y                                        
*                                                                               
*        GOTO1 =A(MERGE03S),DMCB,(RC),RR=Y                                      
COED0280 EQU   *                                                                
         BAS   RE,NEXTEL           FIND NEXT MG REF ELEMENT                     
         BE    COED0140            PROCESS NEXT ONE                             
*        LA    R7,RBUYREC                                                       
*        LA    R7,3000(R7)                                                      
         L     R7,AIO4                                                          
         GOTO1 VMOVEREC,(R1),IOAREA,(R7)                                        
*                                  DELETE MAKE-GOOD ELEMENTS                    
         GOTO1 VDELELEM,DMCB,(5,RBUYREC)                                        
*                                  DELETE MAKEGOOD ELEMENT                      
*                                                                               
* SEE IF WE NEED TO PASS DETAIL COMMENT FROM THE OFFER SCREEN/RECORD            
* IF EXISTING COMMENT STARTS WITH CREDIT:, THEN KEEP THE FIRST LINE OF          
* COMMENT SINCE THIS CAME FROM PREEMPT, AND DELETE THE REST.                    
COED0320 EQU   *                                                                
*&&DO                                                                           
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAMKGF2,X'80'      MAKEGOOD APPLY??                             
         BZ    COED0340            DELETE COMMENTS (CHANGE)                     
         DROP  RF                                                               
*                                                                               
COED0330 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   COED0350            NOT FOUND                                    
         USING RBUYCMEL,R6                                                      
         CLC   =C'CREDIT:',RBUYCMNT                                             
         BE    COED0335                                                         
         CLC   =C'MG=',RBUYCMNT                                                 
         BE    COED0335                                                         
         CLC   =C'CR=',RBUYCMNT                                                 
         BNE   COED0340                                                         
*                                                                               
COED0335 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   COED0350                                                         
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),0,0                                 
         B     COED0330                                                         
         DROP  R6                                                               
*                                                                               
*&&                                                                             
COED0340 DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(4,RBUYREC)                                        
*                                                                               
COED0350 DS    0H                                                               
         MVC   RBUYKMLN,RBUYKLIN   FOR CHANGE IF MG REF ELIMINATED              
         MVI   WORK2,4             ELEM CODE                                    
         CLI   5(R2),0             ENTRY?                                       
         BE    COED1720                                                         
         MVC   WORK2+2(L'BUYCOM1),8(R2)                                         
         IC    RE,5(R2)            LEN OF INPUT                                 
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          LEN                                          
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
*                                  ADD COMMENT 1                                
         CLC   WORK2+2(3),=C'MG='  MAKE-GOOD?                                   
         BE    COED0440                                                         
         CLC   WORK2+2(3),=C'CR='  CREDIT                                       
         BNE   COED1720                                                         
*                                                                               
         LA    R3,131              CR= MUST HAVE NEGATIVE RATE                  
         LA    R2,BUYRATEH                                                      
*                                                                               
         LR    RF,RA               IF COMBO, GET ADDRESS OF CURRENT             
         AH    RF,=Y(TWAWORKQ)     COMBO RATE FIELD                             
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    COED0360                                                         
         DROP  RF                                                               
         LR    R2,R5               RESTORE R2                                   
*                                                                               
COED0360 EQU   *                                                                
         OC    RBUYCOS,RBUYCOS     OK TO CREDIT FOR ZERO RATE                   
         BZ    COED0370                                                         
         TM    RBUYCOS,X'80'                                                    
         BNO   ERROR                                                            
*                                                                               
COED0370 EQU   *                                                                
         LA    R2,BUYCOM1H                                                      
         SPACE 1                                                                
         XC    WORK(10),WORK                                                    
         MVC   WORK(2),=X'050A'    MG REF ELEMENT                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+3(6),2(R6)     START-END DATE                               
         MVC   WORK+9(1),RBUYNW    NUMBER PER WEEK                              
         SPACE 1                                                                
         LA    R3,130              CR=N(N)   N=LINE NUMBER                      
         CLI   WORK2+5,C'0'                                                     
         BL    ERROR                                                            
         CLI   WORK2+5,C'9'                                                     
         BH    ERROR                                                            
         XR    R1,R1                                                            
         CLI   WORK2+6,X'41'                                                    
         BL    COED0400                                                         
         LA    R1,1(R1)                                                         
         CLI   WORK2+6,C'0'                                                     
         BL    ERROR                                                            
         CLI   WORK2+6,C'9'                                                     
         BH    ERROR                                                            
*                                                                               
         CLI   WORK2+7,X'41'                                                    
         BL    COED0400                                                         
         LA    R1,1(R1)                                                         
         CLI   WORK2+7,C'0'                                                     
         BL    ERROR                                                            
         CLI   WORK2+7,C'9'                                                     
         BH    ERROR                                                            
COED0400 EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK2+5(0)                                                   
         CVB   RE,DUB                                                           
         STC   RE,WORK+2           LINE NUMBER                                  
         B     COED0960                                                         
COED0440 EQU   *                                                                
         MVI   MG2NDATE,C'N'       SET 2ND DATE FLAG TO 'OFF'                   
         XC    WORK(10),WORK                                                    
*                                  EDIT MAKE-GOOD REFERENCE                     
         MVC   WORK(2),=X'050A'    ELEM CODE + LEN                              
COED0480 EQU   *                                                                
         CLI   MG2NDATE,C'Y'       FIRST DATE IN PROGRESS?                      
         BE    COED0520            NO                                           
         GOTO1 DATVAL,DMCB,(1,BUYCOM1+3),DUB                                    
*                                  YES                                          
         B     COED0560                                                         
COED0520 EQU   *                                                                
         L     R3,SAVMGADR         SET A(DELIMITER)                             
         LA    R3,1(R3)            BUMP TO NEXT CHAR:  DATE STRING              
         GOTO1 DATVAL,DMCB,(1,(R3)),DUB                                         
*                                                                               
MGADERR  EQU   470                                                              
*                                                                               
COED0560 EQU   *                                                                
         LA    R3,MGDERR                                                        
         CLI   MG2NDATE,C'N'       FIRST DATE IN PROGRESS?                      
         BE    COED0580            YES                                          
         LA    R3,MGADERR          NO  - INSERT ADD'L DATE FORMAT ERR           
COED0580 EQU   *                                                                
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                  FORMAT IS MG=JAN15-N                         
         GOTO1 DATCON,DMCB,DUB,(3,WORK+3)                                       
         MVI   WORK+9,1            NO. SPOTS                                    
         MVC   WORK+3(1),RCONDATE  CONTRACT START YEAR                          
         CLC   WORK+4(2),RCONDATE+1                                             
*                                  VS CONTRACT START MONTH AND DAY              
         BNL   *+10                                                             
         MVC   WORK+3(1),RCONDATE+3                                             
*                                  CONTRACT END YEAR                            
         CLI   MG2NDATE,C'N'       FIRST DATE IN PROGRESS?                      
         BE    COED0640            YES - SET LINE NUMBER                        
         L     RE,SAVMGADR         NO  - SCAN FOR # SPOTS                       
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         XC    SAVMGADR,SAVMGADR   CLEAR 'ANOTHER' FLAG                         
COED0600 EQU   *                                                                
         CLI   0(RE),C','          ANOTHER FIELD FOLLOWS?                       
         BE    COED0605            YES                                          
         CLI   0(RE),C'+'          ANOTHER ON NEXT LINE?                        
         BNE   COED0610            NO                                           
         LA    R2,BUYCOM2H         SET A(FIELD HEADER) FOR ERROR                
         LA    RF,BUYCOM2          CONTINUATION ON 1ST COMMENT LINE?            
         CR    RE,RF               A(+) MUST BE LESS A(COMM2)                   
         BNL   ERROR                                                            
         LA    RE,BUYCOM2-1        YES - SET TO NEXT LINE,                      
*                                     ONE POSITION BACK                         
COED0605 EQU   *                                                                
         ST    RE,SAVMGADR         YES - SAVE A(DELIMITER)                      
         B     COED0960            # SPOTS = 1: NO FURTHER CHECKS               
COED0610 EQU   *                                                                
         CLI   0(RE),0             LAST?                                        
         BE    COED0960            YES - # SPOTS = 1                            
         CLI   0(RE),C' '          LAST?                                        
         BE    COED0960            YES - # SPOTS = 1                            
         CLI   0(RE),C'('          NUMBER SPOTS FOLLOWS?                        
         BE    COED0820            YES - VALIDATE NUMBER OF SPOTS               
         CLI   0(RE),C'-'          NO  - WRONG MONTH FORMAT?                    
         BE    ERROR               YES - WRONG MONTH FORMAT                     
         LA    RE,1(RE)            NO  - SKIP OVER DATE FIELD AGAIN             
         B     COED0600            RESCAN FIELD                                 
COED0640 EQU   *                                                                
         LA    RE,WORK2+9          LINE NUMBER                                  
         CLI   WORK2+8,C'-'                                                     
         BE    COED0680                                                         
         LA    RE,WORK2+10         LINE NUMBER                                  
         CLI   WORK2+9,C'-'                                                     
         BE    COED0680                                                         
         LA    RE,WORK2+11         LINE NUMBER                                  
         CLI   WORK2+10,C'-'                                                    
         BNE   ERROR                                                            
* PACK LINE NUMBER                                                              
COED0680 EQU   *                                                                
         LR    R4,RE                                                            
         SR    R1,R1                                                            
         XC    SAVMGADR,SAVMGADR   TURN OFF 'ANOTHER DATE'                      
*                                                                               
COED0720 EQU   *                                                                
         CLI   0(RE),C','          ANOTHER ON LINE?                             
         BE    COED0740            YES                                          
         CLI   0(RE),C'+'          ANOTHER ON NEXT LINE?                        
         BNE   COED0760            NO                                           
         LA    R2,BUYCOM2          CONTINUATION ON 1ST COMMENT LINE?            
         CR    RE,R2               A(+) MUST BE LESS A(COMM2)                   
         BNL   ERROR                                                            
         LA    RE,BUYCOM2-1        YES - SET TO NEXT LINE,                      
*                                     ONE POSITION BACK                         
COED0740 EQU   *                                                                
         ST    RE,SAVMGADR         SAVE A(DELIMITER)                            
         B     COED0800            PROCEED                                      
COED0760 EQU   *                                                                
         CLI   0(RE),C' '          LAST?                                        
         BE    COED0800                                                         
         CLI   0(RE),0             LAST?                                        
         BE    COED0800                                                         
         CLI   0(RE),C'('                                                       
         BE    COED0800                                                         
         CLI   0(RE),X'F0'         VALID NUMBER?                                
         BL    ERROR                                                            
         CLI   0(RE),X'F9'                                                      
         BH    ERROR                                                            
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CH    R1,=H'3'                                                         
         BH    ERROR                                                            
         B     COED0720                                                         
*                                                                               
COED0800 LTR   R1,R1                                                            
         BZ    ERROR                                                            
*                                                                               
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         LINE NUMBER                                  
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
*                                                                               
         STC   R0,WORK+2                                                        
         CLI   0(RE),C'('          NUMBER OF SPOTS FOLLOWS?                     
         BE    COED0820            YES - VALIDATE                               
         CLI   0(RE),C','          NO  - ANOTHER DATE STRING FOLLOWS?           
         BE    COED0810            YES                                          
         CLI   0(RE),C'+'          ANOTHER ON NEXT LINE?                        
         BNE   COED0960            NO                                           
         LA    R2,BUYCOM2H         SET A(FIELD HEADER) FOR ERROR                
         LA    RF,BUYCOM2          CONTINUATION ON 1ST COMMENT LINE?            
         CR    RE,RF               A(+) MUST BE LESS A(COMM2)                   
         BNL   ERROR                                                            
         LA    RE,BUYCOM2-1        YES - SET TO NEXT LINE,                      
*                                     ONE POSITION BACK                         
COED0810 EQU   *                                                                
         ST    RE,SAVMGADR         YES - SAVE A(DELIMITER)                      
         B     COED0960                                                         
COED0820 LTR   R1,R1                                                            
*                                  GET NUMBER OF MISSED SPOTS                   
         LA    RE,1(RE)                                                         
         LR    R4,RE                                                            
         SR    R1,R1                                                            
*                                  FORMAT IS MG=FEB4-1(3)                       
*                                     OR MG=FEB4=1(0)                           
         LA    R3,MG3ERR                                                        
COED0840 EQU   *                                                                
         CLI   0(RE),C')'                                                       
         BE    COED0880                                                         
         CLI   0(RE),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(RE),X'F9'                                                      
         BH    ERROR                                                            
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CH    R1,=H'3'                                                         
         BH    ERROR                                                            
         B     COED0840                                                         
COED0880 EQU   *                                                                
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
COED0920 EQU   *                                                                
         PACK  DUB,0(0,R4)         MISSED NUMBER OF SPOTS                       
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,WORK+9           NO. MISSED SPOTS                             
         LA    RE,1(RE)            CHECK CHAR FOLLOWING ')'                     
         CLI   0(RE),C','          ANOTHER DATE STRING FOLLOWS?                 
         BE    COED0940            NO                                           
         CLI   0(RE),C'+'          ANOTHER ON NEXT LINE?                        
         BNE   COED0960            NO                                           
         LA    R2,BUYCOM2H         SET A(FIELD HEADER) FOR ERROR                
         LA    RF,BUYCOM2          CONTINUATION ON 1ST COMMENT LINE?            
         CR    RE,RF               A(+) MUST BE LESS A(COMM2)                   
         BNL   ERROR                                                            
         LA    RE,BUYCOM2-1        YES - SET TO NEXT LINE,                      
*                                     ONE POSITION BACK                         
COED0940 EQU   *                                                                
         ST    RE,SAVMGADR         YES - SAVE A(DELIMITER)                      
COED0960 EQU   *                                                                
         CLC   WORK+2(1),RBUYKLIN  SAME AS THIS (CHANGE)?                       
         BE    ERROR                                                            
         CLC   WORK+2(1),IOAREA+26                                              
*                                  MISSED LINE ALREADY IN CORE?                 
         BE    COED1080                                                         
*                                  SEE IF MAKE-GOOD LINE EXISTS                 
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+16(2),REPALPHA                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+18(4),TWACNUM                                                
         DROP  RF                                                               
         LA    R3,MGNERR           MAKE-GOOD RECORD ERROR                       
         GOTO1 VHIGH                                                            
         B     COED1040                                                         
COED1000 EQU   *                                                                
         GOTO1 VSEQ                                                             
COED1040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   ERROR                                                            
*                                                                               
*        CLC   KEY+25(1),KEY+26    MUST BE NON-MAKEGOOD LINE                    
*        BNE   COED1000                                                         
*        CLC   KEY+25(1),WORK+2    MISSED LINE?                                 
         CLC   KEY+26(1),WORK+2    ALLOW MAKEGOOD FOR MAKEGOOD                  
         BNE   COED1000                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    RF,IOAREA                                                        
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    *+12                NO - OK                                      
         LA    R3,880                                                           
         B     ERROR                                                            
*                                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED BUY?                               
         BNE   COED1050            NO - OK                                      
         LA    R3,983                                                           
         B     ERROR                                                            
         DROP  RF                                                               
*                                                                               
* CANNOT HAVE MAKEGOOD AGAINST CREDIT                                           
*                                                                               
COED1050 DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   COED1080                                                         
         USING RBUYCMEL,R6                                                      
         CLC   =C'CR=',RBUYCMNT                                                 
         BNE   COED1080                                                         
         LA    R3,650                                                           
         B     ERROR                                                            
         DROP  R6                                                               
         EJECT                                                                  
*                                  CHECK FOR VALID DATE                         
*                                     (NOT FEB29 IN NON-LEAP YEAR)              
COED1080 EQU   *                                                                
CKMISS   NTR                                                                    
         GOTO1 DATCON,DMCB,(3,WORK+3),DUB                                       
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),MYSPACES                                                 
         BNE   COED1120                                                         
MISSERR  MVI   TEMP,X'FF'                                                       
         XIT                                                                    
         LA    R3,MG2ERR                                                        
         B     ERROR                                                            
COED1120 EQU   *                                                                
         MVC   FULL(1),DMCB        DAY OF WEEK                                  
*                                  GET VALID DAYS FOR MISSED LINE               
         MVI   FULL+1,0                                                         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   COED1160                                                         
         OC    FULL+1(1),3(R6)     GET VALID DAYS IN FULL+1                     
         BAS   RE,NEXTEL                                                        
         BE    *-10                                                             
*                                  CHECK MISSED DAY IN MG VS                    
*                                     VALID MISSED LINE DAYS                    
COED1160 EQU   *                                                                
         ZIC   R4,FULL             MISSED DAY OF WEEK                           
         LA    R5,128              X'80'                                        
         SRL   R5,1                                                             
         BCT   R4,*-4                                                           
*                                                                               
         EX    R5,*+8                                                           
         B     *+8                                                              
         TM    FULL+1,X'00'                                                     
         BZ    MISSERR                                                          
*                                  CHECK MISSED DAY VS EFFECTIVE                
*                                     WEEKS OF MISSED LINE                      
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   MISSERR                                                          
*                                                                               
         GOTO1 DATCON,(R1),(3,WORK+3),WORK+14                                   
*                                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         LA    RE,IOAREA                                                        
         IC    R4,RBUYSTED-RBUYREC(RE)   LOW AND HI DAYS                        
         SRDL  R4,4                                                             
         SRL   R5,28                                                            
         CR    R4,R5                                                            
         BNH   *+8                                                              
         LA    R5,7(R5)            OUT OF WEEK ROTATOR                          
         SR    R5,R4               R7 NOW HAS DAY SPAN IN WEEK                  
COED1200 EQU   *                                                                
         GOTO1 DATCON,(R1),(3,2(R6)),WORK+20                                    
*                                  START DATE                                   
         GOTO1 (RF),(R1),(3,5(R6)),WORK+26                                      
*                                  END   DATE                                   
*                                  GET LAST DAY IN WEEK                         
COED1240 EQU   *                                                                
         GOTO1 ADDAY,(R1),WORK+20,WORK+32,(R5)                                  
*                                                                               
*                                  CHECK IF DATE WITHIN THIS WEEK               
         CLC   WORK+14(6),WORK+20                                               
         BL    MISSERR                                                          
*                                  TEST AGAINST END OF THIS WEEK                
         CLC   WORK+14(6),WORK+32                                               
         BNH   COED1280                                                         
*                                  NOT THIS WEEK - TRY NEXT                     
         LA    R3,7                                                             
         TM    8(R6),X'40'         ALTERNATE WEEKS?                             
         BZ    *+8                                                              
         LA    R3,14                                                            
         GOTO1 ADDAY,(R1),WORK+20,WORK+38,(R3)                                  
         MVC   WORK+20(6),WORK+38                                               
         CLC   WORK+20(6),WORK+26  RUNNING DATE V ELEM END DATE                 
         BNH   COED1240                                                         
*                                  GET NEXT DATE ELEM                           
         BAS   RE,NEXTEL                                                        
         BE    COED1200                                                         
         B     MISSERR                                                          
*                                                                               
*                                  CHECK MISSED SPOTS FOR                       
*                                     ENOUGH SPOTS IN WEEK                      
COED1280 EQU   *                                                                
         SR    R3,R3                                                            
         IC    R3,9(R6)            NO. SPOTS PER WEEK                           
         ST    R6,A03ELT           SAVE A(TARGET X'03' ELEMENT)                 
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'        CHECK MISSED SPOT ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    COED1320                                                         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'07'        CHECK CREDIT ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    COED1320                                                         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'66'        CHECK MAKEGOOD OFFER ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   COED1560                                                         
*                                                                               
* SKIP TOTAL MISSED SPOT CHECK IF 'APPLYING' MAKEGOODS                          
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG+1,X'80'                                                 
         BO    COED1560                                                         
         DROP  RF                                                               
                                                                                
         B     COED1440            PROCESS MAKEGOOD OFFER SEPARATELY            
*                                  PROCESS X'06' AND X'07' ELEMENTS             
*                                  SEE IF MISSED SPOT IN THIS WEEK              
COED1320 EQU   *                                                                
         LR    R4,R6                                                            
         CLI   0(R6),X'66'         SKIP REFERENCE DATA FOR X'66'                
         BNE   COED1330                                                         
         LA    R4,5(R4)                                                         
*                                                                               
COED1330 EQU   *                                                                
         GOTO1 DATCON,(R1),(3,2(R4)),WORK+38                                    
*                                  CHECK IF WITHIN RELEVANT WEEK                
*                                  TEST START OF WEEK                           
         CLC   WORK+38(6),WORK+20                                               
         BL    COED1360                                                         
*                                  TEST AGAINST END OF WEEK                     
         CLC   WORK+38(6),WORK+32                                               
         BH    COED1400            FINISHED WITH X'06'/X'07'S                   
*                                     GO CHECK X'66' ELTS                       
         SR    RE,RE                                                            
         IC    RE,6(R4)            NO. OF MISSED SPOTS                          
         SR    R3,RE               SUBTRACT FROM NUMBER PER WEEK                
COED1360 EQU   *                                                                
         ZIC   R4,1(R6)            NEXT ELEM                                    
         AR    R6,R4                                                            
         CLI   0(R6),6             MISSED ELEM?                                 
         BE    COED1320            YES - CHECK IT OUT                           
         CLI   0(R6),7             CREDIT ELEMENT?                              
         BE    COED1320            YES - CHECK IT OUT                           
*                                  NOT X'06' OR X'07' - CHECK X'66'             
* SEE IF MAKEGOOD OFFER SPOT IN THIS WEEK                                       
*     FORMAT OF MAKEGOOD OFFER MISSED SPOT ELEMENT IS DIFFERENT                 
*        FROM ORIGINAL MISSED SPOT ELEMENT.  THEREFORE, IT IS                   
*        HANDLED IN SEPARATE CODE.                                              
*                                                                               
*                                  NEED TO GET 1ST X'66' IF DROP-THRU           
COED1400 EQU   *                                                                
                                                                                
* SKIP TOTAL MISSED SPOT CHECK IF 'APPLYING' MAKEGOODS                          
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG+1,X'80'                                                 
         BO    COED1560                                                         
         DROP  RF                                                               
                                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'66'        CHECK MAKEGOOD OFFER ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   COED1560            ELEMENT NOT FOUND - TEST COUNTER             
COED1440 EQU   *                                                                
         GOTO1 DATCON,(R1),(3,7(R6)),WORK+38                                    
*                                 CHECK IF WITHIN RELEVANT WEEK                 
*                                 TEST START OF WEEK                            
         CLC   WORK+38(6),WORK+20                                               
         BL    COED1520                                                         
*                                  TEST AGAINST END OF WEEK                     
         CLC   WORK+38(6),WORK+32                                               
         BH    COED1560                                                         
         ZIC   RF,6(R6)            CHECK MULTI-LINE COUNTER                     
         SLL   RF,28               DROP HIGH-ORDER NYBBLE                       
         SRL   RF,28                                                            
         CH    RF,=H'1'            FIRST RECORD?                                
         BH    COED1520            NO  - MULTI-LINE SET - SKIP IT               
COED1480 EQU   *                                                                
         SR    RE,RE                                                            
         IC    RE,11(R6)           NO. OF MISSED SPOTS                          
         SR    R3,RE               SUBTRACT FROM NUMBER PER WEEK                
*                                                                               
*                                                                               
COED1520 EQU   *                                                                
         ZIC   R4,1(R6)            NEXT ELEM                                    
         AR    R6,R4                                                            
         CLI   0(R6),X'66'         MAKEGOOD OFFER ELEMENT?                      
         BE    COED1440            YES - GO BACK AND PROCESS                    
COED1560 EQU   *                                                                
         ST    R3,DMCB             SPOT CTR                                     
         XIT                                                                    
*                                                                               
*                                  CHANGE TO MAKE-GOOD STATUS                   
         LA    R3,MG2ERR           INVALID MISSED DATE                          
         L     R6,DMCB             SPOT CTR                                     
         SR    RE,RE                                                            
         IC    RE,WORK+9           NO. OF MISSED SPOTS                          
         SR    R6,RE               ANY LEFT IN WEEK?                            
         BM    ERROR                                                            
         EJECT                                                                  
*                                                                               
*                                  ADD REFERENCE TO MISSED LINE                 
COED1600 EQU   *                                                                
         MVC   DUB(2),=X'0607'     ELEM CODE + LEN                              
         MVC   DUB+2(3),WORK+3     MISSED DATE                                  
         MVC   DUB+5(1),RBUYKLIN   MAKE-GOOD LINE                               
         MVC   DUB+6(1),WORK+9     NO. OF MISSED SPOTS                          
         CLC   WORK2+2(3),=C'CR='                                               
         BNE   *+8                                                              
         MVI   DUB,7               ELEMENT 0M FOR MISSED-CREDIT ISSUED          
*                                                                               
         LA    R3,IOAREA+34                                                     
         SR    R4,R4                                                            
COED1640 EQU   *                                                                
         IC    R4,1(R3)            NEXT ELEM                                    
         AR    R3,R4                                                            
         CLI   0(R3),0             LAST?                                        
         BE    COED1680                                                         
         CLC   DUB(6),0(R3)                                                     
         BH    COED1640                                                         
         BNE   *+6                                                              
         DC    H'0'                MISSED ELEM ALREADY IN MISSED LINE           
*                                                                               
*                                  ADD MISSED ELEM                              
COED1680 EQU   *                                                                
         BAS   RE,MGADDCDE         UPDATE VER/MOD/DATE IN TARGET BUY            
         CLI   DUB,7               CREDIT ELEMENT?                              
         BE    COED1700            YES - DON'T SPLIT IT OUT                     
**********************************************************************          
*                                                                               
*   FOLLOWING THREE INSTRUCTIONS SKIP SPLIT-OUTS FOR ALTERNATING                
*        WEEKS.  IN EVENT OF DIFFICULTIES                                       
*                                                                               
****     L     RF,A03ELT           CHECK FOR ALTERNATING WEEKS                  
****     TM    RBUYDTIN-RBUYDTEL(RF),X'40'                                      
*                                  ALTERNATING WEEK ELEMENT?                    
****     BO    COED1700            YES - DON'T SPLIT IT...                      
*                                                                               
**********************************************************************          
         GOTO1 =A(SPLIT03S),DMCB,(RC),RR=Y                                      
         MVI   DUB,X'56'           CHANGE THE ELEMENT CODE                      
*                                     AFTER EFF DATE ELT SPLITOUT               
COED1700 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,IOAREA),(0,DUB)                 
*                                  SAVE NEW MISSED LINE                         
*        LA    R3,RBUYREC                                                       
*        LA    R3,2000(R3)                                                      
         L     R3,AIO3                                                          
         GOTO1 VMOVEREC,(R1),IOAREA,(R3)                                        
         MVC   RBUYKPLN,KEY+22     MOVE MISSED PLAN CODE                        
         MVC   RBUYKMLN,WORK+2     MISSED LINE NUMBER                           
*                                  ADD MAKE-GOOD ELEMENT                        
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK                                       
*                                                                               
         MVI   MG2NDATE,C'Y'       SET FIRST-TIME SWITCH OFF                    
         OC    SAVMGADR,SAVMGADR   ANOTHER DATE IN MG= STRING?                  
         BNZ   COED0480            YES - GO BACK FOR IT                         
*                                  EDIT COMMENT 2                               
COED1720 EQU   *                                                                
         LA    R2,BUYCOM2H                                                      
         LA    R3,MGLERR                                                        
         CLC   BUYCOM2(3),=C'MG='                                               
         BE    ERROR                                                            
         CLC   BUYCOM2(3),=C'CR='                                               
         BE    ERROR                                                            
         CLI   5(R2),0             ENTRY?                                       
         BE    ORDCOM                                                           
         MVC   WORK2+2(L'BUYCOM2),8(R2)                                         
         IC    RE,5(R2)            LEN                                          
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          LEN                                          
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
*                                  ADD COMMENT 2                                
*                                                                               
*  NOTE: A NEW BUY ORDER COMMENT IS REQUIRED UNLESS THE CONTRACT HAS            
*        NEVER BEEN SENT (VERSION 1) OR UNLESS THE ONLY CHANGE MADE             
*        WAS A CHANGE TO PROGRAM TYPE (STAT2 NOT X'80')                         
*                                                                               
*  ****  SKIP BUY ORDER COMMENT VALIDATION IF ACTION IS MAKEGOOD APPLY          
*        SINCE NO ORDER COMMENTS WILL BE PASSED                                 
*                                                                               
*        A BUY LINE CAN BE DESIGNATED 'DON'T SEND' IF                           
*         A. IT'S AN ACE OR GRAPHNET CONTRACT AND                               
*         B. IT'S NOT VERSION 1 AND                                             
*         C. THE CHARACTERS IN QUOTES '#DS' ARE THE 1ST 3 CHARACTERS            
*            ON THE 1ST REP BUY ORDER COMMENT LINE AND                          
*         D. THE STATION IS DESIGNATED OK FOR 'DON'T SEND' BUYLINES             
*                                                                               
ORDCOM   DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG+1,X'80'    DID WE COME FROM RECNT34?                    
         BO    ENDCOM              SKIP VALIDATION IF YES                       
         DROP  RF                                                               
                                                                                
         TM    STAT2,X'80'         CHANGES MADE TO REQ NEW ORD CMT?             
         BZ    ORDCOMD             NO                                           
         TM    PROFILES+CNTBOCMB,CNTBOCMA  PROF#35?                             
         BZ    ORDCOM2             NO - ORD CMT REQ                             
*                                                                               
         XC    RSTAREC(32),RSTAREC CHECK STAREAC FOR OVERRIDE                   
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   ORDCOMD             NO ELEM - OCM NOT REQ                        
         TM    RSTAOPTA,X'08'      STA OPT #14?                                 
         BO    ORDCOM2             YES OCM REQUIRED                             
         DROP  R6                                                               
*                                                                               
ORDCOMD  DS    0H                                                               
         LA    R2,BUYORDCH         NO                                           
         TM    4(R2),X'20'         HAVE ORD CMTS CHANGED                        
         BZ    ORDCOM1                                                          
         LA    R2,BUYORD2H                                                      
         TM    4(R2),X'20'                                                      
         BO    ENDCOM                                                           
*                                                                               
ORDCOM1  OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
ORDCOM2  LA    R2,BUYORDCH         BUY ORDER COMMENT                            
         LA    R4,2                BCT THROUGH 2 COMMENT LINES                  
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ORDCOM7                                                          
         CLI   5(R2),0             NO INPUT ON 1ST LINE                         
         BNE   ORDCOM3A                                                         
*                                                                               
         LA    R2,BUYORD2H         LOOK AT 2ND LINE                             
         LA    R4,1                                                             
         CLI   5(R2),0                                                          
         BNE   ORDCOM3                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ORDCOM2A                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
ORDCOM2A OC    RCONSRDT,RCONSRDT   HAS CONTRACT EVER BEEN SENT?                 
         BNZ   ORDCOM2C            YES                                          
         TM    RCONSENF,X'01'      TAKEOVER CONTRACT?                           
         BO    ORDCOM2C                                                         
         DROP  R6                                                               
                                                                                
         GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)                                    
*                                  DELETE ANY ORDCOMS PRESENT                   
         B     ENDCOM                                                           
ORDCOM2C EQU   *                                                                
         TM    PROFILES+CNTBOCMB,CNTBOCMA  PROF#35?                             
         BZ    ORDCOM2D            NO - ORD CMT REQ                             
*                                                                               
         XC    RSTAREC(32),RSTAREC CHECK STAREAC FOR OVERRIDE                   
         MVI   RSTAREC,2           INSERT TYPE                                  
         MVC   RSTAKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA   INSERT STATION                               
         MVC   KEY,RSTAKEY         LOAD KEY FOR READ                            
         GOTO1 VHIGH               ACCESS KEY                                   
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NOT FOUND? UNLIKELY.                         
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   ENDCOM              NO ELEM - OCM NOT REQ                        
         TM    RSTAOPTA,X'08'      STA OPT #14?                                 
         BZ    ENDCOM              NO OCM REQUIRED                              
         DROP  R6                                                               
ORDCOM2D DS    0H                                                               
         LA    R3,1                MISSING INP FLD                              
         LA    R2,BUYORDCH                                                      
         B     ERROR                                                            
*                                                                               
ORDCOM3  EQU   *                                                                
         LA    R2,BUYORDCH         RESET A(1ST LINE INPUT)                      
ORDCOM3A TM    4(R2),X'20'         1ST LINE INPUT THIS TIME?                    
         BZ    ORDCOM7             YES                                          
         LA    R2,BUYORD2H                                                      
         TM    4(R2),X'20'         2ND LINE INPUT THIS TIME?                    
         BO    ORDCOM5                                                          
         CLI   5(R2),0             IS THERE 2ND LINE?                           
         BE    ORDCOM5                                                          
         LA    R2,BUYORDCH       ONE INPUT THIS TIME, REDO BOTH LINES           
         B     ORDCOM7                                                          
         SPACE 1                                                                
ORDCOM5  LA    R3,129            NEW COMMENT REQUIRED                           
         LA    R2,BUYORDCH                                                      
         B     ERROR                                                            
         SPACE 1                                                                
ORDCOM7  CLC   8(3,R2),=C'#DS'     'DON'T SEND' LINE NOT ALLOWED ON             
         BNE   ORDCOM7K                                                         
         TM    RCONMODR+1,X'C0'          NON ACE OR GRAPHNET                    
         BZ    ORDCOM8                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONSTAT,X'02'           OR IF STATION NOT TURNED ON             
         BZ    ORDCOM8                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ORDCOM7A                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
ORDCOM7A OC    RCONSRDT,RCONSRDT         OR VERSION 1                           
         BZ    ORDCOM8K                                                         
         SPACE 1                                                                
         CLI   5(R2),3             AND MUST INPUT MORE THAN JUST #DS            
         BH    ORDCOM7K                                                         
         LA    R3,135              COMMENT REQUIRED FOR #DS                     
         B     ERROR                                                            
         SPACE 1                                                                
ORDCOM7K LA    R2,BUYORD2H                                                      
         CLC   8(3,R2),=C'#DS'     AND NOT ALLOWED ON LINE 2                    
         BE    ORDCOM8P                                                         
         LA    R2,BUYORDCH                                                      
         B     ORDCOM9                                                          
         DROP  R6                                                               
ORDCOM8  LA    R3,133              NOT VALID FOR CONTRACT                       
         B     ERROR                                                            
ORDCOM8K LA    R3,134              NOT VALID FOR VERSION 1                      
         B     ERROR                                                            
ORDCOM8P LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
         SPACE 1                                                                
ORDCOM9  GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)    DELETE OLD                      
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BO    ORDCOM11                                                         
         CLI   5(R2),0             NON ACE/GRAPH DOESN'T REQUIRE                
         BNE   ORDCOM11              COMMENT                                    
         LA    R2,BUYORD2H         2ND LINE                                     
         LA    R4,1                                                             
         CLI   5(R2),0             IS THERE 2ND LINE?                           
         BE    ENDCOM                                                           
         SPACE 1                                                                
ORDCOM11 XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         MVI   WORK2,X'84'                                                      
         OI    WORK2+2,X'80'       REP COMMENT                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+3(0),8(R2)                                                 
         LA    RE,4(RE)            GET TOTAL LENGTH (1 FROM BCTR + 2)           
         STC   RE,WORK2+1                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         SPACE 1                                                                
         LA    R2,BUYORD2H         2ND COMMENT LINE IS ALWAYS OPTIONAL          
         CLI   5(R2),0                                                          
         BE    ENDCOM                                                           
         BCT   R4,ORDCOM11                                                      
         SPACE 1                                                                
ENDCOM   DS    0H                                                               
*                                                                               
* EDIT PGM (PROGRAM) FIELD                                                      
*                                                                               
         LA    R2,BUYPGMH                                                       
         TM    4(R2),X'20'                                                      
         BO    ENDPGM              UNCHANGED                                    
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VDELELEM,DMCB,(X'21',RBUYREC)                                    
         CLI   5(R2),0                                                          
         BE    ENDPGM                                                           
         XC    WORK2(255),WORK2                                                 
         MVI   WORK2,X'21'         ELEM CODE                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK2+2(0),8(R2)                                                 
         AHI   R1,3                                                             
         STC   R1,WORK2+1                                                       
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
ENDPGM   DS    0H                                                               
*                                                                               
         TITLE 'T80216 REPPAK EDIT END'                                         
ENDEDIT  DS    0H                                                               
         OI    CONBACTH+4,X'20'                                                 
         OI    CONBNUMH+4,X'20'                                                 
         LA    R2,CONBACTH         CURSOR                                       
* PUT RBUYNW IN 03 ELEMENTS (EFF DATES) IF NO OVERRIDE                          
*     AND NOT 'DARE' ORDER (EFF DATES CAN'T BE CHANGED FOR THEM)                
         MVI   ELCODE,X'1D'        DARE ELEMENT?                                
         BAS   RE,GETEL                                                         
         BNE   END080              NO  - PROCEED                                
         LA    R6,RCONREC          YES - CHECK FOR 'CONFIRMED'                  
         MVI   ELCODE,X'1F'        EXTENDED DESCRIPTION ELEMENT?                
         BAS   RE,GETEL                                                         
         BNE   END200              NOT FOUND - NEW DARE ORDER.                  
         TM    RCONCONF-RCONXEL(R6),X'E0'                                       
*                                  ANY 'CONFIRMED' FLAG ON?                     
         BZ    END200              NO  - TREAT AS DARE ORDER                    
*                                  YES - UPDATE # SPOTS/WEEK                    
*                                                                               
*   NOTE - THIS TEST MAY HAVE TO BE CHANGED FOR ORDERS WHICH ARE                
*        CONFIRMED/UNCONFIRMED FOR DARE.                                        
*                                                                               
***********************************************************************         
* SPECIAL FIX FOR XML DARE ORDERS...REMOVE AFTER 1/1/2010                       
* FIX 0 SPOT WEEKS AT FLIGHT BEGINNING                                          
***********************************************************************         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        DARE ELEMENT?                                
         BAS   RE,GETEL                                                         
         BNE   END080              YES - PROCEED                                
         TM    RCONDRF2-RCONDREL(R6),X'01'                                      
         BZ    END080              XML?                                         
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   END200                                                           
*                                                                               
END050   DS    0H                                                               
         TM    8(R6),X'01'                                                      
         BO    END080                                                           
         CLI   9(R6),0                                                          
         BNE   END080                                                           
         OI    8(R6),X'01'         SET OVERRIDE FLAG                            
         BAS   RE,NEXTEL                                                        
         BE    END050                                                           
***********************************************************************         
* END SPECIAL FIX FOR XML DARE ORDERS...REMOVE AFTER 1/1/2010                   
***********************************************************************         
*                                                                               
END080   EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   END200                                                           
END100   DS    0H                                                               
         TM    8(R6),X'01'                                                      
         BO    *+10                                                             
         MVC   9(1,R6),RBUYNW                                                   
         BAS   RE,NEXTEL                                                        
         BE    END100                                                           
END200   CLC   BUYACT(3),=C'BUY'                                                
         BE    ADDBUY                                                           
         CLC   BUYACT(3),=C'BUX'   SPECIAL NO BUCKET ACTION                     
         BE    ADDBUY                                                           
*                                                                               
*                                  MISSED ELTS NAKED BY CHANGE?                 
         GOTO1 VMOVEREC,DMCB,RBUYREC,IOAREA                                     
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'        MISSED SPOT ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    MDAY0040                                                         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'07'        CREDIT ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   MDAY0160            NOT FOUND - CHECK X'66' ELTS                 
*                                  STILL COVERED BY DAYS/DATES?                 
*                                                                               
MDAY0040 MVC   WORK+3(3),2(R6)     MISSED DATE                                  
         BAS   RE,CKMISS           CHECK IF EXPOSED                             
         L     R3,DMCB             NUMBER OF SPOTS IN WEEK                      
         LTR   R3,R3                                                            
         BM    MDAY0080                                                         
         CLI   TEMP,X'FF'                                                       
         BNE   MDAY0120                                                         
MDAY0080 LA    R3,MG1ERR           MUST DELETE MAKE-GOODS                       
         B     ERROR                                                            
*                                                                               
MDAY0120 LR    R7,R6                                                            
         ZIC   R4,1(R6)                                                         
         AR    R6,R4               NEXT ELEM                                    
         CLI   0(R6),6             MISSED?                                      
         BE    *+12                                                             
         CLI   0(R6),7                                                          
         BNE   MDAY0160                                                         
         CLC   2(3,R6),2(R7)                                                    
         BE    MDAY0120            IF SAME MISSED ELEM DATE - SKIP              
         B     MDAY0040                                                         
MDAY0160 EQU   *                                                                
         MVI   HALF2,X'56'           SET MG SPLIT ELEMENT CODE                  
MDAY0170 EQU   *                                                                
         LA    R6,IOAREA                                                        
         MVC   ELCODE(1),HALF2     MAKEGOOD OFFER OR SPLIT ELT                  
         BAS   RE,GETEL                                                         
         BNE   MDAY0320            NOT FOUND - FINISHED                         
*                                  STILL COVERED BY DAYS/DATES?                 
MDAY0200 DS    0H                                                               
         MVC   WORK+3(3),2(R6)     MISSED DATE                                  
         CLI   0(R6),X'66'                                                      
         BNE   MDAY0210                                                         
         MVC   WORK+3(3),7(R6)                                                  
*                                  STILL COVERED BY DAYS/DATES?                 
MDAY0210 DS    0H                                                               
         BAS   RE,CKMISS           CHECK IF EXPOSED                             
         L     R3,DMCB             NUMBER OF SPOTS IN WEEK                      
         LTR   R3,R3                                                            
         BM    MDAY0240            MAY NOT LEAVE MINUS NUMBER                   
         BZ    MDAY0280            ZERO SPOTS PERMITTED:                        
*                                     SPLIT MAY HAVE REDUCED NUMBER TO          
*                                     ZERO LEGALLY                              
         CLI   TEMP,X'FF'                                                       
         BNE   MDAY0280                                                         
MDAY0240 EQU   *                                                                
         LA    R3,MG1ERR                                                        
         CLI   HALF2,X'56'         MG SPLIT IN PROGRESS?                        
         BE    ERROR               YES - USE MAKEGOOD ERROR                     
         LA    R3,MGOERR           NO  - MUST DELETE MAKE-GOOD OFFERS           
         B     ERROR                                                            
*                                                                               
MGOERR   EQU   458                                                              
*                                                                               
MDAY0280 LR    R7,R6                                                            
         ZIC   R4,1(R6)                                                         
         AR    R6,R4               NEXT ELEM                                    
         CLC   0(1,R6),HALF2       MAKEGOOD OFFER OR SPLIT ELT?                 
         BNE   MDAY0320                                                         
         CLI   0(R6),X'66'                                                      
         BE    MDAY0290                                                         
         CLC   2(3,R6),2(R7)       FOR ELEMENT X'56'                            
         BE    MDAY0280            IF SAME MISSED ELEM DATE - SKIP              
         B     MDAY0200                                                         
*                                                                               
MDAY0290 DS    0H                                                               
         CLC   7(3,R6),7(R7)       FOR ELEMENT X'66'                            
         BE    MDAY0280            IF SAME MISSED ELEM DATE - SKIP              
         B     MDAY0200                                                         
*                                  ALL MISSED ELTS COVERED                      
MDAY0320 EQU   *                                                                
         CLI   HALF2,X'66'         MG OFFER FINISHED?                           
         BE    MDAY0340            YES - FINISHED                               
         MVI   HALF2,X'66'         NO  - GO BACK AND DO IT                      
         B     MDAY0170                                                         
MDAY0340 EQU   *                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET-DON'T CARE IF                   
         BNZ   MDAY0360            BUY CREATED TODAY                            
         SPACE 1                                                                
         CLC   RBUYCREA,TODAY      BUY CREATED TODAY?                           
         BE    END300                                                           
*                                  ANALYZE CHANGE                               
MDAY0360 DS    0H                                                               
*        LA    R6,RBUYREC                                                       
*        LA    R6,1000(R6)                                                      
         L     R6,AIO2                                                          
*                                  SET PARAMETER LIST TO VCHECKEL               
         L     RF,VCHECKEL         ELEMENT COMPARE ROUTINE IN BASE              
         GOTO1 (RF),DMCB,RBUYREC,(R6),BYTE                                      
*                                  CHECK DAY                                    
         MVI   DMCB,2              DAY ELEM CODE                                
         MVI   BYTE,C'D'           DAY CHG CODE                                 
         BASR  RE,RF               CHECK CHANGE                                 
         BAS   RE,ADDCODE                                                       
*                                  CHECK TIME                                   
         MVI   BYTE,C'T'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK EFF. DATES                             
         MVI   DMCB,3                                                           
         MVI   BYTE,C'E'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK COMMENTS                               
         MVI   DMCB,4                                                           
         MVI   BYTE,C'Z'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
         CLI   BYTE,0              CHANGE?                                      
         BNE   MDAY0380            YES - NO NEED TO CHECK PROGRAM               
*                                  CHECK PROGRAM CHANGE                         
         MVI   DMCB,X'21'                                                       
         MVI   BYTE,C'Z'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK MAKE-GOOD CHANGE                       
MDAY0380 DS    0H                                                               
         MVI   DMCB,5                                                           
         MVI   BYTE,C'M'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK ORDER COMMENT CHANGE                   
         MVI   DMCB,X'84'                                                       
         MVI   BYTE,C'O'                                                        
         BASR  RE,RF                                                            
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK LENGTH                                 
         LA    R5,RBUYDUR                                                       
         L     R6,AIO2                                                          
         LA    R6,RBUYDUR-RBUYREC(R6)                                           
         MVI   BYTE,C'L'                                                        
         CLC   0(2,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK SPOTS/PER/WEEK                         
         LA    R5,RBUYNW                                                        
         L     R6,AIO2                                                          
         LA    R6,RBUYNW-RBUYREC(R6)                                            
         MVI   BYTE,C'S'                                                        
         CLC   0(1,R5),0(R6)                                                    
         BE    MDAY0520                                                         
         BAS   RE,ADDCODE                                                       
*                                  CHANGE NPW IN 03 DATE ELEMENTS               
*                                     IF = TO OLD NPW                           
         LA    R3,RBUYELEM                                                      
         SR    R4,R4                                                            
MDAY0400 CLI   0(R3),0                                                          
         BE    MDAY0520                                                         
         CLI   0(R3),3                                                          
         BE    MDAY0480                                                         
*                                                                               
MDAY0440 IC    R4,1(R3)            NEXT ELEM                                    
         AR    R3,R4                                                            
         B     MDAY0400                                                         
*                                                                               
MDAY0480 CLC   9(1,R3),0(R6)       SAME AS OLD NPW?                             
         BNE   MDAY0440            IF SO NO CHANGE                              
         MVC   9(1,R3),RBUYNW                                                   
         B     MDAY0440                                                         
*                                                                               
*                                  CHECK RATE                                   
MDAY0520 LA    R5,RBUYCOS                                                       
         L     R6,AIO2                                                          
         LA    R6,RBUYCOS-RBUYREC(R6)                                           
         MVI   BYTE,C'R'                                                        
         CLC   0(4,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
*                                  CHECK PLAN, CLASS OR SECTION                 
         MVI   BYTE,C'P'                                                        
         LA    R5,RBUYKPLN         PLAN                                         
         L     R6,AIO2                                                          
         LA    R6,RBUYKPLN-RBUYREC(R6)                                          
         CLC   0(3,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
         LA    R5,RBUYSEC          SECTION                                      
         L     R6,AIO2                                                          
         LA    R6,RBUYSEC-RBUYREC(R6)                                           
         CLC   0(3,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
         LA    R5,RBUYCLS          CLASS                                        
         L     R6,AIO2                                                          
         LA    R6,RBUYCLS-RBUYREC(R6)                                           
         CLC   0(3,R5),0(R6)                                                    
         BE    *+8                                                              
         BAS   RE,ADDCODE                                                       
*                                                                               
         B     END300                                                           
         EJECT                                                                  
*                                  ADD BUY                                      
*                                  UPDATE CHANGE INDICATORS                     
ADDBUY   MVC   RBUYCREA,TODAY      CREATION DATE                                
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET CONTRACT                        
         BNZ   *+12                                                             
*                                                                               
         CLI   RCONMOD,X'FF'       -1?                                          
         BE    ADD150                                                           
         CLI   RBUYKLIN,1          LINE 1?                                      
         BNE   ADD150                                                           
         CLC   RCONCREA,TODAY                                                   
         BE    *+14                                                             
         OI    TAREQ,1             T/A REQ IND                                  
         MVC   RCONCREA,TODAY      LINE 1 SHOULD NOT BUMP K MOD                 
*                                                                               
         OI    RCONMODR,X'20'+X'10' X'20'=LINE 1 ADDED                          
*                                   X'10'=NOT PENDING/BUYLINE(S) WERE           
*                                         ADDED AT ONE POINT.  THIS BIT         
*                                         DOES NOT GET RESET.                   
         B     *+8                                                              
ADD150   BAS   RE,BUMPNUM          BUMP MODIFICATION NUMBER IN K                
         MVC   RBUYKMOD,RCONMOD                                                 
         TM    RCONMODR+1,X'C0'    IF ACE/GRAPHNET SKIP THIS CODE               
         BNZ   ADD160              (X'FF' IS VALID MOD #)                       
         SPACE 1                                                                
         CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RBUYKMOD,0                                                       
ADD160   MVC   RBUYCHGI,=C'A '     ADD IND                                      
*                                                                               
         LR    RF,RA               SPECIAL IF COMBO K                           
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          WE WANT TO ADD THE EXTRA DESCRIPTION         
         BE    ADD170              ONLY ONCE                                    
         CLI   TWACMBPT,1                                                       
         BH    ADD180                                                           
         DROP  RF                                                               
*                                                                               
ADD170   DS    0H                                                               
*                                  ADD EXTRA DESCRIPTION ELEMENT X'10'          
         GOTO1 =A(ADDEXTRA),DMCB,(RC),RR=Y                                      
*                                                                               
ADD180   DS    0H                                                               
*                                                                               
         CLI   RCONKSTA+4,C'F'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
         CLI   RCONKSTA+4,C'A'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
         CLI   RCONKSTA+4,C'C'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
*                                                                               
         GOTO1 =A(SARCOMS),RR=Y                                                 
         B     END300                                                           
         SPACE 4                                                                
* ROUTINE TO BUMP CONTRACT MODIFICATION MUMBER                                  
         SPACE 1                                                                
BUMPNUM  OI    RCONMODR,X'40'      BUY CHANGE IND                               
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BCR   8,RE                                                             
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BNZ   BUMP5                                                            
*                                                                               
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BCR   8,RE                                                             
* UPDATE MODIFICATION NUMBER                                                    
BUMP5    OI    TAREQ,1             T/A REQ IND                                  
         NI    RCONMODR,X'5F'      NO K HEADLINE CHANGE                         
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET- DON'T BUMP MOD NUMBER          
         BNZ   BUMP10                                                           
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,RCONMOD          MOD NUMBER                                   
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
BUMP10   MVC   RCONMODD,TODAY                                                   
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*  UPDATE REP VERSION NUMBER                                                    
*                                                                               
END300   DS    0H                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    END500                                                           
         TM    STAT2,X'80'         BUY CHANGED-UP VER/MOD                       
         BO    END325                                                           
         OI    BYTE4,X'02'         NO, BUT STILL DO PUTREC                      
         B     END500                                                           
         SPACE 1                                                                
END325   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    END330                                                           
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
END330   DS    0H                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    END350                                                           
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATE                                   
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    END340                                                           
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
END340   DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC)                                     
         BNZ   ERROR                                                            
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
                                                                                
END350   DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSRV         STORE REP VERSION NO. IN BUY                  
         DROP  R6                                                               
         STC   R1,RBUYVER          INSERT REP VERSION #                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    END400                                                           
         SPACE 1                                                                
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
END400   OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         SPACE 1                                                                
END500   B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD CODE TO RBUYCHGI  (CODE IN BYTE)                               
***********************************************************************         
ADDCODE  NTR1                                                                   
         CLI   BYTE,0                                                           
         BE    ADDCXIT                                                          
         BAS   RE,BUMPNUM          BUMP K NUMBER                                
         MVC   RBUYKMOD,RCONMOD    K MODIFICATION NUMBER                        
         SPACE 1                                                                
*                                                                               
* FOR ACE/GRAPHNET -  THE VERSION, NOT THE DAY, IS WHAT MATTERS                 
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ADDC50                                                           
         CLI   TWAACCS,C'$'        STATION ONLY HAS 1 CHG CODE, SO IT           
         BE    ADDC75              DOESN'T MATTER IF UPDATED ALREADY            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ADDC10                                                           
         DC    H'0'                                                             
ADDC10   TM    4(R6),X'20'         REP VERSION NOT ADVANCED                     
         BNO   ADDC30                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
         SPACE 1                                                                
ADDC30   CLC   5(1,R6),RBUYVER     COMPARE CONTRACT TO BUY VERSION              
         BE    ADDC35                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
ADDC35   CLI   RBUYCHGI,C'A'       IF ADDED THIS VERSION,                       
         BE    ADDCXIT             DON'T CARE ABOUT OTHER CHANGES               
         B     ADDC100                                                          
         SPACE 1                                                                
*  DO SOME TESTS FOR NON ACE/GRAPHNET CONTRACTS                                 
         SPACE 1                                                                
ADDC50   CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RBUYKMOD,0                                                       
         CLC   TODAY,RBUYCHGD                                                   
         BE    ADDC100                                                          
ADDC75   MVC   RBUYCHGD,TODAY                                                   
         MVC   RBUYCHGI(1),BYTE    CHANGE CODE                                  
         MVI   RBUYCHGI+1,C' '                                                  
*                                                                               
ADDCXIT  XIT1                                                                   
* BUY ALREADY CHANGED TODAY                                                     
ADDC100  CLI   RBUYCHGI,C' '                                                    
         BNE   *+14                                                             
         MVC   RBUYCHGI(1),BYTE    FIRST                                        
         B     ADDCXIT                                                          
         CLI   RBUYCHGI,C'*'                                                    
         BE    ADDCXIT                                                          
         CLC   RBUYCHGI(1),BYTE                                                 
         BE    ADDCXIT                                                          
         CLI   RBUYCHGI+1,C' '                                                  
         BNE   *+14                                                             
         MVC   RBUYCHGI+1(1),BYTE  2D                                           
         B     ADDCXIT                                                          
         CLC   RBUYCHGI+1(1),BYTE                                               
         BE    ADDCXIT                                                          
         MVC   RBUYCHGI,=C'* '     MORE THAN 2                                  
         B     ADDCXIT                                                          
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE UPDATES THE MOD CODE IN THE TARGET MISSED LINE FOR A             
* MAKEGOOD. THE TARGET MISSED BUY IS IN 'IOAREA'. ROUTINE IS LIFTED             
* FROM ADDCODE.                                                                 
**********************************************************************          
MGADDCDE NTR1                                                                   
IOA      USING RBUYREC,IOAREA                                                   
         MVC   IOA.RBUYKMOD,RCONMOD K MOD NUM                                   
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    MGADDC20                                                         
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* TAKE THE LATEST/HIGHEST VERSION NUMBER                                        
         TM    RCONSENF,X'20'                                                   
         BZ    MGADDC10                                                         
*                                                                               
         ZIC   R1,RCONSRV          NEXT REP VERSION NUMBER                      
         LA    R1,2(R1)            SHOULD BE THE GREATER OF                     
         STC   R1,IOA.RBUYVER      REP VERSION NUMBER + 2                       
         CLC   IOA.RBUYVER,RCONSSV                                              
         BH    MGADDC30            OR                                           
         ZIC   R1,RCONSSV                                                       
         LA    R1,1(R1)            STA VERSION NUMBER + 1                       
         STC   R1,IOA.RBUYVER                                                   
         B     MGADDC30                                                         
                                                                                
MGADDC10 DS    0H                  REP VERSION ADVANCED                         
         CLC   RCONSRV,IOA.RBUYVER                                              
         BE    MGADDC40                                                         
         MVC   IOA.RBUYVER,RCONSRV                                              
         B     MGADDC30                                                         
         DROP  R6                                                               
*                                                                               
*  DO SOME TESTS FOR NON ACE/GRAPHNET CONTRACTS                                 
*                                                                               
MGADDC20 DS    0H                                                               
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BE    MGADDC30                                                         
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BE    MGADDC30                                                         
* UPDATE MODIFICATION NUMBER IN TARGET MISSED LINE FOR NON-ACE/GRAPH            
* CONTRACT MOD NUMBER WILL BE UPDATED LATER BY ADDCODE                          
         ZIC   R1,IOA.RBUYKMOD                                                  
         LA    R1,1(R1)                                                         
         STC   R1,IOA.RBUYKMOD                                                  
         CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   IOA.RBUYKMOD,0                                                   
         CLC   TODAY,IOA.RBUYCHGD                                               
         BE    MGADDC40                                                         
                                                                                
MGADDC30 MVC   IOA.RBUYCHGD,TODAY                                               
         MVC   IOA.RBUYCHGI,=C'E ' MAKEGOOD CHANGE CODE                         
         B     MGADDCX                                                          
*                                                                               
* BUY ALREADY CHANGED TODAY                                                     
MGADDC40 CLI   IOA.RBUYCHGI,C' '                                                
         BNE   MGADDC50                                                         
         MVI   IOA.RBUYCHGI,C'E'                                                
         B     MGADDCX                                                          
                                                                                
MGADDC50 DS    0H                                                               
         CLI   IOA.RBUYCHGI,C'*'                                                
         BE    MGADDCX                                                          
         CLI   IOA.RBUYCHGI,C'E'                                                
         BE    MGADDCX                                                          
         CLI   IOA.RBUYCHGI+1,C' '                                              
         BNE   MGADDC60                                                         
         MVI   IOA.RBUYCHGI+1,C'E'                                              
         B     MGADDCX                                                          
                                                                                
MGADDC60 DS    0H                                                               
         CLI   IOA.RBUYCHGI+1,C'E'                                              
         BE    MGADDCX                                                          
         MVC   IOA.RBUYCHGI,=C'* ' MORE THAN 2                                  
*                                                                               
MGADDCX  XIT1                                                                   
         DROP  IOA                                                              
         TITLE 'SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS'                
*        P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                        
*        P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR. FOUND            
*        AN ASTERISK DELIMITS SUB-FIELDS                                        
SCAN     NTR1                                                                   
*                                                                               
         L     R2,4(R1)       FIELD HEADER                                      
         L     R3,0(R1)       LAST FIELD                                        
*                                                                               
         ZIC   R4,0(R1)       LENGTH OF PREVIOUS FIELD                          
         LA    R3,1(R4,R3)    NEW SCAN PLUS DELIMITER                           
         ST    R3,0(R1)       LAST FIELD                                        
         SR    R5,R5          LENGTH COUNTER                                    
         IC    R4,5(R2)       TOTAL LENGTH OF INPUT                             
         LA    R2,8(R4,R2)    TOTAL FIELD END                                   
         MVI   4(R1),0        ELIM. STOP INDICATOR                              
FIELD25  CR    R3,R2          END OF INPUT?                                     
         BL    FIELD100                                                         
FIELD50  STC   R5,0(R1)       NEW FIELD LENGTH                                  
         XIT1                                                                   
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   *+12                                                             
         MVI   4(R1),C'*'                                                       
         B     FIELD50                                                          
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
         EJECT                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
*                                                                               
       ++INCLUDE RECNTFED                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
*********************************************************************           
* SARCOMS - CHANGES PENDING COMMENTS TO SPL COMMENTS                *           
*********************************************************************           
T80215   CSECT                                                                  
         DS    0H                                                               
SARCOMS  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*SARCOMS'                                                    
*                                                                               
SARCM2   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   SARCMX                                                           
*                                                                               
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R6)                                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),(R6)                                  
*                                                                               
         LA    R6,RCONELEM                                                      
SARCM4   CLI   0(R6),0                                                          
         BE    SARCM6                                                           
         CLI   0(R6),X'07'                                                      
         BH    SARCM6                                                           
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     SARCM4                                                           
*                                                                               
SARCM6   MVI   WORK2,X'07'                                                      
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),WORK2,(R6)                            
         B     SARCM2                                                           
*                                                                               
SARCMX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         CSECT                                                                  
********************************************************************            
* CHECK IF CONTRACT IS A TAKEOVER CONTRACT                                      
********************************************************************            
ISTAKOV  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK2,WORK2                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         USING RCONCMEL,R6                                                      
         ZIC   R1,RCONCMLN                                                      
         SH    R1,=H'3'                                                         
         BM    TKOVNO                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONCMNT                                                
         DROP  R6                                                               
*                                                                               
         CLC   =C'C=TO',WORK2      COMMET MUST BE C=TOXXXX-X                    
         BNE   TKOVNO              WHERE XXXX-X IS THE STATION                  
         MVC   WORK(6),WORK2+4                                                  
                                                                                
         CLC   =C'-TV',CONSTA+4                                                 
         BE    TKOV20                                                           
         CLC   =C'-TV',CONSTA+3    CHECK INCASE OF 3 LETTER CALLS               
         BE    TKOV30                                                           
         CLC   =C'-C',WORK2+7      CHECK IF COMBO                               
         BE    TKOV50                                                           
         CLC   =C'-C',WORK2+8                                                   
         BE    TKOV50                                                           
                                                                                
* COMPARE FOR RADIO                                                             
         CLI   WORK+5,C' '         CHECK INCASE OF 3 LETTER CALLS               
         BNE   TKOV10                                                           
         MVI   WORK+5,C'M'                                                      
                                                                                
TKOV10   DS    0H                                                               
         CLC   WORK(6),CONSTA                                                   
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* COMPARE FOR TV                                                                
TKOV20   DS    0H                  TV CAN BE SPECIFIED AS                       
         CLC   WORK(4),CONSTA      XXXX OR XXXX-T                               
         BNE   TKOVNO                                                           
         CLC   WORK+4(2),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+4                                                    
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* AND 3 LETTER CALLS                                                            
TKOV30   DS    0H                                                               
         CLC   WORK(3),CONSTA                                                   
         BNE   TKOVNO                                                           
         CLC   WORK+3(3),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+3                                                    
         BE    TKOVYES                                                          
         B     TKOVNO                                                           
                                                                                
* CHECK FOR COMBO TAKEOVER                                                      
TKOV50   DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
                                                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TKOVNO                                                           
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         CLI   WORK+3,C'-'         TAKEOVER COMMENT STATION MUST MATCH          
         BE    TKOV60              PARENT STATION                               
         CLC   RSTACS(4),WORK                                                   
         BNE   TKOVNO                                                           
                                                                                
TKOV60   DS    0H                  INCASE PARENT'S CALL IS 3 LETTERS            
         CLC   RSTACS(3),WORK                                                   
         BE    TKOVYES                                                          
         DROP  R6                                                               
                                                                                
TKOVNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
TKOVYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* SET TRADE ORDER FLAG                                                          
***********************************************************************         
SETRADE  NMOD1 0,*SETR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         USING RCONRFEL,R6                                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OI    RCONRF1,X'08'       SET TRADE CONTRACT                           
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE BUY                                                                    
***********************************************************************         
DELBUY   NMOD1 0,*DELB*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    DELB0040                                                         
*                                                                               
*  IF ACE/GRAPHNET, CAN ONLY DELETE IF BUYLINE HAS NEVER BEEN SENT, AND         
*  CAN ONLY CANCEL IF BUYLINE HAS PREVIOUSLY BEEN SENT                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            GET SEND ELEMENT                             
         BE    DELB0020                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
DELB0020 MVC   SVKVER,RCONSRV      SAVE K REP VER                               
         MVC   SVKSENF,RCONSENF     SAVE SEND INFO                              
         DROP  R6                                                               
*                                                                               
DELB0040 LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWABADDR                                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    DELB0120                                                         
*                                                                               
* ACTION CANCEL                                                                 
*                                                                               
         CLC   BUYACT,=C'CAN'                                                   
         BNE   DELB0080                                                         
         LA    R3,198              BUYLINE HAS BEEN SENT                        
         CLI   RBUYCHGI,C'A'       OK TO CANCEL IF MOD CODE <> 'A'              
         BNE   DELB0120                                                         
         CLC   RBUYVER,SVKVER      IF MOD CODE='A', CHECK BUY VERSION           
         BNE   DELB0120            AGAINST K VERSION                            
*                                  OK TO CANCEL SINCE UNEQUAL VERSIONS          
*                                  IMPLIES K WAS SENT                           
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DELB0060                                                         
         TM    SVKSENF,X'20'        SENT BY REP                                 
         BZ    ERROR                                                            
         B     DELB0120                                                         
*                                                                               
DELB0060 DS    0H                                                               
         TM    SVKSENF,X'10'                                                    
         BZ    ERROR                                                            
         B     DELB0120                                                         
*                                                                               
* ACTION DELETE                                                                 
*                                                                               
DELB0080 DS    0H                                                               
         CLC   =C'DELDDS',CONBACT                                               
         BE    DELB0180                                                         
* PROFILE ALLOWS DELETE OF ANY BUYLINE NOT TRANSFERRED TO SPOT                  
         TM    PROFILES+CNTBDELB,CNTBDELA                                       
         BNZ   DELB0120            PROF ON, SKIP TO SPOT XFER CHECK             
*                                                                               
         LA    R3,197                                                           
*        CLI   RBUYCHGI,C'A'       CHANGE MODE MUST BE 'A'                      
*        BNE   ERROR                                                            
         CLC   RBUYVER,SVKVER      BUYLINE CAN BE DELETED ONLY                  
         BL    ERROR               IF IT HAS NEVER BEEN SENT                    
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DELB0100                                                         
         TM    SVKSENF,X'20'        SENT BY REP                                 
         BO    ERROR                                                            
         B     DELB0120                                                         
*                                                                               
DELB0100 DS    0H                                                               
         TM    SVKSENF,X'10'                                                    
         BO    ERROR                                                            
*                                                                               
* MAKE SURE BUY HASN'T BEEN XFERRED TO SPOTPAK                                  
DELB0120 DS    0H                                                               
*                                                                               
* REMOVE RTS CHECK                                                              
*                                                                               
*&&DO                                                                           
         CLC   =C'CAN',BUYACT      CAN?                                         
         BE    DELB0180            OK                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    DELB0140            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    DELB0160            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
DELB0140 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   DELB0180                                                         
DELB0160 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   DELB0180                                                         
         USING RBUYSPEL,R6                                                      
         CLI   RBUYSPL#,0          HAS BEEN XFERRED?                            
         BE    DELB0180            NO, OKAY TO DEL                              
         DROP  R6                                                               
         LA    R3,269              CAN'T DEL BUY XFER'D TO SPOT                 
         B     ERROR                                                            
*&&                                                                             
*                                                                               
* CHECK FOR MISSED ELEMENTS                                                     
DELB0180 LA    R5,RBUYELEM                                                      
         SR    R4,R4                                                            
         LA    R3,MG1ERR           NO MASTER DELETE ERROR                       
DELB0200 CLI   0(R5),0             LAST?                                        
         BE    DELB0280                                                         
         CLI   0(R5),5             MAKE-GOOD?                                   
         BE    ERROR                                                            
         CLI   0(R5),6             MISSED?                                      
         BE    ERROR                                                            
         CLI   0(R5),7             MISSED CREDIT ISSUED                         
         BE    ERROR                                                            
         CLI   0(R5),X'56'         MISSED (NEW SPLIT 03 VERSION)?               
         BNE   DELB0220                                                         
         EDIT  (1,5(R5)),(3,WORK2),ALIGN=LEFT                                   
         LA    R3,652              MUST DELETE MAKEGOOD BUYLINE FIRST           
         B     DELB0240                                                         
*                                                                               
DELB0220 EQU   *                                                                
         CLI   0(R5),X'66'         MAKEGOOD OFFER ELEMENT?                      
         BNE   DELB0260                                                         
         MVC   WORK2(2),2(R5)                                                   
         LA    R0,2                                                             
         LA    R3,653              MUST DELETE MAKEGOOD OFFER FIRST             
*                                                                               
DELB0240 EQU   *                                                                
         LA    RF,WORK2            BUFFER FOR VERSION NUMBER                    
         ST    RF,DMCB+12                                                       
         STC   R0,DMCB+12          INDICATE SIZE OF VERSION NUMBER              
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    CONBACTH+6,X'40'    FORCE CURSOR TO ACTION                       
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
*                                                                               
DELB0260 EQU   *                                                                
         IC    R4,1(R5)            NEXT ELEM                                    
         AR    R5,R4                                                            
         B     DELB0200                                                         
*                                                                               
DELB0280 DS    0H                                                               
         CLC   BUYACT,=C'CAN'      CANCELLED?                                   
         BNE   DELB0285            NO - DELETE IT                               
         TM    PROFILES+CNTKCANB,CNTKCANA  KEEP CANCELLED BUYS?                 
         BO    DELB0290                    YES - SKIP DELETE                    
DELB0285 DS    0H                                                               
         OI    RBUYCNTL,X'80'      DELETE BIT                                   
*                                                                               
DELB0290 DS    0H                                                               
*        LA    R6,RBUYREC                                                       
*        LA    R6,1000(R6)                                                      
         L     R6,AIO2                                                          
         GOTO1 VMOVEREC,DMCB,RBUYREC,(R6)    SAVE OLD BUYREC                    
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD                                                            
         CLC   BUYACT,=C'CAN'      CANCEL?                                      
         BNE   DELB0295            NO                                           
         TM    PROFILES+CNTKCANB,CNTKCANA  KEEP CANCELLED BUYS?                 
         BO    *+8                                                              
DELB0295 DS    0H                                                               
         OI    KEY+27,X'80'        DELETE                                       
         GOTO1 VWRITE                                                           
* UPDATE PASSIVE KEY                                                            
         GOTO1 VLOAD,DMCB,(X'19',0),RBUYREC                                     
*                                                                               
         MVC   RBUYCHGD,TODAY                                                   
         MVC   RBUYCHGI,=C'C '     CANCEL INDICATOR                             
         BAS   RE,BUMPNUM          BUMP K MOD NUMBER                            
*                                                                               
         MVC   RBUYKMOD,RCONMOD                                                 
         CLC   BUYACT,=C'CAN'                                                   
         BE    *+10                                                             
         MVC   RBUYCHGI,=C'X '                                                  
         MVC   BUYACT,=C'DEL'                                                   
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE OR GRAPHNET                              
         BNZ   DELB0300            YES                                          
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     DELB0380                                                         
*                                                                               
*  UPDATE REP VERSION NUMBER AND MARK UNCONFIRMED                               
*                                                                               
DELB0300 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    DELB0320                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
DELB0320 DS    0H                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    DELB0340                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    DELB0330                                                         
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
DELB0330 DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC)                                     
         BNZ   ERROR                                                            
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
                                                                                
DELB0340 DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSRV          STORE REP VERSION NO. IN BUY                 
         DROP R6                                                                
         STC   R1,RBUYVER          STORE REP VERSION NO. IN BUY                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    DELB0360                                                         
         SPACE 1                                                                
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
DELB0360 OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         SPACE 1                                                                
DELB0380 GOTO1 VMOVEREC,DMCB,RBUYREC,IOAREA                                     
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         BAS   RE,ECXDAY           EC BIAS CROSS DAY RECONTRUCT                 
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* IF EC/BIAS, READ ALL BUY RECORDS AND RECONSTRUCT CROSS DAY EXCLUSION          
***********************************************************************         
ECXDAY   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        EC BIAS?                                     
         BNE   ECXDAYX                                                          
         DROP  RF                                                               
                                                                                
         XC    WORK2,WORK2         WILL HOLD CROSS DAY                          
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(22),IOAREA      READ ALL BUYS UNDER THIS CONTRACT            
         GOTO1 VHIGH                                                            
         B     ECXDAY15                                                         
                                                                                
ECXDAY10 DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
ECXDAY15 DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   ECXDAY30                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ECXDAY10                                                         
                                                                                
         USING RBUYDYEL,R6                                                      
         ZIC   RF,RBUYDYIN         START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,RBUYDAYS         DAYS                                         
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   ECXDAY20                                                         
                                                                                
         USING RCONCCEL,R6                                                      
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    ECXDAY20                                                         
         LA    R2,BUYDAYSH                                                      
         LA    R3,399                                                           
         B     ERROR               CANNOT CROSS CROSS DAY DEFAULT               
         DROP  R6                                                               
                                                                                
ECXDAY20 DS    0H                                                               
         OC    WORK2(1),WORK       COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK2,X'7F'         ERROR IF ALL ON                              
         BNO   ECXDAY10            GET NEXT BUY RECORD                          
         LA    R2,BUYDAYSH                                                      
         B     ERROR                                                            
                                                                                
ECXDAY30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
*                                                                               
* DO WE HAVE TO PUT IN A CHECK TO SEE IF THE EC BIAS X'13' ELEMENT              
* BE ADDED TO A CONTRACT BEFORE A CERTAIN (RELEASE?) DATE?                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   ECXDAY40                                                         
         USING RCONCCEL,R6         UPDATE CROSS DAY                             
         MVC   RCONCCCD,WORK2                                                   
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     ECXDAY50                                                         
         DROP  R6                                                               
                                                                                
ECXDAY40 DS    0H                  NO BIAS EC ELEMENT, CONSTRUCT ONE            
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL1                                                
         MVC   RCONCCCD,WORK2      DAYS IN CROSS DAY FOR ALL BUYS               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
ECXDAY50 DS    0H                  REFRESH CROSS DAY INDICATORS                 
         MVC   BUYXDAY,=7C'.'      CLEAR                                        
         ZIC   RF,WORK2            CROSS DAY BYTE                               
         SLL   RF,25                                                            
         LA    RE,BUYXDAY                                                       
         LA    R1,C'1'                                                          
                                                                                
ECXDAY60 DS    0H                                                               
         LTR   RF,RF                                                            
         BNM   ECXDAY70            IF DAY BIT IS ON                             
         STC   R1,0(RE)            PUT CORRESPONDING NUMBER THERE               
                                                                                
ECXDAY70 DS    0H                                                               
         SLL   RF,1                NEXT DAY BIT                                 
         LA    RE,1(RE)            NEXT DAY DISPLAY POSITION                    
         LA    R1,1(R1)            NEXT EBCDIC DISPLAY NUMBER                   
         LA    R0,BUYXDAY+7        CHECK IF WE'VE LOOKED AT ALL 7 DAYS          
         CR    RE,R0                                                            
         BNH   ECXDAY60                                                         
         OI    BUYXDAYH+6,X'80'    XMIT IT                                      
                                                                                
ECXDAYX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF MULTI-MAKEGOOD IN USE                                                
***********************************************************************         
CHECKMG  NTR1  BASE=*,LABEL=*                                                   
         CLI   TWAACCS,C'$'        REP ONLY                                     
         BE    CMGX                                                             
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CMGX                                                             
*                                                                               
CMG10    DS    0H                                                               
CURRENTD USING RBUYMGEL,R3                                                      
NEXTD    USING RBUYMGEL,R6                                                      
         LR    R3,R6                                                            
         BAS   RE,NEXTEL                                                        
         BNE   CMGX                                                             
         CLC   CURRENTD.RBUYMGLI,NEXTD.RBUYMGLI                                 
         BE    CMG10                                                            
         LA    R3,863                                                           
         B     ERROR                                                            
*                                                                               
CMGX     DS    0H                                                               
         B     EXXMOD                                                           
         DROP  CURRENTD,NEXTD                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SECTION                                                              
***********************************************************************         
SECEDIT  NMOD1 0,*SECD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,BUYSECH          SECTION                                      
*                                                                               
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    SECE0015                                                         
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   SECE0015                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWASTAOB,X'80'      STATION PROF #18?                            
         BZ    SECE0015            NO - OK                                      
         DROP  RF                                                               
         LA    R3,850                                                           
         B     ERROR                                                            
*                                                                               
SECE0015 DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    SECX                                                             
*                                                                               
         CLI   TWAACCS,C'$'        CHANGE ANYTIME BY STATION W/O                
         BE    SECE0017            BUMPING VERSION NUMBER                       
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET?                                 
         BO    SECE0017            YES - DON'T BUMP VER                         
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
*                                                                               
SECE0017 CLI   ECFORMAT,C'B'       BIAS EC FORMAT?                              
         BE    SECE0020            YES - TEST WITHIN BIAS LIMITS                
         LA    R3,SECERR           SET POSSIBLE ERROR                           
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         GOTO1 VMOVE                                                            
         MVC   RBUYSEC,WORK                                                     
         B     SECX                                                             
*                                                                               
SECE0020 EQU   *                                                                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    SECE0040            NO                                           
         LA    R3,BIASSEC          SET POSSIBLE ERROR: BIAS FORMAT              
         CLI   5(R2),2             BIAS ONLY PERMITS 2 CHARS                    
         BH    ERROR                                                            
         CLI   8(R2),C'0'          FIRST CHAR:  BETWEEN 0-9?                    
         BL    ERROR               NO                                           
         CLI   8(R2),C'9'                                                       
         BH    ERROR                                                            
         CLI   5(R2),2             TWO CHARACTERS ENTERED?                      
         BL    SECE0040            NO                                           
         CLI   9(R2),C'0'          YES - SECOND CHAR:  BETWEEN 0-9?             
         BL    ERROR               NO                                           
         CLI   9(R2),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
SECE0040 EQU   *                                                                
         GOTO1 VMOVE                                                            
         MVC   RBUYSEC,WORK                                                     
*                                                                               
SECX     DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
* ELEM20 - WRITE EMPTY X'20' ELEM TO BUYREC IF PATTERN BUY IN PROGRESS          
***********************************************************************         
ELEM20   NMOD1 0,*EL20*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    ELEM20X             DON'T WRITE ELEM                             
         MVI   ELCODE,X'20'                                                     
         LA    R6,RBUYREC                                                       
         BAS   RE,GETEL                                                         
         BE    ELEM20X             ALREADY PRESENT                              
*                                                                               
         XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         LA    R6,WORK2                                                         
         USING RBUYPTEL,R6                                                      
         MVI   RBUYPTCD,X'20'                                                   
         MVI   RBUYPTLN,RBUYPTLQ                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
ELEM20X  XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
* VALIDATE PATTERN                                                              
***********************************************************************         
PTNEDIT  NMOD1 0,*PTNE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    NOTX                SKIP PATTERN/NOTATION FIELDS                 
         LA    R2,BUYCLSH          POINT TO PATTERN INPUT                       
         TM    4(R2),X'20'         VALIDATED BEFORE?                            
         BO    PTNX                YES, SKIP                                    
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         MVI   ELCODE,X'20'                                                     
         LA    R6,RBUYREC                                                       
         BAS   RE,GETEL                                                         
         BE    PTN010                                                           
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    PTNX                NO - LEAVE                                   
         XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         LA    R6,WORK2                                                         
         USING RBUYPTEL,R6                                                      
         MVI   RBUYPTCD,X'20'                                                   
         MVI   RBUYPTLN,RBUYPTLQ                                                
         MVC   RBUYPTPT,8(R2)                                                   
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         B     PTNX                                                             
         DROP  R6                                                               
*                                                                               
PTN010   DS    0H                                                               
         USING RBUYPTEL,R6                                                      
         CLI   5(R2),0             ANY INPUT?     .                             
         BNE   PTN020              YES                                          
         XC    RBUYPTPT,RBUYPTPT   NO - CLEAR PATTERN                           
         OC    RBUYPTNT,RBUYPTNT   ANY NOTATION IN ELEM?                        
         BNZ   PTNX                YES - KEEP IT                                
         GOTO1 VDELELEM,DMCB,(X'20',RBUYREC)  YES, DELETE OLD                   
         B     PTNX                                                             
PTN020   DS    0H                                                               
         MVC   RBUYPTPT,8(R2)                                                   
PTNX     DS    0H                                                               
         DROP  R6                                                               
*              VALIDATE PATTERN NOTATION                                        
         LA    R2,BUYNOTH          POINT TO PATTERN INPUT                       
         TM    4(R2),X'20'         VALIDATED BEFORE?                            
         BO    NOTX                YES, SKIP                                    
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    NOT010                                                           
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    NOTX                NO - LEAVE                                   
         XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         LA    R6,WORK2                                                         
         USING RBUYPTEL,R6                                                      
         MVI   RBUYPTCD,X'20'                                                   
         MVI   RBUYPTLN,RBUYPTLQ                                                
         MVC   RBUYPTNT,8(R2)                                                   
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         B     NOTX                                                             
         DROP  R6                                                               
*                                                                               
NOT010   DS    0H                                                               
         USING RBUYPTEL,R6                                                      
         CLI   5(R2),0             ANY INPUT?     .                             
         BNE   NOT020              YES                                          
         XC    RBUYPTNT,RBUYPTNT   NO - CLEAR NOTATION                          
         OC    RBUYPTPT,RBUYPTPT   ANY PATTERN IN ELEM?                         
         BNZ   NOTX                YES - KEEP IT                                
         GOTO1 VDELELEM,DMCB,(X'20',RBUYREC)  YES, DELETE OLD                   
         B     NOTX                                                             
NOT020   DS    0H                                                               
         MVC   RBUYPTNT,8(R2)                                                   
NOTX     DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
* VALIDATE 'USE PATTERN TIMES'                                                  
***********************************************************************         
UPTEDIT  NMOD1 0,*UPTE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    UPTX                         NO - SKIP                           
         LA    R2,BUYUPTH          POINT TO PATTERN INPUT                       
         TM    4(R2),X'20'         VALIDATED BEFORE?                            
         BZ    UPT010              NO - VALIDATE                                
         TM    STAT2,X'80'         ANYTHING ELSE CHANGED?                       
         BZ    UPTX                NO NEED TO VALIDATE                          
*                                                                               
UPT010   DS    0H                                                               
         LA    R3,INVINP                                                        
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         MVI   ELCODE,X'20'                                                     
         LA    R6,RBUYREC                                                       
         BAS   RE,GETEL                                                         
         BE    UPT020                                                           
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         LA    R3,846              NO PATTERN INPUT                             
         B     ERROR                                                            
         CLI   8(R2),0                                                          
         BE    UPTX                                                             
         CLI   8(R2),C'N'                                                       
         BE    UPTX                                                             
         B     ERROR                                                            
*                                                                               
UPT020   DS    0H                                                               
         USING RBUYPTEL,R6                                                      
         CLI   8(R2),0             NO INPUT                                     
         BNE   UPT030                                                           
         MVI   8(R2),C'N'          DEFAULT 'N'                                  
*                                                                               
UPT030   DS    0H                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   UPT040                                                           
         OC    RBUYPTPT,RBUYPTPT   HAVE A PATTERN?                              
         BNZ   *+12                NO                                           
         LA    R3,846                                                           
         B     ERROR                                                            
         OI    RBUYPTFL,X'80'                                                   
         B     UPTX                                                             
UPT040   DS    0H                                                               
         CLI   8(R2),C'N'                                                       
         BNE   ERROR                                                            
         NI    RBUYPTFL,X'FF'-X'80'                                             
         DROP  R6                                                               
UPTX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* CORRSPOT:  SCAN PETRY ORDERS FOR MISSING OVERRIDE FLAG                        
***********************************************************************         
CORRSPOT NMOD1 0,*COSP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R1,IOAREA           SET A(BUY RECORD)                            
         LA    R6,IOAREA           SET A(BUY RECORD)                            
         MVI   ELCODE,3            FIND EFF DATE ELEMENT(S)                     
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     COSP0040                                                         
COSP0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT(S)                          
COSP0040 EQU   *                                                                
         BNE   COSP0800            NO MORE ELEMENTS: FINISHED                   
         CLC   RBUYNW-RBUYREC(1,R1),RBUYDTNW-RBUYDTEL(R6)                       
*                                  OVERALL #/WK VS EFF DATE # WEEK              
         BE    COSP0020            SAME:  GO BACK FOR NEXT                      
         OI    RBUYDTIN-RBUYDTEL(R6),X'01'                                      
*                                  SET #/WEEK OVERRIDE                          
         B     COSP0020            GO BACK FOR NEXT                             
COSP0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*&&                                                                             
*                                                                               
***********************************************************************         
* ADD EXTRA DESCRIPTION ELEMENT X'80'                                           
***********************************************************************         
ADDEXTRA NMOD1 0,*ADEX*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         XC    WORK,WORK                                                        
WKD      USING RBUYXXEL,WORK                                                    
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         MVC   WKD.RBUYXXMD,RCONMOD    CONTRACT MOD# AT BUY CREATION            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDEXT10                                                         
         USING RCONSEND,R6                                                      
         MVC   WKD.RBUYXXVR,RCONSRV    CONTRACT VER# AT BUY CREATION            
         TM    RCONSENF,X'20'                                                   
         BZ    ADDEXT10                                                         
         ZIC   R1,WKD.RBUYXXVR         REP VERSION NOT ADVANCED                 
         LA    R1,2(R1)                ADVANCE VERSION MANUALLY                 
         STC   R1,WKD.RBUYXXVR                                                  
         DROP  R6,WKD                                                           
*                                                                               
ADDEXT10 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK                                       
*                                                                               
ADDEXTX  DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SPLIT03S:  FOR A MAKEGOOD, BREAK THE TARGETED 03 ELEMENT (EFFECT-           
*        IVE DATE/TIME) INTO TWO OR MORE 03 ELEMENTS, SUBTRACTING               
*        THE MISSED SPOTS FROM THE EFFECTED WEEK.                               
*        THIS ROUTINE WORKS BY STARTING AT THE LAST DATE OF THE X'03'           
*        ELEMENT TO BE SPLIT.  IF THE ELEMENT IS A SINGLE WEEK, IT              
*        CANNOT BE SPLIT, AND THE SPOTS MISSED ARE SUBTRACTED FROM              
*        IT.  WHEN THE ELEMENT IS MORE THAN ONE WEEK IN LENGTH,                 
*        THE DATES ARE BACKED UP ONE WEEK AT A TIME, UNTIL THE WEEK             
*        OF THE MISSED SPOT IS ENCOUNTERED.  THE ELEMENT AFTER THE              
*        MISSED SPOT WEEK IS PUT OUT, THEN THE ELEMENT CONTAINING               
*        THE MISSED SPOT IS PUT OUT, WITH THE OVERRIDE BIT SET.                 
*        FINALLY, IF THE MISSED SPOT IS NOT IN THE FIRST WEEK, AN               
*        ADDITIONAL ELEMENT IS PUT OUT WITH THE EARLIEST DATES.                 
*        THE ORIGINAL ELEMENT IS DELETED.                                       
*                                                                               
         CSECT                                                                  
SPLIT03S NMOD1 0,*SPLT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,A03ELT           RESET A(03 ELT TO BE SPLIT)                  
         USING RBUYDTEL,R6         COVER ORIGINAL 03 ELT                        
         LA    R2,DUB              A(NEW 06 MISSED SPOT ELT)                    
         USING RBUYMSEL,R2         COVER NEW 06 MISSED SPOT ELT                 
         XC    SAVENDTE,SAVENDTE   CLEAR SAVE AREA FOR END DATE                 
         CLI   RBUYDTWK,1          ORIGINAL WEEKS = 1?                          
         BNE   SPLS0040            NO  - MUST SPLIT IT UP                       
*                                  YES - CAN'T BE SPLIT                         
         ZIC   RF,RBUYMSSP         NUMBER SPOTS MISSED                          
         ZIC   RE,RBUYDTNW         NUMBER SPOTS/WEEK                            
         SR    RE,RF               SPOTS/WEEK - SPOTS MISSED                    
         STC   RE,RBUYDTNW         PUT IT BACK                                  
         OI    RBUYDTIN,X'01'      SET SPOT OVERRIDE FLAG                       
         B     SPLS0840            EXIT                                         
SPLS0040 EQU   *                                                                
         MVI   0(R6),X'FF'         PREPARE ORIGINAL FOR DELETION                
         ZIC   RF,1(R6)            MOVE ELEMENT TO STORAGE                      
         EX    RF,SPLS0880            BY LENGTH                                 
         MVI   ORIDEFLG,0          SET ORIGINAL OVERRIDE FLAG OFF               
         TM    RBUYDTIN,X'01'      OVERRIDE FLAG ON?                            
         BNO   SPLS0080            NO  - LEAVE SET TO OFF                       
         MVI   ORIDEFLG,1          SET ORIGINAL OVERRIDE FLAG ON                
SPLS0080 EQU   *                                                                
         TM    RBUYDTIN,X'40'      ALTERNATING FLAG SET?                        
         BNO   SPLS0120            NO                                           
         MVI   ALTWKFLG,1          YES - SET ALTERNATING WEEK VALUE             
         BAS   RE,ALTWKCHK         TEST NUMBER OF WEEKS IN FLIGHT               
*                                  IF NUMBER OF WEEKS IS EVEN,                  
*                                     ALTWKFLG X'02' NOT SET                    
*                                     IF ODD, X'02' IS SET                      
SPLS0120 EQU   *                                                                
         MVC   SAV03ELT,SAV03ORI   SET UP 1ST WORK AREA                         
         MVI   SAV03ELT,X'03'      RESET ELEMENT TYPE                           
         XC    SAV03EL2,SAV03EL2   CLEAR 2ND WORK AREA                          
         LA    R6,SAV03ELT         RESET 'USING' ADDR TO 1ST WORK AREA          
         MVC   SDATSPLT,RBUYDTST   CONVERT START DATE TO EBCDIC                 
         GOTO1 DATCON,DMCB,(3,SDATSPLT),(0,SDATSPLT+4)                          
         GOTO1 GETDAY,(R1),SDATSPLT+4,FULL                                      
*                                  GET DAY OF WEEK OF START DATE                
         MVC   FULL(1),DMCB        GET DAY NUMBER                               
         MVC   EDATSPLT,RBUYDTED   CONVERT END   DATE TO EBCDIC                 
         GOTO1 DATCON,DMCB,(3,EDATSPLT),(0,EDATSPLT+4)                          
         GOTO1 GETDAY,(R1),EDATSPLT+4,FULL+1                                    
*                                  GET DAY OF WEEK OF END   DATE                
         MVC   FULL+1(1),DMCB      GET DAY NUMBER                               
         CLC   FULL(1),FULL+1      START DAY = END DAY?                         
         BNE   SPLS0160            NO                                           
         MVC   RBUYDTST,RBUYDTED   YES - MOVE END DATE TO START DATE            
         MVI   RBUYDTWK,1          SET NUMBER OF WEEKS TO 1                     
         B     SPLS0280                                                         
SPLS0160 EQU   *                                                                
         BL    SPLS0200            START DAY < END DAY                          
*                                  OUT-OF-WEEK ROTATOR:                         
*                                     START DAY > END DAY                       
         ZIC   RF,FULL             CALCULATE START DAY                          
         LA    RE,7                                                             
         SR    RE,RF               SUBTRACT START FROM 7                        
         ZIC   RF,FULL+1           END DAY                                      
         AR    RE,RF               ADD END DAY TO CALC'D START                  
         B     SPLS0240                                                         
SPLS0200 EQU   *                                                                
         ZIC   RE,FULL+1           END DAY                                      
         ZIC   RF,FULL             START DAY                                    
         SR    RE,RF               SUBTRACT START FROM END                      
SPLS0240 EQU   *                                                                
         TM    ALTWKFLG,X'01'      ALTERNATING WEEK BUYLINE?                    
         BNO   SPLS0260            NO                                           
         TM    ALTWKFLG,X'02'      YES - ODD NUMBER OF WEEKS?                   
         BO    SPLS0260            YES                                          
         LA    RE,7(RE)            NO  - ADD EXTRA WEEK TO START                
SPLS0260 EQU   *                                                                
         LNR   RE,RE               NEGATE THE RESULT                            
         LR    R4,RE                                                            
         PRINT GEN                                                              
         GOTO1 ADDAY,DMCB,EDATSPLT+4,SDATSPLT+4,(R4)                            
         PRINT NOGEN                                                            
*                                  CALCULATE A NEW START DATE                   
         GOTO1 DATCON,DMCB,SDATSPLT+4,(3,RBUYDTST)                              
         MVI   RBUYDTWK,1          RESET NUMBER OF WEEKS TO 1                   
SPLS0280 EQU   *                                                                
         CLC   RBUYMSDT,RBUYDTST   MISSED DATE VS 03 START DATE                 
         BNL   SPLS0480            MISSED = OR > THAN START DATE                
         MVC   SAV03EL2,SAV03ELT   SAVE 03 ELEMENT BEFORE START                 
*                                     DATE IS BUMPED BACK 1 WEEK                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,SDATSPLT+4)                          
         LA    R4,7                SUBTRACT 7 DAYS FROM DATE                    
         CLI   ALTWKFLG,0          ALTERNATING WEEK?                            
         BE    SPLS0320            NO  - DON'T NEED ADJUSTMENT                  
         LA    R4,7(R4)            YES - ADD ANOTHER WEEK TO BACK UP            
SPLS0320 EQU   *                                                                
         LNR   R4,R4                                                            
         GOTO1 ADDAY,DMCB,SDATSPLT+4,SDATSPLT+4,(R4)                            
         GOTO1 DATCON,(R1),SDATSPLT+4,(3,RBUYDTST)                              
*                                  INSERT NEW START DATE INTO ELEMENT           
         ZIC   RF,RBUYDTWK         BUMP # WEEKS BY 1                            
         LA    RF,1(RF)                                                         
         STC   RF,RBUYDTWK         PUT IT BACK                                  
         OC    SAVENDTE,SAVENDTE   ANY END DATE SET?                            
         BZ    SPLS0360            NO  - USE END DATE IN ELEMENT                
*                                  YES - USE END DATE SET                       
         GOTO1 DATCON,DMCB,(3,SAVENDTE),(0,EDATSPLT+4)                          
         B     SPLS0400                                                         
SPLS0360 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,EDATSPLT+4)                          
SPLS0400 EQU   *                                                                
         LA    R4,7                SUBTRACT 7 DAYS FROM DATE                    
         CLI   ALTWKFLG,0          ALTERNATING WEEK?                            
         BE    SPLS0440            NO  - DON'T NEED ADJUSTMENT                  
         CLI   FRSTWEEK,0          FIRST PASS?                                  
         BE    SPLS0440            YES - DON'T DOUBLE UP                        
         LA    R4,7(R4)            ADD ANOTHER WEEK TO BACK UP                  
SPLS0440 EQU   *                                                                
         MVI   FRSTWEEK,1          TURN OFF FIRST PASS SWITCH                   
         LNR   R4,R4                                                            
         GOTO1 ADDAY,DMCB,EDATSPLT+4,EDATSPLT+4,(R4)                            
         GOTO1 DATCON,(R1),EDATSPLT+4,(3,SAVENDTE)                              
*                                  SAVE NEW END DATE                            
         B     SPLS0280            GO BACK AND CHECK DATES AGAIN                
SPLS0480 EQU   *                                                                
         OC    SAV03EL2,SAV03EL2   ANYTHING IN 2ND WORK SPACE?                  
         BZ    SPLS0520            NO  - EMPTY                                  
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,SAV03EL2,0                  
*                                  ADD 03 ELT FROM 2ND WORK SPACE               
*                                     WITH ORIGL OVERRIDE FLAG VALUE            
         XC    SAV03EL2,SAV03EL2   CLEAR 2ND WORK SPACE                         
SPLS0520 EQU   *                                                                
         OC    SAVENDTE,SAVENDTE   ANYTHING IN SAVED END DATE?                  
         BZ    SPLS0560            NO                                           
         MVC   RBUYDTED,SAVENDTE   YES - RESET END DATE TO SAVED                
SPLS0560 EQU   *                                                                
         ZIC   RF,RBUYMSSP         NUMBER OF SPOTS MISSED                       
         ZIC   RE,RBUYDTNW         # SPOTS/WEEK IN 03 ELEMENT                   
         MVC   SAVEDTNW,RBUYDTNW   SAVE ORIGINAL #/WEEK                         
         SR    RE,RF               SUBTRACT MISSED FROM #/WEEK                  
         STC   RE,RBUYDTNW         RETURN CALCULATED VALUE                      
         OI    RBUYDTIN,X'01'      SET #/WEEK OVERRIDE                          
         MVI   RBUYDTWK,1          MISSED ELEMENT ONLY FOR 1 WEEK               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,SAV03ELT,0                  
*                                  ADD 03 ELT FROM 1ST WORK SPACE               
*                                     THIS IS THE MISSED ELEMENT!               
*                                     OVERRIDE FLAG FORCED TO 'ON'              
         CLC   RBUYDTST,SAV03ORI+2 MISSED DATE ELT = START                      
*                                     DATE OF ORIGINAL ELEMENT?                 
         BE    SPLS0800            YES - MISSED ELEMENT IS LAST/FIRST           
*                                     ELEMENT TO BE ADDED                       
*                                  NO  - BACK UP BOTH DATES ONE WEEK            
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,SDATSPLT+4)                          
         LA    R4,7                SUBTRACT 7 DAYS FROM DATE                    
         CLI   ALTWKFLG,0          ALTERNATING WEEK?                            
         BE    SPLS0600            NO  - DON'T NEED ADJUSTMENT                  
         LA    R4,7(R4)            ADD ANOTHER WEEK TO BACK UP                  
SPLS0600 EQU   *                                                                
         LNR   R4,R4                                                            
         GOTO1 ADDAY,DMCB,SDATSPLT+4,SDATSPLT+4,(R4)                            
         GOTO1 DATCON,(R1),SDATSPLT+4,(3,RBUYDTST)                              
*                                  INSERT NEW START DATE INTO ELEMENT           
         MVI   RBUYDTWK,1          RESET # WEEKS TO 1                           
         MVC   RBUYDTNW,SAVEDTNW   RESET #SPOTS/WEEK                            
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,EDATSPLT+4)                          
         LA    R4,7                SUBTRACT 7 DAYS FROM DATE                    
         CLI   ALTWKFLG,0          ALTERNATING WEEK?                            
         BE    SPLS0640            NO  - DON'T NEED ADJUSTMENT                  
         LA    R4,7(R4)            ADD ANOTHER WEEK TO BACK UP                  
SPLS0640 EQU   *                                                                
         LNR   R4,R4                                                            
         GOTO1 ADDAY,DMCB,EDATSPLT+4,EDATSPLT+4,(R4)                            
         GOTO1 DATCON,(R1),EDATSPLT+4,(3,RBUYDTED)                              
*                                                                               
* WE MIGHT HAVE OVER-COMPENSATED FOR ALTERNATE WEEKS, CHECK IF WE               
* BACKED UP TOO MUCH                                                            
*                                                                               
         CLI   ALTWKFLG,0          ALTERNATING WEEK?                            
         BE    SPLS0680            NO  - DON'T NEED TO CHECK                    
         CLC   RBUYDTST,SAV03ORI+2                                              
         BNL   SPLS0680                                                         
         MVC   RBUYDTST,SAV03ORI+2                                              
         B     SPLS0760                                                         
*                                  CYCLE OUT REMAINING 03 ELT                   
SPLS0680 EQU   *                                                                
         CLC   RBUYDTST,SAV03ORI+2 NEW START DATE = ORIGINAL 03 DATE?           
         BE    SPLS0760            YES                                          
*                                  NO  - DROP START DATE BACK 1 WEEK            
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,SDATSPLT+4)                          
         LA    R4,7                SUBTRACT 7 DAYS FROM DATE                    
         CLI   ALTWKFLG,0          ALTERNATING WEEK?                            
         BE    SPLS0720            NO  - DON'T NEED ADJUSTMENT                  
         LA    R4,7(R4)            ADD ANOTHER WEEK TO BACK UP                  
SPLS0720 EQU   *                                                                
         LNR   R4,R4                                                            
         GOTO1 ADDAY,DMCB,SDATSPLT+4,SDATSPLT+4,(R4)                            
         GOTO1 DATCON,(R1),SDATSPLT+4,(3,RBUYDTST)                              
*                                  INSERT NEW START DATE INTO ELEMENT           
         ZIC   RF,RBUYDTWK         INCREMENT # OF WEEKS                         
         LA    RF,1(RF)                                                         
         STC   RF,RBUYDTWK         PUT IT BACK                                  
         B     SPLS0680            GO BACK AND CHECK DATE                       
SPLS0760 EQU   *                                                                
         NI    RBUYDTIN,X'FF'-X'01'                                             
*                                  TURN OFF OVERRIDE FLAG                       
         OC    RBUYDTIN,ORIDEFLG   SET BASED ON ORIGINAL FLAG                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,SAV03ELT,0                  
*                                  ADD 03 ELT FROM 1ST WORK SPACE               
*                                  THIS IS THE FIRST 03 ELT                     
SPLS0800 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'FF',IOAREA)                                     
*                                  DELETE ORIGINAL X'03' ELEMENT                
SPLS0840 EQU   *                                                                
         XIT1                                                                   
SPLS0880 MVC   SAV03ORI(0),0(R6)                                                
*                                                                               
         EJECT                                                                  
*  ALTWKCHK:  TESTS NUMBER OF WEEKS IN ALTERNATING WEEK FLIGHT.  THE            
*        03 ELEMENT IS SET UP DIFFERENTLY WHEN CONSTRUCTED BY ENTRY             
*        OF DATES AS 'S-EA' OR 'JAN9-3WA'.  THE FORMER PRODUCES AN              
*        ODD NUMBER OF WEEKS, WHILE THE LATTER PRODUCES AN EVEN                 
*        NUMBER OF WEEKS.  THIS WILL DETERMINE THE DATE INCREMENT FOR           
*        THE MAKEGOOD BREAKOUT ROUTINE.                                         
*        RESULTS RETURNED BY PERVERT MAY BE:                                    
*          ODD # OF WEEKS, DAYS REMAINING    --->  EVEN # WEEKS                 
*          ODD # OF WEEKS, NO DAYS REMAINING --->  ODD # OF WEEKS               
*          EVEN # OF WEEKS, DAYS REMAINING   --->  ODD # OF WEEKS               
*          EVEN # OF WEEKS, NO DAYS REMAINING--->  EVEN # OF WEEKS              
*                                                                               
ALTWKCHK NTR1                                                                   
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,SDATSPLT)                            
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,EDATSPLT)                            
*                                  CONVERT END   DATE TO EBCDIC                 
         GOTO1 PERVERT,DMCB,SDATSPLT,EDATSPLT,0,0                               
         TM    DMCB+13,X'01'       NUMBER OF WEEKS ODD?                         
         BNO   ALTW0040            NO  - CHECK # WEEKS EVEN                     
         OC    DMCB+10(2),DMCB+10  ANY DAYS REMAINING?                          
         BNZ   ALTW0080            YES - EVEN NUMBER OF WEEKS                   
         OI    ALTWKFLG,X'02'      NO  - SET ODD WEEK INDICATOR                 
         B     ALTW0120                                                         
ALTW0040 EQU   *                                                                
*                                  # OF WEEKS EVEN - CHECK DAYS                 
         OC    DMCB+10(2),DMCB+10  ANY DAYS REMAINING?                          
         BZ    ALTW0080            NO  - EVEN NUMBER OF WEEKS                   
         OI    ALTWKFLG,X'02'      YES - SET ODD WEEK INDICATOR                 
         B     ALTW0120                                                         
ALTW0080 EQU   *                                                                
         MVI   FRSTWEEK,1          TURN OFF ONE-TIME SWITCH                     
ALTW0120 EQU   *                                                                
         XIT1                                                                   
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
         CSECT                                                                  
SAVEMG03 NMOD1 0,*SV03*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'56'        FIND FIRST MG REF ELT                        
         BAS   RE,GETEL                                                         
         BNE   SVMG0200            NONE FOUND - EXIT                            
         LR    R5,R6               SAVE A(56 ELT)                               
         USING RBYMGSEL,R5                                                      
SVMG0020 EQU   *                                                                
         LA    R6,RBUYREC          RESET FOR FIRST 03 ELT                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     SVMG0060                                                         
SVMG0040 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
SVMG0060 EQU   *                                                                
         BNE   SVMG0080            NOT FOUND                                    
*                                     REALLY SHOULDN'T HAPPEN                   
         USING RBUYDTEL,R6                                                      
         CLC   RBYMGSDT,RBUYDTST   MG MISSED DATE EARLIER?                      
         BL    SVMG0040            YES                                          
         CLC   RBYMGSDT,RBUYDTED   MG MISSED DATE LATER?                        
         BH    SVMG0040            YES - FIND NEXT 03 ELT                       
         OI    RBUYDTIN,X'02'      DATE FOUND: SET FLAG                         
SVMG0080 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP X'56' ELTS                              
         AR    R5,RF                                                            
         CLI   0(R5),X'56'         ANOTHER X'56' ELT?                           
         BE    SVMG0020            YES - GO BACK FOR 03 ELTS                    
SVMG0200 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'        FIND FIRST MG REF ELT                        
         BAS   RE,GETEL                                                         
         B     SVMG0240                                                         
SVMG0220 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
SVMG0240 EQU   *                                                                
         BNE   SVMG0300            NONE FOUND - EXIT                            
         TM    RBUYDTIN,X'02'      MG FLAG SET?                                 
         BNO   SVMG0220            NO  - GO BACK FOR NEXT                       
         MVI   RBUYDTCD,X'33'      YES - RESET ELT CODE TO X'33'                
         NI    RBUYDTIN,X'FF'-X'02'                                             
*                                  TURN MG FLAG OFF                             
         B     SVMG0220            GO BACK FOR NEXT                             
SVMG0300 EQU   *                                                                
         XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         CSECT                                                                  
CHKMGDS  NMOD1 0,*CMGS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'33'        FIND FIRST MG CHECK ELT                      
         BAS   RE,GETEL                                                         
         BNE   CKMG0100            NONE FOUND - CC = ZERO, EXIT                 
         LR    R5,R6               SAVE A(33 ELT)                               
CKMG0020 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'        FIND FIRST 03 ELT                            
         BAS   RE,GETEL                                                         
         B     CKMG0080                                                         
CKMG0040 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
CKMG0060 EQU   *                                                                
         BNE   CKMG0120            NONE FOUND - ERROR                           
CKMG0080 EQU   *                                                                
         CLC   2(6,R6),2(R5)       COMPARE ELEMENT DATES:                       
*                                     NEW 03 ELT VS MG CHECK ELT                
         BNE   CKMG0040            NOT FOUND - GO BACK                          
         OI    RBUYDTIN-RBUYDTEL(R6),X'01'                                      
*                                  TURN ON OVERRIDE FLAG IN 03                  
         ZIC   RF,1(R5)            FOUND - CHECK NEXT 33 ELT                    
         AR    R5,RF                                                            
         CLI   0(R5),X'33'         ANOTHER 33 ELT?                              
         BE    CKMG0020            YES - CHECK IT OUT                           
         GOTO1 VDELELEM,DMCB,(X'33',RBUYREC)                                    
*                                  DELETE MG CHECK ELTS                         
CKMG0100 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     CKMG0200                                                         
CKMG0120 EQU   *                                                                
         LTR   RB,RB               SET CC = NON-ZERO                            
CKMG0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* BUILD GRID BASED ON TARGET BUY RECORD'S X'03' ELEMENTS                        
* REMOVE SPOTS AS SPECIFIED IN SAV56ORI                                         
* REBUILD X'03' ELEMENTS FROM GRID                                              
*                                                                               
MGPROC   NMOD1 MGWORKX-MGWORKD,*MGPROC*,CLEAR=YES,RR=R3                         
         LR    R7,RC                                                            
         USING MGWORKD,R7                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   SVELCODE,ELCODE     SAVE OFF CURRENT ELCODE                      
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   PERVAL,CPERVAL                                                   
         DROP  RE                                                               
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         MVC   GSTRDATE,RBUYDTST   GET BUY START DATE                           
         XC    BUYGRID,BUYGRID                                                  
         LA    R4,BUYGRID          POINT TO START OF GRID                       
         B     MGP20               SKIP OFFSET FOR FIRST DATES                  
*                                                                               
MGP10    DS    0H                                                               
         LA    R4,BUYGRID          RESET TO START OF GRID                       
         XC    ELEM,ELEM                                                        
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,GSTRDATE),(5,ELEM)                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELEM+9)                              
         MVI   ELEM+8,C'-'                                                      
         GOTO1 PERVAL,DMCB,(17,ELEM),WORK2                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         SLL   R1,1                MULTIPLY BY 2                                
         AR    R4,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
MGP20    DS    0H                                                               
         ZIC   R1,RBUYDTWK                                                      
*                                                                               
MGP30    DS    0H                                                               
         MVC   0(1,R4),RBUYDTNW                                                 
*                                                                               
         CLI   RBUYDTNW,0          IF ZERO SPOT, FORCE OVERRIDE                 
         BE    MGP35                                                            
         TM    RBUYDTIN,X'01'      NPW OVERRIDE??                               
         BZ    MGP38                                                            
*                                                                               
MGP35    DS    0H                                                               
         OI    1(R4),X'01'                                                      
*                                                                               
MGP38    DS    0H                                                               
         AHI   R4,2                BUMP TO NEXT WEEK                            
         TM    RBUYDTIN,X'40'      ALTERNATE WEEKS??                            
         BZ    *+8                                                              
         AHI   R4,2                YES, BUMP ONE MORE                           
         LTR   R1,R1                                                            
         BZ    MGP40                                                            
         BCT   R1,MGP30            PROCESS FOR SPECIFIED NUMBER OF WKS          
*                                                                               
MGP40    DS    0H                  INCASE OF MULTIPLE EFFECTIVE DATES           
         BAS   RE,NEXTEL                                                        
         BE    MGP10                                                            
         DROP  R6                                                               
*                                                                               
* REMOVE SPOT AS SPECIFIED IN SAV56ORI                                          
*                                                                               
S56D     USING RBYMGSEL,SAV56ORI                                                
*                                                                               
         LA    R2,BUYGRID          RESET TO START OF GRID                       
         XC    ELEM,ELEM                                                        
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,GSTRDATE),(5,ELEM)                                
         GOTO1 DATCON,DMCB,(3,S56D.RBYMGSDT),(5,ELEM+9)                         
         MVI   ELEM+8,C'-'                                                      
         GOTO1 PERVAL,DMCB,(17,ELEM),WORK2                                      
         LA    R3,MG2ERR                                                        
         CLI   DMCB+4,0                                                         
         BNE   ERROR                                                            
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         SLL   R1,1                MULTIPLY BY 2                                
         AR    R2,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
         ZIC   RF,S56D.RBYMGSSP                                                 
         ZIC   RE,0(R2)                                                         
         AR    RE,RF                                                            
         STC   RE,0(R2)                                                         
*                                                                               
         BAS   RE,REBLDBUY                                                      
*                                                                               
         MVC   ELCODE,SVELCODE     RESTORE ELCODE                               
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* REBUILD X'03' ELEMENT FROM BUY GRID                                           
***********************************************************************         
REBLDBUY NTR1                                                                   
*                                                                               
         LA    R5,BUYGRID          BUY GRID TO BE REBUILD                       
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RBLD03   DS    0H                  POINT TO LAST X'02' ELEMENT TO GET           
         LR    R2,R6               END DAY                                      
         BAS   RE,NEXTEL                                                        
         BE    RBLD03                                                           
         LR    R6,R2                                                            
         USING RBUYDYEL,R6                                                      
         MVI   MSENDDAY,0     GET END DAY                                       
         MVN   MSENDDAY,RBUYDYIN                                                
         DROP  R6                                                               
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6         SAVE OFF BUY START DATE                      
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,MSSTDT)                              
         DROP  R6                                                               
*                                                                               
* DELETE OLD X'03'S                                                             
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'03',IOAREA)                                     
*                                                                               
         SR    R3,R3                                                            
*                                                                               
         LA    R6,IOAREA                                                        
TBUYD    USING RBUYREC,R6                                                       
*                                                                               
RBLD05   DS    0H                                                               
         LA    R4,1                                                             
         XC    MSBUYEL,MSBUYEL                                                  
WKD      USING RBUYDTEL,MSBUYEL                                                 
*                                                                               
         MVI   WKD.RBUYDTCD,X'03'                                               
         MVI   WKD.RBUYDTLN,11                                                  
*                                                                               
RBLD08   DS    0H                                                               
         CLI   0(R5),0                                                          
         BNE   RBLD10                                                           
         TM    1(R5),X'01'         OVERRIDE??                                   
         BZ    RBLD40                                                           
*                                                                               
RBLD10   DS    0H                                                               
         OC    WKD.RBUYDTST,WKD.RBUYDTST                                        
         BNZ   RBLD20              NEED TO FIND START DATE                      
         LTR   R3,R3                                                            
         BNZ   RBLD15                                                           
         MVC   WKD.RBUYDTST,MSSTDT                                              
         MVC   MSSTDT2,MSSTDT                                                   
         GOTO1 DATCON,DMCB,(0,MSSTDT),(3,WKD.RBUYDTST)                          
         B     RBLD20                                                           
*                                                                               
RBLD15   DS    0H                  SUBSEQUENT X'03'S NEED TO COMPUTE            
         LR    R2,R3               START DATES RELATIVE TO INITIAL              
         MHI   R2,7                FLIGHT START FOR THIS BUY                    
         GOTO1 ADDAY,DMCB,MSSTDT,MSSTDT2,(R2)                                   
         GOTO1 DATCON,DMCB,(0,MSSTDT2),(3,WKD.RBUYDTST)                         
*                                                                               
RBLD20   DS    0H                                                               
         CLC   0(1,R5),2(R5)                                                    
         BNE   RBLD23                                                           
         CLI   0(R5),0             INCASE ALL SPOTS CREDITED OUT FOR            
         BNE   RBLD21              THIS WEEK, CHECK IF ZERO SPOT                
         TM    3(R5),X'01'         OVERRIDE FLAG IS SET                         
         BZ    RBLD23                                                           
*                                                                               
RBLD21   DS    0H                                                               
         AHI   R5,2                                                             
         AHI   R4,1                                                             
         AHI   R3,1                                                             
         CHI   R3,53                                                            
         BL    RBLD08              BUMP TO NEXT CELL                            
*                                                                               
RBLD23   DS    0H                                                               
         MVC   WKD.RBUYDTNW,0(R5)                                               
         STC   R4,WKD.RBUYDTWK                                                  
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         OI    WKD.RBUYDTIN,X'80'  DEFAULT IS WEEKLY                            
*        TM    1(R5),X'01'         OVERRIDE??                                   
*        BZ    *+8                                                              
*        OI    WKD.RBUYDTIN,X'01'                                               
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         CLC   TBUYD.RBUYNW,0(R5)                                               
         BE    RBLD25                                                           
         OI    WKD.RBUYDTIN,X'01'                                               
         DROP  TBUYD                                                            
***********                                                                     
***********                                                                     
*                                                                               
* CALCULATE END DATE                                                            
*                                                                               
RBLD25   DS    0H                                                               
         ZIC   R2,WKD.RBUYDTWK                                                  
         MHI   R2,7                                                             
         GOTO1 ADDAY,DMCB,MSSTDT2,MSSTDT2,(R2)                                  
*                                                                               
         GOTO1 GETDAY,DMCB,MSSTDT2,FULL                                         
         ZIC   RE,MSENDDAY                                                      
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    RBLD35                                                           
         BP    RBLD30                                                           
*                                                                               
* HANDLE OUT OF WEEK ROTATIONS                                                  
*                                                                               
         LR    R2,RE                                                            
         B     RBLD38                                                           
*                                                                               
RBLD30   DS    0H                                                               
         LA    R2,7                                                             
         LNR   R2,R2                                                            
         AR    R2,RE                                                            
         B     RBLD38                                                           
*                                                                               
RBLD35   DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    R2,7                                                             
         LNR   R2,R2                                                            
*                                                                               
RBLD38   DS    0H                                                               
         GOTO1 ADDAY,DMCB,MSSTDT2,MSSTDT2,(R2)                                  
         GOTO1 DATCON,DMCB,(0,MSSTDT2),(3,WKD.RBUYDTED)                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,IOAREA),               X        
               (0,WKD.RBUYDTEL)                                                 
*                                                                               
RBLD40   DS    0H                                                               
         AHI   R5,2                                                             
         LA    R3,1(R3)                                                         
         CH    R3,=H'53'                                                        
         BL    RBLD05              BUMP TO NEXT CELL                            
*                                                                               
RBLDX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  WKD,R7                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* NEW CREDIT PROCESSING WHERE THE CREDITED SPOTS ARE TAKEN OUT OF THE           
* ORIGINAL BUY INSTEAD OF ADDING A BUY WITH NEGATIVE RATES                      
*                                                                               
* WORK3 HAS COMBINED DATES FIELDS FAKED AS A HEADER FIELD                       
* USES FIRST 106 BYTES OF WORK2 FOR GRID BUILDING AND PROCESSING                
* PERVAL BLOCK IS USED AT WORK2+120                                             
***********************************************************************         
CREDIT   NMOD1 CREDITDX-CREDITD,*CREDIT*,CLEAR=YES                              
         LR    R5,RC                                                            
         USING CREDITD,R5                                                       
         L     RC,0(R1)                                                         
*                                                                               
* BUILD SPOT GRID FROM CURRENT X'03' ELEMENTS                                   
*                                                                               
         XC    WORK2,WORK2                                                      
*                                                                               
         BAS   RE,BLDGRID                                                       
*                                                                               
* REMOVE SPOTS FROM GRID                                                        
*                                                                               
         BAS   RE,REMGRID                                                       
*                                                                               
* REBUILD X'03' ELEMENT FROM GRID                                               
*                                                                               
         BAS   RE,BLDBUY                                                        
*                                                                               
* ADD AUDIT TRAIL AND BUY COMMENT                                               
*                                                                               
         BAS   RE,ADDAUDIT                                                      
*                                                                               
CREDITX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTRUCT A GRID BASE ON X'03' ELEMENTS IN THE BUY RECORD                     
* EACH WEEK CONSIST OF 2 1-BYTE ENTRIES WITH THE FIRST ENTRY THE NUMBER         
* OF SPOTS FOR THAT WEEK, AND THE SECOND BYTE FOR VARIOUS FLAGS                 
* (ZERO OVERRIDE, ALTERNATE, ETC)                                               
* THE START OF THE GRID IS THE FIRST START DATE OF THE BUY                      
***********************************************************************         
BLDGRID  NTR1                                                                   
*                                                                               
         LA    R3,818              NO TARGET DATE                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         USING RBUYDTEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,CRSTDT)                              
         LA    R3,WORK2                                                         
*                                                                               
BGRID05  DS    0H                                                               
         ZIC   R1,RBUYDTWK                                                      
*                                                                               
BGRID10  DS    0H                                                               
         MVC   0(1,R3),RBUYDTNW                                                 
         TM    RBUYDTIN,X'01'      NPW OVERRIDE??                               
         BZ    *+8                                                              
         OI    1(R3),X'01'                                                      
         LA    R3,2(R3)            BUMP TO NEXT WEEK                            
         TM    RBUYDTIN,X'40'      ALTERNATE WEEKS??                            
         BZ    *+8                                                              
         LA    R3,2(R3)            YES, BUMP ONE MORE                           
         BCT   R1,BGRID10          PROCESS FOR SPECIFIED NUMBER OF WKS          
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   BGRIDX                                                           
*                                                                               
         MVI   CRSTDT+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,CRSTDT2)                             
         L     RE,ACOMFACS         CALL COMFACS FOR ADDRESS OF PERVAL           
         USING COMFACSD,RE                                                      
         L     RF,CPERVAL                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,(17,CRSTDT),WORK2+120                                  
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2+120                                                
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R3,WORK2                                                         
         SLL   R1,1                MULTIPLY BY 2                                
         AR    R3,R1               BUMP TO START OF NEXT X'03' ELEMENT          
         B     BGRID05             IN THE GRID                                  
*                                                                               
BGRIDX   DS    0H                                                               
         B     CREDITX                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PARSE USER INPUT DATES TO BE CREDITED. FIND START DATE, NUMBER OF             
* WEEKS, AND THE NUMBER OF SPOTS/WEEK.                                          
*                                                                               
* IF VALID, REMOVE THESE SPOTS FROM THE BUY GRID BUILT FROM ABOVE.              
*                                                                               
* USES IO4 FOR SCANNER WORK AREA.                                               
* USES IO4+1000 TO BUILD CREDIT AUDIT TRAIL ELEMENTS                            
***********************************************************************         
REMGRID  NTR1                                                                   
         L     RE,AIO4                                                          
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF                                                                   
*                                                                               
         LA    R3,803                                                           
         GOTO1 SCANNER,DMCB,WORK3,AIO4,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
*                                                                               
         L     R7,AIO4                                                          
         LA    R6,1000(R7)                                                      
CATD     USING RBUYCAEL,R6         USE X'16' CREDIT AUDIT TRAIL DSECT           
*                                  PROCESS START DATE                           
RGRID5   DS    0H                                                               
         LA    RE,12(R7)                                                        
         CLC   =C'CR=',12(R7)      SKIP CREDIT KEYWORD                          
         BNE   *+8                                                              
         LA    RE,15(R7)                                                        
         ST    RE,DMCB                                                          
         MVI   DMCB,1                                                           
         GOTO1 DATVAL,DMCB,,CRSTDT2                                             
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,TMPDATE,0,DUB                     
         MVC   CRSTDT2(2),TMPDATE     K START YEAR                              
         CLC   CRSTDT2+2(4),TMPDATE+2   BUY MMDD V K MMDD                       
         BNL   RGRID8                                                           
         CLC   CRSTDT2(2),TMPDATE2  START AND END YEARS SAME?                   
         BE    RGRIDERR                                                         
         MVC   CRSTDT2(2),TMPDATE2   USE K END YEAR                             
*                                                                               
RGRID8   DS    0H                                                               
         GOTO1 DATCON,DMCB,CRSTDT2,(3,USRSDATE)                                 
         GOTO1 DATCON,DMCB,CRSTDT2,(5,CRSTDT2)                                  
*                                                                               
         CLI   1(R7),0                                                          
         BNE   RGRID10             ANY END DATE??                               
*                                  GET NUMBER OF SPOTS TO BE CREDITED           
*                                                                               
* INPUT DATE IS IN THE FORM MMMDD(NNN) IE JAN7(3)                               
*                                                                               
         GOTO1 GETCRSPT,DMCB,12(R7)                                             
         XC    USREDATE,USREDATE                                                
         MVI   CRNUMWK,1                                                        
         B     RGRID200                                                         
*                                                                               
* CHECK WHICH FORMAT END DATE IS IN:                                            
*   MMMDD-MMMDD(NNN)                                                            
*   MMMDD-NNNW(NNN)                                                             
*   MMMDD-E(NNN)                                                                
*                                                                               
RGRID10  DS    0H                  CHECK FOR END WEEKS OPTION                   
         XC    CRFLAG,CRFLAG       CLEAR FLAGS                                  
         CLI   22(R7),C'E'         FORMAT IS MMMDD-E(NNN)                       
         BNE   RGRID50                                                          
*        CLI   23(R7),C'A'         CHECK FOR ALTERNATING WEEKS                  
*        BNE   *+8                                                              
*        OI    CRFLAG,CRFALTQ                                                   
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
         GOTO1 DATCON,DMCB,(0,WORK+6),(5,CRSTDT3)                               
         B     RGRID90                                                          
*                                                                               
RGRID50  DS    0H                                                               
         LA    R4,1                                                             
         CLI   23(R7),C'W'         WEEKS IND?                                   
         BE    RGRID60                                                          
         LA    R4,2                                                             
         CLI   24(R7),C'W'                                                      
         BNE   RGRID80                                                          
*                                                                               
RGRID60  DS    0H                                                               
         LA    R1,23(R4,R7)                                                     
*        CLI   0(R1),C'A'          CHECK FOR ALTERNATING WEEKS                  
*        BNE   *+8                                                              
*        OI    CRFLAG,CRFALTQ                                                   
*                                                                               
         LR    R1,R4                                                            
         LA    R2,22(R7)                                                        
*                                                                               
RGRID70  CLI   0(R2),C'0'          NUMERIC?                                     
         BL    RGRIDERR                                                         
         CLI   0(R2),C'9'                                                       
         BH    RGRIDERR                                                         
         LA    R2,1(R2)                                                         
         BCT   R4,RGRID70                                                       
*                                  NUMERIC WEEKS ENTERED                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,22(0,R7)                                                     
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BZ    RGRIDERR                                                         
         STC   R4,CRNUMWK                                                       
         B     RGRID90                                                          
*                                                                               
RGRID80  DS    0H                  FORMAT IS MMMDD-MMMDD(NN)                    
         GOTO1 DATVAL,DMCB,(1,22(R7)),CRSTDT3                                   
******                                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,TMPDATE,0,DUB                     
         MVC   CRSTDT3(2),TMPDATE     K START YEAR                              
         CLC   CRSTDT3+2(4),TMPDATE+2   BUY MMDD V K MMDD                       
         BNL   RGRID85                                                          
         CLC   CRSTDT3(2),TMPDATE2  START AND END YEARS SAME?                   
         BE    RGRIDERR                                                         
         MVC   CRSTDT3(2),TMPDATE2   USE K END YEAR                             
RGRID85  DS    0H                  SAVE END DATE                                
******                                                                          
         GOTO1 DATCON,DMCB,(0,CRSTDT3),(3,USREDATE)                             
         GOTO1 DATCON,DMCB,(0,CRSTDT3),(5,CRSTDT3)                              
*                                  GET NUMBER OF SPOTS TO BE CREDITED           
RGRID90  DS    0H                                                               
         GOTO1 GETCRSPT,DMCB,22(R7)                                             
*                                                                               
RGRID100 DS    0H                  GET NUMBER OF WEEKS                          
         OC    CRNUMWK,CRNUMWK                                                  
         BNZ   RGRID200                                                         
*                                                                               
         L     RE,ACOMFACS         CALL COMFACS FOR ADDRESS OF PERVAL           
         USING COMFACSD,RE                                                      
         L     RF,CPERVAL                                                       
         DROP  RE                                                               
*                                                                               
         MVI   CRSTDT2+8,C'-'                                                   
         GOTO1 (RF),DMCB,(17,CRSTDT2),WORK2+120                                 
         CLI   DMCB+4,0                                                         
         BE    RGRID150                                                         
         LA    R3,474              MUST FALL WITHIN EXISTING DATES              
         B     RGRIDERR                                                         
*                                                                               
WKD      USING PERVALD,WORK2+120                                                
RGRID150 DS    0H                                                               
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         MVC   CRNUMWK,WKD.PVALNWKS+1                                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
RGRID200 DS    0H                  REMOVE SPOTS FROM GRID                       
         L     RE,ACOMFACS         CALL COMFACS FOR ADDRESS OF PERVAL           
         USING COMFACSD,RE                                                      
         L     RF,CPERVAL                                                       
         DROP  RE                                                               
*                                                                               
         MVI   CRSTDT+8,C'-'                                                    
         GOTO1 (RF),DMCB,(17,CRSTDT),WORK2+120                                  
         CLI   DMCB+4,0                                                         
         BE    RGRID220                                                         
         LA    R3,474              MUST FALL WITHIN EXISTING DATES              
         B     RGRIDERR                                                         
*                                                                               
WKD      USING PERVALD,WORK2+120                                                
RGRID220 DS    0H                                                               
         ZICM  R4,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R4,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
RGRID300 DS    0H                                                               
*                                                                               
*******                                                                         
* BUILD AUDIT TRAIL ELEMENT                                                     
*******                                                                         
         MVI   CATD.RBUYCACD,RBUYCACQ                                           
         MVI   CATD.RBUYCALN,RBUYCALQ                                           
         LR    R2,R4                                                            
         MHI   R2,7                                                             
         GOTO1 DATVAL,DMCB,CRSTDT,TMPDATE                                       
         GOTO1 ADDAY,DMCB,TMPDATE,TMPDATE,(R2)                                  
*        GOTO1 DATCON,DMCB,(0,TMPDATE),(3,CATD.RBUYCASD)                        
         MVC   CATD.RBUYCASD,USRSDATE                                           
         MVC   CATD.RBUYCAED,USREDATE                                           
         MVC   CATD.RBUYCANW,CRNUMWK                                            
         MVC   CATD.RBUYCASP,CRNUMSPT                                           
         GOTO1 DATCON,DMCB,(5,0),(2,CATD.RBUYCACT)                              
         TM    CRFLAG,CRFALTQ                                                   
         BZ    *+8                                                              
         OI    CATD.RBUYCAFL,X'80' FLAG ALTERNATING                             
         LA    R6,RBUYCALQ(R6)     BUMP TO NEXT SLOT                            
*                                                                               
         LA    R2,WORK2                                                         
         SLL   R4,1                MULTIPLY BY 2                                
         AR    R2,R4               BUMP TO STARTING CELL FOR THIS DATE          
         ZIC   R4,CRNUMWK     NUMBER OF WEEKS TO PROCESS                        
         LA    R3,811              NO SPOTS LEFT TO BE CREDITED                 
*                                                                               
RGRID310 DS    0H                                                               
         CLC   CRNUMSPT,0(R2)                                                   
         BH    RGRIDERR                                                         
         ZIC   RE,0(R2)                                                         
         ZIC   RF,CRNUMSPT                                                      
         SR    RE,RF                                                            
         STC   RE,0(R2)                                                         
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         OI    1(R2),X'01'         IF ALL CREDITED OUT, FLAG OVERRIDE           
*                                  SO WE'LL REBUILD THIS DATE WITH (0)          
         LA    R2,2(R2)                                                         
         TM    CRFLAG,CRFALTQ                                                   
         BZ    *+8                 ALTERNATING WEEKS BUMPS ONE MORE             
         LA    R2,2(R2)                                                         
         BCT   R4,RGRID310                                                      
*                                                                               
         LA    R7,32(R7)           NEXT SET OF DATES                            
         CLI   0(R7),0                                                          
         BNE   RGRID5                                                           
*                                                                               
RGRIDX   DS    0H                                                               
         B     CREDITX                                                          
*                                                                               
RGRIDERR DS    0H                                                               
         LA    R2,BUYDTESH                                                      
         B     ERROR                                                            
         DROP  CATD                                                             
         EJECT                                                                  
***********************************************************************         
* REBUILD X'03' ELEMENT FROM BUY GRID                                           
***********************************************************************         
BLDBUY   NTR1                                                                   
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BBUY03   DS    0H                  POINT TO LAST X'02' ELEMENT TO GET           
         LR    R2,R6               END DAY                                      
         BAS   RE,NEXTEL                                                        
         BE    BBUY03                                                           
         LR    R6,R2                                                            
         USING RBUYDYEL,R6                                                      
         MVI   CRENDDAY,0     GET END DAY                                       
         MVN   CRENDDAY,RBUYDYIN                                                
         DROP  R6                                                               
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6         SAVE OFF BUY START DATE                      
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,CRSTDT)                              
         DROP  R6                                                               
*                                                                               
* DELETE OLD X'03'S                                                             
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'03',RBUYREC)                                    
*                                                                               
         LA    R6,WORK2                                                         
*                                                                               
         SR    R7,R7                                                            
*                                                                               
BBUY05   DS    0H                                                               
         LA    R4,1                                                             
         XC    CRBUYEL,CRBUYEL                                                  
WKD      USING RBUYDTEL,CRBUYEL                                                 
*                                                                               
         MVI   WKD.RBUYDTCD,X'03'                                               
         MVI   WKD.RBUYDTLN,11                                                  
*                                                                               
BBUY08   DS    0H                                                               
         CLI   0(R6),0                                                          
         BNE   BBUY10                                                           
         TM    1(R6),X'01'         OVERRIDE??                                   
         BZ    BBUY40                                                           
*                                                                               
BBUY10   DS    0H                                                               
         OC    WKD.RBUYDTST,WKD.RBUYDTST                                        
         BNZ   BBUY20              NEED TO FIND START DATE                      
         LTR   R7,R7                                                            
         BNZ   BBUY15                                                           
         MVC   WKD.RBUYDTST,CRSTDT                                              
         MVC   CRSTDT2,CRSTDT                                                   
         GOTO1 DATCON,DMCB,(0,CRSTDT),(3,WKD.RBUYDTST)                          
         B     BBUY20                                                           
*                                                                               
BBUY15   DS    0H                  SUBSEQUENT X'03'S NEED TO COMPUTE            
         LR    R2,R7               START DATES RELATIVE TO INITIAL              
         MHI   R2,7                FLIGHT START FOR THIS BUY                    
         GOTO1 ADDAY,DMCB,CRSTDT,CRSTDT2,(R2)                                   
         GOTO1 DATCON,DMCB,(0,CRSTDT2),(3,WKD.RBUYDTST)                         
*                                                                               
BBUY20   DS    0H                                                               
         CLC   0(1,R6),2(R6)                                                    
         BNE   BBUY23                                                           
         CLI   0(R6),0             INCASE ALL SPOTS CREDITED OUT FOR            
         BNE   BBUY21              THIS WEEK, CHECK IF ZERO SPOT                
         TM    3(R6),X'01'         OVERRIDE FLAG IS SET                         
         BZ    BBUY23                                                           
*                                                                               
BBUY21   DS    0H                                                               
         LA    R6,2(R6)                                                         
         LA    R4,1(R4)                                                         
         LA    R7,1(R7)                                                         
         CH    R7,=H'53'                                                        
         BL    BBUY08              BUMP TO NEXT CELL                            
*                                                                               
BBUY23   DS    0H                                                               
         MVC   WKD.RBUYDTNW,0(R6)                                               
         STC   R4,WKD.RBUYDTWK                                                  
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         OI    WKD.RBUYDTIN,X'80'  DEFAULT IS WEEKLY                            
         TM    1(R6),X'01'         OVERRIDE??                                   
         BZ    *+8                                                              
         OI    WKD.RBUYDTIN,X'01'                                               
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         CLC   RBUYNW,0(R6)                                                     
         BE    BBUY25                                                           
         OI    WKD.RBUYDTIN,X'01'                                               
***********                                                                     
***********                                                                     
*                                                                               
* CALCULATE END DATE                                                            
*                                                                               
BBUY25   DS    0H                                                               
         ZIC   R2,WKD.RBUYDTWK                                                  
         MHI   R2,7                                                             
         GOTO1 ADDAY,DMCB,CRSTDT2,CRSTDT2,(R2)                                  
*                                                                               
         GOTO1 GETDAY,DMCB,CRSTDT2,FULL                                         
         ZIC   RE,CRENDDAY                                                      
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    BBUY35                                                           
         BP    BBUY30                                                           
*                                                                               
* HANDLE OUT OF WEEK ROTATIONS                                                  
*                                                                               
         LR    R2,RE                                                            
         B     BBUY38                                                           
*                                                                               
BBUY30   DS    0H                                                               
         LA    R2,7                                                             
         LNR   R2,R2                                                            
         AR    R2,RE                                                            
         B     BBUY38                                                           
*                                                                               
BBUY35   DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    R2,7                                                             
         LNR   R2,R2                                                            
*                                                                               
BBUY38   DS    0H                                                               
         GOTO1 ADDAY,DMCB,CRSTDT2,CRSTDT2,(R2)                                  
         GOTO1 DATCON,DMCB,(0,CRSTDT2),(3,WKD.RBUYDTED)                         
*                                                                               
***********                                                                     
***********                                                                     
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RBUYREC,WKD.RBUYDTEL                               
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
BBUY40   DS    0H                                                               
         LA    R6,2(R6)                                                         
         LA    R7,1(R7)                                                         
         CH    R7,=H'53'                                                        
         BL    BBUY05              BUMP TO NEXT CELL                            
*                                                                               
BBUYX    DS    0H                                                               
         B     CREDITX                                                          
         DROP  WKD                                                              
         EJECT                                                                  
***********************************************************************         
* ADD CREDIT AUDIT TRAIL ELEMENTS TO BUY                                        
* ADD CREDIT COMMENT TO FIRST LINE OF BUY COMMENT                               
* IF FIRST LINE OF COMMENT IS A MAKEGOOD OR CREDIT (MG=/CR=) REFERENCE          
* USE THE REST OF THE LINE FOR CREDIT COMMENT                                   
* OTHERWISE THE EXISTING SECOND LINE WILL BE DELETED                            
***********************************************************************         
ADDAUDIT NTR1                                                                   
         XC    WORK2,WORK2                                                      
*                                                                               
         LA    R3,WORK2+2                                                       
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   AAUD50                                                           
*                                                                               
         USING RBUYCMEL,R6                                                      
         CLC   =C'MG=',RBUYCMNT                                                 
         BE    AAUD10                                                           
         CLC   =C'CR=',RBUYCMNT                                                 
         BNE   AAUD50                                                           
*                                                                               
AAUD10   DS    0H                                                               
         ZIC   R1,RBUYCMLN                                                      
*                                                                               
         LA    R3,RBUYCMNT                                                      
AAUD20   CLC   =C'CREDIT:',0(R3)                                                
         BE    AAUD30                                                           
         AHI   R3,1                                                             
         BCT   R1,AAUD20                                                        
         ZIC   R3,RBUYCMLN                                                      
         AHI   R3,-3                                                            
         B     AAUD40                                                           
*                                                                               
AAUD30   DS    0H                                                               
         LA    R1,RBUYCMNT                                                      
         SR    R3,R1                                                            
         AHI   R3,-2               OVERHEAD                                     
*                                                                               
AAUD40   DS    0H                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),RBUYCMNT                                              
         LA    R1,WORK2+2                                                       
         LA    R3,2(R1,R3)                                                      
*                                                                               
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),0,0                                 
         DROP  R6                  REMOVE ORIGINAL MG=/CR= ELEMENT              
*                                                                               
AAUD50   DS    0H                                                               
         MVC   0(7,R3),=C'CREDIT:'                                              
         AHI   R3,7                                                             
*                                                                               
         L     R4,AIO4                                                          
         LA    R4,1000(R4)                                                      
CATD     USING RBUYCAEL,R4         USE X'16' CREDIT AUDIT TRAIL DSECT           
*                                                                               
AAUD60   DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,CATD.RBUYCAEL                              
*                                                                               
* REDISPLAY USER INPUT END DATE: IN DATE FORMAT                                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,CATD.RBUYCASD),(4,(R3))                           
         AHI   R3,5                                                             
         OC    CATD.RBUYCAED,CATD.RBUYCAED                                      
         BZ    AAUD70                                                           
         MVI   0(R3),C'-'                                                       
         AHI   R3,1                                                             
         GOTO1 DATCON,DMCB,(3,CATD.RBUYCAED),(4,(R3))                           
         AHI   R3,5                                                             
         B     AAUD80                                                           
*                                                                               
* REDISPLAY USER INPUT END DATE: IN NUMBER OF WEEKS FORMAT                      
*                                                                               
AAUD70   DS    0H                                                               
         CLI   CATD.RBUYCANW,1                                                  
         BNH   AAUD80                                                           
         MVI   0(R3),C'-'                                                       
         AHI   R3,1                                                             
         EDIT  CATD.RBUYCANW,(3,0(R3)),ALIGN=LEFT                               
         AR    R3,R0                                                            
         MVI   0(R3),C'W'                                                       
         AHI   R3,1                                                             
         TM    CATD.RBUYCAFL,X'80' ALTERNATING WEEKS?                           
         BZ    AAUD80                                                           
         MVI   0(R3),C'A'                                                       
         AHI   R3,1                                                             
*                                                                               
AAUD80   DS    0H                                                               
         MVI   0(R3),C'('                                                       
         AHI   R3,1                                                             
         EDIT  CATD.RBUYCASP,(3,0(R3)),ALIGN=LEFT                               
         AR    R3,R0                                                            
         MVC   0(2,R3),=C'),'                                                   
         AHI   R3,2                                                             
*                                                                               
         LA    R4,RBUYCALQ(R4)                                                  
*                                                                               
         CLI   CATD.RBUYCACD,RBUYCACQ                                           
         BE    AAUD60                                                           
         AHI   R3,-1               NO MORE, REMOVE COMMA                        
         MVI   0(R3),0                                                          
         DROP  CATD                                                             
*                                                                               
         LA    R1,WORK2                                                         
         SR    R3,R1               CALCULATE ELEMENT LENGTH                     
*                                                                               
         MVI   WORK2,X'04'         INSERT BUY COMMENT ELEMENT CODE              
         STC   R3,WORK2+1          INSERT ELEMENT LENGTH                        
         CLI   WORK2+1,62                                                       
         BNH   AAUD85                                                           
         MVI   WORK2+1,62          MAX LENGTH                                   
         MVI   WORK2+61,C'>'       INSERT MORE TO FOLLOW INDICATOR              
*                                                                               
AAUD85   DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    AAUD90                                                           
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         B     AAUD120                                                          
*                                                                               
AAUD90   DS    0H                                                               
         USING RBUYCMEL,R6                                                      
         CLC   =C'CREDIT:',RBUYCMNT                                             
         BNE   AAUD100                                                          
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),0,0                                 
*                                                                               
AAUD100  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(2,RBUYREC),WORK2,(R6)                               
*                                                                               
*&&DO                                                                           
         CHI   R3,62               NEED TO WRAP TO SECOND LINE??                
         BNH   AAUD120                                                          
*                                                                               
         MVI   WORK2+60,X'04'      INSERT BUY COMMENT ELEMENT CODE              
         AHI   R3,-60              ACTUALLY 62 MINUS 2 FOR OVERHEAD             
         STC   R3,WORK2+61                                                      
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,(2,RBUYREC),WORK2+60,(R6)                            
*&&                                                                             
*                                                                               
* REDISPLAY COMMENTS SO THEY WILL BE REVALIDATED LATER IN THE COMMENT           
* VALIDATION SECTION. CLEAR ALL X'04' COMMENTS ELEMENTS HERE. THEY              
* WILL BE REBUILT LATER ON                                                      
*                                                                               
AAUD120  DS    0H                                                               
         XC    BUYCOM1,BUYCOM1                                                  
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RBUYCMEL,R6                                                      
         ZIC   R1,RBUYCMLN                                                      
         AHI   R1,-2                                                            
         STC   R1,BUYCOM1H+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUYCOM1(0),RBUYCMNT                                              
         DROP  R6                                                               
*                                                                               
         CLI   BUYCOM2H+5,0                                                     
         BE    AAUD130                                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAMKGF2,X'80'      MAKEGOOD APPLY??                             
         BO    AAUD140             YES, SECOND HAS DETAIL COMMENT               
         DROP  RF                                                               
*                                                                               
AAUD130  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   AAUD140                                                          
*                                                                               
         XC    BUYCOM2,BUYCOM2                                                  
         USING RBUYCMEL,R6                                                      
         ZIC   R1,RBUYCMLN                                                      
         AHI   R1,-2                                                            
         STC   R1,BUYCOM2H+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUYCOM2(0),RBUYCMNT                                              
         DROP  R6                                                               
*                                                                               
AAUD140  DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(4,RBUYREC)                                        
*                                                                               
         NI    BUYCOM1H+4,X'FF'-X'20'                                           
         NI    BUYCOM2H+4,X'FF'-X'20'                                           
*                                                                               
AAUDX    DS    0H                                                               
         B     CREDITX                                                          
         EJECT                                                                  
***********************************************************************         
* PARSE INPUT STRING TO FIND NUMBER OF SPOTS TO BE CREDITED                     
***********************************************************************         
GETCRSPT NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         SR    R4,R4                                                            
GCRS10   DS    0H                                                               
         LA    R1,0(R4,R2)                                                      
         CLI   0(R1),C'('                                                       
         BE    GCRS20                                                           
         LA    R4,1(R4)                                                         
         CH    R4,=H'20'                                                        
         BL    GCRS10                                                           
         LA    R3,808                                                           
         B     GCRSERR                                                          
*                                                                               
GCRS20   DS    0H                                                               
         MVI   0(R1),C' '          INSERT SEPARATOR                             
         XC    CRNUMSPT,CRNUMSPT                                                
*                                                                               
* GET SPOTS CREDITED - FORMAT IS JAN15(4) JAN15-JAN26(5) JAN5-E(6)              
*                           OR JAN15-8W(3)                                      
         LA    R3,NPWERR                                                        
         LA    R4,1(R4,R2)                                                      
         LR    RF,R4                                                            
         SR    R1,R1                                                            
*                                                                               
* CHECK NUMBER PER WEEK                                                         
*                                                                               
GCRS30   CLI   0(R4),C'0'                                                       
         BL    GCRSERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    GCRSERR                                                          
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=H'3'                                                         
         BH    GCRSERR                                                          
         CLI   0(R4),C')'                                                       
         BNE   GCRS30                                                           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'                                                       
         BH    GCRSERR                                                          
*                                                                               
         LTR   R1,R1                                                            
         BZ    GCRSERR                                                          
         STC   R1,CRNUMSPT    NUMBER OF SPOTS TO BE CREDITED                    
*                                                                               
         LA    R3,816              MUST BE SEPARATED BY A COMMA                 
         CLI   1(R4),C' '          THERE MUST NOT BE ANY INPUT AFTER            
         BE    CREDITX             THE NUMBER OF SPOTS                          
*                                                                               
GCRSERR  DS    0H                                                               
         LA    R2,BUYDTESH                                                      
         B     ERROR                                                            
         EJECT                                                                  
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF USER WANTS TO +/- WEEK(S) TO CURRENT DATES                           
* WORK3 HAS COMBINED DATE FIELDS                                                
* IE:     +FEB06-2W                                                             
*         -FEB06                                                                
* IN:     WORK3 HAS COMBINED BUYDTES AND BUYDTE2 HEADER FIELDS                  
* DURING: WORK        CONTRACT START DATE                                       
*         WORK+6      CONTRACT END DATE                                         
*         WORK+12     USER INPUT START DATE                                     
*         WORK+18     CALCULATED INPUT END DATE                                 
* OUT:    WORK3 WILL HAVE NEW DATES INCORPORATED WITH CURRENT BUCKETS           
***********************************************************************         
DTADDSUB CSECT                                                                  
         NMOD1 0,*DTADSU*                                                       
         L     RC,0(R1)                                                         
                                                                                
         MVC   DMCB+8,=C',=+-'                                                  
         CLI   WORK3+8,C'+'        MUST START WITH +/-                          
         BE    DTAS10                                                           
         MVC   DMCB+8,=C',=*-'     INTERNALLY CHANGE MINUS(-) TO *              
         CLI   WORK3+8,C'-'        TO DISTINGUISH IT FROM A DASH '-'            
         BNE   DTASX               ELSE EXIT                                    
         MVI   WORK3+8,C'*'                                                     
*                                                                               
* CALCULATE START AND END DATES                                                 
*                                                                               
DTAS10   DS    0H                  CHECK PRESENT OF ANY COMMAS ','              
         LA    R3,491              ONLY SINGLE +/- ENTRY ALLOWED                
         LA    RF,WORK3            IT MEANS USER TRIED TO ENTER                 
         LA    RE,L'BUYDTES+L'BUYDTE2                                           
DTAS15   CLI   0(RF),C','          MULTIPLE +/- DATES                           
         BE    ERROR               AND IT IS NOT SUPPORTED                      
         LA    RF,1(RF)                                                         
         BCT   RE,DTAS15                                                        
                                                                                
         LA    R3,INVINP                                                        
         GOTO1 SCANNER,DMCB,WORK3,(3,WORK2)                                     
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
*                                                                               
WKD      USING DTASD,WORK                                                       
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WKD.KSTADATE,0,DUB                
*                                  VALIDATE INPUT DATE                          
         GOTO1 DATVAL,DMCB,(1,WORK2+44),WKD.ISTADATE                            
                                                                                
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
         LA    R5,WORK2+44         POINT PASS END OF DATE FIELD                 
         LA    R5,0(R7,R5)                                                      
*                                  START YEAR                                   
         MVC   WKD.ISTADATE(2),WKD.KSTADATE                                     
*                                  BUY MMDD V K MMDD                            
         CLC   WKD.ISTADATE+2(4),WKD.KSTADATE+2                                 
         BNL   DTAS20                                                           
*                                  BUY MMDD LOW                                 
*                                  K START AND END YEARS SAME?                  
         CLC   WKD.KSTADATE(2),WKD.KENDDATE                                     
         BE    ERROR                                                            
*                                  USE K END YEAR                               
         MVC   WKD.ISTADATE(2),WKD.KENDDATE                                     
                                                                                
DTAS20   DS    0H                  VALIDATE END DATE                            
         GOTO1 DATVAL,DMCB,(1,WORK2+54),WKD.IENDDATE                            
                                                                                
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    DTAS25                                                           
         LA    R5,WORK2+54         POINT PASS END OF DATE FIELD                 
         LA    R5,0(R7,R5)                                                      
*                                  GET DAY SPAN FOR WEEK                        
         SR    R1,R1               GET START AND END DAYS                       
         ZIC   R0,RBUYSTED                                                      
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         STM   R0,R1,DMCB+16                                                    
         B     DTAS65                                                           
                                                                                
DTAS25   DS    0H                                                               
         MVC   WKD.TMP1DATE,WKD.ISTADATE                                        
         XC    FULL,FULL           ANY WEEKS? EX: -3W                           
         CLI   WORK2+54,C' '                                                    
         BE    DTAS60                                                           
*                                  CHECK FOR END WEEKS OPTION                   
         LA    R3,EDTERR                                                        
         LA    R5,WORK2+54                                                      
         LA    R4,1                                                             
         CLI   1(R5),C'W'          WEEKS IND?                                   
         BE    DTAS30                                                           
         LA    R4,2                                                             
         CLI   2(R5),C'W'                                                       
         BE    *+8                                                              
         B     ERROR                                                            
*                                  W HAS BEEN ENTERED - PACK WEEKS              
DTAS30   LR    R7,R5                                                            
         LA    R5,1(R4,R5)         END OF FIELD                                 
         LR    R6,R4                                                            
*                                                                               
DTAS40   CLI   0(R7),C'0'          NUMERIC?                                     
         BNL   *+8                                                              
         B     ERROR                                                            
         CLI   0(R7),C'9'                                                       
         BNH   *+8                                                              
         B     ERROR                                                            
         LA    R7,1(R7)                                                         
         BCT   R4,DTAS40                                                        
*                                  NUMERIC WEEKS ENTERED                        
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,WORK2+54(0)                                                  
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         B     ERROR                                                            
         LA    R3,7                                                             
         BCTR  R4,0                NUMBER OF WEEKS                              
         LTR   R4,R4                                                            
         BZ    DTAS60                                                           
*                                  TEST FOR ALTERNATE WEEKS                     
         CLI   0(R5),C'A'                                                       
         BNE   DTAS50                                                           
         LA    R3,14                                                            
*                                  GET NEXT WEEK                                
DTAS50   GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.IENDDATE,(R3)                        
         MVC   WKD.TMP1DATE,WKD.IENDDATE                                        
         BCT   R4,DTAS50           GET NUMBER OF WEEKS-1                        
*                                  GET DAY SPAN FOR WEEK                        
DTAS60   DS    0H                                                               
         SR    R1,R1               GET START AND END DAYS                       
         ZIC   R0,RBUYSTED                                                      
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         STM   R0,R1,DMCB+16                                                    
                                                                                
         L     R6,DMCB+20          END DAY OF WEEK                              
         C     R6,DMCB+16          END V START DAY                              
         BNL   *+8                                                              
         LA    R6,7(R6)            OUT OF WEEK ROTATOR                          
         S     R6,DMCB+16          GET DAY SPAN                                 
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.IENDDATE,(R6)                        
*                                  END DATE IS VALID MONTH-DAY                  
DTAS65   DS    0H                  K END YEAR                                   
         MVC   WKD.IENDDATE(2),WKD.KENDDATE                                     
*                                  BUY END MMDD V K END MMDD                    
         CLC   WKD.IENDDATE+2(4),WKD.KENDDATE+2                                 
         BNH   *+10                                                             
*                                  MOVE K START YEAR                            
         MVC   WKD.IENDDATE(2),WKD.KSTADATE                                     
*                                                                               
*                                  VALIDATE END DATE                            
         GOTO1 GETDAY,DMCB,WKD.IENDDATE,FULL                                    
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
*                                                                               
         ZIC   R4,DMCB             DAY OF WEEK                                  
*                                                                               
         C     R4,DMCB+20          SAME DAY AS END DAY?                         
         BE    DTAS70                                                           
         LA    R3,EDYERR                                                        
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* CHECK IF ANY SPOT OVERRIDE AND SAVE IN FULL                                   
*                                                                               
DTAS70   DS    0H                                                               
* SEE IF NO. PER WEEK GIVEN                                                     
         XC    FULL,FULL                                                        
         LA    RE,0(R5)                                                         
         CLI   0(R5),C'A'                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CLI   0(RE),C'('                                                       
         BNE   DTAS80                                                           
*                                                                               
* GET NPW OVERRIDE - FORMAT IS JAN15(4) JAN15-JAN26(5) JAN5-E(6)                
*                           OR JAN15-8W(3)                                      
         LA    R3,NPWERR                                                        
         SR    R1,R1                                                            
         LA    RE,1(RE)                                                         
         LR    RF,RE                                                            
* CHECK NUMBER PER WEEK                                                         
DTAS75   CLI   0(RE),C'0'                                                       
         BL    ERROR                                                            
         CLI   0(RE),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=H'3'                                                         
         BH    ERROR                                                            
         CLI   0(RE),C')'                                                       
         BNE   DTAS75                                                           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'                                                       
         BH    ERROR                                                            
*                                                                               
         STC   R1,FULL             NPW                                          
*                                                                               
DTAS80   DS    0H                                                               
         XC    WORK3,WORK3                                                      
WK3D     USING RBUYDTEL,WORK3                                                   
*                                  CONVERT START DATE TO BINARY                 
         GOTO1 DATCON,DMCB,(0,WKD.ISTADATE),(3,WK3D.RBUYDTST)                   
*                                  CONVERT END DATE TO BINARY                   
         GOTO1 DATCON,DMCB,(0,WKD.IENDDATE),(3,WK3D.RBUYDTED)                   
                                                                                
*                                  COMPARE DATES WITH EXISING BUCKETS           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         USING RBUYDTEL,R6                                                      
         LA    R3,INVINP                                                        
         EJECT                                                                  
***********************************************************************         
* PROCESS FOR '-' DATES                                                         
***********************************************************************         
DTAS90   DS    0H                                                               
         CLI   BUYDTES,C'-'                                                     
         BNE   DTAS400                                                          
*                                                                               
* INPUT DATES MUST BE WITHIN EXISTING BUCKETED DATES                            
*                                                                               
DTAS100  DS    0H                                                               
         LA    R3,474                                                           
         CLC   WK3D.RBUYDTST,RBUYDTST EXISTING DATES                            
         BL    ERROR                                                            
DTAS110  CLC   WK3D.RBUYDTED,RBUYDTED                                           
         BNH   DTAS120                                                          
         BAS   RE,NEXTEL                                                        
         BE    DTAS100                                                          
         B     ERROR               NO 'REMOVABLE' DATES FOUND                   
                                                                                
DTAS120  DS    0H                  X'03' ELEMENT FOUND                          
*                                                                               
* CANNOT MINGLE WEEKLY AND ALTERNATES EXCEPT IF INPUT DATES ARE 1 WEEK          
*                                                                               
         LA    R3,475                                                           
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BE    DTAS130                                                          
*                                                                               
* NO, DO NOT HAVE TO SAY -5/14A IF SINGLE DATE                                  
*                                                                               
         MVC   WKD.TMP1DATE,WKD.ISTADATE                                        
         LA    R7,7                                                             
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.TMP1DATE,(R7)                        
         CLC   WKD.TMP1DATE,WKD.IENDDATE                                        
         BNH   DTAS142             SINGLE DATE?                                 
         TM    RBUYDTIN,X'80'      YES, MUST BE EVERY WEEK                      
         BO    DTAS142                                                          
                                                                                
DTAS130  DS    0H                                                               
         TM    RBUYDTIN,X'40'      YES, MUST BE ALT WEEKS                       
         BZ    ERROR                                                            
                                                                                
DTAS140  DS    0H                                                               
*                                                                               
* ALTERNATE WEEK CHECK                                                          
* NEW + DATES MUST ALTERNATE IN SYNC WITH EXISTING BUCKETS                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WKD.TMP1DATE)                        
         LA    R7,14                                                            
DTAS141  DS    0H                                                               
         CLC   WKD.TMP1DATE,WKD.ISTADATE                                        
         BNL   DTAS141A                                                         
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.TMP1DATE,(R7)                        
         B     DTAS141                                                          
                                                                                
DTAS141A DS    0H                                                               
         LA    R3,474                                                           
         CLC   WKD.TMP1DATE,WKD.ISTADATE                                        
         BNE   ERROR                                                            
*                                                                               
DTAS142  DS    0H                                                               
         LA    R3,475                                                           
         CLC   WK3D.RBUYDTST,RBUYDTST                                           
         BNE   DTAS190                                                          
         CLC   WK3D.RBUYDTED,RBUYDTED                                           
         BNE   DTAS170             CHECK IF EXACT START/END DATES               
         OC    FULL,FULL           YES, MUST BE SPOT OVERRIDES ONLY             
         BNZ   DTAS145                                                          
*                                                                               
* FOR -, IF NO SPOT OVERRIDE, DELETE BUCKET                                     
*                                                                               
         CLI   BUYDTES,C'-'                                                     
         BNE   ERROR                                                            
         TM    RBUYDTIN,X'01'      IF EXISTING BUCKET HAS SPOT OVERRIDE         
         BZ    DTAS143             DON'T DELETE EXISTING BUCKET                 
         ZIC   RF,RBUYDTNW                                                      
         ZIC   RE,RBUYNW                                                        
         B     DTAS150                                                          
                                                                                
DTAS143  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),(R6)                                
         B     DTAS800                                                          
*                                                                               
DTAS145  DS    0H                                                               
         ZIC   RF,RBUYDTNW                                                      
         ZIC   RE,FULL                                                          
         CLI   BUYDTES,C'-'                                                     
         BE    DTAS150                                                          
         AR    RF,RE                                                            
         B     DTAS160                                                          
                                                                                
DTAS150  DS    0H                                                               
         LA    R3,476                                                           
         SR    RF,RE                                                            
         BM    ERROR                                                            
                                                                                
DTAS160  DS    0H                                                               
         STC   RF,RBUYDTNW                                                      
         B     DTAS800                                                          
*                                                                               
* INPUT DATES CAN EITHER MATCH THE START DATE, THE END DATE OR BE               
* EMBEDDED WITHIN A BUCKETED DATE.                                              
*                                                                               
* CASE 1: ONLY START DATE MATCH, CALCULATE NEW START DATE                       
*                                                                               
DTAS170  DS    0H                                                               
         MVC   WKD.TMP1DATE,WKD.ISTADATE                                        
         LA    R7,7                                                             
         TM    RBUYDTIN,X'40'      ALTERNATING WEEKS?                           
         BZ    DTAS180                                                          
         LA    R7,14                                                            
DTAS180  DS    0H                  PUSH FORWARD START DATE                      
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.TMP1DATE,(R7)                        
         CLC   WKD.TMP1DATE,WKD.IENDDATE                                        
         BNH   DTAS180                                                          
         GOTO1 DATCON,DMCB,(0,WKD.TMP1DATE),(3,RBUYDTST)                        
         B     DTAS240                                                          
                                                                                
DTAS190  DS    0H                  OR REMOVE FROM END?                          
         CLC   WK3D.RBUYDTED,RBUYDTED                                           
         BNE   DTAS210                                                          
*                                                                               
* CASE 2: ONLY END DATE MATCH, CALCULATE NEW END DATE                           
*                                                                               
         MVC   WKD.TMP1DATE,WKD.IENDDATE                                        
         LA    R7,7                                                             
         TM    RBUYDTIN,X'40'      ALTERNATING WEEKS?                           
         BZ    *+8                                                              
         LA    R7,14                                                            
         LNR   R7,R7                                                            
DTAS200  DS    0H                  BACK UP END DATE                             
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.TMP1DATE,(R7)                        
         CLC   WKD.TMP1DATE,WKD.ISTADATE                                        
         BNL   DTAS200                                                          
         GOTO1 DATCON,DMCB,(0,WKD.TMP1DATE),(3,RBUYDTED)                        
         B     DTAS240                                                          
*                                                                               
* CASE 3: DATES ARE EMBEDDED                                                    
*                                                                               
* CURRENT X'03' ELEMENT WILL BE SPLIT IN TO TWO SINCE THE DATES                 
* TO BE REMOVED ARE EMBEDDED                                                    
*                                                                               
DTAS210  DS    0H                  KEEP OLD ELEMENT WITH NEW END DATE           
         MVC   WORK3(11),RBUYDTEL  MAKE A COPY OF EXISTING DATES                
         MVC   WKD.CENDDATE,WKD.IENDDATE                                        
         LA    R7,7                                                             
         TM    RBUYDTIN,X'40'      ALTERNATING WEEKS?                           
         BZ    *+8                                                              
         LA    R7,14                                                            
         LNR   R7,R7                                                            
DTAS220  DS    0H                                                               
         GOTO1 ADDAY,DMCB,WKD.CENDDATE,WKD.CENDDATE,(R7)                        
         CLC   WKD.CENDDATE,WKD.ISTADATE                                        
         BNL   DTAS220                                                          
         GOTO1 DATCON,DMCB,(0,WKD.CENDDATE),(3,RBUYDTED)                        
*                                                                               
* BUILD NEW ELEMENT WITH NEW START DATE                                         
*                                                                               
         MVC   WKD.CSTADATE,WKD.ISTADATE                                        
         LA    R7,7                                                             
         TM    RBUYDTIN,X'40'      ALTERNATING WEEKS?                           
         BZ    *+8                                                              
         LA    R7,14                                                            
DTAS230  DS    0H                                                               
         GOTO1 ADDAY,DMCB,WKD.CSTADATE,WKD.CSTADATE,(R7)                        
         CLC   WKD.CSTADATE,WKD.IENDDATE                                        
         BNH   DTAS230                                                          
         GOTO1 DATCON,DMCB,(0,WKD.CSTADATE),(3,WK3D.RBUYDTST)                   
         GOTO1 MYADELM,DMCB,RBUYREC,WORK3                                       
                                                                                
DTAS240  DS    0H                                                               
         OC    FULL,FULL           CHECK IF SPOT OVERRIDE                       
         BZ    DTAS800                                                          
         B     DTAS520                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS FOR '+' DATES                                                         
***********************************************************************         
DTAS400  DS    0H                  IF '+', DATES CANNOT OVERLAP                 
         SR    R4,R4               UNLESS SPOT OVERRIDE IS SPECIFIED            
         LA    R3,477                                                           
                                                                                
DTAS405  DS    0H                                                               
         CLC   WK3D.RBUYDTED,RBUYDTST EXISTING DATES                            
         BNL   DTAS408                                                          
         OC    FULL,FULL                                                        
         BZ    DTAS420                                                          
         OI    FULL+1,X'80'        ADDING OVERRIDE TO (0) SPOT WEEK(S)          
         B     DTAS520                                                          
                                                                                
DTAS408  DS    0H                                                               
         CLC   WK3D.RBUYDTST,RBUYDTED                                           
         BNH   DTAS410                                                          
         LR    R4,R6                                                            
         BAS   RE,NEXTEL           CHECK ALL BUCKETS TO MAKE SURE               
         BE    DTAS405             NONE OF THEM OVERLAP                         
         LR    R6,R4               LOAD LAST ELEMENT ADDRESS                    
         OC    FULL,FULL                                                        
         BZ    DTAS470                                                          
         OI    FULL+1,X'80'        ADDING OVERRIDE TO (0) SPOT WEEK(S)          
         B     DTAS520                                                          
                                                                                
DTAS410  DS    0H                                                               
         OC    FULL,FULL           ANY SPOT OVERRIDE?                           
         BZ    ERROR               IF YES, INPUT DATES MUST BE WITHIN           
         LA    R3,474              EXISTING DATES                               
         CLC   WK3D.RBUYDTST,RBUYDTST                                           
         BL    ERROR                                                            
         CLC   WK3D.RBUYDTED,RBUYDTED                                           
         BH    ERROR                                                            
         B     DTAS120                                                          
*                                                                               
* INPUT DATES CAN BE EARLIER DATES OR LATER DATE.,                              
*                                                                               
* CASE 1:ADDING EARLIER DATES                                                   
* CHECK IF WE NEED TO REPLACE JUST THE START DATE                               
* ALSO CHECK IF WE CAN COMPRESS WITH A PREVIOUS BUCKET, IF IT EXISTS            
*                                                                               
DTAS420  DS    0H                  ADDING EARLIER DATES                         
         OC    FULL,FULL           NEED TO ADD NEW X'03' IF OVERRIDE            
         BNZ   DTAS520                                                          
                                                                                
         MVC   WKD.CENDDATE,WKD.IENDDATE                                        
         LA    R7,7                                                             
         CLI   0(R5),C'A'          ALTERNATING WEEKS?                           
         BNE   *+8                                                              
         LA    R7,14                                                            
         GOTO1 ADDAY,DMCB,WKD.CENDDATE,WKD.CENDDATE,(R7)                        
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WKD.CSTADATE)                        
         CLC   WKD.CENDDATE,WKD.CSTADATE                                        
         BNL   DTAS425                                                          
*                                                                               
* DO NOT HAVE TO SPECIFY ALTERNATE (IE:+5/14A <-> +5/14) IF SINGLE WEEK         
*                                                                               
         MVC   WKD.TMP1DATE,WKD.ISTADATE                                        
         LA    R7,7                CHECK IF SINGLE WEEK                         
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.TMP1DATE,(R7)                        
         CLC   WKD.TMP1DATE,WKD.IENDDATE                                        
         BNH   DTAS520             NOT SINGLE, NEED TO ADD NEW ELEMENT          
         TM    RBUYDTIN,X'40'      YES, MUST BE ALT WEEKS                       
         BZ    DTAS520             NO, ADD NEW ELEMENT                          
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'A'          WILL FORCE 'A' AUTOMATICALLY                 
                                                                                
DTAS425  DS    0H                                                               
         TM    RBUYDTIN,X'40'      CHECK IF EXISTING BUCKET IS ALT WKS          
         BZ    DTAS440             NO? NORMAL PROCESSING                        
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DTAS440                                                          
*                                                                               
* ALTERNATE WEEK CHECK                                                          
* NEW + DATES MUST ALTERNATE IN SYNC WITH EXISTING BUCKETS                      
*                                                                               
         MVC   WKD.TMP1DATE,WKD.ISTADATE                                        
         LA    R7,14                                                            
DTAS430  DS    0H                                                               
         CLC   WKD.TMP1DATE,WKD.IENDDATE                                        
         BH    DTAS435                                                          
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.TMP1DATE,(R7)                        
         B     DTAS430                                                          
                                                                                
DTAS435  DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WKD.TMP2DATE)                        
         CLC   WKD.TMP1DATE,WKD.TMP2DATE                                        
         BNE   DTAS520                                                          
*                                                                               
* CANNOT MINGLE WEEKLY AND ALTERNATES                                           
*                                                                               
DTAS440  DS    0H                                                               
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BE    DTAS450                                                          
         TM    RBUYDTIN,X'80'      NO, MUST BE EVERY WEEK                       
         BZ    DTAS520             GO AND ADD NEW ELEMENT                       
         B     DTAS460                                                          
                                                                                
DTAS450  DS    0H                                                               
         TM    RBUYDTIN,X'40'      YES, MUST BE ALT WEEKS                       
         BZ    DTAS520             GO AND ADD NEW ELEMENT                       
                                                                                
DTAS460  DS    0H                                                               
         MVC   RBUYDTST,WK3D.RBUYDTST                                           
         B     DTAS590                                                          
*                                                                               
* CASE 2:ADDING LATER DATES                                                     
* CHECK IF WE NEED TO REPLACE ONLY THE END DATES                                
* ALSO CHECK IF WE MAY COMPRESS WITH NEXT BUCKET ELEMENT                        
*                                                                               
DTAS470  DS    0H                  ADDING LATER DATES                           
         OC    FULL,FULL           NEED TO ADD NEW X'03' IF OVERRIDE            
         BNZ   DTAS520                                                          
         LA    R7,7                GO BACK A WEEK                               
         CLI   0(R5),C'A'          ALTERNATING WEEKS?                           
         BNE   *+8                                                              
         LA    R7,14                                                            
         LNR   R7,R7                                                            
         MVC   WKD.CSTADATE,WKD.ISTADATE                                        
         GOTO1 ADDAY,DMCB,WKD.CSTADATE,WKD.CSTADATE,(R7)                        
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WKD.CENDDATE)                        
         CLC   WKD.CSTADATE,WKD.CENDDATE                                        
         BH    DTAS480             CHECK NEXT ELEMENT                           
         TM    RBUYDTIN,X'40'      CHECK IF EXISTING BUCKET IS ALT WKS          
         BZ    DTAS490             NO? NORMAL PROCESSING                        
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DTAS490                                                          
*                                                                               
* ALTERNATE WEEK CHECK                                                          
* NEW + DATES MUST ALTERNATE IN SYNC WITH EXISTING BUCKETS                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WKD.TMP1DATE)                        
         XC    HALF,HALF                                                        
         MVC   HALF+1,RBUYDTWK                                                  
         LA    R7,14                                                            
         MH    R7,HALF                                                          
         GOTO1 ADDAY,DMCB,WKD.TMP1DATE,WKD.TMP1DATE,(R7)                        
         CLC   WKD.TMP1DATE,WKD.ISTADATE                                        
         BE    DTAS490                                                          
                                                                                
DTAS480  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DTAS400                                                          
         B     DTAS520                                                          
*                                                                               
* CANNOT MINGLE WEEKLY AND ALTERNATES                                           
*                                                                               
DTAS490  DS    0H                                                               
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BE    DTAS500                                                          
         TM    RBUYDTIN,X'80'      NO, MUST BE EVERY WEEK                       
         BZ    DTAS520             GO AND ADD NEW ELEMENT                       
         B     DTAS510                                                          
                                                                                
DTAS500  DS    0H                                                               
         TM    RBUYDTIN,X'40'      YES, MUST BE ALT WEEKS                       
         BZ    DTAS520             GO AND ADD NEW ELEMENT                       
                                                                                
DTAS510  DS    0H                                                               
         MVC   RBUYDTED,WK3D.RBUYDTED                                           
         MVC   WK3D.RBUYDTST,RBUYDTST                                           
*                                                                               
* NEED TO CHECK IF WE CAN COMPRESS THIS ELEMENT WITH THE NEXT                   
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DTAS800                                                          
         GOTO1 DATCON,DMCB,(3,WK3D.RBUYDTED),(0,WKD.CENDDATE)                   
         LA    R7,7                                                             
         CLI   0(R5),C'A'                                                       
         BNE   *+8                                                              
         LA    R7,14                                                            
         GOTO1 ADDAY,DMCB,WKD.CENDDATE,WKD.CENDDATE,(R7)                        
         GOTO1 DATCON,DMCB,(0,WKD.CENDDATE),(3,WK3D.RBUYDTED)                   
         CLC   WK3D.RBUYDTED,RBUYDTST                                           
         BL    DTAS800             NO COMPRESSION                               
         CLC   WK3D.RBUYDTIN,RBUYDTIN                                           
         BNE   DTAS800             NO COMPRESSION                               
         CLC   WK3D.RBUYDTNW,RBUYDTNW                                           
         BNE   DTAS800             NO COMPRESSION                               
*                                                                               
* YES, TAKE START DATE FROM PREVIOUS ELT AND DELETE THE PREVIOUS ELT            
*                                                                               
         MVC   RBUYDTST,WK3D.RBUYDTST                                           
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),(R6)                                
         B     DTAS800                                                          
         EJECT                                                                  
*                                                                               
* ADD NEW X'03' ELEMENT                                                         
*                                                                               
DTAS520  DS    0H                                                               
         MVC   WK3D.RBUYDTEL(2),=X'030B'                                        
         MVI   WK3D.RBUYDTIN,X'80' DEFAULT WEEKLY                               
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   *+8                                                              
         MVI   WK3D.RBUYDTIN,X'40'                                              
                                                                                
         OC    FULL,FULL           SPOT OVERRIDE?                               
         BNZ   DTAS530                                                          
         MVC   WK3D.RBUYDTNW,RBUYNW                                             
         B     DTAS560                                                          
                                                                                
DTAS530  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WKD.ISTADATE),(3,WK3D.RBUYDTST)                   
         GOTO1 DATCON,DMCB,(0,WKD.IENDDATE),(3,WK3D.RBUYDTED)                   
         OI    WK3D.RBUYDTIN,X'01' SET OVERRIDE FLAG                            
                                                                                
         ZIC   RF,RBUYNW                                                        
         ZIC   RE,FULL                                                          
         TM    RBUYDTIN,X'01'      IF EXISTING BUCKET HAS SPOT OVERRIDE         
         BZ    DTAS535             USE OVERRIDE NUMBER OF SPOTS                 
         ZIC   RF,RBUYDTNW                                                      
                                                                                
DTAS535  DS    0H                                                               
         CLI   BUYDTES,C'-'                                                     
         BE    DTAS540                                                          
         TM    FULL+1,X'80'        CHECK IF WE ARE ADDING A BRAND NEW           
         BZ    *+6                 PERIOD WITH OVERRIDE                         
         SR    RF,RF               YES, DON'T ADD TO CURRENT SPOT/WK            
         AR    RF,RE                                                            
         B     DTAS550                                                          
                                                                                
DTAS540  DS    0H                  INVALID IF TOTAL NPW IS NEGATIVE             
         LA    R3,476                                                           
         SR    RF,RE                                                            
         BM    ERROR                                                            
                                                                                
DTAS550  DS    0H                                                               
         STC   RF,WK3D.RBUYDTNW                                                 
                                                                                
DTAS560  DS    0H                                                               
         SR    R7,R7               COUNT NUMBER OF WEEKS                        
         LA    R3,7                                                             
         TM    WK3D.RBUYDTIN,X'40' ALT?                                         
         BZ    *+8                                                              
         LA    R3,14                                                            
DTAS570  LA    R7,1(R7)                                                         
         GOTO1 ADDAY,DMCB,WKD.ISTADATE,WKD.ISTADATE,(R3)                        
         CLC   WKD.ISTADATE,WKD.IENDDATE                                        
         BNH   DTAS570                                                          
*                                                                               
         STC   R7,WK3D.RBUYDTWK                                                 
*                                                                               
DTAS580  DS    0H                                                               
         GOTO1 MYADELM,DMCB,RBUYREC,WORK3                                       
*                                                                               
* NEED TO CHECK IF WE CAN COMPRESS THIS ELEMENT WITH THE PREVIOUS               
*                                                                               
DTAS590  DS    0H                                                               
         LTR   R4,R4               ADDRESS OF PREVIOUS ELEMENT PRESENT?         
         BZ    DTAS800                                                          
         CLI   BUYDTES,C'+'                                                     
         BNE   DTAS800                                                          
                                                                                
         LR    RF,R6                                                            
         LR    R6,R4               R6 NOW HAS 'PREVIOUS' ELEMENT                
         LR    R4,RF               AND R4 HAS 'CURRENT' ELEMENT                 
         MVC   WK3D.RBUYDTEL(11),0(R4)                                          
         CLC   WK3D.RBUYDTIN,RBUYDTIN                                           
         BNE   DTAS800             NO COMPRESSION                               
         CLC   WK3D.RBUYDTNW,RBUYDTNW                                           
         BNE   DTAS800             NO COMPRESSION                               
                                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WKD.CSTADATE)                        
         XC    HALF,HALF                                                        
         MVC   HALF+1,RBUYDTWK                                                  
         LA    R7,7                                                             
         CLI   0(R5),C'A'                                                       
         BNE   *+8                                                              
         LA    R7,14                                                            
         MH    R7,HALF                                                          
         GOTO1 ADDAY,DMCB,WKD.CSTADATE,WKD.CSTADATE,(R7)                        
         GOTO1 DATCON,DMCB,(3,WK3D.RBUYDTST),(0,WKD.TMP1DATE)                   
         CLC   WKD.CSTADATE,WKD.TMP1DATE                                        
         BNE   DTAS800             NO COMPRESSION                               
*                                                                               
* YES, TAKE END DATE FROM CURRENT ELT AND DELETE THE CURRENT ELT                
*                                                                               
         MVC   RBUYDTED,WK3D.RBUYDTED                                           
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R4),(R4)                                
         DROP  WK3D                                                             
         DROP  WKD                                                              
         EJECT                                                                  
*                                                                               
DTAS800  DS    0H                                                               
*                                                                               
         LA    R3,478                                                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         LA    R7,WORK2            OUTPUT                                       
         MVI   WORK2,C' '                                                       
*                                                                               
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R5,WORK2+60         OUTPUT END                                   
*                                                                               
         LA    R4,WORK2+120                                                     
DTAS810  LA    R3,WORK+20                                                       
*              DISPLAY DATES                                                    
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R3))     START DATE                    
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R3,5(R3)                                                         
         B     DTAS820                                                          
*                                                                               
         MVI   5(R3),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R3))      END DATE                      
*                                                                               
         LA    R3,11(R3)                                                        
*                                                                               
DTAS820  TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
* DISPLAY NPW IF NOT = RBUYNW                                                   
         CLC   RBUYNW,9(R6)                                                     
         BE    DTAS830                                                          
         MVI   0(R3),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R3)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R3),C'0'                                                       
         LA    R3,2(R3)                                                         
         MVI   2(R3),C' '                                                       
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
DTAS830  LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM DISPLAY LEN                         
         LR    RF,R7                                                            
         AR    RF,R3               OUTPUT PTR                                   
* CHECK IF ROOM IN FIRST LINE                                                   
         CR    RF,R5               WORK2+60                                     
         BNH   DTAS840                                                          
* FIRST LINE EXCEEDED - START AT BUYDTE2                                        
         LA    R5,500(R5)          ELIM. FIRST TEST                             
         LA    R7,WORK2+60         START 2D LINE                                
         CLI   WORK2+60,C'*'                                                    
         BNE   DTAS850                                                          
         LA    R7,1(R7)                                                         
         B     DTAS850                                                          
*                                                                               
DTAS840  CR    RF,R4               WORK2+120                                    
         BH    DTAS860             DOESN'T FIT                                  
*                                                                               
DTAS850  BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK+20                                                  
         MVC   WORK+20(20),MYSPACES                                             
         LA    R7,1(R3,R7)         OUTPUT PTR                                   
         BAS   RE,NEXTEL                                                        
         BNE   DTAS910                                                          
*                                                                               
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         B     DTAS810                                                          
* DATES DON'T FIT - TRY TO COMPRESS - DISPLAY WEEKS AS END DATE                 
DTAS860  LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R3,WORK2                                                         
*                                                                               
DTAS870  GOTO1 DATCON,(R1),(3,2(R6)),(4,(R3))                                   
         LA    R7,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   DTAS880                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  R7,R0                                                            
DTAS880  AR    R3,R7                                                            
* TEST 1 WEEK                                                                   
         CLI   10(R6),1            1 WEEK?                                      
         BE    DTAS890                                                          
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
* DISPLAY WEEKS                                                                 
         EDIT  (1,10(R6)),(2,(R3)),ALIGN=LEFT                                   
         CLI   10(R6),9                                                         
         BH    *+16                                                             
         MVI   1(R3),C'W'                                                       
         LA    R3,2(R3)                                                         
         B     *+12                                                             
         MVI   2(R3),C'W'                                                       
         LA    R3,3(R3)                                                         
         TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
* NUMBER PER WEEK                                                               
DTAS890  CLC   RBUYNW,9(R6)        SAME FOR BUY?                                
         BE    DTAS900                                                          
* DISPLAY NPW                                                                   
         MVI   0(R3),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R3)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R3),C'0'                                                       
         LA    R3,2(R3)                                                         
         MVI   2(R3),C' '                                                       
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
* GET NEXT ELEM                                                                 
DTAS900  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DTAS910                                                          
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         B     DTAS870                                                          
DTAS910  MVC   BUYDTES,WORK2       DATES                                        
         CLI   WORK2+60,C' '                                                    
         BE    *+10                                                             
         MVC   BUYDTE2,WORK2+60    MOVE 2D LINE                                 
                                                                                
         OI    BUYDTESH+6,X'80'    XMIT FIELD                                   
         OI    BUYDTE2H+6,X'80'    XMIT FIELD                                   
*                                                                               
* REBUILD WORK3 AS COMBINED BUYDTESH AND BUYDTE2 HEADER FIELDS                  
*                                                                               
         MVC   WORK3(8),BUYDTESH                                                
         MVC   WORK3+8(60),WORK2                                                
         LA    RE,WORK3+59                                                      
         LA    RF,60               LENGTH OF 1 INPUT DATE LINE                  
                                                                                
DTAS920  DS    0H                  FIND FIRST NON-BLANK CHAR                    
         CLI   0(RE),C' '                                                       
         BNE   DTAS930                                                          
         BCTR  RE,0                                                             
         BCT   RF,DTAS920                                                       
*                                                                               
DTAS930  DS    0H                  SECOND LINE                                  
         MVC   1(60,RE),WORK2+60                                                
         LA    RE,60(RE)                                                        
         LA    RF,60(RF)           LENGTH OF 1 INPUT DATE LINE                  
DTAS935  DS    0H                  FIND FIRST NON-BLANK CHAR                    
         CLI   0(RE),C' '                                                       
         BNE   DTAS940                                                          
         BCTR  RE,0                                                             
         BCT   RF,DTAS935                                                       
                                                                                
DTAS940  DS    0H                                                               
         STC   RF,WORK3+5          LENGTH OF NEW INPUT FIELD                    
*                                                                               
DTASX    DS    0H                                                               
         XMOD1                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* MY ADDELEM                                                                    
***********************************************************************         
MYADELM  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
                                                                                
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
*                                  ADD DATE ELEM TO BUYREC                      
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),(R3),0                        
         TM    DMCB+12,X'05'       REC TOO LONG                                 
         BZ    MYADELMX                                                         
         LA    R3,339              REC FULL                                     
         B     ERROR                                                            
                                                                                
MYADELMX DS    0H                                                               
         MVC   DMCB(24),DMCB2      RESTORE                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DTASD    DSECT                                                                  
KSTADATE DS    CL6                 CONTRACT START DATE - YYMMDD                 
KENDDATE DS    CL6                 CONTRACT END DATE                            
ISTADATE DS    CL6                 USER INPUT START DATE                        
IENDDATE DS    CL6                 USER INPUT END DATE                          
CSTADATE DS    CL6                 STRT DATE TO COMPRESS X'03' ELEMENTS         
CENDDATE DS    CL6                 END DATE TO COMPRESS X'03' ELEMENTS          
TMP1DATE DS    CL6                 TEMPORARY AREA FOR DATE CALCULATIONS         
TMP2DATE DS    CL6                 TEMPORARY AREA FOR DATE CALCULATIONS         
*                                                                               
CREDITD  DSECT                                                                  
CRSTDT   DS    CL8                 BUY GRID START DATE                          
         DS    CL1                 SEPARATOR                                    
CRSTDT2  DS    CL8                 USER INPUT START DATE                        
         DS    CL1                 SEPARATOR                                    
CRSTDT3  DS    CL8                 USER INPUT END DATE                          
CRNUMWK  DS    X                   NUMBER OF WEEKS FOR THIS CREDIT              
CRNUMSPT DS    X                   NUMBER OF SPOTS CREDITED                     
CRFLAG   DS    X                                                                
CRFALTQ  EQU   X'80'               ALTERNATING WEEKS                            
USRSDATE DS    XL3                 USER INPUT START DATE OF CREDIT              
USREDATE DS    XL3                 USER INPUT END DATE OF CREDIT                
TMPDATE  DS    CL6                 TEMPORARY AREA FOR DATE CALCULATIONS         
TMPDATE2 DS    CL6                 TEMPORARY AREA FOR DATE CALCULATIONS         
CRBUYEL  DS    XL11                BUY ELEMENT BUILD AREA                       
CRENDDAY DS    X                                                                
CREDITDX EQU   *                                                                
*                                                                               
MGWORKD  DSECT                                                                  
PERVAL   DS    V                                                                
GSTRDATE DS    XL3                 (YMD) MISSED BUY START DATE FOR GRID         
*                                                                               
* MAX 53 WEEKS EACH WITH 2 BYTES FOR EACH WEEK                                  
* FIRST BYTE IS THE NUMBER OF SPOTS FOR THAT WEEK                               
* SECOND BYTE IS THE FLAGS: SPOT OVERRIDE, ALTERNATING, ETC                     
*                                                                               
BUYGRID  DS    XL(53*2)            BUYGRID FOR TARGET MISSED BUY                
*                                                                               
ELEM     DS    XL256                                                            
*                                  SO WE KNOW WHERE TO PUT CURSOR IN            
*                                  CASE OF ERROR                                
*                                                                               
MSSTDT   DS    CL8                 BUY GRID START DATE                          
         DS    CL1                 SEPARATOR                                    
MSSTDT2  DS    CL8                 USER INPUT START DATE                        
         DS    CL1                 SEPARATOR                                    
MSSTDT3  DS    CL8                 USER INPUT END DATE                          
MSBUYEL  DS    XL11                BUY ELEMENT BUILD AREA                       
MSENDDAY DS    X                                                                
*                                                                               
SVELCODE DS    X                                                                
MGWORKX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'162RECNT15   09/18/09'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
