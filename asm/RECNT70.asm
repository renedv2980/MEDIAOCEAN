*          DATA SET RECNT70    AT LEVEL 055 AS OF 07/21/03                      
*          DATA SET RECNT70    AT LEVEL 059 AS OF 04/03/96                      
*PHASE T80270A,+0                                                               
*INCLUDE RECONDAT                                                               
*INCLUDE REVALDAT                                                               
         TITLE 'T80270 - REP SAR DISPLAY/EDIT'                                  
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT70 (T80270) --- SAR DISPLAY/EDIT                    *             
*                                                                 *             
*  NOTE:  THIS MODULE NEEDS TO BE IN SYNC WITH RECNT71 & REPRO12  *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 22JUN92 (BU ) --- ORIGINAL ENTRY                                *             
*                                                                 *             
* 14JUN93 (BU ) --- ALL BUDGET $$$ NOW MARKET $$$.                *             
*                                                                 *             
* 29JUL93 (BU ) --- PROHIBIT BUDGET,SHR GOAL,SPL ENTRY FOR TYPE N *             
*                   CONTRACTS                                     *             
*                                                                 *             
* 03AUG93 (BU ) --- PROHIBIT BUDGET,SHR GOAL FOR GEN/ZZ CONTRACTS *             
*                                                                 *             
* 05AUG93 (BU ) --- INSTALL TRAP FOR EC KEY W/ZERO D/A            *             
*                                                                 *             
* 17AUG93 (BU ) --- RELOCATE DBLOCK:  WAS STEPPING ON END OF      *             
*                   LONG CONTRACTS                                *             
*                                                                 *             
* 22SEP93 (BU ) --- REMOVE REQUIREMENT FOR COMMENT CHANGE FOR     *             
*                   UPDATE TO TRUE ACTIVITY DATE FOR SPL          *             
*                                                                 *             
* 01OCT93 (BU ) --- 'SPREDFOR' CHANGED TO BROADCAST MONTH BASIS   *             
*                                                                 *             
* 21OCT93 (BU ) --- DON'T ROUND PENNIES FOR 'SPREDFOR'            *             
*                                                                 *             
* 07JAN94 (BU ) --- ADD 'FORECAST?' FLAG                          *             
*                                                                 *             
* 22APR94 (SKU) --- ADD FORECASTING SUPPORT                       *             
*                                                                 *             
* 17MAY94 (SKU) --- FIX MARKET BUDGET DIVISION BY ZERO BUG        *             
*                                                                 *             
* 12SEP94 (BU ) --- REQUIRE REP STA SPL > $0 IF STA HAS MONEY     *             
*                                                                 *             
* 09DEC94 (SKU) --- FIX DISPLAY BUGS                              *             
*                                                                 *             
* 08MAR95 (BU ) --- FIX STATION LIST PROBLEM                      *             
*                                                                 *             
* 04APR95 (BU ) --- REQUIRE BOTH BUDGET $ AND SHARE GOAL FOR      *             
*                   PENDING...                                    *             
*                                                                 *             
* 18JUL95 (SKU) --- FIX DEMO/BOOK DR VALIDATION BUG               *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 23JAN96 (BU ) --- CHANGE DAYPART TABLE LOOKUP TO NOT ABORT IF   *             
*                   UNRECOGNIZED DAYPART ENCOUNTERED.             *             
*                                                                 *             
* 22FEB96 (SKU) --- IF PROPOSAL EXPANSION USED, DON'T ALLOW EDIT  *             
*                                                                 *             
* 05MAR96 (JRD) --- CONVERTED TO SAR FULLSCREEN                   *             
*                                                                 *             
* 27MAR96 (JRD) --- ADDED BONUS/TRADE/DR & REORGANIZED CODE       *             
*                                                                 *             
* 03APR96 (JRD) --- ADDED ORDER                                                 
*                                                                 *             
* 06MAY96 (JRD) --- ADDED ERROR MESSAGE FOR DEMO W/DR BOOK                      
*                                                                 *             
* 31MAY96 (SKU) --- ADD PP TO PENDING AS PAID PROGRAMMING         *             
*                                                                 *             
* 12JUN96 (JRD) --- ADD PARENT REP CODE FOR DAYPART VALIDATION    *             
*                                                                 *             
* 21AUG96 (JRD) --- ADD PENDING COMMENT PROFILE CHECK             *             
*                                                                 *             
* 21JAN97 (SKU) --- FIX FORECAST BUCKET BUG                       *             
*                                                                 *             
* 24JAN97 (DBU) --- DON'T REQUIRE DPT CODE IF SPECIAL MKT BUDGET  *             
*                   ARE ENTERED. ALLOW GEN AVL IN BUDGET FIELD    *             
*                                                                 *             
* 12FEB97 (SKU) --- SUPPORT SPECIAL BOOK TYPE                     *             
*                                                                 *             
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
*                                                                 *             
* 20AUG98 (SKU) --- REFINE CHECK FOR ADV GEN                      *             
*                                                                 *             
* 18JUL02 (SKU) --- DEMO RATING SUPPORT                           *             
*                                                                 *             
* 09JUN03 (SKU) --- CLEAR OFFERS IF PENDING DEMO CATEGORY CHANGES *             
*                                                                 *             
* 21JUL03 (HQ ) --- DD ELEMENT EMPTY BUG FIX FOR DEMO CAT CHANGE  *             
*                   **  END TOMBSTONE  **                         *             
*******************************************************************             
*                                                                               
T80270   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80270,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         ST    R5,TEMP                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)     4K                                           
         ST    R7,SAVUSING         SAVE FOR LATER 'USING' NEEDS                 
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VPARSNIP,CPARSNIP                                                
         DROP  RE                                                               
         SPACE 1                                                                
         MVC   BUYACT(6),MYSPACES  BLANK OUT BUYACT,BUYNUM                      
*                                                                               
         MVC   LOCALFLT,RCONDATE                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   MAIN010                                                          
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    MAIN010                                                          
         MVC   LOCALFLT,RCONRFLT   USE REVISED FLIGHT DATE                      
         DROP  R6                                                               
*                                                                               
MAIN010  DS    0H                                                               
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)      DISPLAY REQUEST?                             
         BE    DISPSAR                                                          
         CLC   =C'EDIT',0(R2)      UPDATE REQUEST?                              
         BE    EDIT                                                             
         DC    H'0'                NO  - SHOULDN'T HAPPEN                       
         EJECT                                                                  
*                                                                               
*          DATA SET RECNT45A   AT LEVEL 087 AS OF 03/05/96                      
*                                                                               
         SPACE 3                                                                
*                                                                               
*   BEGIN DISPLAY OF SAR OF SCREEN                                              
*                                                                               
DISPSAR  EQU   *                                                                
         GOTO1 VLOAD,DMCB,(X'80',0)                                             
*                                                                               
*   ABOVE VLOAD CALLS THE T80280 MODULE, WHICH RESOLVES THE VADDRESSES          
*       OF COMMONLY-USED ROUTINES.                                              
*                                                                               
DSAR0000 EQU   *                                                                
*                                                                               
*   FOUTBLK CALL CLEARS NON-SPACE FIELDS, TURNS ON TRANSMIT BITS                
*      FOR THOSE FIELDS, BUT DOES NOT TURN ON TRANSMIT BITS FOR                 
*      FIELDS WHICH ARE BLANK, BUT MAY BE FILLED IN BY NEXT DISPLAY             
*      LOGIC.                                                                   
*                                                                               
         GOTO1 VFOUTBLK,DMCB,SARSBUXH,SARLAST,0                                 
*                                                                               
*   TRANOUT TURNS ON TRANSMIT BITS FOR ALL UNPROTECTED FIELDS ON                
*      SCREEN.  THIS IS A BIT PROFLIGATE, BUT....                               
*                                                                               
         GOTO1 =A(TRANOUT),DMCB,(RC),SARSBUXH,SARLAST,RR=Y                      
*                                                                               
*   CLEAR PROTECTED FIELDS BETWEEN DISPLAYS                                     
*                                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSAR0240            SAR INFO NOT FOUND                           
         SPACE 1                                                                
*                                                                               
         USING RSARXEL,R6                                                       
         OI    SARSBUDH+6,X'80'    TRANSMIT                                     
         TM    RSARXFL2,X'80'      'BONUS' BUDGET?                              
         BZ    *+14                NO                                           
         MVC   SARSBUD,=CL8'BONUS'                                              
         B     DSAR0020                                                         
         TM    RSARXFL2,X'40'      'TRADE' BUDGET?                              
         BZ    *+14                NO                                           
         MVC   SARSBUD,=CL8'TRADE'                                              
         B     DSAR0020                                                         
         TM    RSARXFL2,X'20'      'DR' BUDGET?                                 
         BZ    *+14                NO                                           
         MVC   SARSBUD,=CL8'DR'                                                 
         B     DSAR0020                                                         
         TM    RSARXFL2,X'10'      'ORDER' BUDGET?                              
         BZ    *+14                NO                                           
         MVC   SARSBUD,=CL8'ORDER'                                              
         B     DSAR0020                                                         
         TM    RSARXFL2,X'08'      'PP' BUDGET?                                 
         BZ    *+14                NO                                           
         MVC   SARSBUD,=CL8'PP'                                                 
         B     DSAR0020                                                         
         TM    RSARXFL2,X'04'      'GEN AVAIL' BUDGET?                          
         BZ    *+14                NO                                           
         MVC   SARSBUD,=CL8'GEN AVL'                                            
         B     DSAR0020                                                         
*                                                                               
         LA    R8,SARSBUD          ADDRESS OF BUDGET FIELD                      
         MVC   FULL,RSARXBGT       DISPLAY BUDGET, IF ANY                       
         L     R2,FULL                                                          
         LTR   R2,R2               ANY VALUE HERE?                              
         BZ    DSAR0010            NO  - SKIP IT                                
         EDIT  (R2),(8,(R8)),ALIGN=LEFT                                         
         B     DSAR0015                                                         
DSAR0010 EQU   *                                                                
         TM    RSARXFLG,X'40'      BUDGET ENTERED AS ZERO?                      
         BNO   DSAR0015            NO                                           
         MVI   0(R8),C'0'          YES - SEND BACK A ZERO                       
DSAR0015 EQU   *                                                                
         CLI   RSARXLEN,120        OLD OR NEW ELEMENT?                          
         BNH   DSAR0020            OLD - NEXT FIELD DOESN'T EXIST               
         ZIC   R2,RSARXSHG         DISPLAY SHARE GOAL, IF ANY                   
         LTR   R2,R2               ANY VALUE HERE?                              
         BZ    DSAR0018            NO  - SKIP IT                                
         EDIT  (R2),(3,SARSGOL),ALIGN=LEFT                                      
*                                                                               
* STATION BUDGET = MARKET BUDGET * SHARE GOAL                                   
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,FULL                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+1(1),RSARXSHG                                               
         MH    RF,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+3,100                                                       
         D     RE,WORK                                                          
         LR    R2,RF                                                            
         EDIT  (R2),(8,SARSTBD),ALIGN=LEFT                                      
         B     DSAR0020                                                         
DSAR0018 EQU   *                                                                
         TM    RSARXFLG,X'20'      SHARE GOAL ENTERED AS ZERO?                  
         BNO   DSAR0020            NO                                           
         MVI   SARSGOL,C'0'        YES - SEND BACK A ZERO                       
         MVI   SARSTBD,C'0'        STATION BUDGET MUST BE ZERO, TOO             
DSAR0020 EQU   *                                                                
         CLI   RSARXLEN,120        OLD OR NEW ELEMENT?                          
         BNH   DSAR0030            OLD - NEXT FIELD DOESN'T EXIST               
         OC    RSARXLAD,RSARXLAD   ANY LAST ACTIVITY DATE?                      
         BZ    DSAR0030            NO                                           
         GOTO1 DATCON,DMCB,(3,RSARXLAD),(5,SARSACT)                             
*                                  DISPLAY LAST ACTIVITY DATE                   
         LA    R2,SARSACTH         SET TRANSMIT FOR THIS FIELD                  
         FOUT  (R2)                                                             
DSAR0030 EQU   *                                                                
         EJECT                                                                  
*********************                                                           
** DISPLAY LENGTHS **                                                           
*********************                                                           
DSARLN   LA    R2,WORK3            PUT OUT LENGTHS AND CLASS                    
         MVI   WORK3,C' '                                                       
         MVC   WORK3+1(L'WORK3-1),WORK3                                         
         LA    R5,RSARXRFR                                                      
         LA    R3,6                                                             
         SPACE 1                                                                
DSARLN10 OC    0(2,R5),0(R5)                                                    
         BZ    DSARLN20            NO LENGTH IN THIS FIELD                      
         CH    R3,=H'6'                                                         
         BE    *+12                FIRST TIME                                   
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         ZIC   RE,1(R5)            LENGTH                                       
         TM    1(R5),X'80'         MINUTES?                                     
         BZ    *+8                 NO                                           
         SH    RE,=XL2'0080'                                                    
         EDIT  (RE),(5,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0               LENGTH OF OUTPUT                             
         TM    1(R5),X'80'         MINUTES?                                     
         BZ    *+12                NO                                           
         MVI   0(R2),C'M'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         CLI   0(R5),0             CLASS??                                      
         BE    DSARLN20            NO                                           
         EDIT  (B1,0(R5)),(3,0(R2)),1,ALIGN=LEFT                                
         AR    R2,R0                                                            
*                                                                               
DSARLN20 LA    R5,2(R5)                                                         
         BCT   R3,DSARLN10                                                      
         MVC   SARSLEN,WORK3                                                    
         EJECT                                                                  
         CLC   RSARXBKS(2),=C'DR'                                               
         BE    DSAR0060                                                         
         CLC   RSARXBKS(2),=C'PP'                                               
         BE    DSAR0060                                                         
         SPACE 1                                                                
DSAR0052 LA    R2,SARSDEMH         DEMOS                                        
         LA    R4,RBUYREC                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         SPACE 1                                                                
         LA    R5,WORK3                                                         
         XC    WORK3(30),WORK3                                                  
         MVC   0(L'RSARXDEM,R5),RSARXDEM        DEMOS + ENDING ZERO             
         LA    R3,7                                                             
DSAR0054 CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R3,DSAR0054                                                      
         SPACE 1                                                                
         LA    R5,WORK3                                                         
         SPACE 1                                                                
***********NOTE- DEMCON IS REALLY DEMOCON - SEE T80280***********               
         SPACE 1                                                                
         GOTO1 DEMCON,DMCB,(7,(R5)),(9,8(R2)),(0,DBLOCKD)                       
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* MOVE BOOK AND DTP/CPP INFO INTO A COMMON SAVE AREA FOR SAR OR                 
*  PROPOSAL EXPANSION TYPE DATA TO SIMPLFIY HANDLING OLD AND NEW                
*  RECORDS                                                                      
*********************************************************************           
DSAR0060 XC    SAVBOOKS,SAVBOOKS                                                
         XC    SAVDPTS,SAVDPTS                                                  
*                                                                               
         CLI   RSARXLEN,RSARXLTH   NEW ELEMENT?                                 
         BNE   *+12                NO                                           
         TM    RSARXFLG,X'04'      PROPOSAL EXPANSION USED?                     
         BO    DSAR0064            YES                                          
         SPACE 1                                                                
*-------------------------------------                                          
* COPY INFORMATION FROM SARX ELEMENT - OR SAR ELEMENT                           
*--------------------------------------                                         
         CLC   RSARXBKS(2),=C'DR'  FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    DSAR0062                                                         
         CLC   RSARXBKS(2),=C'PP'  FOR PAID PROGRAMMING - SKIP BOOKS            
         BE    DSAR0062                                                         
*                                                                               
         LA    R0,6                NUMBER OF BOOKS TO COPY                      
         LA    RE,RSARXBKS                                                      
         LA    RF,SAVBOOKS                                                      
         MVC   2(3,RF),0(RE)       MOVE BOOK                                    
         LA    RF,5(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,*-14                                                          
*                                                                               
DSAR0062 DS    0H                                                               
         LA    R0,6                NUMBER OF DAYPARTS                           
         LA    RE,RSARXDPT                                                      
         LA    RF,SAVDPTS                                                       
         MVC   0(1,RF),0(RE)       MOVE DAYPART                                 
         MVC   3(2,RF),1(RE)       MOVE CPP                                     
         LA    RF,5(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,*-20                                                          
*                                                                               
         B     DSAR0070                                                         
         SPACE 1                                                                
*--------------------------------------------------                             
* COPY INFORMATION FROM PROPOSAL EXPANSION ELEMENTS                             
*--------------------------------------------------                             
DSAR0064 CLC   RSARXBKS(2),=C'DR'  FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    DSAR0066                                                         
         CLC   RSARXBKS(2),=C'PP'  FOR PAID PROGRAMMING - SKIP BOOKS            
         BE    DSAR0066                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,RCPRBKEQ     GET PROPOSAL EXP BOOK ELEMENT                
         USING RCPRBKEL,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD BE HERE                               
         SR    R0,R0                                                            
         ZIC   R1,RCPRBKLN                                                      
         SH    R1,=AL2(RCPRBKOQ)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRBKBK                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,RCPRBKBK                                                      
         LA    RF,SAVBOOKS                                                      
         MVC   0(L'RCPRBKBK,RF),0(RE)   MOVE BOOK OR LABEL                      
         LA    RF,L'RCPRBKBK(RF)                                                
         LA    RE,L'RCPRBKBK(RE)                                                
         BCT   R1,*-14                                                          
*                                                                               
DSAR0066 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,RCPRDPEQ     GET PROPOSAL EXP DAYPART/CPP ELEM            
         USING RCPRDPEL,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD BE HERE                               
         SR    R0,R0                                                            
         ZIC   R1,RCPRDPLN                                                      
         SH    R1,=AL2(RCPRDPOX)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRDPDP                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BZ    DSAR0070                                                         
*        DC    H'0'                                                             
*                                                                               
         LTR   R0,R0               OLD ELEMENT?                                 
         BNZ   DSAR0067            NO                                           
         LA    RE,RCPRDPDP-(RCPRDPOQ-RCPRDPOX)                                  
         B     DSAR0068                                                         
*                                                                               
DSAR0067 LA    RE,RCPRDPDP                                                      
         TM    RCPRDPFL,RCPRDPMQ   USING DAYPART MENU?                          
         BO    DSAR0068            YES                                          
*                                                                               
         LA    RE,RCPRDPDP                                                      
DSAR0068 LA    RF,SAVDPTS                                                       
         MVC   0(L'RCPRDPDP,RF),0(RE)   MOVE DAYPART & CPP                      
         LA    RF,L'RCPRDPDP(RF)                                                
         LA    RE,L'RCPRDPDP(RE)                                                
         BCT   R1,*-14                                                          
         SPACE 1                                                                
**************************                                                      
** READ OVERRIDE LABELS **                                                      
**************************                                                      
DSAR0070 DS    0H                                                               
         MVC   SAVLBLS,MYSPACES                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSAR0071            NO BOOK RENAMES                              
*                                                                               
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'            OVEHEAD AND 1 FOR EX                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVLBLS(0),2(R6)                                                 
         SPACE 1                                                                
*************************                                                       
** RESTORE SAR ELEMENT **                                                       
*************************                                                       
DSAR0071 LA    R6,RCONREC                                                       
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                WHERE DID IT GO?                             
         SPACE 1                                                                
********************                                                            
** COUNT DAYPARTS **                                                            
********************                                                            
         DS    0H                                                               
         SR    RF,RF                                                            
         LA    RE,SAVDPTS                                                       
         LA    R0,SAVDPTS+L'SAVDPTS                                             
*                                                                               
DSAR0072 CLI   0(RE),C' '          ANY DAYPART HERE?                            
         BNH   DSAR0074            NO - END OF LIST                             
         LA    RF,1(RF)                                                         
         LA    RE,L'SAVDPT(RE)                                                  
         CR    RE,R0               END OF TABLE?                                
         BL    DSAR0072                                                         
*                                                                               
DSAR0074 ST    RF,DPTCNT           STORE DAYPART COUNT                          
**       LTR   RF,RF               SHOULD ALWAYS BE 1                           
**       BNZ   *+6                                                              
**       DC    H'0'                SOME UPLOADS DON'T HAVE DAYPARTS             
         EJECT                                                                  
*************************                                                       
** DISPLAY BOOKS FIELD **                                                       
*************************                                                       
DSARBK   CLC   RSARXBKS(2),=C'DR'                                               
         BNE   DSARBK05                                                         
         MVC   SARSBUX,MYSPACES                                                 
         MVC   SARSBUX(2),=C'DR'                                                
         MVC   SARSDEM,MYSPACES                                                 
         B     DSARBKX                                                          
*                                                                               
DSARBK05 CLC   RSARXBKS(2),=C'PP'                                               
         BNE   DSARBK10                                                         
         MVC   SARSBUX,MYSPACES                                                 
         MVC   SARSBUX(2),=C'PP'                                                
         MVC   SARSDEM,MYSPACES                                                 
         B     DSARBKX                                                          
*                                                                               
DSARBK10 LA    R2,SARSBUX                                                       
         LA    R3,SAVBOOKS                                                      
         LA    R4,SAVLBLS                                                       
*                                                                               
DSARBK20 CLI   0(R3),0                USER DEFINED BOOK?                        
         BNE   DSARBK30               YES                                       
*                                                                               
         OC    0(L'SAVBOOK,R3),0(R3)  ANY BOOK?                                 
         BZ    DSARBK60               NO MORE BOOKS                             
*                                                                               
         CLI   1(R3),0             SPECIAL BOOK TYPE USED FOR THIS BOOK         
         BE    DSARBK21                                                         
         XC    0(5,R4),0(R4)                                                    
         MVC   0(1,R4),1(R3)                                                    
*                                                                               
DSARBK21 DS    0H                                                               
         XC    WORK3,WORK3                                                      
         MVC   WORK3(L'SARSBUXH),SARSBUXH                                       
         LA    R1,RBUYREC                                                       
         XC    0(80,R1),0(R1)                                                   
         MVC   0(2,R1),=X'0B07'    LABEL EL. + LENGTH                           
         MVC   2(5,R1),0(R4)       RENAME LABEL                                 
*                                                                               
         LR    R0,R1                                                            
         GOTO1 UNBOOK,DMCB,(1,2(R3)),WORK3,(C'L',(R0)),(C'+',=CL6' ')           
         ZIC   RE,WORK3                                                         
         LA    RE,WORK3(RE)                                                     
         TM    WORK3+1,X'02'       EXT FIELD HDR?                               
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   DSARBK26                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DSARBK26                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
DSARBK26 LA    RF,WORK3+8                                                       
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK3+8                                                  
         LA    R2,1(RE,R2)                                                      
         B     DSARBK50                                                         
*                                                                               
DSARBK30 MVC   0(L'SAVBOOK,R2),0(R3) SHOW THE USER-DEFINED LABEL                
*                                                                               
         LA    R2,L'SAVBOOK-1(R2)    REMOVE AS MUCH SPACES AS WE CAN            
DSARBK33 CLI   0(R2),C' '                                                       
         BH    DSARBK36                                                         
         BCTR  R2,0                                                             
         B     DSARBK33                                                         
DSARBK36 LA    R2,1(R2)                                                         
*                                                                               
DSARBK50 MVI   0(R2),C','                                                       
*                                                                               
         LA    R2,1(R2)            BUMP TO THE NEXT BOOK                        
         LA    R3,L'SAVBOOK(R3)                                                 
         LA    R4,L'SAVLBL(R4)                                                  
         LA    RE,SAVBOOKS+L'SAVBOOKS                                           
         CR    R3,RE                                                            
         BL    DSARBK20                                                         
*                                                                               
DSARBK60 BCTR  R2,0                REMOVE THE LAST COMMA                        
         MVI   0(R2),C' '                                                       
*                                                                               
DSARBKX  DS    0H                                                               
         SPACE 1                                                                
**********************************                                              
** DISPLAY DAYPART & CPP FIELDS **                                              
**********************************                                              
DSARDP   DS    0H                                                               
         L     RE,DPTCNT                                                        
         LTR   RE,RE                                                            
         BZ    DSARDPX                                                          
*                                                                               
         LA    R2,SARSDPT                                                       
         LA    R3,SARSCPP                                                       
         LA    R4,SAVDPTS                                                       
*                                                                               
DSARDP10 DS    0H                                                               
         MVC   0(1,R2),0(R4)       COPY DAYPART CODE                            
         EDIT  (B4,1(R4)),(L'SARSCPP,0(R3)),2,ALIGN=LEFT                        
         LA    R2,SARSNDP-SARSDPT(R2)                                           
         LA    R3,SARSNCP-SARSCPP(R3)                                           
         LA    R4,L'SAVDPT(R4)                                                  
         BCT   RE,DSARDP10                                                      
*                                                                               
DSARDPX  DS    0H                                                               
         EJECT                                                                  
*********************************                                               
** DISPLAY READY TO BOOK FIELD **                                               
*********************************                                               
DSARRB   DS    0H                                                               
         LA    R2,SARSRTBH                                                      
         MVI   8(R2),C'N'          DEAFULT TO 'N'                               
         CLI   RSARXLEN,RSARXLTH                                                
         BL    DSARRBX                                                          
         TM    RSARXFLG,X'02'      READY TO BOOK?                               
         BZ    DSARRBX             NO                                           
         MVI   8(R2),C'Y'          YES                                          
DSARRBX  DS    0H                                                               
*************************                                                       
** DISPLAY GRPS FIELDS **                                                       
*************************                                                       
DSARGP   DS    0H                                                               
DSARGPX  DS    0H                                                               
*************************                                                       
** DISPLAY TGRP FIELD  **                                                       
*************************                                                       
DSARGT   DS    0H                                                               
DSARGTX  DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
****************************                                                    
** DISPLAY COMMENT FIELDS **                                                    
****************************                                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSAR0240                                                         
         LA    R2,SARSCM1H                                                      
         SPACE 1                                                                
DSAR0230 MVI   WORK3,C' '                                                       
         MVC   WORK3+1(L'WORK3-1),WORK3                                         
         ZIC   R3,1(R6)            GET L(COMMENT ELEMENT)                       
         SH    R3,=H'3'            MINUS CONTROL +1 FOR 'EX'                    
         EX    R3,*+8              MOVE IT BY LENGTH TO WORK3                   
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)            GET L(SCREEN FIELD)                          
         TM    1(R2),X'02'         EXTENDED FIELD HEADER PRESENT?               
         BZ    *+8                                                              
         SH    R1,=H'8'            YES - SUBTRACT 8                             
         SH    R1,=H'9'            MINUS FIELD HDR +1 FOR 'EX'                  
         BNP   DSAR0240            NEGATIVE?  DON'T MOVE IT                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
*                                                                               
         LA    R2,SARSCM2H-SARSCM1H(R2)      NEXT COMMENT                       
         LA    R0,SARSLCMH                   LAST COMMENT ON SCREEN             
         CR    R2,R0                         PAST IT?                           
         BH    *+12                          YES - ALL DONE                     
         BAS   RE,NEXTEL                     NO - READ NEXT                     
         BE    DSAR0230                                                         
*                                                                               
         SR    R3,R3               TURN OFF ERROR PASSBACK                      
DSAR0240 EQU   *                                                                
         LA    R2,SARSBUDH         SET CURSOR POSITION                          
         B     EXXMOD              EXIT THE FUNCTION                            
         EJECT                                                                  
**********************************************************************          
*                                                                               
* VALIDATE SCREEN AND CHANGE THE ELEMENT                                        
*    THIS ROUTINE HANDLES INPUT FORM SAR & SARX ELEMENTS AS WELL AS             
*    THE SARX, PRBK & PRDP COMBO                                                
*                                                                               
*    IT ALWAS WRITE OUT AS AN SARX, PRBK & PRDP COMBO.                          
*                                                                               
**********************************************************************          
***********************                                                         
** ERROR MSG EQUATES **                                                         
***********************                                                         
TOOMNYBK EQU   161                                                              
MXOF2UDB EQU   517                                                              
NOBUDGOL EQU   467                                                              
NODUPES  EQU   401                                                              
**********************************************                                  
** MAKE NEWFILE CONVERSION A LITTLE CLEANER **                                  
**********************************************                                  
EXITNV   LA    R3,INVINP                                                        
         B     ERROR                                                            
         SPACE 2                                                                
*                                                                               
EDIT     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'80',0)                                             
*                                                                               
*   ABOVE VLOAD CALLS THE T80280 MODULE, WHICH RESOLVES THE VADDRESSES          
*       OF COMMONLY-USED ROUTINES.                                              
*                                                                               
         CLC   CONACT,=C'ADDS'                                                  
         BE    ESAR0010                                                         
*                                                                               
         L     R7,SAVUSING                                                      
         USING TWAWORK,R7                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         DROP  R7                                                               
*                                  DELETE BOOK RENAME ELEMENT                   
         GOTO1 VGETREC,DMCB,RCONREC                                             
         GOTO1 VDELELEM,DMCB,(X'0B',RCONREC)                                    
*                                                                               
ESAR0010 EQU   *                                                                
         XC    SAVDEMOS,SAVDEMOS                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESAR0015            FOUND A X'41'(DPT EXPANSION)                 
         USING RSARXEL,R6                                                       
         MVC   SAVDEMOS,RSARXDEM                                                
         DROP  R6                                                               
*                                                                               
ESAR0015 EQU   *                                                                
         LA    R4,WORK2            BUILD NEW SAR ELEMENT                        
         USING RSARXEL,R4                                                       
*                                                                               
         XC    RSARXEL(200),RSARXEL                                             
         MVI   RSARXCO,X'12'                                                    
         MVI   RSARXLEN,RSARXLTH                                                
         GOTO1 DATCON,DMCB,(5,WORK),(3,RSARXLAD)                                
*                                  INSERT LAST ACTIVITY DATE (TODAY)            
         SPACE 2                                                                
****************                                                                
** EDIT BOOKS **                                                                
****************                                                                
         MVC   RSARXSRC,RCONRTGS   DEFAULT IS RATING SERVICE                    
         DROP  R4                                                               
ESARBK   DS    0H                                                               
         SR    R7,R7               # OF BOOKS                                   
         L     R5,AIO3             BUILD X'0B' ELEMENT HERE                     
         LA    R5,2(R5)                                                         
         MVC   0(L'SAVLBLS,R5),MYSPACES                                         
         LA    R2,SARSBUXH                                                      
         OC    SARSBUX,SARSBUX                                                  
         BNZ   *+12                                                             
         LA    R3,INVINP                                                        
         B     ERROR                                                            
*                                                                               
         LA    RE,RBUYREC          CLEAR FOR PARSNIP                            
         LA    RF,480                                                           
         XCEFL                                                                  
         MVC   BYTE,SARSBUXH+5     INPUT LENGTH                                 
*                                                                               
         GOTO1 VPARSNIP,DMCB,(BYTE,SARSBUX),(16,RBUYREC),('PSNAOKQ',0)          
         CLI   8(R1),0                                                          
         BNE   EXITNV                                                           
*                                                                               
         MVI   BYTE,0                                                           
         LA    R4,SAVBOOKS         R4 = A(1ST 5-BYTE BK OR LABEL)               
         LA    R3,RBUYREC          R3 = A(1ST PARSNIP FIELD)                    
         USING PSND,R3                                                          
*                                                                               
ESARBK10 DS    0H                                                               
         CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    ESARBK40            NO                                           
*                                                                               
         CLI   PSNERR,0            COMPONENT IS IN ERROR?                       
         BNE   EXITNV                                                           
*                                                                               
         CLI   PSNTAG,C'F'         REGUALR FIELD COMPONENT                      
         BNE   EXITNV                                                           
*                                                                               
         TM    PSNSTAT,PSNNUMQ     FIELD SHOULD NOT BE NUMERIC                  
         BNZ   EXITNV                                                           
*                                                                               
         CLI   PSNLEN,0            MISSING FIELD                                
         BE    EXITNV                                                           
*                                                                               
         XC    WORK3,WORK3                                                      
         L     RE,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         STC   R1,WORK3+5                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK3+8(0),0(RE)                                                 
         LA    RF,WORK3+9(R1)      WHERE THE RENAME SHOULD GO                   
*                                                                               
         OC    PSNATTR,PSNATTR     RENAME?                                      
         BZ    ESARBK18            NO                                           
*                                                                               
         LA    R3,PSNL(R3)                                                      
         OC    PSNATTR,PSNATTR     ANOTHER RENAME?                              
         BNZ   EXITNV              YES                                          
*                                                                               
         L     RE,PSNCOMP                                                       
         BCTR  RE,0                WANT THE '('                                 
         ZIC   R1,PSNLEN                                                        
         LA    R1,2(R1)            AND THE ')'                                  
         ZIC   R0,WORK3+5                                                       
         AR    R0,R1                                                            
         STC   R0,WORK3+5                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)                                                    
*                                                                               
ESARBK18 GOTO1 BOOKVAL,DMCB,(RCONRTGS,WORK3),(1,WORK),                 X        
               (C'L',SCANNER),(R5)                                              
*                                                                               
         ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BNE   ESARBK20            YES                                          
*                                                                               
         CLI   PSNLEN,2            COULD IT BE DR?                              
         BNE   ESARBK19            NO                                           
         L     RE,PSNCOMP                                                       
         CLC   0(2,RE),=C'DR'                                                   
         BNE   ESARBK1A                                                         
         XC    SAVBOOKS,SAVBOOKS                                                
         MVC   SAVBOOKS+2(2),=C'DR'                                             
         B     ESARBK50                                                         
*                                                                               
ESARBK1A DS    0H                                                               
         CLC   0(2,RE),=C'PP'                                                   
         BNE   EXITNV                                                           
         XC    SAVBOOKS,SAVBOOKS                                                
         MVC   SAVBOOKS+2(2),=C'PP'                                             
         B     ESARBK50                                                         
*                                                                               
ESARBK19 CLI   PSNLEN,3            MAKE SURE USER DEFINED BTWN 3-5 CHR          
         BL    EXITNV                                                           
         CLI   PSNLEN,L'SAVBOOK                                                 
         BH    EXITNV                                                           
*                                                                               
         ZIC   R1,BYTE2                                                         
         AH    R1,=H'1'                                                         
         STC   R1,BYTE2                                                         
*                                                                               
         CLI   BYTE2,2                                                          
         BNH   *+12                MAX OF 2 USER-DEFINED BOOKS                  
         LA    R3,MXOF2UDB                                                      
         B     ERROR                                                            
*                                                                               
         L     RE,PSNCOMP          SAVE THE LABEL                               
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RE)                                                    
         OC    0(L'SAVBOOK,R4),MYSPACES                                         
         B     ESARBK30                                                         
*                                                                               
ESARBK20 MVC   2(3,R4),WORK        SAVE THE BOOK DEFINITION                     
*                                                                               
ESARBK30 DS    0H                  ONE CHARACTER RENAME IS                      
         CLI   PSNLEN,1            ACTUALLY SPECIAL BOOK TYPE                   
         BNE   ESARBK35                                                         
         MVC   1(1,R4),0(R5)                                                    
         XC    0(5,R5),0(R5)                                                    
*                                                                               
ESARBK35 DS    0H                                                               
         LA    R3,PSNL(R3)         BUMP TO THE NEXT FIELD                       
         LA    R4,L'SAVBOOK(R4)                                                 
         OC    0(5,R5),MYSPACES                                                 
         LA    R5,5(R5)            NEXT RENAME                                  
         B     ESARBK10                                                         
         DROP  R3                                                               
*                                                                               
ESARBK40 DS    0H                  NEED X'0B' BOOK RENAME ELEMENT?              
         CLI   BYTE,0                                                           
         BE    EXITNV              1 BOOK EQUIRED                               
         CLI   BYTE,7                                                           
         BNH   *+12                TOO MANY BOOKS                               
         LA    R3,TOOMNYBK                                                      
         B     ERROR                                                            
*                                                                               
         L     RE,AIO3                                                          
         CLC   2(L'SAVLBLS,RE),MYSPACES                                         
         BE    ESARBK50            NO                                           
*                                                                               
         MVI   0(RE),X'0B'                                                      
         L     RF,AIO3                                                          
         SR    R5,RF               ELEMENT LENGTH                               
         STC   R5,1(RE)                                                         
         GOTO1 VADDELEM,DMCB,RCONREC,AIO3                                       
*                                                                               
*--------------------------------------------------------------------           
ESARBK50 DS    0H                  DEL & ADD X'40' ELEMENT                      
*                                                                               
         GOTO1 VDELELEM,DMCB,('RCPRBKEQ',RCONREC)                               
*                                                                               
ESARBK51 LA    R6,WORK3                                                         
         XC    WORK3,WORK3                                                      
         USING RCPRBKEL,R6                                                      
         MVI   RCPRBKCO,RCPRBKEQ                                                
*                                                                               
         LA    R0,7                                                             
         LA    RE,SAVBOOKS                                                      
         LA    RF,RCPRBKBK                                                      
ESARBK52 OC    0(L'SAVBOOK,RE),0(RE) END OF BOOKS?                              
         BZ    ESARBK54            YES                                          
         MVC   0(L'RCPRBKBK,RF),0(RE)                                           
         LA    RF,L'RCPRBKBK(RF)                                                
         LA    RE,L'SAVBOOK(RE)                                                 
         BCT   R0,ESARBK52                                                      
*                                                                               
ESARBK54 SR    RF,R6               ELEMENT LENGTH                               
         STC   RF,RCPRBKLN                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK3                                      
*--------------------------------------------------------------------           
         SPACE 1                                                                
         LA    R0,6                UPDATE SAR ELEMENT                           
         LA    R4,WORK2                                                         
         USING RSARXEL,R4                                                       
         XC    RSARXBKS,RSARXBKS                                                
         LA    RE,RSARXBKS                                                      
         LA    RF,SAVBOOKS                                                      
ESARBK60 OC    0(L'SAVBOOK,RF),0(RF)                                            
         BZ    ESARBKX             END OF BOOKS & LABELS                        
         CLI   0(RF),0             USER DEFINED LABEL?                          
         BNE   *+14                YES                                          
         MVC   0(3,RE),2(RF)       COPY IN NEW BOOK                             
         LA    RE,3(RE)                                                         
         LA    RF,L'SAVBOOK(RF)                                                 
         BCT   R0,ESARBK60                                                      
         DROP  R4                                                               
*                                                                               
ESARBKX  DS    0H                                                               
         SR    R3,R3               CLEAR ERRORS                                 
         LA    R4,WORK2            BUILD NEW SAR ELEMENT HERE                   
         USING RSARXEL,R4                                                       
*                                                                               
         EJECT                                                                  
****************                                                                
** EDIT DEMOS **                                                                
****************                                                                
ESARDM   LA    R2,SARSDEMH         EDIT DEMOS                                   
         CLC   =C'DR',SAVBOOKS+2                                                
         BNE   ESARDM05                                                         
         CLI   5(R2),0                                                          
         BNE   EXITNV                                                           
         B     ESARDM30                                                         
*                                                                               
ESARDM05 DS    0H                                                               
         CLC   =C'PP',SAVBOOKS+2                                                
         BNE   ESARDM10                                                         
         CLI   5(R2),0                                                          
         BNE   EXITNV                                                           
         B     ESARDM30                                                         
*                                                                               
ESARDM10 GOTO1 VANY                                                             
         XC    RSARXDEM,RSARXDEM                                                
         XC    WORK3(30),WORK3                                                  
         DROP  R4                                                               
         LA    R5,RBUYREC          AREA FOR DBLOCK                              
         USING DBLOCKD,R5                                                       
         USING RSARXEL,R4                                                       
         LA    R4,WORK2                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         GOTO1 VDEMOVAL,DMCB,(R2),(7,WORK3),(C'Y',(R5))                         
         DROP  R5                                                               
         LA    R3,160              TOO MANY DEMOS                               
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
*                      ELIMINATE EOL MARKER (X'FF')                             
         ZIC   R5,DMCB+4                                                        
         MH    R5,=H'3'                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RSARXDEM(0),WORK3                                                
*                                                                               
* TEST PROFILE SET TO REQUIRE PRIMARY DEMO                                      
         TM    PROFILES+CNTPDEMB,CNTPDEMA                                       
         BZ    ESARDM30                                                         
         LA    R1,RSARXDEM                                                      
         LA    R0,8                                                             
         SPACE 1                                                                
ESARDM20 TM    0(R1),X'40'         PRIMARY DEMO CHECK                           
         BO    ESARDM30                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,ESARDM20                                                      
         LA    R3,PDEMERR                                                       
         B     ERROR                                                            
ESARDM30 EQU   *                                                                
         SR    R3,R3               CLEAR ERRORS                                 
         EJECT                                                                  
******************                                                              
** EDIT LENGTHS **                                                              
******************                                                              
ESARLN   LA    R2,SARSLENH                                                      
         GOTO1 VANY                                                             
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(7,IOAREA),C',=,.'                             
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR               INVALID INPUT                                
         CH    R5,=H'6'                                                         
         BH    ERROR                                                            
         SPACE 1                                                                
         LA    R6,IOAREA                                                        
         LA    R1,RSARXRFR                                                      
         SPACE 1                                                                
ESARLN10 TM    2(R6),X'80'                                                      
         BNO   ESARLN15            LENGTH NOT NUMERIC                           
         ICM   RE,15,4(R6)         LENGTH                                       
         LTR   RE,RE                                                            
         BZ    ERROR               ZERO IS INVALID                              
         CH    RE,=H'127'          THIS IS TOO HIGH                             
         BH    ERROR                                                            
         B     ESARLN30                                                         
*                                                                               
ESARLN15 DS    0H                  CHECK FOR MINUTES                            
         CLI   0(R6),4             MAX INPUT LENGTH(127M)                       
         BH    ERROR                                                            
         ZIC   RE,0(R6)                                                         
         LA    RF,12(R6)           DATA                                         
*                                                                               
ESARLN20 CLI   0(RF),C'M'                                                       
         BE    ESARLN25                                                         
         CLI   0(RF),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(RF),X'F9'                                                      
         BH    ERROR                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,ESARLN20                                                      
         B     ERROR                                                            
*                                                                               
ESARLN25 DS    0H                                                               
         CH    RE,=H'1'            EXTRA CHARS?                                 
         BNE   EXITNV              YES                                          
         ZIC   RE,0(R6)                                                         
         SH    RE,=H'2'            1 FOR EX + 1 FOR C'M'                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,12(0,R6)                                                     
         CVB   RE,DUB                                                           
         LTR   RE,RE                                                            
         BZ    ERROR               ZERO IS INVALID                              
         CH    RE,=H'127'          THIS IS TOO HIGH                             
         BH    ERROR                                                            
         O     RE,=XL4'80'         SET MINUTES FLAG                             
*                                                                               
ESARLN30 STC   RE,1(R1)            SAVE IT IN RSARXRFR                          
         SPACE 1                                                                
         CLI   1(R6),0                                                          
         BE    ESARLN50            NO CLASS NUMBER                              
         TM    3(R6),X'80'                                                      
         BNO   ERROR               CLASS NOT NUMERIC                            
         MVC   FULL,8(R6)          CLASS CODE                                   
         L     RE,FULL                                                          
         LTR   RE,RE                                                            
         BZ    ERROR               ZERO IS INVALID                              
         CH    RE,=H'9'                                                         
         BH    ERROR                                                            
         STC   RE,0(R1)            SAVE CLASS IN RSARXRFR                       
ESARLN50 LA    R1,2(R1)            NEXT RSARXRFR                                
         LA    R6,32(R6)           NEXT SCAN BLOCK                              
         BCT   R5,ESARLN10                                                      
*                                                                               
ESARLNX  DS    0H                                                               
         SR    R3,R3               CLEAR ERRORS                                 
         EJECT                                                                  
************************************                                            
** VALIDATE 'READY TO BOOK' FIELD **                                            
************************************                                            
         LA    R2,SARSRTBH                                                      
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         OI    RSARXFLG,X'02'                                                   
         B     ERTBX                                                            
         NI    RSARXFLG,X'FF'-X'02'                                             
         CLI   8(R2),C'N'                                                       
         BE    ERTBX                                                            
         CLI   5(R2),0                                                          
         BNE   EXITNV                                                           
ERTBX    DS    0H                                                               
****************************                                                    
** CHECK FOR BUDGET INPUT **                                                    
****************************                                                    
         CLI   RCONTYPE,C'N'       CONTRACT TYPE = N?                           
         BE    ESAR0020            YES - CAN'T HAVE DATA                        
*                                     IGNORE NEXT TESTS                         
         CLC   RCONKADV,=C'GEN '   ADV = GEN?                                   
         BE    ESAR0020            YES - CAN'T HAVE DATA                        
*                                     IGNORE NEXT TESTS                         
         CLC   RCONCTGY,=C'ZZ'     CATEGORY = ZZ?                               
         BE    ESAR0020            YES - CAN'T HAVE DATA                        
*                                     IGNORE NEXT TESTS                         
         CLI   SARSBUDH+5,0                                                     
         BNE   ESAR0020            MUST ENTER AT LEAST                          
         CLI   SARSTBDH+5,0        EITHER MARKET BUDGET OR STATION              
         BNE   ESAR0020              BUDGET CAN                                 
         LA    R2,SARSBUDH                                                      
         LA    R3,1                MISSING                                      
         B     ERROR                                                            
*************************************                                           
***  EDIT ANY BUDGET FIGURE INPUT ***                                           
*************************************                                           
ESAR0020 DS    0H                                                               
         LA    R2,SARSBUDH         BUDGET FIELD                                 
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ESAR0041            NO                                           
         TM    4(R2),X'08'         NUMERIC?                                     
         BNZ   ESAR0041            YES                                          
**********************************                                              
** PROCESS SPECIAL BUDGET INPUT **                                              
**********************************                                              
ESAR0021 DS    0H                                                               
         OC    SARSBUD,=CL8' '                                                  
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SARSBUD(0),=CL8'BONUS'                                           
         BE    ESAR0024                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SARSBUD(0),=CL8'TRADE'                                           
         BE    ESAR0023                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SARSBUD(0),=CL8'ORDER'                                           
         BE    ESAR0026                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SARSBUD(0),=CL8'DR'                                              
         BE    ESAR0022                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SARSBUD(0),=CL8'GEN AVL'                                         
         BE    ESAR0028                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SARSBUD(0),=CL8'PP'                                              
         BNE   EXITNV                                                           
*                                                                               
         OI    RSARXFL2,X'08'      PP BUDGET                                    
         B     ESAR0090                                                         
*                                                                               
ESAR0022 OI    RSARXFL2,X'20'      DR BUDGET                                    
         B     ESAR0090                                                         
*                                                                               
ESAR0023 OI    RSARXFL2,X'40'      TRADE BUDGET                                 
         B     ESAR0090                                                         
*                                                                               
ESAR0024 OI    RSARXFL2,X'80'      BONUS BUDGET                                 
         B     ESAR0090                                                         
*                                                                               
ESAR0026 OI    RSARXFL2,X'10'      ORDER BUDGET                                 
         B     ESAR0090                                                         
*                                                                               
ESAR0028 OI    RSARXFL2,X'04'      GEN AVAIL BUDGET                             
         B     ESAR0090                                                         
***********************************                                             
** PROCESS STANDARD BUDGET INPUT **                                             
***********************************                                             
ESAR0041 OI    RSARXFLG,X'80'      SET BUDGET = MARKET $$                       
         CLI   RCONTYPE,C'N'       CONTRACT TYPE = N?                           
         BE    ESAR0042            YES - CAN'T HAVE DATA                        
         CLC   RCONKADV,=C'GEN '   NO  - ADV = 'GEN'?                           
         BNE   ESAR0050            NO  - MUST HAVE DATA                         
         CLC   RCONCTGY,=C'ZZ'     YES - CATEGORY = 'ZZ'?                       
         BNE   ESAR0050            NO  - MUST HAVE DATA                         
ESAR0042 EQU   *                                                                
         LA    R3,NODATAIN         YES - SET ERROR MESSAGE                      
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ESAR0060            NO  - OKAY: NOT ALLOWED FOR                  
*                                     TYPE N CONTRACTS                          
         B     ERROR               YES - GO TO ERROR                            
ESAR0050 EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    ESAR0060                                                         
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               ERROR - NOT NUMERIC                          
         MVC   RSARXBGT,4(R6)      USE BINARY VALUE                             
         OC    RSARXBGT,RSARXBGT   COMPARE FOR ZERO                             
         BNZ   ESAR0058            NOT ZERO                                     
         OI    RSARXFLG,X'40'      SET 'ENTERED AS ZERO'                        
ESAR0058 EQU   *                                                                
         MVI   FORSPRED,C'Y'       SET FLAG TO SPREAD FORECAST $                
ESAR0060 EQU   *                                                                
         LA    R2,SARSGOLH         SHARE GOAL FIELD                             
         CLI   RCONTYPE,C'N'       CONTRACT TYPE = N?                           
         BE    ESAR0062            YES - CAN'T HAVE DATA                        
         CLC   RCONKADV,=C'GEN '   NO  - ADV = 'GEN'?                           
         BNE   ESAR0065            NO  - MUST HAVE DATA                         
         CLC   RCONCTGY,=C'ZZ'     YES - CATEGORY = 'ZZ'?                       
         BNE   ESAR0065            NO  - MUST HAVE DATA                         
ESAR0062 EQU   *                                                                
         LA    R3,NODATAIN         YES - SET ERROR MESSAGE                      
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ESAR0080            NO  - OKAY: NOT ALLOWED FOR                  
*                                     TYPE N CONTRACTS                          
         B     ERROR               YES - GO TO ERROR                            
ESAR0065 EQU   *                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   ESAR0070            NO  -                                        
         TM    RSARXFLG,X'80'      BUDGET $$ FOR MARKET ENTERED?                
         BNO   ESAR0070            NO                                           
         LA    R3,370              YES - NEEDS SHARE GOAL                       
         B     ERROR                                                            
ESAR0070 EQU   *                                                                
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               ERROR - NOT NUMERIC                          
         MVC   FULL,4(R6)          CHECK VALUE FOR 100%                         
         L     RF,FULL                                                          
         CH    RF,=H'100'          COMPARE FOR 100%                             
         BH    ERROR               EXCEEDS IT -                                 
****>                              ZERO VALUE PERMITTED                         
****>    LTR   RF,RF               COMPARE FOR ZERO                             
****>    BZ    ERROR               CAN'T ENTER ZERO VALUE                       
         LTR   RF,RF               COMPARE FOR ZERO                             
         BNZ   ESAR0072            CAN'T ENTER ZERO VALUE                       
         OI    RSARXFLG,X'20'      SET 'ENTERED AS ZERO'                        
ESAR0072 EQU   *                                                                
         MVC   RSARXSHG,7(R6)      USE BINARY VALUE: LAST BYTE                  
                                                                                
         CLI   SARSBUDH+5,0        CALCULATE MARKET BUDGET IF                   
         BNE   ESAR0080            STATION BUDGET WAS ENTERED INSTEAD           
         LA    R3,INVINP           OF MARKET BUDGET                             
         LA    R2,SARSGOLH                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         LA    R2,SARSTBDH                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         GOTO1 =A(VALSTBUD),DMCB,(RC),(R4),RR=Y                                 
*                                                                               
         MVI   FORSPRED,C'Y'       SET FLAG TO SPREAD FORECAST $                
*                                                                               
ESAR0080 EQU   *                                                                
         LA    R2,SARSBUDH         SET CURSOR POSITION                          
         LA    R3,NOBUDGOL         SET ERROR MESSAGE                            
*                                                                               
         TM    PROFILES+CNT0SHGB,CNT0SHGA     CHECK ZERO PROFILE                
         BNZ   ESAR0085            SET - ALLOW ZERO                             
*                                                                               
         TM    RSARXFLG,X'40'      $0 BUDGET ENTERED?                           
         BO    ERROR               YES - PROHIBIT IT                            
         TM    RSARXFLG,X'20'      0% SHARE GOAL ENTERED?                       
         BO    ERROR               YES - PROHIBIT IT                            
*                                                                               
ESAR0085 CLI   FORSPRED,C'Y'       SPREAD FORECAST $?                           
         BNE   ESAR0090            NO                                           
         GOTO1 =A(SPREDFOR),DMCB,(RC),(R4),RR=Y                                 
ESAR0090 EQU   *                                                                
         SR    R3,R3               CLEAR ERRORS                                 
         EJECT                                                                  
*******************                                                             
** EDIT DAYPARTS **                                                             
*******************                                                             
ESARDP   DS    0H                                                               
         LA    R2,SARSDPTH                                                      
         LA    R7,SARSDPT                                                       
         LA    R5,SAVDPTS                                                       
         XC    SAVDPTS,SAVDPTS                                                  
*                                                                               
         OI    MISCFLG1,MF1DPTAB                                                
         MVI   ELCODE,RCPRDPEQ                                                  
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    ESARDP2             FOUND A X'41'(DPT EXPANSION)                 
*                                                                               
         MVI   ELCODE,X'12'        NO X'41' IF ITS CHANGE USE TABLE             
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    ESARDP5             THERE'S AN X'12' SO ITS A CHANGE             
         B     ESARDP4             THERES NOT SO ITS AND ADD                    
*                                                                               
ESARDP2  DS    0H                                                               
         USING RCPRDPEL,R6                                                      
         TM    RCPRDPFL,RCPRDPMQ   USES MENU?                                   
         BZ    ESARDP5             NO                                           
*                                                                               
ESARDP4  NI    MISCFLG1,X'FF'-MF1DPTAB                                          
*                                                                               
ESARDP5  CLI   0(R7),C' '              ANY INPUT ON THIS LINE?                  
         BH    ESARDP10                                                         
         LA    R0,SARSLDP              ARE WE ON THE LAST DPT LINE?             
         CR    R7,R0                                                            
         BNE   ESARDP60                NO - OKAY                                
*                                                                               
         TM    RSARXFL2,X'80'      'BONUS' BUDGET?                              
         BNZ   ESARCP              YES                                          
         TM    RSARXFL2,X'40'      'TRADE' BUDGET?                              
         BNZ   ESARCP              YES                                          
         TM    RSARXFL2,X'20'      'DR' BUDGET?                                 
         BNZ   ESARCP              YES                                          
         TM    RSARXFL2,X'10'      'ORDER' BUDGET?                              
         BNZ   ESARCP              YES                                          
         TM    RSARXFL2,X'08'      'PP' BUDGET?                                 
         BNZ   ESARCP              YES                                          
*        TM    RSARXFL2,X'04'      'GEN AVAIL' BUDGET?                          
*        BNZ   ESARCP              YES                                          
         OC    SAVDPTS,SAVDPTS     YES, NEED AT LEAST ONE DPT                   
         BNZ   ESARDPX             THERE WAS ONE                                
         LA    R2,SARSDPTH                                                      
         B     EXITNV                                                           
*                                                                               
ESARDP10 TM    MISCFLG1,MF1DPTAB   USES HARDCODED TABLE?                        
         BO    ESARDP20            YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING RRDPKEY,RE                                                       
         L     RF,SAVUSING                                                      
         USING TWAWORK,RF                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,TWAREPAR                                                
         DROP  RE,RF                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         BE    *+6                 SCREW UP ON THE READ HIGH                    
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(RRDPKDPT-RRDPKEY),KEYSAVE                                    
         BNE   ESARDP20            NO DAYPART RECORDS                           
*                                                                               
****************************************                                        
** READ DAYPART RECORD FOR VALIDATION **                                        
****************************************                                        
         NI    MISCFLG1,X'FF'-MF1DPTAB                                          
         MVC   KEY+RRDPKDPT-RRDPKEY(L'RRDPKDPT),0(R7)                           
*                                                                               
         GOTO1 VHIGH                                                            
         BE    *+6                 SCREW UP ON THE READ HIGH                    
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'RRDPKEY),KEYSAVE                                           
         BNE   EXITNV              INVALID DAYPART                              
*                                                                               
         B     ESARDP50                                                         
*************************************************                               
** READ HARDCODED DAYPART TABLE FOR VALIDATION **                               
*************************************************                               
ESARDP20 OI    MISCFLG1,MF1DPTAB      USE TABLE                                 
         LA    R3,DPTABLE             VALIDATE THE DAYPART CODE                 
*                                                                               
ESARDP25 CLI   0(R3),X'FF'         DID WE HIT THE END OF DPTTABLE?              
         BE    EXITNV              YES, INVALID DAYPART CODE                    
         CLC   0(1,R7),3(R3)                                                    
         BE    ESARDP50                                                         
         LA    R3,L'DPTABLE(R3)                                                 
         B     ESARDP25                                                         
*                                                                               
ESARDP50 DS    0H                                                               
         LA    RE,SAVDPTS                                                       
ESARDP55 CLI   0(RE),0             ANY DAYPART?                                 
         BE    ESARDP56            NO - SKIP                                    
         CLC   0(1,R7),0(RE)       CHECK FOR REPEAT DPT?                        
         BNE   ESARDP56            NO                                           
         LA    R3,NODUPES                                                       
         B     ERROR                                                            
ESARDP56 LA    RE,L'SAVDPT(RE)                                                  
         LA    RF,SAVDPTS+L'SAVDPTS                                             
         CR    RE,RF                                                            
         BL    ESARDP55                                                         
*                                                                               
         MVC   0(1,R5),0(R7)       COPY THE 1-BYTE DAYPART CODE                 
         LA    R5,L'SAVDPT(R5)                                                  
ESARDP60 LA    R2,SARSNDPH-SARSDPTH(R2)    NEXT FIELD                           
         LA    R7,SARSNDP-SARSDPT(R7)                                           
         LA    R0,SARSLDP                  PAST LAST DPT FIELD?                 
         CR    R7,R0                                                            
         BNH   ESARDP5                     NO - CONTINUE                        
*                                                                               
ESARDPX  DS    0H                                                               
         DROP  R4                                                               
**************                                                                  
** EDIT CPP **                                                                  
**************                                                                  
ESARCP   DS    0H                                                               
         LA    R2,SARSCPPH                                                      
         LA    R3,SARSDPTH                                                      
         LA    R4,SAVDPTS                                                       
*                                                                               
ESARCP5  CLI   5(R2),0                 ANY INPUT ON THIS LINE?                  
         BE    ESARCP10                NO, OKAY                                 
*                                                                               
         CLI   8(R3),C' '          ANY DAYPART                                  
         BNH   EXITNV                                                           
*                                                                               
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R0)                                      
         CLI   0(R1),0                                                          
         BNE   EXITNV                                                           
         CLC   =F'9999999',4(R1)                                                
         BL    EXITNV              WON'T DISPLAY                                
         MVC   1(4,R4),4(R1)                                                    
*                                                                               
ESARCP10 DS    0H                                                               
         CLI   8(R3),C' '          ANY DAYPART                                  
         BNH   *+8                                                              
         LA    R4,L'SAVDPT(R4)                                                  
         LA    R2,SARSNCPH-SARSCPPH(R2)    NEXT FIELD                           
         LA    R3,SARSNDPH-SARSDPTH(R3)                                         
         LA    R0,SARSLCPH                 PAST LAST CPP FIELD?                 
         CR    R2,R0                                                            
         BNH   ESARCP5                     NO - CONTINUE                        
*                                                                               
*--------------------------------------------------------------------           
*                                        DELETE AND ADD X'41' ELEMENT           
*                                                                               
         GOTO1 VDELELEM,DMCB,('RCPRDPEQ',RCONREC)                               
*                                                                               
         LA    R6,WORK3                                                         
         XC    WORK3,WORK3                                                      
         USING RCPRDPEL,R6                                                      
         MVI   RCPRDPCO,RCPRDPEQ                                                
         TM    MISCFLG1,MF1DPTAB   DAYPART TABLE?                               
         BNZ   *+8                 YES                                          
         OI    RCPRDPFL,RCPRDPMQ                                                
*                                                                               
         LA    RE,SAVDPTS                                                       
         LA    RF,RCPRDPDP                                                      
*                                                                               
ESARCP22 OC    0(L'SAVDPT,RE),0(RE)    ANY DAYPART?                             
         BZ    ESARCP24                NO - SKIP                                
         MVC   0(L'RCPRDPDP,RF),0(RE)                                           
         LA    RF,L'RCPRDPDP(RF)                                                
*                                                                               
ESARCP24 LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    ESARCP22                                                         
*                                                                               
         SR    RF,R6               ELEMENT LENGTH                               
         STC   RF,RCPRDPLN                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK3                                      
*--------------------------------------------------------------------           
         SPACE 1                                                                
         LA    R4,WORK2                                                         
         USING RSARXEL,R4                                                       
         LA    RF,SAVDPTS          UPDATE SAR ELEMENT                           
         LA    RE,RSARXDPT                                                      
         XC    RSARXDPT,RSARXDPT                                                
         LA    R0,6                                                             
ESARCP30 MVC   0(1,RE),0(RF)                                                    
         LA    RF,L'SAVDPT(RF)                                                  
         LA    RE,3(RE)                                                         
         BCT   R0,ESARCP30                                                      
         DROP  R4                                                               
*                                                                               
ESARCPX  DS    0H                                                               
         EJECT                                                                  
********************                                                            
** EDIT GRP INPUT **                                                            
********************                                                            
ESARGP   DS    0H                                                               
ESARGPX  DS    0H                  ADD X'4?' ELEMENT                            
         EJECT                                                                  
*                                  EDIT BUDGET (NBC ONLY)                       
ESAR0300 EQU   *                                                                
         LA    R4,WORK2                                                         
         USING RSARXEL,R4                                                       
         MVC   RSARXEDT,TODAY                                                   
         OI    RSARXFLG,X'04'      USES EXPANSION ELEMENTS                      
         MVC   BYTE,RSARXFL2       NEED THIS FOR COMMENTS                       
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESAR0310                                                         
         SPACE 1                                                                
         MVC   RSARXEDT,RSARXEDT-RSARXEL(R6)    SAVE SOME OLD STUFF             
         MVC   RSARXSHR,RSARXSHR-RSARXEL(R6)                                    
         MVC   RSARXRDT,RSARXRDT-RSARXEL(R6)                                    
         DROP  R4                                                               
         SPACE 1                                                                
ESAR0310 GOTO1 VDELELEM,DMCB,(X'12',RCONREC)                                    
         GOTO1 VADDELEM,DMCB,RCONREC,(R4)                                       
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESAR0320                                                         
         OI    4(R6),X'08'         SAR UPDATED                                  
ESAR0320 DS    0H                                                               
         EJECT                                                                  
*******************                                                             
** EDIT COMMENTS **                                                             
*******************                                                             
ESARCM   DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'11',RCONREC)                                    
*                                                                               
*   IF CONTRACT CONTAINS AN SPL ELEMENT, DON'T PERMIT THE COMMENTS              
*      FOR SAR TO BE RE-ENTERED.                                                
*                                                                               
         LA    R6,RCONREC          CHECK FOR SPL ELEMENT                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL            IF PRESENT, SKIP COMMENTS                    
         BE    ESARCMX             FOUND                                        
         LA    R2,SARSCM1H         ADD COMMENTS                                 
         LA    R3,4                CHECK 4 COMMENT LINES                        
         MVI   HALF,C'N'           SET 'NO COMMENTS'                            
*                                                                               
ESARCM10 CLI   5(R2),0                                                          
         BE    ESARCM20                                                         
*                                                                               
         MVI   HALF,C'Y'           SET 'COMMENTS PRESENT'                       
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK3+2(0),8(R2)    DATA TO ELEMENT                              
         AH    R1,=H'3'            ELEMENT LENGTH                               
         STC   R1,WORK3+1                                                       
         MVI   WORK3,X'11'                                                      
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK3                                      
*                                                                               
ESARCM20 DS    0H                                                               
         LA    R2,SARSCM2H-SARSCM1H(R2)       NEXT COMMENT                      
         LA    R0,SARSLCMH         LAST COMMENT                                 
         CR    R2,R0               PAST IT?                                     
         BNH   ESARCM10            NO                                           
*                                                                               
         CLI   HALF,C'N'           ANY COMMENTS?                                
         BNE   ESARCMX             YES - ACCEPTED                               
         TM    BYTE,X'80'+X'40'+X'20'+X'10'+X'08'                               
         BNZ   ESARCMX             YES                                          
         TM    PROFILES+CNTPCOMB,CNTPCOMA                                       
         BNZ   ESARCMX             COMMENTS ARE OPTIONAL                        
         LA    R3,383              NO  - COMMENTS MUST BE PRESENT               
         LA    R2,SARSCM1H                                                      
         B     ERROR                                                            
ESARCMX  DS    0H                                                               
         EJECT                                                                  
         L     R7,SAVUSING         ESTABLISH ADDRESSABILITY FOR                 
         USING TWAWORK,R7             TWA FOR THIS SET OF ROUTINES              
*                                                                               
* NEXT ROUTINE WRITES/REWRITES CONTRACT EC (SAR) KEY                            
*                                                                               
ESAR0360 EQU   *                                                                
         CLC   CONACT,=C'ADDS'                                                  
         BE    EXXMOD              ADDS T80211 CREATES POINTER                  
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                      CHECK 8D/8E PASSIVE POINTERS             
         GOTO1 =A(PPNTRS),DMCB,(RC),0,RR=Y                                      
*                                                                               
         USING RCONATYP,R5                                                      
         LA    R5,WORK                                                          
         XC    WORK(40),WORK       CREATE POINTER                               
         MVI   RCONATYP,X'EC'                                                   
*                                                                               
         MVC   RCONACON,TWACNUM    CONTRACT NUMBER 9'S COMP.                    
         MVC   RCONAREP,REPALPHA                                                
         MVC   KEY,RCONATYP                                                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    ESAR0370            ALREADY THERE                                
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
*                                                                               
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
         B     ESAR0380                                                         
         SPACE 1                                                                
ESAR0370 CLI   KEY+27,0                                                         
         BE    ESAR0380            NOT DELETED                                  
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
         SPACE 1                                                                
ESAR0380 DC    0H'0'                                                            
         GOTO1 =A(DEMOPROC),DMCB,(RC),RR=Y                                      
*                                                                               
         NI    TWASTAT,X'FD'       TURN OFF 02 - DISPLAYED                      
         NI    DMINBTS,X'F7'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
*   RETRANSMIT SCREEN FIELDS                                                    
*                                                                               
         B     DSAR0000            GO BACK AND REDISPLAY RECORD                 
         SPACE 1                                                                
         DROP  R7                                                               
CHECK    TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*    VALID DAYPARTS                                                             
DPTABLE  DS    0CL4                                                             
         DC    CL4'MNGM'            MORNING                                     
         DC    CL4'DAYD'            DAYTIME                                     
         DC    CL4'ELYE'            EARLY FRINGE                                
         DC    CL4'ENWR'            EARLY NEWS                                  
         DC    CL4'ACCA'            PRIME ACCESS                                
         DC    CL4'LNWT'            LATE NEWS                                   
         DC    CL4'LTEL'            LATE FRINGE                                 
         DC    CL4'WKDW'            WEEKEND                                     
         DC    CL4'KIDK'            KIDS                                        
         DC    CL4'FRGF'            FRINGE                                      
         DC    CL4'NWSN'            NEWS                                        
         DC    CL4'PRIP'            PRIME                                       
         DC    CL4'MOVV'            MOVIES                                      
         DC    CL4'SPES'            SPECIALS                                    
         DC    CL4'SPOJ'            SPORTS                                      
         DC    CL4'SPSO'            SOAPS                                       
         DC    CL4'COMU'            COMPETITIVE                                 
         DC    CL4'LOCX'            LOCAL                                       
         DC    X'FF'                                                            
*                                                                               
DPTFLAG  DS    XL1                                                              
DPTCNT   DS    F                                                                
GRPENTRY DS    XL1                                                              
CPPENTRY DS    XL1                                                              
*                                                                               
*  MAY AS WELL ADD A COUPLE MORE NON RE-ENTRANT VARIABLES                       
*    AND NOT MESS WITH THE TWA                                                  
SAVBOOKS DS    0CL(7*5)            TABLE OF SAVED BOOKS                         
SAVBOOK  DS    7CL5                                                             
*                                                                               
SAVLBLS  DS    0CL(7*5)            TABLE OF SAVED RENAME LABELS                 
SAVLBL   DS    7CL5                                                             
*                                                                               
SAVDPTS  DS    0CL(8*(1+4))        SAVED 1 BYTE DPT 4 BYTE CPP                  
SAVDPT   DS    8CL(1+4)                                                         
*                                                                               
SAVDEMOS DS    0CL(8*3)            SAVED DEMOS                                  
SAVDEMO  DS    8CL3                                                             
*                                                                               
MISCFLG1 DS    XL1                                                              
MF1DPTAB EQU   X'80'               USE HARDCODED TABLE FOR VALIDATION           
VPARSNIP DS    A                   V(PARSNIP) FROM COMFACS                      
         EJECT                                                                  
*                                                                               
*        LOCAL VARIABLES                                                        
*                                                                               
NAVALUE  DC    F'-1'                                                            
NAPACKED DC    PL8'-1'                                                          
TOTAMT   DC    F'0'                                                             
SAVASCRN DS    A                   SAVE FOR A(SCREEN FIELD)                     
SAVUSING DS    A                   SAVE FOR TWA 'USING' REGISTER                
ESTDOLS  DC    F'0'                                                             
ATWAWORK DS    A                                                                
COMMFLAG DC    C'N'                COMMENT FLAG                                 
FORSPRED DC    C'N'                SPREAD FORECAST DOLLARS                      
LOCALFLT DS    CL6                 REVISED HEADER FLIGHT DATES                  
*                                                                               
         DS    0F                                                               
*                   .0.0.0.0.0.0.0.0.0.1.1.1                                    
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
NEW08ELT DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
*                                                                               
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTF7D                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
       ++INCLUDE REGENRDP                                                       
         EJECT                                                                  
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                 BINARY MONTH START DATE                      
BRDEND   DS    XL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    XL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
         EJECT                                                                  
*              ROUTINE TO FOUT ENTIRE SCREEN                                    
*                                                                               
*              PARAMETER  1        A(FIRST FIELD)                               
*              PARAMETER  2        A(END-ADDR) (FIELD NOT CHECKED!)             
*                                                                               
         CSECT                                                                  
TRANOUT  NMOD1 0,*TOUT*                                                         
         L     RC,0(R1)            RELOAD A(WORKSPACE)                          
         LM    R2,R3,4(R1)         LOAD OPTIONS                                 
         SPACE 1                                                                
TOUT0010 ZIC   RE,0(R2)            LENGTH(SCREEN FIELD)                         
         TM    1(R2),X'20'         IS FIELD PROTECTED?                          
         BO    TOUT0020            YES - SKIP IT                                
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
TOUT0020 LA    R2,0(RE,R2)         BUMP TO NEXT FIELD                           
         CR    R2,R3               HAVE WE REACHED LAST FIELD?                  
         BL    TOUT0010            NO  - GO BACK FOR NEXT                       
         XMOD1                                                                  
*        SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ROUTINE TO VALIDATE STATION BUDGET                                          
*                                                                               
         DS    0F                                                               
VALSTBUD NMOD1 0,*VSTB*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET A(SAR ELEMENT)                         
         USING RSARXEL,R4                                                       
                                                                                
         LA    R2,SARSTBDH                                                      
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               ERROR - NOT NUMERIC                          
         MVC   WORK(4),4(R6)       USE BINARY VALUE                             
         LA    R3,291              STATION BUDGET CANNOT BE ZERO                
         OC    WORK(4),WORK        COMPARE FOR ZERO                             
         BZ    ERROR               MUST ENTER AN NON-ZERO VALUE                 
*                                                                               
* CALCULATE MARKET BUDGET = STATION BUDGET / SHARE GOAL                         
*                                                                               
         CLI   RSARXSHG,0          CHECK FOR DIVISION BY ZERO                   
         BNE   VALSTB10                                                         
         XC    RSARXBGT,RSARXBGT                                                
         OI    RSARXFLG,X'40'      ENTERED AS ZERO                              
         XC    SARSTBD,SARSTBD     FORCE STATION BUDGET TO ZERO                 
         MVI   SARSTBD,C'0'        SINCE SHARE GOAL IS 0%                       
         MVI   SARSTBDH+5,1                                                     
         OI    SARSTBDH+6,X'80'    XMIT                                         
         B     VALSTBX                                                          
                                                                                
VALSTB10 DS    0H                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,WORK                                                       
         MH    RF,=H'100'                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+3(1),RSARXSHG                                               
         D     RE,WORK                                                          
         STCM  RF,15,RSARXBGT                                                   
                                                                                
VALSTBX  DS    0H                                                               
         XMOD1                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   ROUTINE TO SPREAD FORECAST DOLLARS FROM SAR ELEMENT OVER FLIGHT             
*        OF ORDER.  CONTRACT FLIGHT DATES ARE USED, NOT SAR FLIGHT              
*        DATES.                                                                 
*                                                                               
         DS    0F                                                               
SPREDFOR NMOD1 0,*FORE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET A(SAR ELEMENT)                         
         USING RSARXEL,R4                                                       
*                                                                               
*   INITIALIZE WORKSPACE FOR FORECAST SPREADING....                             
*                                                                               
         XC    NEW23ELT,NEW23ELT   SET NEW ELEMENT                              
         MVC   NEW23ELT(2),=X'230A'                                             
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'23',RCONREC)                                    
*                                  DELETE EXISTING FORECAST BUCKETS             
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK)                                    
*                                  GET TODAY'S DATE EBCDIC                      
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                  GET DAY OF WEEK OF TODAY'S DATE              
         ZIC   R2,DMCB             SAVE DAY OF WEEK RETURNED                    
         BCTR  R2,0                MAKE DAY OF WEEK ZERO/MONDAY REL             
         LNR   R2,R2               NEGATE THE VALUE                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,NEW23ELT+4)                            
*                                  INSERT IT INTO NEW 23 ELEMENT                
         BAS   RE,GENDAYS          GENERATE DAYTABLE                            
         BAS   RE,SPREDAYS         GENERATE DAYS WITHIN TABLE                   
SFOR0020 EQU   *                                                                
         SR    RF,RF                                                            
         LA    R2,DAYTABLE         ACCUMULATE TOTAL DAYS                        
SFOR0030 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0040            YES                                          
         ZIC   RE,3(R2)            TAKE DAYS FROM TABLE                         
         AR    RF,RE               ACCUMULATE                                   
         LA    R2,4(R2)            BUMP TO NEXT ENTRY                           
         B     SFOR0030            GO BACK FOR NEXT                             
SFOR0040 EQU   *                                                                
         ST    RF,TOTDAYS          SAVE IT FOR LATER                            
         MVC   FULL,RSARXBGT       LOAD MARKET $$ BUDGET FIGURE                 
         L     RF,FULL                                                          
         ZIC   R2,RSARXSHG         ADJUST WITH SHARE GOAL                       
         MR    RE,R2               MULTIPLY MARKET $ BY SHARE GOAL              
*                                     GIVING STATION $$                         
*                                                                               
*   NOW MULTIPLY BY 10 FOR PROPER DECIMAL ALIGNMENT                             
*                                                                               
         M     RE,=F'10'           MULTIPLY BY 10                               
         L     R2,TOTDAYS          DIV STA $$ BY TOTAL DAYS                     
*                                     GIVING $$ PER DAY                         
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AR    RF,R2               ADD TOTDAYS FOR ROUNDING                     
         DR    RE,R2               DIVIDE BY TOTDAYS                            
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,TOTDAYS          SAVE $$ PER DAY                              
         LA    R2,DAYTABLE                                                      
SFOR0050 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0060            YES - FINISHED                               
         BAS   RE,GENBUCKS         NO  - GEN X'23' FORECAST BUCKET              
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         B     SFOR0050            GO BACK FOR NEXT                             
SFOR0060 EQU   *                                                                
         XC    MYP,MYP             CLEAR PRINT AREA FOR OTHERS                  
         XMOD1                                                                  
         EJECT                                                                  
GENDAYS  NTR1                                                                   
         MVC   CYCLEDAT,LOCALFLT   CONTRACT FLIGHT DATES                        
*                                                                               
*   EXTRA PRINT LINE IS USED TO SET UP BROADCAST MONTH ARRAY.                   
*                                                                               
         LA    R2,MYP              A(DAYTABLE)                                  
         XC    MYP,MYP             INITIALIZE TABLE                             
         USING BROADTBL,R2                                                      
GDAY0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,CYCLEDAT),(0,DAYTABLE)                            
*                                  CONVERT START DATE TO EBCDIC                 
GDAY0040 EQU   *                                                                
         GOTO1 VGTBROAD,DMCB,(1,DAYTABLE),DAYTABLE+6,GETDAY,ADDAY               
         MVC   BRDWEEKS,DMCB       INSERT NUMBER OF WEEKS                       
*                                  GET BROADCAST DATES FOR MONTH                
         CLI   DMCB,X'FF'          ERROR?                                       
         BNE   *+6                 NO                                           
         DC    H'0'                SHOULDN'T HAPPEN!!                           
         GOTO1 DATCON,DMCB,(0,DAYTABLE+6),(3,BRDSTART)                          
*                                  INSERT START DATE IN TABLE                   
         GOTO1 DATCON,DMCB,(0,DAYTABLE+12),(3,BRDEND)                           
*                                  INSERT END   DATE IN TABLE                   
         CLC   CYCLEDAT+3(3),BRDEND                                             
*                                  CONTRACT FLIGHT END REACHED?                 
         BNH   GDAY0060            YES                                          
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,DAYTABLE+6)                            
*                                  CONVERT END DATE TO EBCDIC                   
         LA    RF,1                DATE INCREMENT                               
         GOTO1 ADDAY,DMCB,DAYTABLE+6,DAYTABLE,(RF)                              
*                                  GET NEXT DAY, WHICH IS FIRST                 
*                                     DAY OF NEXT BDCST MONTH                   
         LA    R2,BRDLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         B     GDAY0040            GO BACK, SET NEXT MONTH                      
GDAY0060 EQU   *                                                                
         XC    DAYTABLE(56),DAYTABLE     CLEAR THE WORKAREA                     
         LA    R2,MYP              RESET A(BDCST MONTH TABLE)                   
         LA    R3,DAYTABLE                                                      
GDAY0080 EQU   *                                                                
         CLI   BRDEND,0            ANY ENTRY?                                   
         BZ    GDAY0100            NO  - FINISHED                               
         MVC   0(2,R3),BRDEND      MOVE BDCST MON END (YM) TO TABLE             
         LA    R2,BRDLEN(R2)       BUMP TO NEXT BDCST MONTH                     
         LA    R3,4(R3)            BUMP TO NEXT DAYTABLE                        
         B     GDAY0080            GO BACK FOR NEXT                             
GDAY0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
SPREDAYS NTR1                                                                   
         LA    R2,DAYTABLE         A(DAYTABLE)                                  
         LA    R3,MYP              A(BDCST MONTH TABLE)                         
         USING BROADTBL,R3                                                      
         CLC   BRDSTART,LOCALFLT   IS FLIGHT START FIRST DAY                    
*                                     OF FIRST BROADCAST MONTH?                 
         BE    SPDA0040            YES                                          
         GOTO1 DATCON,DMCB,(3,LOCALFLT),(0,WORK)                                
*                                  CONVERT FLIGHT START DATE                    
         CLC   LOCALFLT+3(3),BRDEND                                             
*                                  IS FLIGHT END DATE EARLIER                   
*                                     THAN BROADCAST MONTH END DATE?            
         BNL   SPDA0020            NO  -                                        
         GOTO1 DATCON,DMCB,(3,LOCALFLT+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
*                                                                               
*   AT THIS POINT, BOTH FLIGHT START AND END ARE WITHIN THE FIRST               
*     BROADCAST MONTH, SO THAT THE NUMBER OF DAYS CALCULATION IS                
*     DONE FROM FLIGHT START TO FLIGHT END .                                    
*                                                                               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0100            EXIT ROUTINE                                 
*                                                                               
*   AT THIS POINT, FLIGHT START IS OTHER THAN BEGINNING OF BDCST                
*     MONTH, AND FLIGHT END IS EITHER AFTER THE BDCST MONTH, OR                 
*     THE LAST DAY OF THE MONTH.  NUMBER OF DAYS IS CALCULATED                  
*     FROM FLIGHT START TO BDCST MONTH END.                                     
*                                                                               
SPDA0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,WORK+6)                                
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0080            FIRST ENTRY DONE                             
SPDA0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SPDA0100            YES                                          
         CLC   LOCALFLT+3(3),BRDEND                                             
*                                  END OF FLIGHT REACHED?                       
         BL    SPDA0060            YES - PARTIAL MONTH TO DO                    
         ZIC   RF,BRDWEEKS         NO  - CALCULATE DAYS FROM WEEKS              
         SR    RE,RE                                                            
         LA    R1,7                                                             
         MR    RE,R1               MULT WEEKS BY 7                              
         STC   RF,3(R2)            INSERT # DAYS INTO TABLE                     
         B     SPDA0080            GO TO NEXT SLOT                              
SPDA0060 EQU   *                                                                
*                                                                               
*   AT THIS POINT, FLIGHT END IS OTHER THAN END OF BROADCAST                    
*     MONTH.  NUMBER OF DAYS IS CALCULATED FROM BROADCAST MONTH                 
*     START DATE THROUGH FLIGHT END.                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,BRDSTART),(0,WORK)                                
*                                  CONVERT BROADCAST MONTH START                
         GOTO1 DATCON,DMCB,(3,LOCALFLT+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   2(2,R2),DMCB+8                                                   
*                                  MOVE NUM DAYS TO LAST TABLE NTRY             
         B     SPDA0100            FINISHED                                     
*                                                                               
SPDA0080 EQU   *                                                                
         LA    R2,4(R2)            BUMP DAYTABLE                                
         LA    R3,BRDLEN(R3)       BUMP BDCST MONTH TABLE                       
         B     SPDA0040            GO BACK FOR NEXT                             
SPDA0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
GENBUCKS NTR1                                                                   
*                                                                               
         MVC   NEW23ELT+2(2),0(R2) INSERT MONTH INTO 23 ELT                     
         SR    RE,RE                                                            
         ZIC   RF,3(R2)            NUMBER OF DAYS FOR MONTH *                   
         M     RE,TOTDAYS             $$ PER DAY = $$ FOR MONTH                 
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AH    RF,=H'10'           ADD FOR ROUNDING                             
         D     RE,=F'10'           DIVIDE FOR DECIMAL SCALING                   
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,FULL                                                          
         MVC   NEW23ELT+6(4),FULL  INSERT INTO X'23' ELEMENT                    
         GOTO1 VADDELEM,DMCB,RCONREC,NEW23ELT                                   
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET RECNT70    AT LEVEL 005 AS OF 02/22/96                      
         SPACE 2                                                                
* IF SAR DEMOS ADDED/CHANGED NEEDS TO SET 8D/8E PASSIVE POINTERS                
* USING WORK3 (CL240) AS WORK AREA                                              
*                                                                               
PPNTRS   NMOD1 0,**PSV**                                                        
         L     RC,0(R1)                                                         
*                                                                               
         XC    WORK3,WORK3                                                      
                                                                                
* BUILD 8D/8E POINTERS WITH CURRENT SAR DEMO                                    
* GET FLIGHT START/END DATES AND SAVE IN WORK3+140                              
         GOTO1 DATCON,DMCB,(3,LOCALFLT),(2,WORK3+140)    START DATE             
         GOTO1 DATCON,DMCB,(3,LOCALFLT+3),(2,WORK3+142)  END DATE               
                                                                                
* - GET DEMO FROM SAR ELEMENT AND SAVE IN WORK3+145                             
* - LOOK FOR DEMO MARKED AS PRIMARY (X'40' IN 1ST BYTE)                         
* - IF NO DEMO MARKED AS PRIMARY, USE 1ST DEMO AS DEFAULT                       
*                                                                               
         LA    R4,RCONREC                                                       
         LA    R4,34(R4)                                                        
PPC8DLP  CLI   0(R4),0                                                          
         BE    PPC8D00                                                          
         CLI   0(R4),X'12'         SAR ELEMENT                                  
         BE    PPC8DDD                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC8D00                                                          
         AR    R4,R1                                                            
         B     PPC8DLP             GO TO LOOP                                   
         USING RSARCO,R4                                                        
PPC8DDD  LA    RE,RSARDEM            DEMO                                       
         LA    RF,8                                                             
         MVC   WORK3+145(3),RSARDEM    DEMO                                     
PPC8DDE  TM    0(RE),X'40'         IS IT MARKED AS PRIMARY ?                    
         BO    PPC8DDF               YES                                        
         LA    RE,3(RE)                                                         
         MVC   WORK3+145(3),0(RE)                                               
         BCT   RF,PPC8DDE                                                       
         MVC   WORK3+145(3),RSARDEM    NO/USE 1ST AS DEFAULT                    
PPC8DDF  NI    WORK3+145,X'FF'-X'40'          CLEAR MAIN DEMO INDICATOR         
         B     PPC8D00                                                          
                                                                                
         DROP R4                                                                
                                                                                
PPC8D00  DS    0H                                                               
* GET DISK ADDRESS                                                              
         MVC   KEY,RCONREC                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     GET DISK ADDRESS                             
         BE    *+6                                                              
         DC    H'0'                CONTRACT REC HAS TO BE THERE                 
         MVC   FULL,KEY+28                                                      
                                                                                
* BUILD BASIC KEY                                                               
         LA    R4,WORK3                                                         
         MVC   1(2,R4),RCONKREP                                                 
         MVC   8(2,R4),WORK3+140    START DATE                                  
         MVC   10(2,R4),WORK3+142    END DATE                                   
         MVC   12(4,R4),RCONKCON   CONTRACT NUMBER                              
         MVI   16(R4),3                                                         
* DELETE OLD 8D KEY                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(27),WORK3                                                    
         MVI   KEY,X'8D'                                                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(17),KEYSAVE     IS KEY ON FILE ?                             
         BNE   PPC8E00                                                          
         MVI   KEY+27,X'80'        YES/DELETE IT                                
         GOTO1 VWRITE                                                           
                                                                                
* DELETE OLD 8E KEY                                                             
PPC8E00  MVC   KEY,WORK3                                                        
         MVI   KEY,X'8E'                                                        
         MVC   KEY+3(5),RCONKSTA                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(17),KEYSAVE     IS KEY ON FILE ?                             
         BNE   AP00                                                             
         MVI   KEY+27,X'80'        YES/DELETE IT                                
         GOTO1 VWRITE                                                           
                                                                                
                                                                                
AP00     DS    0H                                                               
                                                                                
* NEW KEY DATA                                                                  
                                                                                
         LA    R4,WORK3                                                         
         MVC   17(2,R4),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R4))                              
         MVC   19(3,R4),WORK3+145       DEMO                                    
*                                                                               
*ADD 8D KEY                                                                     
         MVC   KEY(27),WORK3                                                    
         MVI   KEY,X'8D'                                                        
         XC    KEY+3(5),KEY+3      CLEAR STATION FIELD                          
         BAS   R5,ADDNEW                                                        
                                                                                
* END X'8D' PASSIVE POINTERS                                                    
                                                                                
                                                                                
*   CREATE X'8E' POINTERS                                                       
*   SIMILAR TO X'8D' POINTERS BUT HAVE STATION IN KEY INSTEAD OF 0'S            
                                                                                
         MVC   KEY,WORK3                                                        
         MVI   KEY,X'8E'                                                        
         MVC   KEY+3(5),RCONKSTA                                                
         BAS   R5,ADDNEW                                                        
* END X'8E' PASSIVE POINTERS                                                    
                                                                                
         B     PNTX                                                             
                                                                                
                                                                                
* ADD NEW PTR                                                                   
ADDNEW   DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     AD50                                                             
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
         B     AD100                                                            
* ADD PTR                                                                       
AD50     MVI   KEY+27,0                                                         
         MVC   KEY+28(4),FULL      DISK ADDR                                    
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
AD100    BR    R5                                                               
*                                                                               
PNTX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOR NON-DARE ORDERS, CHECK IF PENDING DEMO CATEGORIES CHANGED                 
* IF SO, WE'LL NEED TO UPDATE ALL THE BUY RECORDS TO REFLECT THE CHANGE         
***********************************************************************         
DEMOPROC NMOD1 DEMPRDX-DEMPRD,*DEMPR*,CLEAR=YES                                 
         LR    R5,RC                                                            
         USING DEMPRD,R5                                                        
         L     RC,0(R1)                                                         
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'DD'        AGENCY DEMO PRESENT                          
         BAS   RE,GETEL                                                         
         BNE   DEMPR05                                                          
*                                                                               
         ZIC   RF,1(R6)                                                         
         SHI   RF,3                EX+OVERHEAD                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R6),MYSPACES                                                 
         BNE   DEMPRX              AND IT IS <> SPACES, EXIT                    
*                                                                               
DEMPR05  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        CHECK IF PENDING DEMOS CHANGED               
         BAS   RE,GETEL                                                         
         BNE   DEMPRX                                                           
         USING RSARXEL,R6                                                       
         CLC   SAVDEMOS,RSARXDEM                                                
         BE    DEMPRX                                                           
*                                                                               
         LA    R3,RSARXDEM                                                      
         LA    R4,8                                                             
*                                                                               
DEMPR10  DS    0H                                                               
         TM    0(R3),X'40'         PRIMARY?                                     
         BO    DEMPR20                                                          
         AHI   R3,3                                                             
         BCT   R4,DEMPR10                                                       
         LA    R3,RSARXDEM                                                      
*                                                                               
DEMPR20  DS    0H                                                               
         MVC   NEWPRIM,0(R3)                                                    
*                                                                               
         LA    R3,SAVDEMOS                                                      
         LA    R4,8                                                             
*                                                                               
DEMPR40  DS    0H                                                               
         TM    0(R3),X'40'         PRIMARY?                                     
         BO    DEMPR50                                                          
         AHI   R3,3                                                             
         BCT   R4,DEMPR40                                                       
         LA    R3,SAVDEMOS                                                      
*                                                                               
DEMPR50  DS    0H                                                               
         MVC   OLDPRIM,0(R3)                                                    
*                                                                               
         NI    OLDPRIM,X'FF'-X'40' REMOVE PRIMARY FLAG                          
         NI    NEWPRIM,X'FF'-X'40' REMOVE PRIMARY FLAG                          
         CLC   OLDPRIM,NEWPRIM                                                  
         BE    DEMPRX              PRIMARY DEMO CAT HAS NOT CHANGED             
*                                                                               
* DEMO CATEGORY HAS BEEN CHANGED                                                
* READ ALL BUYS AND REMOVE DEMO VALUE ELEMENTS                                  
*                                                                               
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         DROP  R6                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
*                                                                               
DEMPR60  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   DEMPR90             DONE                                         
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         MVI   ELCODE,X'0E'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEMPR70                                                          
         GOTO1 VDELELEM,DMCB,(X'0E',AIO4)                                       
         GOTO1 VPUTREC,DMCB,AIO4                                                
*                                                                               
DEMPR70  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     DEMPR60                                                          
*                                                                               
* DEMO CATEGORY HAS BEEN CHANGED                                                
* READ ALL OFFERS AND REMOVE DEMO VALUE ELEMENTS                                
*                                                                               
DEMPR90  DS    0H                                                               
         LA    R6,KEY                                                           
MGKEYD   USING RMKGREC,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   MGKEYD.RMKGKTYP,X'11'                                            
         MVC   MGKEYD.RMKGKREP,REPALPHA                                         
         MVC   MGKEYD.RMKGKOFF,RCONKOFF   INSERT OFFICE CODE                    
         MVC   MGKEYD.RMKGKSTA,RCONKSTA   INSERT STATION CALL LETTERS           
         MVC   MGKEYD.RMKGKCON,TWACNUM                                          
         DROP  MGKEYD                                                           
*                                                                               
DEMPR95  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
*                                                                               
DEMPR100 DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   DEMPRX              DONE                                         
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN,RMKGKPLN   HEADER RECORD?                               
         BNZ   DEMPR110            YES, CHECK IF APPLIED OR CANCELLED           
         TM    RMKGSCST,RMKGSAPQ+RMKGSCNQ                                       
         BNZ   DEMPR103            IF SO, SKIP TO NEXT GROUP                    
         GOTO1 VSEQ                                                             
         B     DEMPR100                                                         
         DROP  R6                                                               
*                                                                               
DEMPR103 DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGREC,R6                                                       
         CLI   RMKGKGR2,C'Z'                                                    
         BL    DEMPR105                                                         
         MVI   RMKGKGR2,C'A'                                                    
         ZIC   RF,RMKGKGR1                                                      
         AHI   RF,1                                                             
         STC   RF,RMKGKGR1                                                      
         B     DEMPR95                                                          
*                                                                               
DEMPR105 DS    0H                                                               
         ZIC   RF,RMKGKGR2                                                      
         AHI   RF,1                                                             
         STC   RF,RMKGKGR2                                                      
         B     DEMPR95                                                          
*                                                                               
DEMPR110 DS    0H                                                               
         MVI   ELCODE,X'0E'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEMPR120                                                         
         GOTO1 VDELELEM,DMCB,(X'0E',AIO4)                                       
         GOTO1 VPUTREC,DMCB,AIO4                                                
*                                                                               
DEMPR120 DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     DEMPR100                                                         
*                                                                               
DEMPRX   EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
DEMPRD   DSECT                                                                  
OLDPRIM  DS    XL3                 OLD PRIMARY DEMO                             
NEWPRIM  DS    XL3                 NEW PRIMARY DEMO                             
DEMPRDX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055RECNT70   07/21/03'                                      
         END                                                                    
