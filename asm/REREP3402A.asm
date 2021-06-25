*          DATA SET REREP3402A AT LEVEL 194 AS OF 05/01/02                      
*          DATA SET REREP3402  AT LEVEL 159 AS OF 05/03/00                      
*PHASE RE3402A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'RE3402 - REREP3402 - STATION MONTHLY SUMMARY REPORT'            
*                                                                               
       ++INCLUDE REREQPRF                                                       
*******************************************************************             
*                                                                 *             
*        REREP3402 --- STATION MONTHLY SUMMARY REPORT             *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* DEC04/90 (MRR) --- MODIFY FIRST BUCKET FROM VALUEMON WITH       *             
*                     ACCOUNTING OPTION ON TO BE ONLY CHANGES     *             
*                                                                 *             
* JAN17/91 (MRR) --- FURTHER FIX FROM DEC04/90                    *             
*                                                                 *             
* MAR19/91 (MRR) --- FIX BUG IN FIXPOST                           *             
*                                                                 *             
* MAY16/91 (MRR) --- FIX BUG IN FIXPOST                           *             
*                                                                 *             
* JUN06/91 (MRR) --- FIX BUG IN FIXPOST                           *             
*                                                                 *             
* JUL16/91 (MRR) --- FIX BUG IN FIXPOST                           *             
*                                                                 *             
* AUG12/91 (MRR) --- FIX BUG IN FIXPOST                           *             
*                                                                 *             
* OCT24/91 (BU ) --- UPGRADE TO USE NEW SPACEND FACILITIES/FORMAT *             
*                                                                 *             
* DEC03/91 (BU ) --- UPGRADE FOR VALUENEW FACILITY                *             
*                                                                 *             
* FEB07/92 (MRR) --- >PUT STATION/MARKET ON A SEPERATE LINE       *             
*                    >ADD HEADHOOK FOR PRINTING ACC PERIOD        *             
*                                                                 *             
* MAR25/92 (MRR) --- >DROP ACCUM STORE FOR COVAIL CALL            *             
*                                                                 *             
* MAR27/92 (BU ) ---  MAKE COMPATIBLE WITH VALU2NEW               *             
*                     REREPRGEQU ---> REREPRGEQA                  *             
*                                                                 *             
* AUG25/92 (BU ) ---  ENHANCED DIAGNOSTIC PRINTING CAPABILITIES   *             
*                     OF THE 'FIXPOST' ROUTINE.  CORRECTED CON-   *             
*                     TINUING AMBIGUITIES.  SPECIAL PRINTOUT WHEN *             
*                     REQUESTOR = 'PRINTIT'                       *             
*                                                                 *             
* SEP10/92 (BU ) ---  FIXED 'FIXPOST':  INVOICE-ONLY CONTRACTS    *             
*                     NOT ACCUMULATING PRIOR-PERIOD $ WITHIN      *             
*                     ACCTG PERIOD CORRECTLY                      *             
*                                                                 *             
* DEC17/92 (BU ) ---  INSTALL ADDITIONAL DISPLAY INFORMATION:     *             
*                     QOPTION3 = S = DISPLAY: SIGNIFICANT INFO    *             
*                                    FOUND+AGY/ADV/CON#           *             
*                              = T = DISPLAY: CONTRACT#, BUCKET#, *             
*                                    DOLLARS                      *             
*                                                                 *             
* AUG05/93 (BU ) ---  NON-ACCTING REQUESTS:  HONOR DATE RANGE     *             
*                                                                 *             
* MAR13/96 (BU ) ---  IMPLEMENT CONF/UNCONF FILTERING OF CONTRACTS*             
*                                                                 *             
* MAY21/97 (BU ) ---  UPGRADE FOR YEAR 2000                       *             
*                                                                   *           
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* MAY03/00 (BU ) --- EXPAND OFFICE LIST TABLE SPACE                 *           
*                                                                   *           
*                                                                   *           
*                     ***  END TOMBSTONE  ***                     *             
*******************************************************************             
*                                                                               
RE3402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3402,R7,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         L     R9,ACCUMS                                                        
*                                                                               
         STM   R2,RC,SAVEREGS                                                   
         EJECT                                                                  
*              CHECK MODE SETTINGS                                              
         SPACE 3                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   MN1                                                              
         L     RE,ADCONLST                                                      
         USING ADCONSD,RE                                                       
         L     RF,COVAIL                                                        
         DROP  RE                                                               
         GOTO1 (RF),DMCB,C'GET',400000,400000                                   
         OC    P2,P2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ACCUMS,P2                                                        
         L     RF,=F'400000'                                                    
         L     RE,ACCUMS                                                        
         XCEF                                                                   
         B     MNEXT                                                            
*                                                                               
MN1      EQU   *                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MN2                                                              
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   NMONTHS,0                                                        
         PACK  WORK+0(8),QSTART+2(2)                                            
         PACK  WORK+8(8),QEND+2(2)                                              
         CVB   RE,WORK+0                                                        
         CVB   RF,WORK+8                                                        
         CR    RE,RF                                                            
         BNH   MN1A                                                             
         AH    RF,=H'12'                                                        
MN1A     EQU   *                                                                
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,NMONTHS                                                       
*                                                                               
         XC    QSTBIN(3),QSTBIN                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(3,QSTBIN)                                
*                                                                               
         LA    RF,HHOOK                                                         
         ST    RF,HEADHOOK                                                      
*                                                                               
         BAS   RE,INITIAL                                                       
         BAS   RE,PROFILE          SET REQUEST PROFILE                          
         BAS   RE,OFFLST                                                        
         L     R9,ACCUMS                                                        
         XC    0(20,R9),0(R9)                                                   
         CLI   QOPTION1,C'C'                                                    
         BE    BLDID                                                            
         LH    R3,TOTLIN           TOTAL NUMBER OF LINES                        
         GOTO1 ROLLER,DMCB,0,(R9),(R3),13                                       
*                                                                               
         B     MNEXT                                                            
         SPACE 2                                                                
MN2      CLI   MODE,STAFRST                                                     
         BNE   MN3                                                              
         CLI   QOPTION1,C'C'                                                    
         BE    MNEXT                                                            
         MVC   P(8),SPACES                                                      
         MVC   P(4),RSTAKSTA                                                    
         LA    RE,P+3                                                           
         OI    0(RE),C' '                                                       
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-12                                                          
         MVI   1(RE),C'-'                                                       
         MVC   2(2,RE),=C'TV'                                                   
         CLI   RSTAKSTA+4,C' '                                                  
         BE    MN2A                                                             
         MVC   2(1,RE),RSTAKSTA+4                                               
         MVI   3(RE),C'M'                                                       
MN2A     EQU   *                                                                
         MVI   P+08,C'/'                                                        
         MVC   P+10(20),RSTAMKT                                                 
         MVC   LINESMKT(40),P                                                   
         MVC   P(40),SPACES                                                     
         B     MNEXT                                                            
         SPACE 2                                                                
MN3      CLI   MODE,SUBFRST                                                     
         BNE   MN4                                                              
         CLI   QOPTION1,C'C'                                                    
         BE    MNEXT                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     MNEXT                                                            
         SPACE 2                                                                
MN4      CLI   MODE,PROCCONT                                                    
         BNE   MN6                                                              
         BAS   RE,POST                                                          
         B     MNEXT                                                            
         SPACE 2                                                                
MN6      CLI   MODE,STALAST                                                     
         BNE   MN8                                                              
         CLI   QOPTION1,C'C'                                                    
         BE    MNEXT                                                            
         LA    R2,2                                                             
         BAS   RE,TOTALS                                                        
         B     MNEXT                                                            
         SPACE 2                                                                
MN8      CLI   MODE,SUBLAST                                                     
         BNE   MN10                                                             
         CLI   QOPTION1,C'C'                                                    
         BE    MNEXT                                                            
         CLC   QSTATION,SPACES     TEST 1 STATION ONLY                          
         BNE   MNEXT               IF YES, NO SUBGROUP TOTALS                   
         MVC   LINESMKT(40),SPACES                                              
         MVC   LINESMKT(8),=C'SUBGROUP'                                         
         MVC   LINESMKT+09(2),RGRPKGRP                                          
         MVI   LINESMKT+11,C'/'                                                 
         MVC   LINESMKT+13(10),RGRPNAME                                         
         MVC   LINESMKT+24(10),RGRPSBNM                                         
         LH    R2,LINARA           LINES IN AREA                                
         AH    R2,=H'2'            1ST LINE FOR SUBGROUP                        
         BAS   RE,TOTALS                                                        
         B     MNEXT                                                            
         SPACE 2                                                                
MN10     CLI   MODE,GRUPLAST                                                    
         BNE   MN11                                                             
         CLI   QOPTION1,C'C'                                                    
         BE    MNEXT                                                            
         CLC   QSTATION,SPACES     TEST 1 STATION ONLY                          
         BNE   MNEXT               IF YES, NO GROUP TOTALS                      
         CLI   QSBGROUP,C' '                                                    
         BNE   MNEXT                                                            
         MVI   FORCEHED,C'N'                                                    
         MVC   LINESMKT(40),SPACES                                              
         MVC   LINESMKT(5),=C'GROUP'                                            
         MVC   LINESMKT+06(1),RGRPKGRP                                          
         MVI   LINESMKT+08,C'/'                                                 
         MVC   LINESMKT+10(10),RGRPNAME                                         
         LH    R2,LINARA           GET 1ST LINE                                 
         MH    R2,=H'2'            FOR GROUP                                    
         AH    R2,=H'2'                                                         
         BAS   RE,TOTALS                                                        
         B     MNEXT                                                            
         SPACE 2                                                                
MN11     CLI   MODE,OFFLAST                                                     
         BNE   MN12                                                             
         CLI   QOPTION1,C'C'       TEST FOR COMMISSION REPORT                   
         BNE   MNEXT                                                            
         SPACE 1                                                                
*                                  PUT OUT COMMISSION CARD                      
         MVC   CARD,SPACES                                                      
         LA    R5,CARD                                                          
         USING CARDD,R5                                                         
*                                                                               
         MVC   CARD(4),=C'DATA'                                                 
         MVC   CRDOFF,ROFFKOFF                                                  
         MVC   CRDGRP,RGRPKGRP                                                  
         MVC   CRDSTA,RSTAKSTA                                                  
         LA    R5,CRDAMTS                                                       
         LA    R6,4                                                             
         LR    R3,R9               ACCUMS                                       
MN11A    L     R0,0(R3)                                                         
         CVD   R0,DOUBLE                                                        
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DOUBLE+7,X'0F'                                                   
         UNPK  0(10,R5),DOUBLE                                                  
         LA    R5,12(R5)                                                        
         LA    R3,4(R3)                                                         
         BCT   R6,MN11A                                                         
*                                                                               
         GOTO1 WORKER,DMCB,=C'ADD',WORKBUFF,ID,CRD1                             
         TM    DMCB+8,X'C0'        TEST FOR ERROR                               
         BZ    *+6                                                              
         DC    H'0'                BLOW UP IF WORKER RETURNS ERROR              
*                                                                               
         XC    0(16,R9),0(R9)                                                   
         B     MNEXT                                                            
         SPACE 2                                                                
MN12     CLI   MODE,REQLAST                                                     
         BNE   MN15                                                             
         CLI   QOPTION1,C'C'                                                    
         BE    MNEXT                                                            
         CLC   QSTATION,SPACES     NO TOTS ON 1 STATION REQUEST                 
         BNE   MN14                                                             
         CLI   QGROUP,C' '                                                      
         BNE   MN14                                                             
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(7),=C'REQUEST'                                                 
         LH    R2,LINARA           GET 1ST LINE                                 
         MH    R2,=H'3'            FOR REQUEST                                  
         AH    R2,=H'2'                                                         
         BAS   RE,TOTALS                                                        
         SPACE 1                                                                
MN14     MVI   RCSUBPRG,2                                                       
         MVI   FORCEFUT,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   RCSUBPRG,0                                                       
         SPACE 2                                                                
MN15     CLI   MODE,RUNLAST                                                     
         BNE   MNEXT                                                            
CLOSWRK  B     MNEXT     NOP AFTER BUILDID                                      
*                                                                               
CLOSID   GOTO1 WORKER,DMCB,=C'CLOSE',WORKBUFF,ID,CRD1                           
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     MNEXT                                                            
         SPACE 2                                                                
MNEXT    XMOD1 1                                                                
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              PROCESS A CONTRACT                                               
         SPACE 2                                                                
POST     NTR1                                                                   
*                                                                               
POST0020 EQU   *                                                                
         CLI   QOPTION4,C'+'       SECOND CARD IN USE?                          
         BNE   POST0030            NO                                           
         L     R1,ADCONLST                                                      
         L     RE,MASTC-ADCONSD(R1)                                             
         LA    R2,MCIO-MASTD(,RE)                                               
         CLI   QCONUNC-QREC2+80(R2),C' '                                        
*                                  CONFIRMED/UNCONFIRMED REQUESTED?             
         BE    POST0030            NO  - PROCESS ALL ORDERS                     
         CLI   QCONUNC-QREC2+80(R2),X'00'                                       
*                                  CONFIRMED/UNCONFIRMED REQUESTED?             
         BE    POST0030            NO  - PROCESS ALL ORDERS                     
         LA    R6,RCONREC          YES - FIND EXTENDED DESCRIP ELT              
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   POST0030            NOT FOUND - PROCESS ORDER                    
         TM    RCONCONF-RCONXEL(R6),X'40'                                       
*                                  CONFIRMED NOW?                               
         BO    POST0022            YES - ACCEPT IF OPTION CONFIRMED             
         TM    RCONCONF-RCONXEL(R6),X'20'                                       
*                                  CONFIRMED PREVIOUSLY?                        
         BO    POST0022            YES - ACCEPT IF OPTION CONFIRMED             
*                                  NO  - ACCEPT IF OPTION UNCONFIRMED           
         CLI   QCONUNC-QREC2+80(R2),C'U'                                        
         BE    POST0030                                                         
         B     XIT                                                              
POST0022 EQU   *                                                                
         CLI   QCONUNC-QREC2+80(R2),C'C'                                        
         BNE   XIT                                                              
*                                                                               
POST0030 EQU   *                                                                
         MVI   TESTDISP,X'00'      SET FLAG OFF                                 
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
         CLI   QOPTION1,C'C'                                                    
         BE    POST0300                                                         
         GOTO1 ROLLER,DMCB,1,(R9),1                                             
         L     R3,DMCB                                                          
         LA    R4,12               SET DISPLAY LOOP COUNTER                     
*                                     FOR ACCOUNTING REPORTS                    
         CLI   QACCTOPT,C'A'       ACCOUNTING REPORT?                           
         BE    POST0040            YES                                          
         ZIC   R4,NMONTHS          NO  - LOAD NUMBER OF MONTHS                  
POST0040 EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0060            FOUND - BEGIN TO PULL FIGURES                
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST0040                                                         
         SPACE 2                                                                
POST0060 EQU   *                                                                
         LR    R6,R2               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         CLI   QACCTOPT,C'A'                                                    
         BNE   POST0080                                                         
         L     R5,PRASATOR(R6)     PRIOR PERIOD AS AT ORDERED                   
         CLI   NMONTHS,12                                                       
         BNE   POST0100                                                         
         CH    R4,=H'12'           FIRST-TIME SWITCH                            
         BNE   POST0100                                                         
         XC    FULL,FULL                                                        
         BAS   RE,FIXPOST          PERFORM ONLY ON FIRST PASS                   
         CLI   QOPTION3,C'T'       DISPLAY WHERE INFO COMES FROM                
         BNE   POST0070                                                         
         BAS   RE,POSTDIS2                                                      
POST0070 EQU   *                                                                
         L     R5,FULL                                                          
         B     POST0100                                                         
*                                                                               
POSTDIS2 NTR1                                                                   
         MVC   P+1(10),=C'FIXPOST : '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+12,4,=C'TOG'                              
         MVC   P+25(08),=C'BUCKET# '                                            
         LA    R2,13               CALCULATE BUCKET NUMBER                      
         SR    R2,R4               SUBTRACT LOOP COUNTER                        
         EDIT  (R2),(2,P+33)                                                    
         EDIT  (R5),(12,P+40),COMMAS=YES,MINUS=YES                              
         GOTO1 REPORT                                                           
         XIT                                                                    
*                                                                               
POST0080 EQU   *                                                                
*                                                                               
*                                  THIS IS A CURRENT ESTIMATE BUCKET            
*                                  (BEST CURRENT FIGURE)                        
         L     R5,TOTORD(R6)       CURR AS AT EST (TOTAL ORDERED)               
*                                                                               
*   SPECIAL OPTION TESTING:  IF 3RD BIT OF REQUEST PROFILE IS ON,               
*        CHECK STATUS OF ORDER.  IF UNCONFIRMED, DO NOT USE INVOICE             
*        $$ EVEN IF ENTERED.  UNCONFIRMED IS DEFINED AS 'NOT PRES-              
*        ENTLY OR PREVIOUSLY CONFIRMED'.                                        
*                                                                               
         TM    SVPGPBIT,X'20'      OPTION: USE ORDERED FIGURE ONLY?             
         BNO   POST0090            NO  - CHECK ORDER CONFIRMATION               
*                                     ORDER IS UNCONFIRMED                      
         BAS   RE,CHKCONF          CHECK CONFIRMED/UNCONFIRMED                  
         BZ    POST0090            CONFIRMED/PREV CONFIRMED                     
         B     POST0100            UNCONFIRMED:  NO CHECK                       
*                                                                               
CHKCONF  NTR1                                                                   
         LA    R6,RCONREC          YES - FIND EXTENDED DESCRIP ELT              
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKC0020            NOT FOUND - EXIT CC NOT ZERO                 
         TM    RCONCONF-RCONXEL(R6),X'40'                                       
*                                  CONFIRMED NOW?                               
         BO    CHKC0040            YES - EXIT CC = ZERO                         
         TM    RCONCONF-RCONXEL(R6),X'20'                                       
*                                  CONFIRMED PREVIOUSLY?                        
         BO    CHKC0040            YES - EXIT CC = ZERO                         
*                                  NO  - ACCEPT IF OPTION UNCONFIRMED           
CHKC0020 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CHKC0060                                                         
CHKC0040 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
CHKC0060 EQU   *                                                                
         XIT1                                                                   
*                                                                               
POST0090 EQU   *                                                                
         TM    FLAG6(R2),X'01'     ANY CURR INV?                                
         BZ    POST0100                                                         
         L     R5,CUASATIN(R6)     CURR AS AT INV                               
POST0100 ST    R5,0(R3)            MOVE 12 BUCKETS TO LINE 1                    
         LTR   R5,R5               ANY VALUE?                                   
         BZ    POST0120            NO                                           
         CLI   QOPTION3,C'T'       DISPLAY WHERE INFO COMES FROM                
         BNE   POST0120                                                         
         BAS   RE,POSTDISP                                                      
         B     POST0120                                                         
*                                                                               
POSTDISP NTR1                                                                   
         MVC   P+1(10),=C'CONTRACT: '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+12,4,=C'TOG'                              
         MVC   P+25(08),=C'BUCKET# '                                            
         LA    R2,13               CALCULATE BUCKET NUMBER                      
         SR    R2,R4               SUBTRACT LOOP COUNTER                        
         EDIT  (R2),(2,P+33)                                                    
         EDIT  (R5),(12,P+40),COMMAS=YES,MINUS=YES                              
         GOTO1 REPORT                                                           
         XIT                                                                    
*                                                                               
POST0120 EQU   *                                                                
         CH    R4,=H'1'            LAST BUCKET?                                 
         BH    POST0140            NO                                           
         LTR   R5,R5               ANY VALUE IN BUCKET?                         
         BZ    POST0140            NO                                           
         MVI   TESTDISP,X'01'      YES - SET FLAG ON                            
POST0140 EQU   *                                                                
         LA    R2,NEXTBUCK(R2)                                                  
         LA    R3,4(R3)                                                         
         BCT   R4,POST0060                                                      
*                                                                               
POST0160 EQU   *                                                                
         CLI   TESTDISP,X'00'      FLAG SET?                                    
         BNE   POST0180            NO  - SKIP DISPLAY                           
         CLI   QOPTION3,C'S'       SHORT DISPLAY ASKED FOR?                     
         BNE   POST0180            NO  - SKIP DISPLAY                           
         BAS   RE,DISPCON          YES - DISPLAY CONTRACT INFO                  
POST0180 EQU   *                                                                
*                                  CROSSCAST AND ADD TO TOTALS                  
         GOTO1 ROLLER,DMCB,5,(R9),1                                             
         L     R2,AOFFLIST         FIND OFFICE LINE NUMBER (0-19)               
         LH    R3,NUMOFF                                                        
         SPACE 2                                                                
POST0200 CLC   0(20,R2),ROFFNAME                                                
         BE    POST0220                                                         
         LA    R2,20(R2)                                                        
         LA    R4,1(R4)                                                         
         BCT   R3,POST0200                                                      
         DC    H'0'                                                             
         SPACE 2                                                                
*                                  REGULAR TOTALS                               
POST0220 CLI   QOPTION2,C'C'       TEST COMBNED TOTS WANTED                     
         BE    POST0260                                                         
         CLI   RCONTYPE,C'N'       TEST NETWORK CONTRACT                        
         BE    POST0240                                                         
         LA    R5,2                                                             
         AR    R5,R4                                                            
         GOTO1 (RF),(R1),4,,,(R5)  ADD TO OFFICE/STATION                        
         BAS   R8,NXTSET           ADD TO OFFICE -SBGRP/FROUP/OVERALL           
         SPACE 1                                                                
         LH    R5,LINTYP           LINES PER TYPE                               
         AH    R5,=H'1'                                                         
         ST    R5,DMCB+12                                                       
         BASR  RE,RF               ADD TO TOTAL STATION                         
         BAS   R8,NXTSET           ADD TO TOTAL . SBGRP/GROUP/OVERALL           
         SPACE 2                                                                
*                                  TYPED TOTALS                                 
POST0240 CLC   RCONTYPE,QOPTION2   TEST TYPT & TOT AGREE                        
         BNE   POST0260                                                         
*                                                                               
         LH    R5,LINTYP                                                        
         AH    R5,=H'2'                                                         
         AR    R5,R4                                                            
         GOTO1 (RF),(R1),4,,,(R5)  ADD TO OFFICE/STATION                        
         BAS   R8,NXTSET           ADD TO OFFICE -SBGRP/GROUP/OVERALL           
         SPACE 1                                                                
         LH    R5,LINTYP                                                        
         MH    R5,=H'2'                                                         
         AH    R5,=H'1'                                                         
         ST    R5,DMCB+12                                                       
         BASR  RE,RF               ADD TO TOTAL  STATION                        
         BAS   R8,NXTSET           ADD TO TOTAL -SBGRP/GROUP/OVERALL            
         SPACE 2                                                                
*                                  COMPLETE STATION TOTALS                      
POST0260 CLI   QOPTION2,C'C'       TEST COMBINED ONLY                           
         BE    POST0280                                                         
         CLI   QCONTYPE,C' '       NOT C TOT, CHECK IF Q-TYPED                  
         BNE   XIT                 IF IT IS, DONT PUT IT TOT                    
         CLI   RCONTYPE,C'N'       NO Q TYPE, IS IT NETWORK                     
         BNE   POST0280            NO, THEN PUT IN TOT                          
         CLC   RCONTYPE,QOPTION2   AND ACCEPT OTHER  MATCHING TYPES             
         BNE   XIT                                                              
*                                                                               
POST0280 LH    R5,LINTYP                                                        
         MH    R5,=H'2'                                                         
         AH    R5,=H'2'                                                         
         AR    R5,R4                                                            
         GOTO1 (RF),(R1),4,,,(R5)  ADD TO OFFICE/STATION                        
         BAS   R8,NXTSET           ADD TO OFFICE -SBGRP/GROUP/OVERALL           
         SPACE 1                                                                
         LH    R5,LINTYP                                                        
         MH    R5,=H'3'                                                         
         AH    R5,=H'1'                                                         
         ST    R5,DMCB+12                                                       
         BASR  RE,RF               ADD TO TOTAL STATION                         
         BAS   R8,NXTSET           ADD TO TOTAL -SBGRP/GROUP/OVERALL            
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
POST0300 EQU   *                                                                
         LR    R3,R9                                                            
POST0320 EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0340            FOUND - BEGIN TO PULL FIGURES                
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST0320                                                         
         SPACE 2                                                                
POST0340 CLI   0(R2),0             ANY MORE MONTHS?                             
         BE    XIT                                                              
         CLC   0(4,R2),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    XIT                 TABLE > END DATE - EXIT                      
         LR    R6,R2               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         CLC   0(4,R2),QFAEND      MONTH OF REPORT?                             
         BE    POST0380                                                         
         CLC   NEXTBUCK(4,R2),QFAEND                                            
*                                  MONTH PRIOR TO MONTH OF REPORT               
         BE    POST0360                                                         
*                                                                               
         L     R5,PRASATOR(R6)     ACCOUNTING AMOUNT/ADJUSTMENTS                
         A     R5,0(R3)                                                         
         ST    R5,0(R3)                                                         
         B     POST0050                                                         
         SPACE 1                                                                
POST0360 L     R5,GROSSORD(R6)     INVOICE (GROSS ORDERED)                      
         TM    FLAG6(R2),X'01'     TEST ANY INVOICE DATA                        
         BO    *+12                                                             
         L     R5,TOTORD(R6)       TOTAL ORDERED                                
         A     R5,CUASATIN(R6)     CURR AS AT INV                               
         A     R5,4(R3)                                                         
         ST    R5,4(R3)                                                         
         B     POST0050                                                         
         SPACE 1                                                                
POST0380 L     R5,GROSSORD(R6)     INVOICE (GROSS ORDERED)                      
         TM    FLAG6(R2),X'01'     TEST ANY INVOICE DATA                        
         BO    *+12                                                             
         L     R5,TOTORD(R6)       TOTAL ORDERED                                
         A     R5,CUASATIN(R6)     CURR AT AS INV                               
         A     R5,8(R3)                                                         
         ST    R5,8(R3)                                                         
         SPACE 1                                                                
POST0050 LA    R2,NEXTBUCK(R2)                                                  
         B     POST0340                                                         
         SPACE 1                                                                
*                                  ADD TO SUBGRP/GROUP/OVERALL                  
NXTSET   LH    R3,NUMAREA          NUMBER OF AREAS                              
         BCTR  R3,0                                                             
         AH    R5,LINARA           LINES IN AN AREA                             
         ST    R5,DMCB+12                                                       
         BASR  RE,RF                                                            
         BCT   R3,*-10                                                          
         BR    R8                                                               
         EJECT                                                                  
*                                                                               
*        DISPCON --- DISPLAY PERTINENT CONTRACT INFO UPON REQUEST               
*                                                                               
DISPCON  NTR1                                                                   
         MVC   P+4(20),=C'SIGNIFICANT $ FOR:  '                                 
         MVC   P+28(6),RCONKAGY                                                 
         MVC   P+36(4),RCONKADV                                                 
         MVC   P+42(4),RCONKCON                                                 
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        FIXPOST --- IF THE ACCOUNTING OPTION IS 'A' AND IT IS THE              
*                    FIRST MONTH, THEN ONLY REPORT THE ACTIVITY FOR             
*                    THE EOM PERIOD AND NOT THE TOTAL DOLLARS                   
*                                                                               
FIXPOST  NTR1                                                                   
*                                                                               
         L     R9,AMONARCH                                                      
         USING MONARCHD,R9                                                      
*                                                                               
         XC    FULL,FULL                                                        
*                                                                               
         L     R3,=F'-2'                USE R3 TO HOLD EST $                    
         L     R4,=F'-2'                USE R4 TO HOLD INV $.                   
         SR    R5,R5                    USE R5 AS A FLAG                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL            GET 1ST ESTIMATE BUCKET                      
         BNE   FIXP0290            NONE FOUND - CHECK INVOICE BUCKETS           
*                                                                               
FIXP0010 EQU   *                                                                
         CLC   2(2,R6),QSTBIN      MON OF SERVICE 1ST MON OF REPORT?            
         BH    FIXP0260            NO  - ESTIMATE BUCKETS FINISHED              
         LR    R8,R6               SAVE POINTER TO THIS EL                      
FIXP0020 EQU   *                                                                
         SR    RF,RF                                                            
         ZICM  RE,6(R6),4          SET ESTIMATE $ AMOUNT                        
         BZ    FIXP0030            NO DOLLARS                                   
         SRDA  RE,31               SHIFT TO LOW REG, DOUBLE VALUE               
         D     RE,=F'100'          DIVIDE BY 100: DROP CENTS                    
         LTR   RF,RF               VALUE NEGATIVE?                              
         BM    *+8                 YES- DON'T ROUND                             
         AH    RF,=H'1'            NO  - ROUND                                  
         SRA   RF,1                DIVIDE BY TWO                                
FIXP0030 EQU   *                                                                
         C     R3,=F'-2'           FIRST PASS?                                  
         BNE   FIXP0040            NO                                           
         SR    R3,R3               YES - INITIALIZE ACCUMULATOR                 
FIXP0040 EQU   *                                                                
         CLC   2(2,R6),QSTBIN      BUCKET FOR 1ST MONTH?                        
         BNE   FIXP0050            NO  - ADD IT AND CONTINUE                    
*                                                                               
*   ESTIMATE DOLLARS FOR FIRST MONTH MUST BE WITHING ACCOUNTING                 
*     PERIOD IF NO CORRESPONDING INVOICE BUCKET EXISTS FOR IT.                  
*                                                                               
         BAS   RE,INVBUCK          LOOK FOR INVOICE BUCKET                      
         CLI   P4,0                CORRESPONDING X'04' FOUND?                   
         BZ    FIXP0050            YES - ACCUMULATE DOLLARS                     
         CLC   4(2,R6),KEYMONTH    NO  - BUCKET W/IN ACCTING PERIOD?            
         BL    FIXP0070            NO  - EARLIER - SKIP IT                      
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FIXP0080            LATER - ESTIMATE DOLLARS FINISHED            
FIXP0050 EQU   *                                                                
         STCM  RF,15,WORK                                                       
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0060            **TEST**                                     
         MVC   P+55(14),=C'EST $ TO ACCUM'                                      
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0060 EQU   *                                                                
         AR    R3,RF               ADD PRIOR PERIOD $ TO ACCUM                  
         CLC   4(2,R6),KEYMONTH    BUCKET WITHIN ACCOUNTING PERIOD?             
         BL    FIXP0070            NO  - EARLIER - SKIP IT                      
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FIXP0080            LATER - ESTIMATE DOLLARS FINISHED            
         LA    RE,1                                                             
         OR    R5,RE               SET FLAG FOR ESTIMATE DOLLARS                
         STCM  RF,15,WORK          FOR 'PRINTIT' ROUTINE                        
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0070            **TEST**                                     
         MVC   P+55(15),=C'EST $ IN PERIOD'                                     
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0070 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ESTIMATE BUCKET                     
         BNE   FIXP0080            NOT FOUND -                                  
         CLC   0(4,R8),0(R6)       SAME MONTH OF SERVICE?                       
         BE    FIXP0020            YES - PROCESS IT                             
FIXP0080 EQU   *                                                                
         LA    R6,RCONREC          NO  - RETRIEVE INV ELEMENTS                  
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   FIXP0170            NO INV ELEMENTS - CHECK RESULT               
FIXP0090 EQU   *                                                                
         CLC   2(2,R8),2(R6)       MON OF SERVICE SAME AS X'03' ELT?            
         BNE   FIXP0160            NO  - SKIP IT                                
         CLC   4(2,R6),KEYMONTH+2  BUCKET BEYOND ACCTING PERIOD?                
         BH    FIXP0170            YES - CHECK RESULT                           
         SR    RF,RF                                                            
         ZICM  RE,6(R6),4                                                       
         BZ    FIXP0100                                                         
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
FIXP0100 EQU   *                                                                
         C     R4,=F'-2'              FIRST PASS - INV $ ?                      
         BNE   FIXP0110               NO                                        
         SR    R4,R4                  YES - INITIALIZE ACCUMULATOR              
FIXP0110 EQU   *                                                                
         AR    R4,RF               ADD DOLLARS TO ACCUMULATOR                   
         STCM  RF,15,WORK                                                       
         CLC   4(2,R6),KEYMONTH    BUCKET WITHIN ACCTING PERIOD?                
         BL    FIXP0150            NO  - EARLIER - CHECK EST $                  
         LA    RE,2                SET FLAG 'ON'                                
         OR    R5,RE                  DITTO                                     
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0120            **TEST**                                     
         MVC   P+55(15),=C'INV $ IN PERIOD'                                     
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0120 EQU   *                                                                
         CLC   4(2,R8),4(R6)        ACTIVITY IN SAME WEEK?                      
         BE    FIXP0130             YES - INV REPLACES ORDERED                  
         CLC   4(2,R8),KEYMONTH     NO  - ORDERED IN KEYMONTH?                  
         BL    FIXP0160             NO  - SKIP IT                               
         CLC   4(2,R8),KEYMONTH+2      DITTO                                    
         BH    FIXP0160                                                         
FIXP0130 EQU   *                                                                
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0140            **TEST**                                     
         MVC   P+55(16),=C'EST SET TO ZERO '                                    
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0140 EQU   *                                                                
         L     R3,=F'-2'           RESET 'NO EST $' FLAG                        
         B     FIXP0160            GET NEXT INV BUCKET                          
FIXP0150 EQU   *                                                                
         C     R3,=F'-2'                                                        
         BE    FIXP0160                                                         
         LR    R3,R4               IF INV IS IN PAST, USE BEST DOLLAR           
         LA    RE,2                                                             
         NR    R5,RE               AND UPDATES TO ORDERED ARE USELESS           
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0160            **TEST**                                     
         MVC   P+55(16),=C'INV REPLACES EST'                                    
         BAS   RE,PRINTIT          **TEST**                                     
         B     FIXP0160                                                         
FIXP0160 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BE    FIXP0090                                                         
FIXP0170 EQU   *                                                                
         LTR   R5,R5                FLAG SET?                                   
         BNZ   FIXP0190             NOPE, SKIP THIS MONTH                       
         XC    WORK(4),WORK                                                     
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0180            **TEST**                                     
         MVC   P+55(23),=C'NOT IN PER: MON SKIPPED'                             
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0180 EQU   *                                                                
         B     FIXP0240                                                         
FIXP0190 EQU   *                                                                
         C     R3,=F'-2'                                                        
         BE    FIXP0230                                                         
         C     R4,=F'-2'            INV $ STILL SET TO NOT USED?                
         BE    FIXP0210             YUP, NO NET AMOUNT FOR B.C.                 
         SR    R4,R3                GET NET AMOUNT FOR THE B.C. MONTH           
         ST    R4,WORK                                                          
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0200            **TEST**                                     
         MVC   P+55(13),=C'INV MINUS EST'                                       
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0200 EQU   *                                                                
         B     FIXP0230                                                         
FIXP0210 EQU   *                                                                
         MVI   WORK+4,X'F6'                                                     
         ST    R3,WORK                                                          
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0220            **TEST**                                     
         MVC   P+55(16),=C'EST REPLACES INV'                                    
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0220 EQU   *                                                                
         LR    R4,R3                                                            
FIXP0230 EQU   *                                                                
         A     R4,FULL              ADD IN RUNNING TOTAL                        
         ST    R4,FULL              STORE IT                                    
FIXP0240 EQU   *                                                                
         L     R3,=F'-2'                                                        
         L     R4,=F'-2'                                                        
         SR    R5,R5                                                            
         MVI   ELCODE,X'03'                                                     
         LR    R6,R8               RE-SET TO LAST X'03' EL                      
FIXP0250 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   FIXP0260                                                         
         CLC   0(4,R6),0(R8)      MAKE SURE WE'RE NOT ON THE NEXT MONTH         
         BE    FIXP0250                                                         
         B     FIXP0010                                                         
*                                                                               
FIXP0260 EQU   *               PROCESS X'04' ELS NOT COVERED BY X'03'           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL            GET FIRST X'04' ELEMENT                      
         BNE   FIXPEXIT            NONE FOUND - EXIT                            
         B     FIXP0280                                                         
FIXP0270 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT X'04' ELEMENT                       
         BNE   FIXPEXIT            NO MORE - FINISHED                           
FIXP0280 EQU   *                                                                
         CLC   2(2,R6),QSTBIN      MON OF SERVICE 1ST MON OF REPORT?            
         BH    FIXPEXIT            NO  - FINISHED                               
         CLC   4(2,R6),KEYMONTH    BUCKET WITHIN ACCOUNTING PERIOD?             
         BL    FIXP0270            NO  - EARLIER - SKIP IT                      
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FIXP0270            NO  - LATER   - SKIP IT                      
         XC    P4(4),P4            FIND CORRESPONDING X'03' ELEMENT             
         GOTO1 =V(HELLO),P1,(C'G',=C'REPFIL'),(X'03',RCONREC),         X        
               (2,2(R6)),RR=RELO                                                
         CLI   P4,0                CORRESPONDING X'03' FOUND?                   
         BE    FIXP0270            YES - SKIP INVOICED BUCKET                   
         ZICM  RE,6(R6),4          NO  - USE  INVOICED BUCKETDOLLARS            
         BZ    FIXP0270            NO DOLLARS - SKIP BUCKET                     
         SR    RF,RF               ADJUST, ROUND DOLLARS, ETC                   
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         L     RE,FULL             LOAD RUNNING TOTAL                           
         AR    RE,RF               ADD INVOICE DOLLARS TO TOTAL                 
         ST    RE,FULL             SAVE RUNNING TOTAL                           
         B     FIXP0270                                                         
*                                                                               
*        SEPARATE PROCESSING FOR CONTRACTS WITH ONLY INV ELEMENTS               
*                                                                               
FIXP0290 EQU   *                                                                
         SR    R4,R4                    USE R4 TO HOLD INV $                    
         SR    R5,R5                    USE R5 AS A FLAG                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL            GET FIRST INVOICE ELEMENT                    
         BNE   FIXPEXIT            NONE FOUND - GET OUT                         
*                                                                               
FIXP0300 EQU   *                   PROCESS AN INVOICE ELEMENT                   
         SR    R3,R3                                                            
         CLC   2(2,R6),QSTBIN      MONTH OF SERVICE = 1ST MONTH?                
         BH    FIXPEXIT            NO  - AFTER START MONTH - DONE               
         LR    R8,R6               SAVE A(X'04' ELEMENT)                        
FIXP0310 EQU   *                                                                
         SR    RF,RF               INITIALIZE FOR MATH                          
         ZICM  RE,6(R6),4          TAKE AMOUNT OF INV BUCKET                    
         BZ    FIXP0320            NO DOLLARS                                   
         SRDA  RE,31               DROP PENNIES, ROUND AS NEEDED                
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
FIXP0320 EQU   *                                                                
         CLC   4(2,R6),KEYMONTH    WITHIN ACCOUNTING PERIOD?                    
         BL    FIXP0330            NO  - PRIOR - SKIP IT                        
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FIXP0340            NO  - AFTER - CHECK FLAG                     
         LA    R5,1                WITHIN PERIOD - SET FLAG                     
         AR    R4,RF               ADD DOLLARS TO ACCUMULATOR                   
         STCM  RF,15,WORK          STORE DOLLARS OFF                            
         MVI   WORK+4,X'F4'           FOR PRINT ROUTINE                         
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FIXP0330            **TEST**                                     
         BAS   RE,PRINTIT          **TEST**                                     
FIXP0330 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT X'04' ELEMENT                       
         BNE   FIXP0340            NONE FOUND - CHECK FLAG                      
         CLC   0(4,R8),0(R6)       SAME MONTH?                                  
         BE    FIXP0310            YES - PROCESS IT                             
         LR    R3,R6               NO  - THIS MONTH IS FINISHED                 
FIXP0340 EQU   *                                                                
         LTR   R5,R5                FLAG SET?                                   
         BZ    FIXP0350             NO  - SKIP THIS MONTH                       
         A     R4,FULL              ADD IN RUNNING TOTAL                        
         ST    R4,FULL              STORE IT                                    
FIXP0350 EQU   *                                                                
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         CR    R3,R6                                                            
         BE    FIXP0300                                                         
*                                                                               
*        FIXPOST EXIT                                                           
*                                                                               
FIXPEXIT EQU   *                                                                
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   FPOSTX01            **TEST**                                     
         BAS   RE,PRINTIT2         **TEST**                                     
FPOSTX01 EQU   *                                                                
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R9                                                               
         EJECT                                                                  
*                                                                               
*   INVBUCK:  LOOKS FOR CORRESPONDING INVOICE ELEMENT FOR ESTIMATE              
*      ELEMENT.  IF FOUND, P4 IS ZERO, AND IS RETURNED                          
*                                                                               
INVBUCK  NTR1                                                                   
         XC    P4(4),P4                                                         
         GOTO1 =V(HELLO),P1,(C'G',=C'REPFIL'),(X'04',RCONREC),         X        
               (2,2(R6)),RR=RELO                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINTIT --- PRINT THE BUCKET ABOUT TO BE USED                          
*                                                                               
PRINTIT  NTR1                                                                   
*                                                                               
         MVC   P+2(4),=C'EST$'     SET EST $                                    
         CLI   0(R6),3             ESTIMATE BUCKET?                             
         BE    PIT10               YES                                          
         MVC   P+2(4),=C'INV$'     NO  - SET INV $                              
PIT10    EQU   *                                                                
         MVC   WORK+6(2),2(R6)                                                  
         MVI   WORK+8,X'01'                                                     
         MVC   P+08(03),=C'BCM'                                                 
         GOTO1 DATCON,DMCB,(3,WORK+6),(6,P+12)                                  
         MVC   P+19(03),=C'A/D'                                                 
         GOTO1 DATCON,DMCB,(2,4(R6)),(5,P+23)                                   
         GOTO1 HEXOUT,DMCB,RCONKCON,P+33,4,=C'TOG'                              
         MVC   P+42(01),=C'$'                                                   
         EDIT  (B4,WORK),(8,P+44),MINUS=YES,ZERO=NOBLANK                        
PIT20    EQU   *                                                                
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINTIT2 --- PRINT RESULT OF PREVIOUS CONTRACT                         
*                                                                               
PRINTIT2 NTR1                                                                   
*                                                                               
         MVC   P+10(16),=C'CONTRACT RESULT:'                                    
         MVC   P+42(01),=C'$'                                                   
         EDIT  FULL,(8,P+44),MINUS=YES,ZERO=NOBLANK                             
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              TOTALS CONTROL                                                   
         SPACE 3                                                                
TOTALS   NTR1                                                                   
         GOTO1 ROLLER,DMCB,1,(R9),(R2)                                          
         L     R2,DMCB                                                          
         LR    R5,R2                                                            
*                                  TEST ANY ACTIVITY                            
         LH    R4,LINARA                                                        
TOTS0004 EQU   *                                                                
         OC    0(52,R5),0(R5)                                                   
         BNZ   TOTS0008                                                         
         LA    R5,52(R5)                                                        
         BCT   R4,TOTS0004                                                      
         B     XIT                 NO ACTIVITY--GO HOME                         
TOTS0008 EQU   *                                                                
         L     R3,AOFFLIST                                                      
         LH    R4,LINTYP                                                        
         LR    R5,R2                                                            
         AH    R5,SIZOFF                                                        
         LR    R6,R5                                                            
         AH    R6,SIZTYP                                                        
         LR    R8,R6                                                            
         AH    R8,SIZTYP                                                        
*              TEST IF ANY TOTALS EQUAL                                         
         CLC   0(52,R5),0(R8)                                                   
         BE    CLEARTOT                                                         
         CLC   0(52,R8),0(R6)                                                   
         BE    CLEARTOT                                                         
*                                                                               
TOTS0012 LR    R5,R2                                                            
         SR    R9,R9                                                            
         LH    R8,NUMOFF                                                        
         MH    R8,=H'20'                                                        
         LA    R8,LOFFLIST(R8)                                                  
         MVI   FIRSTPNT,C'Y'                                                    
         MVC   3(7,R8),=C'REGULAR'                                              
         BAS   RE,TOTS0032                                                      
*                                                                               
         LR    R2,R5                                                            
         AH    R2,SIZTYP                                                        
         L     R5,VTYPETBL                                                      
         LH    R6,0(R5)                                                         
         L     R9,2(R5)                                                         
         LA    R5,6(R5)                                                         
TOTS0016 EQU   *                                                                
         CLC   0(1,R5),QOPTION2                                                 
         BE    TOTS0020                                                         
         BXLE  R5,R6,TOTS0016                                                   
         DC    H'0'                                                             
TOTS0020 EQU   *                                                                
         LH    R8,NUMOFF                                                        
         MH    R8,=H'20'                                                        
         LA    R8,LOFFLIST(R8)                                                  
         MVC   3(7,R8),1(R5)                                                    
         LR    R5,R2                                                            
         SR    R9,R9                                                            
         MVI   FIRSTPNT,C'Y'                                                    
         BAS   RE,TOTS0032                                                      
         LR    R2,R5                                                            
         AH    R2,SIZTYP                                                        
         SPACE 1                                                                
         LH    R8,NUMOFF                                                        
         MH    R8,=H'20'                                                        
         LA    R8,LOFFLIST(R8)                                                  
         MVC   3(7,R8),=C'OVERALL'                                              
         LA    R9,1                                                             
         MVI   FIRSTPNT,C'Y'                                                    
         BAS   RE,TOTS0032                                                      
         B     XIT                                                              
         SPACE 2                                                                
TOTS0032 NTR1                                                                   
         LH    R4,LINTYP                                                        
         L     R3,AOFFLIST                                                      
TOTS0036 OC    0(52,R2),0(R2)                                                   
         BZ    TOTS0048                                                         
         MVI   ANYPRINT,C'Y'                                                    
         CH    R4,=H'1'            NO OVERALL LINE FOR 1 OFF REQS               
         BNE   *+14                                                             
         CLC   QOFFICE,SPACES                                                   
         BNE   TOTS0044                                                         
         MVC   P+9(20),0(R3)                                                    
***      CLC   P(5),SPACES         THIS CODE MEANS NOTHING....                  
***      BE    TOTS0040                                                         
***      CLI   MODE,STALAST                                                     
***      BNE   TOTS0040                                                         
         SPACE 1                                                                
TOTS0040 EQU   *                                                                
         CLI   FIRSTPNT,C'Y'                                                    
         BNE   TOTS0041                                                         
         MVC   PHOLD(132),P                                                     
         MVC   P(40),LINESMKT                                                   
         GOTO1 REPORT                                                           
         MVC   P(132),PHOLD                                                     
         MVI   FIRSTPNT,C'N'                                                    
TOTS0041 EQU   *                                                                
         CLI   QOPTION1,C'S'       STATION TOTALS ONLY?                         
         BNE   TOTS0042            NO  - PRINT ALL LINES                        
         CLC   =C'OVERALL',3(R3)   YES - 'OVERALL' LINE?                        
         BNE   TOTS0044            NO  - SKIP IT                                
         MVC   P+9(20),SPACES      YES - NO PRINT **OVERALL TOTALS**            
TOTS0042 EQU   *                                                                
         BAS   RE,PRINTEM                                                       
TOTS0044 XC    0(52,R2),0(R2)                                                   
         SPACE 2                                                                
TOTS0048 LA    R2,52(R2)                                                        
         LA    R3,20(R3)                                                        
         BCT   R4,TOTS0036                                                      
         CLI   ANYPRINT,C'Y'                                                    
         BNE   XIT                                                              
         MVI   ANYPRINT,C'N'                                                    
         CLC   QOFFICE,SPACES      SKIP 1 LINE ON 1 OFFICE PRINT                
         BNE   TOTS0052                                                         
         LTR   R9,R9               TEST LAST TOTAL FOR STATION                  
         BZ    *+8                                                              
         MVI   SPACING,2                                                        
         SPACE 1                                                                
TOTS0052 CLI   LINE,55                                                          
         BL    TOTS0056                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
TOTS0056 MVC   BYTE,MAXLINES                                                    
         MVI   MAXLINES,100                                                     
TOTS0060 GOTO1 REPORT              SKIP LINE(S)                                 
         MVC   MAXLINES,BYTE                                                    
         B     XIT                                                              
         SPACE 3                                                                
CLEARTOT LA    R5,52(R6)           POINT TO BEGINNING OF TOTALS AREA            
         LH    R6,LINTYP                                                        
CLRTOT2  XC    0(52,R5),0(R5)      CLEAR EACH LINE                              
         LA    R5,52(R5)                                                        
         BCT   R6,CLRTOT2                                                       
         B     TOTS0012                                                         
         EJECT                                                                  
*              ROUTINE TO PRINT AND FILL MIDLINES                               
         SPACE 3                                                                
PRINTEM  NTR1                                                                   
         L     R8,AMONINFO         A(MONTH INFO TABLE)                          
         L     R3,ANEWMON          A(NEW MONTH TABLE)                           
         CLI   NMONTHS,6           DOUBLE SPACE IF MORE THAN                    
         BNH   *+8                 SIX MONTHS REQUESTED                         
         MVI   PSECOND,0                                                        
PREM0002 EQU   *                                                                
         CLC   0(4,R3),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    PREM0004            FOUND - BEGIN TO PRINT MONTHS                
         LA    R3,NEXTBUCK(R3)     BUMP TO NEXT BUCKET                          
         B     PREM0002            GO BACK FOR NEXT                             
PREM0004 EQU   *                                                                
         LA    R5,P+32                                                          
         LA    R6,6                                                             
         BAS   RE,PRINT2                                                        
         LA    R8,48(R8)                                                        
         LA    R5,PSECOND+32                                                    
         BAS   RE,PRINT2                                                        
         CLI   QACCTOPT,C'P'                                                    
         BE    PREM0006                                                         
         EDIT  (4,(R2)),(12,P+98),COMMAS=YES,FLOAT=-                            
         B     PREM0008                                                         
PREM0006 EQU   *                                                                
         EDIT  (4,(R2)),(12,P+98),2,FLOAT=-                                     
*                                                                               
PREM0008 EQU   *                                                                
         DS    0H                                                               
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
PRINT2   NTR1                                                                   
         SPACE 2                                                                
PRINT4   CLI   0(R3),0                                                          
         BE    PRINT6                                                           
         OC    0(4,R2),0(R2)                                                    
         BZ    PRINT6                                                           
         CLI   QACCTOPT,C'P'                                                    
         BE    PRINT4A                                                          
         EDIT  (4,(R2)),(11,(R5)),COMMAS=YES,FLOAT=-                            
         B     PRINT4B                                                          
PRINT4A  EDIT  (4,(R2)),(11,(R5)),2,FLOAT=-                                     
*                                                                               
PRINT4B  DS    0H                                                               
         SPACE 2                                                                
PRINT6   LA    R2,4(R2)                                                         
         LA    R3,NEXTBUCK(R3)     NEXT NEW MONTH TABLE ENTRY                   
         LA    R4,11(R4)                                                        
         LA    R5,11(R5)                                                        
         LA    R8,16(R8)           NEXT MONTH INFO ENTRY                        
         BCT   R6,PRINT4                                                        
         XIT1  REGS=(R2,R3)                                                     
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
BLDID    XC    ID,ID                                                            
         MVC   ID(2),RCREPFL       USER ID                                      
         MVC   ID+2(3),=C'R34'     SYSTEM/PROGRAM                               
         MVC   ID+5(1),QGROUP      SET SUB-PROGRAM TO MEDIA                     
         PACK  DUB(2),RCDATE+3(3)  CONVERT DAY TO PWO                           
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'S'           SET CLASS TO S                               
         OI    ID+13,X'01'         ALLOW DUPLICATE INDICES                      
         MVC   CARD,QRECORD                                                     
         MVI   CLOSWRK+1,X'00'    NOP BRANCH AROUND CLOSE                       
*                                                                               
         GOTO1 WORKER,DMCB,=CL6'OPEN',WORKBUFF,ID,CRD1                          
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 WORKER,DMCB,=C'ADD',WORKBUFF,ID,CRD1                             
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     MNEXT                                                            
         EJECT                                                                  
OFFLST   NTR1                                                                   
         L     R2,AOFFLIST                                                      
         SR    R3,R3                                                            
         SR    R4,R4                                                            
         L     R5,ASPACND4         A(1ST ENTRY IN SPACEND TABLE)                
OFFLST00 EQU   *                                                                
         CLI   0(R5),4                                                          
         BE    OFFLST04            OFFICE REC IN SPACEND FOUND                  
         IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     OFFLST00                                                         
         SPACE 1                                                                
OFFLST04 EQU   *                                                                
         CLC   RCREPFL,2(R5)       SAME REP?                                    
         BNE   OFFLST16            NO  - KEEP LOOKING                           
OFFLST08 CLC   QOFFICE,SPACES                                                   
         BE    OFFLST12                                                         
         CLC   QOFFICE,6(R5)       COMPARE FOR OFFICE CODE                      
         BNE   OFFLST16            NOT FOUND                                    
         SPACE 1                                                                
OFFLST12 MVC   0(20,R2),8(R5)      FOUND - TAKE OFFICE NAME                     
*                                                                               
*  TEST                                                                         
**       LR    RE,R3                                                            
**       LA    RE,1(RE)                                                         
**       EDIT (RE),(2,P+10)                                                     
**       MVC  P+1(6),=C'OFFICE'                                                 
**       MVC   P+20(20),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LA    R2,20(R2)                                                        
         AH    R3,=H'1'                                                         
         SPACE 1                                                                
OFFLST16 IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         CLI   0(R5),4                                                          
         BE    OFFLST04                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STH   R3,NUMOFF                                                        
         MVC   0(20,R2),=CL20'**         TOTALS **'                             
         AH    R3,=H'1'                                                         
         STH   R3,LINTYP                                                        
         B     XIT                                                              
         EJECT                                                                  
INITIAL  NTR1                                                                   
         L     RF,=A(OFFLIST)                                                   
         A     RF,RELO                                                          
         ST    RF,AOFFLIST                                                      
*        S     RF,=F'8'                                                         
*        MVC   P+1(07),=C'OFFLIST'                                              
*        MVC   P+8(08),0(RF)                                                    
*        GOTO1 REPORT                                                           
*        DC    H'0'                                                             
*                                                                               
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*   TEST END DUMP                                                               
****     DC    H'0'                                                             
*   TEST END DUMP END                                                           
*                                                                               
*                                     SPECIAL YR 2000 FORMAT                    
         MVC   NUMAREA,=H'4'                                                    
         LH    R3,NUMOFF           NUMBER OF OFFICES                            
         MH    R3,=H'52'                                                        
         STH   R3,SIZOFF           SIZE OF AREA LINE 1- X                       
         AH    R3,=H'52'                                                        
         STH   R3,SIZTYP           SIZE OF AREA LINE 1 - TOTAL                  
         LH    R3,LINTYP                                                        
         MH    R3,=H'3'                                                         
         STH   R3,LINARA           LINES FOR ALL TYPES                          
         MH    R3,NUMAREA                                                       
         STH   R3,TOTLIN           TOTAL NUMBER OF LINES                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*   PROFILE:  READ PROFILE RECORD.  RETRIEVE PROFILE FOR 'REQUEST'              
*        PROFILE.  SAVE OFF PROFILE.                                            
*                                                                               
PROFILE  NTR1                                                                   
         LA    R3,RREPREC          SET A(IO AREA FOR PROCEDURE)                 
         ST    R3,AIOAREA                                                       
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,1               SET REP RECORD TYPE                          
         MVC   KEY+25(2),RCREPFL   INSERT REP CODE INTO KEY                     
         GOTO1 HIGH1               READ FIRST RECORD                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                KEY NOT ON FILE?                             
         GOTO1 GREC1               RETRIEVE RECORD                              
         XC    SVPGMPRF,SVPGMPRF   CLEAR PROGRAM PROFILE                        
         MVI   SVPGP#,RREPQREQ     SET EQUATE FOR 'REQUEST'                     
         L     RE,AIOAREA                                                       
         ZIC   RF,RREPLEN                                                       
         AR    RF,RE                                                            
         MVI   0(RF),0             SET 0 AT END OF RECORD                       
         LA    RE,34(RE)           A(1ST ELEMENT)                               
PROF0020 EQU   *                                                                
         CLI   0(RE),0             END OF RECORD?                               
         BE    PROF0800            YES - NO MATCH                               
         CLI   0(RE),4             PROGRAM PROFILE ELEMENT?                     
         BE    PROF0040            YES                                          
         ZIC   RF,1(RE)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RE,RF                                                            
         B     PROF0020            GO BACK FOR NEXT                             
PROF0040 EQU   *                                                                
*                                                                               
*   FIND REQUEST PROGRAM ELEMENT WITHIN PROFILE ELEMENTS                        
*                                                                               
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         # OF PROGRAM UNITS                           
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
*                                                                               
         DROP  RE                                                               
*                                                                               
PROF0060 EQU   *                                                                
         CLI   0(RE),RREPQREQ      REQUEST PROFILE ELEMENT?                     
         BE    PROF0080            YES                                          
         LA    RE,RREPPGML(RE)     NO  - NEXT UNIT                              
         BCT   R0,PROF0060         GO BACK FOR NEXT                             
         B     PROF0800            NOT FOUND - EXIT                             
PROF0080 EQU   *                                                                
         MVC   SVPGMPRF,0(RE)      SAVE PROFILE UNIT                            
PROF0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*        COMMUNICATION WITH DATA MANAGER (DIRECTORY)                            
         SPACE                                                                  
READ1    MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY1                                                         
         SPACE 2                                                                
SEQ1     MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY1                                                         
         SPACE 2                                                                
HIGH1    MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY1                                                         
         SPACE 2                                                                
ADD1     MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY1                                                         
         SPACE 2                                                                
WRITE1   MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY1                                                         
         SPACE 2                                                                
DIRCTRY1 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         B     RGENIOD1                                                         
         EJECT                                                                  
*        COMMUNICATION WITH DATA MANAGER (FILE)                                 
         SPACE 3                                                                
GREC1    MVC   COMMAND(8),GETREC                                                
         B     FILE1                                                            
         SPACE 2                                                                
PREC1    MVC   COMMAND(8),PUTREC                                                
         B     FILE1                                                            
         SPACE 2                                                                
AREC1    MVC   COMMAND(8),ADDREC                                                
         B     FILE1                                                            
         SPACE 2                                                                
FILE1    NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),(R5),DMWORK                                                 
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
RGENIOD1 OC    DMCB+8(1),DMCB+8                                                 
*                                                                               
         XIT1                      RETURN                                       
*                                                                               
         EJECT                                                                  
*                                                                               
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF                                                         
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         CLI   QACCTOPT,C'A'                                                    
         BNE   HHOOK05                                                          
         MVC   HEAD4+033(28),=C'FOR THE ACCOUNTING PERIOD OF'                   
         L     R4,AMONARCH                                                      
         USING MONARCHD,R4                                                      
         GOTO1 DATCON,DMCB,(X'12',KEYMONTH),(5,HEAD4+062)                       
         DROP  R4                                                               
HHOOK05  EQU   *                                                                
*                                                                               
         L     R8,AMONINFO         A(MONTH INFO TABLE)                          
         L     R3,ANEWMON          A(NEW MONTH TABLE)                           
HHOOK10  EQU   *                                                                
         CLC   0(4,R3),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    HHOOK20             FOUND - BEGIN TO PRINT MONTHS                
         LA    R3,NEXTBUCK(R3)     BUMP TO NEXT BUCKET                          
         B     HHOOK10             GO BACK FOR NEXT                             
HHOOK20  EQU   *                                                                
         MVC   HEAD8+34(66),SPACES                                              
         MVC   HEAD9+34(66),SPACES                                              
         LA    R4,HEAD8+34                                                      
         LA    R6,6                                                             
         LA    R9,2                                                             
HHOOK50  CLI   0(R3),0                                                          
         BE    HHOOK60                                                          
         GOTO1 DATCON,DMCB,(0,(R3)),(6,(R4))                                    
         MVC   7(2,R4),=C'28'                                                   
         CLI   2(R8),28                                                         
         BE    HHOOK60                                                          
         MVC   7(2,R4),=C'35'                                                   
HHOOK60  EQU   *                                                                
         LA    R3,NEXTBUCK(R3)     NEXT NEW MONTH TABLE ENTRY                   
         CLI   QACCTOPT,C'A'       ACCOUNTING REPORT?                           
         BE    HHOOK70             YES - SKIP 'HONOR REQUEST' TEST              
         CLC   0(4,R3),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    HHOOKEXT            LAST MONTH PASSED                            
HHOOK70  EQU   *                                                                
         LA    R4,11(R4)                                                        
         LA    R8,16(R8)           NEXT MONTH INFO ENTRY                        
         BCT   R6,HHOOK50                                                       
         LA    R4,HEAD9+34                                                      
         LA    R6,6                                                             
         BCT   R9,HHOOK50                                                       
*                                                                               
HHOOKEXT EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        WORK AREA                                                              
         SPACE 2                                                                
         DS    0F                                                               
SAVEREGS DS    11F                 FOR REG 2->C                                 
ID       DS    CL8                                                              
         DC    8X'00'                                                           
SAVEGRP  DS    CL2                                                              
ANYPRINT DC    C'N'                                                             
FIRSTPNT DC    C'N'                                                             
CRD1     DS    0F                                                               
         DC    H'84'                                                            
         DC    H'0'                                                             
CARD     DS    CL80                                                             
         SPACE 2                                                                
RELO     DS    A                                                                
ACCUMS   DS    F                   A(ACCUMS)                                    
NUMAREA  DS    H                   NUMBER OF AREAS                              
NUMOFF   DS    H                   NUMBER OF OFFICES                            
SIZOFF   DS    H                   SIZE OF OFFICE AREA FOR A TYPE               
LINTYP   DS    H                   LINES PER TYPE                               
SIZTYP   DS    H                   SIZE OF TYPE AREA                            
LINARA   DS    H                   LINES IN AN AREA                             
TOTLIN   DS    H                   TOTAL NUMBER OF LINES                        
NMONTHS  DS    CL1                 NUMBER OF MONTHS FOR THIS REQUEST            
ELCODE   DS    CL1                 GETEL/EL CODE                                
QSTBIN   DS    CL3                 BINARY START YY/MM                           
FRSTIME  DS    CL1                 ONE-TIME SWITCH                              
TESTDISP DS    CL1                 CONTRACT DISPLAY FLAG                        
LINESMKT DS    CL40                STATION CALLS + MARKET NAME                  
PHOLD    DS    CL132                                                            
AOFFLIST DS    A                   A(OFFLIST TABLE)                             
LOFFLIST EQU   20                                                               
         SPACE 1                                                                
SVPGMPRF DS    0CL10               PROGRAM PROFILE FROM REP                     
SVPGP#   DS    CL1                 LFM EQUATE (RREPQRIS)                        
         DS    CL1                 UNDEFINED                                    
SVPGPBIT DS    XL8                 PROGRAM PROFILE BITS                         
*                                                                               
COMMAND  DS    CL8                                                              
AIOAREA  DS    F                                                                
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
         ENTRY WORKBUFF                                                         
WORKBUFF DS    0D                                                               
         DC    4096X'00'           WORK FILE BUFFER                             
         EJECT                                                                  
CARDD    DSECT                                                                  
         DS    CL5                                                              
CRDGRP   DS    CL2                                                              
CRDSTA   DS    CL5                                                              
CRDOFF   DS    CL2                                                              
         DS    CL1                                                              
CRDAMTS  DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         EJECT                                                                  
       ++INCLUDE REMONARCHD                                                     
         EJECT                                                                  
** A SCHEMATIC OF THE ACCUMULATORS FOR ROLLER FOLLOWS-                          
*                                                                               
* COLUMNS 1-48 REPRESENT 12 MONTHLY BUCKETS (4 BYTE EACH)                       
* COLUMNS 49-52 CONTAINS THE TOTAL                                              
*                                                                               
* ROW 1  IS THE WORK AREA                                                       
*     2-21     REGULAR STATION BY OFFICE (MAX. 50 OFFICES)                      
*     22       REGULAR STATION TOTAL                                            
*     23-42    TYPED STATION BY OFFICE                                          
*     43       TYPED STATION TOTAL                                              
*     44-63    TOTAL STATION BY OFFICE                                          
*     64       TOTAL STATION TOTAL                                              
*                                                                               
*     65-84    REGULAR SUB-GROUP BY OFFICE                                      
*     85       REGULAR SUB-GROUP TOTAL                                          
*     86-105   TYPED SUB-GROUP BY OFFICE                                        
*     106      TYPED SUB-GROUP TOTAL                                            
*     107-126  TOTAL SUB-GROUP BY OFFICE                                        
*     127      TOTAL SUB-GROUP TOTAL                                            
*                                                                               
*     128-147  REGULAR GROUP BY OFFICE                                          
*     148      REGULAR GROUP TOTAL                                              
*     149-168  TYPED GROUP BY OFFICE                                            
*     169      TYPED GROUP TOTAL                                                
*     170-189  TOTAL GROUP BY OFFICE                                            
*     190      TOTAL GROUP TOTAL                                                
*                                                                               
*     191-210  REGULAR OVERALL BY OFFICE                                        
*     211      REGULAR OVERALL TOTAL                                            
*     212-231  TYPED OVERALL BY OFFICE                                          
*     232      TYPED OVERALL TOTAL                                              
*     233-252  TOTAL OVERALL BY OFFICE                                          
*     253      TOTAL OVERALL TOTAL                                              
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE REGENREQ2                                                      
       ++INCLUDE REGENREQ3                                                      
         EJECT                                                                  
         CSECT                                                                  
OFFFLAG  DC    C'OFFLIST '                                                      
OFFLIST  DS    250CL20                                                          
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'194REREP3402A05/01/02'                                      
         END                                                                    
