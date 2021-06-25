*          DATA SET RECNT36    AT LEVEL 217 AS OF 02/25/15                      
*PHASE T80236A                                                                  
*INCLUDE OUTDAY                                                                 
*INCLUDE REGENPCB                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE UNTIME                                                                 
         TITLE 'T80236 - REPPAK MULTIPLE BUY DISPLAY- COMBO ORDER'              
***********************************************************************         
*                                                                     *         
* RECNT36 -- MULTIPLE BUY DISPLAY (COMBO ORDER)                       *         
*                                                                     *         
* HISTORY:                                                            *         
*                                                                     *         
*                                                                     *         
* 07JUN00 BU  REWRITE OF ALL TRADE TOTAL DISPLAY ROUTINES             *         
* 01JUN00 BU  TRADE AND EXPANDED TOTALS DISPLAY                       *         
* 21DEC98 RHV HANDLE NON-DELETED CANCELLED LINES                      *         
* 11JUL96 RHV DISPLAY MONTHLY # OF SPOT TOTALS                        *         
* 15JAN96 SKU GRAND TOTAL BUG FIX                                     *         
* 09OCT95 SKU 2K CONTRACT SUPPORT                                     *         
* 15DEC92 BU  FIX GRAND TOTAL ROUTINE:  STATION IN PROGRESS           *         
*              RUNOUT WRONG.  RTN TOTO0060.                           *         
* 14JUL92 BU  ORIGINAL ENTRY                                          *         
*                                                                     *         
***********************************************************************         
T80236   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80236,R9,RR=R5                                                
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,R7                                                       
MAINLINE EQU   *                                                                
         LA    RF,TWABYTOT         CALCULATE BUCKET ADDRESSES                   
         AH    RF,=Y(TWABYTRD-TWABYTOT)                                         
         ST    RF,TWAATRD          SET A(TRADE ARRAY)                           
         AH    RF,=Y(TWABYTTL-TWABYTRD)                                         
         ST    RF,TWAATTL          SET A(TOTAL TOTAL ARRAY)                     
         AH    RF,=Y(TWABYCSH-TWABYTTL)                                         
         ST    RF,TWAACASH         SET A(TOTAL CASH  ARRAY)                     
         AH    RF,=Y(TWABYTTR-TWABYCSH)                                         
         ST    RF,TWAATRDE         SET A(TOTAL TRADE ARRAY)                     
*                                                                               
         MVC   WORK2(4),VGTBROAD   ADDRESS BLOCK FOR REGENBUC                   
         MVC   WORK2+4(4),GETDAY                                                
         MVC   WORK2+8(4),ADDAY                                                 
         MVC   WORK2+12(4),DATCON                                               
         MVI   GTOTFLG,C'N'             INITIALIZE GRAND TOT FLAG               
*                                                                               
* FOUT AND CLEAR NON-ZERO LINES                                                 
MLIN0010 LA    R2,CINLIN1H                                                      
         LA    R3,CINLAST-9                                                     
         SR    RE,RE                                                            
         CLC   TWACNUM,TWANWCON    SAME CONTRACT IN PROCESS?                    
         BE    MLIN0020            YES - NOTHING TO RESET                       
         MVC   TWANWCON,TWACNUM    NO  - SET TO NEW CONTRACT                    
         CLC   CONBNUM(4),=C'NEXT' RESET BUY NUM IF 'NEXT'                      
         BNE   MLIN0020                                                         
         MVC   CONBNUM(4),MYSPACES                                              
*                                                                               
MLIN0020 OC    8(79,R2),8(R2)                                                   
         BZ    MLIN0030                                                         
         XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         NI    1(R2),X'F7'         TURN OFF ANY HI-INTENSITY                    
MLIN0030 IC    RE,0(R2)            FIELD LEN                                    
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3                                                            
         BL    MLIN0020                                                         
*                                                                               
MLIN0040 MVC   KEY+28(4),TWAKADDR  K DISK ADDR                                  
         GOTO1 VGETREC,DMCB,AIO3                                                
         GOTO1 VMOVEREC,DMCB,AIO3,RCONREC                                       
         L     RE,=V(OUTDAY)                                                    
         AR    RE,R5                                                            
         ST    RE,DMCB2                                                         
         L     RE,=V(UNTIME)                                                    
         AR    RE,R5                                                            
         ST    RE,DMCB2+4                                                       
         MVC   DMCB2+8(4),DATCON                                                
*                                                                               
*  RETRIEVE AND SAVE COMBO CONTROL ELEMENT FROM CONTRACT RECORD.                
*     ABORT IF NOT FOUND, BECAUSE WE SHOULDN'T EVEN BE IN THIS                  
*     OVERLAY.                                                                  
*                                                                               
MLIN0060 EQU   *                                                                
         MVC   CONKSTA,RCONKSTA                                                 
         MVC   STA0CON#,RCONKCON                                                
         LA    R1,RCONELEM                                                      
MLIN0070 EQU   *                                                                
         ZIC   R0,1(R1)            L(ELEMENT)                                   
         AR    R1,R0               A(NEXT ELEMENT)                              
         CLI   0(R1),0             END OF RECORD?                               
         BNE   MLIN0080            NO                                           
         DC    H'0'                ***ABORT***                                  
MLIN0080 EQU   *                                                                
         CLI   0(R1),X'17'         COMBO CONTROL ELEMENT?                       
         BNE   MLIN0070            NO  - CHECK NEXT ELEMENT                     
         ZIC   R2,1(R1)            YES - SAVE IT BY LENGTH                      
         BCTR  R2,0                DECREMENT FOR EX                             
         LA    R3,COMBOCTL         A(STORAGE AREA)                              
         EX    R2,MLIN0090                                                      
         B     MLIN0100                                                         
*                                                                               
MLIN0090 MVC   0(0,R3),0(R1)       SAVE IT                                      
*                                                                               
MLIN0100 EQU   *                                                                
         MVC   CONKSTA,RCONKSTA    SAVE STATION OF ORDER                        
         MVI   CONPROG,1           SET CONTRACT IN PROGRESS TO 1                
*                                                                               
*  CALCULATE # OF CONTRACTS IN COMBO BY DIVIDING THE LENGTH OF THE              
*   COMBO CONTROL ELEMENT (MINUS ELEMENT CODE AND LENGTH BYTES)                 
*   BY LENGTH OF ONE COMBO CONTRACT ENTRY                                       
*                                                                               
         ST    R1,FULL             SAVE A(COMBO CONTROL) TEMPOR                 
         ZIC   R3,1(R1)            L(COMBO CONTROL ELEMENT)                     
         SR    R2,R2               EVEN REGISTER OF PAIR                        
         BCTR  R3,0                SUBTRACT L(ELEMENT CODE BYTE)                
         BCTR  R3,0                SUBTRACT L(ELEMENT LEN  BYTE)                
         LA    R1,9                L(CONTRACT CONTROL)                          
         DR    R2,R1               DIVIDE LEN BY SIZE TO CALCULATE              
         STC   R3,CONCTR              NUMBER OF ENTRIES                         
*                                                                               
*   SET UP STATIONS IN ORDER FOR TOTAL DISPLAY.  FOR STATION OF                 
*     ORDER, ONLY CALL LETTERS ARE SAVED.  'NEXT PAGE' HANDLES                  
*     STATION OF ORDER.  IF TOTALS FOR ANY OTHER CONTRACT IN                    
*     COMBO ORDER ARE NEEDED, THE CONTRACT NUMBER IS REQUIRED.                  
*                                                                               
         XC    COMSTA1(20),COMSTA1 INITIALIZE STATIONS                          
         L     R3,FULL             RELOAD A(COMBO CONTROL ELT)                  
         LA    R3,2(R3)            POINT TO 1ST STATION                         
         LA    R2,COMSTA1                                                       
         ZIC   R0,CONCTR                                                        
MLIN0102 EQU   *                                                                
         CLC   0(5,R3),CONKSTA     STATION OF ORDER?                            
         BE    MLIN0104            YES - DON'T SAVE AGAIN                       
         MVC   0(9,R2),0(R3)       NO  - SAVE IT                                
         LA    R2,9(R2)            BUMP SAVE AREA ADDRESS                       
MLIN0104 EQU   *                                                                
         LA    R3,9(R3)            BUMP A(COMBO CONTROL ELT)                    
         BCT   R0,MLIN0102         GO BACK FOR NEXT                             
*                                                                               
         BAS   RE,STACALLS         INSERT STATION LETTERS INTO SCREEN           
*                                                                               
         SR    R6,R6               LINE CTR                                     
         LA    R2,CINLIN1H                                                      
*                                                                               
         CLC   CONBNUM(4),=C'NEXT' CONTINUATION?                                
         BE    MLIN0110            YES                                          
         TM    RCONMODR+1,X'20'    MON DATA ADDED?                              
         BO    MLIN0105            YES                                          
         CLC   CONBNUM(3),=C'TOT'  STATION DETAIL TOTALS ONLY?                  
         BNE   MLIN0110                                                         
MLIN0105 EQU   *                                                                
*                                                                               
MLIN0110 MVI   RBUYKTYP,X'0B'      BUILD BUY KEY                                
         MVC   RBUYKREP,REPALPHA   REP                                          
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
*                                                                               
         LA    R2,CONBNUMH                                                      
         MVI   BYFLT,0             ZERO FLIGHT NUMBER                           
         CLI   CONBNUM,C'F'        TEST FOR FLIGHT NUMBER FILTER PREFIX         
         BNE   MLIN0120            NOT THERE                                    
         SPACE                                                                  
         LA    R3,INVINP                                                        
         CLI   5(R2),1             TEST INPUT LENGTH FOR UP TO                  
         BE    ERROR               2 DIGIT FLIGHT NUMBER                        
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         SPACE                                                                  
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         MVC   WORK(3),=3X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),CONBNUM+1                                                
         CLC   WORK(3),=3X'F0'                                                  
         BL    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CONBNUM+1(0)                                                 
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR               ZERO IS INVALID FLIGHT NUMBER                
         STC   R0,BYFLT                                                         
         B     MLIN0120                                                         
         SPACE                                                                  
MLIN0120 GOTO1 VPACK                                                            
*                                                                               
         STC   R0,STARTLIN         STARTING LINE NUMBER                         
         CLC   CONBNUM(4),=C'NEXT' CONTINUATION?                                
         BE    MLIN0126            YES                                          
         TM    RCONMODR+1,X'20'    MON DATA ADDED?                              
         BO    MLIN0125            YES                                          
         CLC   CONBNUM(3),=C'TOT'  STATION DETAIL TOTALS ONLY?                  
         BNE   MLIN0126                                                         
MLIN0125 MVI   STARTLIN,X'FF'                                                   
*                                                                               
MLIN0126 LA    R2,CINLIN1H                                                      
         CLC   CONBNUM(4),=C'NEXT'                                              
         BNE   MLIN0130                                                         
         CLI   TWADSMTL,0          'NEXT' FOR TOTALS?                           
         BH    TOTALIT             YES - GO DO TOTALS                           
         B     MLIN0140            NO  - PROCESS BUYLINES                       
MLIN0130 XC    TWALSTKY,TWALSTKY   RESET LAST BUYREC READ                       
         XCEF  TWABYTOT,1000       CLEAR TOTAL BUCKETS AREA                     
         L     RE,TWAATRD          SET A(TRADE BUCKETS)                         
         XCEF  (RE),1000                                                        
         L     RE,TWAATTL          SET A(TOTAL TOTAL BUCKETS)                   
         XCEF  (RE),1000                                                        
         L     RE,TWAACASH         SET A(TOTAL CASH  BUCKETS)                   
         XCEF  (RE),250                                                         
         L     RE,TWAATRDE         SET A(TOTAL TRADE BUCKETS)                   
         XCEF  (RE),250                                                         
         LA    RF,4               INITIALIZE 4-240 BYTE AREAS                   
         LA    R1,TWABYTOT                                                      
         MVC   0(2,R1),=X'0002'                                                 
         LA    R1,240(R1)                                                       
         BCT   RF,*-10                                                          
         LA    RF,4               INITIALIZE 4-240 BYTE AREAS                   
         L     R1,TWAATRD                                                       
         MVC   0(2,R1),=X'0002'                                                 
         LA    R1,240(R1)                                                       
         BCT   RF,*-10                                                          
         LA    RF,4               INITIALIZE 4-240 BYTE AREAS                   
         L     R1,TWAATTL                                                       
         MVC   0(2,R1),=X'0002'                                                 
         LA    R1,240(R1)                                                       
         BCT   RF,*-10                                                          
         LA    RF,1               INITIALIZE 1-240 BYTE AREAS                   
         L     R1,TWAACASH                                                      
         MVC   0(2,R1),=X'0002'                                                 
         LA    R1,240(R1)                                                       
         BCT   RF,*-10                                                          
         LA    RF,1               INITIALIZE 1-240 BYTE AREAS                   
         L     R1,TWAATRD                                                       
         MVC   0(2,R1),=X'0002'                                                 
         LA    R1,240(R1)                                                       
         BCT   RF,*-10                                                          
         B     MLIN0150                                                         
*                                                                               
* NEXT PAGE REQUESTED                                                           
MLIN0140 CLC   TWALSTKY(22),RBUYKEY     SAME K?                                 
         BNE   MLIN0130                                                         
         MVC   RBUYKEY+22(5),TWALSTKY+22                                        
         SR    R4,R4                                                            
         IC    R4,RBUYKEY+26                                                    
         LA    R4,1(R4)                                                         
         CH    R4,=H'255'                                                       
         BH    *+8                                                              
         STC   R4,RBUYKEY+26       FOR READ HIGH TO NEXT KEY                    
*                                                                               
MLIN0150 EQU   *                                                                
         MVI   TWACLEVL,0          SET TOTAL DISPLAY REFERENCES TO              
         MVI   TWABUCKT,0             ZERO DURING BUYLINE READING               
         MVI   TWADSMTL,0          TURN OFF 'DISPLAY TOTAL' FLAG                
*                                                                               
         MVC   KEY,RBUYKEY                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         B     MLIN0170                                                         
         SPACE 1                                                                
MLIN0160 EQU   *                                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
MLIN0170 CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   TOTALIT                                                          
         TM    KEY+27,X'C0'        VOID?                                        
         BO    MLIN0160                                                         
         CLC   CONBNUM(4),=C'NEXT'                                              
         BE    MLIN0180                                                         
*                                                                               
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BE    MLIN0180                                                         
*                                                                               
         CLC   KEY+26(1),STARTLIN                                               
         BNE   MLIN0180                                                         
         MVI   STARTLIN,0          DON'T USE AGAIN                              
*                                                                               
* GET BUY RECORD                                                                
MLIN0180 EQU   *                                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   MLIN0194            NO                                           
* DISPLAY BUY                                                                   
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BNE   MLIN0194                                                         
         XC    DMCB2+12(4),DMCB2+12   NO ALT WEEK EXPLOSION                     
         L     RF,AIO3             A(FORMATTED DATA AREA)                       
         ST    RF,PCBYADDR         SAVE A(FORMATTED DATA)                       
         GOTO1 =V(REGENPCB),DMCB,RBUYREC,(10,(RF)),DMCB2,              X        
               RCONREC,PROFILES,RR=YES                                          
         CLI   DMCB+4,0            DID BUYLINE PRODUCE ANY PRINTLINES?          
         BE    MLIN0160            NO  - SKIP IT                                
* DISPLAY LINE                                                                  
*                                                                               
MLIN0190 EQU   *                                                                
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DMCB+4           NUMBER OF BUY DISPLAY LINES                  
         LA    RE,1(RE)            ADD 1 FOR 'RATES' LINE                       
         AR    R6,RE               WILL BUY'S PRINT LINES FIT SCREEN?           
         CH    R6,=H'10'           R6 = TOTAL.  MAX OF 10 PERMITTED.            
         BH    EXIT0070            NO ROOM FOR THIS BUYLINE                     
         BCTR  RE,0                REMOVE 'RATES' LINE ADJUSTMENT               
*                                                                               
MLIN0194 EQU   *                                                                
         ST    RE,SAVERE           SAVE LOOP CONTROL REG                        
         MVI   COMFLAG,C'N'        SET COMMENT FLAG TO NO                       
         XC    TWATTLAR,TWATTLAR   INITIALIZE GRAND TOTAL STORAGE               
         XC    TWATRADE,TWATRADE   INITIALIZE TRADE TOTAL STORAGE               
         XC    TWATOTAL,TWATOTAL   INITIALIZE TOTAL TOTAL STORAGE               
*                                                                               
         TM    RBUYCOMB,X'80'      N/A BUYLINE?                                 
         BO    MLIN0195            YES - DON'T ADD INTO TOTALS                  
         GOTO1 (RFGENBUC,VREPFACS),DMCB,RBUYREC,AIO2,WORK2                      
         L     R1,AIO2                                                          
         ST    R1,ADDAREA                                                       
         LA    R1,TWABYTOT                                                      
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   MLIN194A            NO                                           
         L     R1,TWAATRD          SET A(TRADE ARRAY)                           
MLIN194A EQU   *                                                                
         ST    R1,TOTAREA                                                       
         BAS   RE,ADDBUCS                                                       
         L     R1,TWAATTL          SET A(TOTAL TOTAL ARRAY)                     
         ST    R1,TOTAREA                                                       
         BAS   RE,ADDBUCS                                                       
         L     R1,TWAACASH         SET A(TOTAL CASH ARRAY)                      
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   MLIN194B            NO                                           
         L     R1,TWAATRDE         SET A(TOTAL TRADE ARRAY)                     
MLIN194B EQU   *                                                                
         ST    R1,TOTAREA                                                       
         BAS   RE,ADDBUCS          ACCUM CASH OR TRADE TOTAL DOLLARS            
*                                                                               
MLIN0195 CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BNE   MLIN0211                                                         
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   MLIN0211            NO                                           
*                                                                               
         L     R8,PCBYADDR         SET A(FORMATTED DATA)                        
         MVC   8(2,R2),=C'A:'      MOVE 'DETAIL LINE' INDICATOR                 
         L     RE,SAVERE                                                        
MLIN0200 EQU   *                                                                
         ST    RE,SAVERE                                                        
         CLC   =C'COM',PRAT-PRTLN(R8)      COMMENT LINE?                        
         BNE   MLIN0210            NO                                           
         CLI   COMFLAG,C'Y'        PRIOR COMMENT LINE?                          
         BE    MLIN0210            YES                                          
*                                                                               
*  INSERT RATES AT THIS POINT, BEFORE COMMENTS                                  
*                                                                               
         MVI   COMFLAG,C'Y'        NO  - SET TO YES                             
*                                                                               
         GOTO1 SETRATES,DMCB,(R2)                                               
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
*                                     FOR THIS LINE                             
         FOUT  (R2)                TURN ON TRANSMIT BIT                         
         SR    R4,R4                                                            
         IC    R4,0(R2)            BUMP TO NEXT SCREEN LINE                     
         LA    R2,0(R4,R2)                                                      
MLIN0210 EQU   *                                                                
         MVC   11(73,R2),4(R8)     MOVE DETAILS FROM LINE                       
         FOUT  (R2)                TURN ON TRANSMIT BIT                         
         SR    R4,R4                                                            
         IC    R4,0(R2)            BUMP TO NEXT SCREEN LINE                     
         LA    R2,0(R4,R2)                                                      
         LA    R8,L'PRTLN(R8)      BUMP TO NEXT LINE OF DATA                    
         L     RE,SAVERE           RESTORE LOOP CONTROL REGISTER                
         BCT   RE,MLIN0200         LOOP TO DISPLAY BUY LINES                    
*                                                                               
* ALL DATA DISPLAYED.  SAVE 'LAST KEY PROCESSED' FOR RESTART                    
*                                                                               
MLIN0211 MVC   TWALSTKY,KEY        SAVE KEY                                     
*                                                                               
*  SET RATES FOR SAME BUYLINE IN OTHER CONTRACTS OF COMBO                       
*                                                                               
         CLI   COMFLAG,C'Y'        ARE COMMENTS DISPLAYED?                      
         BE    MLIN0160            YES - RATES ALREADY PRINTED                  
*                                                                               
         GOTO1 SETRATES,DMCB,(R2)                                               
*                                                                               
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BNE   MLIN0160                                                         
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   MLIN0160            NO                                           
*                                                                               
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
*                                     FOR THIS LINE                             
         FOUT  (R2)                TURN ON TRANSMIT BIT                         
         SR    R4,R4                                                            
         IC    R4,0(R2)            BUMP TO NEXT SCREEN LINE                     
         LA    R2,0(R4,R2)                                                      
         B     MLIN0160            GO BACK FOR NEXT BUY                         
         EJECT                                                                  
*                                                                               
*   ADDBUCS - ADDS BUCKET ELEMENTS ADRESSED ADDAREA TO TOTAL BUCKETS            
*             ADDRESSED BY TOTAREA                                              
*                                                                               
ADDBUCS  NTR1                                                                   
*                                                                               
**       CLI   GTOTFLG,C'Y'        BYPASS THE NEXT CHECK IF GRAND TOT           
**       BE    ADDB005                                                          
         TM    RBUYCNTL,X'80'      DELETED?                                     
         BO    ADDBX               DON'T ADD INTO TOTALS                        
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    ADDBX               DON'T ADD INTO TOTALS                        
*                                                                               
ADDB005  L     R3,ADDAREA                                                       
         L     R4,TOTAREA                                                       
         CLC   0(2,R3),=H'2'      NO DATA                                       
         BE    ADDBX                                                            
         SR    R0,R0                                                            
* ADD BUY BUCKETS TO TOTAL CONTRACT BUCKETS                                     
         LA    R3,2(R3)            A(FIRST ELEM)                                
         LA    R4,2(R4)            A(FIRST ELEM)                                
         ST    R4,FULL             PRESERVE R4 IN LOOP                          
ADDB010  L     R4,FULL             RESTORE R4                                   
*                                                                               
ADDB020  CLI   0(R4),0             LAST?                                        
         BE    ADDB030                                                          
         CLC   2(2,R3),2(R4)       COMPARE BUCKET DATES                         
         BL    ADDB030                                                          
         BE    ADDB050                                                          
* GET NEXT TOTAL BUCKET                                                         
         ZIC   R0,1(R4)            LEN                                          
         AR    R4,R0                                                            
         B     ADDB020                                                          
*                                                                               
* ADD NEW TOTAL BUCKET                                                          
ADDB030  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(X'FF',TOTAREA),(R3),(R4)                            
*                                                                               
* GET NEXT BUY BUCKET                                                           
ADDB040  ZIC   R0,1(R3)            LENGTH                                       
         AR    R3,R0                                                            
         CLI   0(R3),0             LAST?                                        
         BNE   ADDB010                                                          
         B     ADDBX                                                            
         SPACE 1                                                                
* ADD TO TOTAL BUCKET                                                           
ADDB050  MVC   DUB(8),6(R3)                                                     
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R4)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R4),DUB                                                      
         B     ADDB040                                                          
ADDBX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*   SETRATES - BUILDS LINE WITH RATES FOR STATIONS IN COMBO BUY.                
*     1ST RATE COMES FROM BUYLINE SETUP.  NEXT 1 TO 3 RATES COME                
*     FROM BUY RECORDS RETRIEVED BY USING CALL LETTERS IN COMBO                 
*     CONTROL ELEMENT.  AFTER THE STATION OF ORDER, THE REMAINING               
*     STATIONS WILL ALWAYS BE IN THE SAME SEQUENCE.                             
*                                                                               
SETRATES NTR1                                                                   
         MVC   SAVEOKEY,KEY        SAVE KEY FOR RESET                           
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BNE   SETR0005                                                         
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   SETR0005            NO                                           
         MVC   8(2,R2),=C'B:'      INSERT 'RATE LINE' INDICATOR                 
         L     RF,PCBYADDR         A(PCBY FORMATTED DATA)                       
         MVC   21(10,R2),PRAT-PRTLN(RF)                                         
         MVC   31(02,R2),PTOT+4-PRTLN(RF)                                       
*                                  MOVE POSSIBLE 'TR' INDICATOR                 
         MVC   PTOT+4-PRTLN(2,RF),=C'  '                                        
*                                  CLEAR POSSIBLE 'TR' INDICATOR                
*                                  INSERT 1ST STATION'S RATE                    
SETR0005 LA    R3,COMBOCTL+2       A(COMBO CONTROL ELT'S 1ST STATION)           
         LA    R5,35(R2)           A(NEXT RATE FIELD ON SCREEN)                 
         ZIC   R0,CONCTR           # CONTRACTS IN COMBO                         
         LA    R4,TWABYTOT         SECOND SET OF TOTAL BUCKETS                  
         LA    R4,240(R4)                                                       
SETR0010 EQU   *                                                                
         STC   R0,SAVCTR           SAVE LOOP CONTROL                            
         CLC   CONKSTA,0(R3)       STATION OF ORDER VS COMBO CONTROL            
         BE    SETR0050            SAME - ALREADY SHOWN - SKIP IT               
SETR0015 EQU   *                                                                
         L     RE,=X'99999999'     CALC 9'S COMP OF CONTRACT#                   
         MVC   FULL,5(R3)          LOAD CONTRACT#                               
         L     RF,FULL                                                          
         SR    RE,RF               GET 9'S COMP                                 
         ST    RE,FULL             STORE IT FOR REVERSAL                        
*                                                                               
         MVC   WORK(4),FULL                                                     
         PACK  WORK(1),FULL+3(1)   REVERSE THE COMPLEMENT                       
         PACK  WORK+1(1),FULL+2(1)                                              
         PACK  WORK+2(1),FULL+1(1)                                              
         PACK  WORK+3(1),FULL(1)                                                
*                                                                               
         MVC   RBUYKCON,WORK       LOAD REV 9'S COMP CONTRACT#                  
         MVC   KEY,RBUYKEY                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     BUYLINE FOUND?                               
         BE    SETR0020            YES                                          
         DC    H'0'                NO  - MUST BE IN SYNCH                       
SETR0020 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         TM    RBUYCOMB,X'80'      IS 'NA' FLAG SET?                            
         BNO   SETR0030            NO                                           
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BNE   SETR0040                                                         
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   SETR0040            NO                                           
         MVC   8(2,R5),=C'NA'      YES - SET 'NA' ON SCREEN                     
         B     SETR0040                                                         
SETR0030 EQU   *                                                                
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BNE   SETR0032                                                         
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   SETR0032            NO                                           
         EDIT  (4,RBUYCOS),(10,(R5)),2,FLOAT=-,COMMAS=YES                       
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   SETR0032            NO                                           
         MVC   10(2,R5),=C'TR'     INSERT 'TR' INDICATOR                        
*                                                                               
SETR0032 GOTO1 (RFGENBUC,VREPFACS),DMCB,RBUYREC,AIO2,WORK2                      
         L     R1,AIO2                                                          
         ST    R1,ADDAREA                                                       
         ST    R4,SVTOTAR          SAVE CURRENT A(TOTAREA)                      
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   SETR0034            NO                                           
         AH    R4,=Y(TWABYTRD-TWABYTOT)                                         
*                                  YES - BUMP TO TRADE ARRAY BUCKETS            
SETR0034 EQU   *                                                                
         ST    R4,TOTAREA                                                       
         L     R4,SVTOTAR          RESET A(TOTAREA)                             
         BAS   RE,ADDBUCS                                                       
         AH    R4,=Y(TWABYTTL-TWABYTOT)                                         
*                                  BUMP TO TOTAL TOTAL ARRAY BUCKETS            
         ST    R4,TOTAREA                                                       
         L     R4,SVTOTAR          RESET A(TOTAREA)                             
         BAS   RE,ADDBUCS          ACCUMULATE TOTALS                            
         L     R4,TWAATRDE         SET TOTAL TRADE DOLLARS AS TOTAREA           
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    SETR0036            YES                                          
         L     R4,TWAACASH         NO  - SET TOTAL CASH $ AS TOTAREA            
*                                  YES - BUMP TO TRADE ARRAY BUCKETS            
SETR0036 EQU   *                                                                
         ST    R4,TOTAREA                                                       
         L     R4,SVTOTAR          RESET A(TOTAREA)                             
         BAS   RE,ADDBUCS          ADD CASH OR TRADE TOTALS                     
SETR0040 EQU   *                                                                
         LA    R5,14(R5)           NEXT RATE SLOT ON SCREEN                     
         LA    R4,240(R4)          NEXT SET OF STATION TOTAL BUCKETS            
SETR0050 EQU   *                                                                
         LA    R3,9(R3)            NEXT STATION IN CONTROL ELEMENT              
         ZIC   R0,SAVCTR           RESET LOOP CONTROL                           
         BCT   R0,SETR0010         GO BACK FOR NEXT                             
         MVC   KEY,SAVEOKEY        RETRIEVE ORIGINAL ORDERED BUY                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH               RESTORE KEY FOR NEXT READ                    
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  STACALLS - INSERTS STATION CALL LETTERS FROM COMBO CONTROL ELEMENT           
*     INTO THE SCREEN FIELDS OVER THE 'RATES' FIELDS.                           
*                                                                               
STACALLS NTR1                                                                   
*                                                                               
*  MOVE STATION TO HEADING                                                      
*                                                                               
         LA    R2,CONKSTA          STATION OF ORDER                             
         GOTO1 INSERTIT,DMCB,(R2),CINCMB1H,0                                    
         LA    R2,COMBOCTL+2       A(COMBO CONTROL ELT'S 1ST STATION)           
         ZIC   R0,CONCTR           # CONTRACT IN COMBO                          
         LA    R4,CINCMB1H         A(1ST SCREEN HEADER STATION FIELD)           
STAC0010 EQU   *                                                                
         STC   R0,SAVCTR           SAVE LOOP CONTROL                            
         CLC   CONKSTA,0(R2)       STATION OF ORDER VS COMBO CONTROL            
         BE    STAC0030            SAME - ALREADY INSERTED - SKIP IT            
         ZIC   R0,0(R4)            BUMP TO NEXT SCREEN FIELD                    
         AR    R4,R0                                                            
*                                                                               
*  MOVE STATION TO HEADING                                                      
*                                                                               
         GOTO1 INSERTIT,DMCB,(R2),(R4),0                                        
STAC0030 EQU   *                                                                
         LA    R2,9(R2)            NEXT STATION IN CONTROL ELEMENT              
         ZIC   R0,SAVCTR           RESET LOOP CONTROL                           
         BCT   R0,STAC0010         GO BACK FOR NEXT                             
         XIT1                                                                   
         SPACE 5                                                                
*                                                                               
*  INSERTIT - INSERTS STATION CALL LETTERS INTO SCREEN, TURNS ON                
*    TRANSMIT BIT                                                               
*         P1  =  A(STATION CALL LETTERS IN COMBO CONTROL ELEMENT)               
*         P2  =  SCREEN FIELD HEADER                                            
*         P3  =  BLANK, CASH, TRADE INDICATOR                                   
*                                                                               
INSERTIT NTR1                                                                   
         L     R2,0(R1)            LOAD A(STA CALL LETTERS TABLE)               
         MVC   BYTE,11(R1)         SAVE INDICATOR BYTE                          
         L     R1,4(R1)            LOAD A(SCREEN FIELD HEADER)                  
         CLI   BYTE,0              NON-TOTAL INSERT?                            
         BE    INSE0002            YES - NO DISPLACEMENT                        
         ZIC   RF,TWABUCKT         DETERMINE STATION DISPLACEMENT               
         MH    RF,=H'9'            CALCULATE DISPLACEMENT                       
         AR    R2,RF               ADD DISPLACEMENT TO A(TABLE)                 
INSE0002 EQU   *                                                                
         MVC   8(4,R1),0(R2)       INSERT CALL LETTERS                          
         CLI   4(R2),C' '          NO MEDIA?                                    
         BE    INSE0010            NO MEDIA                                     
         CLI   4(R2),X'00'         NO MEDIA?                                    
         BE    INSE0010            NO MEDIA                                     
         CLI   4(R2),C'T'          MEDIA = TELEVISION?                          
         BE    INSE0010            YES                                          
         MVI   12(R1),C'-'         INSERT HYPHEN                                
         MVC   13(1,R1),4(R2)      INSERT MEDIA                                 
INSE0010 EQU   *                                                                
         CLI   BYTE,0              NON-TOTAL INSERT?                            
         BE    INSE0060            YES -                                        
         CLI   TWACLEVL,0          YES - CASH?                                  
         BNE   INSE0020            NO                                           
         MVC   15(8,R1),=C'**CASH**'                                            
*                                  YES - SET INDICATOR                          
         MVC   65(8,R1),=C'**CASH**'                                            
         OI    1(R1),X'08'         TURN ON HIGH INTENSITY                       
         B     INSE0060                                                         
INSE0020 EQU   *                                                                
         CLI   TWACLEVL,1          YES - TRADE?                                 
         BNE   INSE0030            NO                                           
         MVC   15(9,R1),=C'**TRADE**'                                           
         MVC   65(9,R1),=C'**TRADE**'                                           
         OI    1(R1),X'08'         TURN ON HIGH INTENSITY                       
*                                  YES - SET INDICATOR                          
         B     INSE0060                                                         
INSE0030 EQU   *                                                                
         CLI   TWACLEVL,2          YES - TRADE?                                 
         BNE   INSE0040            NO                                           
         MVC   15(14,R1),=C'**CASH+TRADE**'                                     
*                                  YES - SET INDICATOR                          
         MVC   65(14,R1),=C'**CASH+TRADE**'                                     
         OI    1(R1),X'08'         TURN ON HIGH INTENSITY                       
         B     INSE0060                                                         
INSE0040 EQU   *                                                                
         DC    H'0'                UNRECOGNIZED VALUE                           
INSE0060 EQU   *                                                                
         FOUT  (R1)                TURN ON TRANSMIT BIT                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  INSERT#2 - INSERTS STATION CALL LETTERS INTO A DUMMY SCREEN LINE             
*         P1  =  A(STATION CALL LETTERS IN COMBO CONTROL ELEMENT)               
*         P2  =  DUMMY SCREEN LINE                                              
*                                                                               
INSERT#2 NTR1                                                                   
         L     R2,0(R1)            LOAD A(STA CALL LETTERS)                     
         L     R1,4(R1)            LOAD A(SCREEN FIELD HEADER)                  
         MVC   0(4,R1),0(R2)       INSERT CALL LETTERS                          
         CLI   4(R2),C' '          NO MEDIA?                                    
         BE    INS20010            NO MEDIA                                     
         CLI   4(R2),X'00'         NO MEDIA?                                    
         BE    INS20010            NO MEDIA                                     
         CLI   4(R2),C'T'          MEDIA = TELEVISION?                          
         BE    INS20010            YES                                          
         MVI   04(R1),C'-'         INSERT HYPHEN                                
         MVC   05(1,R1),4(R2)      INSERT MEDIA                                 
INS20010 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*              DISPLAY STATION DETAIL TOTALS                                    
TOTALIT  DS    0H                                                               
         CLI   TWADSMTL,1          RESTART AT STATION DETAIL?                   
         BH    TOTL0160            NO  - RESTART AT HIGHER LEVEL                
*                                                                               
         TM    RCONMODR+1,X'20'    MON DATA ADDED                               
         BZ    TOTL0020                                                         
         CLC   CONBNUM(4),=C'NEXT' CONTINUATION?                                
         BE    TOTL0020             YES - SKIP, TOTS ALREADY IN BUFFER          
         BAS   RE,MONCASE                                                       
TOTL0020 EQU   *                                                                
         CLC   CONBNUM(4),=C'TOT$' STATION GRAND TOTALS ONLY?                   
         BNE   TOTL0040                                                         
         MVI   TWADSMTL,2          SET TO START AT CASH                         
         B     TOTL0160                                                         
TOTL0040 EQU   *                                                                
         LA    RF,TWABYTOT         SET A(STATION TOTAL: CASH)                   
         CLI   TWACLEVL,0          PROCESSING CASH?                             
         BE    TOTL0060            YES                                          
         L     RF,TWAATRD          SET A(STATION TOTAL: TRADE)                  
         CLI   TWACLEVL,1          PROCESSING TRADE?                            
         BE    TOTL0060            YES                                          
         L     RF,TWAATTL          SET A(STATION TOTAL: CASH+TRADE?             
         CLI   TWACLEVL,2          PROCESSING CASH+TRADE?                       
         BE    TOTL0060            YES                                          
         DC    H'0'                UNRECOGNIZED VALUE                           
TOTL0060 EQU   *                                                                
         LA    RE,240              CALCULATE BUCKET DISPLACEMENT                
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),TWABUCKT  ZERO-REL BUCKET DISPLACEMENT                 
         MH    RE,HALF             CALCULATE BUCKET DISPL                       
         AR    RF,RE               ADD DISPLACEMENT TO ARRAY ADDRESS            
         ST    RF,DMWORK           SAVE A(ARRAY IN PROGRESS)                    
         CLI   TWACLEVL,0          CASH  LEVEL IN PROCESS?                      
         BNE   TOTL0063            NO                                           
         ST    RF,DMWORK+8         YES - SAVE ITS ADDR                          
TOTL0063 EQU   *                                                                
         CLI   TWACLEVL,1          TRADE LEVEL IN PROCESS?                      
         BNE   TOTL0065            NO                                           
         ST    RF,DMWORK+12        YES - SAVE ITS ADDR                          
TOTL0065 EQU   *                                                                
                                                                                
         CLC   =X'0002',0(RF)      ANY DATA IN ARRAY?                           
         BE    TOTL0110            NO  - SKIP PRINTING THIS ARRAY               
         CLI   TWACLEVL,2          YES - TOTAL LEVEL IN PROCESS?                
         BNE   TOTL0068            NO                                           
         L     RE,DMWORK+8         YES - SET A(CASH TOTALS)                     
         CLC   0(240,RF),0(RE)     TOTAL $ = CASH $?                            
         BE    TOTL0110            YES - NO TRADE: DON'T PRINT TOTALS           
         L     RE,DMWORK+12        YES - SET A(TRADE TOTALS)                    
         CLC   0(240,RF),0(RE)     TOTAL $ = TRADE $?                           
         BE    TOTL0110            YES - NO CASH: DON'T PRINT TOTALS            
TOTL0068 EQU   *                                                                
*                                                                               
         GOTO1 =V(REGENTL2),DMCB,(RF),AIO3,RR=YES                               
*                                                                               
         MVI   TWADSMTL,1          RESTART AT STATION TOTAL FLAG                
         ZIC   RE,DMCB+4           NUMBER OF TOTAL LINES                        
         LA    RE,1(RE)            ADD 1 FOR STATION CALL LETTERS               
         ST    R6,DMWORK+4         SAVE LINE COUNT FOR POSSIBLE RESET           
         AR    R6,RE               TOTAL NUMBER OF LINES                        
         CH    R6,=H'10'           LINES ON SCREEN VS MAX OF 10                 
         BH    EXIT0070            NO ROOM - END WITH 'NEXT'                    
         ST    RE,FULL             SAVE LOOP CONTROL                            
         L     RE,FULL             RESET LOOP CONTROL                           
*                                                                               
         BCTR  RE,0                DEDUCT STATION CALL LETTER LINE              
         L     RF,DMWORK           RESTORE A(ARRAY IN PROGRESS)                 
         CLI   TWACLEVL,1          TRADE LEVEL IN PROGRESS?                     
         BNE   TOTL0070            NO                                           
         CLC   =X'0002',0(RF)      YES - ANY DATA IN ARRAY?                     
         BNE   TOTL0070            YES -                                        
         L     R6,DMWORK+4         NO  - RESET LINE COUNT                       
*                                                                               
         B     TOTL0120            SKIP- NO TRADE, CASH+TRADE TOTALS            
TOTL0070 EQU   *                                                                
         LTR   RE,RE               ANY LINES FOUND?                             
         BNZ   TOTL0080            YES                                          
         LA    RE,1                NO  - DISPLAY AT LEAST ONE BLANK             
*                                                                               
*              MOVE BUCKET TOTALS TO SCREEN                                     
*                                                                               
TOTL0080 EQU   *                                                                
         ST    RE,FULL             SAVE LOOP CONTROL                            
*                                                                               
*  INSERT STATION/TOTAL TITLE ON SCREEN LINE                                    
*                                                                               
         GOTO1 INSERTIT,DMCB,CONKSTA,(R2),1                                     
         L     RE,FULL             RESET LOOP CONTROL                           
         ZIC   R0,0(R2)            SKIP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         L     R8,AIO3                                                          
*                                                                               
TOTL0100 MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)            NEXT DISPLAY LINE                            
         AR    R2,R0                                                            
         BCT   RE,TOTL0100                                                      
*                                  ALL LINES DISPLAYED:                         
TOTL0110 EQU   *                                                                
*                                  DETERMINE NEXT LEVEL TO DISPLAY              
         CLI   TWACLEVL,2          ALL THREE LEVELS DISPLAYED?                  
         BE    TOTL0120            YES                                          
         ZIC   RF,TWACLEVL         NO  - BUMP LEVEL                             
         LA    RF,1(RF)                                                         
         STC   RF,TWACLEVL         RESTORE LEVEL                                
         B     TOTL0140            GO BACK FOR NEXT TOTALS                      
TOTL0120 EQU   *                                                                
         MVI   TWACLEVL,0          RESET LEVEL TO STATION DETAIL                
         ZIC   RF,TWABUCKT         BUMP BUCKET (= STATION)                      
         LA    RF,1(RF)                                                         
         STC   RF,TWABUCKT         RESTORE BUCKET                               
         B     TOTL0140            GO BACK FOR NEXT TOTALS                      
TOTL0140 EQU   *                                                                
         LA    RF,CONKSTA          CHECK FOR STATION EXISTENCE                  
         XC    HALF,HALF           CLEAR MULTIPLIER                             
         MVC   HALF+1(1),TWABUCKT  GET LEVEL                                    
         LA    RE,9                SET LENGTH OF ENTRIES                        
         MH    RE,HALF             CALCULATE DISPLACEMENT                       
         AR    RF,RE               DISPLACE TO ACTIVE STATION                   
         OC    0(5,RF),0(RF)       ANY STATION IN SLOT?                         
         BNZ   TOTL0040            YES - GO BACK AND SEE IF IT FITS             
*                                                                               
TOTL0160 EQU   *                                                                
*                                                                               
         CLI   TWADSMTL,4          PROCESSING GRAND TOTALS?                     
         BE    TOTL0340            YES                                          
TOTL0180 EQU   *                                                                
         L     RF,TWAACASH         SET A(TOTAL CASH)                            
         CLI   TWADSMTL,2          PROCESSING CASH?                             
*                                  MAY BE TRYING TO ADD AFTER A DETAIL          
*                                     OR RESTART AT TOTAL CASH                  
*                                     VALUES COULD BE 1 OR 2                    
         BNH   TOTL0200            YES                                          
         L     RF,TWAATRDE         SET A(TOTAL TRADE)                           
TOTL0200 EQU   *                                                                
         CLC   =X'0000',0(RF)      CHECK DATA ARRAY: INITIALIZED?               
         BNE   TOTL0210            YES -                                        
         MVC   0(2,RF),=X'0002'    NO  - SET INITIAL VALUE                      
TOTL0210 EQU   *                                                                
         CLC   =X'0002',0(RF)      ANY DATA IN ARRAY?                           
         BE    TOTL0310            NO  - SKIP PRINTING THIS ARRAY               
*                                                                               
         GOTO1 =V(REGENTL2),DMCB,(RF),AIO3,RR=YES                               
*                                                                               
         CLI   TWADSMTL,1          COMING FROM DETAIL?                          
         BNE   TOTL0220            NO                                           
         MVI   TWADSMTL,2          YES - RESTART AT TOTAL CASH                  
TOTL0220 EQU   *                                                                
         ZIC   RE,DMCB+4           NUMBER OF TOTAL LINES                        
         LA    RE,1(RE)            ADD 1 FOR TOTALS BANNER                      
         AR    R6,RE               TOTAL NUMBER OF LINES                        
         CH    R6,=H'10'           LINES ON SCREEN VS MAX OF 10                 
         ST    R6,DMWORK+4         SAVE LINE COUNT FOR POSSIBLE RESET           
         BH    EXIT0070            NO ROOM - END WITH 'NEXT'                    
         ST    RE,FULL             SAVE LOOP CONTROL                            
         L     RE,FULL             RESET LOOP CONTROL                           
         BCTR  RE,0                DEDUCT STATION CALL LETTER LINE              
         CLI   TWADSMTL,3          TRADE LEVEL IN PROGRESS?                     
         BNE   TOTL0230            NO                                           
         L     RF,TWAATRDE         YES - SET A(TRADE TOTAL ARRAY)               
         OC    0(240,RF),0(RF)     ANYTHING IN ARRAY?                           
         BZ    TOTL0228            NO                                           
         CLC   =X'0002',0(RF)      YES - ANY DATA IN ARRAY?                     
         BNE   TOTL0230            YES -                                        
TOTL0228 EQU   *                                                                
         MVI   TWADSMTL,0          CLEAR GRAND TRADE LEVEL IN PROCESS           
         L     R6,DMWORK+4         NO  - RESET LINE COUNT                       
         B     EXIT0010            SKIP- NO TRADE, NO GRAND TOTAL               
*                                     FOR CASH+TRADE                            
TOTL0230 EQU   *                                                                
         LTR   RE,RE               ANY LINES FOUND?                             
         BNZ   TOTL0240            YES                                          
         LA    RE,1                NO  - DISPLAY AT LEAST ONE BLANK             
*                                                                               
*              MOVE BUCKET TOTALS TO SCREEN                                     
*                                                                               
TOTL0240 EQU   *                                                                
         ST    RE,FULL             SAVE LOOP CONTROL                            
*                                                                               
*  INSERT STATION/TOTAL TITLE ON SCREEN LINE                                    
*                                                                               
         XC    8(79,R2),8(R2)      CLEAR OUT BANNER LINE                        
         CLI   TWADSMTL,2          CASH TOTALS?                                 
         BNE   TOTL0260            NO                                           
         MVC   08(15,R2),=C'**CASH  TOTAL**'                                    
         MVC   65(15,R2),=C'**CASH  TOTAL**'                                    
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         B     TOTL0280                                                         
TOTL0260 EQU   *                                                                
         MVC   08(15,R2),=C'**TRADE TOTAL**'                                    
         MVC   65(15,R2),=C'**TRADE TOTAL**'                                    
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
TOTL0280 EQU   *                                                                
         FOUT  (R2)                                                             
TOTL0290 EQU   *                                                                
         L     RE,FULL             RESET LOOP CONTROL                           
         ZIC   R0,0(R2)            SKIP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         L     R8,AIO3                                                          
*                                                                               
TOTL0300 EQU   *                                                                
         XC    8(79,R2),8(R2)      CLEAR OUT DISPLAY LINE                       
         MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)            NEXT DISPLAY LINE                            
         AR    R2,R0                                                            
         BCT   RE,TOTL0300                                                      
TOTL0310 MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         CLI   TWADSMTL,3          TRADE COMPLETED?                             
         BE    TOTL0320            YES - DO GRAND TOTALS                        
         MVI   TWADSMTL,3          ROOM - RESTART AT TOTAL TRADE                
         B     TOTL0180            GO BACK AND DO TRADE                         
*                                  ALL LINES DISPLAYED:                         
TOTL0320 EQU   *                                                                
         MVI   TWADSMTL,4          RESTART AT GRAND TOTAL                       
****     B     EXIT0010            NO  - TEMPORARY: FINISHED                    
TOTL0340 EQU   *                                                                
         MVI   GTOTFLG,C'Y'             WE ARE PROCESSING GRAND TOT             
         XC    TWATTLAR,TWATTLAR                                                
         LA    R1,TWATTLAR                                                      
         MVC   0(2,R1),=X'0002'                                                 
         ST    R1,TOTAREA                                                       
         LA    R4,2                                                             
         L     R3,TWAACASH         ADD CASH+TRADE TOTALS                        
TOTL0360 ST    R3,ADDAREA                                                       
         BAS   RE,ADDBUCS                                                       
         LA    R3,250(R3)                                                       
         BCT   R4,TOTL0360                                                      
                                                                                
         GOTO1 =V(REGENTL2),DMCB,TWATTLAR,AIO3,RR=YES                           
*                                                                               
         OC    TWATTLAR,TWATTLAR   ANY VALUE IN ARRAY?                          
         BNZ   TOTL0370            YES                                          
         XC    8(79,R2),8(R2)      CLEAR OUT TOTAL LINE                         
         MVC   68(15,R2),=C'TOTAL      $.00'                                    
                                                                                
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     EXIT0010                                                         
TOTL0370 EQU   *                                                                
         ZIC   RE,DMCB+4           NUMBER OF TOTAL LINES                        
         LA    RE,1(RE)            ADD 1 FOR 'GRAND TOTAL' LINE                 
         AR    R6,RE               TOTAL NUMBER OF LINES                        
         CH    R6,=H'10'           LINES ON SCREEN VS MAX OF 10                 
         BH    EXIT0070            NO ROOM - END WITH 'NEXT'                    
         ST    RE,FULL             SAVE LOOP CONTROL                            
*                                                                               
* ACCUMULATE MONTHLY BUCKETS FOR TOTALS                                         
*                                                                               
         L     RE,FULL             RESET LOOP CONTROL                           
         BCTR  RE,0                DEDUCT 'GRAND TOTAL' LINE                    
         LTR   RE,RE               ANY LINES FOUND?                             
         BNZ   TOTL0380            YES                                          
         LA    RE,1                NO  - DISPLAY AT LEAST ONE BLANK             
*              MOVE BUCKET TOTALS TO SCREEN                                     
TOTL0380 EQU   *                                                                
         XC    8(79,R2),8(R2)      CLEAR OUT TOTAL LINE                         
         MVC   08(15,R2),=C'**GRAND TOTAL**'                                    
         MVC   65(15,R2),=C'**GRAND TOTAL**'                                    
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)            SKIP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         L     R8,AIO3                                                          
*                                                                               
TOTL0400 EQU   *                                                                
         XC    8(79,R2),8(R2)      CLEAR OUT TOTAL LINE                         
         MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)            NEXT DISPLAY LINE                            
         AR    R2,R0                                                            
         BCT   RE,TOTL0400                                                      
         B     EXIT0010                                                         
         EJECT                                                                  
*              DISPLAY TOTALS                                                   
EXIT0010 LA    R2,CONBACTH         CURSOR                                       
         XC    DMCB(24),DMCB                                                    
         SR    R3,R3                                                            
         MVI   DMCB+3,53                                                        
         CLI   BYFLT,0             HAVE BUYS BEEN FILTERED BY FLIGHT            
         BE    EXIT0020            NO                                           
         MVI   DMCB+3,54                                                        
         EDIT  (B1,BYFLT),(3,TEMP),ALIGN=LEFT                                   
         LR    R3,R0                                                            
         B     EXIT0040                                                         
*                                                                               
EXIT0020 EQU   *                                                                
         CLC   CONBNUM(3),=C'TOT'   TOTALS ONLY?                                
         BNE   EXIT0040                                                         
         B     EXIT0040                                                         
EXIT0030 MVI   DMCB+3,55                                                        
EXIT0040 FOUT  CONBNUMH,MYSPACES,8                                              
EXIT0050 OI    CONBACTH+4,X'20'                                                 
         OI    CONBNUMH+4,X'20'                                                 
         LTR   R3,R3                                                            
         BZ    EXIT0060                                                         
         GOTO1 VDISMSG,DMCB,,0,0,((R3),TEMP)                                    
         B     EXIT                                                             
EXIT0060 GOTO1 VDISMSG,DMCB,,                                                   
         B     EXIT                                                             
*                                                                               
EXIT0070 DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         SR    R3,R3                                                            
         NI    CONBACTH+6,X'BF'    NO CURSOR                                    
         MVI   DMCB+3,56            HIT ENTER FOR NEXT SCRN                     
         CLI   BYFLT,0             DISPLAY THE FLIGHT FILTER IF ANY             
         BE    EXIT0080            NONE                                         
         MVI   DMCB+3,57            HIT ENTER FOR NEXT SCRN - FLIGHT NN         
         EDIT  (B1,BYFLT),(3,TEMP),ALIGN=LEFT                                   
         LR    R3,R0                                                            
EXIT0080 LA    R2,CONBNUMH                                                      
         MVC   8(4,R2),=C'NEXT'                                                 
         OI    1(R2),X'01'         TURN ON MODIFIED FOR AUTO PAGING             
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         B     EXIT0050                                                         
         EJECT                                                                  
*                                                                               
XIT      XIT1  EXIT ROUTINE                                                     
*                                                                               
* ROUTINE TO READ '03' BUCKETS OF COMBO K'S INTO BUFFER - DONE WHEN             
* MON DATA HAS BEEN ADDED - BECAUSE NO BUYS EXIST BUT MONTHLY BUCKETS           
* HAVE BEEN ADDED                                                               
*                                                                               
MONCASE  NTR1                                                                   
         LA    R6,RCONREC               CURRENT K                               
         LA    R4,TWABYTOT              FIRST BUFFER AREA                       
         BAS   RE,COPYBUC               COPY BUCKETS TO BUFFER                  
*                                                                               
         ZIC   R3,COMBOCTL+1            COMBO CONTROL ELEM LEN                  
         SH    R3,=H'2'                 SUBTRACT ELEM OVERHEAD                  
         SR    R2,R2                                                            
         D     R2,=A(RCONCBEN-RCONCBST) R3 = HOW MANY K IN COMBO                
         LA    R2,COMBOCTL+2            1ST K IN COMBO CONTROL                  
*                                                                               
MONC010  DS    0H                                                               
         CLC   RCONKCON,5(R2)                                                   
         BE    MONC030                                                          
         LA    R4,240(R4)                                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONREC,R6                                                       
         MVI   RCONKTYP,X'8C'                                                   
         MVC   RCONPREP,REPALPHA                                                
         ICM   R0,15,=X'99999999'                                               
         ICM   R1,15,5(R2)                                                      
         SR    R0,R1                                                            
         STCM  R0,15,RCONPCON                                                   
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         BAS   RE,COPYBUC                                                       
MONC030  LA    R2,RCONCBEN-RCONCBST(R2)                                         
         BCT   R3,MONC010                                                       
         DROP  R6                                                               
*                                                                               
MONCX    B     XIT                                                              
*                                                                               
* COPIES X'03' BUCKETS FROM REC AT R6 TO BUFFER ADDRESSED BY R4                 
*                                                                               
COPYBUC  NTR1                                                                   
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
COPYB010 BAS   RE,NEXTEL                                                        
         BNE   COPYBX            NO ELEMENT - ALL DONE                          
         ZICM  R3,0(R4),2        GET LENGTH OF BUFFER                           
         LA    R3,0(R3,R4)       POINT R3 TO END OF BUFFER                      
         XC    WORK,WORK                                                        
         ZIC   R1,1(R6)          LENGTH OF ELEMENT                              
         BCTR  R1,0              READY FOR EX                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)                                                    
         MVI   WORK+1,X'0E'      ADJUST LENGTH                                  
         GOTO1 VRECUP,DMCB,(X'FF',(R4)),WORK,(R3)  ADD EL TO END                
         B     COPYB010                                                         
*                                                                               
COPYBX   B     XIT                                                              
*                                                                               
*                                                                               
*    *****   LOCAL WORK SPACE   *****                                           
*                                                                               
STARTLIN DS    CL1                 1 BYTE BINARY STARTING LINE #                
COMBOCTL DS    CL60                STORAGE FOR COMBO CONTROL ELT                
CONCTR   DS    XL1                 CONTRACT COUNTER                             
CONPROG  DS    XL1                 CONTRACT IN PROGRESS                         
CONKSTA  DS    CL5                 STATION OF ORDER                             
STA0CON# DS    CL4                 CONTRACT #                                   
COMSTA1  DS    CL5                 FIRST COMBO STATION                          
STA1CON# DS    CL4                 CONTRACT #                                   
COMSTA2  DS    CL5                 SECOND COMBO STATION                         
STA2CON# DS    CL4                 CONTRACT #                                   
COMSTA3  DS    CL5                 THIRD COMBO STATION                          
STA3CON# DS    CL4                 CONTRACT #                                   
COMSTA4  DS    CL5                 4TH COMBO STATION (NOT USED)                 
STA4CON# DS    CL4                 CONTRACT #                                   
SAVEOKEY DS    CL27                SAVE OLD KEY                                 
SAVCTR   DS    X                   SAVE LOOP CONTROL VALUE                      
COMFLAG  DS    CL1                 COMMENT FLAG                                 
SAVERE   DS    A                   LOOP CONTROL REGISTER SAVE AREA              
ADDAREA  DS    A                   A(BUCKETS TO BE ADDED TO TOTAL)              
TOTAREA  DS    A                   A(BUCKET TOTAL AREA)                         
SVTOTAR  DS    A                   SAVE A(BUCKET TOTAL AREA)                    
PCBYADDR DS    A                   A(FORMATTED OUTPUT OF PCBY)                  
TOT$LIN1 DS    CL79                STATION GRAND TOTAL PRINT LINE 1             
TOT$LIN2 DS    CL79                STATION GRAND TOTAL PRINT LINE 2             
GTOTFLG  DS    CL1                 GRAND TOTAL FLAG                             
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
*   SCREEN FOR MULTIPLE BUY DISPLAY, COMBO ORDER                                
       ++INCLUDE RECNTF9D                                                       
*   DSECT FOR PRINT LINE FORMAT, REVISED FOR COMBO ORDER.                       
*                                                                               
       ++INCLUDE REGENPBYC                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'217RECNT36   02/25/15'                                      
         END                                                                    
