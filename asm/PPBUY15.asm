*          DATA SET PPBUY15    AT LEVEL 080 AS OF 06/22/10                      
*PHASE T41115A                                                                  
*INCLUDE OUTER                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY15 - TEARSHEET EDIT'                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NEW ACTION LIMIT ACCESS                                                       
* T411FFD+12  X'02' = WESTERN BUYER  (BUYBILL SET TO 'B')                       
*             X'04' = WESTERN BILLER (BUYBILL SET TO 'L')                       
*             X'08' = NO T/S CHG, DATA IS PROTECTED IN PPBUY07                  
*                                                                               
* NOTE: IF X'02' AND X'04' BUYBILL SET TO 'X'                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 6/17/10  DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM         
*                                                                               
* KWAN 05/12/06 CHANGE ELEM - CHG BIT FOR T/S APPROVE STATUS                    
*                                                                               
* KWAN 07/22/02 FIX MASTER/SUB CLIENT RECORD LOCKING BUG                        
*                                                                               
* KWAN 05/17/01 BEFORE CHANGING REC, NEED TO CHECK FOR LOCKS                    
*                                                                               
* KWAN 03/05/01 RELINK WITH MODIFIED PPBUYWRK1 (4000K BUY REC)                  
*                                                                               
* KWAN 03/05/01 PREVENT TR CODE "B" ON TEARSHEET                                
*                                                                               
* BPLA 01/98    NO LONGER TREAT SJR AS A WESTERN AGENCY                         
*                                                                               
* BPLA 04/97    CHANGES TO ALLOW BOTH BILLER AND BUYER                          
*               FUNCTIONS IF T411FFD+12 IS X'06'                                
*                                                                               
* BPLA 03/97    ADD MX TO WESTERN AGENCY LIST                                   
*                                                                               
* BPLA 03/96    ALLOW FOR 4 COMMENTS                                            
*                                                                               
* BPLA 03/31/95 IF THEY UNAPPROVE THE TEARSHEET                                 
*               SET OFF THE X'10' BIT IN PBDSTAT                                
*                                                                               
* BPLA 02/02/95 IF BILLER - DON'T CHANGE COMMENTS                               
*               AT CHG50 (THEY WERE PROTECTED)                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41115   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41115*                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         LA    R8,4095(RB)                                                      
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         LA    R8,BUYHDH                                                        
         USING PPF4D,R8            TEARSHEET LOWER SCREEN                       
*                                                                               
         LA    R2,BUYTR1H                                                       
         CLI   5(R2),1                                                          
         BNE   INIT10                                                           
         CLI   8(R2),C'B'          BUYING FROM TEARSHEET?                       
         BNE   INIT10                                                           
         LA    R3,30               BUY ON T/S DISPLAY ERR MSG                   
         B     ERROR                                                            
*                                                                               
INIT10   MVI   BUYBILL,0                                                        
         MVI   BYTE2,0                                                          
*                                                                               
         CLC   AGYALPHA,=C'WJ'     SPECIAL FOR WESTERN  - TEST                  
         BE    WEST                                                             
         CLC   AGYALPHA,=C'WR'     SPECIAL FOR WESTERN   WRLA                   
         BE    WEST                                                             
         CLC   AGYALPHA,=C'WI'     SPECIAL FOR WESTERN                          
         BNE   TEARS                                                            
WEST     MVC   TSHSTOK+7(18),=CL18'(A,I,M,R)'                                   
         FOUT  TSHSTOKH                                                         
*                                                                               
         MVI   BUYBILL,C'X'        ALLOW BILLER AND BUYER FUNCTIONS             
         TM    T411FFD+12,X'06'                                                 
         BO    TEARS                                                            
         MVI   BUYBILL,0                                                        
*                                                                               
         TM    T411FFD+12,X'02'    SEE IF BUYER                                 
         BNO   WEST5                                                            
         MVI   BUYBILL,C'B'                                                     
         B     TEARS                                                            
*                                                                               
WEST5    TM    T411FFD+12,X'04'    SEE IF BILLER                                
         BNO   TEARS                                                            
         MVI   BUYBILL,C'L'                                                     
*                                                                               
         RELOC RELO05              CAN USE REL05                                
*                                                                               
TEARS    DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(3,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT41103,0(R1)                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
         BRAS  R9,NXTTR                                                         
         BNZ   *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,NOTRERR                                                       
         B     ERROR                                                            
*                                                                               
         CLI   SVSCRN,X'F4'                                                     
         BE    TS1                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
*                                                                               
TS1      DS    0H                                                               
         TM    T411FFD+12,X'08'    SEE IF NO T/S CHANGES ALLOWED                
         BNO   TS3                                                              
         LA    R2,BUYTR1H                                                       
         LA    R3,FACCERR                                                       
         B     ERROR                                                            
*                                                                               
TS3      BRAS  R9,BLDREC                                                        
         BRAS  R9,FNDINS                                                        
         BRAS  R9,EDTINS                                                        
*                                                                               
* SEE IF INSERTION DATE CHANGED                                                 
*                                                                               
         CLC   PBUYKDAT(3),PBUYKDAT-PBUYREC+NEWREC                              
         BE    TS5                                                              
*                                                                               
TSERR    BRAS  R9,BUMPFLD         POINT TO DATE                                 
         LA    R3,NOCHAERR        DATA CAN NOT BE CHANGED                       
         B     ERROR                                                            
*                                                                               
* SEE IF INSERTION LINE NUMBER IS DIFFERENT                                     
*                                                                               
TS5      CLC   PBUYKLIN(1),PBUYKLIN-PBUYREC+NEWREC                              
         BE    TS8                                                              
         CLI   PBUYKLIN-PBUYREC+NEWREC,0                                        
         BNE   TSERR                                                            
         CLI   PBUYKLIN,1          IF NO LINE NUMBER IN NEWREC                  
         BNE   TSERR               LINE NUMBER IN REC MUST BE 1                 
*                                                                               
TS8      LA    R2,BUYTR1H                                                       
         LHI   RF,17               TO GET TO STATUS FIELD                       
         BRAS  R9,BUMPFLDS                                                      
         BRAS  R9,EDTTEAR          BUILDS NEW TSHEET ELEM IN NEWREC             
         BRAS  R9,BUMPFLD2                                                      
         BRAS  R9,EDTCOM           TEARSHEET COMMENTS IN NEWREC                 
         B     CHG                                                              
         EJECT                                                                  
*                                                                               
CHG      BRAS  R9,FNDINS                                                        
*                                                                               
         XC    FULL,FULL                                                        
         SR    R4,R4                                                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'95'        FIND T/S ELEM IN NEWREC                      
         BRAS  R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND ONE                                
         LR    R4,R5               SAVE ITS ADDRESS                             
         LA    R5,REC+33                                                        
         BRAS  R9,NEXTEL                                                        
         BNE   CHG40               NO OLD T/S ELEM, GO ADD NEW ONE              
*                                                                               
         ST    R5,FULL             SAVE ADDRESS OF OLD ELEM                     
         USING PTSHTELD,R5                                                      
         CLC   PTSHCDAT,PTSHCDAT-PTSHTEL(R4)                                    
         BNE   CHG4                                                             
         CLC   PTSHPID,PTSHPID-PTSHTEL(R4)                                      
         BNE   CHG4                                                             
         B     CHG4X               SAME DATE & PID, DON'T CLEAR                 
*                                                                               
CHG4     MVC   PTSHCDAT,PTSHCDAT-PTSHTEL(R4)   SAVE NEW CHANGE DATE             
         MVC   PTSHBID,PTSHBID-PTSHTEL(R4)     SAVE BUYER'S ID                  
         MVC   PTSHPID,PTSHPID-PTSHTEL(R4)     SAVE BUYER'S ID                  
         MVI   PTSHCIN1,0                      ZERO CHANGE INDICATORS           
         MVI   PTSHCIN2,0                                                       
*                                                                               
CHG4X    LA    R9,CHGTAB           TABLE OF CHANGE BITS                         
         LA    R6,PTSHSTAT                                                      
         LA    R7,PTSHSTAT-PTSHTEL(R4)                                          
CHG5     CLI   0(R9),X'FF'         END OF TABLE?                                
         BE    CHG15                                                            
         CLC   0(1,R6),0(R7)       CHANGED?                                     
         BE    CHG10                                                            
         MVC   0(1,R6),0(R7)                                                    
         OC    PTSHCIN1,0(R9)                                                   
CHG10    LA    R9,1(R9)                                                         
         LA    R6,1(R6)                                                         
         LA    R7,1(R7)                                                         
         B     CHG5                                                             
*                                                                               
CHG15    CLC   PTSHREPO,PTSHREPO-PTSHTEL(R4)                                    
         BE    CHG20                                                            
         MVC   PTSHREPO,PTSHREPO-PTSHTEL(R4)                                    
         OI    PTSHCIN2,X'01'                                                   
         B     CHG20                                                            
*                                                                               
CHG20    MVC   PTSHPAGE,PTSHPAGE-PTSHTEL(R4)                                    
         B     CHG50                                                            
*                                                                               
CHG40    GOTO1 VRECUP,DMCB,(1,REC),(R4),(R5)                                    
         ST    R5,FULL               SAVE ADDRESS OF NEW T/S ELEM               
*                                                                               
*  T/S COMMENTS - CK IF COMMENTS CHANGED                                        
*                                                                               
CHG50    CLI   BUYBILL,C'L'        BILLER?                                      
         BE    CHG90               SKIP, FIDS ARE PROTECTED                     
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'69'                                                     
         ZAP   DUB,=P'0'                                                        
CHG50C   BRAS  R9,NEXTEL                                                        
         BNE   CHG60                                                            
         LR    R4,R5               SAVE ITS ADDRESS                             
         AP    DUB,=P'1'                                                        
CHG50N   LA    R5,NEWREC+33                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   CHG70               COMMENTS MUST HAVE CHANGED                   
         CP    DUB,=P'1'           LOOKING FOR 1ST ELEM?                        
         BE    CHG50Q                                                           
         BRAS  R9,NEXTEL                                                        
         BNE   CHG70               COMMENTS MUST HAVE CHANGED                   
         CP    DUB,=P'2'           LOOKING FOR 2ND ELEM?                        
         BE    CHG50Q                                                           
         BRAS  R9,NEXTEL           MUST BE THIRD COMMENT                        
         BNE   CHG70               COMMENTS MUST HAVE CHANGED                   
         CP    DUB,=P'3'           LOOKING FOR 3RD ELEM?                        
         BE    CHG50Q                                                           
         BRAS  R9,NEXTEL           MUST BE 4TH COMMENT                          
         BNE   CHG70               COMMENTS MUST HAVE CHANGED                   
*                                                                               
CHG50Q   ZIC   R1,1(R4)                                                         
         AHI   R1,-1                                                            
         EX    R1,COMCOMP                                                       
         BNE   CHG70               COMMENT CHANGED, DELETE AND ADD              
*                                                                               
         LR    R5,R4                                                            
         CP    DUB,=P'4'           MAX IS NOW 4                                 
         BH    CHG60                                                            
         B     CHG50C              GO CHECK FOR ANOTHER                         
*                                                                               
* CK "NEW" COMMENTS IN NEWREC AND COMPARE THEM TO "OLD" COMMENTS IN REC         
*                                                                               
CHG60    LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'69'                                                     
         ZAP   DUB,=P'0'                                                        
CHG60C   BRAS  R9,NEXTEL                                                        
         BNE   CHG90               NO COMMENTS - NO CHANGES                     
         LR    R4,R5               SAVE ITS ADDRESS                             
         AP    DUB,=P'1'                                                        
CHG60N   LA    R5,REC+33                                                        
         BRAS  R9,NEXTEL                                                        
         BNE   CHG70                                                            
         CP    DUB,=P'1'           LOOKING FOR 1ST ELEM?                        
         BE    CHG60Q                                                           
         BRAS  R9,NEXTEL                                                        
         BNE   CHG70               COMMENTS MUST HAVE CHANGED                   
         CP    DUB,=P'2'           LOOKING FOR 2ND ELEM?                        
         BE    CHG60Q                                                           
         BRAS  R9,NEXTEL           3RD COMMENT                                  
         BNE   CHG70               COMMENTS MUST HAVE CHANGED                   
         CP    DUB,=P'3'           LOOKING FOR 3RD ELEM?                        
         BE    CHG60Q                                                           
         BRAS  R9,NEXTEL           4TH COMMENT                                  
         BNE   CHG70               COMMENTS MUST HAVE CHANGED                   
*                                                                               
CHG60Q   ZIC   R1,1(R4)                                                         
         AHI   R1,-1                                                            
         EX    R1,COMCOMP                                                       
         BNE   CHG70               COMMENT CHANGED                              
*                                  GO DELETE OLD ONES AND ADD NEW ONES          
         LR    R5,R4                                                            
         CP    DUB,=P'4'           MAX IS NOW 4                                 
         BH    CHG90                                                            
         B     CHG60C              GO CHECK FOR ANOTHER                         
*                                                                               
COMCOMP  CLC   0(0,R4),0(R5)                                                    
*                                                                               
CHG70    L     R5,FULL             COMMENTS CHANGED - DELETE AND ADD            
         OI    PTSHCIN2,X'02'      COMMENTS CHANGED                             
         DROP  R5                                                               
*                                                                               
CHG70A   LA    R5,REC+33                                                        
         BRAS  R9,NEXTEL                                                        
         BNE   CHG75                                                            
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
         B     CHG70A                                                           
*                                                                               
CHG75    MVC   HALF,REC+25         CLEAR END OF RECORD                          
         SR    R4,R4                                                            
         LH    R4,HALF                                                          
         LA    RE,REC                                                           
         AR    RE,R4                                                            
         LA    RF,REC                                                           
         AHI   RF,4000                                                          
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
CHG80    LA    R5,NEWREC+33                                                     
CHG85    MVI   ELCODE,X'69'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   CHG90                                                            
         LR    R4,R5               SAVE ADDRESS                                 
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'FF'        ADD TO END OF RECORD                         
         BRAS  R9,NEXTEL                                                        
         GOTO1 VRECUP,DMCB,(1,REC),(R4),(R5)                                    
         LR    R5,R4                                                            
         B     CHG85               GO DO ANOTHER COMMENT                        
*                                                                               
CHG90    NI    PBDSTAT-PBUYREC+REC,X'EF'                                        
*                                                                               
         TM    BYTE2,X'10'         STATUS IS APPROVED ('A')?                    
         BNO   *+8                 ('I' FOR WESTERN OR SJR)                     
         OI    PBDSTAT-PBUYREC+REC,X'10'                                        
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         B     ERROR                                                            
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    CHG_X                                                            
         TM    CHGIND5,PCHGTSAQ    T/S APPROVE STATUS CHANGED?                  
         BZ    CHG94                                                            
         GOTOR VT41103,DMCB,(RC),(RA),CHGELEMQ                                  
CHG94    BRAS  RE,PUTREC                                                        
*                                                                               
CHG_X    B     ALLDONE                                                          
         EJECT                                                                  
*                                                                               
EDTTEAR  ST    R9,SAVER9           BUILD NEW T/S ELEMENT IN X                   
         XC    X(50),X                                                          
         MVC   X(2),=X'9527'                                                    
*                                                                               
         LA    R5,X                                                             
         USING PTSHTELD,R5                                                      
         MVC   PTSHCDAT,BTODAY                                                  
*                                                                               
         MVC   PTSHPID,SVPID                                                    
*                                                                               
         MVC   PTSHBID,BUYNM                                                    
         CLI   BUYNM,C'*'          IF STARTS WITH * - DON'T MOVE *              
         BNE   *+10                                                             
         MVC   PTSHBID,BUYNM+1                                                  
         MVC   PTSHSTAT(10),=CL10' '                                            
         MVC   PTSHPAGE(10),=CL10' '                                            
*                                                                               
         MVI   BYTE2,0             NEEDED TO SET PBDSTAT                        
         CLC   AGYALPHA,=C'WI'     WESTERN?                                     
         BE    EDTWS                                                            
         CLC   AGYALPHA,=C'WJ'     WESTERN - TEST?                              
         BE    EDTWS                                                            
         CLC   AGYALPHA,=C'WR'     WESTERN - WRLA?                              
         BE    EDTWS                                                            
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    EDTST5                                                           
         CLI   5(R2),0                                                          
         BE    EDTEV                                                            
         CLI   8(R2),C'A'                                                       
         BE    EDTST5                                                           
         CLI   8(R2),C'N'                                                       
         BE    EDTST5                                                           
         LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
EDTST5   MVC   PTSHSTAT(1),8(R2)                                                
*                                                                               
         OI    CHGIND5,PCHGTSAQ    T/S APPROVED STATUS CHANGED                  
*                                                                               
         CLC   AGYALPHA,=C'WI'     WESTERN?                                     
         BE    EDTST5W                                                          
         CLC   AGYALPHA,=C'WJ'     WESTERN - TEST?                              
         BE    EDTST5W                                                          
         CLC   AGYALPHA,=C'WR'     WESTERN - WRLA?                              
         BE    EDTST5W                                                          
         CLI   PTSHSTAT,C'A'       APPROVED?                                    
         BNE   *+8                                                              
         OI    BYTE2,X'10'         SET TO APPROVED FOR PBDSTAT                  
         B     EDTST5X                                                          
*                                                                               
EDTST5W  CLI   PTSHSTAT,C'I'       APPROVED FOR PAYMENT BY BILLER?              
         BNE   *+8                                                              
         OI    BYTE2,X'10'         SET TO APPROVED FOR PBDSTAT                  
         B     EDTST5X                                                          
*                                                                               
EDTST5X  BRAS  R9,BUMPFLD2                                                      
         B     EDTEV                                                            
*                                                                               
* SPECIAL STATUS VALUES FOR WESTERN                                             
*                                                                               
EDTWS    TM    1(R2),X'20'         PROTECTED?                                   
         BO    EDTST5                                                           
*                                                                               
         CLI   BUYBILL,C'L'        BILLER?                                      
         BE    EDTWSB                                                           
*                                                                               
         CLI   BUYBILL,C'B'        BUYER WITH T/S ACCESS?                       
         BE    EDTWSBY                                                          
*                                                                               
         CLI   BUYBILL,C'X'        BOTH BUYER AND BILLER ACCESS?                
         BE    EDTWSX                                                           
*                                                                               
         LA    R2,BUYTR1H          INVALID TRANSACTION                          
         LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
EDTWSX   CLI   5(R2),0             BUYER AND BILLER ACCESS                      
         BE    EDTEV                                                            
         CLI   8(R2),C'A'                                                       
         BE    EDTST5                                                           
         CLI   8(R2),C'M'                                                       
         BE    EDTST5                                                           
         CLI   8(R2),C'R'                                                       
         BE    EDTST5                                                           
         CLI   8(R2),C'I'                                                       
         BE    EDTST5                                                           
         B     EDTWSERR                                                         
*                                                                               
EDTWSBY  CLI   5(R2),0                                                          
         BE    EDTEV                                                            
         CLI   8(R2),C'A'                                                       
         BE    EDTST5                                                           
         CLI   8(R2),C'M'                                                       
         BE    EDTST5                                                           
         B     EDTWSERR                                                         
*                                                                               
* BILLER VALID ENTRIES                                                          
*                                                                               
EDTWSB   CLI   8(R2),C'R'                                                       
         BE    EDTST5                                                           
         CLI   8(R2),C'I'                                                       
         BE    EDTST5                                                           
         B     EDTWSERR                                                         
*                                                                               
EDTWSERR LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
* EDIT EVALUATIONS                                                              
*                                                                               
EDTEV    LA    R2,TSHTS1H                                                       
         LA    R4,4                NEXT 4 FIELDS ARE N,Y                        
         LA    R7,PTSHIND1                                                      
*                                                                               
EDTT5    TM    1(R2),X'20'         PROTECTED?                                   
         BO    EDTT7                                                            
         CLI   5(R2),0             ANY INPUT?                                   
         BE    EDTT7X                                                           
         CLI   8(R2),C'N'                                                       
         BE    EDTT7                                                            
         CLI   8(R2),C'Y'                                                       
         BE    EDTT7                                                            
         LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
EDTT7    MVC   0(1,R7),8(R2)                                                    
EDTT7X   BCT   R4,EDTT8                                                         
         B     EDTT10                                                           
*                                                                               
EDTT8    BRAS  R9,BUMPFLD2         GET PAST ANNOTATION                          
         LA    R7,1(R7)                                                         
         B     EDTT5                                                            
*                                                                               
         B     EDTT7X                                                           
*                                                                               
EDTT10   BRAS  R9,BUMPFLD2         BUMP TO REPO QUALITY FLD                     
         TM    1(R2),X'20'         PROTECTED?                                   
         BZ    EDTT10C                                                          
         MVI   5(R2),2             SET INPUT LENGHT                             
         CLI   9(R2),C' '                                                       
         BH    EDTT10C                                                          
         MVI   5(R2),1                                                          
         CLI   8(R2),C' '                                                       
         BH    EDTT10C                                                          
         MVI   5(R2),0                                                          
*                                                                               
EDTT10C  CLI   5(R2),0             ANY INPUT?                                   
         BE    EDTT20                                                           
         LA    R3,INVERR                                                        
         ZIC   R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BE    EDTT20                                                           
         CP    DUB,=P'0'                                                        
         BL    ERROR               CAN'T BE NEGATIVE                            
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         BNE   ERROR                                                            
         CP    DUB(6),=P'10'                                                    
         BH    ERROR                                                            
         ZAP   DUB,DUB(6)                                                       
         CVB   R0,DUB                                                           
         STC   R0,PTSHREPO                                                      
*                                                                               
EDTT20   BRAS  R9,BUMPFLD2                                                      
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    EDTT20C                                                          
         CLI   5(R2),0                                                          
         BE    EDTT25                                                           
         CLI   8(R2),C'N'                                                       
         BE    EDTT20C                                                          
         CLI   8(R2),C'Y'                                                       
         BE    EDTT20C                                                          
         LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
EDTT20C  MVC   PTSHIND5,8(R2)                                                   
*                                                                               
EDTT25   BRAS  R9,BUMPFLD2                                                      
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    EDTT25C                                                          
         CLI   5(R2),0                                                          
         BE    EDTT30                                                           
EDTT25C  MVC   PTSHPAGE,8(R2)                                                   
         OC    PTSHPAGE,=CL10' '                                                
*                                                                               
EDTT30   OC    PTSHSTAT(10),=CL10' '                                            
*                                                                               
EDTT35   CLC   AGYALPHA,=C'WJ'     SPECIAL WESTERN - TEST EDIT?                 
         BE    EDTT35W                                                          
         CLC   AGYALPHA,=C'WR'     SPECIAL WESTERN - WRLA?                      
         BE    EDTT35W                                                          
         CLC   AGYALPHA,=C'WI'     SPECIAL WESTERN EDIT?                        
         BNE   EDTT80                                                           
*                                                                               
* SPECIAL WESTERN EDIT - FOR STATUS = A                                         
*                                                                               
EDTT35W  CLI   PTSHSTAT,C'A'       STATUS IS APPROVED?                          
         BNE   EDTT60                                                           
*                                                                               
* ALL INDICATORS MUST BE 'Y' OR COMMENTS MUST BE ENTERED                        
*                                                                               
         LA    RE,PTSHIND1                                                      
         LA    RF,5                NOTE - THIS INCLUDES ZONES                   
*                                                                               
EDTT40   CLI   0(RE),C'Y'                                                       
         BNE   EDTT50                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,EDTT40                                                        
         B     EDTT80                                                           
*                                                                               
EDTT50   CLI   TSHCOM1H+5,0        COMMENT ENTERED?                             
         BNE   EDTT80                                                           
         CLI   TSHCOM2H+5,0                                                     
         BNE   EDTT80                                                           
         CLI   TSHCOM3H+5,0                                                     
         BNE   EDTT80                                                           
         CLI   TSHCOM4H+5,0                                                     
         BNE   EDTT80                                                           
*                                                                               
         LA    R2,TSHSTATH         CURSOR TO STATUS                             
         LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
* ANOTHER SPECIAL EDIT FOR WESTERN                                              
*                                                                               
EDTT60   CLI   BUYBILL,C'L'        BILLER?                                      
         BE    EDTT60X                                                          
         CLI   BUYBILL,C'X'        BILLER/BUYER?                                
         BE    EDTT60X                                                          
         ST    R5,FULL             SAVE ADDRESS OF ELEM                         
*                                                                               
* CHECK FOR ANY T/S DATA, IF NONE, SET STATUS TO SPACES                         
*                                                                               
         CLC   PTSHIND1(9),=CL10' '                                             
         BNE   EDTT60F                                                          
         CLC   PTSHPAGE,=CL10' '                                                
         BNE   EDTT60F                                                          
         LA    R5,NEWREC+33        COMMENT ENTERED?                             
         MVI   ELCODE,X'69'                                                     
         BRAS  R9,NEXTEL                                                        
         BE    EDTT60F                                                          
*                                                                               
EDTT60C  L     R5,FULL                                                          
         MVI   PTSHSTAT,C' '       NO T/S DATA - SET STATUS TO SPACE            
         MVI   TSHSTAT,C' '                                                     
         FOUT  TSHSTATH                                                         
         B     EDTT60X                                                          
*                                                                               
EDTT60F  L     R5,FULL             IF DATA ENTERED, FORCE STATUS TO 'M'         
         MVI   PTSHSTAT,C'M'       NO T/S DATA - SET STATUS TO 'M'              
         MVI   TSHSTAT,C'M'        DISPLAY ON SCREEN                            
         FOUT  TSHSTATH                                                         
*                                                                               
EDTT60X  B     EDTT80                                                           
         DROP  R5                                                               
*                                                                               
EDTT80   LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'        POINT TO END OF RECORD                       
         BRAS  R9,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,(1,NEWREC),X,(R5)                                    
*                                                                               
EDTT90   L     R9,SAVER9                                                        
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
EDTCOM   ST    R9,SAVER9                                                        
         LA    R4,4                4 COMMENT LINES                              
*                                                                               
EDTC5    XC    X,X                                                              
         CLI   5(R2),0             ANY INPUT?                                   
         BE    EDTC20              GO CHECK NEXT LINE                           
*                                                                               
         MVI   X,X'69'                                                          
         ZIC   R1,5(R2)                                                         
         AHI   R1,2                FOR ELEM CODE AND LENGTH                     
         STC   R1,X+1                                                           
         AHI   R1,-3               BACK TO INPUT LENGTH -1                      
         EX    R1,MVCOM                                                         
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'        POINT TO END OF RECORD                       
         BRAS  R9,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,(1,NEWREC),X,(R5)                                    
*                                                                               
EDTC20   BRAS  R9,BUMPFLD                                                       
         BCT   R4,EDTC5                                                         
*                                                                               
EDTCX    L     R9,SAVER9                                                        
         BR    R9                                                               
*                                                                               
MVCOM    MVC   X+2(0),8(R2)                                                     
         EJECT                                                                  
*                                                                               
BUMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,BUMPFLDS                                                      
         BR    R9                                                               
*                                                                               
BUMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BR    R9                                                               
*                                                                               
NXTTR    GOTO1 VNXTTR                                                           
         CLI   ERRAREA,0                                                        
         BNE   TESTERR                                                          
         XC    INSDA,INSDA                                                      
         XC    INSKEY,INSKEY                                                    
         XC    INSADR,INSADR                                                    
         XC    BINSDT,BINSDT                                                    
         MVI   BSUBLN,0                                                         
         L     R2,TRADDR           GET NEW TR ADDR                              
         MVC   TRCODE,8(R2)                                                     
         LTR   R2,R2                                                            
         BR    R9                                                               
*                                                                               
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                                                             
         BCTR  R1,0                                                             
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
*                                                                               
FMTTR    GOTO1 VFMTTR,DMCB,REC                                                  
         BR    R9                                                               
*                                                                               
FMTINS   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VFMTINS,DMCB,REC                                                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         BR    R9                                                               
*                                                                               
EDTINS   GOTO1 VEDTINS,DMCB,(RC),(RA)                                           
         CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
NEXTEL   CLI   0(R5),0                                                          
         BC    8,NEXTELX                                                        
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                EXIT WITH CC EQUAL                           
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
NEXTELX  LTR   R5,R5               SET CC TO NOT EQUAL                          
         BR    R9                                                               
*                                                                               
ALLDONE  FOUT  BUYMSGH,CMPMSG                                                   
         MVI   ERRAREA,C'D'        FAKE ERROR TO SEND THIS MSG                  
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         B     EXIT                                                             
*                                                                               
CMPMSG   DC    C'** ACTION COMPLETED **'                                        
*                                                                               
BLDREC   LA    RF,BLDBRECQ                                                      
         B     G_T41103                                                         
*                                                                               
FNDINS   LA    RF,FINDINSQ                                                      
         B     G_T41103                                                         
*                                                                               
G_T41103 GOTOR VT41103,DMCB,(RC),(RA),(RF)                                      
         B     TESTERR                                                          
*                                                                               
TESTERR  CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD)            UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3               LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
CHGTAB   DC    X'01'               STATUS CHANGE                                
         DC    X'02'               SPACE                                        
         DC    X'04'               CAPTION                                      
         DC    X'08'               POSITION                                     
         DC    X'10'               INSERTION DATE                               
         DC    X'20'               FUTURE USE                                   
         DC    X'40'               FUTURE USE                                   
         DC    X'80'               FUTURE USE                                   
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
*                                                                               
TSTLK2   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK3                                                           
*                                                                               
TSTLK4   XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
         MVC   L.LOCKPUB,REC+10    PACKED BASE PUB NUMBER                       
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
TSTLK5   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLKEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
TSTLKEQ  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
*                                                                               
         DROP  L                                                                
*                                                                               
TSTLKWKA DS    0H                                                               
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   REC                 MAP BUY RECORD TO REC                        
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
PPF4D    DSECT                                                                  
       ++INCLUDE PPBUYF4D                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080PPBUY15   06/22/10'                                      
         END                                                                    
