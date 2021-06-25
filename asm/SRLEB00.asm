*          DATA SET SRLEB00    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T12400A                                                                  
         TITLE '$LERB - DISPLAY/CLEAR LERB DATA'                                
LERB     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,*$LERB**,RR=RE                                         
         USING WRKD,RC                                                          
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         USING SRPARMD,R1                                                       
         MVC   SRPARS,0(R1)                                                     
         DROP  R1                                                               
         L     RA,SRPAR6           A(TWA)                                       
         USING SRLEBFFD,RA                                                      
         L     R4,SRPAR1           A(SYSFAC)                                    
         USING SYSFACD,R4                                                       
*                                                                               
         L     RE,VSSB             EXIT IF VTAM APPLICATION                     
         CLI   SSBVTID-SSBD(RE),C' '                                            
         BH    EXIT                                                             
*                                                                               
         MVC   VDECB,VDECBLST                                                   
         L     R4,SRPAR4                                                        
         USING COMFACSD,R4                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VSCANNER,CSCANNER                                                
         DROP  R4                                                               
         LA    R0,MINTAB                                                        
         LA    R1,L'MINTAB                                                      
         SR    R3,R3                                                            
         MVCL  R0,R2               CLEAR MINTAB.                                
         XC    MINIMS,MINIMS                                                    
         MVI   ANDSW,0             SET FOR OR ON THRESH ARGUMENTS.              
         MVI   IDIPLEN,0                                                        
         XC    IDIP,IDIP                                                        
         EJECT                                                                  
* TEST FOR CURRENT (DEFAULT) OR TOTAL DISPLAY                                   
         MVC   LBOPT,=CL7'TOTAL'   FIND DISPLAY TYPE.                           
         ZICM  R2,SRVP2H+5                                                      
         BZ    LB1044                                                           
         BCTR  R2,0                                                             
         MVI   ERRTYP,TYPINVAL                                                  
         LA    R8,TYPTAB                                                        
         USING TYPTABD,R8                                                       
*                                                                               
LB1020   CLI   TYPNAM,X'FF'                                                     
         BE    ERROR               INVALID DISPLAY TYPE.                        
         EX    R2,TYPCMP                                                        
         BE    LB1040                                                           
         LA    R8,TYPTBLNQ(R8)                                                  
         B     LB1020                                                           
TYPCMP   CLC   TYPNAM(0),SRVP2                                                  
*                                                                               
LB1040   MVC   LBOPT,TYPTYP                                                     
         DROP  R8                                                               
         USING DECBD,R9                                                         
LB1044   L     R9,VDECB            R9 = A(DECBLST)                              
         LH    R2,0(R9)            SET UP FOR BXLE THROUGH DECB.                
         L     R3,2(R9)                                                         
         LA    R9,6(R9)                                                         
         STM   R2,R3,SAVBXLE       SAVE THE BXLE REGS.                          
*                                                                               
         CLI   SRVP2H+5,0          NO INPUT = ALL FROM THE FIRST.               
         BE    LB1046                                                           
         MVI   ERRTYP,IDALLIP                                                   
         CLI   SRVP2,C'R'          BUT TYPE RESET MUST HAVE SOME                
         BNE   LB1046              INPUT.                                       
         CLI   SRVP1H+5,0                                                       
         BE    ERROR                                                            
LB1046   CLC   SRVP1(3),=C'ALL'    ALLOW 'ALL'.                                 
         BE    LB1060                                                           
         MVI   ERRTYP,IDINVAL                                                   
         CLI   SRVP1H+5,0                                                       
         BE    LB1060                                                           
         CLI   SRVP1H+5,4          OTHERWISE FIND DECB ENTRY FOR LINE           
         BH    ERROR               ID I/P.                                      
         OI    SRVP1+3,X'40'                                                    
         ZIC   RE,SRVP1H+5         SAVE INPUT LENGTH.                           
         BCTR  RE,0                                                             
         STC   RE,IDIPLEN                                                       
         MVC   IDIP,SRVP1          SAVE DECB ID INPUT.                          
*                                                                               
LB1050   ZIC   RE,IDIPLEN          RE = L'INPUT-1.                              
         EX    RE,DECBCLC          CLC   DECBID(0),IDIP.                        
         BE    LB1060                                                           
         BXLE  R9,R2,LB1050                                                     
         B     ERROR               INVALID I/P.                                 
*                                                                               
LB1060   MVC   SRVMSG(7),LBOPT                                                  
         CLI   SRVP3H+5,0                                                       
         BE    LB11982             NO THRESHOLDS I/P.                           
         MVC   MINIMS(2),=X'7FFF'  MAXIMIZE THRESHOLD VALUES.                   
         MVC   MINIMS+2(L'MINIMS-2),MINIMS                                      
         MVI   ERRTYP,INVKYWD                                                   
         GOTO1 VSCANNER,DMCB,(0,SRVP3H),(10,SCANBLK),0                          
         LA    R8,SCANBLK                                                       
         ZICM  R5,DMCB+4                                                        
         BNZ   LB1080                                                           
         GOTO1 (RF),(R1),,(10,SCANBLK),C',=&&='                                 
         ZICM  R5,DMCB+4                                                        
         BZ    ERROR                                                            
         MVI   ANDSW,1             SET FOR AND ON THRESH ARGUMENTS.             
*                                                                               
LB1080   ZICM  R6,0(R8)                                                         
         BZ    LB11982             NO MORE THRESHOLDS.                          
         BCTR  R6,0                                                             
         LA    R7,DESTAB           R7 = A(TYPE DESCRIPTION TABLE).              
         USING DESTABD,R7                                                       
         MVI   PCNTIND,0                                                        
         MVI   CCMASK,X'80'                                                     
LB1100   LA    RE,12(R6,R8)        RE = A((NEXT) HI BYTE OF KEYWORD).           
         LA    RF,OPTAB            TEST THE BYTE FOR                            
         USING OPTABD,RF           LEGAL OPERATORS.                             
LB1110   CLI   OPTYP,X'FF'                                                      
         BE    LB1140                                                           
         CLC   OPTYP,0(RE)                                                      
         BE    LB1120              GOOD OPERATOR.                               
LB1112   LA    RF,OPTBLNQ(RF)                                                   
         B     LB1110                                                           
*                                                                               
LB1120   BCTR  R6,R0               POINT TO NEXT HIEST BYTE.                    
         CLI   OPTYP,C'%'          TREAT PERCENT INDICATOR SPECIALLY.           
         BNE   *+12                                                             
         MVI   PCNTIND,1                                                        
         B     *+10                TEST FOR OTHER OPERATORS.                    
         OC    CCMASK,OPCCMSK      HOLD THE REQUISITE CONDITION CODE.           
         B     LB1100                                                           
*                                                                               
LB1140   CLI   DESTYP,X'FF'                                                     
         BE    ERROR               INVALID THRESHOLD TYPE                       
         EX    R6,THRCMP                                                        
         BE    LB1160                                                           
         LA    R7,DESTBLNQ(R7)                                                  
         B     LB1140                                                           
THRCMP   CLC   DESTYP(0),12(R8)                                                 
*                                                                               
LB1160   TM    3(R8),X'80'         VALUE MUST BE NUMERIC.                       
         BZ    ERROR                                                            
         MVI   ERRTYP,PCNTINV                                                   
         CLI   PCNTIND,0                                                        
         BE    *+12                                                             
         TM    DESSTAT,PCNTOK                                                   
         BZ    ERROR               PERCENTAGE IS INVALID FOR KEYWORD.           
         MVI   ERRTYP,OPINV                                                     
         CLI   CCMASK,0                                                         
         BE    *+12                                                             
         TM    DESSTAT,GTLTOK                                                   
         BZ    ERROR               GT OR LT NOT VALID FOR KEYWORD.              
         MVI   ERRTYP,VALEXMAX                                                  
         L     R1,8(R8)                                                         
         C     R1,=F'32767'                                                     
         BH    ERROR               VALUE OUT OF RANGE                           
         STH   R1,HALF             HOLD THRESHOLD ARGUMENT.                     
*                                                                               
         TM    DESSTAT,MULTI       KEYWRD GENERATES MULTIPLE ARGUMENTS.         
         BO    LB1180                                                           
         GOTO1 BILDTAB,DESTYP      SET A MINTAB ENTRY                           
         B     LB1198                                                           
*                                                                               
LB1180   LA    R6,DESTAB                                                        
         TM    DESSTAT,ANDONLY                                                  
         BZ    *+8                 OR/AND AS ENTERED.                           
         MVI   ANDSW,1             ANDING IS FORCED                             
         DROP  R7                                                               
         USING DESTABD,R6                                                       
LB1190   CLI   DESTYP,X'FF'        'ANY' BUILDS TABLE ENTRIES FOR               
         BE    LB1198              EACH ERROR TYPE LERB COUNTER.                
         TM    DESSTAT,ERREQ                                                    
         BZ    LB1194                                                           
         GOTO1 BILDTAB,DESTYP                                                   
LB1194   LA    R6,DESTBLNQ(R6)                                                  
         B     LB1190                                                           
*                                                                               
*                                                                               
LB1198   LA    R8,32(R8)           CHECK FOR MORE KEYWORDS.                     
         BCT   R5,LB1080                                                        
*                                                                               
LB11982  LA    R8,DESTAB           SET UP TO BUILD TABLE TO DRIVE               
         USING DESTABD,R8          DISPLAY OF LERBS.                            
         LA    R7,DISTAB                                                        
         USING DISTABD,R7                                                       
         XC    DISTAB,DISTAB                                                    
*                                                                               
LB1200   CLI   DESTYP,X'FF'                                                     
         BE    LB1240              END OF DESCRIPTIONS.                         
         TM    DESSTAT,DISPLAY     TAKE ONLY DISPLAYABLE ITEMS.                 
         BZ    LB1220                                                           
         MVC   DISASCR,DESASCR     SET SCREEN LINE DISPLACEMENT.                
         CLI   LBOPT,C'C'                                                       
         BE    *+14                                                             
         MVC   DISALRB,DESATLRB    SET FOR TOTAL DISPLAY.                       
         B     *+10                                                             
         MVC   DISALRB,DESACLRB    SET FOR CURRENT DISPLAY.                     
LB1220   LA    R8,DESTBLNQ(R8)     NEXT DESCRIPTION.                            
         LA    R7,DISTBLNQ(R7)     NEXT DISPLAY TABLE SLOT.                     
         B     LB1200                                                           
*                                                                               
LB1240   LA    R6,SRVLN1           R6 = A(FIRST DISPLAY LINE).                  
         ST    R6,ALINE            SAVE IT.                                     
*                                                                               
LB2020   L     R8,DECBPOLL         R8 = A(POLLING LIST).                        
         L     R7,DECBLERB         R7 = A(LERBS).                               
         MVI   FSTPOL,0            MARK FIRST POLLIST ENTRY IND.                
         OC    IDIP,IDIP                                                        
         BZ    LB2040              NOTHING FOR COMPARISON.                      
         CLI   IDIPLEN,3                                                        
         BNL   LB2040              WHOLE ID I/P, NORMAL MODE.                   
         ZIC   RE,IDIPLEN          RE = L'ID INPUT.                             
         EX    RE,DECBCLC          CLC   DECBID(0),IDIP.                        
         BNE   LB4080              NO MATCH, TRY THE NEXT.                      
         USING LERBD,R7                                                         
*                                                                               
LB2040   TM    DECBSTAT,X'20'                                                   
         BZ    LB2050              NOT MULTI-DROP. SINGLE LERB.                 
         GOTO1 POLSTAT,DMCB,(R8),(R9) GET POLLIST ENTRY STATUS.                 
         TM    DECBTYPE,X'01'                                                   
         BZ    LB2050              S/S, POLLIST & LERBS COSEQUENTIAL.           
         BAS   RE,GETLERB          BSC, GET LERB FOR POLLIST ENTRY.             
LB2050   CLI   SRVP3H+5,0                                                       
         BE    LB3020              NO THRESHOLDS TO APPLY.                      
         LTR   R7,R7               IF NO LERB FOR THIS DECB IT                  
         BZ    LB4040              DOESN'T PASS THE THRESHOLDS.                 
         LA    R6,MINTAB           SET UP TO APPLY THRESHOLDS.                  
*                                                                               
         USING MINTABD,R6                                                       
LB2060   LA    RF,LB4040           FOR OR, TESTING ALL ARGUMENTS IS             
         CLI   ANDSW,1             FAILURE, FOR AND, IT'S SUCCESS.              
         BNE   *+8                                                              
         LA    RF,LB3020                                                        
         ZICM  RE,MINAMIN                                                       
         BZR   RF                                                               
         LA    RE,0(RE,RC)                                                      
         LH    R1,0(RE)                                                         
         CLI   MINPCNT,1                                                        
         BNE   *+8                                                              
         BAS   RE,GETPCNT                                                       
         STH   R1,HALF             HOLD THRESHOLD VALUE.                        
         LA    R5,MINALRB          R5 = A(LERB COUNTER,L'COUNTER).              
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    R4,10                                                            
*                                                                               
LB2080   ZICM  R3,0(R5)                                                         
         BM    LB2100              NO MORE TO ACCUMULATE.                       
         LA    R3,0(R3,R7)         R3 = A(LERB COUNTER).                        
         ZIC   R2,1(R5)            R2 = L LERB COUNTER                          
         BCTR  R2,0                                                             
         LA    R1,2                DERIVE MASK FOR ICM FROM LENGTH.             
         STH   R2,*+6              MASK = 2**LNTH - 1.                          
         SLL   R1,0                IF LENTH = 2, MASK = X'03'.                  
         BCTR  R1,0                                                             
         LR    R2,R1                                                            
         SR    R1,R1                                                            
         EX    R2,*+8                                                           
         B     *+8                                                              
         ICM   R1,0,0(R3)          R1 = VALUE IN LERB COUNTER                   
         AR    R0,R1                                                            
         LA    R5,2(R5)                                                         
         BCT   R4,LB2080           GET ANY OTHERS FOR ACCUMULATION.             
*                                                                               
LB2100   ZIC   R2,MINMSK           GET THE CONDITION CODE MASK.                 
         LA    RF,LB3020           FOR OR, PASSING ANY ARGUMENT IS              
         CLI   ANDSW,1             SUCCESS, FOR AND, FAILING ANY IS             
         BNE   *+14                FAILURE.                                     
         LA    RF,LB4040                                                        
         LA    RE,255              AND USES 1'S COMP OF SET                     
         XR    R2,RE               CONDITION CODE.                              
         CH    R0,HALF                                                          
         EX    R2,*+4                                                           
         BCR   0,RF                                                             
         LA    R6,MINTBLNQ(R6)     FAILED ON THIS ONE,                          
         B     LB2060              TRY ANOTHER.                                 
*                                                                               
         DROP  R6                                                               
LB3020   L     R6,ALINE            DISPLAY FOR THIS DECB/LERB.                  
         USING LBLINE,R6           R6 = A(SCREEN LINE).                         
         MVI   LBSTAT,C'+'                                                      
         TM    DECBSTAT,X'80'                                                   
         BO    LB3040                                                           
         MVI   LBSTAT,C'-'                                                      
         TM    DECBSTAT,X'10'                                                   
         BZ    LB3040                                                           
         MVI   LBSTAT,C'*'                                                      
LB3040   MVC   LBID,DECBID                                                      
         TM    DECBSTAT,X'20'                                                   
         BZ    LB3060                                                           
         ZIC   R0,LERBID                                                        
         BAS   RE,CVHEX                                                         
         MVC   LBPOLL,WORK+8                                                    
         TM    DECBSTAT,X'80'      LINE UP,                                     
         BZ    LB3060              NO.                                          
         MVC   LBSTAT,SKIPIND      USE PASSED ACTIVE/SKIP MARK.                 
*                                                                               
LB3060   L     R5,DECBDTF          R5 = A(DTF)                                  
         ZIC   R0,47(R5)                                                        
         BAS   RE,CVD                                                           
         MVC   LBSYS,WORK+7                                                     
         CLI   LBSYS,X'4B'                                                      
         BNE   *+8                                                              
         MVI   LBSYS,X'F0'                                                      
*                                                                               
         LTR   R7,R7               IF NO LERB ADDRESS,                          
         BZ    LB4024              GET NEXT.                                    
         LA    R5,DISTAB                                                        
         USING DISTABD,R5                                                       
*                                  DISPLAY THE LERB VALUES.                     
LB3080   CLI   DISASCR,0                                                        
         BE    LB4020              DONE THEM ALL.                               
         LA    R4,DISALRB          R4 = A(A(LERB COUNTER)                       
         LA    R3,10                                                            
         SR    R0,R0                                                            
LB3100   ZICM  RF,0(R4)                                                         
         BM    LB3120                                                           
         LA    RF,0(RF,R7)         RF = A(LERB COUNTER)                         
         ZIC   R2,1(R4)            R2 = L'LERB COUNTER.                         
         BCTR  R2,0                                                             
         LA    R1,2                DERIVE MASK FOR  CM FROM LENGTH,             
         STH   R2,*+6              MASK = 2**LN - 1,                            
         SLL   R1,0                IF LENGTH = 3, MASK = X'07'                  
         BCTR  R1,0                                                             
         LR    R2,R1                                                            
         SR    R1,R1                                                            
         EX    R2,*+8                                                           
         B     *+8                                                              
         ICM   R1,0,0(RF)                                                       
         AR    R0,R1                                                            
         LA    R4,2(R4)                                                         
         BCT   R3,LB3100                                                        
*                                                                               
LB3120   BAS   RE,CVD                                                           
         ZIC   R4,DISASCR                                                       
         LA    R4,0(R4,R6)         R4 A(POSITION IN SCREEN LINE).               
         MVC   0(5,R4),WORK+5                                                   
         LA    R5,DISTBLNQ(R5)                                                  
         B     LB3080                                                           
*                                                                               
*                                                                               
LB4020   CLI   SRVP2,C'R'                                                       
         BNE   *+10                                                             
         XC    0(29,R7),0(R7)                                                   
LB4024   ZIC   RE,SRVLN1H          GET L'(LINE +HEADER)                         
         LA    RF,SRVLAST+7        SCREEN END.                                  
         BXLE  R6,RE,LB4030        GOTO NEXT SCREEN LINE.                       
         LA    RF,SRVLAST+8        A HALF SCREEN IS FINISHED.                   
         CR    R6,RF               WHICH HALF.....                              
         BH    LB4100              BOTH HALVES ARE FULL                         
         LA    R6,SRVLN1+40        START SECOND HALF OF SCREEN.                 
LB4030   ST    R6,ALINE            SAVE A(NEXT SCREEN POSITION).                
*                                                                               
LB4040   TM    DECBSTAT,X'20'      TEST MULTI-DROP LINE                         
         BZ    LB4060              NO                                           
         CLI   POLEND,1                                                         
         BE    LB4060              THIS ENTRY WAS LAST IN POLLIST.              
         LA    R7,LERBLEN(R7)      GET LERB FOR NEXT POLLIST ENTRY.             
         L     R8,ANXTPOL          GET A(NEXT POLLIST ENTRY).                   
         TM    DECBTYPE,X'01'                                                   
         BZ    LB2040              R7 IS OK FOR S/S.                            
         L     R7,DECBLERB         BSC POINT R7 TO 1ST LERB FOR LINE.           
         B     LB2040                                                           
*                                                                               
*                                                                               
LB4060   CLI   SRVP2,C'R'                                                       
         BE    *+16                                                             
         CLI   SRVP2,C'+'                                                       
         BNE   LB4120                                                           
         B     LB4080                                                           
         CLC   SRVP1(3),=C'ALL'                                                 
         BE    LB4080                                                           
         OC    IDIP,IDIP                                                        
         BZ    EXIT                                                             
         CLI   IDIPLEN,3                                                        
         BNL   EXIT                                                             
LB4080   LM    R2,R3,SAVBXLE                                                    
         BXLE  R9,R2,LB2020                                                     
         B     EXIT                                                             
*                                                                               
LB4100   LM    R2,R3,SAVBXLE                                                    
         BXH   R9,R2,EXIT          ALSO FINISHED DECB                           
         MVC   SRVP1(4),DECBID     SET NEXT LINE ID FOR NEXT CALL.              
         OI    SRVP1H+6,X'81'                                                   
         B     EXIT                                                             
*                                                                               
LB4120   LA    R6,L'SRVLN1(R6)                                                  
         MVC   0(3,R6),=X'000100'                                               
*                                                                               
EXIT     XMOD1 1                                                                
DECBCLC  CLC   DECBID(0),IDIP                                                   
*                                                                               
POLSTAT  NTR1                                                                   
         L     R8,0(R1)            R8 = A(POLLIST ENTRY).                       
         L     R9,4(R1)            R9 = A(DECB ENTRY)                           
         CLI   FSTPOL,1                                                         
         BE    POLSS020            NOT FIRST FOR POLLIST.                       
         MVI   SKIPIND,C'+'        SET SKIP/ACTIVE IND TO ACTIVE                
         MVI   POLEND,0            SET POLLIST END IND TO NOT END.              
*                                                                               
POLSS020 TM    DECBTYPE,X'01'      TEST FOR S/S OR BSC.                         
         BO    POLBSC              TWAS BSC.                                    
*&&DO                                                                           
         TM    2(R8),X'80'         START/STOP.                                  
         BZ    *+8                 NOT END OF POLLIST.                          
*&&                                                                             
*&&OS                                                                           
         CLI   3(R8),X'FF'         START/STOP.                                  
         BNE   *+8                 NOT END OF POLLIST.                          
*&&                                                                             
         MVI   POLEND,1            THIS ENTRY IS LAST.                          
         CLC   0(1,R8),3(R8)                                                    
         BL    *+8                                                              
         MVI   POLEND,1            FORCE LIST END ON NONASCENDING SEQ.          
         MVI   SKIPIND,C'+'                                                     
         TM    2(R8),X'40'                                                      
         BZ    *+8                 ACTIVE.                                      
         MVI   SKIPIND,C'-'        SKIP.                                        
         LA    R8,3(R8)                                                         
         ST    R8,ANXTPOL          PASS A(NEXT POLLIST ENTRY).                  
         B     EXIT                RETURN.                                      
*              BI-SYNC LISTS.                                                   
POLBSC   CLI   FSTPOL,0                                                         
         BNE   POLBS020            NOT FIRST FOR POLLIST.                       
         MVI   FSTPOL,1                                                         
         CLI   0(R8),X'37'                                                      
         BNE   POLBS040            THERE ARE ACTIVE ENTRIES.                    
         MVI   SKIPIND,C'-'        NO ACTIVE ENTRIES.                           
*&&DO*&& LA    R8,1(R8)                                                         
*&&OS*&& LA    R8,6(R8)                                                         
         CLI   0(R8),X'FE'                                                      
         BNE   POLBS060            THERE ARE INACTIVE ENTRIES.                  
         MVI   SKIPIND,C'?'        NOT ACTIVE OR INACTIVE, QUERY.               
         MVI   POLEND,1            FORCE POLEND                                 
         B     XIT8                RETURN.                                      
*              ENTRIES OF BOTH TYPES.                                           
POLBS020 CLI   SKIPIND,C'-'                                                     
         BE    POLBS060            THIS ONE'S INACTIVE.                         
         CLI   0(R8),X'37'                                                      
         BNE   POLBS040            ACTIVE ENTRY, DEAL WITH IT.                  
         MVI   SKIPIND,C'-'        END OF ACTIVE ENTRIES.                       
*&&DO*&& LA    R8,1(R8)            POINT TO FIRST INACTIVE.                     
*&&OS*&& LA    R8,6(R8)            POINT TO FIRST INACTIVE.                     
         B     POLBS060            DEAL WITH IT.                                
*              ACTIVE ENTRIES ONLY.                                             
POLBS040 CLI   6(R8),X'37'                                                      
         BNE   POLBS080            NOT LAST ACTIVE, GET A(NEXT).                
*&&DO*&& CLI   7(R8),X'FE'         LAST ACTIVE ENTRY,                           
*&&OS*&& CLI   12(R8),X'FE'        LAST ACTIVE ENTRY,                           
         BNE   POLBS080            BUT THERE ARE STILL SOME INACTIVES.          
         MVI   POLEND,1            NO INACTIVES, THIS IS LAST FOR LIST.         
         B     XIT8                                                             
*              INACTIVE ENTRIES ONLY.                                           
POLBS060 CLI   6(R8),X'FE'                                                      
         BNE   POLBS080            MORE INACTIVES TO COME.                      
         MVI   POLEND,1            LAST INACTIVE = LAST FOR LIST.               
         B     XIT8                                                             
*              ALL ENTRIES BUT LAST FOR LIST.                                   
POLBS080 LA    R7,6(R8)                                                         
         ST    R7,ANXTPOL          PASS A(NEXT POLLIST ENTRY).                  
XIT8     XIT1  REGS=(R8)           RETURN                                       
*                                                                               
GETLERB  NTR1                                                                   
         L     R2,DECBPOLL         AT ENTRY R8 = A(POLLIST ENTRY).              
         SH    R2,=H'3'            R2 = A(POLLIST CONTROL BYTES).               
         ZIC   R1,0(R2)            R1 = NO OF LERBS FOR LINE.                   
*&&OS*&& BCTR  R1,0                ADJUST                                       
         L     R7,DECBLERB         R7 = A(1ST LERB FOR LINE).                   
*                                                                               
GLB040   CLC   LERBID,0(R8)                                                     
         BE    XIT7                RIGHT LERB FOR POLLIST ENTRY.                
         LA    R7,LERBLEN(R7)      R7 = A(NEXT LERB).                           
         BCT   R1,GLB040                                                        
         DC    H'0',C'NO MATCH ON LERBID AND POLLIST CONTROLLER ID'             
*                                                                               
         DS    0H                                                               
XIT7     XIT1  REGS=(R7)                                                        
*                                                                               
         EJECT                                                                  
         USING ERRTABD,R7                                                       
ERROR    LA    R7,ERRTAB                                                        
ERR010   CLI   ERRNUM,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ERRNUM,ERRTYP                                                    
         BE    *+12                                                             
         LA    R7,ERRTBLNQ(R7)                                                  
         B     ERR010                                                           
*                                                                               
         ZICM  R2,ERRSCR,2         R2 = A(APT FIELD HDR)                        
         LA    R2,0(R2,RA)                                                      
         MVC   SRVMSG(L'BASERR),BASERR  SET BASE ERROR MSG.                     
         ZICM  RE,ERRAMSG,3                                                     
         A     RE,RELO                                                          
         ZICM  RF,ERRARTN,3                                                     
         A     RF,RELO             RF = A(ERROR ROUTINE).                       
         BR    RF                                                               
*                                                                               
ERROR1   MVC   SRVMSG+L'BASERR(MSGLEN),0(RE)                                    
         B     ERRXT                                                            
*                                                                               
ERROR2   ZIC   R6,0(R8)                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   SRVMSG+L'BASERR(0),12(R8) MOVE THRSHTYP I/P.                     
         LA    R6,SRVMSG+L'BASERR+1(R6)                                         
         MVC   0(MSGLEN2,R6),0(RE)                                              
         B     ERRXT                     MOVE IN DETAIL MSG.                    
*                                                                               
ERRXT    NI    SRVIDH+6,X'BF'      UNSET CURSOR                                 
         OI    6(R2),X'40'         INSERT CURSOR AT ERROR                       
         MVC   SRVHD1H(3),=X'000100'   TRUNCATE SCREEN.                         
         B     EXIT                                                             
*                                                                               
CVD      SLL   R0,8                                                             
         SRL   R0,8                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         CLI   WORK+5,C'0'                                                      
         BNER  RE                                                               
         MVI   WORK+5,C' '                                                      
         CLC   WORK+6(4),=4C'0'                                                 
         BNER  RE                                                               
         MVC   WORK+6(4),=4C'.'                                                 
         BR    RE                                                               
         EJECT                                                                  
CVHEX    NTR1                                                                   
         XC    WORK,WORK                                                        
         ST    R0,DUB                                                           
         GOTO1 VHEXOUT,DMCB,DUB,WORK+2,4,=C'TOG'                                
         XIT1                                                                   
*                                                                               
BILDTAB  NTR1                                                                   
         LR    R8,R1               R8 = A(DESCRIPTION ENTRY)                    
         USING DESTABD,R8                                                       
         LA    R7,MINTAB           R7 = A(MINIMUN TABLE)                        
         USING MINTABD,R7                                                       
         LA    R6,MINIMS-WRKD      SET TO GET DISP OF 1ST FREE MINIM.           
         LA    R5,15                                                            
*                                                                               
BIL020   CLI   MINAMIN,0           FIND FIRST FREE MINTAB LINE.                 
         BE    BIL040                                                           
         LA    R7,MINTBLNQ(R7)                                                  
         LA    R6,2(R6)            NEXT MINIM.                                  
         BCT   R5,BIL020                                                        
         MVI   ERRTYP,THREXMAX                                                  
         B     ERROR                                                            
*                                                                               
BIL040   STC   R6,MINAMIN          SET A(MINIM) FOR THIS ARGUMENT.              
         LA    R6,0(R6,RC)         GET RELO A(MINIM)                            
         MVC   0(2,R6),HALF        IN WHICH TO STORE THE ARGUMENT.              
         MVC   MINPCNT,PCNTIND     SET PERCENTAGE MARKER.                       
         MVC   MINMSK,CCMASK       SET CONDITION CODE MASK.                     
         MVC   MINALRB,DESATLRB                                                 
         CLI   LBOPT,C'C'                                                       
         BNE   *+10                                                             
         MVC   MINALRB,DESACLRB                                                 
         XIT1                                                                   
*                                                                               
GETPCNT  NTR1                                                                   
         USING LERBD,R7                                                         
*                                                                               
         L     R0,LERBTTR          GET TOTAL TRANS FROM THIS LERB.              
         CLI   LBOPT,C'C'                                                       
         BNE   *+10                                                             
         ZIC   R0,LERBCTR          GET CURRENT TRANS FROM THIS LERM.            
         LTR   R0,R0               LERB OF ZERO TRANSACTIONS                    
         BNZ   *+12                FAILS THE TEST.                              
         L     R1,=F'32767'                                                     
         B     GETEXIT                                                          
         MR    R0,R0               APPLY PERCENTAGE IN R1.                      
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
GETEXIT  XIT1  REGS=(R1)                                                        
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
ERREQ    EQU   X'01'                                                            
DISPLAY  EQU   X'02'                                                            
MULTI    EQU   X'04'                                                            
PCNTOK   EQU   X'10'                                                            
GTLTOK   EQU   X'20'                                                            
ANDONLY  EQU   X'40'                                                            
ALL      EQU   ERREQ+DISPLAY+PCNTOK+GTLTOK                                      
*                                                                               
TYPINVAL EQU   1                                                                
IDALLIP  EQU   2                                                                
IDINVAL  EQU   3                                                                
INVKYWD  EQU   4                                                                
PCNTINV  EQU   5                                                                
OPINV    EQU   6                                                                
VALEXMAX EQU   7                                                                
THREXMAX EQU   8                                                                
         EJECT                                                                  
DESTAB   DC    C'TR ',AL1(LBTR-LBLINE)                                          
         DC    AL1(DISPLAY+GTLTOK)                                              
         DC    AL1(LERBTTR-LERBD,L'LERBTTR)                                     
         DC    9AL1(255,255)                                                    
         DC    AL1(LERBCTR-LERBD,L'LERBCTR)                                     
         DC    9AL1(255,255)                                                    
*                                                                               
         DC    C'TO ',AL1(LBTO-LBLINE)                                          
         DC    AL1(ALL)                                                         
         DC    AL1(LERBTTO-LERBD,L'LERBTTO)                                     
         DC    9AL1(255,255)                                                    
         DC    AL1(LERBCTO-LERBD,L'LERBCTO)                                     
         DC    9AL1(255,255)                                                    
*                                                                               
         DC    C'DC ',AL1(LBDC-LBLINE)                                          
         DC    AL1(ALL)                                                         
         DC    AL1(LERBTDC-LERBD,L'LERBTDC)                                     
         DC    9AL1(255,255)                                                    
         DC    AL1(LERBCDC-LERBD,L'LERBCDC)                                     
         DC    9AL1(255,255)                                                    
*                                                                               
         DC    C'IR ',AL1(LBIR-LBLINE)                                          
         DC    AL1(ALL)                                                         
         DC    AL1(LERBTIR-LERBD,L'LERBTDC)                                     
         DC    9AL1(255,255)                                                    
         DC    AL1(LERBCIR-LERBD,L'LERBCDC)                                     
         DC    9AL1(255,255)                                                    
*                                                                               
         DC    C'OTH',AL1(LBOTH-LBLINE)               )                         
         DC    AL1(ALL)                                                         
         DC    AL1(LERBTCR-LERBD,L'LERBTCR)                                     
         DC    AL1(LERBTBO-LERBD,L'LERBTBO)                                     
         DC    AL1(LERBTEC-LERBD,L'LERBTEC)                                     
         DC    AL1(LERBTLD-LERBD,L'LERBTLD)                                     
         DC    AL1(LERBTXX-LERBD,L'LERBTXX)                                     
         DC    5AL1(255,255)                                                    
         DC    AL1(LERBCCR-LERBD,L'LERBCCR)                                     
         DC    AL1(LERBCBO-LERBD,L'LERBCBO)                                     
         DC    AL1(LERBCEC-LERBD,L'LERBCEC)                                     
         DC    AL1(LERBCLD-LERBD,L'LERBCLD)                                     
         DC    AL1(LERBCXX-LERBD,L'LERBCXX)                                     
         DC    5AL1(255,255)                                                    
*                                                                               
         DC    C'ANY',AL1(0)                                                    
         DC    AL1(PCNTOK+GTLTOK+MULTI)                                         
         DC    20AL1(255,255)                                                   
*                                                                               
         DC    C'ALL',AL1(0)                                                    
         DC    AL1(PCNTOK+GTLTOK+ANDONLY+MULTI)                                 
         DC    20AL1(255,255)                                                   
*                                                                               
         DC    AL1(255)                                                         
*                                                                               
TYPTAB   DC    C'CURRENT',C'CURRENT'                                            
         DC    C'TOTAL  ',C'TOTAL  '                                            
         DC    C'RESET  ',C'RESET  '                                            
         DC    CL7'+',CL7'TOTAL'                                                
         DC    AL1(255)                                                         
*                                                                               
ERRTAB   DC    AL1(TYPINVAL),AL2(SRVP2H-SRLEBFFD),AL3(ERROR1),AL3(TYP)          
         DC    AL1(IDALLIP),AL2(SRVP1H-SRLEBFFD),AL3(ERROR1),AL3(IDALL)         
         DC    AL1(IDINVAL),AL2(SRVP1H-SRLEBFFD),AL3(ERROR1),AL3(IDINV)         
         DC    AL1(INVKYWD),AL2(SRVP3H-SRLEBFFD),AL3(ERROR2)                    
         DC    AL3(KYVAL)                                                       
         DC    AL1(PCNTINV),AL2(SRVP3H-SRLEBFFD),AL3(ERROR2),AL3(PCNT)          
         DC    AL1(OPINV),AL2(SRVP3H-SRLEBFFD),AL3(ERROR2),AL3(OPINVAL)         
         DC    AL1(VALEXMAX),AL2(SRVP3H-SRLEBFFD),AL3(ERROR2)                   
         DC    AL3(EXMAX)                                                       
         DC    AL1(THREXMAX),AL2(SRVP3H-SRLEBFFD),AL3(ERROR1)                   
         DC    AL3(THREX)                                                       
         DC    AL1(255)                                                         
*                                                                               
TYP      DC    CL48'VALID TYPES ARE TOTAL, CURRENT, RESET'                      
IDALL    DC    CL48'FOR RESET LINE ID OR ''ALL'' ARE NEEDED'                    
IDINV    DC    CL48'LINE ID IS INVALID'                                         
KYVAL    DC    CL40'IS INVALID KEYWORD'                                         
PCNT     DC    CL40'CAN NOT BE A PERCENTAGE OF ''TR'''                          
OPINVAL  DC    CL40'CAN NOT USE ''<'' OR ''>'''                                 
EXMAX    DC    CL40'VALUE EXCEED MAXIMUM (32767)'                               
THREX    DC    CL48'TOO MANY THRESHOLD ARGUMENTS'                               
*                                                                               
BASERR   DC    CL12'** ERROR ** '                                               
*                                                                               
MSGLEN   EQU   L'TYP                                                            
MSGLEN2  EQU   L'KYVAL                                                          
*                                                                               
OPTAB    DC    C'>',X'B0'                                                       
         DC    C'<',X'D0'                                                       
         DC    C'%',X'00'                                                       
         DC    AL1(255)                                                         
*                                                                               
*                                                                               
DESTABD  DSECT                                                                  
DESTYP   DS    CL3                                                              
DESASCR  DS    AL1                                                              
DESSTAT  DS    AL1                                                              
DESATLRB DS    10AL2                                                            
DESACLRB DS    10AL2                                                            
DESTBLNQ EQU   *-DESTABD                                                        
*                                                                               
MINTABD  DSECT                                                                  
MINAMIN  DS    CL1                                                              
MINPCNT  DS    CL1                                                              
MINMSK   DS    AL1                                                              
MINALRB  DS    CL(10*2)                                                         
MINTBLNQ EQU   *-MINTABD                                                        
*                                                                               
DISTABD  DSECT                                                                  
DISASCR  DS    CL1                                                              
DISALRB  DS    CL(10*2)                                                         
DISTBLNQ EQU   *-DISTABD                                                        
*                                                                               
TYPTABD  DSECT                                                                  
TYPNAM   DS    CL7                                                              
TYPTYP   DS    CL7                                                              
TYPTBLNQ EQU   *-TYPTABD                                                        
*                                                                               
OPTABD   DSECT                                                                  
OPTYP    DS    C                                                                
OPCCMSK  DS    X                                                                
OPTBLNQ  EQU   *-OPTABD                                                         
*                                                                               
ERRTABD  DSECT                                                                  
ERRNUM   DS    AL1                                                              
ERRSCR   DS    AL2                                                              
ERRARTN  DS    AL3                                                              
ERRAMSG  DS    AL3                                                              
ERRTBLNQ EQU   *-ERRTABD                                                        
*                                                                               
*                                                                               
*                                                                               
WRKD     DSECT                                                                  
SRPARS   DS    0CL24                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
*                                                                               
MINIMS   DS    0CL30                                                            
         DS    15H                                                              
*                                                                               
SAVBXLE  DS    2F                                                               
DMCB     DS    CL24                                                             
RELO     DS    A                                                                
VHEXOUT  DS    A                                                                
VSCANNER DS    A                                                                
VDECB    DS    A                                                                
ALINE    DS    A                                                                
ANXTPOL  DS    A                                                                
*                                                                               
DUB      DS    D                                                                
HALF     DS    H                                                                
WORK     DS    CL10                                                             
LBOPT    DS    CL7                                                              
POLLWORK DS    XL78                                                             
MINTAB   DS    CL(15*MINTBLNQ+1)                                                
DISTAB   DS    CL(5*DISTBLNQ+1)                                                 
SCANBLK  DS    10CL32                                                           
PCNTIND  DS    CL1                                                              
CCMASK   DS    AL1                                                              
ERRTYP   DS    AL1                                                              
ANDSW    DS    CL1                                                              
FSTPOL   DS    C                                                                
POLEND   DS    C                                                                
SKIPIND  DS    C                                                                
IDIPLEN  DS    X                                                                
IDIP     DS    CL4                                                              
WRKX     EQU   *                                                                
*                                                                               
         SPACE 2                                                                
*                                                                               
* DSECT FOR DISPLAY LINE                                                        
LBLINE   DSECT                                                                  
*                                                                               
LBSTAT   DS    C                                                                
LBID     DS    CL4                 STATUS                                       
         DS    C                                                                
LBPOLL   DS    CL2                 POLL CHAR (MULTI-DROP ONLY)                  
         DS    C                                                                
LBSYS    DS    CL3                 SYSNUM                                       
LBTR     DS    CL5                 TRANSMISSIONS                                
LBTO     DS    CL5                 TIME-OUTS                                    
LBDC     DS    CL5                 DATA CHECKS                                  
LBIR     DS    CL5                 INTVNTN REQ                                  
LBOTH    DS    CL5                 OTHER                                        
*                                                                               
*        MAX LEN = 39                                                           
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
SRLEBFFD DSECT                                                                  
         DS    CL64                                                             
* SRLEBFFD                                                                      
       ++INCLUDE SRLEBFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SRLEB00   05/01/02'                                      
         END                                                                    
