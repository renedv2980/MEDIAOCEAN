*          DATA SET GASEV00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0700A                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'SEVENS CARD GAME'                                               
         PRINT NOGEN                                                            
SEVENS   CSECT                                                                  
         NMOD1 40,GASEV00                                                       
         LR    R2,R1                                                            
         L     RA,4(R1)                                                         
         USING SEVFRST,RA                                                       
         LR    R9,RC                                                            
         USING SEVWRK,R9                                                        
         CLI   16(RA),X'FF'                                                     
         BE    SEVXITX                                                          
         LA    R5,HANDS                                                         
         ST    R5,AHAND1                                                        
         LA    R5,18(R5)                                                        
         ST    R5,AHAND2                                                        
         LA    R5,18(R5)                                                        
         ST    R5,AHAND3                                                        
         MVC   AHANDX(12),AHANDS                                                
         CLI   16(RA),0                                                         
         BNE   SEV18                                                            
         MVC   SAVPLIST(15),=C'     ALICEBASIL'                                 
         LA    R2,SEVINPTH                                                      
         BAS   RE,SEVANY                                                        
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SAVPLIST(0),8(R2)                                                
         MVC   SEVCARD+1(5),SAVPLIST                                            
         LA    R4,SEVCARDH                                                      
         MVC   17(2,R4),=C'17'                                                  
         FOUT  (R4)                                                             
         LA    R4,87(R4)                                                        
         MVC   17(2,R4),=C'17'                                                  
         FOUT  (R4)                                                             
         LA    R4,87(R4)                                                        
         MVC   17(2,R4),=C'18'                                                  
         FOUT  (R4)                                                             
         EJECT                                                                  
*              START OF NEW GAME ROUTINES                                       
         SPACE 1                                                                
SEV2     XC    HANDS(54),HANDS                                                  
         MVC   PUSED+0(64),=4CL16'YNNNNNNNNNNNNNYY'                             
*                                 B123456789TJQKBB                              
         LA    R3,52                                                            
         MVC   PACK2,PACK                                                       
         LA    R4,PACK2                                                         
         TBIN  MILLI                                                            
         SLL   R1,2                                                             
         SRL   R1,2                                                             
         MR    R0,R1                                                            
         SPACE 1                                                                
SEV6     SR    R0,R0                                                            
         D     R0,=F'52'                                                        
         AH    R0,=H'1'                                                         
         STC   R0,0(R4)                                                         
         AR    R1,R0                                                            
         LA    R4,2(R4)                                                         
         BCT   R3,SEV6                                                          
         GOTO1 =V(XSORT),DMCB,(0,PACK2),52,2,1,0,RR=RB                          
         SPACE 1                                                                
SEV10    LA    R4,PACK2                                                         
         LA    R5,18                                                            
         SPACE 1                                                                
SEV12    LA    R6,3                                                             
         SPACE 1                                                                
SEV14    LR    R7,R6                                                            
         BCTR  R7,R0                                                            
         MH    R7,=H'4'                                                         
         LA    R7,AHANDS(R7)                                                    
         L     R8,0(R7)                                                         
         MVC   0(1,R8),1(R4)                                                    
         LA    R8,1(R8)                                                         
         ST    R8,0(R7)                                                         
         LA    R4,2(R4)                                                         
         BCT   R6,SEV14                                                         
         BCT   R5,SEV12                                                         
         MVI   16(RA),X'01'                                                     
         MVI   MESSG,9                                                          
         BAS   RE,SEVMESG                                                       
         LA    R2,SEVINPTH                                                      
         B     SEVXIT                                                           
         EJECT                                                                  
*              VALIDATE INPUT FIELDS                                            
         SPACE 1                                                                
SEV18    LA    R2,SEVINPTH                                                      
         BAS   RE,SEVANY                                                        
         CLI   SEVINPT,C'P'                                                     
         BE    SEVPASS                                                          
         CLI   5(R2),2                                                          
         BE    SEV19                                                            
         CLC   8(5,R2),=C'GOFOR'                                                
         BNE   SEVERR                                                           
         MVI   PLAYER,0            PLAY FOR USER CODE                           
         BAS   RE,SEVMYGO                                                       
         MVI   MESSG,0                                                          
         BAS   RE,SEVMESG                                                       
         B     SEVCOM                                                           
         SPACE 1                                                                
SEV19    DS    0H                                                               
         LA    R3,CARDS                                                         
         LA    R4,13                                                            
         CLC   0(1,R3),SEVINPT                                                  
         BE    SEV20                                                            
         LA    R3,1(R3)                                                         
         BCT   R4,*-14                                                          
         MVI   MESSG,1                                                          
         LA    R2,SEVINPTH                                                      
         BAS   RE,SEVMESG                                                       
         B     SEVXIT                                                           
         SPACE 1                                                                
SEV20    LA    R4,CARDS                                                         
         SR    R3,R4                                                            
         LA    R3,1(R3)                                                         
         STC   R3,CARD                                                          
         MVC   CARDH,CARD                                                       
         LA    R3,SUITS                                                         
         LA    R4,4                                                             
         CLC   0(1,R3),SEVINPT+1                                                
         BE    SEV22                                                            
         LA    R3,1(R3)                                                         
         BCT   R4,*-14                                                          
         MVI   MESSG,2                                                          
         LA    R2,SEVINPTH                                                      
         BAS   RE,SEVMESG                                                       
         B     SEVXIT                                                           
         SPACE 1                                                                
SEV22    LA    R4,SUITS                                                         
         SR    R3,R4                                                            
         LA    R3,1(R3)                                                         
         STC   R3,SUITH                                                         
         SLL   R3,4                                                             
         STC   R3,SUIT                                                          
         OC    CARD,SUIT           CARD= 4BIT SUIT+4BIT CARD#                   
         L     R3,AHANDA           GET ADDRESS OF PLAYERS HAND                  
         LA    R4,18                                                            
         CLC   0(1,R3),CARD        CHECK IF PLAYER HAS THIS CARD                
         BE    SEV24               IN HIS HAND                                  
         LA    R3,1(R3)                                                         
         BCT   R4,*-14                                                          
         MVI   MESSG,3             PLAYER HAS NOT GOT CARD                      
         LA    R2,SEVINPTH                                                      
         BAS   RE,SEVMESG                                                       
         B     SEVXIT              EXIT                                         
         SPACE 1                                                                
SEV24    CLI   PUSED+7,C'Y'        HAS THE GAME STARTED YET ?                   
         BE    SEV26               YES                                          
         CLI   CARD,X'17'          HAS HE INPUT THE 7 OF DIAMONDS               
         BNE   SEVCHT              NO - HE IS TRYING TO CHEAT                   
         LA    R4,CARD                                                          
         MVI   PLAYER,0            PLAYER                                       
         BAS   RE,SEVCHK                                                        
         CLI   VALID,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SETOFF           SET OFF THIS CARD                            
         BAS   RE,SETCARD          MOVE VALUES TO TWA                           
         MVI   MESSG,0                                                          
         BAS   RE,SEVMESG                                                       
         B     SEVCOM              NOW ITS MY GO                                
         SPACE 1                                                                
SEV26    LA    R4,CARD                                                          
         MVI   PLAYER,0                                                         
         BAS   RE,SEVCHK           CHECK IF THIS IS A VALID GO                  
         CLI   VALID,0                                                          
         BNE   SEVERR              IF NOT - DO SOMETHING                        
         BAS   RE,SETOFF                                                        
         BAS   RE,SETCARD          MOVE CARD VALUES TO TWA                      
         L     R4,AHANDA                                                        
         CLC   1(17,R4),0(R4)      HAS HE ANY CARDS LEFT                        
         BE    SEVHWIN             NO - THEN HE HAS WON                         
         MVI   MESSG,0                                                          
         BAS   RE,SEVMESG                                                       
         B     SEVCOM              YES- ITS MY GO                               
         SPACE 1                                                                
SEVCHT   MVI   MESSG,4                                                          
         BAS   RE,SEVMESG                                                       
         LA    R2,SEVINPTH                                                      
         B     SEVXIT                                                           
         SPACE 1                                                                
SEVERR   MVI   MESSG,5                                                          
         BAS   RE,SEVMESG                                                       
         LA    R2,SEVINPTH                                                      
         B     SEVXIT                                                           
         EJECT                                                                  
*              PRINTING OF ERROR MESSAGES                                       
         SPACE 1                                                                
SEVMESG  NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,MESSG            ERROR #                                      
         MH    R1,=H'60'                                                        
         LA    R1,SEVELIST(R1)                                                  
         FOUT  SEVMESSH,(R1),60                                                 
         B     SEVXIT1                                                          
         SPACE 1                                                                
*              VALIDATE INPUT PRESENCE                                          
         SPACE 1                                                                
SEVANY   CLI   5(R2),0                                                          
         BNER  RE                                                               
         FOUT  SEVMESSH,=CL60'MISSING INPUT FIELD'                              
         B     SEVXIT                                                           
         SPACE 1                                                                
*              GENERAL EXIT FROM NTRD ROUTINES                                  
         SPACE 1                                                                
SEVXIT1  XIT1                                                                   
         EJECT                                                                  
*              FORMAT SCREEN FOR 1 PLAYER                                       
         SPACE 1                                                                
SETCARD  NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,CARDH                                                         
         LA    R2,13                                                            
         SR    R2,R1                                                            
         MH    R2,=H'87'                                                        
         LA    R2,SEVLIN1(R2)      R2 POINTS TO SCREEN LINE                     
         LR    R4,R2                                                            
         SH    R4,=H'8'                                                         
         FOUT  (R4)                                                             
         IC    R1,SUITH                                                         
         BCTR  R1,R0                                                            
         MH    R1,=H'11'                                                        
         LA    R2,4(R2,R1)         R1 ACTUAL TWA ADDRESS                        
         SR    R1,R1                                                            
         IC    R1,PLAYER                                                        
         MH    R1,=H'5'                                                         
         LA    R1,SAVPLIST(R1)                                                  
         MVC   0(5,R2),0(R1)       MOVE PLAYER ONTO SCREEN                      
         SR    R1,R1                                                            
         IC    R1,CARDH                                                         
         BCTR  R1,R0                                                            
         LA    R1,CARDS(R1)        R1 POINTS TO CARD                            
         SR    R2,R2                                                            
         IC    R2,SUITH                                                         
         BCTR  R2,R0                                                            
         MH    R2,=H'8'                                                         
         LA    R2,SUITF(R2)        R2 POINTS TO SUIT                            
         SR    R3,R3                                                            
         IC    R3,PLAYER                                                        
         MH    R3,=H'87'                                                        
         LA    R3,SEVCARD(R3)                                                   
         LR    R4,R3                                                            
         SH    R4,=H'8'                                                         
         FOUT  (R4)                                                             
         MVC   13(10,R3),SPACES                                                 
         MVC   13(1,R3),0(R1)      MOVE CARD AND SUIT                           
         MVC   15(8,R3),0(R2)      ONTO SCREEN                                  
         PACK  DUB(2),9(2,R3)                                                   
         SP    DUB(2),=P'1'                                                     
         UNPK  9(2,R3),DUB(2)                                                   
         OI    10(R3),X'F0'                                                     
         B     SEVXIT1                                                          
         EJECT                                                                  
*              ROUTINE TO SQUASH A HAND                                         
         SPACE 1                                                                
SEVSQ    NTR1                                                                   
         MVC   WORK(13),SPACES                                                  
         LA    R2,WORK+12                                                       
         LR    R3,R4                                                            
         SH    R3,=H'8'                                                         
         FOUT  (R3)                                                             
         LA    R6,22(R4)                                                        
         LA    R5,13                                                            
         SPACE 1                                                                
SEVSQ2   CLI   0(R6),C' '                                                       
         BE    SEVSQ4                                                           
         MVC   0(1,R2),0(R6)                                                    
         BCTR  R2,R0                                                            
         SPACE 1                                                                
SEVSQ4   BCTR  R6,R0                                                            
         BCT   R5,SEVSQ2                                                        
         MVC   10(13,R4),WORK                                                   
         B     SEVXIT1                                                          
         EJECT                                                                  
*              CHECK FOR A VALID TRY                                            
         SPACE 1                                                                
SEVCHK   NTR1                                                                   
         MVI   VALID,0                                                          
         MVC   BYTE,0(R4)                                                       
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         LA    RF,PUSED(RF)                                                     
         SH    RF,=H'16'                                                        
         CLI   0(RF),C'Y'          HAS THIS CARD ALREADY BEEN USED              
         BE    SEVCNO                                                           
         TM    BYTE,X'07'                                                       
         BO    SEVCYE                                                           
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'07'                                                       
         BH    SEVHGH                                                           
         SR    RF,RF               CARD LESS THAN 7 IN SUIT                     
         IC    RF,0(R4)                                                         
         LA    RF,PUSED(RF)                                                     
         SH    RF,=H'16'                                                        
         CLI   1(RF),C'Y'                                                       
         BNE   SEVCNO                                                           
         B     SEVXIT1                                                          
         SPACE 1                                                                
SEVHGH   SR    RF,RF               CARD HIGHER THAN 7 IN SUIT                   
         IC    RF,0(R4)                                                         
         LA    RF,PUSED(RF)                                                     
         SH    RF,=H'16'                                                        
         LR    R1,RF                                                            
         BCTR  R1,R0                                                            
         CLI   0(R1),C'Y'                                                       
         BNE   SEVCNO                                                           
         SPACE 1                                                                
SEVCYE   B     SEVXIT1                                                          
         SPACE 1                                                                
SEVCNO   MVI   VALID,1                                                          
         B     SEVXIT1                                                          
         EJECT                                                                  
*              CHECK IF USERS PASS WAS VALID                                    
         SPACE 1                                                                
SEVPASS  L     R4,AHANDA                                                        
         LA    R3,18                                                            
         MVI   MESSG,6             YOU CHEATED ON A PASS                        
         CLI   PUSED+7,C'Y'                                                     
         BE    SEVPAS2                                                          
         CLI   0(R4),X'17'                                                      
         BE    SEVPAS6                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,*-12                                                          
         B     SEVPAS5                                                          
         BE    SEVPAS6                                                          
         MVI   MESSG,0                                                          
         BAS   RE,SEVMESG                                                       
         B     SEVCOM                                                           
         SPACE 1                                                                
SEVPAS2  CLI   0(R4),0                                                          
         BE    SEVPAS4                                                          
         BAS   RE,SEVCHK                                                        
         CLI   VALID,0                                                          
         BE    SEVPAS6                                                          
         SPACE 1                                                                
SEVPAS4  LA    R4,1(R4)                                                         
         BCT   R3,SEVPAS2                                                       
         SPACE 1                                                                
SEVPAS5  MVI   MESSG,0                                                          
         BAS   RE,SEVMESG                                                       
         MVI   PLAYER,0                                                         
         BAS   RE,SEVPAME                                                       
         B     SEVCOM              NOW MY GO                                    
         SPACE 1                                                                
SEVPAS6  BAS   RE,SEVMESG                                                       
         B     SEVXIT                                                           
         EJECT                                                                  
*              ROUTINE TO FORMAT HAND ON SCREEN                                 
         SPACE 1                                                                
SEVFORM  NTR1                                                                   
         L     R3,AHANDA                                                        
         LA    R4,18                                                            
         LA    R7,SEVHAND                                                       
         LA    R8,4                                                             
         MVC   10(13,R7),SPACES                                                 
         LA    R7,87(R7)                                                        
         BCT   R8,*-10                                                          
         SPACE 1                                                                
SEVF2    SR    R5,R5                                                            
         LA    R7,SEVHAND                                                       
         MVC   BYTE,0(R3)                                                       
         CLI   BYTE,0                                                           
         BE    SEVF4                                                            
         IC    R5,BYTE                                                          
         SRL   R5,4                                                             
         BCTR  R5,R0                                                            
         MH    R5,=H'87'                                                        
         LA    R6,10(R7,R5)                                                     
         NI    BYTE,X'0F'                                                       
         SR    R5,R5                                                            
         IC    R5,BYTE                                                          
         BCTR  R5,R0                                                            
         AR    R6,R5                                                            
         LA    R5,CARDS(R5)                                                     
         MVC   0(1,R6),0(R5)                                                    
         SPACE 1                                                                
SEVF4    LA    R3,1(R3)                                                         
         BCT   R4,SEVF2                                                         
         LA    R4,SEVHAND                                                       
         LA    R5,4                                                             
         BAS   RE,SEVSQ                                                         
         LA    R4,87(R4)                                                        
         BCT   R5,*-8                                                           
         B     SEVXIT1                                                          
         EJECT                                                                  
*              SET OFF PLAYERS CARD                                             
         SPACE 1                                                                
SETOFF   NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,PLAYER                                                        
         MH    R1,=H'4'                                                         
         LA    R1,AHANDX(R1)                                                    
         L     R1,0(R1)                                                         
         LA    R2,18                                                            
         CLC   0(1,R1),CARD                                                     
         BE    SETOFF2                                                          
         LA    R1,1(R1)                                                         
         BCT   R2,*-14                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
SETOFF2  SR    R2,R2                                                            
         IC    R2,0(R1)                                                         
         LA    R2,PUSED(R2)                                                     
         SH    R2,=H'16'                                                        
         MVI   0(R2),C'Y'                                                       
         MVI   0(R1),0                                                          
         B     SEVXIT1                                                          
         EJECT                                                                  
*              COMPUTER PLAYING CONTROL                                         
         SPACE 1                                                                
SEVCOM   MVI   PLAYER,1                                                         
         BAS   RE,SEVMYGO                                                       
         MVI   PLAYER,2                                                         
         BAS   RE,SEVMYGO                                                       
         LA    R2,SEVINPTH                                                      
         B     SEVXIT                                                           
         SPACE 1                                                                
SEVMYGO  NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,PLAYER                                                        
         MH    R1,=H'4'                                                         
         LA    R1,AHANDX(R1)                                                    
         L     R1,0(R1)                                                         
         ST    R1,DMCB                                                          
         CLI   PUSED+7,C'Y'                                                     
         BE    SEVME2                                                           
         LA    R2,18                                                            
         CLI   0(R1),X'17'                                                      
         BE    SEVPLAY                                                          
         LA    R1,1(R1)                                                         
         BCT   R2,*-12                                                          
         B     SEVCPA                                                           
         SPACE 1                                                                
SEVME2   XC    PACK2(66),PACK2                                                  
         LA    R6,18                                                            
         SR    R2,R2                                                            
         SR    R7,R7                                                            
         SPACE 1                                                                
SEVCO2   CLI   0(R1),X'00'                                                      
         BE    SEVCO4                                                           
         IC    R2,0(R1)                                                         
         LA    R3,PACK2(R2)                                                     
         SH    R3,=H'16'                                                        
         STC   R2,0(R3)                                                         
         LR    R4,R1                                                            
         BAS   RE,SEVCHK                                                        
         CLI   VALID,0                                                          
         BNE   SEVCO4                                                           
         OI    0(R3),X'80'                                                      
         LA    R7,1(R7)                                                         
         SPACE 1                                                                
SEVCO4   LA    R1,1(R1)                                                         
         BCT   R6,SEVCO2                                                        
         LA    R1,PACK2                                                         
         LA    R2,4                                                             
         LTR   R7,R7                                                            
         BZ    SEVCPA                                                           
         ST    R1,DMCB+4                                                        
         EJECT                                                                  
*              DECISION CONTROL                                                 
         SPACE 1                                                                
PROHN1   CLC   1(15,R1),=16X'00'                                                
         BE    PROHN2A                                                          
         LA    R3,13                                                            
         LA    RF,1(R1)                                                         
         LR    R1,RF                                                            
         B     PROCD2                                                           
         SPACE 1                                                                
PROCD1   CH    R3,=H'7'                                                         
         BNE   PROCD2                                                           
         CLC   0(3,RF),=16X'00'                                                 
         BE    PROCDN                                                           
         CLC   4(2,RF),=16X'00'                                                 
         BNE   PROCDN                                                           
         B     SEVPLAY                                                          
         SPACE 1                                                                
PROCDN   CLC   10(3,RF),=16X'00'                                                
         BE    SEVKEEP                                                          
         CLC   7(2,RF),=16X'00'                                                 
         BE    SEVKEEP                                                          
         B     SEVPLAY                                                          
         SPACE 1                                                                
         EJECT                                                                  
*              KEEP AND REJECT                                                  
         SPACE 1                                                                
PROCD2   TM    0(R1),X'80'                                                      
         BNO   SEVKEEP                                                          
         CH    R3,=H'7'                                                         
         BE    PROCD1                                                           
         CH    R3,=H'1'                                                         
         BE    SEVPLAY                                                          
         CH    R3,=H'13'                                                        
         BE    SEVPLAY                                                          
         LA    R7,14                                                            
         SR    R7,R3                                                            
         CH    R7,=H'7'                                                         
         BH    PROCD3                                                           
         LA    RE,11                                                            
         SR    RE,R7                                                            
         B     PROCD4                                                           
PROCD3   SH    R7,=H'8'                                                         
PROCD4   LR    RE,R7                                                            
         MH    RE,=H'12'                                                        
         LA    RE,PROCD5(RE)                                                    
         BR    RE                                                               
PROCD5   EQU   *                                                                
         CLI   08(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   09(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   10(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   11(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   12(RF),0                                                         
         BE    SEVKEEP                                                          
         B     SEVPLAY                                                          
         CLI   04(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   03(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   02(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   01(RF),0                                                         
         BE    *+8                                                              
         B     SEVPLAY                                                          
         CLI   00(RF),0                                                         
         BE    SEVKEEP                                                          
         B     SEVPLAY                                                          
         EJECT                                                                  
*              AFTER DECISION                                                   
         SPACE 1                                                                
SEVKEEP  LA    R1,1(R1)                                                         
         BCT   R3,PROCD2                                                        
         SPACE 1                                                                
PROHN2A  LA    R3,13                                                            
         L     R1,DMCB+4                                                        
         LA    R1,16(R1)                                                        
         ST    R1,DMCB+4                                                        
         BCT   R2,PROHN1                                                        
         LA    R1,PACK2                                                         
         EJECT                                                                  
*                                                                               
*              ROUTINE TO MAKE THE BEST POSSIBLE CHOICE IF                      
*              A GOOD CARD CANNOT BE FOUND                                      
*                                                                               
         LA    R4,10                                                            
         LA    R5,12                                                            
         SPACE 1                                                                
SEVLOE   LA    R6,16                                                            
         LA    R7,4                                                             
         SPACE 1                                                                
SEVLOG   STC   R6,WORK                                                          
         STC   R5,WORK+1                                                        
         OC    WORK(1),WORK+1                                                   
         BAS   RE,SEVFND                                                        
         LR    R8,R5                                                            
         SR    R8,R4                                                            
         STC   R6,WORK                                                          
         STC   R8,WORK+1                                                        
         OC    WORK(1),WORK+1                                                   
         BAS   RE,SEVFND                                                        
         AH    R6,=H'16'                                                        
         BCT   R7,SEVLOG                                                        
         SH    R4,=H'2'                                                         
         BCTR  R5,R0                                                            
         B     SEVLOE                                                           
         SPACE 1                                                                
SEVFND   SR    R1,R1                                                            
         IC    R1,WORK                                                          
         SH    R1,=H'16'                                                        
         LA    R1,PACK2(R1)                                                     
         TM    0(R1),X'80'                                                      
         BO    SEVPLAY                                                          
         BR    RE                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*              ROUTINE TO PLAY COMPUTERS SELECTED CARD                          
         SPACE 1                                                                
SEVPLAY  NI    0(R1),X'7F'                                                      
         MVC   CARD,0(R1)                                                       
         SR    R1,R1                                                            
         IC    R1,CARD                                                          
         SRL   R1,4                                                             
         STC   R1,SUITH                                                         
         MVC   CARDH,CARD                                                       
         NI    CARDH,X'0F'                                                      
         LA    R4,CARD                                                          
         BAS   RE,SEVCHK                                                        
         CLI   VALID,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SETCARD                                                       
         BAS   RE,SETOFF                                                        
         L     R1,DMCB                                                          
         CLC   1(17,R1),0(R1)                                                   
         BE    SEVCWIN                                                          
         B     SEVXIT1                                                          
         SPACE 1                                                                
SEVCWIN  SR    R1,R1                                                            
         IC    R1,PLAYER                                                        
         MH    R1,=H'5'                                                         
         LA    R1,SAVPLIST(R1)                                                  
         MVC   MESS8(5),0(R1)                                                   
         MVI   MESSG,8                                                          
         BAS   RE,SEVMESG                                                       
         MVI   16(RA),X'FF'                                                     
         LM    RE,RC,12(RD)                                                     
         L     RD,4(RD)                                                         
         LA    R2,SEVINPTH                                                      
         B     SEVXIT                                                           
         SPACE 1                                                                
SEVCPA   BAS   RE,SEVPAME                                                       
         B     SEVXIT1                                                          
         SPACE 1                                                                
SEVPAME  NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,PLAYER                                                        
         MH    R1,=H'87'                                                        
         LA    R1,SEVCARD(R1)                                                   
         LR    R3,R1                                                            
         SH    R3,=H'8'                                                         
         FOUT  (R3)                                                             
         MVC   13(10,R1),=CL10'PASSED'                                          
         B     SEVXIT1                                                          
         EJECT                                                                  
*              USER WINS                                                        
         SPACE 1                                                                
SEVHWIN  MVI   MESSG,7                                                          
         MVI   16(RA),X'FF'                                                     
         BAS   RE,SEVMESG                                                       
         LA    R2,SEVINPTH                                                      
         B     SEVXIT                                                           
         SPACE 1                                                                
*              GENERAL EXIT                                                     
         SPACE 1                                                                
SEVXIT   OI    6(R2),OI1C                                                       
         BAS   RE,SEVFORM                                                       
SEVXITX  XMOD1 1                                                                
         EJECT                                                                  
*              ROOT INTERNAL STORAGE REQUIRMENTS                                
         SPACE 1                                                                
CARDS    DC    C'A23456789TJQK'                                                 
SUITS    DC    C'DHCS'                                                          
SUITF    DS    0CL8                                                             
         DC    CL8'DIAMONDS'                                                    
         DC    CL8'HEARTS'                                                      
         DC    CL8'CLUBS'                                                       
         DC    CL8'SPADES'                                                      
PACK     DS    0CL2                                                             
         DC    X'003600380034002700450049002C001A004D0021001300420029'          
         DC    X'003A004B002D004C001800150043002300410011003700160024'          
         DC    X'00320022003100140044002600460019001D002B002A003D0039'          
         DC    X'003300120025003500170028001B003B004A001C003C00470048'          
         DC    XL14'00'                                                         
SEVELIST DS    0CL60                                                            
         DC    CL60'YOUR TURN TO PLAY A CARD'                                   
         DC    CL60'INVALID CARD INPUT - TRY AGAIN'                             
         DC    CL60'INVALID SUIT INPUT - TRY AGAIN'                             
         DC    CL60'THIS CARD IS NOT IN YOUR HAND'                              
         DC    CL60'CHEATING IS NOT ALLOWED PLEASE TRY AGAIN'                   
         DC    CL60'YOU LAST TRY WAS INVALID TRY AGAIN'                         
         DC    CL60'YOU PASSED ON A VALID TRY - PLEASE REINPUT'                 
         DC    CL60'WELL DONE YOU WON THE GAME'                                 
MESS8    DC    CL60'XXXXX WON THE GAME THANK YOU FOR PLAYING'                   
         DC    CL60'ENTER THE 7 OF DIAMONDS (7D) OR PASS (P)'                   
SPACES   DC    CL20' '                                                          
         LTORG                                                                  
         EJECT                                                                  
*              GENERAL WORKING STORAGE DSECT                                    
         SPACE 1                                                                
SEVWRK   DSECT                                                                  
BYTE     DS    C                                                                
WORD     DS    F                                                                
MESSG    DS    C                                                                
CARD     DS    C                                                                
SUIT     DS    C                                                                
CARDH    DS    C                                                                
SUITH    DS    C                                                                
DUB      DS    D                                                                
WORK     DS    CL18                                                             
PLAYER   DS    C                                                                
VALID    DS    C                                                                
DMCB     DS    6F                                                               
PACK2    DS    CL118                                                            
AHANDS   DS    0F                                                               
AHAND1   DS    A                                                                
AHAND2   DS    A                                                                
AHAND3   DS    A                                                                
AHANDX   DS    0F                                                               
AHANDA   DS    A                                                                
AHANDC   DS    A                                                                
AHANDB   DS    A                                                                
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
         SPACE 1                                                                
SEVFRST  DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE GASEVFFD                                                       
         ORG   SEVFRST                                                          
         DS    1700C                                                            
         EJECT                                                                  
*              TWA SAVED STORAGE                                                
         SPACE 1                                                                
SAVPLIST DS    0CL5                                                             
         DS    CL15                                                             
HANDS    DS    CL54                THREE 18 CARD HANDS                          
PUSED    DS    CL64                CARDS USED                                   
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GASEV00   05/01/02'                                      
         END                                                                    
