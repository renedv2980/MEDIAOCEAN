*          DATA SET GAPOK00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0800A                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'FIVE CARD DRAW POKER GAME'                                      
         PRINT NOGEN                                                            
DRAW     CSECT                                                                  
         NMOD1 0,GAPOK00,RR=R8                                                  
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING DRAW+4096,R9                                                     
         L     RA,4(R1)            A(TWA)                                       
         USING GAPOKFFD,RA                                                      
         LA    R2,POKACTH                                                       
         MVC   PHASE,16(RA)                                                     
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(79),SPACES                                              
         TBIN  MILLI                                                            
         ST    R1,RANDOM                                                        
         EJECT                                                                  
*              LOGIC (PHASE 0) FIRST HAND                                       
         SPACE 3                                                                
         CLI   PHASE,0                                                          
         BNE   DR10                                                             
         CLI   17(RA),0                                                         
         BNE   DR1                                                              
         MVI   17(RA),1                                                         
         XC    BALANCE,BALANCE                                                  
         MVC   DECK,RANDECK                                                     
         SPACE 2                                                                
DR1      DS    0H                                                               
         BAS   RE,GETSTAKE                                                      
         LTR   R1,R1                                                            
         BZ    XMOD1                                                            
         ST    R1,ANTE                                                          
         ST    R1,STAKE                                                         
         MH    R1,=H'5'                                                         
         ST    R1,MAX                                                           
         EDIT  (4,ANTE),(8,M6AD+26),ALIGN=LEFT                                  
         GOTO1 MESSOUT,PARA,M6A,POKANTEH                                        
         FOUT  POKMAXH,SPACES,60                                                
         MVC   POKMAX(30),=C'MAXIMUM RAISE FOR THIS GAME IS'                    
         LA    R4,POKMAX+31                                                     
         EDIT  (4,MAX),(9,0(R4)),ALIGN=LEFT                                     
         SPACE 2                                                                
DR2      MVI   PHASE,1                                                          
         BAS   RE,DEAL                                                          
         GOTO1 ASSESS,PARA,MYHAND,MYVALUE                                       
         BAS   RE,SETLIMS                                                       
         MVI   CHECKSW,C'Y'                                                     
         BAS   RE,YOUROUT                                                       
         B     PLAY1                                                            
         EJECT                                                                  
*              LOGIC (PHASE 1) - BETTING BEFORE CHANGE                          
         SPACE 3                                                                
DR10     CLI   PHASE,1                                                          
         BNE   DR20                                                             
         CLI   CHECKSW,C'Y'        FIRST TIME CHECK DOES NOT END                
         BNE   DR12                THE ACTION                                   
         MVI   CHECKSW,C'N'                                                     
         CLI   POKACT,C'C'                                                      
         BE    DR14                                                             
         SPACE 2                                                                
DR12     CLI   POKACT,C'F'         YOU FOLDED                                   
         BE    RESULT1                                                          
         L     R0,STAKE                                                         
         A     R0,RAISE                                                         
         ST    R0,STAKE                                                         
         XC    RAISE,RAISE                                                      
         CLI   POKACT,C'C'                                                      
         BE    DR16                                                             
         CLI   POKACT,C'B'                                                      
         BE    DR13                                                             
         CLI   POKACT,C'R'         YOU RAISED                                   
         BNE   XMOD1                                                            
         SPACE 2                                                                
DR13     BAS   RE,GETSTAKE                                                      
         LTR   R1,R1                                                            
         BZ    XMOD1                                                            
         L     R3,STAKE                                                         
         C     R1,MAX                                                           
         BH    TOOHIGH                                                          
         AR    R3,R1                                                            
         C     R3,MYHIGH                                                        
         BH    RESULT2             TOO HIGH FOR ME                              
         ST    R3,STAKE                                                         
         SPACE 2                                                                
DR14     CLC   STAKE,MYLOW         IS IT HIGH ENOUGH                            
         BL    PLAY3                                                            
         MVC   M1BD(11),=C'I CHECK    '                                         
         CLI   POKACT,C'C'                                                      
         BE    DR18                                                             
         MVC   M1BD(7),=C'I CALL '                                              
         B     DR18                                                             
         SPACE 2                                                                
DR16     MVC   M1BD(11),=C'YOU CALLED '                                         
         SPACE 2                                                                
DR18     MVI   PHASE,2                                                          
         B     PLAY2                                                            
         EJECT                                                                  
*              LOGIC (PHASE 2) CHANGE CARDS                                     
         SPACE 3                                                                
DR20     CLI   PHASE,2                                                          
         BNE   DR30                                                             
         BAS   RE,VALSWAP                                                       
         CLI   HISNUM,6                                                         
         BE    XMOD1                                                            
         GOTO1 =V(XSORT),PARA,(1,YOURHAND),5,2,2,0,RR=R8                        
         BAS   RE,YOUROUT                                                       
         BAS   RE,MYSWAP                                                        
         GOTO1 =V(XSORT),PARA,(1,MYHAND),5,2,2,0,RR=R8                          
         GOTO1 ASSESS,PARA,MYHAND,MYVALUE                                       
         BAS   RE,SETLIMS                                                       
         MVI   CHECKSW,C'Y'                                                     
         MVI   PHASE,3                                                          
         B     PLAY4                                                            
         EJECT                                                                  
*              LOGIC (PHASE 3) HANDLE FINAL ACTION                              
         SPACE 3                                                                
DR30     CLI   CHECKSW,C'Y'                                                     
         BNE   DR32                                                             
         MVI   CHECKSW,C'N'                                                     
         CLI   POKACT,C'C'                                                      
         BNE   DR32                                                             
         B     DR34                                                             
         SPACE 2                                                                
DR32     CLI   POKACT,C'F'                                                      
         BE    RESULT1                                                          
         L     R0,STAKE                                                         
         A     R0,RAISE                                                         
         ST    R0,STAKE                                                         
         XC    RAISE,RAISE                                                      
         CLI   POKACT,C'R'                                                      
         BE    DR33                                                             
         CLI   POKACT,C'B'                                                      
         BE    DR33                                                             
         CLI   POKACT,C'C'                                                      
         BE    DR35                                                             
         B     XMOD1                                                            
         SPACE 2                                                                
DR33     DS    0H                                                               
         BAS   RE,GETSTAKE                                                      
         LTR   R1,R1                                                            
         BZ    XMOD1                                                            
         L     R3,STAKE                                                         
         C     R1,MAX                                                           
         BH    TOOHIGH                                                          
         AR    R3,R1                                                            
         C     R3,MYHIGH                                                        
         BNH   DR33A                                                            
         C     R1,ANTE                                                          
         BH    RESULT2                                                          
         B     DR35                                                             
         SPACE 2                                                                
DR33A    DS    0H                                                               
         ST    R3,STAKE                                                         
         SPACE 2                                                                
DR34     CLC   STAKE,MYLOW                                                      
         BL    PLAY5                                                            
         SPACE 2                                                                
DR35     DS    0H                                                               
         GOTO1 ASSESS,PARA,YOURHAND,URVALUE                                     
         CLC   URVALUE,MYVALUE                                                  
         BL    DR36                                                             
         CLI   POKACT,C'S'                                                      
         BE    RESULT3                                                          
         B     RESULT6                                                          
         SPACE 2                                                                
DR36     CLI   POKACT,C'S'                                                      
         BE    RESULT4                                                          
         B     RESULT5                                                          
         EJECT                                                                  
*              ROUTINE TO GET A CARD AND A RANDOM NUMBER                        
         SPACE 3                                                                
GETRANDM NTR1                                                                   
         L     R2,RANDOM                                                        
         SLL   R2,2                                                             
         SRL   R2,2                                                             
         LR    R3,R2                                                            
         MR    R2,R2                                                            
         SLL   R3,2                                                             
         SRL   R3,2                                                             
         AR    R2,R3                                                            
         ST    R2,RANDOM                                                        
         B     XIT                                                              
         SPACE 2                                                                
GETACARD NTR1                                                                   
         SPACE 2                                                                
GETCARD2 BAS   RE,GETRANDM                                                      
         L     R2,RANDOM                                                        
         SRDA  R2,32                                                            
         D     R2,=F'52'                                                        
         SLL   R2,1                                                             
         SPACE 2                                                                
GETCARD3 LA    R2,DECK(R2)                                                      
         MVC   CARD,0(R2)                                                       
         OC    CARD,CARD                                                        
         BNZ   GETCARD4                                                         
         OC    RANDOM,RANDOM       GUARD AGAINST ZERO TIME                      
         BNZ   GETCARD2                                                         
         LA    R2,2(R2)                                                         
         B     GETCARD3                                                         
         SPACE 2                                                                
GETCARD4 XC    0(2,R2),0(R2)       SET THE CARD OUT OF THE DECK                 
         B     XIT                                                              
         SPACE 2                                                                
RANDECK  DC    X'01010403030201040A020701070203030B030602030408010D01'          
         DC    X'0A03050406010B010C04080301020A0403010704050209040703'          
         DC    X'040205030D0408040902020104040C030B040204060401030C01'          
         DC    X'0B020501090302020D030C020A010802060309010D0204010203'          
         EJECT                                                                  
*              ROUTINES TO ASSESS A HAND                                        
         SPACE 3                                                                
ASSESS   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   NUM+5(2),=X'FEFF'                                                
         LR    R4,R2                                                            
         LA    R5,NUM                                                           
         LA    R6,LET                                                           
         LA    R7,5                                                             
         SPACE 2                                                                
AH2      MVC   0(1,R5),0(R4)       FIRST SPLIT                                  
         MVC   0(1,R6),1(R4)                                                    
         LA    R4,2(R4)                                                         
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R7,AH2                                                           
*                                  NOW SORT THEM                                
         GOTO1 =V(XSORT),PARA,(1,NUM),5,1,1,0,RR=R8                             
         BASR  RE,RF                                                            
         LA    R4,NUM                                                           
         BAS   RE,CHECKORD                                                      
         GOTO1 (RF),(R1),(1,LET)                                                
         BASR  RE,RF                                                            
         LA    R4,LET                                                           
         BAS   RE,CHECKORD                                                      
         MVI   0(R3),0                                                          
         MVC   1(3,R3),NUM                                                      
         LA    R4,NUM              CHECK FOR A PAIR                             
         LA    R5,4                                                             
         SPACE 2                                                                
AH3      CLC   0(1,R4),1(R4)                                                    
         BE    AH4                                                              
         LA    R4,1(R4)                                                         
         BCT   R5,AH3                                                           
         B     AH14                                                             
         SPACE 2                                                                
AH4      MVI   0(R3),1             FOUND ONE                                    
         MVC   1(1,R3),0(R4)                                                    
         CLC   2(1,R4),0(R4)       CHECK FOR THREE                              
         BE    AH10                                                             
         LA    R4,2(R4)            CHECK FOR TWO PAIRS                          
         BCT   R5,*+8                                                           
         B     XIT                                                              
         BCT   R5,*+8                                                           
         B     XIT                                                              
         SPACE 2                                                                
AH6      CLC   0(1,R4),1(R4)                                                    
         BE    AH8                                                              
         LA    R4,1(R4)                                                         
         BCT   R5,AH6                                                           
         B     XIT                                                              
         SPACE 2                                                                
AH8      MVI   0(R3),2             FOUND A SECOND                               
         MVC   2(1,R3),0(R4)                                                    
         B     XIT                                                              
         SPACE 2                                                                
AH10     MVI   0(R3),3             FOUND A THREE                                
         CLC   0(1,R4),3(R4)       CHECK FOR FOUR                               
         BE    AH12                                                             
         CLC   3(1,R4),4(R4)       CHECK FOR FULL HOUSE                         
         BNE   XIT                                                              
         MVI   0(R3),6             FOUND ONE                                    
         MVC   2(1,R3),0(R4)                                                    
         B     XIT                                                              
         SPACE 2                                                                
AH12     MVI   0(R3),7             FOUND A FOUR                                 
         B     XIT                                                              
         SPACE 2                                                                
AH14     SR    R4,R4               CHECK FOR A STRAIGHT (5)                     
         SR    R5,R5                                                            
         IC    R4,NUM                                                           
         IC    R5,NUM+4                                                         
         SR    R4,R5                                                            
         CH    R4,=H'4'                                                         
         BNE   AH16                                                             
         SPACE 2                                                                
AH15     MVI   0(R3),4                                                          
         MVC   1(1,R3),NUM                                                      
         MVI   3(R3),0                                                          
         B     AH20                                                             
         SPACE 2                                                                
AH16     CLC   NUM(5),=X'0D04030201'    CHECK FOR A2345                         
         BE    AH15                                                             
         CLI   PHASE,1                                                          
         BNE   AH20                                                             
         IC    R4,NUM              CHECK FOR A STRAIGHT (4)                     
         IC    R5,NUM+3                                                         
         SR    R4,R5                                                            
         CH    R4,=H'3'                                                         
         BNE   AH18                                                             
         MVI   0(R3),4                                                          
         MVC   1(1,R3),NUM                                                      
         MVC   3(1,R3),NUM+4       SAVE MISSING CARD                            
         B     AH20                                                             
         SPACE 2                                                                
AH18     IC    R4,NUM+1                                                         
         IC    R4,NUM+4                                                         
         SR    R4,R5                                                            
         CH    R4,=H'3'                                                         
         BNE   AH20                                                             
         MVI   0(R3),4                                                          
         MVC   1(1,R3),NUM+1                                                    
         MVC   3(1,R3),NUM                                                      
         SPACE 2                                                                
AH20     LA    R4,LET+1            CHECK FOR FIVE CARD FLUSH                    
         LA    R5,4                                                             
         SPACE 2                                                                
AH22     CLC   0(1,R4),LET                                                      
         BNE   AH26                                                             
         LA    R4,1(R4)                                                         
         BCT   R5,AH22                                                          
         CLI   0(R3),4             IS IT A STRAIGHT AS WELL                     
         BNE   AH24                                                             
         CLI   3(R3),0                                                          
         BNE   AH24                                                             
         MVI   0(R3),8             THEN WE'VE HIT GOLD (STRAIGHT FLUSH)         
         B     XIT                                                              
         SPACE 2                                                                
AH24     MVI   0(R3),5                                                          
         MVC   1(1,R3),LET                                                      
         MVI   3(R3),0                                                          
         B     XIT                                                              
         SPACE 2                                                                
AH26     CLI   PHASE,1                                                          
         BNE   XIT                                                              
         LA    R4,LET+1            CHECK FOR FOUR CARD FLUSH                    
         LA    R5,3                                                             
         SPACE 2                                                                
AH28     CLC   0(1,R4),LET                                                      
         BNE   AH30                                                             
         LA    R4,1(R4)                                                         
         BCT   R5,AH28                                                          
         MVI   0(R3),5                                                          
         MVC   1(1,R3),LET                                                      
         MVC   3(1,R3),LET+4                                                    
         B     XIT                                                              
         SPACE 2                                                                
AH30     LA    R4,LET+1                                                         
         LA    R5,3                                                             
         SPACE 2                                                                
AH32     CLC   0(1,R4),LET+4                                                    
         BNE   XIT                                                              
         LA    R4,1(R4)                                                         
         BCT   R5,AH32                                                          
         MVI   0(R3),5                                                          
         MVC   1(1,R3),LET+4                                                    
         MVC   3(1,R3),LET                                                      
         B     XIT                                                              
         SPACE 2                                                                
CHECKORD LA    R5,4                CHECK XSORT IS OK                            
         SPACE 2                                                                
CO2      CLC   0(1,R4),1(R4)                                                    
         BL    CO4                                                              
         LA    R4,1(R4)                                                         
         BCT   R5,CO2                                                           
         BR    RE                                                               
         SPACE 2                                                                
CO4      DC    H'0'                                                             
         DC    C'XSORT SUCKS'                                                   
         EJECT                                                                  
*              ROUTINE TO CHANGE MY CARDS                                       
         SPACE 3                                                                
MYSWAP   NTR1                                                                   
         LA    R2,MYHAND                                                        
         LA    R3,MYVALUE                                                       
         MVI   MYNUM,0                                                          
         CLI   0(R3),5                                                          
         BH    XIT                                                              
         BE    MY26                                                             
         CLI   0(R3),3                                                          
         BH    MY20                                                             
         BE    MY14                                                             
         CLI   0(R3),1                                                          
         BH    MY8                                                              
         BE    MY2                                                              
         BL    MY32                                                             
         SPACE 2                                                                
MY2      LA    R4,3                USUALLY DRAW 3 TO A PAIR                     
         L     R5,RANDOM                                                        
         SLL   R5,30                                                            
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         LA    R4,2                SOMETIMES TAKE ONLY 2 FOR BLUFF              
         STC   R4,MYNUM                                                         
         SPACE 2                                                                
MY4      CLC   0(1,R2),1(R3)                                                    
         BE    MY6                                                              
         BAS   RE,GETACARD                                                      
         MVC   0(2,R2),CARD                                                     
         BCT   R4,MY6                                                           
         B     XIT                                                              
         SPACE 2                                                                
MY6      LA    R2,2(R2)                                                         
         B     MY4                                                              
         SPACE 2                                                                
MY8      MVI   MYNUM,1             DRAW 1 TO 2 PAIRS                            
         SPACE 2                                                                
MY10     CLC   0(1,R2),1(R3)                                                    
         BE    MY12                                                             
         CLC   0(1,R2),2(R3)                                                    
         BE    MY12                                                             
         BAS   RE,GETACARD                                                      
         MVC   0(2,R2),CARD                                                     
         B     XIT                                                              
         SPACE 2                                                                
MY12     LA    R2,2(R2)                                                         
         B     MY10                                                             
         SPACE 2                                                                
MY14     MVI   MYNUM,2             DRAW 2 TO 3 OF A KIND                        
         LA    R5,5                                                             
         SPACE 2                                                                
MY16     CLC   0(1,R2),1(R3)                                                    
         BE    MY18                                                             
         BAS   RE,GETACARD                                                      
         MVC   0(2,R2),CARD                                                     
         SPACE 2                                                                
MY18     LA    R2,2(R2)                                                         
         BCT   R5,MY16                                                          
         SPACE 2                                                                
MY20     CLI   3(R3),0             CHECK FOR COMPLETED STRAIGHT                 
         BE    XIT                                                              
         MVI   MYNUM,1             DRAW 1 FOR 4 STRAIGHT                        
         SPACE 2                                                                
MY22     CLC   0(1,R2),3(R3)                                                    
         BNE   MY24                                                             
         BAS   RE,GETACARD                                                      
         MVC   0(2,R2),CARD                                                     
         B     XIT                                                              
         SPACE 2                                                                
MY24     LA    R2,2(R2)                                                         
         B     MY22                                                             
         SPACE 2                                                                
MY26     CLI   3(R3),0             CHECK FOR COMPLETED FLUSH                    
         BE    XIT                                                              
         MVI   MYNUM,1             DRAW 1 FOR UNMATCHING SUIT                   
         SPACE 2                                                                
MY28     CLC   1(1,R2),3(R3)                                                    
         BNE   MY30                                                             
         BAS   RE,GETACARD                                                      
         MVC   0(2,R2),CARD                                                     
         B     XIT                                                              
         SPACE 2                                                                
MY30     LA    R2,2(R2)                                                         
         B     MY28                                                             
         SPACE 2                                                                
MY32     MVI   MYNUM,5             OTHERWISE TAKE 5                             
         LA    R5,5                                                             
         SPACE 2                                                                
MY34     BAS   RE,GETACARD                                                      
         MVC   0(2,R2),CARD                                                     
         LA    R2,2(R2)                                                         
         BCT   R5,MY34                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ESTABLISH LIMITS                                      
         SPACE 3                                                                
SETLIMS  NTR1                                                                   
         SR    R2,R2                                                            
         IC    R2,MYVALUE                                                       
         CLI   MYVALUE,4           CHECK FOR 4-CARD STRAIGHT/FLUSH              
         BL    GL2                       IN PHASE 1                             
         CLI   MYVALUE,5                                                        
         BH    GL2                                                              
         CLI   MYVALUE+3,0                                                      
         BE    GL2                                                              
         SR    R2,R2               USELESS IN PHASE 3                           
         CLI   PHASE,3                                                          
         BE    GL2                                                              
         LA    R2,2                BUT NOT BAD IN PHASE 1                       
         SPACE 2                                                                
GL2      SLL   R2,2                                                             
         LA    R2,LIMITBLE(R2)                                                  
         L     R3,ANTE                                                          
         LH    R5,0(R2)            LOW                                          
         MR    R4,R3                                                            
         LH    R7,2(R2)            HIGH                                         
         MR    R6,R3                                                            
         BAS   RE,GETRANDM                                                      
         L     R2,RANDOM                                                        
         SRDA  R2,32                                                            
         D     R2,=F'10'                                                        
         CLI   PHASE,1                                                          
         BE    GL3                                                              
         L     R2,BLUFF                                                         
         SPACE 2                                                                
GL3      ST    R2,BLUFF                                                         
         SLL   R2,2                                                             
         LA    R2,FACTABLE(R2)                                                  
         M     R4,0(R2)                                                         
         M     R6,0(R2)                                                         
         D     R4,=F'2000'                                                      
         D     R6,=F'1000'                                                      
         CLI   PHASE,1                                                          
         BE    GL4                                                              
         SLDA  R4,1                ON PHASE 3, EVERYTHING GETS HIGHER           
         SLDA  R6,1                                                             
         SR    R2,R2               AND WE'RE INFLUENCED BY HOW MANY             
         IC    R2,HISNUM           CARDS HE DREW                                
         SLL   R2,1                                                             
         LA    R2,SWAPTBLE(R2)                                                  
         M     R4,=F'100'                                                       
         D     R4,0(R2)                                                         
         M     R6,=F'100'                                                       
         D     R6,0(R2)                                                         
         SPACE 2                                                                
GL4      ST    R5,MYLOW                                                         
         ST    R7,MYHIGH                                                        
         SRL   R5,1                                                             
         C     R5,MAX                                                           
         BL    *+8                                                              
         L     R5,MAX                                                           
         ST    R5,MYGAP                                                         
         B     XIT                                                              
         EJECT                                                                  
*              TABLE OF FACTORS FOR ESTABLISHING LIMITS                         
         SPACE 2                                                                
LIMITBLE DS    0H                                                               
         DC    H'10'               BAG OF NAILS                                 
         DC    H'50'                                                            
         DC    H'10'               ONE PAIR                                     
         DC    H'50'                                                            
         DC    H'20'               TWO PAIRS                                    
         DC    H'50'                                                            
         DC    H'30'               THREES                                       
         DC    H'60'                                                            
         DC    H'35'               STRAIGHT                                     
         DC    H'70'                                                            
         DC    H'40'               FLUSH                                        
         DC    H'150'                                                           
         DC    H'80'               FULL HOUSE                                   
         DC    H'250'                                                           
         DC    H'200'              FOURS                                        
         DC    H'10000'                                                         
         DC    H'300'              STRAIGHT FLUSH                               
         DC    H'25000'                                                         
         SPACE 2                                                                
SWAPTBLE DC    F'200'                                                           
         DC    F'100'                                                           
         DC    F'150'                                                           
         DC    F'100'                                                           
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 2                                                                
FACTABLE DC    F'100'                                                           
         DC    F'150'                                                           
         DC    F'200'                                                           
         DC    F'250'                                                           
         DC    F'300'                                                           
         DC    F'350'                                                           
         DC    F'400'                                                           
         DC    F'450'                                                           
         DC    F'500'                                                           
         DC    F'550'                                                           
         EJECT                                                                  
*              ROUTINE TO OUTPUT HANDS                                          
         SPACE 2                                                                
YOUROUT  NTR1                                                                   
         LA    R2,YOURHAND                                                      
         LA    R3,POKCRD1H                                                      
         LA    R4,5                                                             
         MVC   WORK,SPACES                                                      
         SPACE 2                                                                
YOUR2    SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         BCTR  R5,0                                                             
         SLL   R5,1                                                             
         LA    R5,SPOTS(R5)                                                     
         MVC   WORK(2),0(R5)                                                    
         SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'8'                                                         
         LA    R5,SUITS(R5)                                                     
         MVC   WORK+3(8),0(R5)                                                  
         FOUT  (R3),WORK,11                                                     
         LA    R2,2(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,0(R3)            BUMP TWO FIELDS                              
         AR    R3,R5                                                            
         IC    R5,0(R3)                                                         
         AR    R3,R5                                                            
         BCT   R4,YOUR2                                                         
         B     XIT                                                              
         SPACE 2                                                                
MYOUT    NTR1                                                                   
         LA    R2,MYHAND                                                        
         LA    R4,5                                                             
         SPACE 2                                                                
MYOUT2   SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         BCTR  R5,0                                                             
         SLL   R5,1                                                             
         LA    R5,SPOTS(R5)                                                     
         MVC   0(1,R3),1(R5)                                                    
         CLI   0(R5),C' '                                                       
         BE    MYOUT4                                                           
         MVC   0(2,R3),0(R5)                                                    
         LA    R3,1(R3)                                                         
         SPACE 2                                                                
MYOUT4   SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'8'                                                         
         LA    R5,SUITS(R5)                                                     
         MVC   1(1,R3),0(R5)                                                    
         MVI   2(R3),C' '                                                       
         LA    R2,2(R2)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,MYOUT2                                                        
         B     XIT                                                              
         SPACE 2                                                                
SPOTS    DC    C' 2 3 4 5 6 7 8 910 J Q K A'                                    
SUITS    DC    C'SPADES  HEARTS  DIAMONDSCLUBS   '                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT VARIABLE MESSAGES                              
         SPACE 3                                                                
MESSOUT  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         FOUT  (R3),SPACES,60                                                   
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     XIT                                                              
         MVC   8(0,3),1(R2)                                                     
         EJECT                                                                  
*              VALIDATE CARDS TO BE SWAPPED                                     
         SPACE 3                                                                
VALSWAP  NTR1                                                                   
         LA    R2,POKACTH                                                       
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         MVC   HISNUM,5(R2)                                                     
         LTR   R1,R3                                                            
         BZ    XIT12                                                            
         LA    R4,8(R2)                                                         
         SPACE 2                                                                
SWAP2    CLI   0(R4),X'F1'         CHECK FOR CARD NO 1-5                        
         BL    SWAPERR                                                          
         CLI   0(R4),X'F5'                                                      
         BH    SWAPERR                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,SWAP2                                                         
         SPACE 2                                                                
         LR    R3,R1                                                            
         LA    R4,8(R2)                                                         
SWAP4    IC    R5,0(R4)            NOW SWAP THE CARDS                           
         SLL   R5,28                                                            
         SRL   R5,28                                                            
         BCTR  R5,0                                                             
         SLL   R5,1                                                             
         LA    R5,YOURHAND(R5)                                                  
         BAS   RE,GETACARD                                                      
         MVC   0(2,R5),CARD                                                     
         LA    R4,1(R4)                                                         
         BCT   R3,SWAP4                                                         
         B     XIT12                                                            
         SPACE 2                                                                
SWAPERR  MVI   HISNUM,6                                                         
         B     XIT12                                                            
         EJECT                                                                  
*              ROUTINE TO DEAL THE CARDS                                        
         SPACE 3                                                                
DEAL     NTR1                                                                   
         LA    R2,YOURHAND                                                      
         LA    R3,MYHAND                                                        
         LA    R4,5                                                             
         SPACE 2                                                                
DEAL2    BAS   RE,GETACARD                                                      
         MVC   0(2,R2),CARD                                                     
         BAS   RE,GETACARD                                                      
         MVC   0(2,R3),CARD                                                     
         LA    R2,2(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,DEAL2                                                         
         GOTO1 =V(XSORT),PARA,(1,MYHAND),5,2,2,0,RR=R8                          
         GOTO1 =V(XSORT),PARA,(1,YOURHAND),5,2,2,0,RR=R8                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET STAKE OUT                                         
         SPACE 3                                                                
GETSTAKE NTR1                                                                   
         LA    R2,POKAMTH                                                       
         SR    R1,R1                                                            
         CLI   5(R2),0                                                          
         BE    XIT12                                                            
         TM    4(R2),X'08'                                                      
         BZ    XIT12                                                            
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LA    R2,POKACTH                                                       
         SPACE 2                                                                
XIT12    XIT1  REGS=(R1,R2)                                                     
         EJECT                                                                  
*              ROUTINES AT END OF GAME                                          
         SPACE 3                                                                
RESULT1  LA    R3,M3A                                                           
         BAS   RE,CHAT                                                          
         L     R0,STAKE                                                         
         S     R0,RAISE                                                         
         ST    R0,STAKE                                                         
         B     LOST                                                             
         SPACE 2                                                                
RESULT2  LA    R3,M3B                                                           
         BAS   RE,CHAT                                                          
         B     WON                                                              
         SPACE 2                                                                
RESULT3  LA    R3,M3CD+33                                                       
         BAS   RE,CHAT2                                                         
         BAS   RE,MYOUT                                                         
         LA    R3,M3C                                                           
         B     WON                                                              
         SPACE 2                                                                
RESULT4  LA    R3,M3DD+33                                                       
         BAS   RE,CHAT2                                                         
         BAS   RE,MYOUT                                                         
         LA    R3,M3D                                                           
         B     LOST                                                             
         SPACE 2                                                                
RESULT5  LA    R3,M3ED+33                                                       
         BAS   RE,CHAT2                                                         
         BAS   RE,MYOUT                                                         
         LA    R3,M3E                                                           
         B     LOST                                                             
         SPACE 2                                                                
RESULT6  LA    R3,M3FD+33                                                       
         BAS   RE,MYOUT                                                         
         BAS   RE,CHAT2                                                         
         LA    R3,M3F                                                           
         SPACE 2                                                                
WON      LA    R4,M4B                                                           
         EDIT  (4,STAKE),(10,M4BD+8),ALIGN=LEFT                                 
         L     R0,BALANCE                                                       
         A     R0,STAKE                                                         
         ST    R0,BALANCE                                                       
         B     ALL                                                              
         SPACE 2                                                                
LOST     EDIT  (4,STAKE),(10,M4AD+9),ALIGN=LEFT                                 
         LA    R4,M4A                                                           
         L     R0,BALANCE                                                       
         S     R0,STAKE                                                         
         ST    R0,BALANCE                                                       
         SPACE 2                                                                
ALL      GOTO1 MESSOUT,PARA,(R3),POKRESH                                        
         GOTO1 (RF),(R1),(R4),POKWLH                                            
         MVC   M5AD+12(21),SPACES                                               
         MVC   M5AD+12(4),=C'UP  '                                              
         EDIT  (4,BALANCE),(10,M5AD+15),ALIGN=LEFT                              
         L     R0,BALANCE                                                       
         LTR   R0,R0                                                            
         BP    ALL2                                                             
         BZ    ALLEVEN                                                          
         MVC   M5AD+12(5),=C'DOWN '                                             
         EDIT  (4,BALANCE),(10,M5AD+17),ALIGN=LEFT                              
         B     ALL2                                                             
         SPACE 2                                                                
ALLEVEN  MVC   M5AD+12(09),=C'EVEN     '                                        
         SPACE 2                                                                
ALL2     GOTO1 MESSOUT,PARA,M5A,POKBALH                                         
         MVC   STAKE,ANTE                                                       
         MVC   DECK,RANDECK                                                     
         MVI   PHASE,0                                                          
         FOUT  POKMSS1H,SPACES,60                                               
         FOUT  POKMSS2H,SPACES,60                                               
         MVC   POKMSS1(33),=C'END OF THIS GAME. RESULT IS BELOW '               
         MVC   POKMSS2(24),=C'INPUT ANTE FOR NEXT GAME'                         
         FOUT  POKACTH,=C'ANTE',4                                               
         LA    R2,POKAMTH                                                       
         B     PLAY12                                                           
         EJECT                                                                  
*              PUT OUT AN EXPLANATION AT END OF GAME                            
         SPACE 3                                                                
CHAT     FOUT  POKCHTH,SPACES,60                                                
         BR    RE                                                               
         SPACE 2                                                                
CHAT2    NTR1                                                                   
         SR    R2,R2                                                            
         MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
         CLC   MYVALUE,URVALUE                                                  
         BL    CHAT4                                                            
         MVC   WORK(2),=C'MY'                                                   
         LA    R3,3(R3)                                                         
         IC    R2,MYVALUE                                                       
         BAS   RE,CHAT6                                                         
         MVC   0(09,R3),=C'BEAT YOUR'                                           
         LA    R3,10(R3)                                                        
         IC    R2,URVALUE                                                       
         BAS   RE,CHAT6                                                         
         B     CHAT8                                                            
         SPACE 2                                                                
CHAT4    LA    R3,WORK+5                                                        
         MVC   WORK(4),=C'YOUR'                                                 
         IC    R2,URVALUE                                                       
         BAS   RE,CHAT6                                                         
         MVC   0(7,R3),=C'BEAT MY'                                              
         LA    R3,8(R3)                                                         
         IC    R2,MYVALUE                                                       
         BAS   RE,CHAT6                                                         
         B     CHAT8                                                            
         SPACE 2                                                                
CHAT6    MH    R2,=H'16'                                                        
         SR    R4,R4                                                            
         LA    R2,HANDTBLE(R2)                                                  
         IC    R4,0(R2)                                                         
         MVC   0(15,R3),1(R2)                                                   
         LA    R3,1(R3,R4)                                                      
         SR    R2,R2                                                            
         BR    RE                                                               
         SPACE 2                                                                
CHAT8    FOUT  POKCHTH,WORK,60                                                  
         B     XIT                                                              
         SPACE 2                                                                
HANDTBLE DC    AL1(12)                                                          
         DC    C'BAG OF NAILS   '                                               
         DC    AL1(4)                                                           
         DC    C'PAIR           '                                               
         DC    AL1(9)                                                           
         DC    C'TWO PAIRS      '                                               
         DC    AL1(15)                                                          
         DC    C'THREE OF A KIND'                                               
         DC    AL1(8)                                                           
         DC    C'STRAIGHT       '                                               
         DC    AL1(5)                                                           
         DC    C'FLUSH          '                                               
         DC    AL1(10)                                                          
         DC    C'FULL HOUSE     '                                               
         DC    AL1(14)                                                          
         DC    C'FOUR OF A KIND '                                               
         DC    AL1(14)                                                          
         DC    C'STRAIGHT FLUSH '                                               
         EJECT                                                                  
*              EXITS FROM PLAY                                                  
         SPACE 3                                                                
PLAY1    LA    R3,M1A              NEW GAME                                     
         LA    R4,M2B                                                           
         XC    RAISE,RAISE                                                      
         B     PLAY10                                                           
         SPACE 2                                                                
PLAY2    LA    R3,M1B              CHECK                                        
         LA    R4,M2D                                                           
         OI    POKACTH+1,X'01'     MODIFY DATA TO ALLOW NO CHANGE               
         LA    R1,POKMSS1H                                                      
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         OI    1(R1),X'01'         MODIFY DATA FOR NO CHANGE                    
         XC    RAISE,RAISE                                                      
         B     PLAY10                                                           
         SPACE 2                                                                
PLAY3    LA    R3,M1C              RAISE (PHASE 1)                              
         LA    R4,M2A                                                           
         MVC   RAISE,MYGAP                                                      
         B     PLAY8                                                            
         SPACE 2                                                                
PLAY4    LA    R3,M1D              (FRESH CARDS)                                
         XC    RAISE,RAISE                                                      
         LA    R4,M2B                                                           
         MVC   M1DD+33(1),MYNUM    (NUMBER 1 DREW)                              
         OI    M1DD+33,X'F0'                                                    
         B     PLAY10                                                           
         SPACE 2                                                                
PLAY5    LA    R3,M1C                                                           
         LA    R4,M2C                                                           
         MVC   RAISE,MYGAP                                                      
         SPACE 2                                                                
PLAY8    EDIT  (4,RAISE),(10,M1CD+12),ALIGN=LEFT                                
         SPACE 2                                                                
PLAY10   GOTO1 MESSOUT,PARA,(R3),POKMSS1H                                       
         GOTO1 MESSOUT,PARA,(R4),POKMSS2H                                       
         FOUT  POKSTKH,SPACES,10                                                
         EDIT  (4,STAKE),(10,POKSTK),ALIGN=LEFT                                 
         SPACE 2                                                                
PLAY12   XC    POKAMTH+4(2),POKAMTH+4                                           
         MVC   POKACT(6),SPACES                                                 
         MVC   POKAMT(6),SPACES                                                 
         FOUT  POKACTH,SPACES,1                                                 
         FOUT  POKAMTH,SPACES,1                                                 
         SPACE 2                                                                
XMOD1    OI    6(R2),X'40'                                                      
         MVC   16(1,RA),PHASE                                                   
         XMOD1 1                                                                
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
TOOHIGH  FOUT  POKMSS1H,SPACES,60                                               
         MVC   POKMSS1(35),=C'RAISE AMOUNT EXCEEDS AGREED MAXIMUM'              
         B     XMOD1                                                            
         EJECT                                                                  
*              POOL OF OUTPUT MESSAGES                                          
         SPACE 3                                                                
M1A      DC    AL1(L'M1AD)                                                      
M1AD     DC    C'HERE''S YOUR HAND FOR A NEW GAME'                              
M1B      DC    AL1(L'M1BD)                                                      
M1BD     DC    C'YOU CHECKED'                                                   
M1C      DC    AL1(L'M1CD)                                                      
M1CD     DC    C'I RAISE YOU              '                                     
M1D      DC    AL1(L'M1DD)                                                      
M1DD     DC    C'YOU NOW HAVE FRESH CARDS. I DREW N'                            
         SPACE 2                                                                
M2A      DC    AL1(L'M2AD)                                                      
M2AD     DC    C'YOU MAY RAISE (R), CALL (C) OR FOLD (F)'                       
M2B      DC    AL1(L'M2BD)                                                      
M2BD     DC    C'YOU MAY BET (B) OR CHECK (C)'                                  
M2C      DC    AL1(L'M2CD)                                                      
M2CD     DC    C'YOU MAY RAISE (R), FOLD (F) OR CALL (C)'                       
M2D      DC    AL1(L'M2DD)                                                      
M2DD     DC    C'INPUT THE NUMBERS OF THE CARDS YOU WISH TO CHANGE'             
         SPACE 2                                                                
M3A      DC    AL1(L'M3AD)                                                      
M3AD     DC    C'YOU FOLDED'                                                    
M3B      DC    AL1(L'M3BD)                                                      
M3BD     DC    C'THE PACE WAS TOO HOT - I FOLDED'                               
M3C      DC    AL1(L'M3CD)                                                      
M3CD     DC    C'YOU SAW ME AND WON.  MY HAND WAS                    '          
M3D      DC    AL1(L'M3DD)                                                      
M3DD     DC    C'YOU SAW ME AND LOST. MY HAND WAS                    '          
M3E      DC    AL1(L'M3ED)                                                      
M3ED     DC    C'I SAW YOU AND WON.   MY HAND WAS                    '          
M3F      DC    AL1(L'M3FD)                                                      
M3FD     DC    C'I SAW YOU AND LOST.  MY HAND WAS                    '          
         SPACE 2                                                                
M4A      DC    AL1(L'M4AD)                                                      
M4AD     DC    C'YOU LOST           '                                           
M4B      DC    AL1(L'M4BD)                                                      
M4BD     DC    C'YOU WON            '                                           
M5A      DC    AL1(L'M5AD)                                                      
M5AD     DC    C'YOU ARE NOW UP                   '                             
M6A      DC    AL1(L'M6AD)                                                      
M6AD     DC    C'THE ANTE FOR THIS GAME IS         '                            
         EJECT                                                                  
GAPOKFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE GAPOKFFD                                                       
DUB      DS    D                                                                
PARA     DS    6F                                                               
WORK     DS    CL80                                                             
MYHAND   DS    CL10                                                             
YOURHAND DS    CL10                                                             
STAKE    DS    F                                                                
MAX      DS    F                                                                
ANTE     DS    F                                                                
RAISE    DS    F                                                                
MYVALUE  DS    F                                                                
URVALUE  DS    F                                                                
RANDOM   DS    F                                                                
BLUFF    DS    F                                                                
MYHIGH   DS    F                                                                
MYLOW    DS    F                                                                
MYGAP    DS    F                                                                
MYNUM    DS    CL1                                                              
DECK     DS    CL106                                                            
SPACES   DS    CL80                                                             
PHASE    DS    CL1                                                              
CARD     DS    CL2                                                              
BALANCE  DS    F                                                                
NUM      DS    CL8                                                              
LET      DS    CL8                                                              
HISNUM   DS    CL1                                                              
CHECKSW  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GAPOK00   05/01/02'                                      
         END                                                                    
