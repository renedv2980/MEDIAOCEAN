*          DATA SET GAYAH00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0400A                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'YAHTZEE GAME'                                                   
         PRINT NOGEN                                                            
YAHTCNTL CSECT                                                                  
         NMOD1 0,GAYAH00,RR=R9                                                  
         L     RA,4(R1)                                                         
         USING YAHTZD,RA                                                        
         EJECT                                                                  
*              LOGIC OF GAME - CHANGE OF DICE                                   
         SPACE 3                                                                
         CLI   36(RA),0            FIRST TIME                                   
         BNE   YA2                                                              
         MVI   36(RA),X'FF'                                                     
         MVI   GAME,1                                                           
         B     GAME0                                                            
         SPACE 2                                                                
YA2      CLI   GAME,7              CHECK NOT PAST GAME END                      
         BE    GAME3                                                            
         LA    R2,YAHACT                                                        
         SR    R3,R3                                                            
         IC    R3,YAHACTH+5                                                     
         LTR   R3,R3               MUST BE SOME INPUT                           
         BZ    ERROR1                                                           
         CLI   0(R2),C'1'          NUMBERS IMPLY CHANGE OF DICE                 
         BL    LETTER                                                           
         CLI   THROW,3             BUT THAT'S NOT ALLOWED IF HE'S               
         BE    GAME2               HAD 3 THROW ALREADY                          
         XC    MASK,MASK                                                        
         SPACE 2                                                                
YA4      CLI   0(R2),C'1'          VALIDATE CHANGE NUMBERS                      
         BL    ERROR3              ARE NUMERIC                                  
         CLI   0(R2),C'5'                                                       
         BH    ERROR3                                                           
         PACK  DUB,0(1,R2)                                                      
         CVB   R4,DUB                                                           
         LA    R4,MASK-1(R4)                                                    
         CLI   0(R4),X'FF'         AND ARE NOT REPEATED                         
         BE    ERROR4                                                           
         MVI   0(R4),X'FF'         SET MASK                                     
         LA    R2,1(R2)                                                         
         BCT   R3,YA4                                                           
         SPACE 2                                                                
         MVC   SORT,HAND                                                        
         BAS   RE,THROWEM          GET FIVE NEW                                 
         LA    R2,SORT                                                          
         LA    R3,HAND                                                          
         LA    R4,MASK                                                          
         LA    R5,5                                                             
         SPACE 2                                                                
YA6      CLI   0(R4),X'FF'         MERGE WITH OLD                               
         BE    *+10                                                             
         MVC   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,YA6                                                           
         BAS   RE,DISPHAND                                                      
         SPACE 2                                                                
         IC    R5,THROW            BUMP THROW                                   
         LA    R5,1(R5)                                                         
         STC   R5,THROW                                                         
         CLI   THROW,3             AND SELECT MESSAGE                           
         BE    GAME2                                                            
         B     GAME1B                                                           
         EJECT                                                                  
*              VALIDATE LETTER FOR SELECTED HAND                                
         SPACE 2                                                                
LETTER   CLI   YAHACTH+5,1                                                      
         BNE   ERROR3                                                           
         LA    R2,HANDLETS                                                      
         LA    R3,HANDOUTS-1                                                    
         LA    R4,HANDROUT                                                      
         LA    R5,SCORES                                                        
         LA    R6,ABOVE                                                         
         LA    R7,13                                                            
         LA    R8,YAHONEH                                                       
         SPACE 2                                                                
YA10     CLC   0(1,R2),YAHACT                                                   
         BE    YA12                                                             
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,2(R5)                                                         
         LA    R6,1(R6)                                                         
         CH    R7,=H'1'                                                         
         BE    ERROR1                                                           
         CLI   0(R2),C'G'                                                       
         BNE   *+8                                                              
         LA    R5,6(R5)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R3)                                                         
         SR    RF,RF                                                            
         IC    RF,0(R8)                                                         
         AR    R8,RF                                                            
         BCT   RE,*-6                                                           
         BCT   R7,YA10                                                          
         B     ERROR1              LETTER NOT IN LIST                           
         SPACE 2                                                                
YA12     CLI   0(R6),0             CHECK NOT USING A LETTER TWICE               
         BE    ERROR2                                                           
         MVI   0(R6),0                                                          
         LA    R3,8(R8)                                                         
         BAS   RE,SORTEM                                                        
         L     RF,0(R4)            SELECT EVALUATION ROUTINE                    
         AR    RF,R9                                                            
         LA    R2,SORT                                                          
         LR    R0,R3                                                            
         LA    R3,5                                                             
         SR    R4,R4               GO AND SCORE THE HAND                        
         BASR  RE,RF                                                            
         SPACE 2                                                                
         LH    R2,0(R5)                                                         
         LR    R3,R0                                                            
         BAS   RE,SCOROUT                                                       
         OC    SCORES+28(2),SCORES+28                                           
         BZ    YA14                SPECIAL CHECK FOR ANOTHER YAHTZEE            
         CLC   HAND+1(4),HAND                                                   
         BNE   YA14                                                             
         CLI   YAHACT,C'M'                                                      
         BE    YA14                                                             
         LH    R2,SCORES+28        HE'S GOT IT, SO WE'LL GIVE HIM               
         LA    R2,100(R2)          ANOTHER 100 AS A BONUS                       
         STH   R2,SCORES+28                                                     
         LA    R3,YAHYAH                                                        
         BAS   RE,SCOROUT                                                       
         LH    R2,SCORES+32                                                     
         LA    R2,100(R2)                                                       
         STH   R2,SCORES+32                                                     
         LA    R3,YAHTBL                                                        
         BAS   RE,SCOROUT                                                       
         EJECT                                                                  
*              SPECIAL ROUTINES FOR ABOVE LINE                                  
         SPACE 3                                                                
YA14     CLI   YAHACT,C'F'                                                      
         BH    YA20                                                             
         LH    R2,0(R5)                                                         
         LTR   R2,R2                                                            
         BZ    YA16                                                             
         AH    R2,SCORES+12        ADD TO ABOVE LINE TALLY                      
         STH   R2,SCORES+12                                                     
         LA    R3,YAHTOT                                                        
         BAS   RE,SCOROUT                                                       
         SPACE 2                                                                
YA16     OC    ABOVE,ABOVE         DOES THIS COMLETE ABOVE THE LINE             
         BNZ   GAME1                                                            
         LH    R2,SCORES+12        CHECK FOR BONUS                              
         CH    R2,=H'63'                                                        
         BL    YA18                                                             
         MVC   SCORES+14(2),=H'35'                                              
         LA    R2,35(R2)                                                        
         SPACE 2                                                                
YA18     LA    R3,YAHTAL                                                        
         BAS   RE,SCOROUT                                                       
         LA    R3,YAHTA2                                                        
         BAS   RE,SCOROUT                                                       
         STH   R2,SCORES+34                                                     
         LH    R2,SCORES+14                                                     
         LTR   R2,R2                                                            
         BZ    YA30                                                             
         LA    R3,YAHBON                                                        
         BAS   RE,SCOROUT                                                       
         B     YA30                                                             
         EJECT                                                                  
*              SPECIAL ROUTINES FOR BELOW LINE AND END GAME                     
         SPACE 2                                                                
YA20     LH    R2,0(R5)                                                         
         LTR   R2,R2                                                            
         BZ    YA30                                                             
         AH    R2,SCORES+32        ADD TO TOTAL BELOW LINE                      
         STH   R2,SCORES+32                                                     
         LA    R3,YAHTBL                                                        
         BAS   RE,SCOROUT                                                       
         SPACE 2                                                                
YA30     OC    ABOVE(13),ABOVE     IS THE GAME OVER ?                           
         BNZ   GAME1                                                            
         LH    R2,SCORES+32                                                     
         AH    R2,SCORES+34                                                     
         LA    R3,YAHGAM                                                        
         BAS   RE,SCOROUT          FORMAT GAME TOTAL                            
         SR    R3,R3               BUMP GAME #                                  
         IC    R3,GAME                                                          
         LA    R3,1(R3)                                                         
         STC   R3,GAME                                                          
         CLI   GAME,7                                                           
         BE    GAME3                                                            
         B     GAME0                                                            
         EJECT                                                                  
*              ROUTINES TO EVALUATE HANDS                                       
         SPACE 3                                                                
ONES     CLI   0(R2),1             R2 = A(SORTED HAND)                          
         BNE   *+8                 R3 = 5                                       
         LA    R4,1(R4)            R4 = ZERO                                    
         LA    R2,1(R2)            R5 = A(H SCORE)                              
         BCT   R3,ONES             RE = RETURN                                  
         STH   R4,0(R5)                                                         
         BR    RE                                                               
         SPACE 2                                                                
TWOS     CLI   0(R2),2                                                          
         BNE   *+8                                                              
         LA    R4,2(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,TWOS                                                          
         STH   R4,0(R5)                                                         
         BR    RE                                                               
         SPACE 2                                                                
THREES   CLI   0(R2),3                                                          
         BNE   *+8                                                              
         LA    R4,3(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,THREES                                                        
         STH   R4,0(R5)                                                         
         BR    RE                                                               
         SPACE 2                                                                
FOURS    CLI   0(R2),4                                                          
         BNE   *+8                                                              
         LA    R4,4(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,FOURS                                                         
         STH   R4,0(R5)                                                         
         BR    RE                                                               
         SPACE 2                                                                
FIVES    CLI   0(R2),5                                                          
         BNE   *+8                                                              
         LA    R4,5(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,FIVES                                                         
         STH   R4,0(R5)                                                         
         BR    RE                                                               
         SPACE 2                                                                
SIXES    CLI   0(R2),6                                                          
         BNE   *+8                                                              
         LA    R4,6(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,SIXES                                                         
         STH   R4,0(R5)                                                         
         BR    RE                                                               
         SPACE 2                                                                
TOFKIND  CLC   1(2,R2),0(R2)                                                    
         BE    CHANCE                                                           
         CLC   2(2,R2),1(R2)                                                    
         BE    CHANCE                                                           
         CLC   3(2,R2),2(R2)                                                    
         BE    CHANCE                                                           
         BR    RE                                                               
         SPACE 2                                                                
FOFKIND  CLC   1(3,R2),0(R2)                                                    
         BE    CHANCE                                                           
         CLC   2(3,R2),1(R2)                                                    
         BE    CHANCE                                                           
         BR    RE                                                               
         SPACE 2                                                                
FULHOUSE CLC   1(2,R2),0(R2)                                                    
         BE    FUL2                                                             
         CLC   1(1,R2),0(R2)                                                    
         BE    FUL4                                                             
         BR    RE                                                               
         SPACE 2                                                                
FUL2     CLC   4(1,R2),3(R2)       CHECK FOR 3 AND 2                            
         BE    FUL6                                                             
         BR    RE                                                               
         SPACE 2                                                                
FUL4     CLC   3(2,R2),2(R2)       OR 2 AND THREE                               
         BE    FUL6                                                             
         BR    RE                                                               
         SPACE 2                                                                
FUL6     MVC   0(2,R5),=H'25'                                                   
         BR    RE                                                               
         SPACE 2                                                                
SHORT    XC    WORK(6),WORK                                                     
         SPACE 2                                                                
SH1      SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         LA    R4,WORK-1(R4)                                                    
         MVI   0(R4),C'Y'                                                       
         LA    R2,1(R2)                                                         
         BCT   R3,SH1                                                           
         CLC   WORK+0(4),=C'YYYY'                                               
         BE    SH2                                                              
         CLC   WORK+1(4),=C'YYYY'                                               
         BE    SH2                                                              
         CLC   WORK+2(4),=C'YYYY'                                               
         BE    SH2                                                              
         BR    RE                                                               
         SPACE 2                                                                
SH2      MVC   0(2,R5),=H'30'                                                   
         BR    RE                                                               
         SPACE 2                                                                
LONG     CLC   0(5,R2),=X'0102030405'                                           
         BE    LONG2                                                            
         CLC   0(5,R2),=X'0203040506'                                           
         BE    LONG2                                                            
         BR    RE                                                               
         SPACE 2                                                                
LONG2    MVC   0(2,R5),=H'40'                                                   
         BR    RE                                                               
         SPACE 2                                                                
YAHTZEE  CLC   1(4,R2),0(R2)                                                    
         BCR   7,RE                                                             
         MVC   0(2,R5),=H'50'                                                   
         BR    RE                                                               
         SPACE 2                                                                
CHANCE   SR    RF,RF               ADD UP THE SPOTS                             
         IC    RF,0(R2)                                                         
         AR    R4,RF                                                            
         LA    R2,1(R2)                                                         
         BCT   R3,CHANCE                                                        
         STH   R4,0(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO PUT THE SCORE OUT                                     
         SPACE 3                                                                
SCOROUT  NTR1                                                                   
         SH    R3,=H'8'                                                         
         OI    6(R3),X'80'         TRANSMIT FIELD                               
         SR    R4,R4                                                            
         IC    R4,GAME             GAME DICTATES COLUMN                         
         BCTR  R4,0                                                             
         MH    R4,=H'5'                                                         
         LA    R3,8+25(R3,R4)                                                   
         EDIT  (R2),(5,0(R3))                                                   
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         MVI   4(R3),C'0'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO THROW THE DICE,SORT AND DISPLAY                       
         SPACE                                                                  
THROWEM  NTR1                                                                   
         TBIN  MILLI                                                            
         SLL   R1,2                                                             
         SRL   R1,2                                                             
         MR    R0,R1                                                            
         LA    R2,HAND                                                          
         LA    R3,5                                                             
         SPACE 2                                                                
THROWEM2 SR    R0,R0                                                            
         D     R0,=F'6'                                                         
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,THROWEM2                                                      
         B     XIT                                                              
         SPACE 2                                                                
SORTEM   NTR1                                                                   
         MVC   SORT,HAND                                                        
         GOTO1 =V(XSORT),WORK,(0,SORT),5,1,1,0,RR=R9                            
         BASR  RE,RF                                                            
         B     XIT                                                              
         SPACE 2                                                                
DISPHAND NTR1                                                                   
         OI    YAHHANDH+6,X'80'                                                 
         LA    R2,HAND                                                          
         LA    R3,YAHHAND                                                       
         LA    R4,5                                                             
         SPACE 2                                                                
DH2      MVC   0(1,R3),0(R2)                                                    
         OI    0(R3),X'F0'                                                      
         LA    R2,1(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,DH2                                                           
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EXITS FROM PROGRAM                                               
         SPACE 3                                                                
ERROR1   LA    R2,1                                                             
         B     ALLX                                                             
         SPACE 2                                                                
ERROR2   LA    R2,2                                                             
         B     ALLX                                                             
         SPACE 2                                                                
ERROR3   LA    R2,3                                                             
         B     ALLX                                                             
         SPACE 2                                                                
ERROR4   LA    R2,4                                                             
         B     ALLX                                                             
         SPACE 2                                                                
GAME0    MVC   ABOVE,=7X'FF'                                                    
         MVC   BELOW,=7X'FF'                                                    
         XC    SCORES(38),SCORES                                                
         SPACE 2                                                                
GAME1    BAS   RE,THROWEM                                                       
         MVI   THROW,1                                                          
         BAS   RE,DISPHAND                                                      
         SPACE 2                                                                
GAME1B   LA    R2,5                                                             
         B     ALLX                                                             
         SPACE 2                                                                
GAME2    LA    R2,6                                                             
         B     ALLX                                                             
         SPACE 2                                                                
GAME3    LA    R2,7                                                             
         SPACE 2                                                                
ALLX     BCTR  R2,0                                                             
         MH    R2,=H'58'                                                        
         LA    R2,MESSAGES(R2)                                                  
         CLC   YAHHEAD(58),0(R2)                                                
         BE    ALLX2                                                            
         FOUT  YAHHEADH,0(R2),58                                                
         SPACE 2                                                                
ALLX2    OI    YAHACTH+6,X'40'     INSERT CURSOR                                
         XMOD1 1                                                                
         EJECT                                                                  
*              MESSAGES TO OUTPUT TO LINE 1                                     
         SPACE 3                                                                
MESSAGES DC    CL58'NOT A VALID HAND LETTER'                                    
         DC    CL58'THIS LETTER HAS ALREADY BEEN USED'                          
         DC    CL58'SELECTED DICE NUMBERS SHOULD BE 1-5'                        
         DC    CL58'DICE NUMBER HAS BEEN REPEATED'                              
         SPACE 2                                                                
         DC    CL45'SELECT A HAND (A-N) OR ENTER NUMBERS OF DICE '              
         DC    CL13'TO BE CHANGED'                                              
         DC    CL58'YOU''VE HAD THREE THROWS. SELECT A HAND (A-N)'              
         DC    CL58'THAT''S THE END OF GAME 6. HOPE YOU ENJOYED IT'             
         SPACE 2                                                                
HANDLETS DC    C'ABCDEFGHJKLMN'                                                 
         SPACE 2                                                                
HANDOUTS DS    0F                                                               
         DC    AL1(1,3,1,4,1,4,1,1,1,1,1,1,1)                                   
         SPACE 2                                                                
HANDROUT DS    0F                                                               
         DC    A(ONES)                                                          
         DC    A(TWOS)                                                          
         DC    A(THREES)                                                        
         DC    A(FOURS)                                                         
         DC    A(FIVES)                                                         
         DC    A(SIXES)                                                         
         DC    A(TOFKIND)                                                       
         DC    A(FOFKIND)                                                       
         DC    A(FULHOUSE)                                                      
         DC    A(SHORT)                                                         
         DC    A(LONG)                                                          
         DC    A(YAHTZEE)                                                       
         DC    A(CHANCE)                                                        
         EJECT                                                                  
YAHTZD   DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE GAYAHFFD                                                       
         SPACE 2                                                                
THROW    DS    CL1                                                              
GAME     DS    CL1                                                              
HAND     DS    CL5                                                              
SORT     DS    CL5                                                              
ABOVE    DS    CL6                                                              
BELOW    DS    CL7                                                              
SCORES   DS    19H                                                              
MASK     DS    CL5                                                              
DUB      DS    D                                                                
WORK     DS    CL32                                                             
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GAYAH00   05/01/02'                                      
         END                                                                    
