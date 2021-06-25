*          DATA SET GABLK00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0300A                                                                  
         TITLE 'BLACKJACK GAME'                                                 
         PRINT NOGEN                                                            
BLCKJACK CSECT                                                                  
         NMOD1 0,GABLK00                                                        
         L     RA,4(R1)                                                         
         USING GABLKFFD,RA                                                      
         BAS   RE,CUT                                                           
         EJECT                                                                  
*                  ROUTINES FOR ORIGINAL STAKE AND DEAL                         
         SPACE 3                                                                
         LA    R2,BLAACTH                                                       
         CLI   NEWGAMSW,X'FF'                                                   
         BE    BUY                 GAME IN PROGRESS                             
*                                  NEW GAME                                     
         LA    R3,11                                                            
         CLI   BLAACT,C'N'                                                      
         BNE   EXIT                                                             
         SPACE 1                                                                
         LA    R2,BLASTKEH                                                      
         LA    R3,2                                                             
         TM    4(R2),X'10'                                                      
         BO    EXIT                NUMERIC                                      
         BAS   RE,PACK                                                          
         LA    R3,3                                                             
         C     R0,TOTRES           NOT GREATER THAN RESERVES                    
         BH    EXIT                                                             
         MVI   NEWGAMSW,X'FF'      SET GAME IN PROGRESS                         
         ST    R0,STAKE                                                         
         ST    R0,TOTSTAKE                                                      
         MVC   BANKHAND(20),=CL20' '                                            
         MVC   YOURHAND(20),=CL20' '                                            
         LA    R4,YOURHAND                                                      
         BAS   RE,DEAL             DEAL THE CARDS                               
         BAS   RE,DEAL                                                          
         LA    R5,YOURSOFT                                                      
         BAS   RE,SCORE                                                         
         LA    R4,BANKHAND                                                      
         BAS   RE,DEAL                                                          
         BAS   RE,DEAL                                                          
         LA    R5,BANKSOFT                                                      
         BAS   RE,SCORE                                                         
         LA    R3,1                DID YOU GET A NATURAL                        
         CLC   YOURSOFT,=F'21'                                                  
         BNE   FORMAT                                                           
         LA    R3,8                                                             
         CLC   BANKSOFT,=F'21'     YES - DID THE BANK                           
         BNE   WONBLACK            NO  - PAY 3 TO 2                             
         LA    R3,5                YES - A DRAW                                 
         B     DRAW                                                             
         EJECT                                                                  
*                  ROUTINE FOR TWIST AND BUY                                    
         SPACE 3                                                                
BUY      CLI   BLAACT,C'H'                                                      
         BE    TWIST                                                            
         CLI   BLAACT,C'D'                                                      
         BNE   STICK                                                            
         L     R0,STAKE            DOUBLE DOWN                                  
         A     R0,TOTSTAKE                                                      
         LA    R3,3                                                             
         C     R0,TOTRES           TOTAL STAKE MUSTNT EXCEED RESERVES           
         BH    EXIT                                                             
         ST    R0,TOTSTAKE                                                      
         SPACE 2                                                                
TWIST    LA    R4,YOURHAND                                                      
         BAS   RE,DEAL                                                          
         LA    R5,YOURSOFT                                                      
         BAS   RE,SCORE                                                         
         LA    R3,4                DO YOU BREAK                                 
         MVC   ERR4+11(2),CARD                                                  
         CLC   YOURHARD,=F'21'                                                  
         BH    LOST                                                             
         LA    R3,1                                                             
         CLI   BLAACT,C'D'                                                      
         BE    BANKDRAW                                                         
         B     FORMAT                                                           
         EJECT                                                                  
*                  ROUTINES FOR STICK                                           
         SPACE 3                                                                
STICK    LA    R3,11                                                            
         LA    R2,BLAACTH                                                       
         CLI   BLAACT,C'S'                                                      
         BNE   EXIT                                                             
         SPACE 2                                                                
BANKDRAW LA    R4,BANKHAND                                                      
         LA    R5,BANKSOFT                                                      
         BAS   RE,SCORE                                                         
         LA    R3,9                CHECK WHETHER BANK BLEW                      
         CLC   BANKHARD,=F'21'                                                  
         BH    WON                                                              
         CLC   BANKSOFT,=F'17'     BANK HITS SOFT 17                            
         BH    BNKSTICK                                                         
         CLC   BANKHARD,=F'16'                                                  
         BH    BNKSTICK                                                         
         BAS   RE,DEAL                                                          
         B     BANKDRAW                                                         
         SPACE 2                                                                
BNKSTICK LA    R3,5                                                             
         CLC   BANKSOFT,YOURSOFT                                                
         BE    DRAW                                                             
         LA    R3,6                                                             
         MVC   ERR6+14(2),SOFT                                                  
         CLC   YOURSOFT,BANKSOFT                                                
         BH    WON                                                              
         LA    R3,7                                                             
         MVC   ERR7+14(2),SOFT                                                  
         B     LOST                                                             
         EJECT                                                                  
*                  ESTABLISH RESERVES AND CUT DECK                              
         SPACE 3                                                                
CUT      NTR1                                                                   
         CLC   BLAHEAD(9),=C'YOU START'                                         
         BNE   *+10                                                             
         MVC   TOTRES,=F'1000'                                                  
         TBIN  MILLI                                                            
         SR    R0,R0                                                            
         D     R0,=F'208'                                                       
         ST    R0,DECKCUT                                                       
         XIT1                                                                   
         EJECT                                                                  
*                  DEAL A CARD AND ADD TO HAND                                  
         SPACE 3                                                                
DEAL     NTR1                                                                   
         L     R1,DECKCUT                                                       
         LA    R1,1(R1)                                                         
         CH    R1,=H'208'                                                       
         BL    *+6                                                              
         SR    R1,R1                                                            
         ST    R1,DECKCUT                                                       
         LA    R1,DECKS(R1)                                                     
         SPACE 2                                                                
DEALOOP  CLI   0(R4),C' '                                                       
         BE    DEALEND                                                          
         LA    R4,1(R4)                                                         
         B     DEALOOP                                                          
         SPACE 2                                                                
DEALEND  MVC   0(1,R4),0(R1)                                                    
         MVC   CARD+1(1),0(R1)                                                  
         MVI   CARD,C' '                                                        
         CLI   CARD+1,C'T'                                                      
         BNE   *+10                                                             
         MVC   CARD(2),=C'10'                                                   
         XIT1                                                                   
         EJECT                                                                  
*                  WORK OUT POINT COUNT                                         
         SPACE 3                                                                
SCORE    NTR1                                                                   
         MVI   SHOWSW,C'N'                                                      
         XC    0(8,R5),0(R5)                                                    
         SR    R6,R6                                                            
         SPACE 2                                                                
SCORLOOP CLI   0(R4),C' '                                                       
         BE    SCOREND                                                          
         LA    R7,1                                                             
         CLI   0(R4),C'A'          CHECK ACE                                    
         BNE   SCORE2                                                           
         MVI   SHOWSW,C'Y'         SET SOFT SWITCH                              
         B     SCORADD                                                          
         SPACE 2                                                                
SCORE2   LA    R7,10                                                            
         CLI   0(R4),X'F0'         CHECK ANY OTHER LETTER                       
         BL    SCORADD                                                          
         PACK  DUB,0(1,R4)                                                      
         CVB   R7,DUB                                                           
         SPACE 2                                                                
SCORADD  AR    R6,R7                                                            
         LA    R4,1(R4)                                                         
         B     SCORLOOP                                                         
         SPACE 2                                                                
SCOREND  ST    R6,0(R5)                                                         
         ST    R6,4(R5)                                                         
         LA    R6,10(R6)                                                        
         CH    R6,=H'21'                                                        
         BH    SCOREXIT                                                         
         CLI   SHOWSW,C'Y'                                                      
         BNE   SCOREXIT                                                         
         ST    R6,0(R5)                                                         
         SPACE 2                                                                
SCOREXIT L     R1,0(R5)                                                         
         CVD   R1,DUB                                                           
         UNPK  SOFT,DUB                                                         
         OI    SOFT+1,X'F0'                                                     
         CLI   SOFT,X'F0'                                                       
         BNE   *+8                                                              
         MVI   SOFT,C' '                                                        
         XIT1                                                                   
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
PACK     SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SR    R0,R0                                                            
         LTR   R1,R1                                                            
         BCR   8,RE                                                             
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
EDIT     NTR1                                                                   
         MVC   0(10,R5),=CL10' '                                                
         CVD   R4,DUB                                                           
         UNPK  0(9,R5),DUB                                                      
         OI    8(R5),X'F0'                                                      
         LA    R6,9                                                             
         SPACE 2                                                                
EDLOOP   CLI   0(R5),X'F0'                                                      
         BNE   EDEND                                                            
         MVC   0(9,R5),1(R5)                                                    
         BCT   R6,EDLOOP                                                        
         SPACE 2                                                                
EDEND    XIT1                                                                   
         EJECT                                                                  
*                  FORMAT OUTPUT SCREEN                                         
         SPACE 2                                                                
WONBLACK L     R0,TOTSTAKE                                                      
         MH    R0,=H'3'                                                         
         AH    R0,=H'1'                                                         
         SRL   R0,1                                                             
         ST    R0,TOTSTAKE                                                      
         SPACE 2                                                                
WON      L     R0,TOTSTAKE                                                      
         A     R0,TOTRES                                                        
         ST    R0,TOTRES                                                        
         B     WONLOST                                                          
         SPACE 2                                                                
LOST     L     R0,TOTRES                                                        
         S     R0,TOTSTAKE                                                      
         ST    R0,TOTRES                                                        
         SPACE 2                                                                
DRAW     EQU   *                                                                
         SPACE 2                                                                
WONLOST  LA    R2,BLASTKEH                                                      
         FOUT  BLAACTH,=C'N',1                                                  
         FOUT  BLASTKEH,=C'0',1                                                 
         MVI   SHOWSW,C'Y'                                                      
         MVI   NEWGAMSW,0          SET FOR NEW GAME                             
         B     FORMAT2                                                          
         EJECT                                                                  
*                  FORMAT A HAND FOR OUTPUT                                     
         SPACE 3                                                                
FORHAND  NTR1                                                                   
         MVC   0(20,R5),=CL20' '                                                
         CLI   SHOWSW,C'Y'                                                      
         BE    FORLOOP                                                          
         MVI   SHOWSW,C'Y'                                                      
         MVI   0(R5),C'?'                                                       
         SPACE 2                                                                
FORNEXT  LA    R4,1(R4)                                                         
         LA    R5,2(R5)                                                         
         SPACE 2                                                                
FORLOOP  CLI   0(R4),C' '                                                       
         BE    FOREND                                                           
         MVC   0(1,R5),0(R4)                                                    
         CLI   0(R4),C'T'          SPECIAL FOR TENS                             
         BNE   FORNEXT                                                          
         MVC   0(2,R5),=C'10'                                                   
         LA    R5,1(R5)                                                         
         B     FORNEXT                                                          
         SPACE 2                                                                
FOREND   XIT1                                                                   
         EJECT                                                                  
FORMAT   LA    R2,BLAACTH                                                       
         MVI   SHOWSW,C'N'                                                      
         FOUT  BLAACTH,=2C' ',2                                                 
         SPACE 2                                                                
FORMAT2  LA    R4,BANKHAND                                                      
         LA    R5,BLABANK                                                       
         BAS   RE,FORHAND                                                       
         LA    R4,YOURHAND                                                      
         LA    R5,BLAYOUR                                                       
         BAS   RE,FORHAND                                                       
         MVC   BLAPNT(10),=CL20' ' EDIT POINT COUNT                             
         L     R0,YOURHARD                                                      
         CVD   R0,DUB                                                           
         UNPK  BLAPNT(2),DUB                                                    
         OI    BLAPNT+1,X'F0'                                                   
         CLC   YOURSOFT,YOURHARD                                                
         BE    FORMAT4                                                          
         MVC   BLAPNT+3(2),=C'OR'                                               
         L     R0,YOURSOFT                                                      
         CVD   R0,DUB                                                           
         UNPK  BLAPNT+6(2),DUB                                                  
         OI    BLAPNT+7,X'F0'                                                   
         SPACE 2                                                                
FORMAT4  CLI   BLAPNT,C'0'                                                      
         BNE   *+10                                                             
         MVC   BLAPNT(9),BLAPNT+1                                               
         L     R4,TOTSTAKE                                                      
         LA    R5,BLATHIS                                                       
         BAS   RE,EDIT                                                          
         L     R4,TOTRES                                                        
         LA    R5,BLARES                                                        
         BAS   RE,EDIT                                                          
         FOUT  BLAYOURH                                                         
         FOUT  BLABANKH                                                         
         FOUT  BLAPNTH                                                          
         FOUT  BLATHISH                                                         
         FOUT  BLARESH                                                          
         SPACE 2                                                                
EXIT     OI    6(R2),X'40'                                                      
         BCTR  R3,R0                                                            
         MH    R3,=H'50'                                                        
         LA    R3,ERRLIST(R3)                                                   
         MVC   BLAHEAD(50),0(R3)                                                
         FOUT  BLAHEADH                                                         
         XMOD1 1                                                                
         EJECT                                                                  
*                  4 DECKS OF CARDS                                             
         SPACE 3                                                                
DECKS    DC    C'KQQ87KAT67T48Q5A86946A722J'                                    
         DC    C'4453J672TT456T286A3553K824'                                    
         DC    C'QA5A37Q2Q9JJ839JJKT76QK993'                                    
         DC    C'JAK988J5473TAK9243KT7Q2659'                                    
         DC    C'A73447KJ79QQ5J8JQ284222ATJ'                                    
         DC    C'A37K5T22239AT46QA4T767AT6T'                                    
         DC    C'99K39K2584T3A3J9586KQKJ888'                                    
         DC    C'9T7546JJ6K366K83547QAQ5Q9A'                                    
         SPACE 3                                                                
*                  ERROR MESSAGE LIST                                           
         SPACE 3                                                                
ERRLIST  DC    CL50'YOU MAY HIT (H) STICK (S) OR DOUBLE-DOWN (D)'               
         DC    CL50'STAKE IS NOT NUMERIC - PLEASE REINPUT'                      
         DC    CL50'STAKE EXCEEDS YOUR CASH BALANCE - PLEASE REINPUT'           
ERR4     DC    CL50'YOU DREW A XX AND BUST - STAKE FOR NEXT HAND'               
         DC    CL50'DRAWN GAME - STAKE FOR NEXT HAND'                           
ERR6     DC    CL50'BANK STUCK AT XX - YOU WON - STAKE FOR NEXT HAND'           
ERR7     DC    CL50'BANK STUCK AT XX - YOU LOST - STAKE FOR NEXT HAND'          
         DC    CL50'YOU WERE DEALT BLACKJACK - STAKE FOR NEXT HAND'             
         DC    CL50'BANK BUST - YOU WON - STAKE FOR NEXT HAND'                  
         DC    CL50'BUY STAKE EXCEEDS ORIGINAL - PLEASE REINPUT'                
         DC    CL50'ACTION NOT RECOGNISED - PLEASE REINPUT'                     
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
GABLKFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE GABLKFFD                                                       
*                                                                               
DUB      DS    D                                                                
STAKE    DS    F                                                                
TOTSTAKE DS    F                                                                
TOTRES   DS    F                                                                
YOURSOFT DS    F                                                                
YOURHARD DS    F                                                                
BANKSOFT DS    F                                                                
BANKHARD DS    F                                                                
DECKCUT  DS    F                                                                
CARD     DS    CL2                                                              
SOFT     DS    CL2                                                              
BANKHAND DS    CL20                                                             
YOURHAND DS    CL20                                                             
SHOWSW   DS    CL1                                                              
NEWGAMSW DS    CL1                                                              
         SPACE 2                                                                
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GABLK00   05/01/02'                                      
         END                                                                    
