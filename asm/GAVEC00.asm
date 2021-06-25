*          DATA SET GAVEC00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0600A                                                                  
         TITLE 'VECTORS (BULLS AND MAGPIES) GAME'                               
         PRINT NOGEN                                                            
VECTORS  CSECT                                                                  
         NMOD1 2,GAVEC00                                                        
         USING VECTEMP,RC                                                       
         L     RA,4(R1)                                                         
         USING VECSAVE,RA                                                       
*                                                                               
         XC    VECHDR,VECHDR                                                    
         CLI   VECGO,0             IS A GAME IN PROGRESS                        
         BE    NEWGAME             NO START A NEW GAME                          
         EJECT                                                                  
GIP      DS    0H                  VALIDATE INPUT GUESS                         
         CLI   VECGUSH+5,4                                                      
         BNE   ERROR               NOT 4 CHR INPUT                              
         CLC   VECGUS(4),=C'LOSE'                                               
         BE    LOSE                PLAYER GIVES IN                              
         TM    VECGUSH+4,X'08'                                                  
         BZ    ERROR               NOT NUMERIC                                  
         LA    RE,VECGUS                                                        
         BAS   R9,ALLDIFF                                                       
         BNZ   ERROR               NOT ALL DIFFERENT                            
*                                                                               
         BAS   R9,BULLMAGS         COUNT BULLS & MAGPIES                        
*                                                                               
         SR    R3,R3               DISPLAY INPUT GUESS & SCORE                  
         IC    R3,VECGO                                                         
         MH    R3,=H'38'                                                        
         LA    R3,VECG1H(R3)                                                    
         MVC   18(4,R3),VECGUS                                                  
         MVC   24(7,R3),=C'SCORE ='                                             
         UNPK  32(1,R3),BULLS                                                   
         OI    32(R3),X'F0'                                                     
         CLI   32(R3),C'0'                                                      
         BNE   *+14                                                             
         MVC   32(2,R3),=C'--'                                                  
         B     *+8                                                              
         MVI   33(R3),C'B'                                                      
         MVI   34(R3),C','                                                      
         UNPK  35(1,R3),MAGS                                                    
         OI    35(R3),X'F0'                                                     
         CLI   35(R3),C'0'                                                      
         BNE   *+14                                                             
         MVC   35(2,R3),=C'--'                                                  
         B     *+8                                                              
         MVI   36(R3),C'M'                                                      
         OI    6(R3),X'80'                                                      
*                                                                               
         CP    BULLS,=P'4'         HAS HE WON                                   
         BE    WIN                 YES                                          
         SR    R5,R5               NO BUMP GUESS COUNT                          
         IC    R5,VECGO                                                         
         LA    R5,1(R5)                                                         
         STC   R5,VECGO                                                         
         CH    R5,MAXGO            LAST GUESS                                   
         BE    LOSE                YES HE LOSES                                 
*                                                                               
         MVC   VECHDR(L'MSG1),MSG1 SET GAME IN PROGRESS MSG                     
*                                                                               
EXIT     OI    VECHDRH+6,X'80'                                                  
         OI    VECGUSH+6,X'40'                                                  
         XMOD1 1                                                                
         EJECT                                                                  
ERROR    MVC   VECHDR(L'MSG2),MSG2 SET ERROR MSG                                
         B     EXIT                                                             
*                                                                               
WIN      MVC   VECANSH+18(4),VECNUM                                             
         PACK  DUB,VECANSH+44(2)   BUMP PLAYER TOTAL                            
         AP    DUB+6(2),=P'1'                                                   
         UNPK  VECANSH+44(2),DUB+6(2)                                           
         OI    VECANSH+45,X'F0'                                                 
         MVI   VECGO,0             SET END OF GAME                              
         MVC   VECHDR(L'MSG3),MSG3 SET WIN MSG                                  
         OI    VECANSH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
LOSE     MVC   VECANSH+18(4),VECNUM                                             
         PACK  DUB,VECANSH+33(2)   BUMP COMPUTER TOTAL                          
         AP    DUB+6(2),=P'1'                                                   
         UNPK  VECANSH+33(2),DUB+6(2)                                           
         OI    VECANSH+34,X'F0'                                                 
         MVI   VECGO,0             SET END OF GAME                              
         MVC   VECHDR(L'MSG4),MSG4 SET LOSE MSG                                 
         OI    VECANSH+6,X'80'                                                  
         B     EXIT                                                             
         EJECT                                                                  
NEWGAME  DS    0H                  INITIALISE FOR NEW GAME                      
         LA    R3,VECG1H                                                        
         LH    R0,MAXGO                                                         
*                                                                               
NG1      MVC   18(20,R3),SPACES    CLEAR GUESSES/SCORES/ANSWER                  
         OI    6(R3),X'80'                                                      
         LA    R3,38(R3)                                                        
         BCT   R0,NG1                                                           
         MVC   VECANSH+18(4),SPACES                                             
         OI    VECANSH+6,X'80'                                                  
*                                                                               
NG2      TBIN  MILLI               GENERATE A 4 DIGIT NUMBER                    
         LR    R0,R1                                                            
NG3      MR    R0,R0               SQUARE                                       
         ALR   R0,R1               FOLD                                         
         CVD   R0,DUB              EXTRACT                                      
         UNPK  VECNUM(4),DUB+5(3)                                               
         OI    VECNUM+3,X'F0'                                                   
         LA    RE,VECNUM                                                        
         BAS   R9,ALLDIFF          ALL DIGITS DIFFERENT                         
         BNZ   NG3                 NO TRY AGAIN                                 
*                                                                               
         B     GIP                                                              
         EJECT                                                                  
ALLDIFF  DS    0H                  CHECK DIGITS AT 0(RE) DIFFERENT              
         CLI   0(RE),C'0'                                                       
         BE    ALLDX               CANT START WITH ZERO                         
         CLC   0(1,RE),1(RE)                                                    
         BE    ALLDX                                                            
         CLC   0(1,RE),2(RE)                                                    
         BE    ALLDX                                                            
         CLC   0(1,RE),3(RE)                                                    
         BE    ALLDX                                                            
         CLC   1(1,RE),2(RE)                                                    
         BE    ALLDX                                                            
         CLC   1(1,RE),3(RE)                                                    
         BE    ALLDX                                                            
         CLC   2(1,RE),3(RE)                                                    
         BE    ALLDX                                                            
         SR    RE,RE               SET CC TO ZERO IF ALL DIFFERENT              
ALLDX    LTR   RE,RE                                                            
         BR    R9                                                               
         EJECT                                                                  
BULLMAGS DS    0H                  COUNT BULLS & MAGPIES                        
         ZAP   BULLS,=P'0'                                                      
         ZAP   MAGS,=P'0'                                                       
         LA    RE,VECNUM                                                        
         LA    RF,VECGUS                                                        
*                                                                               
BM1      CLC   0(1,RE),0(RF)                                                    
         BE    BM1B                                                             
         CLC   0(1,RE),1(RF)                                                    
         BE    BM1M                                                             
         CLC   0(1,RE),2(RF)                                                    
         BE    BM1M                                                             
         CLC   0(1,RE),3(RF)                                                    
         BE    BM1M                                                             
         B     BM2                                                              
BM1B     AP    BULLS,=P'1'                                                      
         B     BM2                                                              
BM1M     AP    MAGS,=P'1'                                                       
*                                                                               
BM2      CLC   1(1,RE),0(RF)                                                    
         BE    BM2M                                                             
         CLC   1(1,RE),1(RF)                                                    
         BE    BM2B                                                             
         CLC   1(1,RE),2(RF)                                                    
         BE    BM2M                                                             
         CLC   1(1,RE),3(RF)                                                    
         BE    BM2M                                                             
         B     BM3                                                              
BM2B     AP    BULLS,=P'1'                                                      
         B     BM3                                                              
BM2M     AP    MAGS,=P'1'                                                       
*                                                                               
BM3      CLC   2(1,RE),0(RF)                                                    
         BE    BM3M                                                             
         CLC   2(1,RE),1(RF)                                                    
         BE    BM3M                                                             
         CLC   2(1,RE),2(RF)                                                    
         BE    BM3B                                                             
         CLC   2(1,RE),3(RF)                                                    
         BE    BM3M                                                             
         B     BM4                                                              
BM3B     AP    BULLS,=P'1'                                                      
         B     BM4                                                              
BM3M     AP    MAGS,=P'1'                                                       
*                                                                               
BM4      CLC   3(1,RE),0(RF)                                                    
         BE    BM4M                                                             
         CLC   3(1,RE),1(RF)                                                    
         BE    BM4M                                                             
         CLC   3(1,RE),2(RF)                                                    
         BE    BM4M                                                             
         CLC   3(1,RE),3(RF)                                                    
         BE    BM4B                                                             
         B     BMX                                                              
BM4B     AP    BULLS,=P'1'                                                      
         B     BMX                                                              
BM4M     AP    MAGS,=P'1'                                                       
*                                                                               
BMX      BR    R9                                                               
         EJECT                                                                  
MAXGO    DC    H'7'                                                             
SPACES   DC    CL20' '                                                          
*                                                                               
MSG1     DC    C'GAME IN PROGRESS - ENTER NEXT GUESS'                           
MSG2     DC    C'INVALID GUESS - REINPUT'                                       
MSG3     DC    C'YOU WON - NEW GAME STARTED -ENTER FIRST GUESS'                 
MSG4     DC    C'YOU LOST - NEW GAME STARTED - ENTER FIRST GUESS'               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
VECTEMP  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
BULLS    DS    P                                                                
MAGS     DS    P                                                                
         EJECT                                                                  
VECSAVE  DSECT                                                                  
         DS    CL16                                                             
VECGO    DS    XL1                                                              
VECNUM   DS    CL4                                                              
         DS    CL43                                                             
       ++INCLUDE GAVECFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GAVEC00   05/01/02'                                      
         END                                                                    
