*          DATA SET GANIM00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0200A                                                                  
         TITLE 'NIM GAME'                                                       
         PRINT NOGEN                                                            
NIM      CSECT                                                                  
         NMOD1 100,GANIM00                                                      
         USING NIMD,RC                                                          
         L     RA,4(R1)                                                         
         USING GAMTWA,RA                                                        
         LR    R9,RA                                                            
         USING GANIMFFD,R9                                                      
         SPACE 2                                                                
*                                  SET FIRST TIME SW                            
         CLI   FIRST,0                                                          
         BNE   *+8                                                              
         MVI   FIRST,C'Y'                                                       
         EJECT                                                                  
*                  ROUTINE TO SAVE AND CLEAR SCREEN                             
         SPACE 3                                                                
         LA    R2,NIMHEAD                                                       
         LA    R3,SAVLINES                                                      
         LA    R4,12                                                            
         SPACE 2                                                                
NM2      MVC   0(50,R3),0(R2)                                                   
         MVI   0(R2),C' '                                                       
         MVC   1(49,R2),0(R2)                                                   
         LA    R2,58(R2)                                                        
         LA    R3,50(R3)                                                        
         BCT   R4,NM2                                                           
         EJECT                                                                  
*                  ROUTINE TO EXTRACT NUMBERS                                   
         SPACE 3                                                                
         XC    THISA(12),THISA     PRESET TO ZERO                               
         LA    R2,NIMAH                                                         
         LA    R3,1                                                             
         LA    R4,THISA                                                         
         LA    R6,3                                                             
         SPACE 2                                                                
NUMLOOP  TM    4(R2),X'10'                                                      
         BC    1,EXITS                                                          
         CLI   5(R2),0                                                          
         BE    NM4                                                              
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,R0                                                            
         EX    R5,VARPAK                                                        
         CVB   R5,DUB                                                           
         ST    R5,0(R4)                                                         
         SPACE 2                                                                
NM4      LA    R2,23(R2)                                                        
         LA    R4,4(R4)                                                         
         BCT   R6,NUMLOOP                                                       
         LA    R2,NIMAH                                                         
         B     NM6                                                              
         SPACE 2                                                                
VARPAK   PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
*                  GAME STARTING ROUTINES                                       
         SPACE 3                                                                
NM6      CLI   FIRST,C'Y'                                                       
         BNE   NOTFIRST                                                         
         OC    THISA(12),THISA                                                  
         BNZ   YOURSTRT                                                         
         MVI   TURN,C'M'                                                        
         MVI   FIRST,C'N'                                                       
         LA    R3,8                                                             
         SPACE 2                                                                
NM8      BAS   RE,RANDOM           COMPUTER STARTING                            
         BAS   RE,STRATEGY                                                      
         BAS   RE,TRIVIAL                                                       
         CLI   SWITCH,C'Y'                                                      
         BE    NM8                                                              
         MVC   NIMSUB(30),LINETWO+30                                            
         MVC   LASTA(12),THISA                                                  
         B     STORE                                                            
         SPACE 2                                                                
YOURSTRT BAS   RE,TRIVIAL          PLAYER STARTING                              
         LA    R3,2                                                             
         CLI   SWITCH,C'Y'                                                      
         BE    EXITS                                                            
         BAS   RE,CANIWIN                                                       
         LA    R3,3                                                             
         CLI   SWITCH,C'Y'                                                      
         BE    EXITS                                                            
         MVI   FIRST,C'N'                                                       
         B     MYPLAY                                                           
         EJECT                                                                  
*                  GAME FLOW ROUTINES                                           
         SPACE 2                                                                
NOTFIRST LA    R3,5                CHECK FOR CHEATING                           
         LA    R4,THISA                                                         
         LA    R5,LASTA                                                         
         LA    R6,3                                                             
         SR    R7,R7                                                            
         SPACE 2                                                                
CHEATLP  CLC   0(4,R4),0(R5)                                                    
         BH    RESET               INCREASING A PILE                            
         BE    *+8                                                              
         LA    R7,1(R7)            COUNT DECREASED PILES                        
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,CHEATLP                                                       
         LA    R3,4                                                             
         CH    R7,=H'1'                                                         
         BH    RESET               MORE THAN 1                                  
         LA    R3,9                                                             
         LTR   R7,R7                                                            
         BZ    RESET               NONE                                         
         SPACE 2                                                                
         BAS   RE,TRIVIAL          CHECK IF HE HAS WON                          
         CLI   SWITCH,C'Y'                                                      
         BNE   NM10                                                             
         LA    R3,6                HE HAS - BREAK THE NEWS                      
         L     R4,SCOREA                                                        
         LA    R4,1(R4)                                                         
         ST    R4,SCOREA                                                        
         B     STARTNEW                                                         
         SPACE 2                                                                
NM10     BAS   RE,CANIWIN          HOW AM I DOING                               
         CLI   SWITCH,C'Y'                                                      
         BNE   MYPLAY                                                           
         LA    R3,7                                                             
         L     R4,SCOREB                                                        
         LA    R4,1(R4)                                                         
         ST    R4,SCOREB                                                        
         EJECT                                                                  
*                  ROUTINE TO START ANOTHER GAME                                
         SPACE 3                                                                
STARTNEW CLI   TURN,C'Y'           MY TURN TO START                             
         BNE   NM12                                                             
         MVI   TURN,C'M'                                                        
         B     NM8                                                              
         SPACE 2                                                                
NM12     MVI   TURN,C'Y'           HIS TURN TO START                            
         MVI   FIRST,C'Y'                                                       
         MVC   NIMSUB(30),LINETWO                                               
         XC    THISA(12),THISA                                                  
         B     STORE                                                            
         EJECT                                                                  
*                  MY PLAYING ROUTINES                                          
         SPACE 3                                                                
MYPLAY   MVC   SAVNUMS(12),THISA                                                
         BAS   RE,STRATEGY                                                      
         LA    R3,8                                                             
         CLC   SAVNUMS(12),THISA                                                
         BNE   STORE               OK HE IS NOT AT STRATEGY POINT               
         CLC   THISA,THISB         FIND HIGHEST                                 
         BL    BORC                                                             
         CLC   THISA,THISC                                                      
         BL    ITSC                                                             
         LA    R4,THISA                                                         
         B     LOWER                                                            
         SPACE 2                                                                
BORC     CLC   THISB,THISC                                                      
         BL    ITSC                                                             
         LA    R4,THISB                                                         
         B     LOWER                                                            
         SPACE 2                                                                
ITSC     LA    R4,THISC                                                         
         SPACE 2                                                                
LOWER    L     R5,0(R4)            AND REDUCE BY ONE                            
         BCTR  R5,R0                                                            
         ST    R5,0(R4)                                                         
         B     STORE                                                            
         EJECT                                                                  
*                  TRIVIAL TEST                                                 
         SPACE 3                                                                
TRIVIAL  NTR                                                                    
         MVI   SWITCH,C'N'                                                      
         LM    R2,R4,THISA         CHECK WHETHER ONE IS ZERO                    
         LTR   R2,R2               AND THE OTHERS ARE EQUAL                     
         BNZ   TR2                                                              
         CR    R3,R4                                                            
         BE    TRYES                                                            
         B     TREXT                                                            
         SPACE 2                                                                
TR2      LTR   R3,R3                                                            
         BNZ   TR4                                                              
         CR    R2,R4                                                            
         BE    TRYES                                                            
         B     TREXT                                                            
         SPACE 2                                                                
TR4      LTR   R4,R4                                                            
         BNZ   TREXT                                                            
         CR    R2,R3                                                            
         BNE   TREXT                                                            
         SPACE 2                                                                
TRYES    MVI   SWITCH,C'Y'                                                      
         SPACE 2                                                                
TREXT    XIT                                                                    
         EJECT                                                                  
*                  CAN I WIN CHECK                                              
         SPACE 3                                                                
CANIWIN  NTR                                                                    
         MVI   SWITCH,C'N'                                                      
         LM    R2,R4,THISA         CHECK IF ANY TWO ARE ZERO                    
         LTR   R2,R2                                                            
         BNZ   CA2                                                              
         LTR   R3,R3               2 AND 3                                      
         BZ    CAYES                                                            
         LTR   R4,R4               2 AND 4                                      
         BZ    CAYES                                                            
         B     CAEXT                                                            
         SPACE 2                                                                
CA2      LTR   R3,R3               3 AND 4                                      
         BNZ   CAEXT                                                            
         LTR   R4,R4                                                            
         BNZ   CAEXT                                                            
         SPACE 2                                                                
CAYES    MVI   SWITCH,C'Y'                                                      
         SPACE 2                                                                
CAEXT    XIT                                                                    
         EJECT                                                                  
*                  EDITING PILE NUMBERS                                         
         SPACE 3                                                                
EDIT     NTR                                                                    
         LA    R2,THISA                                                         
         LA    R3,NIMA                                                          
         LA    R4,3                                                             
         SPACE 3                                                                
EDLOOP   L     R5,0(R2)                                                         
         CVD   R5,DUB                                                           
         UNPK  0(6,R3),DUB                                                      
         OI    5(R3),X'F0'                                                      
         LA    R6,6                                                             
         SPACE 2                                                                
EDINLOOP CLI   0(R3),X'F0'         LEFT ALIGN LOOP                              
         BNE   ED2                                                              
         MVC   0(5,R3),1(R3)                                                    
         MVI   5(R3),0                                                          
         BCT   R6,EDINLOOP                                                      
         SPACE 2                                                                
ED2      LA    R2,4(R2)                                                         
         LR    R5,R3                                                            
         SH    R5,=H'8'                                                         
         STC   R6,5(R5)            SET INPUT LENGTH = OUTPUT                    
         STC   R6,7(R5)                                                         
         LA    R3,23(R3)                                                        
         BCT   R4,EDLOOP                                                        
         FOUT  NIMAH                                                            
         FOUT  NIMBH                                                            
         FOUT  NIMCH                                                            
         XIT                                                                    
         EJECT                                                                  
*                  EDIT THE SCORES                                              
         SPACE 3                                                                
SCORES   NTR                                                                    
         MVC   NIMHEAD+35(12),=CL12'PLAYER'                                     
         MVC   NIMSUB+35(8),=C'COMPUTER'                                        
         L     R2,SCOREA                                                        
         CVD   R2,DUB                                                           
         UNPK  NIMHEAD+48(2),DUB                                                
         OI    NIMHEAD+49,X'F0'                                                 
         CLI   NIMHEAD+48,C'0'                                                  
         BNE   *+8                                                              
         MVI   NIMHEAD+48,C' '                                                  
         SPACE 2                                                                
         L     R2,SCOREB                                                        
         CVD   R2,DUB                                                           
         UNPK  NIMSUB+48(2),DUB                                                 
         OI    NIMSUB+49,X'F0'                                                  
         CLI   NIMSUB+48,X'F0'                                                  
         BNE   *+8                                                              
         MVI   NIMSUB+48,C' '                                                   
         XIT                                                                    
         EJECT                                                                  
*                  RANDOMIZE THREE NUMBERS                                      
         SPACE 3                                                                
RANDOM   NTR                                                                    
         TBIN  MILLI                                                            
         LR    R0,R1                                                            
         MR    R0,R0               SQUARE                                       
         CVD   R1,DUB                                                           
         UNPK  DUB2,DUB                                                         
         PACK  DUB,DUB2(2)         AND EXTRACT                                  
         CVB   R2,DUB                                                           
         PACK  DUB,DUB2+2(2)                                                    
         CVB   R3,DUB                                                           
         PACK  DUB,DUB2+4(2)                                                    
         CVB   R4,DUB                                                           
         LTR   R2,R2               CONVERT ZEROS TO HUNDREDS                    
         BNZ   *+8                                                              
         LA    R2,100                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,100                                                           
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,100                                                           
         STM   R2,R4,THISA                                                      
         XIT                                                                    
         EJECT                                                                  
*                  CONVERT THREE NUMBERS TO STRATEGIC                           
         SPACE 3                                                                
STRATEGY NTR                                                                    
         MVI   SWITCH,C'N'                                                      
         LM    R2,R4,THISA         LOAD INTO 1,3,5                              
         LR    R1,R2                                                            
         LR    R5,R4                                                            
         LA    R6,1                                                             
         SLL   R6,31                                                            
         ST    R6,DUB              INITIALIZE FULL WORD WITH LEFT BIT           
         LA    R8,32                                                            
         SPACE 2                                                                
ST2      SR    R0,R0                                                            
         SR    R2,R2                                                            
         SR    R4,R4                                                            
         SLDL  R0,1                                                             
         SLDL  R2,1                                                             
         SLDL  R4,1                                                             
         LA    R7,0(R2,R4)                                                      
         AR    R7,R0                                                            
         LTR   R7,R7                                                            
         BZ    ST6                                                              
         CH    R7,=H'2'                                                         
         BE    ST6                                                              
         CLI   SWITCH,C'Y'         ONE BIT ON - ESTABLISH WHICH                 
         BE    ST4                                                              
         MVI   SWITCH,C'Y'                                                      
         LA    R6,THISA                        (IF 3 USE A)                     
         LTR   R0,R0                                                            
         BNZ   ST4                                                              
         LA    R6,THISB                                                         
         LTR   R2,R2                                                            
         BNZ   ST4                                                              
         LA    R6,THISC                                                         
         SPACE 2                                                                
ST4      XC    0(4,R6),DUB         TURN BIT ON OR OFF IN KEYWORD                
         SPACE 2                                                                
ST6      L     R0,DUB              MOVE TEST BIT                                
         SRL   R0,1                                                             
         ST    R0,DUB                                                           
         BCT   R8,ST2                                                           
         XIT                                                                    
         EJECT                                                                  
*                  EXPANDING NUMBERS TO CROSSES                                 
         SPACE 3                                                                
EXPAND   NTR                                                                    
         LA    R2,THISA                                                         
         LA    R3,NIMPILE+9                                                     
         BAS   RE,EX2                                                           
         LA    R2,THISB                                                         
         LA    R3,NIMPILE+29                                                    
         BAS   RE,EX2                                                           
         LA    R2,THISC                                                         
         LA    R3,NIMPILE+49                                                    
         BAS   RE,EX2                                                           
         XIT                                                                    
         SPACE 2                                                                
EX2      NTR                                                                    
         LA    R4,100                                                           
         LA    R5,10               SET UP FOR 10 X 10 ROUTINE                   
         SPACE 2                                                                
EX4      LA    R6,10                                                            
         SPACE 2                                                                
EX6      C     R4,0(R2)                                                         
         BH    *+8                                                              
         MVI   0(R3),C'X'                                                       
         BCTR  R4,R0                                                            
         BCTR  R3,R0                                                            
         BCT   R6,EX6                                                           
         LA    R3,68(R3)           DROP DOWN TO NEXT LINE                       
         BCT   R5,EX4              HANDLE TEN LINES                             
         XIT                                                                    
         EJECT                                                                  
*                  FINAL ROUTINES                                               
         SPACE 3                                                                
RESET    MVC   THISA(12),LASTA                                                  
         B     EXITS                                                            
         SPACE 2                                                                
STORE    MVC   LASTA(12),THISA                                                  
         SPACE 2                                                                
EXITS    OI    6(R2),X'40'                                                      
         BAS   RE,EDIT                                                          
         BAS   RE,EXPAND                                                        
         BCTR  R3,R0                                                            
         MH    R3,=H'30'                                                        
         LA    R3,ERRLIST(R3)                                                   
         MVC   NIMHEAD(30),0(R3)                                                
         BAS   RE,SCORES                                                        
         LA    R2,NIMHEADH         CHECK WHICH LINES HAVE CHANGED               
         LA    R3,SAVLINES                                                      
         LA    R4,12                                                            
         SPACE 2                                                                
TRANSLUP CLC   8(50,R2),0(R3)                                                   
         BE    *+8                                                              
         OI    6(R2),X'80'         TRANSMIT CHANGES                             
         LA    R2,58(R2)                                                        
         LA    R3,50(R3)                                                        
         BCT   R4,TRANSLUP                                                      
         XMOD1 1                                                                
         EJECT                                                                  
*                  ERROR MESSAGES                                               
         SPACE 3                                                                
ERRLIST  DC    CL30'NOT A VALID NUMERIC PILE'          1                        
         DC    CL30'THAT''S UNSPORTING - TRY AGAIN'    2                        
         DC    CL30'THAT''S TOO EASY - TRY AGAIN'      3                        
         DC    CL30'YOU CAN ONLY TAKE FROM 1 PILE'     4                        
         DC    CL30'YOU MUST NOT ADD TO A PILE'        5                        
         DC    CL30'YOU HAVE WON THIS GAME'            6                        
         DC    CL30'THE COMPUTER HAS WON THIS GAME'    7                        
         DC    CL30'YOUR TURN TO PLAY'                 8                        
         DC    CL30'YOU MUST CHANGE AT LEAST ONE'      9                        
         SPACE 3                                                                
LINETWO  DC    CL30'YOUR TURN TO START'                1                        
         DC    CL30'COMPUTER HAS STARTED NEW GAME'     2                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                  DSECT FOR THIS PROGRAM                                       
         SPACE 3                                                                
NIMD     DSECT                                                                  
DUB      DS    D                                                                
THISA    DS    F                                                                
THISB    DS    F                                                                
THISC    DS    F                                                                
SAVLINES DS    600C                                                             
SWITCH   DS    CL1                                                              
SAVNUMS  DS    CL12                                                             
DUB2     DS    D                                                                
         EJECT                                                                  
GANIMFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE GANIMFFD                                                       
         EJECT                                                                  
GAMTWA   DSECT                                                                  
         DS    CL16                                                             
PLAYER   DS    CL12                                                             
SCOREA   DS    CL4                                                              
SCOREB   DS    CL4                                                              
LASTA    DS    CL4                                                              
LASTB    DS    CL4                                                              
LASTC    DS    CL4                                                              
FIRST    DS    CL1                                                              
TURN     DS    CL1                                                              
SPARE    DS    CL12                                                             
         DS    CL2                                                              
         SPACE 2                                                                
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GANIM00   05/01/02'                                      
         END                                                                    
         EJECT                                                                  
