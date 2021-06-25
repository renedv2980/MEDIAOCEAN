*          DATA SET GASCR00    AT LEVEL 057 AS OF 08/22/00                      
*PHASE TB1C00A                                                                  
*INCLUDE RANDOM                                                                 
*INCLUDE BINSRCH2                                                               
         TITLE 'GAME - SCRAMBLE'                                                
SCRAMBL  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SCRMWRKX-SCRMWRK,GASCR00,RR=R2                                   
         USING SCRMWRK,RC         WORKSPACE BASE ADDRESS                        
*                                                                               
         L     RA,4(R1)           ADDRESS OF TWA                                
         USING TB1CFFD,RA         TWA BASE ADDRESS                              
         ST    R2,RELO            RELOCATION FACTOR                             
*                                                                               
         EJECT                                                                  
*************************************************************                   
*                      MAIN PROGRAM                         *                   
*************************************************************                   
*                                                                               
         BAS   RE,LCHKERR          VALIDATE LEVEL                               
*                                                                               
         CLI   ERRFLAG,C'N'        LEVEL ERROR ?                                
         BNE   OUT                 YES                                          
*                                                                               
         CLI   CONTFLAG,C'Y'       END OF GAME FLAG                             
         BNE   *+12                NO                                           
         BAS   RE,CONTINIT                                                      
         B     OUT                                                              
*                                                                               
         CLI   LVLFLAG,C'N'        LEVEL CHANGE ?                               
         BNE   OUT                 YES                                          
*                                                                               
         BAS   RE,GCHKERR          VALIDATE GUESS                               
*                                                                               
****     CLI   ERRFLAG,C'N'        GUESS ERROR ?                                
***      BNE   OUT                 YES                                          
*                                                                               
***      MVC   SCRHEAD,REGMESS     REGULAR MESSAGE                              
OUT      OI    SCRHEADH+6,X'80'    TRANSMIT FIELD                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*************************************************************                   
*                      NEW GAME                             *                   
*************************************************************                   
NEWGAME  NTR1                                                                   
*                                                                               
         XC    PSCORE,PSCORE       WILL HOLD PLAYER'S SCORE                     
         XC    CSCORE,CSCORE       WILL HOLD COMPUTER SCORE                     
*                                                                               
         XC    SCRCOMP,SCRCOMP     DISPLAY COMPUTER SCORE                       
         OI    SCRCOMPH+6,X'80'    TRANSMIT                                     
*                                                                               
         XC    SCRYOUR,SCRYOUR     DISPLAY PLAYER SCORE                         
         OI    SCRYOURH+6,X'80'    TRANSMIT                                     
*                                                                               
         XC    TABLE(TABLQ),TABLE  FOR BINSRCH XXX                              
         XC    NUMREC,NUMREC       NUMBER OF RECORDS IN TAB(BINSRCH)XXX         
*                                                                               
         BAS   RE,CONTINIT         INITIALIZE THE REST OF THE FIELDS            
*                                                                               
         MVC   PREVLEVL,SCRLEVL    REMEMBER PREV DIFFICULTY LEV                 
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*************************************************************                   
*               INITIALIZE FIELDS                           *                   
*************************************************************                   
*                                                                               
CONTINIT NTR1                                                                   
*                                                                               
         MVI   CONTFLAG,C'N'       END OF GAME FLAG                             
*                                                                               
         XC    SCRANSW,SCRANSW     CLEAR ANSWER FIELD                           
         OI    SCRANSWH+6,X'80'    TRANSMIT                                     
*                                                                               
         XC    SCRANAN,SCRANAN     ANAGRAM FIELD                                
         OI    SCRANANH+6,X'80'    TRANSMIT                                     
*                                                                               
         XC    SCRGUES,SCRGUES     PLAYER'S GUESS                               
         OI    SCRGUESH+6,X'80'    TRANSMIT                                     
         OI    SCRGUESH+6,X'40'    POSITION CURSOR                              
         OI    SCRGUESH+6,X'01'    MODIFY FOR NEXT INPUT                        
*                                                                               
         LA    R1,SCRGUE1H         HEADER OF FIRST GUESS                        
*                                                                               
CLEARFLD ZIC   R0,0(R1)            LENGTH OF DATA                               
         AR    R1,R0               BUMP TO NEXT FIELD                           
*                                                                               
         OI    6(R1),X'80'         TRANSMIT                                     
*                                                                               
         ZIC   R0,0(R1)            L'FIELD                                      
         AH    R1,=H'8'            BUMP TO DATA FIELD                           
*                                                                               
         XC    0(MAXDIF,R1),0(R1)  CLEAR GUESS FIELD                            
*                                                                               
         AR    R1,R0                                                            
         SH    R1,=H'8'            BUMP TO HEADER OF NEXT FIELD                 
*                                                                               
         LA    R0,SCRGUE3H         CONTAINS A(LAST GUESS)                       
         CR    R1,R0               LAST GUESS FIELD ?                           
         BNE   CLEARFLD            NO                                           
*                                                                               
         XC    GUESSNUM,GUESSNUM   COUNTS # OF GUESSES TAKEN                    
*                                                                               
         XC    SAVEDISP,SAVEDISP   HOLDS DISP INTO THE SCREEN                   
*                                                                               
         BAS   RE,GETWORD          GET WORD FROM DICTIONARY                     
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
**********************************************************                      
*               GET WORD FROM DICTIONARY                 *                      
**********************************************************                      
GETWORD  NTR1                                                                   
*                                                                               
RANNUM   GOTO1 =V(RANDOM),DMCB,MAXWORDQ,RR=RELO                                 
         L     R2,4(R1)            RANDOM NUMBER                                
*                                                                               
         STCM  R2,3,NUMBER                                                      
*                                                                               
GETNUM   GOTO1 =V(BINSRCH),DMCB,(X'01',NUMBER),TABLE,NUMREC,2,         X        
               (0,2),MAXWORDQ,RR=RELO                                           
*                                                                               
         MVC   NUMREC,8(R1)        SAVE NUMBER OF RECORDS IN TABLE              
*                                                                               
         OC    0(4,R1),0(R1)       TABLE FULL ?                                 
         BNZ   NEWWORD                                                          
         XC    TABLE(TABLQ),TABLE  HOLDS PREVIOUSLY SELECTED RANDOM #           
         XC    NUMREC,NUMREC       HOLDS NUMBER OF RECORDS IN TABLE             
         B     RANNUM                                                           
*                                                                               
NEWWORD  CLI   0(R1),X'01'         NEW WORD ?                                   
         BNE   RANNUM              NO                                           
*                                                                               
         CLI   SCRLEVL,C'E'        EASY LEVEL?                                  
         BE    GETEASY             YES                                          
*                                                                               
GETDIF   LA    R5,DIFICULT         POINT TO DIFFICULT WORDS IN DIC              
         MH    R2,=Y(MAXDIF)       MAX WORD LEN = 13                            
         AR    R5,R2               BUMP R5 IN DIC BY RANDOM #                   
         B     SCRAMWRD                                                         
*                                                                               
GETEASY  LA    R5,EASY             POINT TO EASY WORDS IN DIC                   
         MH    R2,=Y(MAXEASY)      MAX WORD LEN = 6                             
         AR    R5,R2               BUMP R5 IN DIC BY RANDOM #                   
*                                                                               
SCRAMWRD BAS   RE,GETLEN           GET L'WORD                                   
*                                                                               
         ZIC   R3,LENGTH           L'(WORD FROM DICTIONARY)                     
         BCTR  R3,0                DECREMENT LEN BY 1                           
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SCRANAN(0),0(R5)    MOVE WORD FROM DICTIONARY                    
*                                                                               
         XC    ANSWER,ANSWER       HOLDS ANSWER                                 
         MVC   ANSWER,SCRANAN      MOVE THE WORD                                
*                                                                               
         LA    R2,SCRANAN          POINTS TO THE WORD                           
*                                                                               
         ZIC   R0,LENGTH                                                        
*                                                                               
         BAS   RE,GETANAG                                                       
         BCT   R0,*-4                                                           
*                                                                               
         OI    SCRANANH+6,X'80'    TRANSMIT                                     
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*************************************************************                   
*               CALCULATE LENGTH OF A WORD                  *                   
*************************************************************                   
GETLEN   NTR1                                                                   
         LA    R0,MAXDIF           MAX WORD LEN                                 
         SR    R3,R3               LETTERS COUNTER                              
*                                                                               
LEN      CLI   0(R5),C' '          END OF WORD?                                 
         BE    GETLENX             YES                                          
         LA    R3,1(R3)            INCREMENT LETTER COUNTER                     
         LA    R5,1(R5)            BUMP TO NEXT LETTER                          
         BCT   R0,LEN                                                           
*                                                                               
GETLENX  STC   R3,LENGTH                                                        
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*************************************************************                   
*                     SCRAMBLE                              *                   
*************************************************************                   
GETANAG  NTR1                                                                   
*                                                                               
         ZIC   R0,LENGTH           L'(WORD)                                     
         BCTR  R0,0                DECREMENT LEN BY 1                           
*                                                                               
SORTL    CLC   0(1,R2),1(R2)       COMPARE TWO LETTERS                          
         BNH   NEXTALPH                                                         
*                                                                               
         XC    0(1,R2),1(R2)       SWAP LETTERS                                 
         XC    1(1,R2),0(R2)                                                    
         XC    0(1,R2),1(R2)                                                    
*                                                                               
NEXTALPH LA    R2,1(R2)            BUMP TO NEXT LETTER                          
         BCT   R0,SORTL                                                         
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*************************************************************                   
*                  VALIDATE THE LEVEL                       *                   
*************************************************************                   
LCHKERR  NTR1                                                                   
*                                                                               
         MVI   ERRFLAG,C'N'        ERROR FLAG                                   
         MVI   LVLFLAG,C'N'        LEVEL CHANGE FLAG                            
*                                                                               
         MVC   SCRHEAD,REGMESS     MOVE REGULAR MESSAGE                         
*                                                                               
         CLI   SCRLEVL,C'E'        EASY LEVEL ?                                 
         BE    LVLCOMP             YES                                          
*                                                                               
         CLI   SCRLEVL,C'D'        DIFFICULT LEVEL ?                            
         BE    LVLCOMP             YES                                          
*                                                                               
         MVC   SCRHEAD,BADLEVEL    PRINT BAD LEVEL MESSAGE                      
         MVI   ERRFLAG,C'Y'                                                     
         OI    SCRLEVLH+6,X'40'    CURSOR POSITION                              
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
LVLCOMP  CLC   PREVLEVL,SCRLEVL    PREV LEVEL=CURRENT LEVEL ?                   
         BE    DONE                YES                                          
*                                                                               
         XC    SCRANSW,SCRANSW     CLEAR ANSWER FIELD                           
         OI    SCRANSWH+6,X'80'    TRANSMIT                                     
*                                                                               
         BAS   RE,NEWGAME                                                       
*                                                                               
         MVI   LVLFLAG,C'Y'        FLIP LEVEL FLAG                              
*                                                                               
DONE     B     EXIT                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*************************************************************                   
*                  VALIDATE THE GUESS                       *                   
*************************************************************                   
GCHKERR  NTR1                                                                   
         MVI   ERRFLAG,C'N'                                                     
*                                                                               
         CLC   SCRGUES(6),=C'GIVEUP' GIVE UP ?                                  
         BE    COMPUTES              YES                                        
*                                                                               
         CLI   SCRGUESH+5,0           GUESS NULL?                               
         BE    NOLEN                  YES                                       
*                                                                               
         CLC   SCRGUESH+5(1),LENGTH   VALID INPUT LENGTH?                       
         BNE   LENERROR               NO                                        
*                                                                               
         ZIC   R3,SCRGUESH+5       L'GUESS                                      
         BCTR  R3,0                DECREMENT BY 1                               
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   SCRGUES(0),ANSWER   ANSWER ?                                     
         BE    GOODANSW            YES                                          
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SORTGUE(0),SCRGUES  GUESSED WORD                                 
*                                                                               
         LA    R2,SORTGUE          WILL HOLD GUESS WORD (SORTED)                
         ZIC   R0,LENGTH                                                        
*                                                                               
         BAS   RE,GETANAG          SORT GUESSED WORD                            
         BCT   R0,*-4                                                           
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TEMPANA(0),SCRANAN  SORTED ANAGRAM                               
*                                                                               
         ZIC   R0,LENGTH           LEN OF A GUESS                               
         LA    R4,SORTGUE          SORTED GUESS                                 
*                                                                               
         LA    R7,TEMPANA          ANAGRAM                                      
         LR    R1,R0               LEN OF AN ANAGRAM                            
*                                                                               
COMPX    MVI   BLNKFLAG,C'N'       BLANK FLAG                                   
         CLC   0(1,R4),0(R7)       GUESS LETTER = ANAGRAM LETTER ?              
         BE    MOVEBLNK            YES                                          
*                                                                               
         LA    R7,1(R7)            BUMP TO NEXT CHAR IN ANAGRAM                 
         BCTR  R1,0                DECREMENT ANGRAM COUNTER BY1                 
         LTR   R1,R1               END OF ANAGRAM ?                             
         BNZ   COMPX               NO                                           
*                                                                               
         B     LOOP                                                             
*                                                                               
MOVEBLNK MVI   0(R7),C' '          MOVE A BLANK TO ANAGRAM                      
         MVI   BLNKFLAG,C'Y'       FLIP BLANK FLAG                              
         LA    R4,1(R4)            BUMP TO NEXT CHAR IN GUESS                   
*                                                                               
LOOP     CLI   BLNKFLAG,C'Y'       WAS THE BLANK MOVED ?                        
         BNE   CHARERR             NO                                           
*                                                                               
         BCT   R0,COMPX                                                         
*                                                                               
         ZIC   R2,GUESSNUM         COUNTS # OF GUESSES                          
         LA    R2,1(R2)            INCREMENT NUM OF GUESSES                     
         STC   R2,GUESSNUM                                                      
*                                                                               
         CLI   GUESSNUM,1          1ST GUESS ?                                  
         BNE   *+12                NO                                           
*                                                                               
         LA    R1,SCRGUE1H         HEADER OF FIRST GUESS                        
         B     *+12                                                             
*                                                                               
         L     R1,SAVEDISP         DISPLACEMENT FROM TOP OF TWA                 
         AR    R1,RA                                                            
*                                                                               
         ZIC   R0,0(R1)            LENGTH OF DATA                               
         AR    R1,R0               BUMP TO NEXT FIELD                           
*                                                                               
         OI    6(R1),X'80'         TRANSMIT                                     
*                                                                               
         ZIC   R0,0(R1)            L'FIELD                                      
         AH    R1,=H'8'            BUMP TO DATA FIELD                           
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SCRGUES     MOVE GUESS TO GUESS#_ FIELD                  
*                                                                               
         AR    R1,R0                                                            
         SH    R1,=H'8'            BUMP TO HEADER OF NEXT FIELD                 
*                                                                               
         LA    R0,SCRGUE3H                                                      
         CR    R1,R0               LAST GUESS?                                  
         BE    COMPUTES                                                         
*                                                                               
         SR    R1,RA               DISPLACEMENT FROM TOP OF TWA                 
         BP    *+6                                                              
         DC    H'0'                NEGATIVE DISPLACEMENT                        
         ST    R1,SAVEDISP         SAVES DISP                                   
*                                                                               
         B     POSCUR                                                           
*                                                                               
COMPUTES L     R2,CSCORE           COMPUTER SCORE                               
         LA    R2,1(R2)            INCREMENT COMPUTER SCORE BY 1                
         ST    R2,CSCORE                                                        
*                                                                               
         MVC   SCRHEAD,MYPOINT     MY POINT MESSAGE                             
*                                                                               
         EDIT  (R2),(3,SCRCOMP)    PRINT COMPUTER SCORE                         
         OI    SCRCOMPH+6,X'80'    TRANSMIT                                     
*                                                                               
         B     TRANSANA                                                         
*                                                                               
GOODANSW L     R2,PSCORE           PLAYER'S SCORE                               
         LA    R2,1(R2)            INCREMENT PLAYER'S SCORE BY 1                
         ST    R2,PSCORE                                                        
*                                                                               
         EDIT  (R2),(3,SCRYOUR)    PRINT PLAYER'S SCORE                         
         OI    SCRYOURH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVC   SCRHEAD,YRPOINT     PLAYER'S POINT MESSAGE                       
*                                                                               
TRANSANA MVC   SCRANSW,ANSWER      DISPLAY THE ANSWER                           
         OI    SCRANSWH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   CONTFLAG,C'Y'       END OF GAME FLAG                             
*                                                                               
POSCUR   XC    SCRGUES,SCRGUES     CLEAR GUESS FIELD                            
         OI    SCRGUESH+6,X'80'    TRANSMIT                                     
         OI    SCRGUESH+6,X'40'    POSITION THE CURSOR                          
         OI    SCRGUESH+6,X'01'    MODIFY FOR NEXT INPUT                        
*                                                                               
         B     EXIT                                                             
*                                                                               
NOLEN    MVC   SCRHEAD,REGMESS     REGULAR MESSAGE                              
         B     ERROR                                                            
*                                                                               
LENERROR MVC   SCRHEAD,BADLEN      DISPLAY BAD LEN MESSAGE                      
         B     ERROR                                                            
*                                                                               
CHARERR  MVC   SCRHEAD(L'BADCHAR),BADCHAR                                       
         MVC   SCRHEAD+L'BADCHAR(1),0(R4)                                       
         MVC   SCRHEAD+L'BADCHAR+1(L'BADCHARX),BADCHARX                         
*                                                                               
ERROR    MVI   ERRFLAG,C'Y'        FLIP ERROR FLAG                              
*                                                                               
         OI    SCRGUESH+6,X'40'    CURSOR POSITION                              
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*************************************************************                   
*                        CONSTANTS                          *                   
*************************************************************                   
RELO     DS    F                     RELOCATION CONSTANT                        
REGMESS  DC    CL60'GUESS THE WORD'                                             
BADLEVEL DC    CL60'VALID DIFFICULTY LEVELS ARE E OR D, TRY AGAIN'              
BADCHAR  DC    CL19'INVALID CHARACTER "'                                        
BADCHARX DC    CL25'" DETECTED IN GUESS FIELD'                                  
BADLEN   DC    CL60'YOUR WORD MUST BE THE SAME LENGTH AS THE ANAGRAM'           
MYPOINT  DC    CL60'MY POINT!!! PRESS ENTER TO CONTINUE'                        
YRPOINT  DC    CL60'YOUR POINT!!! PRESS ENTER TO CONTINUE'                      
*                                                                               
MAXDIF   EQU   13                 MAX LEN OF DIFFICULT WORD                     
MAXEASY  EQU   6                  MAX LEN OF EASY WORD                          
*AXWORDQ EQU   100                # OF WORDS IN DICTIONARY                      
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*************************************************************                   
*                        WORKSPACE                          *                   
*************************************************************                   
SCRMWRK  DSECT                                                                  
DMCB     DS    6F                                                               
WORK     DS    CL17               USED FOR EDIT MACRO                           
DUB      DS    D                                                                
SORTGUE  DS    CL12               HOLDS SORTED GUESS WORD                       
TEMPANA  DS    CL12               HOLDS SORTED GUESS WORD                       
ERRFLAG  DS    C                  ERROR FLAG 'Y'/'N'                            
LVLFLAG  DS    C                  LEVEL CHANGE FLAG 'Y'/'N'                     
BLNKFLAG DS    C                  MOVE IN BLANK FLAG                            
NUMBER   DS    XL2                HOLDS RANDOM NUMBER                           
SCRMWRKX EQU   *                                                                
*                                                                               
         EJECT                                                                  
*************************************************************                   
*                           TWA                             *                   
*************************************************************                   
       ++INCLUDE GASCRFFD                                                       
         EJECT                                                                  
*********************                                                           
*  PART OF TWA                                                                  
*********************                                                           
*                                                                               
LENGTH   DS    X                 WORD LENGTH                                    
PREVLEVL DS    C                 REM PREV DIFFICULTY LEVEL                      
GUESSNUM DS    X                 COUNTS # OF GUESSES TAKEN                      
CONTFLAG DS    C                 END OF GAME FLAG 'Y'/'N'                       
CSCORE   DS    F                 COMPUTER SCORE                                 
PSCORE   DS    F                 PLAYER SCORE                                   
SAVEDISP DS    F                 SAVES PLACE OF NEXT GUESS DISPLAY              
ANSWER   DS    CL12              HOLDS ANSWER                                   
NUMREC   DS    A                 NUMBER OF RECORDS IN TABLE                     
TABLE    DS    100XL2            HOLDS OLD RANDOM NUMBERS                       
TABLQ    EQU   *-TABLE           L(TABLE)                                       
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE GASCRDATA                                                      
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057GASCR00   08/22/00'                                      
         END                                                                    
