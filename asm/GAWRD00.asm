*          DATA SET GAWRD00    AT LEVEL 016 AS OF 05/01/02                      
*PHASE TB1400A                                                                  
         TITLE 'TB1400 - GAME WORD POWER'                                       
WORDMAIN CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORDWRKX-WORDWRK,GAWRD00,CLEAR=YES                               
         USING WORDWRK,RC          NMOD1 WORK AREA                              
         L     RA,4(R1)                                                         
         USING TB14FFD,RA          TWA LOCATION                                 
         EJECT                                                                  
         MVC   WRDHEAD,BLANKS      BLANK OUT HEADER FIELD                       
         L     R2,8(R1)            A(COMFACS)                                   
         MVC   VCALLOV,CCALLOV-COMFACSD(R2)   A(CALLOV)                         
         CLI   FIRSTIME,X'00'      SEE IF GAME IS JUST STARTING                 
         BNE   *+12                                                             
         BAS   RE,INITIAL          KEEP TRYING TO GET MODE UNTIL VALID          
         B     NEWSCRN                                                          
*                                                                               
         CLI   STRTGAME,X'00'      SEE IF ORIGINAL WORD HAS BEEN READ           
         BNE   NOTSTRT             BRANCH IF WE HAVE THE ORIGINAL WORD          
*                                                                               
         MVC   WRDSTRN,BLANKS      BLANK OUT THE SCREEN                         
         MVC   WRDHLP0,BLANKS                                                   
         MVC   WRDHLP1,BLANKS                                                   
         MVC   WRDHLP2,BLANKS                                                   
         MVC   WRDHLP3,BLANKS                                                   
*                                                                               
         LA    R2,WRDFLD1H                                                      
         LA    R3,WRDFLDL                                                       
M2       MVC   8(5,R2),BLANKS                                                   
         LA    R2,13(R2)                                                        
         CR    R2,R3                                                            
         BL    M2                                                               
         BAS   RE,GETWORD          KEEP TRYING TO GET WORD UNTIL VALID          
         B     NEWSCRN                                                          
*                                                                               
NOTSTRT  BAS   RE,VERIFY           CHECK FOR VALID INPUT                        
*                                                                               
         LA    R4,WRDFLD1          SEE IF ALL WORDS HAVE BEEN DISPLAYED         
         LH    R5,NUMWORDS                                                      
M4       LA    R3,5                                                             
M6       LR    R2,R3                                                            
         BCTR  R2,0                                                             
         LA    R6,0(R2,R4)                                                      
         CLI   0(R6),WILDCARD      LOOK FOR THE PRESENCE OF A WILDCARD          
         BE    UPDTSCRN                                                         
         BCT   R3,M6                                                            
*                                                                               
         LA    R4,13(R4)           NO WILDCARD, SO GAME IS OVER                 
         BCT   R5,M4                                                            
         MVI   STRTGAME,X'00'                                                   
         CLI   WRDFOUND,C'Y'       SEE IF GAME ENDED BY USER GUESS              
         BNE   *+14                                                             
         MVC   WRDHEAD,CONGRAMS    IF SO, DISPLAY CONGRATULATIONS               
         B     *+10                                                             
         MVC   WRDHEAD,HELLO       DISPLAY INITIAL SCREEN                       
*                                                                               
         MVC   WRDPRMP,PROMPT                                                   
         MVC   WRDHLP0,HLP0                                                     
         MVC   WRDHLP1,HLP1                                                     
         MVC   WRDHLP2,HLP2                                                     
         MVC   WRDHLP3,HLP3                                                     
         MVI   WRDFOUND,C'N'                                                    
*                                                                               
NEWSCRN  OI    WRDINPTH+6,X'40'    INSERT CURSOR                                
         LA    R2,WRDHEADH                                                      
M1       OI    6(R2),X'80'         XMIT ENTIRE SCREEN                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   M1                                                               
         B     EXIT                                                             
*                                                                               
UPDTSCRN OI    WRDINPTH+6,X'40'    TRANSMIT TEXT FIELDS                         
         OI    WRDHEADH+6,X'80'                                                 
         OI    WRDPRMPH+6,X'80'                                                 
         OI    WRDSTRNH+6,X'80'                                                 
         OI    WRDVERSH+6,X'80'                                                 
         OI    WRDHLP0H+6,X'80'                                                 
         OI    WRDHLP1H+6,X'80'                                                 
         OI    WRDHLP2H+6,X'80'                                                 
         OI    WRDHLP3H+6,X'80'                                                 
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
         ZIC   R3,WRDINPTH+5       MAKE SURE THERE IS INPUT                     
         LTR   R3,R3                                                            
         BZ    NOINPUT                                                          
*                                                                               
         LA    R2,WRDINPT                                                       
         CLI   0(R2),C'H'          SEE IF HARD GAME IS REQUESTED                
         BNE   CHECKE                                                           
         MVI   VERSION,C'H'                                                     
         MVC   VERSMS+1(4),=C'HARD'                                             
         B     VERFOUND                                                         
*                                                                               
CHECKE   CLI   0(R2),C'E'          SEE IF EASY GAME IS REQUESTED                
         BNE   BADINPUT                                                         
         MVI   VERSION,C'E'                                                     
         MVC   VERSMS+1(4),=C'EASY'                                             
*                                                                               
VERFOUND MVI   FIRSTIME,X'FF'      SET THE VERSION                              
         MVC   WRDHEAD,BLANKS      DISPLAY INITIAL SCREEN                       
         MVC   WRDINPT,BLANKS                                                   
         MVC   WRDPRMP,PROMPT                                                   
         MVC   WRDHLP0,HLP0                                                     
         MVC   WRDHLP1,HLP1                                                     
         MVC   WRDHLP2,HLP2                                                     
         MVC   WRDHLP3,HLP3                                                     
         MVC   WRDVERS,VERSMS                                                   
         B     EXIT                                                             
*                                                                               
NOINPUT  MVC   WRDHEAD,NOINPMS     ERROR MESSAGES                               
         B     EXIT                                                             
BADINPUT MVC   WRDHEAD,BADINPMS                                                 
         B     EXIT                                                             
         EJECT                                                                  
GETWORD  NTR1                                                                   
*                                                                               
         MVC   WRDHLP0,HLP0        DISPLAY HELP                                 
         MVC   WRDHLP1,HLP1                                                     
         MVC   WRDHLP2,HLP2                                                     
         MVC   WRDHLP3,HLP3                                                     
         ZIC   R3,WRDINPTH+5       CHECK LENGTH OF INPUT STRING                 
         STH   R3,ORIGWDLN         SAVE LENGTH                                  
         LTR   R3,R3                                                            
         BZ    NOWORDG             MAKE SURE THERE WAS INPUT                    
         CLI   WRDINPTH+5,5        AT LEAST 5 LETTERS IN WORD                   
         BL    BADNUMG                                                          
*                                                                               
         LA    R2,WRDINPT          LOCATION OF INPUT FIELD                      
G1       CLI   0(R2),C'A'          MAKE SURE CHARACTERS ARE ALPHABETIC          
         BL    BADCHARG                                                         
         CLI   0(R2),C'Z'                                                       
         BH    BADCHARG                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,G1               LOOK AT NEXT CHARACTER                       
*                                                                               
         BAS   RE,MAKELIST         FIND ALL VALID WORDS                         
         LH    R4,NUMWORDS                                                      
         LTR   R4,R4                                                            
         BZ    NOLISTG                                                          
*                                                                               
         MVI   STRTGAME,X'FF'      TURN OFF ORIGINAL WORD FLAG                  
         MVC   WRDSTRN,WRDINPT     COPY INPUT TO OUTPUT FIELD                   
         MVC   WRDINPT,BLANKS      BLANK OUT INPUT FIELD                        
         MVC   WRDHLP0,HELP0       WRITE HELP MESSAGE                           
         MVC   WRDHLP1,HELP1                                                    
         MVC   WRDHLP2,HELP2                                                    
         MVC   WRDHLP3,HELP3                                                    
         B     EXIT                                                             
*                                                                               
BADCHARG MVC   INVCHRMS(1),0(R2)   DISPLAY ERROR MESSAGES                       
         MVC   WRDHEAD,INVCHRMS                                                 
         B     EXIT                                                             
NOWORDG  MVC   WRDHEAD,NOWORDMS                                                 
         B     EXIT                                                             
BADNUMG  MVC   WRDHEAD,NUMCHRMS                                                 
         B     EXIT                                                             
NOLISTG  MVC   WRDHEAD,NOLISTMS                                                 
         B     EXIT                                                             
         EJECT                                                                  
VERIFY   NTR1                                                                   
*                                                                               
         ZIC   R3,WRDINPTH+5       LENGTH OF INPUT                              
         LTR   R3,R3                                                            
         BZ    NOWORDV             MAKE SURE THERE IS SOME INPUT                
*                                                                               
         LA    R2,WRDINPT                                                       
         CLI   0(R2),WILDCARD      LOOK FOR HELP ("*" OR NUMBER)                
         BNE   *+12                                                             
         MVI   POSCHAR,WILDCARD    IT IS AN ASTERISK                            
         B     V5                                                               
*                                                                               
         CLI   0(R2),C'1'          SEE IF IT'S A NUMBER FROM 1 TO 5             
         BL    GETWRD              MAYBE IT'S A WORD                            
V6       CLI   0(R2),C'5'                                                       
         BH    BADPOSV                                                          
         MVC   POSCHAR,0(R2)       IT IS A NUMBER                               
V5       CLI   WRDINPTH+5,2        MUST BE EXACTLY 3 CHARS IN HELP CMD          
         BNE   BADSYNV                                                          
         CLI   1(R2),WILDCARD      FINAL CHARACTER MUST BE LETTER OR *          
         BNE   *+16                                                             
*                                                                               
         MVI   SRCHCHAR,WILDCARD   IT IS AN ASTERISK                            
         BAS   RE,SHOWHELP                                                      
         B     EXIT                                                             
*                                                                               
         CLI   1(R2),C'A'          SEE IF IT'S A LETTER                         
         BL    BADMSKV                                                          
         CLI   1(R2),C'Z'                                                       
         BH    BADMSKV                                                          
         MVC   SRCHCHAR,1(R2)      IT'S A LETTER -- SYNTAX IS OK                
         CLI   WRDINPTH+5,2        MUST BE EXACTLY 3 CHARS IN HELP CMD          
         BNE   BADSYNV                                                          
         BAS   RE,SHOWHELP                                                      
         B     EXIT                                                             
*                                                                               
GETWRD   CLI   WRDINPTH+5,5        WORD MUST BE EXACTLY FIVE LETTERS            
         BNE   BADNUMV                                                          
V1       CLI   0(R2),C'A'          MAKE SURE CHARACTERS ARE ALPHABETIC          
         BL    BADCHARV                                                         
         CLI   0(R2),C'Z'                                                       
         BH    BADCHARV                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,V1               LOOK AT NEXT CHARACTER                       
         B     WORDOKV             SEE IF LETTERS ARE ALL VALID                 
*                                                                               
BADCHARV MVC   INVCHRMS(1),0(R2)   DISPLAY ERROR MESSAGES                       
         MVC   WRDHEAD,INVCHRMS                                                 
         B     EXIT                                                             
NOWORDV  MVC   WRDHEAD,NOWORDMS                                                 
         B     EXIT                                                             
BADNUMV  MVC   WRDHEAD,NOT5MS                                                   
         B     EXIT                                                             
BADSYNV  MVC   WRDHEAD,BADSYNMS                                                 
         B     EXIT                                                             
BADPOSV  MVC   WRDHEAD,BADPOSMS                                                 
         B     EXIT                                                             
BADMSKV  MVC   WRDHEAD,BADMSKMS                                                 
         B     EXIT                                                             
         EJECT                                                                  
WORDOKV  LA    R8,5                BUBBLE SORT USER'S WORD                      
         L     R2,=F'-1'                                                        
         LA    R3,1                                                             
         LA    R5,SORTEDWD                                                      
         MVC   SORTEDWD,WRDINPT                                                 
*                                                                               
LX2      LR    R4,R8                                                            
         LA    R6,1(R5)                                                         
         BCTR  R4,0                                                             
LX1      CLC   0(1,R5),0(R6)                                                    
         BNH   NOSWTCH                                                          
         XC    0(1,R5),0(R6)                                                    
         XC    0(1,R6),0(R5)                                                    
         XC    0(1,R5),0(R6)                                                    
*                                                                               
NOSWTCH  LA    R6,1(R6)                                                         
         BCT   R4,LX1                                                           
         LA    R5,1(R5)                                                         
         BXH   R8,R2,LX2                                                        
*                                                                               
         LA    R5,SORTEDWD         POINT TO SORTED SPELLING OF WORD             
         LA    R7,SORTEDWD+5       POINT TO END OF SORTED WORD                  
         LA    R6,SRCHWORD         POINT TO ORIGINAL WORD (SORTED)              
         LH    R0,ORIGWDLN         LENGTH OF ORIGINAL WORD                      
*                                                                               
NXT2     CR    R5,R7                                                            
         BE    NOWAY                                                            
         CLC   0(1,R5),0(R6)       SEE IF WORD CAN BE FORMED FROM ORIG.         
         BL    NOWAY               IT CANNOT                                    
         BH    *+14                IT'S POSSIBLE                                
         LA    R5,1(R5)            A LETTER HAS BEEN MATCHED                    
         CR    R5,R7                                                            
         BE    FNDMATCH            WORD IS A POSSIBILITY                        
*                                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,NXT2                                                          
*                                                                               
NOWAY    MVC   NOWAYMS(1),0(R5)    ALL LETTERS ARE NOT IN WORD                  
         MVC   WRDHEAD,NOWAYMS                                                  
         B     EXIT                                                             
*                                                                               
FNDMATCH LA    R3,ALLWORDS         LOOK FOR A MATCH                             
         LH    R0,NUMWORDS                                                      
         LA    R4,WRDFLD1H                                                      
V2       CLC   0(5,R3),WRDINPT                                                  
         BE    FOUNDIT                                                          
         LA    R4,13(R4)                                                        
         LA    R3,5(R3)                                                         
         BCT   R0,V2                                                            
*                                                                               
         CLI   VERSION,C'H'        IF EASY VERSION, SEARCH HARD LIST            
         BE    V7                                                               
         LA    R8,HRDWORDS                                                      
*                                                                               
V10      CLI   0(R8),X'FF'                                                      
         BE    V7                                                               
         CLC   0(5,R8),WRDINPT                                                  
         BE    FOUNDHRD                                                         
         LA    R8,5(R8)                                                         
         B     V10                                                              
*                                                                               
FOUNDHRD MVC   HRDWDMS(5),WRDINPT                                               
         MVC   WRDHEAD,HRDWDMS                                                  
         B     EXIT                                                             
V7       MVC   BADWDMS(5),WRDINPT  WORD IS NOT IN LIST OF ACCEPTABLES           
         MVC   WRDHEAD,BADWDMS                                                  
         B     EXIT                                                             
*                                                                               
FOUNDIT  MVC   8(5,R4),WRDINPT                                                  
         MVC   GOODWDMS(5),WRDINPT WORD IS IN LIST                              
         MVC   WRDHEAD,GOODWDMS                                                 
         MVI   WRDFOUND,C'Y'                                                    
         OI    6(R4),X'80'                                                      
         B     EXIT                                                             
         EJECT                                                                  
SHOWHELP NTR1                                                                   
*                                                                               
         CLI   POSCHAR,WILDCARD    SEE IF ALL POSITIONS ARE TO BE SHOWN         
         BNE   ONEPOSN                                                          
         CLI   SRCHCHAR,WILDCARD   SEE IF ALL LETTERS ARE TO BE SHOWN           
         BNE   ONELETTR                                                         
*                                                                               
         LA    R3,ALLWORDS         GAME IS OVER -- SHOW ALL WORDS               
         LH    R0,NUMWORDS                                                      
         LA    R4,WRDFLD1H                                                      
S1       MVC   8(5,R4),0(R3)                                                    
         OI    6(R4),X'80'                                                      
         LA    R4,13(R4)                                                        
         LA    R3,5(R3)                                                         
         BCT   R0,S1                                                            
         B     EXIT                                                             
*                                                                               
ONEPOSN  PACK  POSNUMD,POSCHAR     THE POSITION NUMBER FOR HELP                 
         CVB   R1,POSNUMD                                                       
         BCTR  R1,0                                                             
         LA    R3,ALLWORDS         SET UP REGISTERS FOR THE POSITION            
         LH    R0,NUMWORDS                                                      
         LA    R4,WRDFLD1H                                                      
         LA    R3,0(R1,R3)                                                      
         LA    R6,0(R1,R4)                                                      
         CLI   SRCHCHAR,WILDCARD   SEE WHICH LETTER(S) WILL BE SHOWN            
         BNE   ONEPONEL                                                         
*                                                                               
S2       MVC   8(1,R6),0(R3)       ALL LETTERS                                  
         OI    6(R4),X'80'                                                      
         LA    R4,13(R4)                                                        
         LA    R6,13(R6)                                                        
         LA    R3,5(R3)                                                         
         BCT   R0,S2                                                            
         B     EXIT                                                             
*                                                                               
ONEPONEL CLC   0(1,R3),SRCHCHAR    LOOK FOR THE LETTER                          
         BNE   *+14                                                             
         MVC   8(1,R6),0(R3)                                                    
         OI    6(R4),X'80'                                                      
*                                                                               
         LA    R4,13(R4)                                                        
         LA    R6,13(R6)                                                        
         LA    R3,5(R3)                                                         
         BCT   R0,ONEPONEL                                                      
         B     EXIT                                                             
*                                                                               
ONELETTR LA    R3,ALLWORDS         ONE LETTER IN ANY POSITION                   
         LA    R4,WRDFLD1H                                                      
         LH    R0,NUMWORDS                                                      
S4       LA    R1,5                                                             
S6       LR    R2,R1                                                            
         BCTR  R2,0                                                             
         LA    R5,0(R2,R3)                                                      
         LA    R6,0(R2,R4)                                                      
         CLC   0(1,R5),SRCHCHAR                                                 
         BNE   S7                                                               
         MVC   8(1,R6),0(R5)                                                    
         OI    6(R4),X'80'                                                      
S7       BCT   R1,S6                                                            
         LA    R4,13(R4)                                                        
         LA    R3,5(R3)                                                         
         BCT   R0,S4                                                            
         B     EXIT                                                             
         EJECT                                                                  
MAKELIST NTR1                                                                   
*                                                                               
         SR    R9,R9               COUNT THE NUMBER OF WORDS                    
         ZIC   R1,WRDINPTH+5       LENGTH OF WORD                               
         MVC   SRCHWORD,WRDINPT                                                 
         L     R2,=F'-1'           SET UP REGISTERS FOR SORT                    
         LA    R3,1                                                             
         LA    R5,SRCHWORD                                                      
L2       LR    R4,R1                                                            
         LA    R6,1(R5)                                                         
         BCTR  R4,0                                                             
L1       CLC   0(1,R5),0(R6)       BUBBLE SORT THE WORD                         
         BNH   NOSWITCH                                                         
         XC    0(1,R5),0(R6)                                                    
         XC    0(1,R6),0(R5)                                                    
         XC    0(1,R5),0(R6)                                                    
*                                                                               
NOSWITCH LA    R6,1(R6)                                                         
         BCT   R4,L1                                                            
         LA    R5,1(R5)                                                         
         BXH   R1,R2,L2                                                         
*                                                                               
         LA    R3,ALLWORDS         POINT TO LIST OF VALID WORDS                 
         LA    R8,HRDWORDS         LIST OF HARD WORDS (FOR EASY VRSN)           
         LA    R4,WRDFLD1          POINT TO OUTPUT FIELD OF WORD                
         LA    R2,0                                                             
LDPHASE  LA    R2,1(R2)            EACH SUCCESSIVE PHASE NUMBER                 
         CH    R2,=H'4'            THE TOTAL NUMBER OF PHASES                   
         BH    XIT2                                                             
         GOTO1 VCALLOV,DMCB,((R2),0),0   LOAD EACH PHASE                        
         CLI   4(R1),X'FF'         MAKE SURE PHASE EXISTS                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,0(R1)            POINT TO SORTED SPELLING OF WORD             
NEXT     LA    R6,SRCHWORD         POINT TO ORIGINAL WORD                       
         LA    R7,6(R5)            POINT TO REAL SPELLING                       
         ZIC   R0,WRDINPTH+5       LENGTH OF ORIGINAL WORD                      
         MVC   WORDDIFF,0(R5)      SAVE DIFFICULTY OF WORD                      
         LA    R5,1(R5)            POINT TO BEGINNING OF SORTED WORD            
*                                                                               
NEXT2    CR    R5,R7                                                            
         BE    INVALID                                                          
         CLC   0(1,R5),0(R6)       SEE IF WORD CAN BE FORMED FROM ORIG.         
         BL    INVALID             IT CANNOT                                    
         BH    *+14                IT'S POSSIBLE                                
         LA    R5,1(R5)            A LETTER HAS BEEN MATCHED                    
         CR    R5,R7                                                            
         BE    VALID               A MATCH HAS BEEN FOUND                       
*                                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,NEXT2                                                         
         B     INVALID                                                          
*                                                                               
VALID    CLI   VERSION,C'H'        SEE IF HARD VERSION IS BEING PLAYED          
         BE    CONT2               IF SO, ALL WORDS GO ON SCREEN                
         CLI   WORDDIFF,C'E'       IF NOT, SEE IF WORD IS EASY                  
         BE    CONT2               IF SO, WORD GOES ON SCREEN                   
         MVC   0(5,R8),0(R7)       IF NOT, REMEMBER WORD IN CASE. . .           
         LA    R8,5(R8)               . . .USER GUESSES IT                      
         B     INVALID                                                          
*                                                                               
CONT2    MVC   0(5,R3),0(R7)       PUT WORD IN LIST                             
         LA    R3,5(R3)                                                         
         MVC   0(5,R4),=C'*****'   FILL SCREEN SLOT WITH ASTERISKS              
         LA    R4,13(R4)                                                        
         LA    R9,1(R9)                                                         
         CH    R9,=H'128'          MAXIMUM DISPLAYABLE WORDS                    
         BE    XIT2                                                             
*                                                                               
INVALID  LA    R7,5(R7)                                                         
         CLI   0(R7),X'FF'         LOOK FOR END OF LIST MARKER                  
         BE    LDPHASE                                                          
         LR    R5,R7                                                            
         B     NEXT                                                             
*                                                                               
XIT2     STH   R9,NUMWORDS         SAVE NUMBER OF WORDS ON SCREEN               
         MVI   0(R8),X'FF'         MARK END OF HARD WORD LIST                   
         B     EXIT                                                             
         EJECT                                                                  
WILDCARD EQU   C'*'                                                             
BLANKS   DC    CL80' '                                                          
HELLO    DC    CL78'WELCOME TO WORD POWER!'                                     
PROMPT   DC    CL78'ENTER A WORD'                                               
HLP0     DC    CL78' '                                                          
HLP1     DC    CL78'PLEASE ENTER A WORD OF NO MORE THAN 12 LETTERS.  YO+        
               UR OBJECT IS TO'                                                 
HLP2     DC    CL78'FIND AS MANY FIVE-LETTER WORDS AS YOU CAN WHICH MAY+        
                BE CONSTRUCTED'                                                 
HLP3     DC    CL78'FROM THE ORIGINAL WORD.'                                    
HELP0    DC    CL78'ENTER A FIVE-LETTER WORD.  NO FOREIGN OR SLANG WORD+        
               S ARE ALLOWED.  DO'                                              
HELP1    DC    CL78'NOT ADD "S" TO A FOUR-LETTER WORD.  FOR CLUES, ENTE+        
               R "NC", WHERE N IS'                                              
HELP2    DC    CL78'A POSITION NUMBER, AND C IS A LETTER.  A "*" MAY BE+        
                USED AS A WILDCARD'                                             
HELP3    DC    CL78'FOR EITHER N OR C.  ENTER "**" TO SHOW ALL WORDS AN+        
               D END GAME.'                                                     
NOWORDMS DC    CL60'YOU MUST ENTER A WORD.'                                     
NUMCHRMS DC    CL60'WORD MUST BE AT LEAST 5 LETTERS.  RE-ENTER.'                
INVCHRMS DC    CL60'  IS NOT A LETTER.  RE-ENTER.'                              
NOT5MS   DC    CL60'GUESS MUST BE A FIVE-LETTER WORD.  RE-ENTER.'               
BADWDMS  DC    CL60'      IS NOT ONE OF THE LISTED WORDS.  RE-ENTER.'           
GOODWDMS DC    CL60'      IS IN THE LIST.'                                      
HRDWDMS  DC    CL60'      IS A HARD WORD.  GOOD JOB.'                           
BADSYNMS DC    CL60'BAD HELP REQUEST.  RE-ENTER.'                               
BADPOSMS DC    CL60'POSITION NUMBER MUST BE FROM 1 TO 5.  RE-ENTER.'            
BADMSKMS DC    CL60'CHARACTER MUST BE A LETTER OR "*".  RE-ENTER.'              
NOLISTMS DC    CL60'NO WORDS MAY BE FORMED FROM THIS WORD.  RE-ENTER.'          
NOINPMS  DC    CL60'YOU MUST ENTER A DIFFICULTY LEVEL.'                         
BADINPMS DC    CL60'INVALID SYNTAX.  ENTER E OR H.'                             
NOWAYMS  DC    CL60'  IS NOT IN THE ORIGINAL WORD.  RE-ENTER.'                  
CONGRAMS DC    CL60'CONGRATULATIONS!  YOU HAVE FOUND ALL THE WORDS.'            
VERSMS   DC    CL14'(     VERSION)'                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
WORDWRK  DSECT                                                                  
POSNUMD  DS    D                   DECIMAL POSITION NUMBER FOR HELP             
DMCB     DS    6F                                                               
VCALLOV  DS    A                   A(CALLOV)                                    
SORTEDWD DS    CL5                 SORTED USER INPUT WORD                       
WRDFOUND DS    C                   TRUE IF USER FOUND A WORD                    
POSCHAR  DS    C                   POSITION OF CHARACTER SEARCH                 
SRCHCHAR DS    C                   THE CHARACTER TO BE SEARCHED FOR             
WORDDIFF DS    C                   DIFFICULTY OF WORD                           
WORDWRKX EQU   *                                                                
         SPACE 3                                                                
       ++INCLUDE DDCOMFACS                                                      
         SPACE 3                                                                
       ++INCLUDE GAWRDFFD                                                       
         SPACE 3                                                                
NUMWORDS DS    H                   NUMBER OF WORDS IN LIST                      
ORIGWDLN DS    H                   LENGTH OF ORIGINAL WORD                      
ALLWORDS DS    128CL5              THE WORDS WHICH MUST BE FOUND                
SRCHWORD DS    CL12                THE SORTED ORIGINAL WORD                     
FIRSTIME DS    C                   EQUALS 0 IF NO GAMES PLAYED YET              
STRTGAME DS    C                   EQUALS 0 IF NO ORIGINAL WORD YET             
VERSION  DS    C                   H FOR HARD VERSION, E FOR EASY               
HRDWORDS DS    0C                  *** MUST BE LAST FIELD IN DSECT ***          
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016GAWRD00   05/01/02'                                      
         END                                                                    
