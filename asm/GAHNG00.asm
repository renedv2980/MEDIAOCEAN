*          DATA SET GAHNG00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0500A                                                                  
         TITLE 'HANGMAN GAME'                                                   
         PRINT NOGEN                                                            
HANGCNTL CSECT                                                                  
         NMOD1 100,GAHNG00,RR=R7                                                
         USING HANGD,RC                                                         
         L     RA,4(R1)                                                         
         USING GAHNGFFD,RA                                                      
         EJECT                                                                  
*              FIRST TIME CONTROL                                               
         SPACE 3                                                                
         CLI   FIRSTIME,X'FF'                                                   
         BE    HANG2                                                            
         MVI   FIRSTIME,X'FF'                                                   
         MVC   HNGUSED,SPACES                                                   
         ZAP   YOURSCRE,=P'0'                                                   
         ZAP   COMPSCRE,=P'0'                                                   
         ZAP   LIVES,=P'0'                                                      
         L     R2,8(R1)                                                         
         MVC   CALLOV,4(R2)                                                     
         BAS   RE,GETAWORD                                                      
         FOUT  HNGTHISH,PLAYWORD,25                                             
         B     HANG2                                                            
         SPACE 2                                                                
SPACES   DC    CL48' '                                                          
         EJECT                                                                  
*              CHECK LETTER FOR VALIDITY,MATCH AND HANDLE LOGIC                 
         SPACE 2                                                                
HANG2    LA    R4,HNGLET                                                        
         SR    R5,R5                                                            
         IC    R5,HNGLETH+5                                                     
         LTR   R5,R5                                                            
         BZ    HANG4                                                            
         SPACE 2                                                                
HANG3    CLI   0(R4),C'A'                                                       
         BL    HANG4                                                            
         CLI   0(R4),C'Z'                                                       
         BNH   HANG6                                                            
         SPACE 2                                                                
HANG4    LA    R2,MSGB                                                          
         MVC   MSGB(1),HNGLET                                                   
         B     EXIT                                                             
         SPACE 2                                                                
HANG6    MVC   WORK,PLAYWORD                                                    
         BAS   RE,USED                                                          
         BAS   RE,MATCH                                                         
         CLC   WORK(25),PLAYWORD   DID THE LETTER SCORE                         
         BE    HANG8                                                            
         FOUT  HNGTHISH,PLAYWORD,25                                             
         CLC   PLAYWORD,THISWORD   DID HE COMPLETE THE WORD                     
         BE    HANG7                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,HANG3                                                         
         ZAP   DUB,LIVES                                                        
         CVB   R2,DUB                                                           
         BAS   RE,HANGMAN                                                       
         LA    R2,MSGA                                                          
         B     EXIT                                                             
         SPACE 2                                                                
HANG7    DS    0H                                                               
         LA    R2,MSGC                                                          
         AP    YOURSCRE,=P'1'      YES - THEN HE WON                            
         B     HANG10                                                           
         SPACE 2                                                                
HANG8    AP    LIVES,=P'1'         MISS - LOSES A LIFE                          
         CP    LIVES,=P'6'         HAS HE LOST ALL HIS LIVES                    
         BE    HANG9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,HANG3                                                         
         SPACE 2                                                                
HANG9    DS    0H                                                               
         ZAP   DUB,LIVES                                                        
         CVB   R2,DUB                                                           
         BAS   RE,HANGMAN                                                       
         LA    R2,MSGA                                                          
         CP    LIVES,=P'6'         HAS HE LOST ALL HIS LIVES                    
         BL    EXIT                                                             
         AP    COMPSCRE,=P'1'                                                   
         LA    R2,MSGD                                                          
         SPACE 2                                                                
HANG10   FOUT  HNGLSTH,THISWORD,25                                              
         MVC   HNGUSED,SPACES                                                   
         BAS   RE,GETAWORD                                                      
         ZAP   LIVES,=P'0'                                                      
         FOUT  HNGTHISH,PLAYWORD,25                                             
         EDIT  (P6,YOURSCRE),(6,HNGYOUR),ALIGN=LEFT                             
         EDIT  (P6,COMPSCRE),(6,HNGCOMP),ALIGN=LEFT                             
         FOUT  HNGYOURH                                                         
         FOUT  HNGCOMPH                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO MATCH LETTER AGAINST WORD                             
         SPACE 3                                                                
MATCH    NTR1                                                                   
         LA    R2,THISWORD                                                      
         LA    R3,PLAYWORD                                                      
         LA    R6,25                                                            
         SPACE 2                                                                
MATCH2   CLC   0(1,R2),0(R4)                                                    
         BNE   *+10                                                             
         MVC   0(1,R3),0(R4)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R6,MATCH2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SHOW USED LETTERS                                     
         SPACE 3                                                                
USED     NTR1                                                                   
         LA    R2,HNGUSED                                                       
         SPACE 2                                                                
USED2    CLI   0(R2),C' '                                                       
         BE    USED4                                                            
         CLI   0(R2),0                                                          
         BE    USED4                                                            
         LA    R2,1(R2)                                                         
         B     USED2                                                            
         SPACE 2                                                                
USED4    MVC   0(1,R2),0(R4)                                                    
         B     XIT                                                              
         EJECT                                                                  
*              GET A WORD FOR A NEW GAME                                        
         SPACE 3                                                                
GETAWORD NTR1                                                                   
         TBIN  MILLI                                                            
         LR    R4,R1               SAVE TIME IN MILLI SECS                      
         LR    R0,R1                                                            
         O     R0,=F'4'                                                         
         MR    R0,R0                                                            
         ALR   R1,R0                                                            
         SR    R2,R2               R2&R3=RANDOM NUMBER                          
         LR    R3,R1                                                            
         D     R2,=F'4'                                                         
         LA    R2,1(R2)            R2=PHASE NUMBER 1 THRU 4                     
         SPACE 2                                                                
GETOVLY  GOTO1 CALLOV,DMCB,((R2),0),0                                           
         CLI   DMCB+4,X'FF'                                                     
         BNE   GETAW2                                                           
         DC    H'0'                DIE IF BOOK OVERLAY NOT FOUND                
         SPACE 2                                                                
GETAW2   LR    R1,R4               NOW LOOK FOR THE WORD IN A BOOK              
         LR    R0,R1                                                            
         O     R0,=F'500'                                                       
         MR    R0,R0                                                            
         ALR   R1,R0                                                            
         SR    R2,R2               R2&R3=RANDOM NUMBER                          
         LR    R3,R1                                                            
         D     R2,=F'500'                                                       
         LA    R2,1(R2)            R2=WORD NUMBER 1 THRU 500                    
         L     R3,DMCB                                                          
         SPACE 2                                                                
GETAW4   LR    R4,R3               FIND THE LENGTH OF THIS WORD                 
         SR    R5,R5                                                            
         SPACE 2                                                                
GETAW6   CLI   0(R4),C' '          TEST END OF WORD                             
         BE    GETAW8                                                           
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GETAW6                                                           
         SPACE 2                                                                
GETAW8   CH    R2,=H'1'            TEST IF FIRST WORD IN BOOK                   
         BE    GETAW10                                                          
         LA    R3,1(R4)                                                         
         BCTR  R2,0                                                             
         CLC   0(05,R3),=C'BKEND'                                               
         BNE   GETAW4                                                           
         L     R3,DMCB             START AGAIN IF LT 500 WORDS                  
         B     GETAW4                                                           
         SPACE 2                                                                
GETAW10  MVI   THISWORD,C' '                                                    
         MVC   THISWORD+1(L'THISWORD-1),THISWORD                                
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   THISWORD(0),0(R3)                                                
         MVC   PLAYWORD,THISWORD                                                
         LA    R2,PLAYWORD                                                      
         LA    R3,25                                                            
         SPACE 2                                                                
GETAW12  CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
         BCT   R3,GETAW12                                                       
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO BUILD HANGMAN                                         
         SPACE 3                                                                
HANGMAN  NTR1                                                                   
         MVI   HANGAREA,C' '                                                    
         MVC   HANGAREA+1(L'HANGAREA-1),HANGAREA                                
         SLL   R2,2                                                             
         LA    R2,DELIMITS(R2)                                                  
         L     R3,0(R2)            A(END OF PATTERN)                            
         AR    R3,R7               RELOCATE                                     
         LA    R2,HEAD                                                          
         SPACE 2                                                                
HANGM2   CR    R3,R2                                                            
         BE    HANGM4                                                           
         SR    R4,R4               LINE NUMBER DISPLACEMENT                     
         IC    R4,0(R2)                                                         
         BCTR  R4,0                                                             
         MH    R4,=H'15'                                                        
         SR    R5,R5               COLUMNAR DISPLACEMENT                        
         IC    R5,1(R2)                                                         
         BCTR  R5,0                                                             
         AR    R5,R4                                                            
         LA    R4,HANGAREA(R5)                                                  
         MVI   0(R4),C'O'                                                       
         LA    R2,2(R2)                                                         
         B     HANGM2                                                           
         SPACE 2                                                                
HANGM4   LA    R2,HANGAREA         NOW OUTPUT CHANGED FIELDS TO SCREEN          
         LA    R3,HNGFRSTH                                                      
         LA    R5,14                                                            
         SPACE 2                                                                
HANGM6   CLC   0(15,R2),8(R3)                                                   
         BE    HANGM8                                                           
         FOUT  (R3),(R2),15                                                     
         SPACE 2                                                                
HANGM8   LA    R2,15(R2)                                                        
         SR    R4,R4                                                            
         IC    R4,0(R3)                                                         
         AR    R3,R4                                                            
         IC    R4,0(R3)                                                         
         AR    R3,R4                                                            
         BCT   R5,HANGM6                                                        
         XIT1                                                                   
         EJECT                                                                  
*              HANGMAN PATTERN IN COORDINATE FORM                               
         SPACE 3                                                                
HEAD     DS    0H                                                               
         DC    AL1(1,7)                                                         
         DC    AL1(1,8)                                                         
         DC    AL1(1,9)                                                         
         DC    AL1(2,6)                                                         
         DC    AL1(2,10)                                                        
         DC    AL1(3,6)                                                         
         DC    AL1(3,10)                                                        
         DC    AL1(4,7)                                                         
         DC    AL1(4,8)                                                         
         DC    AL1(4,9)                                                         
         SPACE 1                                                                
BODY     DS    0H                                                               
         DC    AL1(5,7)                                                         
         DC    AL1(5,8)                                                         
         DC    AL1(5,9)                                                         
         DC    AL1(6,6)                                                         
         DC    AL1(6,10)                                                        
         DC    AL1(7,6)                                                         
         DC    AL1(7,10)                                                        
         DC    AL1(8,6)                                                         
         DC    AL1(8,10)                                                        
         DC    AL1(9,6)                                                         
         DC    AL1(9,10)                                                        
         DC    AL1(10,7)                                                        
         DC    AL1(10,8)                                                        
         DC    AL1(10,9)                                                        
         SPACE 1                                                                
ARM1     DS    0H                                                               
         DC    AL1(6,2)                                                         
         DC    AL1(6,3)                                                         
         DC    AL1(6,4)                                                         
         DC    AL1(6,5)                                                         
         SPACE 1                                                                
ARM2     DS    0H                                                               
         DC    AL1(6,11)                                                        
         DC    AL1(6,12)                                                        
         DC    AL1(6,13)                                                        
         DC    AL1(6,14)                                                        
         SPACE 1                                                                
LEG1     DS    0H                                                               
         DC    AL1(11,6)                                                        
         DC    AL1(12,5)                                                        
         DC    AL1(13,4)                                                        
         SPACE 1                                                                
FOOT1    DS    0H                                                               
         DC    AL1(14,1)                                                        
         DC    AL1(14,2)                                                        
         DC    AL1(14,3)                                                        
         SPACE 1                                                                
LEG2     DS    0H                                                               
         DC    AL1(11,10)                                                       
         DC    AL1(12,11)                                                       
         DC    AL1(13,12)                                                       
         SPACE 1                                                                
FOOT2    DS    0H                                                               
         DC    AL1(14,13)                                                       
         DC    AL1(14,14)                                                       
         DC    AL1(14,15)                                                       
         SPACE 1                                                                
DEAD     DS    0H                                                               
         SPACE 2                                                                
DELIMITS DS    0F                                                               
         DC    A(HEAD)                                                          
         DC    A(BODY)                                                          
         DC    A(ARM1)                                                          
         DC    A(ARM2)                                                          
         DC    A(LEG1)                                                          
         DC    A(LEG2)                                                          
         DC    A(DEAD)                                                          
         EJECT                                                                  
*              EXIT MESSAGES AND EXIT                                           
         SPACE 2                                                                
MSGA     DC    CL40'GAME IN PROGRESS - SELECT SOME LETTERS '                    
MSGB     DC    CL40'  IS NOT A VALID LETTER - TRY AGAIN'                        
MSGC     DC    CL40'YOU WON - SELECT LETTERS FOR NEW GAME'                      
MSGD     DC    CL40'YOU DIED - SELECT LETTERS FOR NEW GAME'                     
         SPACE 2                                                                
EXIT     FOUT  HNGHEADH,0(R2),40                                                
         FOUT  HNGUSEDH                                                         
         OI    HNGLETH+6,X'40'                                                  
         XMOD1 1                                                                
         EJECT                                                                  
GAHNGFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE GAHNGFFD                                                       
FIRSTIME DS    CL1                                                              
YOURSCRE DS    PL6                                                              
COMPSCRE DS    PL6                                                              
THISWORD DS    CL25                                                             
PLAYWORD DS    CL25                                                             
LIVES    DS    PL2                                                              
CALLOV   DS    F                                                                
         EJECT                                                                  
*              DSECT FOR HANGMAN                                                
         SPACE 3                                                                
HANGD    DSECT                                                                  
DMCB     DS    6F                                                               
HANGAREA DS    CL210                                                            
DUB      DS    D                                                                
WORK     DS    CL64                                                             
         SPACE 2                                                                
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GAHNG00   05/01/02'                                      
         END                                                                    
