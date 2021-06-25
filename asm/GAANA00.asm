*          DATA SET GAANA00    AT LEVEL 004 AS OF 05/01/02                      
*PHASE TB1500A                                                                  
*INCLUDE RANDOM                                                                 
         TITLE 'ANAGRAM GAME'                                                   
         PRINT NOGEN                                                            
ANA      CSECT                                                                  
         NMOD1 ANATEMPX-ANATEMP,GAANA00,RR=R5                                   
         USING ANATEMP,RC                                                       
         L     RA,4(R1)                                                         
         USING ANASAVE,RA                                                       
******************************************************************              
*CONTROL BLOCK                                                                  
******************************************************************              
         BAS   RE,PREL             PRELIMINARIES                                
*                                                                               
C1       CLI   ANAGO,0             MATCH START ?                                
         BNE   C2                  MATCH IN PROGRESS                            
         BAS   RE,STARTM           PROCESS MATCH START                          
         MVI   ANAGO,1                                                          
         B     EXIT                SHOW FIRST ANAGRAM                           
*                                                                               
C2       CLC   ANAGUS(3),=C'OLD'   IS WORD KNOWN                                
         BNE   C3                  NO                                           
         BAS   RE,OLD              PROCESS NEW WORD                             
         B     EXIT                SHOW NER WORD                                
*                                                                               
C3       CLC   ANAGUS(4),=C'HELP'   IS HELP NEEDED                              
         BNE   C4                  NO                                           
         BAS   RE,HELP             GIVE HELP                                    
         B     EXIT                SHOW FIRST LETTER                            
*                                                                               
C4       CLC   ANAGUS(4),=C'LOSE'  DID PLAYER GIVE IN                           
         BNE   C5                  NO                                           
         BAS   RE,LOSE             PROCESS LOSE                                 
         B     EXIT                NEW GAME SHOW NEW WORD&SCORE                 
*                                                                               
C5       BAS   RE,CHECK            CHECK LETTER AMOUNT                          
         C     RF,=F'0'                                                         
         BNE   C6                  CORRECT                                      
         BAS   RE,ERROR            INCORRECT                                    
         B     EXIT                SHOW INVALID                                 
*                                                                               
C6       SR    R5,R5                                                            
         IC    R5,ATTEMPT          ATTEMPT=NUMBER OF ATTEMPTS                   
         LA    R5,1(R5)                                                         
         STC   R5,ATTEMPT                                                       
         BAS   RE,VALID            VALIDATR INPUT GUESS R1=1-RIGHT,R1=0         
         C     RF,=F'1'                                                         
         BNE   C7                  WRONG ATTEMPT                                
         BAS   RE,WIN              PROCESS WIN                                  
         B     EXIT                NEW GAME SHOW WORD&SCORE                     
*                                                                               
C7       CLC   ATTEMPT(1),MAXGO    IS THERE NEW ATTEMPT                         
         BNE   C8                  YES GAME CONTINIUES                          
         BAS   RE,LOSE             PROCESS LOSE                                 
         B     EXIT                NEW GAME SHOW WORD&SCORE                     
*                                                                               
C8       BAS   RE,NEWGUESS            PROCESS NEW GUESS                         
*                                                                               
EXIT     OI    ANAHDRH+6,X'80'                                                  
         OI    ANAGUSH+6,X'40'                                                  
         XMOD1 1                                                                
*                                                                               
*                                                                               
*                                                                               
*******************************************************************             
*  PRELIMINARIES                                                                
*                                                                               
*******************************************************************             
PREL     DS    0H                                                               
         NTR1                                                                   
         MVC   ACOMFACS,16(R1)     FOR LOAD DICTIONARY                          
         L     RE,ACOMFACS         USING CCALLOV                                
         USING COMFACSD,RE                                                      
         MVC   VCALLOV,CCALLOV                                                  
*                                                                               
         ST    R5,RELO                                                          
         L     RE,=V(RANDOM)       FOR CREATION RANDOM NUMBER                   
         A     RE,RELO             ADD RELOCATION                               
         ST    RE,VRANDOM          USING RANDOM                                 
         XIT1                                                                   
*******************************************************************             
*         MATCH START                                                           
*         FUNCTION   1 SELECTION ANAGRAM                                        
*                    2 PREP DICTIONARY                                          
*         INPUT      1 DICTIONARY                                               
*         OUTPUT     1 ANAGRAM                                                  
*                    2 SCREEN                                                   
*******************************************************************             
STARTM   DS    0H                                                               
         NTR1                                                                   
         BAS   RE,SELECT           SELECTION ANAGRAM                            
         XC    ANAHDR,ANAHDR                                                    
         MVC   ANAHDR(L'MSG1),MSG1 SCREEN PREP                                  
         MVC   ANAGM,ANAGRAM                                                    
         OI    ANAGMH+6,X'80'                                                   
         XIT1                                                                   
*******************************************************************             
*              OLD WORD                                                         
*              FUNCTION 1 REPLACE ANAGRAM                                       
*                       2 PREP SCREEN                                           
*              OUTPUT   1 NEW ANAGRAM                                           
*                       2 SCREEN                                                
*******************************************************************             
OLD      DS    0H                                                               
         NTR1                                                                   
         BAS   RE,CLEAN            CLEANING                                     
         BAS   RE,SELECT           SELECTION NEW ANAGRAM                        
         XC    ANAHDR,ANAHDR                                                    
         MVC   ANAHDR(L'MSG2),MSG2                                              
         MVC   ANAGM,ANAGRAM                                                    
         OI    ANAGMH+6,X'80'                                                   
         XIT1                                                                   
**************************************************************                  
*              HELP                                                             
*              FUNCTION 1 TRANSMISSION FIRST LETTER                             
*              OUTPUT   1 FIRST LETTER                                          
*                                                                               
**************************************************************                  
HELP     DS    0H                                                               
         NTR1                                                                   
         MVC   ANAFLET(1),WORD                                                  
         OI    ANAFLETH+6,X'80'                                                 
         XC    ANAHDR,ANAHDR                                                    
         MVC   ANAHDR(L'MSG3),MSG3                                              
         XIT1                                                                   
*****************************************************************               
*              PLAYER GAVE IN                                                   
*              FUNCTION 1 GIVE ANSWER                                           
*                       2 NEW SCORE                                             
*                       3 CLEAN SCREEN                                          
*                       4 SELECTION ANAGRAM                                     
*              OUTPUT   1 SCREEN NEW SCORE&ANAGRAM                              
*****************************************************************               
LOSE     DS    0H                                                               
         NTR1                                                                   
         MVC   ANAANS+14(15),WORD                                               
         PACK  DUB,ANASCO+9(2)                                                  
         AP    DUB+6(2),=P'1'                                                   
         UNPK  ANASCO+9(2),DUB+6(2)                                             
         OI    ANASCO+10,X'F0'                                                  
         OI    ANAANSH+6,X'80'                                                  
         OI    ANASCOH+6,X'80'                                                  
         XC    ANAHDR,ANAHDR                                                    
         MVC   ANAHDR(L'MSG4),MSG4                                              
         BAS   RE,CLEAN            CLEAN SCREEN                                 
         BAS   RE,SELECT           SELECTION NEW ANAGRAM                        
         MVC   ANAGM,ANAGRAM                                                    
         OI    ANAGMH+6,X'80'                                                   
         XIT1                                                                   
******************************************************************              
*              CHECK                                                            
*              FUNCTION 1 CHECK LETTER AMOUNT                                   
*              INPUT    1 WORD                                                  
*                       2 GUESS                                                 
*              OUTPUT   1 RF=1-CORRECT   RF=0-INCORRECT                         
******************************************************************              
CHECK    DS    0H                                                               
         NTR1                                                                   
         CLC   ANAGUSH+5(1),LENAN                                               
         BNE   INCORR                                                           
         L     RF,=F'1'                                                         
         B     CHECKEND                                                         
INCORR   L     RF,=F'0'                                                         
CHECKEND XIT1  REGS=(RF)                                                        
***************************************************************                 
*         ERROR                                                                 
*         FUNCTION 1 MESSAGE FOR PLAYER                                         
***************************************************************                 
ERROR    DS    0H                                                               
         NTR1                                                                   
         XC    ANAHDR,ANAHDR                                                    
         MVC   ANAHDR(L'MSG5),MSG5                                              
         XIT1                                                                   
***************************************************************                 
*              VALIDATE                                                         
*              FUNCTION 1 COMPARING GUESS&WORD                                  
*              INPUT    1 GUESS                                                 
*                       2 WORD                                                  
*              OUTPUT   1 RF=1-RIGHT   RF=0-WRONG                               
***************************************************************                 
VALID    DS    0H                                                               
         NTR1                                                                   
         XR    R2,R2                                                            
         IC    R2,LENAN                                                         
         LA    R3,ANAGUS                                                        
         LA    R4,WORD                                                          
VAL      CLC   0(1,R3),0(R4)                                                    
         BNE   WRONG                                                            
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,VAL                                                           
RIGHT    L     RF,=F'1'                                                         
         B     VALEND                                                           
WRONG    L     RF,=F'0'                                                         
VALEND   XIT1  REGS=(RF)                                                        
***************************************************************                 
*              PLAYER WON                                                       
*              FUNCTION 1 GIVE ANSWER                                           
*                       2 NEW SCORE                                             
*                       3 CLEAN SCREEN                                          
*                       4 SELECTION ANAGRAM                                     
*              OUTPUT   1 SCREEN NEW SCORE&ANAGRAM                              
***************************************************************                 
WIN      DS    0H                                                               
         NTR1                                                                   
         MVC   ANAANS+14(15),WORD                                               
         PACK  DUB,ANASCO+19(2)                                                 
         AP    DUB+6(2),=P'1'                                                   
         UNPK  ANASCO+19(2),DUB+6(2)                                            
         OI    ANASCO+20,X'F0'                                                  
         OI    ANAANSH+6,X'80'                                                  
         OI    ANASCOH+6,X'80'                                                  
         XC    ANAHDR,ANAHDR                                                    
         MVC   ANAHDR(L'MSG7),MSG7                                              
         BAS   RE,CLEAN            CLEAN SCREEN                                 
         BAS   RE,SELECT           SELECTION NEW ANAGRAM                        
         MVC   ANAGM,ANAGRAM                                                    
         OI    ANAGMH+6,X'80'                                                   
         XIT1                                                                   
****************************************************************                
*               NEW GUESS                                                       
*               FUNCTION 1 FILL IN GUESS SCREEN                                 
*                        2 MESSAGE FOR PLAYER                                   
****************************************************************                
NEWGUESS DS    0H                                                               
         NTR1                                                                   
         XC    ANAHDR,ANAHDR                                                    
         MVC   ANAHDR(L'MSG6),MSG6 MESSAGE                                      
*                                                                               
         BAS   RE,SCELET           MAKE SCELETON IN SCAN                        
         SR    R3,R3                                                            
         IC    R3,ATTEMPT                                                       
         S     R3,=F'1'                                                         
         MH    R3,=H'37'                                                        
         LA    R3,ANAG1H(R3)                                                    
         MVC   22(15,R3),SCAN                                                   
         OI    6(R3),X'80'                                                      
*                                                                               
         XIT1                                                                   
*******************************************************************             
*                      SCELETON                                   *             
*                      FUNCTION 1 FORM SCELETON FOR CLUE          *             
*                      INPUT    1 WORD                            *             
*                               2 GUESS                           *             
*                      OUTPUT   1 (*)-WRONG LETTER,LETTER-RIGHT   *             
*                                                         LETTER  *             
*******************************************************************             
SCELET   DS    0H                                                               
         NTR1                                                                   
         XC    SCAN,SCAN                                                        
         XR    R2,R2                                                            
         IC    R2,LENAN                                                         
         LA    R3,ANAGUS                                                        
         LA    R4,WORD                                                          
         LA    R5,SCAN                                                          
SCEL     CLC   0(1,R3),0(R4)                                                    
         BNE   WR                                                               
         MVC   0(1,R5),0(R3)                                                    
NL       LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R2,SCEL                                                          
         B     SCELEND                                                          
WR       MVI   0(R5),C'*'                                                       
         B     NL                                                               
SCELEND  XIT1                                                                   
********************************************************************            
*              SELECTION                                                        
*              FUNCTION 1 LOAD DICTIONARY    DICT STRUCTURE                     
*                       2 RANDOM NUMBER      1 SPACES IN THE BEGINING           
*                       3 SELECTION WORD     2 * BETWEEN WORD&ANAGRAM           
*                                            3 SPACES BETWEEN GROUPS            
*              INPUT    1 DICTIONARY                                            
*              OUTPUT   1 WORD&ANAGRAM                                          
*                       2 LENGTH OF ANAGRAM                                     
*********************************************************************           
SELECT   DS    0H                                                               
         NTR1                                                                   
******                       LOAD DICTIONARY                                    
         GOTO1 VCALLOV,DMCB,(0,DICT),X'D90B1501'                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD                                   
         L     R2,DMCB+8                                                        
         LA    R2,0(R2)            CLEAR TOP BYTE                               
         STH   R2,LENDICT          R2=LENGTH DICTIONARY                         
******                             RANDOM NUMBER                                
         GOTO1 VRANDOM,DMCB,(R2)                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R3,DMCB+4                                                        
         STH   R3,RANDNUM                                                       
******                             SELECTION                                    
         LA    R4,DICT                                                          
         AR    R3,R4               R3=ADDRESS OF RANDOM PLACE                   
*                                                                               
CL       CLI   0(R3),C'*'          RETRIEVAL ASTERISK                           
         BE    ASTER                                                            
         S     R3,=F'1'                                                         
         CR    R3,R4                                                            
         BE    BEGIN               BEGINING OF DICTIONARY                       
         B     CL                                                               
*                                                                               
BEGIN    CLI   0(R3),C'*'                                                       
         BE    ASTER                                                            
         LA    R3,1(R3)                                                         
         B     BEGIN                                                            
*                                                                               
ASTER    LR    R6,R3               R6=ADDRESS ASTERISK                          
CS       S     R3,=F'1'                                                         
         CLI   0(R3),C' '                                                       
         BE    AW                                                               
         B     CS                                                               
AW       LA    R3,1(R3)            R3=WORD ADDRESS                              
***                                MOVE WORD&ANAGRAM                            
         LR    R7,R6                                                            
         SR    R7,R3               R7=LENGTH OF WORD                            
         STC   R7,LENAN                                                         
         S     R7,=F'1'                                                         
         STC   R7,MOVE1+1                                                       
         XC    WORD,WORD                                                        
MOVE1    MVC   WORD(15),0(R3)                                                   
         LA    R6,1(R6)            R6=ADDRESS OF ANAGRAM                        
         STC   R7,MOVE2+1                                                       
         XC    ANAGRAM,ANAGRAM                                                  
MOVE2    MVC   ANAGRAM(15),0(R6)                                                
*                                                                               
         XIT1                                                                   
*****************************************************************               
*               CLEANING                                                        
*               FUNCTION 1 CLEAN GUESS,FIRST LETTER,ANAGUS,ATTEMPT              
*****************************************************************               
CLEAN    DS    0H                                                               
         NTR1                                                                   
         LA    R3,ANAG1H                                                        
         XR    R0,R0                                                            
         IC    R0,MAXGO                                                         
*                                                                               
CLEAN1   MVC   22(15,R3),SPACES                                                 
         OI    6(R3),X'80'                                                      
         LA    R3,37(R3)                                                        
         BCT   R0,CLEAN1                                                        
*                                                                               
CLEAN2   MVI   ANAFLET,C' '                                                     
         OI    ANAFLETH+6,X'80'                                                 
*                                                                               
CLEAN3   MVC   ANAGUS,SPACES                                                    
         OI    ANAGUSH+6,X'80'                                                  
*                                                                               
CLEAN4   MVI   ATTEMPT,X'00'                                                    
         XIT1                                                                   
**************************************************************                  
         LTORG                                                                  
MAXGO    DC    X'03'                                                            
DUB      DS    D                                                                
SPACES   DC    CL15' '                                                          
MSG1     DC    C'FIRST GAME-ENTER GUESS/OLD/LOSE/HELP'                          
MSG2     DC    C'NEW ANAGRAM-ENTER GUESS/OLD/LOSE/HELP'                         
MSG3     DC    C'CLUE-FIRST LETTER-ENTER GUESS/OLD/LOSE'                        
MSG4     DC    C'YOU LOST,NEXT GAME-ENTER GUESS/OLD/LOSE/HELP'                  
MSG5     DC    C'INVALID LENGTH-ENTER GUESS/OLD/LOSE/HELP'                      
MSG6     DC    C'NEXT ATTEMPT-ENTER GUESS/OLD/LOSE/HELP'                        
MSG7     DC    C'YOU WON,NEXT GAME-ENTER GUESS/OLD/LOSE/HELP'                   
***************************************************************                 
ANATEMP  DSECT                                                                  
RELO     DS    F                                                                
VCALLOV  DS    V                                                                
ACOMFACS DS    V                                                                
DMCB     DS    6F                                                               
VRANDOM  DS    V                                                                
LENDICT  DS    H                   LENGTH OF DICTIONARY                         
RANDNUM  DS    H                   RANDOM NUMBER                                
SCAN     DS    CL15                                                             
DICT     DS    4000C               LOAD AREA FOR DICTIONARY                     
ANATEMPX DS    0C                                                               
***************************************************************                 
ANASAVE  DSECT                                                                  
         DS    CL16                                                             
ANAGO    DS    C                   0-START MATCH 1-MATCH IN PROGRESS            
ATTEMPT  DS    C                   NUMBER OF ATTEMPT                            
WORD     DS    CL15                                                             
ANAGRAM  DS    CL15                                                             
LENAN    DS    C                   LENGTH OF ANAGRAM                            
         DS    CL15                                                             
***********************************************************                     
* GAANAFFD                                                                      
       ++INCLUDE GAANAFFD                                                       
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
***********************                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GAANA00   05/01/02'                                      
         END                                                                    
