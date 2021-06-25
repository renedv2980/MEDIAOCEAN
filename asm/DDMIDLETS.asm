*          DATA SET DDMIDLETS  AT LEVEL 004 AS OF 05/01/02                      
*PHASE MIDLETS,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
         TITLE 'PRINTS FAIRLY BIG LETTERS'                                      
MIDLETS  CSECT                                                                  
         NBASE 0,*MIDLETS,=V(REGSAVE)                                           
         GOTO1 =V(PRINT),PARA,P,=C'BC01'                                        
         EJECT                                                                  
*              READ CARDS FOR INPUT LETTERS                                     
         SPACE 3                                                                
READ     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BNE   READ2                                                            
*&&DO*&& EOJ                                                                    
*&&OS*&& XBASE                                                                  
         SPACE 2                                                                
READ2    CLC   C(8),=C'REVERSE='                                                
         BNE   READ3                                                            
         BAS   RE,SETREV                                                        
         B     READ                                                             
         SPACE 2                                                                
READ3    LA    R2,C                                                             
         LA    R3,P+133                                                         
         LA    R4,13                                                            
         BAS   RE,CLEAR                                                         
         SPACE 2                                                                
READ4    GOTO1 =V(EXPAND),PARA,(R2),(124,(R3))                                  
         LA    R2,1(R2)                                                         
         LA    R3,10(R3)                                                        
         BCT   R4,READ4                                                         
         SPACE 2                                                                
         BAS   RE,REVERSE                                                       
         LA    R2,P-1                                                           
         LA    R4,10                                                            
         SPACE 2                                                                
WRITE2   GOTO1 =V(PRINT),PARA,(R2),=C'BL01'                                     
         LA    R2,132(R2)                                                       
         BCT   R4,WRITE2                                                        
         B     READ                                                             
         EJECT                                                                  
*              HANDLE REVERSE LETTERS                                           
         SPACE 3                                                                
SETREV   DS    0H                                                               
         MVC   REVMASK,C+8                                                      
         CLI   REVMASK,C' '                                                     
         BCR   8,RE                                                             
         LA    R5,REVMASK+71       SET R5 TO LAST LETTER IN MASK                
         SPACE 2                                                                
SETREV2  CLI   0(R5),C' '                                                       
         BCR   7,RE                                                             
         BCT   R5,SETREV2                                                       
         SPACE 2                                                                
REVERSE  CLI   REVMASK,C' '                                                     
         BCR   8,RE                                                             
         LA    R2,P                                                             
         LA    R4,1320                                                          
         SPACE 2                                                                
REV2     CLI   0(R2),C' '                                                       
         BE    REV4                                                             
         MVI   0(R2),C' '                                                       
         B     REV6                                                             
         SPACE 2                                                                
REV4     MVC   0(1,R2),REVMASK                                                  
         SPACE 2                                                                
REV6     IC    R6,REVMASK          ROTATE MASK                                  
         MVC   REVMASK(L'REVMASK-1),REVMASK+1                                   
         STC   R6,0(R5)                                                         
         LA    R2,1(R2)                                                         
         BCT   R4,REV2                                                          
         BR    RE                                                               
         SPACE 2                                                                
CLEAR    DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         LA    R6,P                                                             
         LA    R7,9                                                             
         SPACE 2                                                                
CLEAR2   MVC   132(132,R6),0(R6)                                                
         LA    R6,132(R6)                                                       
         BCT   R7,CLEAR2                                                        
         BR    RE                                                               
         EJECT                                                                  
PARA     DS    6F                                                               
C        DS    CL80                                                             
P        DC    1320C' '                                                         
REVMASK  DC    CL72' '                                                          
         DC    C' '                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDMIDLETS 05/01/02'                                      
         END                                                                    
