*          DATA SET DDCHARTEST AT LEVEL 016 AS OF 05/01/02                      
*PHASE CHARTEST,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
         TITLE 'CHARACTER TESTING'                                              
CHARTEST CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*CHAR**,=V(REGSAVE),R9                                         
         EJECT                                                                  
*              BASIC LOOP                                                       
         SPACE 3                                                                
LOOP     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BNE   LOOP2                                                            
         BAS   RE,SKIP                                                          
         LA    R0,12                                                            
         SPACE 1                                                                
LOOP1    MVC   P(64),CHARS                                                      
         BAS   RE,P1                                                            
         MVC   P(64),CHARS                                                      
         OC    P(64),=64X'40'                                                   
         BAS   RE,P1                                                            
         MVC   P(64),CHARS                                                      
         OC    P(64),=64X'80'                                                   
         BAS   RE,P1                                                            
         MVC   P(64),CHARS                                                      
         OC    P(64),=64X'C0'                                                   
         BAS   RE,P1                                                            
         BCT   R0,LOOP1                                                         
         XBASE                                                                  
         SPACE 1                                                                
LOOP2    BAS   RE,SKIP                                                          
         GOTO1 =V(HEXIN),PARA,C,C+4,2                                           
         LA    R0,20                                                            
         MVI   UNDER,0                                                          
         CLI   C+2,C' '                                                         
         BE    LOOP4                                                            
         GOTO1 =V(HEXIN),PARA,C+2,UNDER,2                                       
         SPACE 1                                                                
LOOP4    CLI   UNDER,0                                                          
         BE    LOOP5                                                            
         MVC   P(1),UNDER                                                       
         MVC   P+1(131),P                                                       
         BAS   RE,P0                                                            
         SPACE 1                                                                
LOOP5    MVC   P(1),C+4                                                         
         MVC   P+1(131),P                                                       
         BAS   RE,P1                                                            
         BCT   R0,LOOP4                                                         
         B     LOOP                                                             
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
PFILL    DS    CL1                                                              
P        DC    CL132' '                                                         
C        DC    CL80' '                                                          
CHARS    DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)                      
         DC    AL1(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)             
         DC    AL1(33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48)             
         DC    AL1(49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64)             
SPACES   DC    CL132' '                                                         
LINELIST DC    C'00102030405060708090A0B0C0D0E0F0'                              
COMMAND  DS    CL4                                                              
PARA     DS    6F                                                               
UNDER    DC    X'00'                                                            
GRPHLIST DS    0H                                                               
         DC    C'30',X'30'                                                      
         DC    C'31',X'31'                                                      
         DC    C'32',X'32'                                                      
         DC    C'33',X'33'                                                      
         DC    C'34',X'34'                                                      
         DC    C'35',X'35'                                                      
         DC    C'36',X'36'                                                      
         DC    C'37',X'37'                                                      
         DC    C'38',X'38'                                                      
         DC    C'39',X'39'                                                      
         DC    C'3A',X'3A'                                                      
         DC    C'3B',X'3B'                                                      
         DC    C'3C',X'3C'                                                      
         DC    C'3D',X'3D'                                                      
         DC    C'3E',X'3E'                                                      
         DC    C'3F',X'3F'                                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
SKIP     NTR1                                                                   
         MVC   COMMAND,=C'BC01'                                                 
         B     PALL                                                             
         SPACE 1                                                                
P0       NTR1                                                                   
         MVC   COMMAND,=C'BL00'                                                 
         B     PALL                                                             
         SPACE 1                                                                
P1       NTR1                                                                   
         MVC   COMMAND,=C'BL01'                                                 
         B     PALL                                                             
         SPACE 1                                                                
P2       NTR1                                                                   
         MVC   COMMAND,=C'BL02'                                                 
         SPACE 1                                                                
PALL     GOTO1 =V(PRINT),PARA,P-1,COMMAND                                       
         MVC   P,SPACES                                                         
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DDCHARTEST05/01/02'                                      
         END                                                                    
