*          DATA SET DDBIGLETS  AT LEVEL 007 AS OF 04/25/01                      
*PHASE BIGLETSA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE LOZENGE                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
         TITLE 'BIG LETTER PRINTING'                                            
BIGLETS  CSECT                                                                  
         NBASE 0,BIGLETS,=V(REGSAVE)                                            
         GOTO1 =V(PRINT),PARA,C,=C'BC01'                                        
LOOP     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BNE   LOOP2                                                            
         XBASE                                                                  
         SPACE 2                                                                
LOOP2    GOTO1 =V(LOZENGE),PARA,C                                               
         MVC   C(132),=CL132' '                                                 
         GOTO1 =V(PRINT),PARA,C,=C'BL03'                                        
         B     LOOP                                                             
         SPACE 2                                                                
C        DS    132C                                                             
PARA     DS    D                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDBIGLETS 04/25/01'                                      
         END                                                                    
