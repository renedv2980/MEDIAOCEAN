*          DATA SET BURNEMTST  AT LEVEL 003 AS OF 11/27/85                      
*PHASE BURNTST,*                                                                
*INCLUDE BURNEM                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'MODULE TO TEST BURNEM'                                          
BURNTST  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**BURNTS,WORK=V(REGSAVE)                                       
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(13),=C'BURNEM TESTER'                                      
*                                                                               
BURN2    GOTO1 =V(CARDS),PARA,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'                                                   
         BE    BURNX                                                            
BURN4    LA    R1,CARD+29                                                       
         LA    R0,30                                                            
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         GOTO1 =V(BURNEM),PARA,((R0),CARD),DUB                                  
         MVC   P(30),CARD                                                       
         XOUT  PARA,P+32,8                                                      
         XOUT  DUB,P+50,8                                                       
         GOTO1 =V(PRINTER)                                                      
         B     BURN2                                                            
*                                                                               
BURNX    XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
PARA     DS    6F                                                               
DUB      DS    D                                                                
CARD     DS    CL80                                                             
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003BURNEMTST 11/27/85'                                      
         END                                                                    
