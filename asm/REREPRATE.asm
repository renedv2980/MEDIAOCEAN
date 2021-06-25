*          DATA SET REREPRATE  AT LEVEL 008 AS OF 08/31/00                      
*          DATA SET REREPRATE  AT LEVEL 007 AS OF 04/24/79                      
*PHASE BLRRATEA BLRRATES                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE IJDFYZZZ                                                               
         TITLE 'COMMISSION RATES'                                               
BLRRATES CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,BLRRATES,=V(REGSAVE)                                           
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         EJECT                                                                  
         MVC   TITLE+10(16),=C'COMMISSION RATES'                                
         MVC   MID1(14),=C'STATION   RATE'                                      
         MVC   MID2(16),=C'-------  -------'                                    
*                                                                               
READ     GOTO1 =V(CARDS),P1,CARD,=C'RE00'                                       
*                                                                               
         CLC   =C'/*',CARD                                                      
         BE    EOJ                                                              
         CLC   =C'RATE',CARD                                                    
         BE    *+6                                                              
         DC    H'0'                BAD CARD                                     
         SPACE 2                                                                
         MVC   P(4),CARD+9                                                      
         LA    R1,P+3                                                           
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         CLI   CARD+13,C' '                                                     
         BE    BR10                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),CARD+13                                                  
         MVI   2(R1),C'M'                                                       
*                                                                               
BR10     LA    R8,3                                                             
         LA    R2,P+8                                                           
         LA    R3,CARD+15                                                       
*                                                                               
BR11     MVC   0(2,R2),0(R3)                                                    
         MVI   2(R2),C'.'                                                       
         MVC   3(4,R2),2(R3)                                                    
*                                                                               
         CLI   0(R2),C'0'                                                       
         BNE   BR12                                                             
         MVI   0(R2),C' '                                                       
         CLI   1(R2),C'0'                                                       
         BNE   BR12                                                             
         MVI   1(R2),C' '                                                       
BR12     EQU   *                                                                
         LA    R2,8(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R8,BR11                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     READ                                                             
*                                                                               
EOJ      EOJ                                                                    
*                                                                               
CARD     DS    CL80                                                             
P1       DS    4F                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREPRATE 08/31/00'                                      
         END                                                                    
