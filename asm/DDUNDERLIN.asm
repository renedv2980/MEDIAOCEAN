*          DATA SET DDUNDERLIN AT LEVEL 005 AS OF 08/22/00                      
*PHASE T00A10A                                                                  
         TITLE 'ROUTINE TO UNDERLINE TEXT'                                      
UNDERLIN CSECT                                                                  
         NMOD1 0,UNDERLIN                                                       
         LM    R2,R3,0(R1)                                                      
         SR    R4,R4                                                            
         IC    R4,0(,R1)           LENGTH OF INPUT                              
         AHI   R4,-1                                                            
         BM    XIT                 NONE                                         
*                                                                               
         CLI   0(R2),C' '          ANY DATA TO UNDERLINE                        
         BNE   UL2                 YES                                          
         AHI   R4,-1               LESS ONE FOR REPEATING COMPARE               
         BM    XIT                 NOTHING TO UNDERLINE                         
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R2),0(R2)                                                    
         BE    XIT                 NOTHING TO UNDERLINE                         
         AHI   R4,1                READJUST FOR FOR AHI -1                      
*                                                                               
UL2      MVI   0(R3),C'-'          FILL OUTPUT WITH DASHES                      
         CLI   4(R1),0                       OR                                 
         BE    *+10                                                             
         MVC   0(1,R3),4(R1)          SELECTED CHARACTER                        
         AHI   R4,-1                                                            
         BM    XIT                 DID ALL THE UNDERLINEING WE NEED             
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
*                                                                               
UL4      CLI   0(R2),C' '          THEN CLEAR PRECEDING                         
         BNE   UL6                                                              
         MVI   0(R3),C' '                                                       
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         B     UL4                                                              
*                                                                               
UL6      LM    R2,R3,0(R1)                                                      
         LA    R2,1(R2,R4)                                                      
         LA    R3,1(R3,R4)                                                      
*                                                                               
UL8      CLI   0(R2),C' '          AND SUBSEQUENT                               
         BNE   XIT                                                              
         MVI   0(R3),C' '                                                       
         BCTR  R2,0                                                             
         BCT   R3,UL8                                                           
*                                                                               
XIT      XMOD1 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDUNDERLIN08/22/00'                                      
         END                                                                    
