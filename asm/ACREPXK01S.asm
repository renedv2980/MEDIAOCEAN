*          DATA SET ACREPXK01S AT LEVEL 018 AS OF 08/16/00                      
*PHASE ACXK01A,+0                                                               
         TITLE 'TEST'                                                           
ACXK01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,035,C'REMOVE WC FROM KEY'                                     
         ACDEF H1,70,PAGE                                                       
*                                                                               
         ACDEF H6,003,C'ACCOUNT'                                                
         ACDEF H7,003,C'--------------'                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACREPXK01S08/16/00'                                      
         END                                                                    
