*          DATA SET ACREPZ501  AT LEVEL 018 AS OF 08/16/00                      
*PHASE ACZ501A                                                                  
         TITLE '$CTA FIX, FIX CONTRACT # IN KEYS'                               
ACZ501   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,COMPANY                                                     
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,035,C'FIX CONTRACT # IN KEYS'                                 
         ACDEF H1,70,PAGE                                                       
*                                                                               
         ACDEF H6,003,C'ACCOUNT'                                                
         ACDEF H7,003,C'--------------'                                         
         ACDEF H6,019,C'CONTRA'                                                 
         ACDEF H7,019,C'--------------'                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACREPZ501 08/16/00'                                      
         END                                                                    
