*          DATA SET ACREPCC01  AT LEVEL 057 AS OF 08/16/00                      
*PHASE ACCC01A,*                                                                
         TITLE 'SPECS FOR CLIENT BILLING / TMS CHECK'                           
ACCB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,80,REPORT                                                     
         ACDEF H1,98,PAGE                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057ACREPCC01 08/16/00'                                      
         END                                                                    
