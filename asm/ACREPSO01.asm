*          DATA SET ACREPSO01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACSO01A,*                                                                
ACSO01   TITLE '- SPECS FOR SOFDAT TESTING BED'                                 
ACSO01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF NOSUM                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,56,C'SOFDAT testing bed'                                      
         ACDEF H2,56,C'------------------'                                      
         ACDEF H1,80,REPORT                                                     
         ACDEF H2,80,PAGE                                                       
         ACDEF M2,2,C'---------Input/Output----------'                          
         ACDEF M1,35,C' Date'                                                   
         ACDEF M2,35,C'Today  Options Err#   Days Months  Years'                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPSO01 08/17/00'                                      
         END                                                                    
