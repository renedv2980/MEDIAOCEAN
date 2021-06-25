*          DATA SET ACREPWL01  AT LEVEL 013 AS OF 08/17/00                      
*PHASE ACWL01A                                                                  
ACWL01   TITLE 'PRINT WORKER FILE LIST'                                         
ACWL01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H1,50,C'Worker File Listing'                                     
         ACDEF H4,2,C'Key:-'                                                    
         ACDEF H2,50,C'-------------------'                                     
         ACDEF M1,2,C'Account'                                                  
         ACDEF M2,2,C'-------'                                                  
         ACDEF M1,17,C'Contra-account'                                          
         ACDEF M2,17,C'--------------'                                          
         ACDEF M1,33,C'Codes'                                                   
         ACDEF M2,33,C'-----'                                                   
         ACDEF M1,39,C' Date'                                                   
         ACDEF M2,39,C' ----'                                                   
         ACDEF M1,48,C'Ref#'                                                    
         ACDEF M2,48,C'----'                                                    
         ACDEF M1,55,C'Batch'                                                   
         ACDEF M2,55,C' Ref '                                                   
         ACDEF M1,62,C'Narrative'                                               
         ACDEF M2,62,C'---------'                                               
         ACDEF M1,105,C'      Debits '                                          
         ACDEF M2,105,C'      ------ '                                          
         ACDEF M1,119,C'     Credits '                                          
         ACDEF M2,119,C'     ------- '                                          
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPWL01 08/17/00'                                      
         END                                                                    
