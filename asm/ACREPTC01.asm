*          DATA SET ACREPTC01  AT LEVEL 004 AS OF 07/24/07                      
*PHASE ACTC01A                                                                  
         TITLE 'ACTC01 - 1099 RECORD CREATOR SPECS'                             
ACTC01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         SPROG 0,1,4                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,173,REPORT                                                    
         ACDEF H1,187,PAGE                                                      
         ACDEF H2,173,REQUESTOR                                                 
         ACDEF H2,2,ORIGIN                                                      
*                                                                               
         SPROG 1                                                                
         ACDEF H3,90,C'1099 RECORD REPORT'                                      
         ACDEF H4,90,C'------------------'                                      
         ACDEF H6,2,C'ALPHA'                                                    
         ACDEF H6,8,C'CMP'                                                      
         ACDEF H6,12,C'YEAR'                                                    
         ACDEF H6,17,C'NUM'                                                     
         ACDEF H6,23,C'USER ID'                                                 
         ACDEF H6,33,C'NAME/ADDRESS'                                            
         ACDEF H6,74,C'EMAIL'                                                   
         ACDEF H6,106,C'TCC'                                                    
         ACDEF H6,112,C'TIN'                                                    
         ACDEF H6,122,C'CONTACT NAME'                                           
         ACDEF H6,156,C'CONTACT PHONE'                                          
         ACDEF H6,172,C'D/L'                                                    
         ACDEF H6,176,C'FORMS'                                                  
         ACDEF H6,182,C'MESSAGE'                                                
*                                                                               
         SPROG 4                                                                
         ACDEF H3,87,C'1099 RECORD TOTAL SUMMARY'                               
         ACDEF H4,87,C'-------------------------'                               
*                                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPTC01 07/24/07'                                      
         END                                                                    
