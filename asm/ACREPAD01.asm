*          DATA SET ACREPAD01  AT LEVEL 003 AS OF 03/21/01                      
*PHASE ACAD01A                                                                  
         TITLE 'ADDRESS CHECK'                                                  
ACAD01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,49,REPORT                                                     
         ACDEF H2,49,RUN                                                        
*                                                                               
         ACDEF H4,3,C'ACCOUNT / ADDRESS'                                        
         ACDEF H5,3,C'-----------------'                                        
         ACDEF H4,27,C'MESSAGE'                                                 
         ACDEF H5,27,C'-------'                                                 
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H4,3,C'TOTALS           '                                        
         ACDEF H5,3,C'-----------------'                                        
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H4,3,C'VALID COUNTRY NAMES AND CODES'                            
         ACDEF H5,3,C'-----------------------------'                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPAD01 03/21/01'                                      
         END                                                                    
