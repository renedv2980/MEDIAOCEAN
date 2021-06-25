*          DATA SET ACREPMB01  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACMB01A                                                                  
         TITLE 'ZENITH MEDIA BILLING TRANSFER'                                  
ACMB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'ZENITH MEDIA BILLING'                                   
         ACDEF H1,143,C'REPORT MB02'                                            
         ACDEF H2,143,PAGE                                                      
         ACDEF H3,143,PERIOD                                                    
*                                                                               
         ACDEF H6,003,C'CLI/PRD'                                                
         ACDEF H7,004,C'CODE'                                                   
         ACDEF H7,013,C'NAME'                                                   
         ACDEF H6,057,C'SYS/'                                                   
         ACDEF H7,057,C'MED'                                                    
         ACDEF H7,064,C'MEDIA'                                                  
         ACDEF H6,081,C'BILLING'                                                
         ACDEF H7,081,C'MONTH'                                                  
         ACDEF H7,095,C'BILLING AMOUNT'                                         
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'ZENITH MEDIA BILLING'                                   
         ACDEF H1,143,C'REPORT MB02'                                            
         ACDEF H2,143,PAGE                                                      
         ACDEF H3,143,PERIOD                                                    
*                                                                               
         ACDEF H6,003,C'COSTING'                                                
         ACDEF H7,003,C'ACCOUNT'                                                
         ACDEF H6,023,C'ANALYSIS'                                               
         ACDEF H7,023,C'ACCOUNT'                                                
         ACDEF H6,043,C'BILLING'                                                
         ACDEF H7,043,C'MONTH'                                                  
         ACDEF H7,055,C'BILLING AMOUNT'                                         
         ACDEF H6,081,C'TOTAL'                                                  
         ACDEF H7,077,C'COSTING AMOUNT'                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPMB01 08/17/00'                                      
         END                                                                    
