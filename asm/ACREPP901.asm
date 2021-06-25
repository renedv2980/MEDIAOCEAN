*          DATA SET ACREPP901  AT LEVEL 016 AS OF 08/17/00                      
*PHASE ACP901A                                                                  
         TITLE 'SPECS FOR JW/USMC/PO LIST'                                      
ACP901   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ORDERS                                                      
         ACDEF RESET                                                            
         SPROG 0                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,109,REPORT                                                    
         ACDEF H1,123,PAGE                                                      
         ACDEF H2,109,REQUESTOR                                                 
         ACDEF H3,2,COMPANY                                                     
         ACDEF H1,55,C'PRODUCTION ORDERS ISSUED FOR'                            
         ACDEF H2,55,C' UNITED STATES MARINE CORPS'                             
         ACDEF H5,2,C'ESTIMATE PROD ORDER'                                      
         ACDEF H6,2,C'NUMBER   CODE NUMBER'                                     
         ACDEF H5,24,C'ORDER'                                                   
         ACDEF H6,24,C'DATE'                                                    
         ACDEF H6,32,C'VENDOR NAME'                                             
         ACDEF H5,67,C'ORDER'                                                   
         ACDEF H6,67,C'AMOUNT'                                                  
         ACDEF H5,78,C'INVOICE'                                                 
         ACDEF H6,78,C'NUMBER'                                                  
         ACDEF H5,86,C'INVOICE'                                                 
         ACDEF H6,86,C' DATE  '                                                 
         ACDEF H5,98,C'INVOICE'                                                 
         ACDEF H6,98,C'AMOUNT'                                                  
         ACDEF H7,2,C'-------- ---- ------'                                     
         ACDEF H7,23,C'--------'                                                
         ACDEF H7,32,C'-------------------------------'                         
         ACDEF H7,64,C'-------------'                                           
         ACDEF H7,78,C'-------'                                                 
         ACDEF H7,86,C'-------'                                                 
         ACDEF H7,95,C'-------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREPP901 08/17/00'                                      
         END                                                                    
