*          DATA SET ACREPP001  AT LEVEL 002 AS OF 09/07/11                      
*PHASE ACP001B                                                                  
         TITLE 'SPECS FOR PRODUCTION ORDER LIST'                                
ACP001   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ORDERS                                                      
         ACDEF RESET                                                            
         SPROG 0                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,109,REPORT                                                    
         ACDEF H1,123,PAGE                                                      
         ACDEF H2,109,REQUESTOR                                                 
         ACDEF H3,2,COMPANY                                                     
         ACDEF H1,38,C'PRODUCTION ORDER LIST'                                   
         ACDEF H2,38,C'---------------------'                                   
         ACDEF H6,2,C'UL CLT  PRD  JOB     ORDER'                               
         ACDEF H7,2,C'-- ---  ---  ------  ------'                              
         ACDEF H6,31,C'S'                                                       
         ACDEF H7,31,C'-'                                                       
         ACDEF H6,36,C'DATE'                                                    
         ACDEF H7,34,C'------'                                                  
         ACDEF H6,44,C'VENDOR CODE'                                             
         ACDEF H7,44,C'--------------'                                          
         ACDEF H6,61,C' ORDER AMOUNT'                                           
         ACDEF H7,61,C'-------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPP001 09/07/11'                                      
         END                                                                    
