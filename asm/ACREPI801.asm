*          DATA SET ACREPI801  AT LEVEL 007 AS OF 08/17/00                      
*PHASE ACI801A                                                                  
ACI801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         SPACE                                                                  
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,42,C'PRODUCTION VENDOR REPORT'                                
         ASPEC H2,42,C'------------------------'                                
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         SPACE                                                                  
         ASPEC H09,2,C'VENDOR #'                                                
         ASPEC H10,2,C'--------'                                                
         ASPEC H09,12,C'D'                                                      
         ASPEC H10,12,C'-'                                                      
         ASPEC H09,15,C'NAME'                                                   
         ASPEC H10,15,C'----'                                                   
         ASPEC H09,42,C'ADDRESS'                                                
         ASPEC H10,42,C'-------'                                                
         ASPEC H09,69,C'1099'                                                   
         ASPEC H10,69,C'----'                                                   
         ASPEC H09,75,C'SS OR FED ID'                                           
         ASPEC H10,75,C'------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPI801 08/17/00'                                      
         END                                                                    
