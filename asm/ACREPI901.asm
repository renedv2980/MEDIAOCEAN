*          DATA SET ACREPI901  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACI901A                                                                  
         TITLE 'BACKER-PROD VENDOR TAPE LISTING REPORT SPECS'                   
ACI901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         RSPEC MAXLINES,57                                                      
*                                                                               
         SPROG 0,1                                                              
         ASPEC H2,86,RUN                                                        
         ASPEC H3,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H2,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H3,2,UNIT                                                        
         ASPEC H4,2,LEDGER                                                      
*                                                                               
         SPROG 0                                                                
         ASPEC H1,50,C'VENDOR INTERFACE REPORT'                                 
         ASPEC H2,50,23C'-'                                                     
         ASPEC H7,2,C'ACCOUNT CODE'                                             
         ASPEC H8,2,12C'-'                                                      
         ASPEC H7,17,C'ACCOUNT NAME'                                            
         ASPEC H8,17,36C'-'                                                     
         ASPEC H7,56,C'ADDRESS'                                                 
         ASPEC H8,56,26C'-'                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPI901 08/17/00'                                      
         END                                                                    
