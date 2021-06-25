*          DATA SET ACREPXP01  AT LEVEL 007 AS OF 08/16/00                      
*PHASE ACXP01A                                                                  
         TITLE 'SPECS FOR POSTING GENERATION'                                   
         PRINT NOGEN                                                            
ACXP01   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANSACTIONS                                                
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'CREATE POSTING FILE'                                     
         ASPEC H2,45,C'-------------------'                                     
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H6,2,C'ACCOUNT        NAME'                                      
         ASPEC H7,2,C'-------        ----'                                      
         ASPEC H6,54,C'  AMOUNT  '                                              
         ASPEC H7,54,C' ---------'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPXP01 08/16/00'                                      
         END                                                                    
