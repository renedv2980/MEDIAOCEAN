*          DATA SET ACREPRC01  AT LEVEL 004 AS OF 07/07/07                      
*PHASE ACRC01A                                                                  
         TITLE 'SPECS FOR PRODUCTION CONVERSION'                                
ACRC01   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC MAXLINES,54                                                      
         FSPEC READ,ACCOUNTS                                                    
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H1,38,C'PRODUCTION OPTION CONVERSION'                            
         ASPEC H2,38,C'----------------------------'                            
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         ASPEC H09,3,C'OG OFF CLIENT PRODUCT JOB     MG MEDIA WG WORK'          
         ASPEC H09,51,C'OPTION/CODE'                                            
         ASPEC H09,90,C'SETTING'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPRC01 07/07/07'                                      
         END                                                                    
