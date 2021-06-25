*          DATA SET ACREPOX01  AT LEVEL 005 AS OF 07/07/07                      
*PHASE ACOX01A                                                                  
         TITLE 'SPECS FOR PRODUCTION CONVERSION'                                
ACOX01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,54                                                      
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MODE,PROCLEV                                                     
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,38,C'PRODUCTION OPTION CONVERSION'                            
         ACDEF H2,38,C'----------------------------'                            
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H09,3,C'OG OFF CLIENT PRODUCT JOB     MG MEDIA WG WORK'          
         ACDEF H09,51,C'OPTION/CODE'                                            
         ACDEF H09,90,C'SETTING'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPOX01 07/07/07'                                      
         END                                                                    
