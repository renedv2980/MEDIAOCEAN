*          DATA SET ACREPIE01  AT LEVEL 006 AS OF 08/17/00                      
*PHASE ACIE01A                                                                  
         TITLE 'SPECS FOR TEXACO ESTIMATE INTERFACE'                            
ACIE01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
*        FSPEC READ,ACCOUNTS                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF RESET                                                            
*                                                                               
         ACDEF H1,55,C'TEXACO ESTIMATE TAPE'                                    
         ACDEF H2,55,20C'-'                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPIE01 08/17/00'                                      
         END                                                                    
