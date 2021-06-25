*          DATA SET REREPRG01  AT LEVEL 006 AS OF 08/31/00                      
*          DATA SET REREPRG01  AT LEVEL 005 AS OF 02/28/85                      
*PHASE RERG01A                                                                  
         TITLE 'RERG01 - DUMMY SPECS FOR REPRG'                                 
RERG01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         RSPEC REQUEST,NOREP                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REREPRG01 08/31/00'                                      
         END                                                                    
