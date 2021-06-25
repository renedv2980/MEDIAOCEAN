*          DATA SET REREPJB01  AT LEVEL 003 AS OF 08/31/00                      
*          DATA SET REREPJB01  AT LEVEL 002 AS OF 04/25/86                      
*PHASE REJB01A                                                                  
         TITLE 'REJB01'                                                         
REJB01   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,NOREP                                                    
         FSPEC UPDATE,REPFILE                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REREPJB01 08/31/00'                                      
         END                                                                    
