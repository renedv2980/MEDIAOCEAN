*          DATA SET REREP1001  AT LEVEL 011 AS OF 01/09/97                      
*PHASE RE1001A,+0                                                               
         TITLE 'SPECS FOR CONTRACT PRINTING'                                    
RE1001   CSECT                                                                  
*        PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         RSPEC REQUEST,NOREP                                                    
         RSPEC REQUEST,NOSUM                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011REREP1001 01/09/97'                                      
         END                                                                    
