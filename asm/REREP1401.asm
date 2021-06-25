*          DATA SET REREP1401  AT LEVEL 017 AS OF 02/24/97                      
*PHASE RE1401A,+0                                                               
         TITLE 'SPECS FOR CONTRACT PRINTING'                                    
RE1401   CSECT                                                                  
*        PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC GET,STATION                                                      
*        RSPEC REQUEST,NOREP                                                    
*        RSPEC REQUEST,NOSUM                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017REREP1401 02/24/97'                                      
         END                                                                    
