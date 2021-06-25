*          DATA SET ACREPZY01  AT LEVEL 009 AS OF 01/17/13                      
*PHASE ACZY01C                                                                  
         TITLE '- SPECS FOR ABLELD CONVERSION TO POPULATE ABLTXS'               
*                                                                               
ACZT01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPZY01 01/17/13'                                      
         END                                                                    
