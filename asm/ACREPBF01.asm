*          DATA SET ACREPBF01  AT LEVEL 002 AS OF 08/18/98                      
*PHASE ACBF01A                                                                  
         TITLE 'BILLING PRINT FLAT FILE SPECS'                                  
ACBP01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF GETOPT,NO                                                        
         ACDEF RESET                                                            
*                                                                               
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPBF01 08/18/98'                                      
         END                                                                    
