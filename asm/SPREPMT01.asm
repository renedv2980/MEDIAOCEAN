*          DATA SET SPREPMT01  AT LEVEL 004 AS OF 08/29/00                      
*PHASE SPMT01A                                                                  
         TITLE 'SPMT01 - SPECS FOR MHTO SPOT FILE FIX'                          
SPMT01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         SSPEC H1,55,C'MHTO SPOT FILE FIX'                                      
         SSPEC H2,55,C'------------------'                                      
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPMT01 08/29/00'                                      
         END                                                                    
