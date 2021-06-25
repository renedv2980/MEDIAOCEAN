*          DATA SET SPREPAG01  AT LEVEL 001 AS OF 06/30/04                      
*PHASE SPAG01                                                                   
         TITLE 'SPAG01 - ACCENT FILE EXTRACT  - SPECS'                          
SPAG01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPAG01 06/30/04'                                      
         END                                                                    
