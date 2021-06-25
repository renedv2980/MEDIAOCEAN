*          DATA SET SPREPA901  AT LEVEL 001 AS OF 02/10/04                      
*PHASE SPA901                                                                   
         TITLE 'SPA901 - ACCENT FILE EXTRACT  - SPECS'                          
SPA901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPA901 02/10/04'                                      
         END                                                                    
