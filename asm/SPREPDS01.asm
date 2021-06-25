*          DATA SET SPREPDS01  AT LEVEL 001 AS OF 01/30/04                      
*PHASE SPDS01A                                                                  
         TITLE 'SPDS01 - SUPERDESK DB2 FILES  - SPECS'                          
SPDS01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPDS01 01/30/04'                                      
         END                                                                    
