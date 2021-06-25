*          DATA SET SPREPFF01  AT LEVEL 001 AS OF 11/12/08                      
*PHASE SPFF01A,+0,NOAUTO                                                        
         TITLE 'SPFF01 - EASI - INVOICE CONVERTER'                              
         PRINT NOGEN                                                            
SPFF01   CSECT                                                                  
*                                                                               
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC USE,SP0003                                                       
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPFF01 11/12/08'                                      
         END                                                                    
