*          DATA SET SPREPPF01  AT LEVEL 001 AS OF 10/01/01                      
*PHASE SPPF01A                                                                  
         TITLE 'SPPF01 - PARAMOUNT FILE INTERFACE - SPECS'                      
SPPF01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPPF01 10/01/01'                                      
         END                                                                    
