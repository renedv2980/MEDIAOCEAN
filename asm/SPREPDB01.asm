*          DATA SET SPREPDB01  AT LEVEL 004 AS OF 04/16/07                      
*          DATA SET SPREPPF01  AT LEVEL 048 AS OF 06/25/01                      
*PHASE SPDB01A                                                                  
         TITLE 'SPDB01 - DB2 INTERFACE FILES  - SPECS'                          
SPDB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,SPTFILE                                                     
         FSPEC OPEN,DEMFILES                                                    
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPDB01 04/16/07'                                      
         END                                                                    
