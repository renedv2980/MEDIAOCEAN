*          DATA SET SPREPFX01U AT LEVEL 002 AS OF 08/29/00                      
*          DATA SET SPREPFX01  AT LEVEL 040 AS OF 12/14/95                      
*PHASE SPFX01A                                                                  
         TITLE 'SPFX01 - SPOTPAK FIX PROGRAM - SPECS'                           
SPFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SSPEC H1,55,C'YET ANOTHER FILE FIX'                                    
         SSPEC H2,55,C'--------------------'                                    
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPFX01U08/29/00'                                      
         END                                                                    
