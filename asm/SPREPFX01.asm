*          DATA SET SPREPFX01  AT LEVEL 044 AS OF 11/20/06                      
*PHASE SPFX01A                                                                  
         TITLE 'SPFX01 - SPOTPAK FIX PROGRAM - SPECS'                           
SPFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,STATION                                                   
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SSPEC H1,55,C'YET ANOTHER FILE FIX'                                    
         SSPEC H2,55,C'--------------------'                                    
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPREPFX01 11/20/06'                                      
         END                                                                    
