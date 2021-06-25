*          DATA SET SPREPFX01G AT LEVEL 041 AS OF 12/29/97                      
*PHASE SPFX01A                                                                  
         TITLE 'SPFX01 - SPOTPAK FIX PROGRAM - SPECS'                           
SPFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC USE,SP0003                                                       
         SSPEC H1,57,C'SPOTPAK FILE FIX'                                        
         SSPEC H2,57,C'----------------'                                        
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SPREPFX01G12/29/97'                                      
         END                                                                    
