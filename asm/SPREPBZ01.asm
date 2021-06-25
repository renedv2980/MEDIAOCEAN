*          DATA SET SPREPBZ01  AT LEVEL 004 AS OF 08/29/00                      
*PHASE SPBZ01A                                                                  
         TITLE 'SPFX01 - SPOT NON-POL BUY CONVERSION - SPECS'                   
SPBZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'NON-POL BUY CONVERSION'                                  
         SSPEC H2,55,C'----------------------'                                  
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPBZ01 08/29/00'                                      
         END                                                                    
