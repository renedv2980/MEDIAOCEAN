*          DATA SET SPREPFX01A AT LEVEL 002 AS OF 04/02/96                      
*          DATA SET SPREPFX01  AT LEVEL 040 AS OF 12/14/95                      
*PHASE SPFX01A                                                                  
         TITLE 'SPFX01 - SPOTPAK FIX PROGRAM - SPECS'                           
SPFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'MISSING MINUS OTO REPORT'                                
         SSPEC H2,55,C'------------------------'                                
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPFX01A04/02/96'                                      
         END                                                                    
