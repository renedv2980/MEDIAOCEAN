*          DATA SET SPREPCX01  AT LEVEL 002 AS OF 02/07/04                      
*PHASE SPCX01A                                                                  
         TITLE 'SPCX01 - SPOTPAK FIX PROGRAM - SPECS'                           
SPCX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,41,C'CANADIAN BUY INTEGRITY CHECK'                            
         SSPEC H2,41,C'---------------------------- '                           
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,80,AGYNAME                                                    
         SSPEC H2,80,AGYADDR                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPCX01 02/07/04'                                      
         END                                                                    
