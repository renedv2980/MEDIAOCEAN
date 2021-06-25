*          DATA SET SPREPL201  AT LEVEL 005 AS OF 08/29/00                      
*PHASE SPL201A                                                                  
         TITLE 'SPREPL201 - SPOTPAK ESTIMATE HEADER REPORT'                     
SPL201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,2,3,4                                                        
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,52,C'ESTIMATE HEADER REPORT'                                  
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,52,C'----------------------'                                  
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SSPEC H4,1,CLIENT                                                      
         SPROG 2,3                                                              
         SSPEC H5,1,PRODUCT                                                     
         SPROG 3,4                                                              
         SSPEC H6,1,ESTIMATE                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPL201 08/29/00'                                      
         END                                                                    
