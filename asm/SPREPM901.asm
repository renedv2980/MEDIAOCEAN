*          DATA SET SPREPM901  AT LEVEL 008 AS OF 07/12/94                      
*PHASE SPM901A                                                                  
         TITLE 'PRINT SPECS FOR BRAND MEDIA PLAN'                               
SPM901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,50,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H4,100,PAGE                                                      
         SSPEC H6,100,REPORT                                                    
         SSPEC H1,58,C'BRAND MEDIA PLAN'                                        
         SSPEC H4,50,MGROUP                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPM901 07/12/94'                                      
         END                                                                    
