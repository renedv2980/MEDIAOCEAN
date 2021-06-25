*          DATA SET SPREPPR01  AT LEVEL 001 AS OF 05/16/01                      
*PHASE SPPR01                                                                   
         TITLE 'SPPR01 - PARAMOUNT REPORT - SPECS'                              
SPPR01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
                                                                                
         SSPEC H1,1,REPORT                                                      
         SSPEC H1,47,C'PARAMOUNT PICTURES OVERALL MEDIA RECAP'                  
         SSPEC H2,47,C'--------------------------------------'                  
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H1,123,PAGE                                                      
                                                                                
         SSPEC H3,1,C'AGENCY'                                                   
         SSPEC H3,14,AGYNAME                                                    
         SSPEC H4,1,C'MEDIA'                                                    
         SSPEC H4,14,MEDIA                                                      
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SSPEC H8,1,MARKET                                                      
                                                                                
         SSPEC H4,50,C'-TARGET AUDIENCE-    #TRPS'                              
                                                                                
         SSPEC H3,100,C'TRP GOALS:'                                             
         SSPEC H4,100,C'BUDGET $$:'                                             
         SSPEC H5,100,C'TRP ACHIEVED:'                                          
         SSPEC H6,100,C'ACTUAL $$:'                                             
         SSPEC H7,100,C'OVERALL INDEX TRPS:'                                    
         SSPEC H8,100,C'OVERALL INDEX $$:'                                      
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPPR01 05/16/01'                                      
         END                                                                    
