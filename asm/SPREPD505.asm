*          DATA SET SPREPD505  AT LEVEL 006 AS OF 08/29/00                      
*PHASE SPD505A                                                                  
         TITLE 'SPREPD501-PTS,PRS SPECS'                                        
         PRINT NOGEN                                                            
SPD505   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC READ,PACKAGES                                                    
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,97,AGYADD                                                     
         SSPEC H3,1,PAGE                                                        
         SSPEC H3,97,REPORT                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H5,1,PGROUP                                                      
         SSPEC H8,97,MARKET                                                     
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPD505 08/29/00'                                      
         END                                                                    
