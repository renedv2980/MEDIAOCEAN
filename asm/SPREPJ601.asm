*          DATA SET SPREPJ601  AT LEVEL 018 AS OF 08/12/02                      
*PHASE SPJ601A                                                                  
         TITLE 'SPJ601 - DAILY MEDIA SCHEDULE - SPECS'                          
SPJ601   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
                                                                                
         SPROG 0,THRU,99                                                        
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,C'REQUESTOR'                                                
         SSPEC H2,11,QUESTOR                                                    
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H6,50,MARKET                                                     
*                                                                               
         SSPEC H1,56,C'DAILY TIME SCHEDULE'                                     
         SSPEC H2,56,C'-------------------'                                     
         SSPEC H3,50,PERIOD                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADDR                                                   
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SSPEC H6,100,REPORT                                                    
         SSPEC H6,123,PAGE                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPREPJ601 08/12/02'                                      
         END                                                                    
