*          DATA SET SPREPJV01  AT LEVEL 001 AS OF 05/17/05                      
*PHASE SPJV01A                                                                  
         TITLE 'SPJV01 - SPOTPAK DEMO TRACE - SPECS'                            
SPJV01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
                                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,C'REQUESTOR'                                                
         SSPEC H2,11,QUESTOR                                                    
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
**NOP**        H6,50,MARKET                                                     
**NOP**        H7,50,STATION                                                    
*                                                                               
         SSPEC H1,54,C'SPOTPAK DEMO SOURCE TRACE'                               
         SSPEC H2,54,C'-------------------------'                               
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
**PAN#1  DC    CL21'001SPREPJV01 05/17/05'                                      
         END                                                                    
