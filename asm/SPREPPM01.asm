*          DATA SET SPREPPM01  AT LEVEL 016 AS OF 10/01/01                      
*PHASE SPPM01A                                                                  
         TITLE 'SPPM01 - PARAMOUNT MEDIA SCHEDULE - SPECS'                      
SPPM01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
                                                                                
         SPROG 0,THRU,99                                                        
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,1,PRODUCT                                                     
         SSPEC H4,1,ESTIMATE                                                    
         SSPEC H5,1,MARKET                                                      
*                                                                               
         SSPEC H1,51,C'PARAMOUNT MEDIA SCHEDULE REPORT'                         
         SSPEC H2,51,C'-------------------------------'                         
         SSPEC H3,50,PERIOD                                                     
**NOP**  SSPEC H5,56,C'STATION GROSS DOLLARS'                                   
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,REPORT                                                    
         SSPEC H3,100,PAGE                                                      
         SSPEC H4,100,C'BUYER:'                                                 
***>>>>>>      H4,107,QUESTOR                                                   
*                                                                               
         SSPEC H6,100,RATING                                                    
         SSPEC H7,100,BOOK                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPPM01 10/01/01'                                      
         END                                                                    
