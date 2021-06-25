*          DATA SET SPREPLR01  AT LEVEL 029 AS OF 08/29/00                      
*PHASE SPLR01A                                                                  
         TITLE 'SPREPLR01-DEMOGRAPHIC MARKET RANK REPORT'                       
         PRINT NOGEN                                                            
SPLR01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 1                                                                
         SSPEC H1,55,C'MARKET RANK REPORT'                                      
         SSPEC H2,55,18C'-'                                                     
         SPROG 2                                                                
         SSPEC H1,54,C'ALPHA MARKET REPORT'                                     
         SSPEC H2,54,C'-------------------'                                     
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SPROG 1,2                                                              
         SSPEC H6,1,C'MARKET'                                                   
         SSPEC H7,1,C'NUMBER'                                                   
         SSPEC H8,1,C'------'                                                   
         SSPEC H7,9,C'MARKET NAME'                                              
         SSPEC H8,9,C'-----------'                                              
         SSPEC H6,41,C' DMA'                                                    
         SSPEC H7,41,C'RANK'                                                    
         SSPEC H8,41,C'----'                                                    
         SSPEC H6,47,C' SMA'                                                    
         SSPEC H7,47,C'RANK'                                                    
         SSPEC H8,47,C'----'                                                    
         SSPEC H6,53,C'PCT OF'                                                  
         SSPEC H7,53,C'US POP'                                                  
         SSPEC H8,53,C'------'                                                  
         SSPEC H6,65,C'HOMES'                                                   
         SSPEC H7,63,C'UNIVERSE'                                                
         SSPEC H8,63,C'--------'                                                
         SPROG 3                                                                
         SSPEC H7,1,C'STATION'                                                  
         SSPEC H8,1,C'-------'                                                  
         SSPEC H7,10,C'SPILL MARKET'                                            
         SSPEC H8,10,C'------------'                                            
         SSPEC H7,50,C'BOOKS'                                                   
         SSPEC H8,50,C'-----'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPREPLR01 08/29/00'                                      
         END                                                                    
