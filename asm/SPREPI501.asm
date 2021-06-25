*          DATA SET SPREPI501  AT LEVEL 021 AS OF 09/14/00                      
*PHASE SPI501A                                                                  
         TITLE 'SPI501'                                                         
SPI501   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC READ,BUYS                                                        
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,PAGE                                                       
         SSPEC H5,97,REPORT                                                     
         SSPEC H1,50,C'SPOTPAK INVOICE RECORD LISTING'                          
         SSPEC H2,50,C'------------------------------'                          
         SSPEC H6,50,MARKET                                                     
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,1,PRODUCT                                                     
         SSPEC H4,1,ESTIMATE                                                    
*                                                                               
         SSPEC H10,39,C'DAY-'                                                   
        SSPEC H11,02,C'  PRODUCT   EST  LENGTH   SPOT COST  PART  WEEK X        
                            DATE  DAY   TIME   FILM CODE'                       
         SSPEC H12,2,C'  -------   ---  ------   ---------  ----  -----X        
               ---         -----  ---   ----   ---------'                       
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREPI501 09/14/00'                                      
         END                                                                    
