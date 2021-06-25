*          DATA SET SPREPI301  AT LEVEL 012 AS OF 08/29/00                      
*PHASE SPI301A                                                                  
         TITLE 'SPREPI301 - INVOICE CHECKING SPECS'                             
SPI301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,PACKAGES                                                    
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,97,AGYADD                                                     
         SSPEC H3,1,PAGE                                                        
         SSPEC H3,97,REPORT                                                     
         SSPEC H3,50,PERIOD                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H5,50,MARKET                                                     
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SSPEC H10,1,C'EST-LIN BUY PERIOD WKS  DAY'                             
         SSPEC H10,34,C'N/W TIME'                                               
         SSPEC H10,49,C'DP LEN PROGRAMMING'                                     
         SSPEC H10,80,C'COST'                                                   
         SSPEC H10,87,C'REMARKS'                                                
         SSPEC H10,97,C'GROSS TOTAL'                                            
         SSPEC H10,111,C'NET TOTAL'                                             
         SSPEC H12,48,C'S P O T     S C H E D U L E'                            
         SSPEC H1,57,C'INVOICE CHECKING LIST'                                   
         SSPEC H2,57,C'---------------------'                                   
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPI301 08/29/00'                                      
         END                                                                    
