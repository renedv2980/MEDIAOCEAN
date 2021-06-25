*          DATA SET SPREPMF01  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPMF01A                                                                  
         TITLE 'SPMF01 - SPECS FOR COMMERCIAL USAGE'                            
SPMF01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,10,C'ALL PRODUCTS'                                            
         SSPEC H6,10,C'ALL ESTIMATES'                                           
         SSPEC H1,50,C'SPOT T.V. COMMERCIAL USAGE'                              
         SSPEC H2,50,26C'-'                                                     
         SSPEC H4,47,PERIOD                                                     
         SSPEC H6,47,MARKET                                                     
         SSPEC H4,100,REPORT                                                    
         SSPEC H6,100,PAGE                                                      
         SSPEC H09,1,C'COMMERCIAL  PRODUCT'                                     
         SSPEC H10,1,C'---CODE---  -------'                                     
         SSPEC H09,125,C'TOTAL'                                                 
         SSPEC H10,125,C'-----'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPMF01 08/29/00'                                      
         END                                                                    
