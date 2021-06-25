*          DATA SET SPREPT901  AT LEVEL 007 AS OF 02/11/03                      
*PHASE SPT901A                                                                  
         TITLE 'SPREPT901 - TRAFFIC PLAN SPECS'                                 
         PRINT NOGEN                                                            
SPT901   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SSPEC H1,55,C'T R A F F I C  P L A N'                                  
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,55,22C'-'                                                     
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,50,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H4,51,MGROUP                                                     
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
         SSPEC H8,2,C'START'                                                    
         SSPEC H9,2,C'WEEK'                                                     
         SSPEC H10,2,5C'-'                                                      
         SSPEC H9,9,C'MARKET'                                                   
         SSPEC H10,9,C'------'                                                  
         SSPEC H8,41,C'MAXIMUM'                                                 
         SSPEC H9,41,C'STATIONS'                                                
         SSPEC H10,41,C'--------'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPT901 02/11/03'                                      
         END                                                                    
