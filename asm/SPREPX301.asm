*          DATA SET SPREPX301  AT LEVEL 016 AS OF 08/16/83                      
*PHASE SPX301T,*,NOAUTO                                                         
         TITLE 'SPREPX301 - CHILD PERFORMANCE SPECS'                            
         PRINT NOGEN                                                            
SPX301   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,64,C'PERFORMANCE REPORT'                                      
         SSPEC H2,53,29C'-'                                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,100,RATING                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H4,51,MGROUP                                                     
         SSPEC H5,100,BOOK                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,100,EQUIV                                                     
         SSPEC H8,1,ESTIMATE                                                    
         SSPEC H8,51,DAYPART                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
         SSPEC H13,11,C'DAYPART-LN'                                             
         SSPEC H12,36,C'-EXPENDITURE-'                                          
         SSPEC H13,36,C'PAY       NTP'                                          
         SSPEC H12,54,C'-----------DELIVERED------------'                       
         SSPEC H13,54,C'         DOLLARS     CPM   SPOTS'                       
         SPROG 1,2                                                              
         SSPEC H12,1,C'MARKET'                                                  
         SSPEC H13,1,C'STATION'                                                 
         SPROG 3,4                                                              
         SSPEC H6,51,C'******** PRODUCT SUMMARY ********'                       
         SPROG 5,6                                                              
         SSPEC H6,51,C'***** PRIMARY DEMO SUMMARY *****'                        
         SPROG 7,8                                                              
         SSPEC H6,51,C'******** CLIENT SUMMARY ********'                        
         SPROG 5,THRU,6                                                         
         SSPEC H11,1,C'PRIMARY DEM'                                             
         SPROG 1,THRU,4                                                         
         SSPEC H10,87,44C'-'                                                    
         SSPEC H11,102,C'--DEMOGRAPHICS--'                                      
         SSPEC H13,87,44C'-'                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPX301 08/16/83'                                      
         END                                                                    
