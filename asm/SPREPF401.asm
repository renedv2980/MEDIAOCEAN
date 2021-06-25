*          DATA SET SPREPF401  AT LEVEL 027 AS OF 10/31/84                      
*PHASE SPF401T,+0,NOAUTO                                                        
         TITLE 'SPREPF402-BRAND/MARKET COMMERCIAL PERFORMANCE SPECS'            
SPF401   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,46,C'BRAND/MARKET COMMERCIAL PERFORMANCE REPORT'              
         SSPEC H2,46,C'------------------------------------------'              
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,51,MGROUP                                                     
         SSPEC H4,100,RATING                                                    
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H5,100,BOOK                                                      
         SSPEC H7,100,EQUIV                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H9,51,DAYPART                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
         SPROG 0,THRU,6                                                         
         SSPEC H10,1,11C'-'                                                     
         SSPEC H10,12,23C'-'                                                    
         SSPEC H10,39,35C'-'                                                    
         SSPEC H10,76,9C'-'                                                     
         SSPEC H11,1,C'COMMERCIAL'                                              
         SSPEC H11,71,C'AVE  PCT'                                               
         SSPEC H11,80,C'ACHMT'                                                  
         SSPEC H12,1,C'DAYPART-LN'                                              
         SSPEC H12,20,C'DOLLARS'                                                
         SSPEC H12,45,C'DOLLARS'                                                
         SSPEC H12,64,C'SPOTS'                                                  
         SSPEC H12,81,C'DOLS'                                                   
         SSPEC H13,1,11C'-'                                                     
         SSPEC H13,12,23C'-'                                                    
         SSPEC H13,39,35C'-'                                                    
         SSPEC H13,76,9C'-'                                                     
         SPROG 1,2                                                              
         SSPEC H11,1,C'COMMERCIAL'                                              
         SPROG 3,4                                                              
         SSPEC H6,51,C'******** PRODUCT SUMMARY ********'                       
         SPROG 5,6                                                              
         SSPEC H6,51,C'***** PRIMARY DEMO SUMMARY *****'                        
         SPROG 7,8                                                              
         SSPEC H6,51,C'******** CLIENT SUMMARY ********'                        
         SPROG 5,THRU,6                                                         
         SSPEC H11,1,C'PRIMARY DEM'                                             
         SSPEC H11,17,C'----GOAL----'                                           
         SSPEC H11,42,C'----PURCHASED---'                                       
         SPROG 1,THRU,4                                                         
         SSPEC H10,87,44C'-'                                                    
         SSPEC H11,13,C'----GOAL('                                              
         SSPEC H11,29,C')----'                                                  
         SSPEC H11,42,C'----'                                                   
         SSPEC H11,54,C'----'                                                   
         SSPEC H13,87,44C'-'                                                    
         SPROG 1,3,5                                                            
         SSPEC H12,14,C'PNTS'                                                   
         SSPEC H12,31,C'CPP'                                                    
         SSPEC H12,39,C'PNTS'                                                   
         SSPEC H12,56,C'CPP'                                                    
         SSPEC H12,71,C'PTS  PNTS'                                              
         SPROG 2,4,6                                                            
         SSPEC H12,14,C'IMPS'                                                   
         SSPEC H12,31,C'CPM'                                                    
         SSPEC H12,39,C'IMPS'                                                   
         SSPEC H12,56,C'CPM'                                                    
         SSPEC H12,70,C'IMPS  IMPS'                                             
         SPROG 7,THRU,8                                                         
         SSPEC H11,22,C'GOAL'                                                   
         SSPEC H12,20,C'DOLLARS'                                                
         SSPEC H13,20,C'-------'                                                
         SSPEC H11,44,C'PURCHASED'                                              
         SSPEC H12,45,C'DOLLARS'                                                
         SSPEC H13,45,C'-------'                                                
         SSPEC H12,1,C'DAYPART-LN'                                              
         SSPEC H13,1,C'----------'                                              
         SSPEC H12,64,C'SPOTS'                                                  
         SSPEC H13,64,C'-----'                                                  
         SSPEC H11,81,C'PCNT'                                                   
         SSPEC H12,81,C'DOLS'                                                   
         SSPEC H13,81,C'----'                                                   
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPREPF401 10/31/84'                                      
         END                                                                    
