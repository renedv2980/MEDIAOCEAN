*          DATA SET SPREPM601  AT LEVEL 014 AS OF 09/21/81                      
*PHASE SPM601T,+0                                                               
         TITLE 'SPREPM601-PB BRAND PERFORMANCE SPECS'                           
         PRINT NOGEN                                                            
SPM601   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,51,C'POST-BUY BRAND PERFORMANCE REPORT'                       
         SSPEC H2,51,33C'-'                                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,100,RATING                                                    
         SSPEC H4,51,MGROUP                                                     
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H5,100,BOOK                                                      
         SSPEC H7,100,EQUIV                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H9,51,DAYPART                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
         SSPEC H4,1,PGROUP                                                      
         SPROG 0,THRU,6                                                         
         SSPEC H10,1,11C'-'                                                     
         SSPEC H12,1,C'DAYPART-LN'                                              
         SSPEC H13,1,11C'-'                                                     
         SSPEC H10,13,23C'-'                                                    
         SSPEC H12,22,C'DOLLARS'                                                
         SSPEC H13,13,23C'-'                                                    
         SSPEC H10,39,30C'-'                                                    
         SSPEC H12,49,C'DOLLARS'                                                
         SSPEC H11,65,C'NO.'                                                    
         SSPEC H12,64,C'SPOTS'                                                  
         SSPEC H13,39,30C'-'                                                    
         SSPEC H12,86,C'DOLLARS'                                                
         SSPEC H12,80,C'INDX'                                                   
         SSPEC H12,94,C'INDX'                                                   
         SSPEC H11,108,C'NO.'                                                   
         SSPEC H12,107,C'SPOTS'                                                 
         SSPEC H12,113,C'INDX'                                                  
         SPROG 0,THRU,4                                                         
         SSPEC H10,72,61C'-'                                                    
         SSPEC H13,72,61C'-'                                                    
         SPROG 1,2                                                              
         SSPEC H11,1,C'MARKET'                                                  
         SPROG 3,4                                                              
         SSPEC H6,51,C'******** PRODUCT SUMMARY ********'                       
         SPROG 5,6                                                              
         SSPEC H6,51,C'***** PRIMARY DEMO SUMMARY *****'                        
         SPROG 7,8                                                              
         SSPEC H6,51,C'******** CLIENT SUMMARY ********'                        
         SPROG 5,THRU,6                                                         
         SSPEC H11,1,C'PRIMARY DEM'                                             
         SSPEC H11,14,C'---------GOAL--------'                                  
         SSPEC H11,44,C'----PURCHASED----'                                      
         SSPEC H11,77,C'----INVOICED ACHIEVEMENT----'                           
         SSPEC H10,72,45C'-'                                                    
         SSPEC H13,72,45C'-'                                                    
         SPROG 1,THRU,4                                                         
         SSPEC H11,118,C'-DEMOGRAPHICS-'                                        
         SSPEC H11,14,C'----GOAL('                                              
         SSPEC H11,30,C')----'                                                  
         SSPEC H11,44,C'----PURCHASED----'                                      
         SSPEC H11,77,C'----INVOICED ACHIEVEMENT----'                           
         SPROG 1,3,5                                                            
         SSPEC H12,14,C'POINTS'                                                 
         SSPEC H12,31,C'CPP'                                                    
         SSPEC H12,40,C'POINTS'                                                 
         SSPEC H12,58,C'CPP'                                                    
         SSPEC H12,73,C'POINTS'                                                 
         SSPEC H12,101,C'CPP'                                                   
         SPROG 2,4,6                                                            
         SSPEC H12,16,C'IMPS'                                                   
         SSPEC H12,31,C'CPM'                                                    
         SSPEC H12,42,C'IMPS'                                                   
         SSPEC H12,58,C'CPM'                                                    
         SSPEC H12,75,C'IMPS'                                                   
         SSPEC H12,101,C'CPM'                                                   
         SPROG 7,THRU,8                                                         
         SSPEC H12,1,C'DAYPART-LN'                                              
         SSPEC H13,1,C'----------'                                              
         SSPEC H11,24,C'GOAL'                                                   
         SSPEC H12,22,C'DOLLARS'                                                
         SSPEC H13,22,C'-------'                                                
         SSPEC H11,50,C'----PURCHASED----'                                      
         SSPEC H12,48,C'DOLLARS'                                                
         SSPEC H13,48,C'-------'                                                
         SSPEC H12,64,C'SPOTS'                                                  
         SSPEC H13,64,C'-----'                                                  
         SSPEC H12,113,C'INDX'                                                  
         SSPEC H13,113,C'----'                                                  
         SSPEC H12,107,C'SPOTS'                                                 
         SSPEC H13,107,C'-----'                                                 
         SSPEC H12,86,C'DOLLARS INDX'                                           
         SSPEC H13,86,C'------- ----'                                           
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPM601 09/21/81'                                      
         END                                                                    
