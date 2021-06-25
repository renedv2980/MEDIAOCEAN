*          DATA SET SPREPM610  AT LEVEL 021 AS OF 08/29/00                      
*PHASE SPM610A                                                                  
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
         SSPEC H11,44,C'GOAL'                                                   
         SSPEC H11,58,C'GOAL'                                                   
         SSPEC H12,44,C'INDX  DOLLARS INDX'                                     
         SSPEC H12,70,C'SPTS'                                                   
         SSPEC H10,37,37C'-'                                                    
         SSPEC H13,37,37C'-'                                                    
         SSPEC H10,75,43C'-'                                                    
         SSPEC H13,75,43C'-'                                                    
         SSPEC H11,82,C'GOAL PRCH'                                              
         SSPEC H11,101,C'GOAL PRCH'                                             
         SSPEC H12,82,C'INDX INDX  DOLLARS INDX INDX'                           
         SPROG 0,THRU,4                                                         
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
         SSPEC H10,51,C'PURCHASED'                                              
         SSPEC H10,91,C'INVOICED ACHIEVEMENT'                                   
         SPROG 1,THRU,4                                                         
         SSPEC H10,118,14C'-'                                                   
         SSPEC H13,118,14C'-'                                                   
         SSPEC H11,118,C'-DEMOGRAPHICS-'                                        
         SSPEC H11,14,C'----GOAL('                                              
         SSPEC H11,30,C')----'                                                  
         SSPEC H10,51,C'PURCHASED'                                              
         SSPEC H10,91,C'INVOICED ACHIEVEMENT'                                   
         SPROG 1,3,5                                                            
         SSPEC H12,14,C'POINTS'                                                 
         SSPEC H12,31,C'CPP'                                                    
         SSPEC H12,37,C'POINTS'                                                 
         SSPEC H12,64,C'CPP'                                                    
         SSPEC H12,75,C'POINTS'                                                 
         SSPEC H12,113,C'CPP'                                                   
         SPROG 2,4,6                                                            
         SSPEC H12,16,C'IMPS'                                                   
         SSPEC H12,31,C'CPM'                                                    
         SSPEC H12,39,C'IMPS'                                                   
         SSPEC H12,64,C'CPM'                                                    
         SSPEC H12,77,C'IMPS'                                                   
         SSPEC H12,113,C'CPM'                                                   
         SPROG 7,THRU,8                                                         
         SSPEC H12,1,C'DAYPART-LN'                                              
         SSPEC H13,1,C'----------'                                              
         SSPEC H11,24,C'GOAL'                                                   
         SSPEC H12,22,C'DOLLARS'                                                
         SSPEC H13,22,C'-------'                                                
         SSPEC H10,51,C'PURCHASED'                                              
         SSPEC H10,91,C'INVOICED ACHIEVEMENT'                                   
         SSPEC H11,59,C'GOAL'                                                   
         SSPEC H12,50,C'DOLLARS INDX'                                           
         SSPEC H11,101,C'GOAL PRCH'                                             
         SSPEC H12,93,C'DOLLARS INDX INDX'                                      
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREPM610 08/29/00'                                      
         END                                                                    
