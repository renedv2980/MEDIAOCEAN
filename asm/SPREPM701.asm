*          DATA SET SPREPM701  AT LEVEL 001 AS OF 07/09/81                      
*PHASE SPM701T,+0                                                               
         TITLE 'SPREPM701,PB MARKET PREFORMANCE SPECS'                          
         PRINT NOGEN                                                            
SPM701   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,50,C'POST-BUY MARKET PERFORMANCE REPORT'                      
         SSPEC H2,50,33C'-'                                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,50,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H7,100,EQUIV                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SSPEC H9,50,DAYPART                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
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
         SSPEC H4,50,MGROUP                                                     
         SPROG 0,THRU,4                                                         
         SSPEC H10,72,61C'-'                                                    
         SSPEC H13,72,61C'-'                                                    
         SPROG 5,6                                                              
         SSPEC H7,50,C'***** PRIMARY DEMO SUMMARY *****'                        
         SPROG 7,8                                                              
         SSPEC H6,50,C'******** CLIENT SUMMARY ********'                        
         SPROG 5,THRU,6                                                         
         SSPEC H11,1,C'PRIMARY DEM'                                             
         SSPEC H11,14,C'---------GOAL--------'                                  
         SSPEC H11,44,C'----PURCHASED----'                                      
         SSPEC H11,77,C'----INVOICED ACHIEVEMENT----'                           
         SSPEC H10,72,45C'-'                                                    
         SSPEC H13,72,45C'-'                                                    
         SPROG 1,THRU,4                                                         
         SSPEC H11,1,C'PRODUCT'                                                 
         SSPEC H11,118,C'-DEMOGRAPHICS-'                                        
         SSPEC H11,14,20C'-'                                                    
         SSPEC H11,44,C'----PURCHASED----'                                      
         SSPEC H11,77,C'----INVOICED ACHIEVEMENT----'                           
         SPROG 1,THRU,8                                                         
         SSPEC H12,15,C'DEMO'                                                   
         SSPEC H12,30,C'CPP/M'                                                  
         SSPEC H12,41,C'DEMO'                                                   
         SSPEC H12,57,C'CPP/M'                                                  
         SSPEC H12,74,C'DEMO'                                                   
         SSPEC H12,100,C'CPP/M'                                                 
         SPROG 1,THRU,4                                                         
         SSPEC H12,121,C'DEMO'                                                  
         SSPEC H12,126,C'CPP/M'                                                 
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPM701 07/09/81'                                      
         END                                                                    
