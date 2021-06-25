*          DATA SET SPREPM710  AT LEVEL 004 AS OF 10/07/88                      
*PHASE SPM710T,+0                                                               
         TITLE 'SPREPM710,PB MARKET PREFORMANCE SPECS'                          
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
         SSPEC H11,44,C'GOAL'                                                   
         SSPEC H11,58,C'GOAL'                                                   
         SSPEC H12,44,C'INDX  DOLLARS INDX'                                     
         SSPEC H12,70,C'SPTS'                                                   
         SSPEC H10,37,35C'-'                                                    
         SSPEC H13,37,35C'-'                                                    
         SSPEC H10,75,43C'-'                                                    
         SSPEC H13,75,43C'-'                                                    
         SSPEC H11,82,C'GOAL PRCH'                                              
         SSPEC H11,101,C'GOAL PRCH'                                             
         SSPEC H12,82,C'INDX INDX  DOLLARS INDX INDX'                           
         SSPEC H4,50,MGROUP                                                     
         SPROG 0,THRU,4                                                         
         SSPEC H10,75,58C'-'                                                    
         SSPEC H13,75,58C'-'                                                    
         SPROG 5,6                                                              
         SSPEC H7,50,C'***** PRIMARY DEMO SUMMARY *****'                        
         SPROG 7,8                                                              
         SSPEC H6,50,C'******** CLIENT SUMMARY ********'                        
         SPROG 5,THRU,6                                                         
         SSPEC H11,1,C'PRIMARY DEM'                                             
         SSPEC H11,14,C'---------GOAL--------'                                  
         SSPEC H10,51,C'----PURCHASED----'                                      
         SSPEC H10,91,C'----INVOICED ACHIEVEMENT----'                           
         SSPEC H10,75,43C'-'                                                    
         SSPEC H13,75,43C'-'                                                    
         SPROG 1,THRU,4                                                         
         SSPEC H11,1,C'PRODUCT'                                                 
         SSPEC H11,118,C'-DEMOGRAPHICS-'                                        
         SSPEC H11,14,20C'-'                                                    
         SSPEC H10,51,C'----PURCHASED----'                                      
         SSPEC H10,91,C'----INVOICED ACHIEVEMENT----'                           
         SPROG 1,THRU,8                                                         
         SSPEC H12,15,C'DEMO'                                                   
         SSPEC H12,30,C'CPP/M'                                                  
         SSPEC H12,39,C'DEMO'                                                   
         SSPEC H12,64,C'CPP/M'                                                  
         SSPEC H12,76,C'DEMO'                                                   
         SSPEC H12,113,C'CPP/M'                                                 
         SPROG 1,THRU,4                                                         
         SSPEC H12,121,C'DEMO'                                                  
         SSPEC H12,126,C'CPP/M'                                                 
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPM710 10/07/88'                                      
         END                                                                    
