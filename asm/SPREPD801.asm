*          DATA SET SPREPD801  AT LEVEL 014 AS OF 02/09/79                      
*PHASE SPD801T,+0                                                               
         TITLE 'SPREPD801-SWEEP REPORT SPECS'                                   
         PRINT NOGEN                                                            
SPD801   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,60,C'SWEEP REPORT'                                            
         SSPEC H2,60,12C'-'                                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,100,RATING                                                    
         SSPEC H4,51,MGROUP                                                     
         SSPEC H5,100,BOOK                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H7,100,EQUIV                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H9,51,DAYPART                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
         SSPEC H9,99,C'*=MANUAL OVERRIDE'                                       
         SSPEC H10,99,C'X=DROP GREATER THAN 15 PERCENT'                         
         SSPEC H12,1,C'EST-LIN BUY PERIOD  WKS'                                 
         SSPEC H12,33,C'N/W TIME'                                               
         SSPEC H12,49,C'SPOTS'                                                  
         SSPEC H13,1,53C'-'                                                     
         SSPEC H14,5,C'DPT'                                                     
         SSPEC H14,9,C'LNG EST/ACH PROGRAM     COST  EST BOOK'                  
         SSPEC H12,58,C'COST/DEMO'                                              
         SSPEC H13,56,C'EST  VS.  ACH'                                          
         SSPEC H14,56,C'---       ---'                                          
         SSPEC H13,77,C'DEMO'                                                   
         SSPEC H14,77,C'----'                                                   
         SSPEC H12,95,C'POINTS'                                                 
         SSPEC H13,93,C'EST  V  ACH'                                            
         SSPEC H14,93,C'---     ---'                                            
         SSPEC H12,108,C'SVI FOR'                                               
         SSPEC H13,107,C'EST   ACH'                                             
         SSPEC H14,107,C'---   ---'                                             
         SSPEC H12,120,C'VARIANCE'                                              
         SSPEC H13,120,C'RAW   PCT'                                             
         SSPEC H14,120,C'---   ---'                                             
         SPROG 1,2                                                              
         SSPEC H4,51,MKTGRP                                                     
         SSPEC H11,1,C'MARKET'                                                  
         SPROG 3,4                                                              
         SSPEC H6,51,C'******** PRODUCT SUMMARY ********'                       
         SPROG 5,6                                                              
         SSPEC H6,51,C'***** PRIMARY DEMO SUMMARY *****'                        
         SPROG 7,8                                                              
         SSPEC H6,51,C'******** CLIENT SUMMARY ********'                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPD801 02/09/79'                                      
         END                                                                    
