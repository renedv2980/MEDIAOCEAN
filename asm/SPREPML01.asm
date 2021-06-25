*          DATA SET SPREPML01  AT LEVEL 010 AS OF 12/12/86                      
*PHASE SPML01T,+0,NOAUTO                                                        
         TITLE 'SPREPML01-LOCKIN REPORT SPECS'                                  
         PRINT NOGEN                                                            
SPML01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,STATION                                                      
         FSPEC GET,MARKET                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC OPEN,DEMFILES                                                    
         SPROG 1,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,55,C'SPOTPAK LOCKIN REPORT'                                   
         SSPEC H2,55,21C'-'                                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,51,MARKET                                                     
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H8,1,ESTIMATE                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
         SPROG 1,2                                                              
         SSPEC H11,01,C'PRODUCT'                                                
         SSPEC H12,1,C'-------'                                                 
         SSPEC H11,30,C'EST'                                                    
         SSPEC H12,30,C'---'                                                    
         SSPEC H11,34,C'DPT'                                                    
         SSPEC H12,34,C'---'                                                    
         SSPEC H10,38,C'SPOT'                                                   
         SSPEC H11,38,C'LEN'                                                    
         SSPEC H12,38,C'----'                                                   
         SSPEC H10,43,C'PRD'                                                    
         SSPEC H11,43,C'LEN'                                                    
         SSPEC H12,43,C'---'                                                    
         SSPEC H11,60,C'DOLLARS'                                                
         SSPEC H12,60,C'-------'                                                
         SSPEC H11,69,C'SPOTS'                                                  
         SSPEC H12,69,C'-----'                                                  
         SSPEC H11,78,C'DEMO'                                                   
         SSPEC H12,78,C'----'                                                   
         SPROG 1                                                                
         SSPEC H11,48,C'WEEK OF'                                                
         SSPEC H12,48,C'-------'                                                
         SPROG 2                                                                
         SSPEC H11,48,C'MON. OF'                                                
         SSPEC H12,48,C'-------'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPML01 12/12/86'                                      
         END                                                                    
