*          DATA SET SPREP4801  AT LEVEL 019 AS OF 11/13/96                      
*PHASE SP4801A,*,NOAUTO                                                         
         TITLE 'SP4801 - STATION FILE LISTING - PRINT SPECS'                    
SP4801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP4803                                                       
         SPROG 1,2,3,4,5,6                                                      
         SSPEC H1,56,C'STATION FILE LISTING'                                    
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,56,C'------- ---- -------'                                    
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,3,PAGE                                                        
         SSPEC H3,100,REPORT                                                    
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H4,57,C'REP ADDRESS RECORDS'                                     
         SSPEC H6,3,C'REP'                                                      
         SSPEC H7,3,C'---'                                                      
         SSPEC H6,9,C'NAME'                                                     
         SSPEC H7,9,C'----'                                                     
         SSPEC H6,35,C'ADDRESS'                                                 
         SSPEC H7,35,C'-------'                                                 
         SSPEC H6,61,C'CITY'                                                    
         SSPEC H7,61,C'----'                                                    
         SSPEC H6,87,C'ST'                                                      
         SSPEC H7,87,C'--'                                                      
         SSPEC H6,92,C'ZIP'                                                     
         SSPEC H7,92,C'---'                                                     
         SSPEC H6,105,C'MARKET/STATION'                                         
         SSPEC H7,105,C'--------------'                                         
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H4,59,C'MARKET RECORDS'                                          
         SSPEC H6,3,C'MKT'                                                      
         SSPEC H7,3,C'---'                                                      
         SSPEC H6,10,C'RANK'                                                    
         SSPEC H7,10,C'----'                                                    
         SSPEC H6,16,C'NAME'                                                    
         SSPEC H7,16,C'----'                                                    
         SSPEC H6,42,C'ALPHA'                                                   
         SSPEC H7,42,C'-----'                                                   
         SSPEC H6,51,C'TZ'                                                      
         SSPEC H7,51,C'--'                                                      
         SSPEC H6,57,C'STATION'                                                 
         SSPEC H7,57,C'-------'                                                 
         SSPEC H6,94,C'ACC'                                                     
         SSPEC H7,94,C'---'                                                     
         SSPEC H6,99,C'WT.'                                                     
         SSPEC H7,99,C'---'                                                     
         SSPEC H6,105,C'SHARE'                                                  
         SSPEC H7,105,C'-----'                                                  
         SSPEC H6,112,C'HOMES'                                                  
         SSPEC H7,112,C'-----'                                                  
         SSPEC H6,122,C'REG'                                                    
         SSPEC H7,122,C'---'                                                    
         SSPEC H6,127,C'NTA'                                                    
         SSPEC H7,127,C'---'                                                    
         SPACE 1                                                                
         SPROG 3                                                                
         SSPEC H4,59,C'STATION RECORDS'                                         
         SSPEC H6,3,C'STA'                                                      
         SSPEC H7,3,C'---'                                                      
         SSPEC H6,10,C'CLT'                                                     
         SSPEC H7,10,C'---'                                                     
         SSPEC H6,14,C'MKT'                                                     
         SSPEC H7,14,C'---'                                                     
         SSPEC H6,19,C'MARKET NAME'                                             
         SSPEC H7,19,C'------ ----'                                             
         SSPEC H6,40,C'REP(S)'                                                  
         SSPEC H7,40,C'------'                                                  
         SSPEC H6,52,C'T'                                                       
         SSPEC H7,52,C'-'                                                       
         SSPEC H6,54,C'STATION NAME'                                            
         SSPEC H7,54,C'------- ----'                                            
         SSPEC H6,75,C'STATION ADDRESS'                                         
         SSPEC H7,75,C'------- -------'                                         
         SSPEC H6,100,C'CITY'                                                   
         SSPEC H7,100,C'----'                                                   
         SSPEC H6,119,C'ST'                                                     
         SSPEC H7,119,C'--'                                                     
         SSPEC H6,123,C'ZIP'                                                    
         SSPEC H7,123,C'---'                                                    
         SPACE 1                                                                
         SPROG 4                                                                
         SSPEC H4,60,C'RECORD TOTALS'                                           
         SPROG 5                                                                
         SSPEC H4,55,C'STATION BY SIZE ANALYSIS'                                
         SSPEC H6,3,C'SIZE'                                                     
         SSPEC H7,3,C'----'                                                     
         SSPEC H6,10,C'STATIONS'                                                
         SSPEC H7,10,C'--------'                                                
         SPROG 6                                                                
         SSPEC H4,52,C'STATION BY AFFILIATE ANALYSIS'                           
         SSPEC H6,3,C'AFF'                                                      
         SSPEC H7,3,C'---'                                                      
         SSPEC H6,10,C'STATIONS'                                                
         SSPEC H7,10,C'--------'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPREP4801 11/13/96'                                      
         END                                                                    
