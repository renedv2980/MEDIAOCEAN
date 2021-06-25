*          DATA SET SPREPDR01  AT LEVEL 017 AS OF 08/29/00                      
*PHASE SPDR01A                                                                  
         TITLE 'SPREPDR01 - DIRECT RESPONSE REPORT'                             
         PRINT NOGEN                                                            
SPDR01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,44,C'DIRECT RESPONSE REPORT'                                  
         SSPEC H2,44,C'----------------------'                                  
         SSPEC H1,79,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,79,AGYADD                                                     
         SSPEC H3,43,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,79,RATING                                                     
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H5,79,BOOK                                                       
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,43,MGROUP                                                     
         SSPEC H4,1,PGROUP                                                      
         SSPEC H6,79,EQUIV                                                      
         SSPEC H9,43,DAYPART                                                    
         SSPEC H7,79,REPORT                                                     
         SSPEC H8,79,PAGE                                                       
         SPROG 1,THRU,3                                                         
         SSPEC H12,1,C'DAYPART TIME'                                            
         SSPEC H13,1,C'------- ----'                                            
         SSPEC H12,21,C'DAY'                                                    
         SSPEC H13,21,C'---'                                                    
         SSPEC H12,32,C'DATE'                                                   
         SSPEC H13,32,C'----'                                                   
         SSPEC H12,39,C'ACTUAL PROGRAM'                                         
         SSPEC H13,39,C'--------------'                                         
         SSPEC H12,56,C'COST/SPOT'                                              
         SSPEC H13,56,C'---------'                                              
         SSPEC H12,66,C'RTG'                                                    
         SSPEC H13,66,C'---'                                                    
         SSPEC H12,70,C'RESPONSES'                                              
         SSPEC H13,70,C'---------'                                              
         SSPEC H12,80,C'$/RESP  VIEWERS'                                        
         SSPEC H13,80,C'------  ------- '                                       
         SSPEC H11,80,C'       RESP/1000'                                       
         SSPEC H12,97,C'VIEWERS'                                                
         SSPEC H13,97,C'-------'                                                
         SPROG 4,THRU,8                                                         
         SSPEC H11,10,C'START'                                                  
         SSPEC H11,21,C'END'                                                    
         SSPEC H11,40,C'COST'                                                   
         SSPEC H11,71,C'RESP/1000'                                              
         SSPEC H12,1,C'DAYPART'                                                 
         SSPEC H13,1,C'-------'                                                 
         SSPEC H12,11,C'DATE'                                                   
         SSPEC H13,10,C'-----'                                                  
         SSPEC H12,20,C'DATE'                                                   
         SSPEC H13,20,C'----'                                                   
         SSPEC H11,31,C'TOTAL'                                                  
         SSPEC H12,31,C' COST'                                                  
         SSPEC H13,31,C'-----'                                                  
         SSPEC H12,38,C'PER SPOT'                                               
         SSPEC H13,38,C'--------'                                               
         SSPEC H12,50,C'RTG'                                                    
         SSPEC H13,50,C'---'                                                    
         SSPEC H12,54,C'RESPONSES'                                              
         SSPEC H13,54,C'---------'                                              
         SSPEC H12,64,C'$/RESP'                                                 
         SSPEC H13,64,C'------'                                                 
         SSPEC H12,72,C'VIEWERS'                                                
         SSPEC H13,72,C'-------'                                                
         SSPEC H12,81,C'VIEWERS'                                                
         SSPEC H13,81,C'-------'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPDR01 08/29/00'                                      
         END                                                                    
