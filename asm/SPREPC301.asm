*          DATA SET SPREPC301  AT LEVEL 014 AS OF 04/21/99                      
*PHASE SPC301A                                                                  
         TITLE 'SPC301 - SPOT BUY COS2 UPDATE'                                  
SPC301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,24,C'BUY COS2 UPDATE'                                         
         SSPEC H2,24,C'---------------'                                         
         SSPEC H1,60,REPORT                                                     
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H1,3,PAGE                                                        
         SSPEC H4,3,C'MED'                                                      
         SSPEC H5,3,C'---'                                                      
         SSPEC H4,7,C'CLT'                                                      
         SSPEC H5,7,C'---'                                                      
         SSPEC H4,13,C'MKT'                                                     
         SSPEC H5,13,C'---'                                                     
         SSPEC H4,19,C'STATION'                                                 
         SSPEC H5,19,C'-------'                                                 
         SSPEC H4,27,C'EST-LINE'                                                
         SSPEC H5,27,C'--------'                                                
         SSPEC H4,37,C'OLD COS2'                                                
         SSPEC H5,37,C'--------'                                                
         SSPEC H4,46,C'NEW COS2'                                                
         SSPEC H5,46,C'--------'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPC301 04/21/99'                                      
         END                                                                    
