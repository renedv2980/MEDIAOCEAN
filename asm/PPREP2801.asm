*          DATA SET PPREP2801  AT LEVEL 017 AS OF 07/18/16                      
*PHASE PP2801A                                                                  
         TITLE 'PP2801  PAYERS LIST SUMMARY'                                    
PP2801   CSECT                                                                  
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,2,3,4,5                                                      
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,44,C'CLIENT/VENDOR SUMMARY'                                   
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,44,21C'-'                                                     
         PSPEC H2,76,AGYADD                                                     
         PSPEC H3,43,PERIOD                                                     
         PSPEC H5,1,CLIENT                                                      
         PSPEC H5,76,RUN                                                        
         PSPEC H6,76,REPORT                                                     
         PSPEC H6,101,PAGE                                                      
         PSPEC H10,61,C'GROSS'                                                  
         PSPEC H10,77,C'NET'                                                    
         PSPEC H10,88,C'CASH'                                                   
         PSPEC H10,103,C'NET'                                                   
         PSPEC H11,7,C'MONTH'                                                   
         PSPEC H11,60,C'ORDERED'                                                
         PSPEC H11,75,C'ORDERED'                                                
         PSPEC H11,86,C'DISCOUNT'                                               
         PSPEC H11,101,C'PAYABLE'                                               
         PSPEC H12,5,10C'-'                                                     
         PSPEC H12,60,7C'-'                                                     
         PSPEC H12,75,7C'-'                                                     
         PSPEC H12,86,8C'-'                                                     
         PSPEC H12,101,7C'-'                                                    
         SPROG 2,3                                                              
         PSPEC H11,47,C'LINES'                                                  
         PSPEC H12,47,5C'-'                                                     
         SPROG 4,5                                                              
         PSPEC H11,46,C'INCHES'                                                 
         PSPEC H12,46,6C'-'                                                     
         SPROG 1,3,5                                                            
         PSPEC H7,1,ESTIMATE                                                    
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    X'0102041F0708090A0B0C0D0E0F13191400'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPREP2801 07/18/16'                                      
         END                                                                    
