*          DATA SET PPREP3701  AT LEVEL 006 AS OF 07/18/16                      
*PHASE PP3701A,+0                                                               
         TITLE 'PP3701  PAYERS LIST HEADLINES'                                  
PP3701   CSECT                                                                  
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17                          
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,45,CL19'VENDOR PAYER''S LIST'                                 
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,45,19C'-'                                                     
         PSPEC H2,76,AGYADD                                                     
         PSPEC H3,43,PERIOD                                                     
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
         PSPEC H12,5,9C'-'                                                      
         PSPEC H12,60,7C'-'                                                     
         PSPEC H12,75,7C'-'                                                     
         PSPEC H12,86,8C'-'                                                     
         PSPEC H12,101,7C'-'                                                    
         SPROG 2,3,4,5,6,7,12,13,14,15,16,17                                    
         PSPEC H10,5,C'INSERT'                                                  
         PSPEC H10,14,C'PYABLE'                                                 
         PSPEC H11,6,C'DATE      DATE EST'                                      
         PSPEC H12,5,C'------    ----- ---'                                     
         SPROG 2,3,12,13                                                        
         PSPEC H11,26,C'  SPACE DESCRIPTION       '                             
         PSPEC H12,26,C'  -----------------       '                             
         SPROG 4,5,6,7,14,15,16,17                                              
         PSPEC H10,27,C'UNIT'                                                   
         PSPEC H11,27,C'RATE'                                                   
         PSPEC H10,36,C'SPACE/'                                                 
         PSPEC H11,35,C'PREMIUM'                                                
         PSPEC H12,27,4C'-'                                                     
         PSPEC H12,35,7C'-'                                                     
         SPROG 4,5,14,15                                                        
         PSPEC H11,48,C'LINES'                                                  
         PSPEC H12,48,5C'-'                                                     
         SPROG 6,7,16,17                                                        
         PSPEC H11,47,C'INCHES'                                                 
         PSPEC H12,47,6C'-'                                                     
         SPROG 1,3,5,7,11,13,15,17                                              
         PSPEC H7,1,ESTIMATE                                                    
         SPROG 10,11,12,13,14,15,16,17                                          
         PSPEC H5,1,CLIENT                                                      
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'0102041F0708090A0B0C0D0E0F13181400'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREP3701 07/18/16'                                      
         END                                                                    
