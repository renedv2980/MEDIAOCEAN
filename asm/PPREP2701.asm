*          DATA SET PPREP2701  AT LEVEL 013 AS OF 07/18/16                      
*PHASE PP2701A,+0                                                               
         TITLE 'PP2701  PAYERS LIST HEADLINES'                                  
PP2701   CSECT                                                                  
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,2,3,4,5,6,7,8,10,11,12,13,14,15                                
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,48,CL12'PAYER''S LIST'                                        
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,48,12C'-'                                                     
         PSPEC H3,43,PERIOD                                                     
         PSPEC H5,1,CLIENT                                                      
         PSPEC H10,61,C'GROSS'                                                  
         PSPEC H10,77,C'NET'                                                    
         PSPEC H10,88,C'CASH'                                                   
         PSPEC H11,7,C'MONTH'                                                   
         PSPEC H11,60,C'ORDERED'                                                
         PSPEC H11,75,C'ORDERED'                                                
         PSPEC H11,86,C'DISCOUNT'                                               
         PSPEC H12,5,9C'-'                                                      
         PSPEC H12,60,7C'-'                                                     
         PSPEC H12,75,7C'-'                                                     
         PSPEC H12,86,8C'-'                                                     
         SPROG 0,2,3,4,5,6,7                                                    
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H5,76,RUN                                                        
         PSPEC H6,76,REPORT                                                     
         PSPEC H6,101,PAGE                                                      
         PSPEC H10,103,C'NET'                                                   
         PSPEC H11,101,C'PAYABLE'                                               
         PSPEC H12,101,7C'-'                                                    
         SPROG 8,10,11,12,13,14,15                                              
         PSPEC H1,88,AGYNAME                                                    
         PSPEC H2,88,AGYADD                                                     
         PSPEC H5,88,RUN                                                        
         PSPEC H6,88,REPORT                                                     
         PSPEC H6,113,PAGE                                                      
         PSPEC H10,102,C'GST'                                                   
         PSPEC H11,100,C'HST/PST'                                               
         PSPEC H12,100,7C'-'                                                    
         PSPEC H10,118,C'NET'                                                   
         PSPEC H11,116,C'PAYABLE'                                               
         PSPEC H12,116,7C'-'                                                    
         SPROG 2,3,4,5,6,7,10,11,12,13,14,15                                    
         PSPEC H10,5,C'INSERT'                                                  
         PSPEC H10,14,C'PYABLE'                                                 
         PSPEC H11,6,C'DATE      DATE EST'                                      
         PSPEC H12,5,C'------    ----- ---'                                     
         SPROG 2,3,10,11                                                        
         PSPEC H11,26,C'  SPACE DESCRIPTION       '                             
         PSPEC H12,26,C'  -----------------       '                             
         SPROG 4,5,6,7,12,13,14,15                                              
         PSPEC H10,27,C'UNIT'                                                   
         PSPEC H11,27,C'RATE'                                                   
         PSPEC H10,36,C'SPACE/'                                                 
         PSPEC H11,35,C'PREMIUM'                                                
         PSPEC H12,27,4C'-'                                                     
         PSPEC H12,35,7C'-'                                                     
         SPROG 4,5,12,13                                                        
         PSPEC H11,48,C'LINES'                                                  
         PSPEC H12,48,5C'-'                                                     
         SPROG 6,7,14,15                                                        
         PSPEC H11,47,C'INCHES'                                                 
         PSPEC H12,47,6C'-'                                                     
         SPROG 3,5,7,11,13,15                                                   
         PSPEC H7,1,ESTIMATE                                                    
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'0102041F0708090A0B0C0D0E0F13191400'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREP2701 07/18/16'                                      
         END                                                                    
