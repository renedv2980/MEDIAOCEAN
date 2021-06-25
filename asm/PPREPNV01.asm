*          DATA SET PPREPNV01  AT LEVEL 041 AS OF 07/18/16                      
*PHASE PPNV01A                                                                  
         TITLE 'PPNV01  NV REPORT HEADLINES'                                    
PPNV01   CSECT                                                                  
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,2,3,4,5,6,7,20                                                 
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,28,CL9'NV REPORT'                                             
         PSPEC H1,51,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,28,9C'-'                                                      
         PSPEC H2,51,AGYADD                                                     
         PSPEC H3,23,PERIOD                                                     
         PSPEC H5,51,RUN                                                        
         PSPEC H6,51,REPORT                                                     
         PSPEC H6,76,PAGE                                                       
         PSPEC H11,7,C'MONTH'                                                   
         PSPEC H12,5,9C'-'                                                      
         SPROG 0,2,3,4,5,6,7                                                    
         PSPEC H5,1,CLIENT                                                      
         SPROG 2,3,4,5,6,7                                                      
         PSPEC H10,5,C'INSERT'                                                  
         PSPEC H10,14,C'PYABLE'                                                 
         PSPEC H11,6,C'DATE      DATE EST'                                      
         PSPEC H12,5,C'------    ----- ---'                                     
         SPROG 2,3                                                              
         PSPEC H11,25,C' SPACE DESCRIPTION      '                               
         PSPEC H12,25,C' -----------------      '                               
         SPROG 4,5,6,7                                                          
         PSPEC H10,26,C'UNIT'                                                   
         PSPEC H11,26,C'RATE'                                                   
         PSPEC H10,35,C'SPACE/'                                                 
         PSPEC H11,34,C'PREMIUM'                                                
         PSPEC H12,26,4C'-'                                                     
         PSPEC H12,34,7C'-'                                                     
         SPROG 4,5                                                              
         PSPEC H11,48,C'LINES'                                                  
         PSPEC H12,48,5C'-'                                                     
         SPROG 6,7                                                              
         PSPEC H11,47,C'INCHES'                                                 
         PSPEC H12,47,6C'-'                                                     
         SPROG 3,5,7                                                            
         PSPEC H7,1,ESTIMATE                                                    
         SPROG 10                                                               
         PSPEC H1,25,AGYNAME                                                    
         PSPEC H2,25,AGYADD                                                     
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'0102041F0708090A0B0C0D0E0F13191400'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041PPREPNV01 07/18/16'                                      
         END                                                                    
