*          DATA SET ACREP1401  AT LEVEL 007 AS OF 05/20/16                      
*PHASE AC1401A,+0                                                               
         TITLE 'SPECS FOR ESTIMATE PRINTING'                                    
AC1401   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         RSPEC REQUEST,NOREP                                                    
         SPROG 0,1,2,3,4,5                                                      
         ASPEC U1,2,C' '           THIS IS A BINARY ZERO                        
         ASPEC U2,2,C' '           THIS IS A BINARY ZERO                        
*        ASPEC U1,34,ORIGIN                                                     
*        ASPEC U2,34,ORIGADD                                                    
         ASPEC H1,2,C'CLIENT'                                                   
         ASPEC H2,2,C'PRODUCT'                                                  
         ASPEC H3,2,C'MEDIA'                                                    
         ASPEC H4,2,C'JOB'                                                      
         ASPEC H1,53,C'PRODUCTION ESTIMATE'                                     
         ASPEC H1,75,REPORT                                                     
         ASPEC H2,53,RUN                                                        
         ASPEC H2,079,PAGE                                                      
         ASPEC H11,2,C'DESCRIPTION'                                             
         SPACE 1                                                                
         SPROG 0,1,2,4                                                          
         ASPEC H11,43,C'ORIGINAL'                                               
         ASPEC H12,43,C'ESTIMATE'                                               
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H11,59,C'PRESENT'                                                
         ASPEC H12,59,C'ESTIMATE'                                               
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H11,76,C'DIFFERENCE'                                             
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H11,76,C'ACTUAL'                                                 
         ASPEC H12,76,C'CHARGES'                                                
         ASPEC H11,88,C'PRESENT EST   PER- '                                    
         ASPEC H12,88,C'LESS CHARGES  CENT'                                     
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H11,43,C'PREVIOUS        CURRENT'                                
         ASPEC H12,43,C'ESTIMATE        ESTIMATE'                               
         ASPEC H11,76,C'NET CHANGE'                                             
         SPACE 1                                                                
         SPROG 5                                                                
         ASPEC H11,43,C'ORIGINAL'                                               
         ASPEC H12,43,C'ESTIMATE'                                               
         ASPEC H11,59,C'PREVIOUS'                                               
         ASPEC H12,59,C'ESTIMATE'                                               
         ASPEC H11,75,C'CURRENT'                                                
         ASPEC H12,75,C'ESTIMATE'                                               
         ASPEC H11,91,C'  NET  '                                                
         ASPEC H12,91,C'CHANGE  '                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREP1401 05/20/16'                                      
         END                                                                    
