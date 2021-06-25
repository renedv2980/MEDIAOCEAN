*          DATA SET ACREPC601  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACC601A,+0                                                               
         TITLE 'ACC601 - SPECS FOR DEPT. COST STATEMENT'                        
ACC601   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'DEPARTMENTAL COST STATEMENT'                             
         ASPEC H2,40,27C'-'                                                     
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H3,83,REQUESTOR                                                  
         ASPEC F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPC601 08/16/00'                                      
         END                                                                    
