*          DATA SET REREP7201  AT LEVEL 010 AS OF 11/16/06                      
*PHASE RE7201A,+0                                                               
         TITLE 'SPECS FOR ADVERTISER LISTING'                                   
RE7201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ADVERTISERS                                                 
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,REP1                                                        
         ASPEC H1,48,C'ADVERTISER LISTING'                                      
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H2,2,RUN                                                         
         ASPEC H2,48,18C'-'                                                     
         ASPEC H4,02,C'CODE  ADVERTISER NAME'                                   
         ASPEC H4,30,C'CODE  ADVERTISER NAME'                                   
         ASPEC H4,58,C'CODE  ADVERTISER NAME'                                   
         ASPEC H4,86,C'CODE  ADVERTISER NAME'                                   
         ASPEC H5,02,C'----  ---------------'                                   
         ASPEC H5,30,C'----  ---------------'                                   
         ASPEC H5,58,C'----  ---------------'                                   
         ASPEC H5,86,C'----  ---------------'                                   
         ASPEC H2,88,REQUESTOR                                                  
         SPROG 3                                                                
         ASPEC H1,2,REP1                                                        
         ASPEC H1,44,C'ADVERTISER EQUIVALENCY'                                  
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H2,2,RUN                                                         
         ASPEC H2,44,22C'-'                                                     
         ASPEC H4,02,C'CODE  ADVERTISER NAME        EQUIV CODE'                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REREP7201 11/16/06'                                      
         END                                                                    
