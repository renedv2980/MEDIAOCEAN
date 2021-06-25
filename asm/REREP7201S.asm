*          DATA SET REREP7201S AT LEVEL 008 AS OF 09/09/94                      
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
         ASPEC H4,02,C'CODE ADVERTISER NAME'                                    
         ASPEC H4,29,C'CODE ADVERTISER NAME'                                    
         ASPEC H4,56,C'CODE ADVERTISER NAME'                                    
         ASPEC H4,83,C'CODE ADVERTISER NAME'                                    
         ASPEC H5,02,C'---- ---------------'                                    
         ASPEC H5,29,C'---- ---------------'                                    
         ASPEC H5,56,C'---- ---------------'                                    
         ASPEC H5,83,C'---- ---------------'                                    
         ASPEC H2,88,REQUESTOR                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREP7201S09/09/94'                                      
         END                                                                    
