*          DATA SET REREP7901  AT LEVEL 013 AS OF 07/29/96                      
*          DATA SET REREP7901  AT LEVEL 009 AS OF 11/08/95                      
*PHASE RE7901A,+0                                                               
         TITLE 'SPECS FOR CLASS/CATEGORY LISTING'                               
RE7901   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,42,C'CLASS/CATEGORY LISTING'                                  
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H2,2,REP1                                                        
         ASPEC H2,42,22C'-'                                                     
         ASPEC H2,88,REQUESTOR                                                  
         ASPEC H4,02,C'CODE'                                                    
         ASPEC H4,10,C'CLASS DESCRIPTION'                                       
         ASPEC H5,02,C'----'                                                    
         ASPEC H5,10,C'-----------------'                                       
         ASPEC H4,35,C'CODE'                                                    
         ASPEC H4,42,C'CATEGORY DESCRIPTION'                                    
         ASPEC H5,35,C'----'                                                    
         ASPEC H5,42,C'--------------------'                                    
         SPROG 1                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,42,C'CLASS/CATEGORY/ADVERTISER LISTING'                       
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H2,2,REP1                                                        
         ASPEC H2,42,33C'-'                                                     
         ASPEC H2,88,REQUESTOR                                                  
         ASPEC H4,02,C'CODE'                                                    
         ASPEC H4,10,C'CLASS DESCRIPTION'                                       
         ASPEC H5,02,C'----'                                                    
         ASPEC H5,10,C'-----------------'                                       
         ASPEC H4,35,C'CODE'                                                    
         ASPEC H4,42,C'CATEGORY/ADVERTISER'                                     
         ASPEC H5,35,C'----'                                                    
         ASPEC H5,42,C'-------------------'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013REREP7901 07/29/96'                                      
         END                                                                    
