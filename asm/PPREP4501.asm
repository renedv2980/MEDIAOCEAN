*          DATA SET PPREP4501  AT LEVEL 009 AS OF 07/18/16                      
*PHASE PP4501A,+0                                                               
         TITLE 'PUB LISTS REPORT  -- SPECIFICATIONS'                            
PP4501   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,LISTS                                                       
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
*                                                                               
         SPROG 0                                                                
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,37,C'PUB LISTS REPORT'                                        
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,37,C'----------------'                                        
         PSPEC H2,76,AGYADD                                                     
         PSPEC H3,76,REPORT                                                     
         PSPEC H3,98,PAGE                                                       
         PSPEC H4,76,RUN                                                        
         PSPEC H4,1,CLIENT                                                      
         PSPEC H7,21,C'LIST NO.'                                                
         PSPEC H7,39,C'LIST NAME'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPREP4501 07/18/16'                                      
         END                                                                    
