*          DATA SET PPREP4701  AT LEVEL 008 AS OF 07/18/16                      
*PHASE PP4701A                                                                  
         PRINT NOGEN                                                            
PP4701   CSECT                                                                  
         RSPEC REQUEST,NOREP                                                    
         SPROG 0                                                                
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,44,C'COMMENT LISTING'                                         
         PSPEC H1,75,AGYNAME                                                    
         PSPEC H2,44,15C'-'                                                     
         PSPEC H2,75,AGYADD                                                     
         PSPEC H4,1,REQUESTOR                                                   
         PSPEC H4,75,RUN                                                        
         PSPEC H5,75,REPORT                                                     
         PSPEC H5,95,PAGE                                                       
         PSPEC H7,1,C'COMMENT'                                                  
         PSPEC H8,2,C'NUMBER'                                                   
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPREP4701 07/18/16'                                      
         END                                                                    
