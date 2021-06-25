*          DATA SET PPREP4201  AT LEVEL 008 AS OF 07/18/16                      
*PHASE PP4201A                                                                  
PP4201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,DISTRICTS                                                   
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0                                                                
         FSPEC READ,CLIENTS                                                     
         PSPEC H1,39,C'DIVISION REGION DISTRICT LISTING'                        
         PSPEC H2,39,32C'-'                                                     
         PSPEC H2,2,MEDIANAME                                                   
         PSPEC H2,76,AGYNAME                                                    
         PSPEC H3,76,AGYADD                                                     
         PSPEC H4,2,CLIENT                                                      
         PSPEC H5,2,REQUESTOR                                                   
         PSPEC H5,76,RUN                                                        
         PSPEC H6,76,REPORT                                                     
         PSPEC H6,101,PAGE                                                      
         PSPEC H7,131,C'X'                                                      
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPREP4201 07/18/16'                                      
         END                                                                    
