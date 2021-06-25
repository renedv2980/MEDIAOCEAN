*          DATA SET PPREP4101  AT LEVEL 023 AS OF 07/18/16                      
*PHASE PP4101A,+0                                                               
         TITLE 'PP4101  CLT/PRD/EST     LISTING HEADLINES'                      
PP4101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ESTIMATES                                                   
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20,30                                                       
         PSPEC H1,40,C'CLIENT PRODUCT ESTIMATE LISTING'                         
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,99,AGYNAME                                                    
         PSPEC H2,40,31C'-'                                                     
         PSPEC H2,99,AGYADD                                                     
         PSPEC H4,99,RUN                                                        
         PSPEC H5,1,REQUESTOR                                                   
         PSPEC H5,99,REPORT                                                     
         PSPEC H5,124,PAGE                                                      
         SPROG 0                                                                
         PSPEC H6,49,C'PROFILE AND BILLING OPTIONS'                             
         PSPEC H7,49,C'---------------------------'                             
         SPROG 10                                                               
         PSPEC H6,49,C'BILLING OPTIONS'                                         
         PSPEC H7,49,C'---------------'                                         
         SPROG 20                                                               
         PSPEC H6,49,C'PROFILE OPTIONS'                                         
         PSPEC H7,49,C'---------------'                                         
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023PPREP4101 07/18/16'                                      
         END                                                                    
