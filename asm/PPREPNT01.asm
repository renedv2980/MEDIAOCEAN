*          DATA SET PPREPNT01  AT LEVEL 013 AS OF 07/18/16                      
*PHASE PPNT01A                                                                  
         PRINT NOGEN                                                            
PPNT01   CSECT                                                                  
         RSPEC REQUEST,NOREP                                                    
         SPROG 0                                                                
         PSPEC H1,44,C'NVTEXT RECORD'                                           
         SPROG 10                                                               
         PSPEC H1,44,C'I/OCOM RECORD'                                           
         SPROG 20                                                               
         PSPEC H1,44,C'CONCOM RECORD'                                           
         SPROG 0,10,20                                                          
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,75,AGYNAME                                                    
         PSPEC H2,44,13C'-'                                                     
         PSPEC H2,75,AGYADD                                                     
         PSPEC H4,1,REQUESTOR                                                   
         PSPEC H4,75,RUN                                                        
         PSPEC H5,75,REPORT                                                     
         PSPEC H5,95,PAGE                                                       
         PSPEC H7,1,C'CLIENT'                                                   
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREPNT01 07/18/16'                                      
         END                                                                    
