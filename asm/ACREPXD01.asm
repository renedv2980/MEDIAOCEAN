*          DATA SET ACREPXD01  AT LEVEL 006 AS OF 08/16/00                      
*PHASE ACXD01A,+0                                                               
         TITLE 'SPECS FOR REMOVING/FIXING ELEMENTS'                             
ACXD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC UPDATE,ACCFILE                                                   
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         ASPEC H1,38,C'REMOVE/FIX ELEMENTS'                                     
         ASPEC H2,38,C'-------------------'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPXD01 08/16/00'                                      
         END                                                                    
