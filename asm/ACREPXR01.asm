*          DATA SET ACREPXR01  AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACXR01A                                                                  
         TITLE 'ADD ORIGINATING SERVER TO PRESTO ORDERS'                        
ACXR01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ORDERS                                                      
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF RESET                                                            
         ACDEF GETOPT,N                                                         
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,35,C'PRESTO ORDERS--ADD SERVER'                               
         ACDEF H2,35,C'-------------------------'                               
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPXR01 08/16/00'                                      
         END                                                                    
