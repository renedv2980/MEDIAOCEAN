*          DATA SET REREP3501  AT LEVEL 005 AS OF 08/31/00                      
*          DATA SET REREP3501  AT LEVEL 004 AS OF 07/17/86                      
*PHASE RE3501A                                                                  
         TITLE 'SPECS FOR GROUP BUY TAPE'                                       
RE3501   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC GET,PRODUCT                                                      
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,46,PERIOD                                                     
         ASPEC H4,88,REQUESTOR                                                  
         ASPEC H8,23,C'RECORDS'                                                 
         ASPEC H9,23,7C'-'                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,50,C'GROUP BUY TAPE'                                          
         ASPEC H2,50,14C'-'                                                     
         ASPEC H8,43,C'GROSS'                                                   
         ASPEC H9,40,10C'-'                                                     
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H1,45,C'BLAIR COMBINED FILE TAPE'                                
         ASPEC H2,45,24C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP3501 08/31/00'                                      
         END                                                                    
