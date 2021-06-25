*          DATA SET ACREPZ101  AT LEVEL 050 AS OF 10/04/00                      
*PHASE ACZ101A,*                                                                
         TITLE 'ACZ101 - SPECS FOR DELETE AGENCY ACCOUNTS'                      
ACZ101   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         SPROG 0,1                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H3,2,ORIGIN                                                      
         ACDEF H5,2,C'UNIT   :'                                                 
         ACDEF H6,2,C'LEDGER :'                                                 
*                                                                               
         SPROG 1                                                                
         ACDEF H1,45,C'DELETE SJ WCODE ** TRANSACTIONS'                         
         ACDEF H2,45,C'-------------------------------'                         
*        ACDEF H8,02,C'ACCOUNT'                                                 
*        ACDEF H9,02,C'-------'                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050ACREPZ101 10/04/00'                                      
         END                                                                    
