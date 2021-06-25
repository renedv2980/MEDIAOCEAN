*          DATA SET ACREPJS01  AT LEVEL 013 AS OF 07/20/20                      
*PHASE ACJS01C                                                                  
         TITLE 'JOB STATUS REPORT'                                              
ACJS01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF WIDTH,198                                                        
         ACDEF GETOPT,NO                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'JOB STATUS REPORT'                                      
         ACDEF H1,130,PAGE                                                      
         ACDEF H2,005,C'CLI  :'                                                 
         ACDEF H2,061,C'OPENED :'                                               
         ACDEF H2,081,C'BILL TYPE :'                                            
         ACDEF H3,005,C'PROD :'                                                 
         ACDEF H3,061,C'REVISED:'                                               
         ACDEF H3,081,C'COSTING AC:'                                            
         ACDEF H4,005,C'JOB  :'                                                 
         ACDEF H4,061,C'CLOSED :'                                               
         ACDEF H4,081,C'ACT BAL   :'                                            
         ACDEF H5,005,C'ELIGIBLE FOR DELETION:'                                 
         ACDEF H5,061,C'PEELED :'                                               
         ACDEF H5,081,C'PEEL BAL  :'                                            
         ACDEF H6,005,C'EXPENSE :'                                              
         ACDEF H6,023,C'TIME    :'                                              
         ACDEF H6,040,C'ORDER   :'                                              
         ACDEF H6,061,C'AJ RATE :'                                              
         ACDEF H6,081,C'ESTIMATE:'                                              
         ACDEF H6,098,C'EST VER :'                                              
         ACDEF H7,005,C'AUDIT   :'                                              
         ACDEF H7,023,C'OPTIONS :'                                              
         ACDEF H7,040,C'JOB CYC :'                                              
         ACDEF H7,061,C'PRICING :'                                              
         ACDEF H7,081,C'USR FLD :'                                              
         ACDEF H7,098,C'TEXT    :'                                              
         ACDEF H8,005,C'SESSION :'                                              
         ACDEF H8,023,C'INVOICE :'                                              
         ACDEF H8,040,C'APPROVER:'                                              
         ACDEF H8,061,C'FUND    :'                                              
         ACDEF H8,081,C'STUDIO  :'                                              
         ACDEF H8,098,C'DRAFT   :'                                              
         ACDEF H8,116,C'BALANCE :'                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPJS01 07/20/20'                                      
         END                                                                    
