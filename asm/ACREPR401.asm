*          DATA SET ACREPR401  AT LEVEL 053 AS OF 09/25/96                      
*PHASE ACR401A,*                                                                
         TITLE 'SPECS FOR WORK DISRTIBUTION REPORT'                             
ACR401   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
*                                                                               
         ACDEF H1,54,C'WORK DISTRIBUTION REPORT'                                
         ACDEF H2,54,24C'-'                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
         ACDEF H4,101,MOSFILT                                                   
         ACDEF H6,85,PERIOD                                                     
         ACDEF H4,2,C'OFFICE GROUP'                                             
         ACDEF H5,2,C'OFFICE'                                                   
         SPACE 2                                                                
         ACDEF H8,53,C'EMPLOYEES OF THIS OFFICE'                                
         ACDEF H10,54,C'HOURS'                                                  
         ACDEF H10,67,C'REVENUE'                                                
         ACDEF H8,79,C'EMPLOYEES OF OTHER OFFICES'                              
         ACDEF H10,81,C'HOURS'                                                  
         ACDEF H10,94,C'REVENUE'                                                
         ACDEF H8,112,C'ALL EMPLOYEES'                                          
         ACDEF H10,108,C'HOURS'                                                 
         ACDEF H10,121,C'REVENUE'                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053ACREPR401 09/25/96'                                      
         END                                                                    
