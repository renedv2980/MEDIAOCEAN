*          DATA SET ACREPR301  AT LEVEL 050 AS OF 02/02/95                      
*PHASE ACR301A,*                                                                
         TITLE 'SPECS FOR CLIENT FEE ANALYSIS REPORT'                           
ACR301   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3,4,5,6,7                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF H1,57,C'CLIENT FEE ANALYSIS'                                     
         ACDEF H2,57,19C'-'                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,102,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
         ACDEF H6,86,C'POSTINGS FOR'                                            
         ACDEF H6,106,C'THRU'                                                   
         ACDEF H6,56,PERIOD                                                     
         SPROG 0,1,2,3,4,6                                                      
         ACDEF H4,2,C'OFFICE'                                                   
         SPROG 0,1,2,3,4                                                        
         ACDEF H5,2,C'CLIENT'                                                   
         SPACE 2                                                                
         SPROG 1,3,5,6,7                                                        
         ACDEF H4,54,C'CURRENT MONTH OF:'                                       
         ACDEF H8,65,C'CURRENT'                                                 
         ACDEF H8,81,C'CURRENT'                                                 
         ACDEF H8,97,C'Y.T.D.'                                                  
         ACDEF H8,113,C'Y.T.D.'                                                 
         SPACE 1                                                                
         ACDEF H9,65,C'HOURS'                                                   
         ACDEF H9,77,C'BILLABLE AMOUNT'                                         
         ACDEF H9,97,C'HOURS'                                                   
         ACDEF H9,109,C'BILLABLE AMOUNT'                                        
         SPACE 2                                                                
         SPROG 2,4                                                              
         ACDEF H4,57,C'JOB INQUIRY MONTHLY  '                                   
         ACDEF H4,57,C'    JOB INQUIRY      '                                   
         ACDEF H8,61,C'TIMESHEET'                                               
         ACDEF H9,61,C'  DATE   '                                               
         ACDEF H8,73,C'HOURS'                                                   
         ACDEF H8,83,C'RATE' '                                                  
         ACDEF H8,91,C'BILLABLE AMOUNT'                                         
         SPACE 1                                                                
         SPROG 1,2                                                              
         ACDEF H6,2,C'PRODUCT'     DONT PRINT ON SUMMARY                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050ACREPR301 02/02/95'                                      
         END                                                                    
