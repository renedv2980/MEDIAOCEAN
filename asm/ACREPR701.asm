*          DATA SET ACREPR701  AT LEVEL 066 AS OF 12/18/90                      
*PHASE ACR701A,*                                                                
         TITLE 'SPECS FOR CHARGEABLE ANALYSIS REPORT'                           
ACR701   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3,4,5                                                      
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF H1,55,C'CHARGEABLE TIME ANALYSIS'                                
         ACDEF H2,55,24C'-'               '                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,102,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H2,2,COMPANY                                                     
         ACDEF H4,102,C'POSTINGS FOR'                                           
         ACDEF H4,122,C'THRU'                                                   
         SPROG 0                                                                
         ACDEF H4,54,C'CURRENT MONTH OF:'                                       
         SPACE 1                                                                
         ACDEF H8,78,C'  CURRENT   '                                            
         ACDEF H8,92,C'  CURRENT   '                                            
         ACDEF H8,106,C'   Y.T.D.   '                                           
         ACDEF H8,120,C'   Y.T.D.   '                                           
         SPACE 1                                                                
         ACDEF H9,78,C'    HOURS'                                               
         ACDEF H9,92,C'BILLABLE AMT'                                            
         ACDEF H9,106,C'   HOURS'                                               
         ACDEF H9,120,C'BILLABLE AMT'                                           
         SPACE 2                                                                
         SPROG 2                                                                
         ACDEF H4,58,C'    JOB INQUIRY      '                                   
         ACDEF H8,79,C'TIMESHEET'                                               
         ACDEF H9,79,C'  DATE   '                                               
         ACDEF H8,93,C'  HOURS'                                                 
         ACDEF H8,106,C'   RATE'                                                
         ACDEF H8,120,C'BILLABLE AMT'                                           
         SPACE 1                                                                
         SPROG 3,6                                                              
         ACDEF H8,78,C'  B TIME '                                               
         ACDEF H8,92,C'  R TIME '                                               
         ACDEF H8,106,C'  N TIME'                                               
         ACDEF H8,120,C' ALL TIME'                                              
         SPROG 3                                                                
         ACDEF H9,78,C'   HOURS'                                                
         ACDEF H9,92,C'   HOURS'                                                
         ACDEF H9,106,C'   HOURS'                                               
         ACDEF H9,120,C'   HOURS'                                               
         SPROG 6                                                                
         ACDEF H9,78,C'  AMOUNT'                                                
         ACDEF H9,92,C'  AMOUNT'                                                
         ACDEF H9,106,C'  AMOUNT'                                               
         ACDEF H9,120,C'  AMOUNT'                                               
         SPACE 2                                                                
         SPROG 4                                                                
         ACDEF H8,50,C'   B TIME '                                              
         ACDEF H8,64,C'   B TIME '                                              
         ACDEF H8,78,C'   R TIME'                                               
         ACDEF H8,92,C'   R TIME'                                               
         ACDEF H8,106,C'  B+R TIME'                                             
         ACDEF H8,120,C'  B+R TIME'                                             
         ACDEF H9,50,C'   HOURS   '                                             
         ACDEF H9,64,C'   AMOUNT'                                               
         ACDEF H9,78,C'   HOURS'                                                
         ACDEF H9,92,C'   AMOUNT'                                               
         ACDEF H9,106,C'  HOURS'                                                
         ACDEF H9,120,C'   AMOUNT'                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066ACREPR701 12/18/90'                                      
         END                                                                    
