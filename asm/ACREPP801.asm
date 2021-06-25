*          DATA SET ACREPP801  AT LEVEL 004 AS OF 08/17/00                      
*PHASE ACP801A                                                                  
         TITLE 'SPECS FOR WORK-CODE/BILLING SUMMARY'                            
ACP801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
*                                                                               
*&&US*&& RSPEC MAXLINES,54                                                      
         SPROG 0,1,2,3,4,5                                                      
         ASPEC H1,2,RUN                                                         
         ASPEC H1,43,C'WORK-CODE/BILLING SUMMARY'                               
         ASPEC H2,43,25C'-'                                                     
         ASPEC H1,80,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,80,REQUESTOR                                                  
         ASPEC F1,2,REQDETS                                                     
         ASPEC H5,80,C'PERIOD'                                                  
         ASPEC H8,78,C' NET     COMMISSION    GROSS '                           
         ASPEC H9,78,C'AMOUNT                 AMOUNT'                           
         SPACE 1                                                                
         SPROG 0,2,4                                                            
         ASPEC H5,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SPROG 1,3,5                                                            
         ASPEC H5,2,77C' '                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H8,2,C'JOB NUMBER/NAME'                                          
         ASPEC H8,23,C'------BILL-----'                                         
         ASPEC H9,23,C'NUMBER     DATE'                                         
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H8,2,36C' '                                                      
         ASPEC H9,2,36C' '                                                      
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H8,2,C'JOB NUMBER AND NAME'                                      
         ASPEC H8,35,C'------BILL-----'                                         
         ASPEC H9,35,C'NUMBER     DATE'                                         
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H8,2,48C' '                                                      
         ASPEC H9,2,48C' '                                                      
         SPACE 1                                                                
         SPROG 4                                                                
         ASPEC H8,2,C'JOB NUMBER AND NAME'                                      
         ASPEC H8,47,C'------BILL-----'                                         
         ASPEC H9,47,C'NUMBER     DATE'                                         
         SPACE 1                                                                
         SPROG 5                                                                
         ASPEC H8,2,60C' '                                                      
         ASPEC H9,2,60C' '                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPP801 08/17/00'                                      
         END                                                                    
