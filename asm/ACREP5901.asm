*          DATA SET ACREP5901  AT LEVEL 019 AS OF 10/22/93                      
*PHASE AC5901A,+0                                                               
         TITLE 'SPES FOR JOB EXCEPTION REPORT'                                  
AC5901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
         ACDEF FJOBCOL,CE,CEG,HR,HRG                                            
         ACDEF JOBBER,PROCACC                                                   
         ACDEF GETOPT,W                                                         
*&&US*&& RSPEC MAXLINES,54                                                      
         SPACE 1                                                                
         SPROG 0,1,2,3,4,5,6                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,46,C'PRODUCTION JOB EXCEPTION REPORT'                         
         ASPEC H2,46,31C'-'                                                     
         ASPEC H1,105,REPORT                                                    
         ASPEC H1,122,PAGE                                                      
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,105,REQUESTOR                                                 
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 0                                                                
*&&US                                                                           
         ASPEC H8,2,C'JOB NO./NAME/OPEN-CLOSE DATE/BT'                          
         ASPEC H8,43,C'ESTIMATE'                                                
         ASPEC H8,58,C'ACTUAL'                                                  
         ASPEC H8,71,C'OVER(+)/UNDER'                                           
         ASPEC H9,71,C'AMOUNT   PCT.'                                           
         ASPEC H8,90,C'BILLING'                                                 
         ASPEC H8,102,C'UNBILLED'                                               
         ASPEC H8,113,C'EXCEPTION'                                              
         ASPEC H9,113,C'  CODES  '                                              
*&&                                                                             
*&&UK                                                                           
         ASPEC H8,2,C'JOB NUMBER/NAME/OPEN DATE/BT'                             
         ASPEC H9,2,28C'-'                                                      
         ASPEC H8,40,C'ESTIMATE'                                                
         ASPEC H9,40,8C'-'                                                      
         ASPEC H8,52,C'ACTUAL'                                                  
         ASPEC H9,51,8C'-'                                                      
         ASPEC H8,63,C'OVER(+)/UNDER'                                           
         ASPEC H9,63,C' AMOUNT  PCT.'                                           
         ASPEC H8,81,C'BILLING'                                                 
         ASPEC H9,81,7C'-'                                                      
         ASPEC H8,91,C'UNBILLED'                                                
         ASPEC H9,91,8C'-'                                                      
         ASPEC H8,102,C'EXCP'                                                   
         ASPEC H9,102,C'CODES'                                                  
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACREP5901 10/22/93'                                      
         END                                                                    
