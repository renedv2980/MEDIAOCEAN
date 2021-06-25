*          DATA SET ACREP6601  AT LEVEL 024 AS OF 08/27/93                      
*PHASE AC6601A,+0                                                               
         TITLE 'CLIENT BUDGET REPORT '                                          
AC6601   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF FJOBCOL,CE,CEG,HR,HRG                                            
         ACDEF JOBBER,PROCACC                                                   
         SPROG 0                                                                
         ASPEC H1,2,RUN,WIDE=198                                                
         ASPEC H1,125,REPORT,WIDE=198                                           
         ASPEC H1,138,PAGE,WIDE=198                                             
         ASPEC H3,125,REQUESTOR,WIDE=198                                        
         SPACE 1                                                                
         ASPEC H3,2,COMPANY,WIDE=198                                            
         ASPEC H1,71,C'CLIENT BUDGET REPORT',WIDE=198                           
         ASPEC H2,71,20C'_',WIDE=198                                            
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREP6601 08/27/93'                                      
         END                                                                    
