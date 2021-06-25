*          DATA SET ACREPPA01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACPA01A                                                                  
         TITLE 'ACPA01 - SPECS FOR PROJECT ALLOCATION'                          
ACPA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPACE 1                                                                
         SPROG 0,3                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,42,C'PROJECT ALLOCATION REPORT'                               
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H2,42,25C'-'                                                     
         ASPEC H4,2,C'COMPANY'                                                  
         RSPEC MAXLINES,55                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H5,85,C'FOR THE MONTH'                                           
         ASPEC H9,43,C'CLI'                                                     
         ASPEC H10,43,C'---'                                                    
         ASPEC H09,48,C'DIV'                                                    
         ASPEC H10,48,C'---'                                                    
         ASPEC H09,52,C'PROJ'                                                   
         ASPEC H10,52,C'NO. '                                                   
         ASPEC H09,59,C'PROJECT NAME'                                           
         ASPEC H10,59,C'------------'                                           
         ASPEC H09,88,C'TASK'                                                   
         ASPEC H10,88,C'----'                                                   
         ASPEC H09,94,C'HOURS'                                                  
         ASPEC H10,94,C'-----'                                                  
         ASPEC H09,106,C'COST'                                                  
         ASPEC H10,106,C'----'                                                  
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H9,50,C'DEBITS'                                                  
         ASPEC H9,70,C'CREDITS'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPPA01 08/17/00'                                      
         END                                                                    
