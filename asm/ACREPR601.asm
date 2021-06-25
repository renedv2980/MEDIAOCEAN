*          DATA SET ACREPR601  AT LEVEL 002 AS OF 07/29/96                      
*PHASE ACR601A,*                                                                
         TITLE 'SPECS FOR EMPLOYEE UTILIZATION REPORT'                          
ACR601   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3,4,5                                                      
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANSACTIONS                                                
         ACDEF READ,TIME                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
         SPACE 1                                                                
         SPROG 0                                                                
         ACDEF H1,53,C'EMPLOYEE UTILIZATION REPORT'                             
         ACDEF H2,53,C'---------------------------'                             
         ACDEF H5,2,C'EMPLOYEE'                                                 
         ACDEF H6,2,C'TARGET %'                                                 
         ACDEF H8,34,C'WEEK 1'                                                  
         ACDEF H9,36,C'DAY'                                                     
         ACDEF H8,46,C'WEEK 2'                                                  
         ACDEF H9,46,C'5 DAYS'                                                  
         ACDEF H8,58,C'WEEK 3'                                                  
         ACDEF H9,58,C'5 DAYS'                                                  
         ACDEF H8,70,C'WEEK 4'                                                  
         ACDEF H9,70,C'5 DAYS'                                                  
         ACDEF H8,82,C'WEEK 5'                                                  
         ACDEF H9,84,C'DAY'                                                     
         ACDEF H8,94,C'MONTH'                                                   
         SPACE 1                                                                
         SPROG 1                                                                
         ACDEF H1,52,C'EMPLOYEE UTILIZATION SUMMARY'                            
         ACDEF H2,52,C'----------------------------'                            
         ACDEF H5,2,C'EMPLOYEE'                                                 
         SPACE 1                                                                
         SPROG 2                                                                
         ACDEF H1,50,C'DEPARTMENT % TO STANDARD SUMMARY'                        
         ACDEF H2,50,C'--------------------------------'                        
         ACDEF H8,30,C'TGT %'                                                   
         ACDEF H9,30,C'-----'                                                   
         ACDEF H8,36,C'TYPE'                                                    
         ACDEF H9,36,C'----'                                                    
         SPACE 1                                                                
         SPROG 3                                                                
         ACDEF H1,46,C'DEPARTMENT % TO STANDARD WEEKLY SUMMARY'                 
         ACDEF H2,46,C'---------------------------------------'                 
         ACDEF H8,30,C'TGT %'                                                   
         ACDEF H9,30,C'-----'                                                   
         ACDEF H8,36,C'TYPE'                                                    
         ACDEF H9,36,C'----'                                                    
         ACDEF H8,41,C'WEEK 1'                                                  
         ACDEF H9,43,C'DAY'                                                     
         ACDEF H8,48,C'WEEK 2'                                                  
         ACDEF H9,50,C'DAYS'                                                    
         ACDEF H8,55,C'WEEK 3'                                                  
         ACDEF H9,57,C'DAYS'                                                    
         ACDEF H8,62,C'WEEK 4'                                                  
         ACDEF H9,64,C'DAYS'                                                    
         ACDEF H8,69,C'WEEK 5'                                                  
         ACDEF H9,71,C'DAY'                                                     
         ACDEF H8,76,C'MONTH'                                                   
*                                                                               
         SPROG 4                                                                
         ACDEF H1,56,C'EMPLOYEE METRICS SUMMARY'                                
         ACDEF H2,56,C'------------------------'                                
         ACDEF H8,30,C'METRIC'                                                  
         ACDEF H9,30,C'------'                                                  
         SPACE 1                                                                
         SPROG 5                                                                
         ACDEF H1,50,C'EMPLOYEE METRICS WEEKLY SUMMARY'                         
         ACDEF H2,50,C'-------------------------------'                         
         ACDEF H8,30,C'TGT %'                                                   
         ACDEF H9,30,C'-----'                                                   
         ACDEF H8,36,C'METRIC'                                                  
         ACDEF H9,36,C'------'                                                  
         ACDEF H8,43,C'WEEK 1'                                                  
         ACDEF H9,45,C'DAY'                                                     
         ACDEF H8,50,C'WEEK 2'                                                  
         ACDEF H9,52,C'DAYS'                                                    
         ACDEF H8,57,C'WEEK 3'                                                  
         ACDEF H9,59,C'DAYS'                                                    
         ACDEF H8,64,C'WEEK 4'                                                  
         ACDEF H9,66,C'DAYS'                                                    
         ACDEF H8,71,C'WEEK 5'                                                  
         ACDEF H9,73,C'DAY'                                                     
         ACDEF H8,78,C'MONTH'                                                   
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPR601 07/29/96'                                      
         END                                                                    
