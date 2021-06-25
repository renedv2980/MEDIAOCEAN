*          DATA SET ACREPZZ01  AT LEVEL 024 AS OF 03/02/00                      
*PHASE ACZZ01A,*                                                                
         TITLE 'ACZZ01 - CHECK TEMP X-REF RECORDS'                              
ACZZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANS                                                       
*                                                                               
         SPROG 0,1                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H3,2,C'COMPANY'                                                  
*                                                                               
         SPROG 1                                                                
         ACDEF H1,45,C'CHECK TEMPO X-REF RECORDS'                               
         ACDEF H2,45,C'-------------------------'                               
         ACDEF H8,06,C'PERSON'                                                  
         ACDEF H9,06,C'------'                                                  
         ACDEF H8,19,C'1R ACCT'                                                 
         ACDEF H9,19,C'-------'                                                 
         ACDEF H8,38,C'END DT'                                                  
         ACDEF H9,38,C'------'                                                  
         ACDEF H8,48,C'PER #'                                                   
         ACDEF H9,48,C'-----'                                                   
         ACDEF H8,56,C'STR DT'                                                  
         ACDEF H9,56,C'------'                                                  
         ACDEF H7,67,C'NEW'                                                     
         ACDEF H8,66,C'PER #'                                                   
         ACDEF H9,66,C'-----'                                                   
         ACDEF H7,75,C'NEW'                                                     
         ACDEF H8,74,C'STR DT'                                                  
         ACDEF H9,74,C'------'                                                  
         ACDEF H8,93,C'DESCRIPTION'                                             
         ACDEF H9,93,C'-----------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREPZZ01 03/02/00'                                      
         END                                                                    
