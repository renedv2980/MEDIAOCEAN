*          DATA SET ACREPZL01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACZL01A                                                                  
         TITLE 'MISSING PERSON RECORD REPORT'                                   
ACZL01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,1,RUN                                                         
         ACDEF H1,60,PAGE                                                       
         ACDEF H2,1,C'COMPANY'                                                  
         ACDEF H3,60,C'REPORT ACCN'                                             
         ACDEF H4,1,C'ACCOUNT'                                                  
         ACDEF H4,60,REQUESTOR                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPZL01 08/16/00'                                      
         END                                                                    
