*          DATA SET ACREP7501  AT LEVEL 009 AS OF 09/26/07                      
*PHASE AC7501A                                                                  
         TITLE 'SPECS FOR NAME AND ADDRESS LISTING'                             
AC7501   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MODE,PROCLEV                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,40,C'ACCOUNT NAME AND ADDRESS LISTING'                        
         ACDEF H2,40,32C'-'                                                     
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H4,2,C'COMPANY'                                                  
         ACDEF H5,2,C'UNIT'                                                     
         ACDEF H6,2,C'LEDGER'                                                   
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H9,2,C'-------ACCOUNT--------'                                   
         ACDEF H10,2,C'CODE         FILTR NAME'                                 
         ACDEF H9,59,C'ADDRESS LINE 1             ADDRESS LINE 2'               
         ACDEF H10,59,C'ADDRESS LINE 3             ADDRESS LINE 4'              
         ACDEF F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREP7501 09/26/07'                                      
         END                                                                    
