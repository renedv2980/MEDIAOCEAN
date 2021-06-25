*          DATA SET ACREP6101  AT LEVEL 012 AS OF 11/14/95                      
*PHASE AC6101A,+0                                                               
         TITLE 'SPECS FOR JOB AGEING'                                           
AC6101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
*                                                                               
*&&US*&& RSPEC MAXLINES,54                                                      
         ACDEF RESET                                                            
         SPACE 1                                                                
         SPROG 0,1,2,3,4,5                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H3,74,MOSFILT                                                    
         ACDEF H3,2,COMPANY                                                     
         ACDEF H4,74,REQUESTOR                                                  
*&&UK                                                                           
         ACDEF H9,2,C'PRODUCT AND JOB NUMBER AND NAME'                          
         ACDEF H10,2,C'-------------------------------'                         
         ACDEF H9,39,C'UN/BT/EST/ST'                                            
         ACDEF H10,39,C'------------'                                           
*&&                                                                             
*&&US                                                                           
         ACDEF H9,2,C'PRODUCT / JOB NUMBER AND NAME'                            
         ACDEF H9,32,C'OFF/BT/EST/ST'                                           
*&&                                                                             
         ACDEF F1,1,REQDETS                                                     
         SPROG 1,2                                                              
         ACDEF H9,2,19C' '                                                      
         ACDEF H10,2,19C' '             '                                       
         ACDEF H9,21,C'MEDIA CODE AND NAME     '                                
*&&UK*&& ACDEF H10,21,C'-------------------     '                               
*&&US*&& ACDEF H10,21,C'                        '                               
         SPROG 1                                                                
*&&UK                                                                           
         ACDEF H9,21,C'       UNIT FOR ANALYSIS  '                              
         ACDEF H10,21,C'       -----------------  '                             
*&&                                                                             
*&&US                                                                           
         ACDEF H9,21,C'      OFFICE       '                                     
         ACDEF H10,21,C'                   '                                    
*&&                                                                             
         SPROG 3                                                                
*&&UK                                                                           
         ACDEF H9,2,CL50'CLIENT CODE AND NAME'                                  
         ACDEF H10,2,CL50'--------------------'                                 
*&&                                                                             
*&&US                                                                           
         ACDEF H9,2,CL43'CLIENT CODE AND NAME'                                  
         ACDEF H10,2,CL43' '                                                    
*&&                                                                             
         SPROG 4                                                                
         ACDEF H9,25,25C' '                                                     
         ACDEF H10,25,25C' '                                                    
         ACDEF H9,2,C'INCOME ACCOUNT AND NAME'                                  
*&&US*&& ACDEF H10,3,C'                       '                                 
         ACDEF H9,50,C'AMOUNT'                                                  
*&&UK*&& ACDEF H10,50,C'------'                                                 
*&&US*&& ACDEF H10,50,C'      '                                                 
         SPROG 5                                                                
         ACDEF H9,27,C'                            '                            
         ACDEF H10,27,C'                            '                           
         ACDEF H9,2,C'INPUT TYPE          AMOUNT'                               
*&&UK*&& ACDEF H10,2,C'----------          ------'                              
*&&US*&& ACDEF H10,3,C'                          '                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREP6101 11/14/95'                                      
         END                                                                    
