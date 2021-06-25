*          DATA SET ACREPTM01  AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACTM01A,+0                                                               
         TITLE 'TIMESHEET CONVERSION'                                           
ACTM01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,1,RUN                                                         
         ACDEF H3,1,COMPANY                                                     
         ACDEF H3,133,PAGE                                                      
         ACDEF H4,1,REQUESTOR                                                   
         ACDEF H4,133,C'REPORT ACTM'                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H2,60,C'TIMESHEET CONVERSION REPORT'                             
         ACDEF H3,60,C'---------------------------'                             
         ACDEF H4,63,PERIOD                                                     
         ACDEF H9,02,C'CODE'                                                    
         ACDEF H9,15,C'NAME'                                                    
         ACDEF H8,36,C'WEEK-END'                                                
         ACDEF H9,36,C'  DATE  '                                                
*                                                                               
         ACDEF H08,045,C'-------------PERSON LEDGER------------'                
         ACDEF H09,045,C'   BILLABLE BILLABLE     REAL    OTHER'                
         ACDEF H10,045,C'     COST     HOURS     HOURS    HOURS'                
*                                                                               
         ACDEF H08,084,C'---------JOB  LEDGER---------'                         
         ACDEF H09,084,C'   BILLABLE BILLABLE     REAL'                         
         ACDEF H10,084,C'     COST     HOURS     HOURS'                         
*                                                                               
         ACDEF H08,114,C'-------------OUTPUT RECORDS-----------'                
         ACDEF H09,114,C'   BILLABLE BILLABLE     REAL    OTHER'                
         ACDEF H10,114,C'     COST     HOURS     HOURS    HOURS'                
         ACDEF H09,153,C'ERR '                                                  
         ACDEF H10,153,C'CNT '                                                  
         ACDEF H09,158,C'ERR '                                                  
         ACDEF H10,158,C'E/S '                                                  
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H2,60,C'TIMESHEET SUMMARY REPORT'                                
         ACDEF H3,60,C'------------------------'                                
         ACDEF H8,14,C'          INPUT         OUTPUT'                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPTM01 08/16/00'                                      
         END                                                                    
