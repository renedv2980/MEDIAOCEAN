*          DATA SET ACREPLD01  AT LEVEL 032 AS OF 08/17/00                      
*PHASE ACLD01A                                                                  
         TITLE 'TIMESHEET LOCK DATE CHECK'                                      
ACLD01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'TIMESHEET LOCK DATE'                                     
         ACDEF H2,38,C'-------------------'                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H9,1,C'PERSON    START DT  END DT    OFFICE  DEPT    '           
         ACDEF H10,1,C'------    --------  ------    ------  ----'              
         ACDEF H9,47,C'SUB-DEPT  LOCK-DATE'                                     
         ACDEF H10,47,C'--------  ---------'                                    
         ACDEF H9,69,C'    DESCRIPTION     '                                    
         ACDEF H10,69,C'    -----------    '                                    
         ACDEF H9,99,C'CAL STRT   CAL END  PRD END   TIME END'                  
         ACDEF H10,99,C'--------   -------  -------   --------'                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACREPLD01 08/17/00'                                      
         END                                                                    
