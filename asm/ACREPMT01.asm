*          DATA SET ACREPMT01  AT LEVEL 002 AS OF 06/24/19                      
*PHASE ACMT01A                                                                  
ACMT01   TITLE 'Specs for missing time report'                                  
ACMT01   CSECT ,                                                                
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TIME                                                        
                                                                                
         ACDEF SPROG,1,2,3         Report mode                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,47,C'Missing timesheet report'                                
         ACDEF H2,47,C'------------------------'                                
         ACDEF H1,89,REPORT                                                     
         ACDEF H1,111,PAGE                                                      
         ACDEF H4,2,C'Company'                                                  
         ACDEF H5,2,C'Office'                                                   
         ACDEF H6,2,C'Department'                                               
         ACDEF H7,2,C'Sub-department'                                           
         ACDEF H4,79,C'Statuses:'                                               
         ACDEF H7,65,C'Timesheet period range:'                                 
         ACDEF F1,2,REQDETS                                                     
                                                                                
         ACDEF M2,2,C'Name'                                                     
         ACDEF M2,28,C'Emp. #'                                                  
         ACDEF M2,36,C'Person id'                                               
         ACDEF M1,46,C' Total'                                                  
         ACDEF M2,46,C'Missing'                                                 
         ACDEF M2,54,C'#'                                                       
         ACDEF M2,57,C'Start/End dates'                                         
         ACDEF M2,74,C'St'                                                      
         ACDEF M2,77,C'Submitter'                                               
         ACDEF M2,104,C'Line manager'                                           
                                                                                
         SPROG 2                                                                
         ACDEF H2,47,C' Awaiting approval only '                                
                                                                                
         SPROG 3                                                                
         ACDEF H2,47,C'     Overdue only       '                                
                                                                                
         SPROG 4                                                                
         ACDEF H2,42,C'Awaiting approval and overdue only'                      
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPMT01 06/24/19'                                      
         END   ,                                                                
