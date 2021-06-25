*          DATA SET ACREPCP01  AT LEVEL 002 AS OF 07/09/07                      
*PHASE ACCP01A,+0                                                               
         TITLE 'PERSONNEL INTERFACE REPORT'                                     
ACCP01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF MAXLINES,56                                                      
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,1,RUN,WIDE=198                                                
         ACDEF H1,136,PAGE,WIDE=198                                             
         ACDEF H2,1,C'COMPANY',WIDE=198                                         
         ACDEF H3,136,C'REPORT ACCP',WIDE=198                                   
         ACDEF H4,136,REQUESTOR,WIDE=198                                        
*                                                                               
*              REGULAR REPORT                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,62,C'PERSONNEL INTERFACE REPORT',WIDE=198                     
         ACDEF H2,62,C'--------------------------',WIDE=198                     
         ACDEF H9,03,C'PERSON',WIDE=198                                         
         ACDEF H9,25,C'NAME (LAST, FIRST M.I.)',WIDE=198                        
         ACDEF H8,60,C'HIRE',WIDE=198                                           
         ACDEF H9,60,C'DATE',WIDE=198                                           
         ACDEF H8,69,C'TERMINATE',WIDE=198                                      
         ACDEF H9,69,C'  DATE   ',WIDE=198                                      
         ACDEF H8,80,C'EFFECTIVE',WIDE=198                                      
         ACDEF H9,80,C'  DATE   ',WIDE=198                                      
         ACDEF H8,91,C'TIMESHEET',WIDE=198                                      
         ACDEF H9,91,C'LOCK DATE',WIDE=198                                      
         ACDEF H9,102,C'LOCATION',WIDE=198                                      
         ACDEF H8,112,C'TYPE OF',WIDE=198                                       
         ACDEF H9,112,C' HOURS ',WIDE=198                                       
         ACDEF H9,123,C'EXEC',WIDE=198                                          
         ACDEF H9,129,C'STATUS',WIDE=198                                        
         ACDEF H8,136,C'1R STAFF',WIDE=198                                      
         ACDEF H9,136,C' RECORD ',WIDE=198                                      
         ACDEF H8,146,C' PERSON',WIDE=198                                       
         ACDEF H9,146,C' RECORD',WIDE=198                                       
*                                                                               
*              ERROR REPORT                                                     
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,62,C'PERSONNEL INTERFACE ERROR REPORT',WIDE=198               
         ACDEF H2,62,C'--------------------------------',WIDE=198               
         ACDEF H9,03,C'OFF/DPT/SDPT/PRSN',WIDE=198                              
         ACDEF H9,25,C'NAME (LAST, FIRST M.I.)',WIDE=198                        
         ACDEF H9,57,C'OFF',WIDE=198                                            
         ACDEF H9,61,C'DPT',WIDE=198                                            
         ACDEF H9,65,C'SDPT',WIDE=198                                           
         ACDEF H9,72,C'ADDITIONAL ERRORS',WIDE=198                              
*                                                                               
*              RECAP REPORT                                                     
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H1,64,C'PERSONNEL INTERFACE RECAP',WIDE=198                      
         ACDEF H2,64,C'-------------------------',WIDE=198                      
*                                                                               
         ACDEF H9,03,C'DESCRIPTION',WIDE=198                                    
         ACDEF H9,38,C'RECORD COUNT',WIDE=198                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPCP01 07/09/07'                                      
         END                                                                    
