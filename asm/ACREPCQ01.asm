*          DATA SET ACREPCQ01  AT LEVEL 005 AS OF 07/03/07                      
*PHASE ACCQ01A,+0                                                               
         TITLE 'SALARY INTERFACE REPORT'                                        
ACCQ01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,1,RUN,WIDE=198                                                
         ACDEF H1,136,PAGE,WIDE=198                                             
         ACDEF H2,1,C'COMPANY',WIDE=198                                         
         ACDEF H3,136,C'REPORT ACCQ',WIDE=198                                   
         ACDEF H4,136,REQUESTOR,WIDE=198                                        
*                                                                               
*              REGULAR REPORT                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,62,C'SALARY INTERFACE REPORT',WIDE=198                        
         ACDEF H2,62,C'-----------------------',WIDE=198                        
         ACDEF H9,03,C'PERSON',WIDE=198                                         
         ACDEF H9,13,C'NAME (LAST, FIRST M.I.)',WIDE=198                        
         ACDEF H8,60,C'CHECK',WIDE=198                                          
         ACDEF H9,60,C'DATE ',WIDE=198                                          
         ACDEF H8,69,C'----- PAYROLL ----',WIDE=198                             
         ACDEF H9,69,C'CODE    DESCRIPTION',WIDE=198                            
         ACDEF H9,100,C'AMOUNT',WIDE=198                                        
*                                                                               
*              PERSON REPORT                                                    
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,62,C'SALARY INTERFACE REPORT',WIDE=198                        
         ACDEF H2,62,C'-----------------------',WIDE=198                        
         ACDEF H9,03,C'PERSON',WIDE=198                                         
         ACDEF H9,13,C'NAME (LAST, FIRST M.I.)',WIDE=198                        
         ACDEF H9,100,C'AMOUNT',WIDE=198                                        
*                                                                               
*              ERROR REPORT                                                     
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H1,62,C'SALARY INTERFACE ERROR REPORT',WIDE=198                  
         ACDEF H2,62,C'-----------------------------',WIDE=198                  
         ACDEF H9,03,C'OFF/DPT/SDPT/PRSN',WIDE=198                              
         ACDEF H9,25,C'NAME (LAST, FIRST M.I.)',WIDE=198                        
         ACDEF H8,60,C'CHECK',WIDE=198                                          
         ACDEF H9,60,C'DATE ',WIDE=198                                          
         ACDEF H8,69,C'PAY ',WIDE=198                                           
         ACDEF H9,69,C'CODE',WIDE=198                                           
         ACDEF H9,82,C'AMOUNT',WIDE=198                                         
         ACDEF H9,93,C'OFFC',WIDE=198                                           
         ACDEF H9,98,C'DEPT',WIDE=198                                           
         ACDEF H9,103,C'SDPT',WIDE=198                                          
         ACDEF H9,108,C'STAF',WIDE=198                                          
         ACDEF H9,113,C'PERS',WIDE=198                                          
         ACDEF H9,118,C'CODE',WIDE=198                                          
*                                                                               
*              RECAP REPORT                                                     
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H1,64,C'SALARY INTERFACE RECAP',WIDE=198                         
         ACDEF H2,64,C'----------------------',WIDE=198                         
         ACDEF H9,03,C'DESCRIPTION',WIDE=198                                    
         ACDEF H9,43,C'RECORD COUNT',WIDE=198                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPCQ01 07/03/07'                                      
         END                                                                    
