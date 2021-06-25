*          DATA SET ACREPSC01  AT LEVEL 033 AS OF 08/16/00                      
*PHASE ACSC01A,+0                                                               
         TITLE 'COST SECURITY PERSON NAME MATCH'                                
ACX801   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'PERSON RECORD FIX'                                       
         ACDEF H2,38,C'-----------------'                                       
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
*        ACDEF H9,1,C'ALPHAID'                                                  
*        ACDEF H10,1,C'-------'                                                 
         ACDEF H9,1,C' PERSON'                                                  
         ACDEF H10,1,C' ------'                                                 
         ACDEF H9,11,C'  PID'                                                   
         ACDEF H10,11,C'  ---'                                                  
         ACDEF H9,23,C'LAST NAME (COST)'                                        
         ACDEF H10,23,C'----------------'                                       
         ACDEF H9,41,C'FIRST NAME (COST)'                                       
         ACDEF H10,41,C'-----------------'                                      
         ACDEF H9,61,C' PIDNAME'                                                
         ACDEF H10,61,C' -------'                                               
         ACDEF H9,71,C'  LAST NAME (SEC)'                                       
         ACDEF H10,71,C'  ---------------'                                      
         ACDEF H9,89,C'  FIRST NAME (SEC)'                                      
         ACDEF H10,89,C'  ----------------'                                     
         ACDEF H9,107,C'  MID IN '                                              
         ACDEF H10,107,C'  ------'                                              
         ACDEF H9,127,C'DESCRIPTION'                                            
         ACDEF H10,127,C'-----------'                                           
         ACDEF H9,161,C'TERM DATE'                                              
         ACDEF H10,161,C'---------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACREPSC01 08/16/00'                                      
         END                                                                    
