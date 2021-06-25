*          DATA SET ACREPXS01  AT LEVEL 006 AS OF 07/07/07                      
*PHASE ACXS01A                                                                  
         TITLE 'RESTORE PEELS TO OFFICE B/FRWD'                                 
ACXS01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'RESTORE PEELS TO OFFICE B/FRWD'                          
         ACDEF H2,38,C'------------------------------'                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H9,2,C'ACCOUNT'                                                  
         ACDEF H10,2,C'--------------'                                          
         ACDEF H9,17,C'NAME'                                                    
         ACDEF H10,17,20C'-'                                                    
         ACDEF H9,36,C' BALANCE FRWD'                                           
         ACDEF H10,36,C'---BEFORE---'                                           
         ACDEF H9,50,C' BALANCE FRWD'                                           
         ACDEF H10,50,C'----AFTER---'                                           
         ACDEF H9,65,C'  DR BALANCE '                                           
         ACDEF H10,65,C'---BEFORE---'                                           
         ACDEF H9,79,C'  DR BALANCE '                                           
         ACDEF H10,79,C'----AFTER---'                                           
         ACDEF H9,93,C'  CR BALANCE '                                           
         ACDEF H10,93,C'---BEFORE---'                                           
         ACDEF H9,107,C'  CR BALANCE'                                           
         ACDEF H10,107,C' ---AFTER--'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPXS01 07/07/07'                                      
         END                                                                    
