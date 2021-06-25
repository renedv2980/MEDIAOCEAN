*          DATA SET ACREPAM01  AT LEVEL 016 AS OF 04/11/18                      
*PHASE ACAM01A                                                                  
         TITLE 'TO FIND OUT ELM IN REC HAVING NO TYPE && SUBTYPE'               
ACAM01A  CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                COMPANY & DISK ADDRESS OF RECORD          
         ACDEF H1,001,C'COMP-CODE',9,L                                          
         ACDEF H2,001,C'---------',9,L                                          
         ACDEF H1,013,C'ORDER-NO',8,L                                           
         ACDEF H2,013,C'--------',8,L                                           
         ACDEF H1,024,C'WORK-CODE',9,L                                          
         ACDEF H2,024,C'---------',9,L                                          
         ACDEF H1,037,C'PND-ORDER',9,L                                          
         ACDEF H2,037,C'---------',9,L                                          
         ACDEF H1,049,C'EST-AMOUNT',10,L                                        
         ACDEF H2,049,C'----------',10,L                                        
         ACDEF H1,061,C'NO-OF-INV',9,L                                          
         ACDEF H2,061,C'---------',9,L                                          
         ACDEF H1,074,C'INVC-AMT',8,L                                           
         ACDEF H2,074,C'---------',8,L                                          
         ACDEF H1,088,C'DISK-ADDR',9,L                                          
         ACDEF H2,088,C'---------',9,L                                          
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREPAM01 04/11/18'                                      
         END                                                                    
