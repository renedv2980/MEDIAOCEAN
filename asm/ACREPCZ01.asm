*          DATA SET ACREPCZ01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACCZ01A,+0                                                               
         TITLE 'CHECK AUTHORIZATION FILE RECORD REPORT PROGRAM'                 
ACCZ01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF WIDTH,198                                                        
         ACDEF H1,67,C'CHECK AUTHORIZATION FILE REPORT'                         
         ACDEF H2,67,C'-------------------------------'                         
         ACDEF H3,2,RUN                                                         
         ACDEF H3,143,REPORT                                                    
*        ACDEF H1,99,PAGE                                                       
*        ACDEF H4,2,COMPANY                                                     
*        ACDEF H4,83,REQUESTOR                                                  
*        ACDEF H5,2,UNIT                                                        
*        ACDEF H6,2,LEDGER                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPCZ01 08/16/00'                                      
         END                                                                    
