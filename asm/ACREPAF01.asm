*          DATA SET ACREPAF01  AT LEVEL 001 AS OF 03/27/19                      
*PHASE ACAF01A                                                                  
         TITLE 'ACAF01 - SAP SPECIAL RECS TO STORE ACC KEYS'                    
ACAF01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF GETOPT,N                                                         
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,143,C'FILE UPLOAD'                                            
         ACDEF H2,143,PAGE                                                      
*                                                                               
         ACDEF H6,002,C'VOUCHER CODE '                                          
         ACDEF H6,022,C'GROSS AMOUNT'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPAF01 03/27/19'                                      
         END                                                                    
