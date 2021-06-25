*          DATA SET ACREPWP01  AT LEVEL 004 AS OF 10/18/90                      
*PHASE ACWP01A,+0                                                               
         TITLE 'SPECS FOR WPP AGED DEBTORS REPORT'                              
         PRINT NOGEN                                                            
ACWP01   CSECT                                                                  
*                                                                               
         ACDEF WIDTH,198                                                        
         ACDEF READ,ACCOUNTS                                                    
         ACDEF SPROG,0,1,2,3                                                    
*                                                                               
         ACDEF H1,2,RUN                                                         
         ACDEF H1,70,C'AGED DEBTORS ANALYSIS'                                   
         ACDEF H2,70,C'_____________________'                                   
         ACDEF H3,144,REPORT                                                    
         ACDEF H5,144,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPWP01 10/18/90'                                      
         END                                                                    
