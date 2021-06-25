*          DATA SET ACREPIO01  AT LEVEL 004 AS OF 05/30/12                      
*PHASE ACIO01A                                                                  
         TITLE '- SPECS FOR REQUESTABLE DAILY JOURNAL'                          
ACIO01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF F1,2,REQDETS                                                     
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H2,36,C'INTERCOMPANY BALANCE REPORT'                             
         ACDEF H3,36,C'---------------------------'                             
         ACDEF H4,2,C'BTCH TY MOA    ACCOUNT-CODE   OF CONTRA-CODE   '          
         ACDEF H5,2,C'---- -- ------ -------------- -- --------------'          
         ACDEF H4,50,C'DATE     INVOICE         DEBIT         CREDIT '          
         ACDEF H5,50,C'-------- ------- -------------- --------------'          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPIO01 05/30/12'                                      
         END                                                                    
