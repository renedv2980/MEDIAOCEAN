*          DATA SET ACREPX101  AT LEVEL 007 AS OF 07/07/07                      
*PHASE ACX101A                                                                  
         TITLE 'SPECS FOR CPOEL FIX'                                            
ACX101   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,36,C'FIXED CPOEL REPORT'                                      
         ACDEF H2,36,C'------------------'                                      
         ACDEF H4,2,C'ORDER'                                                    
         ACDEF H5,2,C'------'                                                   
         ACDEF H4,11,C'QNTY '                                                   
         ACDEF H5,11,C'-----'                                                   
         ACDEF H4,18,C'STOCK'                                                   
         ACDEF H5,18,C'--------'                                                
         ACDEF H4,28,C'DESCRIPTION'                                             
         ACDEF H5,28,C'-------------------------'                               
         ACDEF H3,56,C'UNIT'                                                    
         ACDEF H4,56,C'PRICE'                                                   
         ACDEF H5,56,C'------------'                                            
         ACDEF H3,70,C'UNIT '                                                   
         ACDEF H4,70,C'RETAIL'                                                  
         ACDEF H5,70,C'------------'                                            
         ACDEF H4,84,C'RATIO'                                                   
         ACDEF H5,84,C'--------'                                                
         ACDEF H3,94,C'QNTY'                                                    
         ACDEF H4,94,C'INVCD'                                                   
         ACDEF H5,94,C'-----'                                                   
         ACDEF H3,101,C'AMOUNT'                                                 
         ACDEF H4,101,C'INVOICED'                                               
         ACDEF H5,101,C'------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPX101 07/07/07'                                      
         END                                                                    
