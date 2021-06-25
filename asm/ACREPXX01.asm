*          DATA SET ACREPXX01  AT LEVEL 002 AS OF 07/07/07                      
*PHASE ACXX01A                                                                  
         TITLE 'BATCH HEADER FIX'                                               
ACXX01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,36,C'BATCH HEADER FIX'                                        
         ACDEF H2,36,C'----------------'                                        
         ACDEF H3,1,C'COMP'                                                     
         ACDEF H4,1,C'CODE'                                                     
         ACDEF H5,1,C'----'                                                     
         ACDEF H4,9,C'ACT DATE'                                                 
         ACDEF H5,9,C'--------'                                                 
         ACDEF H3,19,C'BATCH'                                                   
         ACDEF H4,19,C'REF  '                                                   
         ACDEF H5,19,C'-----'                                                   
         ACDEF H4,27,C'     BHDCASHA'                                           
         ACDEF H5,27,C'     --------'                                           
         ACDEF H4,43,C'     BHDCASHC'                                           
         ACDEF H5,43,C'     --------'                                           
         ACDEF H4,59,C'       BIAAMT'                                           
         ACDEF H5,59,C'     --------'                                           
         ACDEF H4,75,C'     BHDTOTCR'                                           
         ACDEF H5,75,C'     --------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPXX01 07/07/07'                                      
         END                                                                    
