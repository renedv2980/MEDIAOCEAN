*          DATA SET ACREPXU01  AT LEVEL 005 AS OF 07/07/07                      
*PHASE ACXU01A                                                                  
         TITLE 'SPECS: MISSING TRNS POSTING FOR BATCH'                          
ACXU01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,36,C'MISSING POSTING REPORT'                                  
         ACDEF H2,36,C'----------------------'                                  
         ACDEF H3,1,C'COMP'                                                     
         ACDEF H4,1,C'CODE'                                                     
         ACDEF H5,1,C'----'                                                     
         ACDEF H4,7,C'TYPE'                                                     
         ACDEF H5,7,C'----'                                                     
         ACDEF H3,13,C'BATCH'                                                   
         ACDEF H4,13,C'REF  '                                                   
         ACDEF H5,13,C'-----'                                                   
         ACDEF H3,20,C'ITEM'                                                    
         ACDEF H4,20,C'NUM '                                                    
         ACDEF H5,20,C'----'                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPXU01 07/07/07'                                      
         END                                                                    
