*          DATA SET ACREPWK01  AT LEVEL 002 AS OF 03/17/04                      
*PHASE ACWK01A,*+0                                                              
         TITLE 'ACWK01 - SPECS FOR WORKER FILE COMPARE'                         
ACWK01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         RSPEC REQUEST,NOSUM                                                    
         RSPEC REQUEST,NOREP                                                    
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H2,01,C'OLD-REC'                                                 
         ACDEF H2,09,C'NEW-REC'                                                 
         ACDEF H2,17,C'  STATUS   '                                             
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H2,01,C' COPY  '                                                 
         ACDEF H2,09,C'CHANGE '                                                 
         ACDEF H2,17,C'  STATUS   '                                             
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H1,45,C'COMPARISON SUMMARY'                                      
         ACDEF H2,30,C'OLD FILE'                                                
         ACDEF H2,40,C'NEW-FILE'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPWK01 03/17/04'                                      
         END                                                                    
