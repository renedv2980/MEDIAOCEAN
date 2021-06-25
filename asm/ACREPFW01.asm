*          DATA SET ACREPFW01  AT LEVEL 006 AS OF 03/25/04                      
*PHASE ACFW01A                                                                  
         TITLE 'SPECS FOR FACWK FILES'                                          
ACFW01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF NOREP,REQDETS                                                    
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,53,C'FACWRK RECOVERY FILE REPORT'                             
         ACDEF H2,53,27X'BF'                                                    
         ACDEF H3,2,COMPANY                                                     
         ACDEF H1,99,REPORT                                                     
         ACDEF H1,115,PAGE                                                      
         ACDEF H2,99,REQUESTOR                                                  
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H6,2,C'COUNT TYP  FILE'                                          
         ACDEF H6,40,C'KEY'                                                     
         ACDEF H6,110,C'STATUS'                                                 
         ACDEF H6,125,C'D/A'                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPFW01 03/25/04'                                      
         END                                                                    
