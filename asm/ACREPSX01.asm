*          DATA SET ACREPSX01  AT LEVEL 006 AS OF 01/10/02                      
*PHASE ACSX01A,*                                                                
ACSX01   TITLE '- SPECS FOR SPOT/PRINT CLEARANCE UPDATE'                        
ACSX01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF NOREP,REQDETS                                                    
         ACDEF NOSUM,REQDETS                                                    
         ACDEF MAXLINES,58                                                      
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,90,REPORT                                                     
         ACDEF H1,104,PAGE                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPSX01 01/10/02'                                      
         END                                                                    
