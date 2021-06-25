*          DATA SET ACREPTE01  AT LEVEL 079 AS OF 01/08/02                      
*PHASE ACTE01A                                                                  
         TITLE 'ACTE01 - 1099 TAX REPORT SPECS'                                 
ACTE01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         SPROG 0,1,2                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H3,2,ORIGIN                                                      
*                                                                               
         ACDEF H6,5,C'AGENCY'                                                   
         ACDEF H6,18,C'YEAR'                                                    
         ACDEF H6,27,C'COMPANY NAME'                                            
         ACDEF H6,65,C'PERS'                                                    
         ACDEF H6,71,C'PH #'                                                    
         ACDEF H6,78,C'FRMS'                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079ACREPTE01 01/08/02'                                      
         END                                                                    
