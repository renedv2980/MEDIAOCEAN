*          DATA SET ACREPGR01  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACGR01A                                                                  
         TITLE 'GST RULES RECORD REPORT'                                        
ACGR01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,1,RUN                                                         
         ACDEF H1,105,PAGE                                                      
         ACDEF H2,1,C'COMPANY'                                                  
         ACDEF H3,105,REPORT                                                    
         ACDEF H4,1,AC#TYPE,4                                                   
         ACDEF H4,105,REQUESTOR                                                 
*                                                                               
*              REGULAR REPORT                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,55,AC#GSTRP,23                                                
         ACDEF H8,8,C'OFF/PROV'                                                 
         ACDEF H8,17,AC#EFFDT,8                                                 
         ACDEF H8,27,AC#TAXRG,14                                                
         ACDEF H8,44,AC#CODE,4                                                  
         ACDEF H8,50,AC#TYPE1,4                                                 
         ACDEF H8,62,AC#ACC,12                                                  
         ACDEF H8,77,AC#ACCN,12                                                 
         ACDEF H8,113,AC#RATE,4                                                 
**                                                                              
**   ACDDEQUS                                                                   
**                                                                              
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPGR01 08/17/00'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
