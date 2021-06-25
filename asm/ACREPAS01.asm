*          DATA SET ACREPAS01  AT LEVEL 001 AS OF 06/05/95                      
*PHASE ACAS01A,+0                                                               
         TITLE 'SPECS FOR AUTO SALARY UPDATE PROGRAM'                           
ACAS01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,98,REPORT                                                     
         ASPEC H1,115,PAGE                                                      
         ASPEC H3,2,COMPANY                                                     
         ASPEC F1,2,REQDETS                                                     
         ASPEC H1,40,C'ACCPAK COSTING SYSTEM, FAST SALARY UPDATE'               
*                                                                               
         SPROG 0                                                                
         ASPEC H6,1,C'U/L/ACCOUNT'                                              
         ASPEC H6,20,C'TOTAL'                                                   
         ASPEC H6,30,C'50-SALARY  40-OVERTIME  30-TEMPORARY'                    
         ASPEC H6,70,C'20-BONUS   10-BENIFIT     NAME'                          
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPAS01 06/05/95'                                      
         END                                                                    
