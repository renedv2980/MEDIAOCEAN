*          DATA SET ACREPM201  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACM201A                                                                  
         TITLE 'ACM201 - SPECS FOR MANPOWER REPORTING SYSTEM'                   
ACM201   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
*&&UK*&& ACDEF MAXLINES,59                                                      
         ACDEF GETOPT,N                                                         
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H2,2,REQUESTOR                                                   
*&&UK*&& ACDEF H3,2,CURRENCY                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPM201 08/17/00'                                      
         END                                                                    
