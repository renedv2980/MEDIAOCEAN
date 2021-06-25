*          DATA SET TAREPTOT   AT LEVEL 002 AS OF 08/10/00                      
*PHASE TAREPTOA TAREPTOT                                                        
*                                                                               
         TITLE 'TAREPTOT - TOTTAB FOR TALENT PAYROLL REG (TAREP17)'             
TOTTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF ACCUMULATOR TOTALS                                            
*                                                                               
         ORG   *+MAXTOTS*TOTLEN+1                                               
*                                                                               
*TAREPPYRD                                                                      
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAREPPYRD                                                      
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREPTOT  08/10/00'                                      
         END                                                                    
