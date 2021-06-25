*          DATA SET TAREPTO    AT LEVEL 006 AS OF 12/10/12                      
*PHASE TAREPTOA                                                                 
*                                                                               
         TITLE 'TAREPTO - TOTTAB FOR TALENT PAYROLL REG (TAREP17)'              
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
**PAN#1  DC    CL21'006TAREPTO   12/10/12'                                      
         END                                                                    
