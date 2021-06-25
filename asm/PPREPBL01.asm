*          DATA SET PPREPBL01  AT LEVEL 048 AS OF 12/01/97                      
*PHASE PPBL01A                                                                  
         TITLE 'PPBL01 - PRINT DDS BILLING INTERFACE - SPECS'                   
PPBL01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048PPREPBL01 12/01/97'                                      
         END                                                                    
