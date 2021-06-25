*          DATA SET SPREPZ601  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPZ601A                                                                  
         TITLE 'SPZ601 - EASI - INVOICE FACSIMILE REPORT'                       
         PRINT NOGEN                                                            
SPZ601   CSECT                                                                  
         SPACE 2                                                                
         FSPEC USE,SP0003                                                       
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPZ601 08/29/00'                                      
         END                                                                    
