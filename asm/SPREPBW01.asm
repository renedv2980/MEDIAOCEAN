*          DATA SET SPREPBW01  AT LEVEL 003 AS OF 07/14/00                      
*PHASE SPBW01A                                                                  
         TITLE 'SPBW01 - MOVES RECORDS FROM ONE GENCY TO ANOTHER'               
SPBW01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         SSPEC H1,55,C'COPY RECORDS BETWEEN AGENCIES'                           
         SSPEC H2,55,C'-----------------------------'                           
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,PAGE                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPBW01 07/14/00'                                      
         END                                                                    
