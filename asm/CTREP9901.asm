*          DATA SET CTREP9901  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT9901A                                                                  
         TITLE 'SPECS FOR SALLY''S TEST REPORT'                                 
CT9901   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,51,C'TEST REPORT'                                             
         SSPEC H2,51,C'-----------'                                             
         SSPEC H1,85,REPORT                                                     
         SSPEC H1,99,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP9901 08/22/00'                                      
         END                                                                    
