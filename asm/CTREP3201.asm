*          DATA SET CTREP3201  AT LEVEL 004 AS OF 08/22/00                      
*PHASE CT3201A                                                                  
         TITLE 'SPECS FOR CPP PROJECTION FACTOR'                                
         PRINT NOGEN                                                            
CT3201   CSECT                                                                  
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'CPP PROJECTION FACTORS'                                  
         ASPEC H2,40,22C'-'                                                     
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,C'AGENCY'                                                   
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H8,35,C'FORMULA   FORMULA   PROJECTION'                          
         ASPEC H9,35,C' CODE      MONTH      FACTOR  '                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTREP3201 08/22/00'                                      
         END                                                                    
