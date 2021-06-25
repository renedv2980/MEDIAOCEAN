*          DATA SET CTREP5501  AT LEVEL 005 AS OF 08/22/00                      
*PHASE CT5501A                                                                  
         TITLE 'SPECS FOR ID REPORT'                                            
CT5501   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,IDS                                                         
         ASPEC H1,2,RUN                                                         
         ASPEC H1,51,C'ID REPORT'                                               
         ASPEC H2,51,C'---------'                                               
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,C'SYSTEM    NO    AC    SIGN-ON'                            
         ASPEC H5,2,C'------    --    --    -------'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTREP5501 08/22/00'                                      
         END                                                                    
