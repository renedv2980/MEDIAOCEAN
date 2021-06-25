*          DATA SET ACREPIK01  AT LEVEL 005 AS OF 08/17/00                      
*PHASE ACIK01A                                                                  
         TITLE 'CALVIN KLEIN DOWNLOAD'                                          
ACIK01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF GETOPT,NO                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,                                                         
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
**       ACDEF H1,2,RUN                                                         
**       ACDEF H1,070,C'CALVIN KLEIN DOWNLOAD'                                  
**       ACDEF H1,130,PAGE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPIK01 08/17/00'                                      
         END                                                                    
