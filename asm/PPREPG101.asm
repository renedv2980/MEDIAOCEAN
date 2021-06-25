*          DATA SET PPREPG101  AT LEVEL 003 AS OF 08/09/00                      
*PHASE PPG101A                                                                  
         TITLE 'PPG101 -  INSERTION ORDER TA GENERATOR'                         
PPG101   CSECT                                                                  
         PRINT NOGEN                                                            
         PSPEC H1,47,C'PRINTPAK INSERTION ORDER T/A GENERATOR'                  
         PSPEC H2,47,C'--------------------------------------'                  
*                                                                               
         PSPEC H1,98,C'DONOVAN DATA SYSTEMS'                                    
         PSPEC H4,98,REPORT                                                     
         PSPEC H5,98,RUN                                                        
         PSPEC H5,124,PAGE                                                      
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREPG101 08/09/00'                                      
         END                                                                    
