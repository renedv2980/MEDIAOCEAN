*          DATA SET PPREPIW01  AT LEVEL 001 AS OF 09/22/15                      
*PHASE PPIW01A                                                                  
         TITLE 'PPIW01 - AUTOPAY INVOICE LISTING'                               
PPIW01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 10,11,20,21,30,31,40,41,50,51,60,61                              
         PSPEC H1,51,C'Print Autopay Unsuccessful'                              
         PSPEC H2,51,C'--------------------------'                              
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PPREPIW01 09/22/15'                                      
         END                                                                    
