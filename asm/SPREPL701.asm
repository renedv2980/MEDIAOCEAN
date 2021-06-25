*          DATA SET SPREPL701  AT LEVEL 008 AS OF 08/29/00                      
*PHASE SPL701A                                                                  
         PRINT NOGEN                                                            
SPL701   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         SPROG 0,THRU,8                                                         
         SSPEC H1,4,REQUESTOR                                                   
         SSPEC H1,41,C'NETWORK DEMO OVERRIDE REPORT'                            
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,41,C'----------------------------'                            
         SSPEC H2,75,AGYADD                                                     
         SSPEC H3,4,REPORT                                                      
         SSPEC H3,30,PAGE                                                       
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'1504ANETWORK'                                               
         DC    CL25'1904APROGRAM'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPL701 08/29/00'                                      
         END                                                                    
