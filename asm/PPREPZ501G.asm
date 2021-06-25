*          DATA SET PPREPZ501G AT LEVEL 021 AS OF 08/09/00                      
*PHASE PPZ501A                                                                  
         TITLE 'PPZ501 - PZ - INVOICE CONVERTER'                                
         PRINT NOGEN                                                            
PPZ501   CSECT                                                                  
         SPACE 2                                                                
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
*                                                                               
         PSPEC H1,11,REQUESTOR                                                  
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
*                                                                               
         PSPEC H1,52,C'PZ - INVOICE CONVERSION REPORT'                          
         PSPEC H2,52,C'------------------------------'                          
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PPREPZ501G08/09/00'                                      
         END                                                                    
