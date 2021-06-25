*          DATA SET PPREPZ501  AT LEVEL 022 AS OF 03/24/95                      
*PHASE PPZ501A,+0,NOAUTO                                                        
         TITLE 'PPZ501 - EPIC - INVOICE CONVERTER'                              
*        PRINT NOGEN                                                            
PPZ501   CSECT                                                                  
         SPACE 2                                                                
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
*                                                                               
         SSPEC H1,11,REQUESTOR                                                  
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H3,122,PAGE                                                      
*                                                                               
         SSPEC H1,52,C'EPIC - INVOICE CONVERSION REPORT'                        
         SSPEC H2,52,C'--------------------------------'                        
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022PPREPZ501 03/24/95'                                      
         END                                                                    
