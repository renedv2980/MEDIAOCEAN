*          DATA SET PPREPVK01  AT LEVEL 002 AS OF 03/12/20                      
*PHASE PPVK01A                                                                  
         TITLE 'PPVK01 - Publication Lock Report'                               
PPVK01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PUBDIR                                                    
         FSPEC UPDATE,PUBFILE                                                   
*                                                                               
         SPROG 10,11,20,21,30,31,40,41,50,51,60,61                              
         PSPEC H1,51,C'PrintPak Publication Lock Program'                       
         PSPEC H2,51,C'---------------------------------'                       
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 10,11                                                            
         PSPEC H3,1,C'** Publication Code **'                                   
         PSPEC H4,4,C'---'                                                      
*                                                                               
         SPROG 20,21                                                            
         PSPEC H3,1,C'** Publication Name **'                                   
         PSPEC H4,4,C'---'                                                      
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPVK01 03/12/20'                                      
         END                                                                    
