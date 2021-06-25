*          DATA SET PPREPIP01  AT LEVEL 001 AS OF 09/15/15                      
*PHASE PPIP01A                                                                  
         TITLE 'PPIP01 - Print Autopay records report'                          
PPIP01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         SPROG 10,11,20,21,30,31,40,41,50,51,60,61                              
         PSPEC H1,51,C'Print Autopay records report'                            
         PSPEC H2,51,C'----------------------------'                            
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 10,11                                                            
         PSPEC H3,1,C'** Column ONE **'                                         
         PSPEC H4,4,C'---'                                                      
*                                                                               
         SPROG 20,21                                                            
         PSPEC H3,1,C'** Column TWO **'                                         
         PSPEC H4,4,C'---'                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PPREPIP01 09/15/15'                                      
         END                                                                    
