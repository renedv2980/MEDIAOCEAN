*          DATA SET PPREP0201X AT LEVEL 007 AS OF 12/21/99                      
*PHASE PP0201B,+0                                                               
         TITLE 'PP0201 - PRTFIX SPECS'                                          
PP0201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC READ,BUYS                                                        
*                                                                               
         PSPEC H1,52,C'PRINTPAK RECORD FIX PROGRAM'                             
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'---------------------------'                             
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREP0201X12/21/99'                                      
         END                                                                    
