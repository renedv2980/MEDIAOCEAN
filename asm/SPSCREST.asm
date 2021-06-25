*          DATA SET SPSCREST   AT LEVEL 009 AS OF 05/03/91                      
*PHASE SP9601,+0                                                                
         TITLE 'SP9601 - LISTER SPECS'                                          
SP9601   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
*                                                                               
*        SSPEC H1,52,C'SPOTPAK P&G ESTIMATE LISTING'                            
         SSPEC H3,49,PERIOD                                                     
         SSPEC H2,52,C'----------------------------'                            
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H1,1,MEDIA                                                       
         SSPEC H4,98,REPORT                                                     
         SSPEC H4,118,PAGE                                                      
         SSPEC H3,1,CLIENT                                                      
         SSPEC H5,98,RUN                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPSCREST  05/03/91'                                      
         END                                                                    
