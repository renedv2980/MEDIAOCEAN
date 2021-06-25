*          DATA SET SPREPAS01  AT LEVEL 002 AS OF 02/24/10                      
*PHASE SPAS01A                                                                  
SPAS01   TITLE '- Accent File Extract  - Specs'                                 
SPAS01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
                                                                                
         SSPEC H1,54,C'Accent File Extract'                                     
         SSPEC H2,54,C'-------------------'                                     
                                                                                
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H1,100,PAGE                                                      
         SSPEC H2,100,REPORT                                                    
                                                                                
         SSPEC H5,1,C'AG M CLT'                                                 
         SSPEC H5,10,C'           Ordered  '                                    
         SSPEC H5,31,C'              Paid  '                                    
         SSPEC H5,52,C'            Billed  '                                    
         SSPEC H5,73,C'          Assigned  '                                    
         SSPEC H5,94,C'          Unbilled  '                                    
         SSPEC H5,115,C'  Ins. Mth Ordered  '                                   
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPAS01 02/24/10'                                      
         END                                                                    
