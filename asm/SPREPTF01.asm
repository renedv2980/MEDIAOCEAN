*          DATA SET SPREPTF01  AT LEVEL 009 AS OF 04/08/98                      
*PHASE SPTF01A                                                                  
         TITLE 'SPFX01 - TALENT FACTOR CHANGE- SPECS'                           
SPTF01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'CHANGE THE TALENT FACTOR'                                
         SSPEC H2,55,C'------------------------'                                
         SSPEC H3,58,PERIOD                                                     
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H3,1,MEDIA                                                       
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,C'.'                                                        
         SSPEC H6,1,C'PRODUCT  MARKET  MASTER EST   '                           
         SSPEC H7,1,C'-------  ------  ----------   '                           
         SSPEC H6,32,C'PRODUCT  MARKET  MASTER EST   '                          
         SSPEC H7,32,C'-------  ------  ----------   '                          
         SSPEC H6,63,C'PRODUCT  MARKET  MASTER EST   '                          
         SSPEC H7,63,C'-------  ------  ----------   '                          
         SSPEC H6,94,C'PRODUCT  MARKET  MASTER EST   '                          
         SSPEC H7,94,C'-------  ------  ----------   '                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPTF01 04/08/98'                                      
         END                                                                    
