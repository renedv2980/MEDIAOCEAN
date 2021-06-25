*          DATA SET SPREPI601  AT LEVEL 024 AS OF 08/29/00                      
*PHASE SPI601A                                                                  
         TITLE 'SPI601'                                                         
SPI601   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,PAGE                                                       
         SSPEC H5,97,REPORT                                                     
         SSPEC H1,50,C'SPOTPAK INVOICE RECORD LISTING'                          
         SSPEC H2,50,C'------------------------------'                          
         SSPEC H2,1,CLIENT                                                      
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPREPI601 08/29/00'                                      
         END                                                                    
