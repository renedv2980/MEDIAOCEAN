*          DATA SET SPREPI801  AT LEVEL 006 AS OF 11/08/01                      
*PHASE SPI801A                                                                  
         TITLE 'SPI801'                                                         
SPI801   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
*                                                                               
         SPROG 0,10                                                             
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,PAGE                                                       
         SSPEC H5,97,REPORT                                                     
         SSPEC H1,50,C'SPOTPAK INVOICE RECORD PURGE/LIST'                       
         SSPEC H2,50,C'---------------------------------'                       
         SSPEC H3,50,PERIOD                                                     
         SSPEC H8,1,C'   CLT   STATION     DATE  '                              
         SSPEC H9,1,C'   ---   -------   --------'                              
         SPROG 10                                                               
         SSPEC H3,1,CLIENT                                                      
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPI801 11/08/01'                                      
         END                                                                    
