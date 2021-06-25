*          DATA SET SPREPDL01  AT LEVEL 016 AS OF 08/29/01                      
*PHASE SPDL01A                                                                  
         TITLE 'SPDL01 - SPECS'                                                 
SPDL01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
*                                                                               
         SPROG 0,THRU,9                                                         
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
*                                                                               
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H5,77,PAGE                                                       
         SSPEC H5,87,REPORT                                                     
*                                                                               
         SSPEC H1,35,C'SPOTPAK DEMO UPDATE REPORT'                              
         SSPEC H2,35,C'--------------------------'                              
         SSPEC H9,11,C'NETWORK     EST-LIN      STATION     SHOW'               
         SSPEC H10,11,C'-------     -------      -------     ----'              
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
         DC    C'REQLST='                                                       
         DC    CL25'5004APROGRAM CODE'                                          
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPDL01 08/29/01'                                      
         END                                                                    
