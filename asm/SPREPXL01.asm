*          DATA SET SPREPXL01  AT LEVEL 006 AS OF 08/29/00                      
*PHASE SPXL01A                                                                  
         TITLE 'SPMD01 - SPOTPAK BUYER WORKLOAD ANALYSIS SPECS'                 
SPXL01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC GET,MARKET                                                       
*                                                                               
         SPROG 0,1                                                              
*                                                                               
         SSPEC H1,55,C'BUYER WORKLOAD ANALYSIS'                                 
         SSPEC H2,55,C'-----------------------'                                 
*                                                                               
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H4,1,MEDIA                                                       
         SSPEC H3,51,PERIOD                                                     
         SSPEC H5,55,MGROUP                                                     
*                                                                               
         SSPEC H1,100,REPORT                                                    
         SSPEC H2,100,PAGE                                                      
         SSPEC H2,111,REQUESTOR                                                 
*                                                                               
 END                                                                            
