*          DATA SET SPREPXW01  AT LEVEL 004 AS OF 08/29/00                      
*PHASE SPXW01A                                                                  
         PRINT NOGEN                                                            
SPXW01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,55,C'SPOTPAK INTERAGENCY TRANSFER'                            
         SSPEC H2,55,C'----------------------------'                            
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SSPEC H5,2,C'PRD EST MARKET RECORDS  DESCRIPTION'                      
         SSPEC H6,2,C'--- --- ------ ------  -----------'                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPXW01 08/29/00'                                      
         END                                                                    
