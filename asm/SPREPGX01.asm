*          DATA SET SPREPGX01  AT LEVEL 011 AS OF 08/29/00                      
*PHASE SPGX01A                                                                  
         PRINT NOGEN                                                            
SPXW01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         READ  GOALS                                                            
         SPROG 1,THRU,2                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,55,C'SPOTPAK INTERAGENCY TRANSFER'                            
         SSPEC H2,55,C'----------------------------'                            
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SPROG 2                                                                
         SSPEC H5,2,C'QTR/YR EST  QTRSTD    QTREDT    ESTRT    EEND'            
         SSPEC H5,51,C'CAT'                                                     
         SPROG 1                                                                
         SSPEC H5,2,C'PRD EST MARKET RECORDS  DESCRIPTION'                      
         SSPEC H6,2,C'--- --- ------ ------  -----------'                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPGX01 08/29/00'                                      
         END                                                                    
