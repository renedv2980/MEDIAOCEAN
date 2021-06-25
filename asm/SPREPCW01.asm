*          DATA SET SPREPCW01  AT LEVEL 049 AS OF 10/31/00                      
*PHASE SPCW01                                                                   
         TITLE 'SPCW01 - IM PW TO COST2 CONVERSION - SPECS'                     
SPCW01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SPROG 1,2                                                              
         SSPEC H1,54,C'PW TO COST2 CONVERSION'                                  
         SSPEC H2,54,C'----------------------'                                  
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SPROG 1                                                                
         SSPEC M1,42,C' LOKWIMG    LOKWIMN    LOKCLTG    LOKCLTN'               
         SSPEC M2,42,C' -------    -------    -------    -------'               
         SPROG 2                                                                
         SSPEC M1,42,C'  WIMGRS     WIMNET     CLTGRS     CLTNET'               
         SSPEC M2,42,C'  ------     ------     ------     ------'               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SPREPCW01 10/31/00'                                      
         END                                                                    
