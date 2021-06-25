*          DATA SET SPREPMP01  AT LEVEL 013 AS OF 08/29/00                      
*PHASE SPMP01A                                                                  
         TITLE 'SPREPML01-LOCKIN REPORT SPECS'                                  
         PRINT NOGEN                                                            
SPMP01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,STATION                                                      
         FSPEC GET,MARKET                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC OPEN,DEMFILES                                                    
         SPROG 1,THRU,8                                                         
*                                                                               
         SSPEC H1,2,MEDIA                                                       
*                                                                               
         SSPEC H1,43,C'MULTI STAGE LOCKIN TURNAROUND REPORT'                    
*                                                                               
         SSPEC H1,83,AGYNAME                                                    
*                                                                               
         SSPEC H2,2,REQUESTOR                                                   
*                                                                               
         SSPEC H2,43,C'------------------------------------'                    
*                                                                               
         SSPEC H2,83,AGYADD                                                     
*                                                                               
         SSPEC H3,83,C'RATING SERVICE - ARB'                                    
*                                                                               
         SSPEC H4,43,PERIOD                                                     
*                                                                               
         SSPEC H5,2,CLIENT                                                      
*                                                                               
         SSPEC H6,2,PRODUCT                                                     
*                                                                               
         SSPEC H6,43,MARKET                                                     
*                                                                               
         SSPEC H7,2,ESTIMATE                                                    
*                                                                               
         SSPEC H7,83,PAGE                                                       
*                                                                               
         SSPEC H7,93,REPORT                                                     
*                                                                               
         SSPEC H9,2,C'DPT/LN    WEEK OF    FLIGHT    SPOTS    DOLLARS'          
*                                                                               
         SSPEC H10,2,C'------    -------    ------    -----    -------'         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPMP01 08/29/00'                                      
         END                                                                    
