*          DATA SET SPREPD501  AT LEVEL 010 AS OF 08/29/00                      
*PHASE SPD501A                                                                  
         TITLE 'SPREPD501-PTS,PRS SPECS'                                        
         PRINT NOGEN                                                            
SPD501   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC READ,PACKAGES                                                    
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,77,AGYNAME                                                    
*        SSPEC H1,20,MGROUP                                                     
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H3,1,PAGE                                                        
         SSPEC H3,77,REPORT                                                     
         SSPEC H3,30,PERIOD                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SSPEC H5,1,PGROUP                                                      
         SSPEC H4,77,MARKET                                                     
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPD501 08/29/00'                                      
         END                                                                    
