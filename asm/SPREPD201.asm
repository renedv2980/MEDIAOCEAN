*          DATA SET SPREPD201  AT LEVEL 025 AS OF 01/18/08                      
*PHASE SPD201A                                                                  
         TITLE 'SPREPD201-BTS,BRS,SAL SPECS'                                    
         PRINT NOGEN                                                            
SPD201   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC READ,PACKAGES                                                    
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,100,RATING                                                    
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H5,100,BOOK                                                      
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,51,MGROUP                                                     
         SSPEC H4,1,PGROUP                                                      
         SSPEC H7,100,EQUIV                                                     
         SSPEC H9,51,DAYPART                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
* FOLLOWING FOR 110 CHAR CP REPORT                                              
         SPROG 9                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,50,C'DRAFT ORDER'                                             
         SSPEC H2,50,C'-----------'                                             
         SSPEC H1,74,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,74,AGYADD                                                     
         SSPEC H3,41,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,41,MGROUP                                                     
         SSPEC H4,1,PGROUP                                                      
         SSPEC H9,51,DAYPART                                                    
         SSPEC H8,74,PAGE                                                       
         SSPEC H8,85,REPORT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPD201 01/18/08'                                      
         END                                                                    
