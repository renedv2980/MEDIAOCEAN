*          DATA SET SPREPM401  AT LEVEL 033 AS OF 01/26/90                      
*PHASE SPM401A,+0,NOAUTO                                                        
         TITLE 'SPREPM401-MARKET PERFORMANCE SPECS'                             
         PRINT NOGEN                                                            
SPM401   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,50,SP#MKTPR,33,C                                              
         SSPEC H2,50,SP#MKTPR,33,CU                                             
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H4,51,MGROUP                                                     
         SSPEC H7,100,EQUIV                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H9,51,DAYPART                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
         SPROG 3,4                                                              
         SSPEC H7,51,SP#PRSUM,33,C                                              
         SPROG 5,6                                                              
         SSPEC H7,51,SP#PDSUM,32,C                                              
         SPROG 7,8                                                              
         SSPEC H7,51,SP#CLSUM,32,C                                              
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPREPM401 01/26/90'                                      
         END                                                                    
