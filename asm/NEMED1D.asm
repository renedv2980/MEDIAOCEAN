*          DATA SET NEMED1D    AT LEVEL 007 AS OF 11/28/01                      
*PHASE T31E1DA                                                                  
         TITLE 'T31E1D - SPECS FOR NETWORK AGENCY SUMMARY'                      
T31E1D   CSECT                                                                  
         PRINT NOGEN                                                            
**********SPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H1,45,C'AGENCY SUMMARY'                                          
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,45,C'--------------'                                          
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,1,PERIOD                                                      
         SSPEC H5,41,C'NETWORK='                                                
         SSPEC H5,90,PAGE                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEMED1D   11/28/01'                                      
         END                                                                    
