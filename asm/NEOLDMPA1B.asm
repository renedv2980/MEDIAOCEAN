*          DATA SET NEOLDMPA1B AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E1BA                                                                  
         TITLE 'T31E1B - SPECS FOR MPALIST'                                     
T31E1B   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,2,C'LISTING OF MPA PROGRAMS'                                  
         SSPEC H2,2,23C'-'                                                      
         SSPEC H1,45,RUN                                                        
         SSPEC H1,72,PAGE                                                       
         SSPEC H2,45,REPORT                                                     
         SSPEC H2,61,REQUESTOR                                                  
         SSPEC H4,2,C'BOOK - '                                                  
         SSPEC H7,2,C'PROGRAM NAME     CODE NET'                                
         SSPEC H8,2,C'------------     ---- ---'                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEOLDMPA1B08/10/00'                                      
         END                                                                    
