*          DATA SET SPREPKS01  AT LEVEL 002 AS OF 10/26/05                      
*PHASE SPKS01A                                                                  
SPKS01   TITLE 'SPREPKS01 - KATZ SALESPERSON EXTRACT'                           
SPKS01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'KATZ SALESPERSON'                                        
         SSPEC H2,57,C'----------------'                                        
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPKS01 10/26/05'                                      
         END                                                                    
