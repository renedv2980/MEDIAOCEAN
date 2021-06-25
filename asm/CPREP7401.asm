*          DATA SET CPREP7401  AT LEVEL 006 AS OF 09/01/00                      
*PHASE CP7401A                                                                  
         TITLE 'CPREP7401-CPP MARKET SUMMARY'                                   
         PRINT NOGEN                                                            
CP7401   CSECT                                                                  
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,103,PAGE                                                      
         SSPEC H1,41,C'CPP MARKET SUMMARY'                                      
         SSPEC H2,41,C'------------------'                                      
         SSPEC H09,1,C'MARKET RANK AND NAME'                                    
         SSPEC H10,1,C'--------------------'                                    
         SSPEC H09,21,C'  10    15    20    30    45    60  OTHER'              
         SSPEC H10,21,C'----- ----- ----- ----- ----- ----- -----'              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CPREP7401 09/01/00'                                      
         END                                                                    
