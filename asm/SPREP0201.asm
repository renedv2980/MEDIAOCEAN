*          DATA SET SPREP0201  AT LEVEL 015 AS OF 08/29/00                      
*PHASE SP0201A                                                                  
         TITLE 'STATION FILE PURGE - PRINT SPECS'                               
SP0201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0203                                                       
         FSPEC UPDATE,STATION                                                   
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         SPROG 0,1,2,3,4                                                        
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,49,C'STATION FILE PURGE REPORT'                               
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,49,C'------- ---- ----- ------'                               
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,3,PAGE                                                        
         SSPEC H4,47,PERIOD                                                     
         SSPEC H4,98,REPORT                                                     
         SPROG 1                                                                
         SSPEC H4,49,C'STATION MASTERS'                                         
         SSPEC H5,49,C'---------------'                                         
         SPROG 2                                                                
         SSPEC H4,49,C'STATION ADDRESSES'                                       
         SSPEC H5,49,C'-----------------'                                       
         SPROG 3                                                                
         SSPEC H4,49,C'REP RECORDS'                                             
         SSPEC H5,49,C'-----------'                                             
         SPROG 4                                                                
         SSPEC H4,49,C'MARKET RECORDS'                                          
         SSPEC H5,49,C'--------------'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREP0201 08/29/00'                                      
         END                                                                    
