*          DATA SET REREP1901  AT LEVEL 024 AS OF 08/28/96                      
*PHASE RE1901A,*                                                                
         TITLE 'REREP1901 (RE1901)- INVOICE CONTROL LIST SPECS'                 
***********************************************************************         
*                                                                     *         
*  REREP1901 (RE1901) --- INVOICE CONTROL LIST                        *         
*                         (AKA DONUT REPORT)                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*  MOD LOG:                                                           *         
*  -------                                                            *         
*                                                                     *         
*  MAY25/90 (MRR) --- INITIAL DEVELOPMENT                             *         
*                                                                     *         
*  JUL31/91 (BU ) --- ADD CONTRACT TYPE FILTERING HEADLINE DISPLAY    *         
*                                                                     *         
*  OCT08/91 (SKU) --- ADD INTERFACE NUMBER COLUMN                     *         
*                                                                     *         
*  NOV11/93 (BU ) --- ADD PRIOR MONTHS, RECAP SUMMARY                 *         
*                                                                     *         
*  MAR30/94 (BU ) --- ADD INTERFACE CODE ORDER SEQUENCE               *         
*                                                                     *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
*                                                                               
RE1901   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2                                                            
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'INVOICE CONTROL LIST'                                  
         ASPEC H01,099,RENUM                                                    
         ASPEC H01,115,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,056,C'--------------------'                                  
         ASPEC H02,099,RUN                                                      
*                                                                               
         ASPEC H04,040,C'STATIONS NOT REPORTING INVOICES FOR THE'               
         ASPEC H04,080,C'BROADCAST MONTH'                                       
*                                                                               
         SPROG 3                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,060,C'REPORT RECAP'                                          
         ASPEC H01,099,RENUM                                                    
         ASPEC H01,115,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,056,C'--------------------'                                  
         ASPEC H02,099,RUN                                                      
*                                                                               
         ASPEC H06,002,C'GROUP'                                                 
         ASPEC H07,002,C'------------'                                          
         ASPEC H04,016,C'STATIONS'                                              
         ASPEC H05,018,C'WITH'                                                  
         ASPEC H06,016,C'CONTRACTS'                                             
         ASPEC H07,016,C'---------'                                             
         ASPEC H04,027,C'STATIONS'                                              
         ASPEC H05,030,C'NOT'                                                   
         ASPEC H06,027,C'CONVERTED'                                             
         ASPEC H07,027,C'---------'                                             
         ASPEC H04,039,C'PERCENT'                                               
         ASPEC H05,042,C'NOT'                                                   
         ASPEC H06,038,C'CONVERTED'                                             
         ASPEC H07,038,C'---------'                                             
*                                                                               
*                                                                               
         SPROG 0                                                                
         ASPEC H06,002,C'INTERFACE#'                                            
         ASPEC H07,002,C'----------'                                            
         ASPEC H06,013,C'STAT'                                                  
         ASPEC H07,013,C'------'                                                
         ASPEC H06,020,C'MARKET'                                                
         ASPEC H07,020,C'-------------------'                                   
         ASPEC H06,040,C'END DATE'                                              
         ASPEC H07,040,C'--------'                                              
         ASPEC H06,049,C'TELEPHONE #'                                           
         ASPEC H07,049,C'------------'                                          
         ASPEC H06,062,C'FAX NUMBER'                                            
         ASPEC H07,062,C'----------------'                                      
*                                                                               
         SPROG 1                                                                
         ASPEC H06,055,C'CONTRACT TYPE   FILTER'                                
*                                                                               
         SPROG 2                                                                
         ASPEC H06,055,C'CONTRACT TYPE   EXCLUDED'                              
*                                                                               
         SPROG 1,2                                                              
         ASPEC H08,002,C'INTERFACE #'                                           
         ASPEC H09,002,C'-----------'                                           
         ASPEC H08,014,C'STATION'                                               
         ASPEC H09,014,C'-------'                                               
         ASPEC H08,029,C'MARKET'                                                
         ASPEC H09,022,C'--------------------'                                  
         ASPEC H08,044,C'TELEPHONE #'                                           
         ASPEC H09,044,C'------------'                                          
         ASPEC H08,059,C'FAX NUMBER'                                            
         ASPEC H09,058,C'------------'                                          
*                                                                               
         DC    X'00'               END MARKER FOR ASPECS                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024REREP1901 08/28/96'                                      
         END                                                                    
