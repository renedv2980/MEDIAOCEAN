*          DATA SET SPREPXG01  AT LEVEL 016 AS OF 08/29/00                      
*PHASE SPXG01A                                                                  
         TITLE 'SPREPXG01 - P&&G INTERFACE SPECS'                               
         PRINT NOGEN                                                            
SPXG01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SPROG 0,THRU,2                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,44,C'P AND G INTERFACE TAPE'                                  
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,44,C'----------------------'                                  
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H3,39,PERIOD                                                     
*                                                                               
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,102,PAGE                                                      
*                                                                               
         SSPEC H6,1,C' MKT  STAT  PRD  EST-LIN  DAY(S)  TIME(S)'                
         SSPEC H7,1,C' ---  ----  ---  -------  ------  -------'                
         SSPEC H6,48,C'PROGRAM            SPOTS    COST   SLN'                  
         SSPEC H7,48,C'-------            -----    ----   ---'                  
         SSPEC H6,88,C'CODES  ERRORS'                                           
         SSPEC H7,88,C'-----  ------'                                           
*                                                                               
         SPROG 2                                                                
         SSPEC  H5,1,C'*** DATA NOT ON OUTPUT TAPE ***'                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPXG01 08/29/00'                                      
         END                                                                    
