*          DATA SET REREPS101  AT LEVEL 020 AS OF 01/28/97                      
*PHASE RES101A,*                                                                
         TITLE 'SPECS FOR STATION TAKEOVER'                                     
*                                                                               
*- REREPS101 -- PHASE REKS01 -- SPECS MODULE FOR STATION TAKEOVER               
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
RES101   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'STATION TAKEOVER REPORT'                                 
         SPROG 1                                                                
         ASPEC H2,36,C'AGENCY CODE ASSIGNMENT LISTING'                          
         ASPEC H4,06,C'-----AGENCY  CODES----'                                  
         ASPEC H4,34,C'SWITCH   REQUEST'                                        
         ASPEC H5,04,C'  ORIG              NEW'                                 
         ASPEC H5,34,C'  TO      MADE? '                                        
         ASPEC H5,60,C'AGENCY  NAME   '                                         
         SPROG 2                                                                
         ASPEC H2,34,C'ADVERTISER CODE ASSIGNMENT LISTING'                      
         ASPEC H4,06,C'-----ADVERT  CODES----'                                  
         ASPEC H4,34,C'SWITCH   REQUEST'                                        
         ASPEC H5,04,C'  ORIG              NEW'                                 
         ASPEC H5,34,C'  TO      MADE? '                                        
         ASPEC H5,60,C'ADVERT  NAME   '                                         
         SPROG 3                                                                
         ASPEC H2,33,C'FILE CONTROL TOTALS: TAKEOVER REPORT'                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020REREPS101 01/28/97'                                      
         END                                                                    
