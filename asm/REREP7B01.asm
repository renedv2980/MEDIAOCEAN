*          DATA SET REREP7B01  AT LEVEL 089 AS OF 03/04/02                      
*PHASE RE7B01A,                                                                 
         TITLE 'REREPTK01 - TAKEOVER REPORT SPECS'                              
**********************************************************************          
*                                                                    *          
*        REREPTK01 --- REPPACK TAKEOVER REPORT HDLINE SPEC           *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* FEB14/02 (HQ ) --- BORN ON VALENTINE'S DAY                         *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE7B01   CSECT                                                                  
         PRINT GEN                                                              
*--->    FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,040,C'STATION CONTRACT END DATE LIST'                        
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H08,002,C'STATION  ACTIVE   JOIN       CONTRACT   REMA'          
         ASPEC H08,046,C'INING A/R         EFF       COMM'                      
         ASPEC H08,096,C'                     '                                 
         ASPEC H09,002,C'                  DATE       END DATE   TERM'          
         ASPEC H09,046,C'      CODE        DATE      RATE '                     
         ASPEC H09,096,C'                                            '          
         SPACE 1                                                                
*                                                                               
***<<<                                                                          
***:::                                                                          
*                                                                               
* > > > > > > > > >  > > END OF REREPTK01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089REREP7B01 03/04/02'                                      
         END                                                                    
