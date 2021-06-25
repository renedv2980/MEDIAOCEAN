*          DATA SET REREP1E01  AT LEVEL 004 AS OF 04/25/91                      
*PHASE RE1E01A,                                                                 
         TITLE 'REREP1E01 - STATION REC COMBO ELEM SWITCHER SPECS'              
**********************************************************************          
*                                                                    *          
*        REREP1E01 --- STATION REC COMBO ELEM SWITCHER SPECS         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 10APR91 (EFJ) --- INITIAL ENTRY                                    *          
*                                                                    *          
**********************************************************************          
RE1E01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,055,C'COMBO ELEM SWITCHER'                                   
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004REREP1E01 04/25/91'                                      
         END                                                                    
