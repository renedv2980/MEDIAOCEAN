*          DATA SET RERMP25    AT LEVEL 027 AS OF 06/27/08                      
*PHASE T81025A,*                                                                
         TITLE 'RERMP25 - T81025 - SPECS FOR OVERNIGHT PROJECTION'              
*                                                                               
*******************************************************************             
*                                                                 *             
*        RERMP25 --- REP INVENTORY OVERNIGHT PROJECTION SPECS     *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* OCT26/90 (MRR) --- MODIFY TITLE FOR 6 DEMOS DUE TO 1 DECIMAL    *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T81025   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         PSPEC H01,001,AGYNAME                                                  
         PSPEC H01,045,C'INVENTORY PROJECTION'                                  
         PSPEC H01,085,REPORT                                                   
         PSPEC H01,101,PAGE                                                     
         PSPEC H02,001,REQUESTOR                                                
         PSPEC H02,045,20C'-'                                                   
         PSPEC H02,085,RUN                                                      
         PSPEC H04,001,C'STATION -'                                             
         PSPEC H04,075,C'SVC FROM BOOK1 TO BOOK2'                               
         SPACE 1                                                                
         SPROG 0                                                                
         PSPEC H04,045,C'DAYPART -'                                             
         PSPEC H08,001,C' DAY(S)   TIME(S)    PROGRAMMING'                      
         PSPEC H08,050,C'INV.   START  CODE'                                    
         PSPEC H09,050,C'NUM.   DATE'                                           
         PSPEC H08,072,C'                     W18+   M18+  T.V '                
         PSPEC H09,072,C' HUT    SHR    RTG   RTG    RTG   HOMES'               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027RERMP25   06/27/08'                                      
         END                                                                    
