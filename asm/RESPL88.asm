*          DATA SET RESPL88    AT LEVEL 018 AS OF 08/31/95                      
*PHASE T80888A,*                                                                
         TITLE 'RESPL88 - T80888 - SPECS FOR OVERNINGHT PROJECTION'             
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESPL88 --- REP INVENTORY OVERNIGHT PROJECTION SPECS     *             
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
T80888   CSECT                                                                  
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
         PSPEC H04,041,C'DAYPART -'                                             
         PSPEC H08,001,C' DAY(S)   TIME(S)     PROGRAMMING'                     
         PSPEC H08,051,C'INV.   START   CODE'                                   
         PSPEC H09,051,C'NUM.   DATE'                                           
         PSPEC H08,073,C'                    RTG    RTG   T.V '                 
         PSPEC H09,073,C'HUT    SHR    RTG   W18+   M18+  HOMES'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018RESPL88   08/31/95'                                      
         END                                                                    
