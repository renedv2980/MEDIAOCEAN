*          DATA SET RERMP22    AT LEVEL 005 AS OF 06/27/08                      
*          DATA SET RESPL22    AT LEVEL 008 AS OF 11/08/90                      
*PHASE T81022A,*                                                                
         TITLE 'RESPL22 - T81022 - SPECS FOR OVERNIGHT TRANSFER'                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESPL22 --- REP INVENTORY OVERNIGHT TRANSFER SPECS       *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* OCT26/90 (MRR) --- MODIFY TITLE FOR 3 DEMOS DUE TO 1 DECIMAL    *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T81022   CSECT                                                                  
         PRINT NOGEN                                                            
         PSPEC H01,001,AGYNAME                                                  
         PSPEC H01,051,C'INVENTORY TRANSFER'                                    
         PSPEC H01,099,REPORT                                                   
         PSPEC H01,115,PAGE                                                     
         PSPEC H02,001,REQUESTOR                                                
         PSPEC H02,051,18C'-'                                                   
         PSPEC H02,099,RUN                                                      
         PSPEC H04,001,C'STATION -'                                             
         PSPEC H04,051,C'DAYPART -'                                             
         PSPEC H04,095,C'SOURCE'                                                
         PSPEC H04,109,C'BOOK'                                                  
         PSPEC H08,001,C' NUM. ST/END    DAY(S)    TIME(S)     P'               
         PSPEC H08,039,C'PROGRAMMING'                                           
         PSPEC H08,063,C'TR.'                                                   
         PSPEC H08,070,C'NUM.  DAY  TIME   PROGRAMMING'                         
         PSPEC H08,105,C' WT    RTG    SHR    HUT'                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005RERMP22   06/27/08'                                      
         END                                                                    
