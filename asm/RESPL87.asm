*          DATA SET RESPL87    AT LEVEL 008 AS OF 11/08/90                      
*PHASE T80887A,*                                                                
         TITLE 'RESPL87 - T80887 - SPECS FOR OVERNIGHT TRANSFER'                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESPL87 --- REP INVENTORY OVERNIGHT TRANSFER SPECS       *             
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
T80887   CSECT                                                                  
         PRINT NOGEN                                                            
         PSPEC H01,001,AGYNAME                                                  
         PSPEC H01,045,C'INVENTORY TRANSFER'                                    
         PSPEC H01,099,REPORT                                                   
         PSPEC H01,115,PAGE                                                     
         PSPEC H02,001,REQUESTOR                                                
         PSPEC H02,045,18C'-'                                                   
         PSPEC H02,099,RUN                                                      
         PSPEC H04,001,C'STATION -'                                             
         PSPEC H04,051,C'DAYPART -'                                             
         PSPEC H04,095,C'SOURCE'                                                
         PSPEC H04,109,C'BOOK'                                                  
         PSPEC H08,001,C' NUM. ST/END    DAY(S)    TIME(S)     P'               
         PSPEC H08,039,C'PROGRAMMING'                                           
         PSPEC H08,068,C'TR.'                                                   
         PSPEC H08,075,C'NUM.  DAY  TIME   PROGRAMMING'                         
         PSPEC H08,110,C'WT  RTG    SHR    HUT'                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008RESPL87   11/08/90'                                      
         END                                                                    
