*          DATA SET REREP1D01  AT LEVEL 063 AS OF 02/04/91                      
*PHASE RE1D01A,                                                                 
         TITLE 'REREP1D01 - BUDGET ALLOCATION RESETTER SPECS'                   
**********************************************************************          
*                                                                    *          
*        REREP1D01 --- REPPACK BUDGET ALLOCATION RESETTER SPECS      *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* SEP11/90 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
**********************************************************************          
RE1D01   CSECT                                                                  
         PRINT GEN                                                              
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,054,C'BUDGET  ALLOCATION RESETTER'                           
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H08,002,C'STATION'                                               
         ASPEC H08,011,C'OFFICE'                                                
         ASPEC H08,030,C' ALLOCATION'                                           
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H07,020,C'CONTRACT'                                              
         ASPEC H08,020,C'  TYPE  '                                              
         SPACE 1                                                                
*                                                                               
* > > > > > > > > >  > > END OF REREP1D01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063REREP1D01 02/04/91'                                      
         END                                                                    
