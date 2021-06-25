*          DATA SET REREP1C01  AT LEVEL 061 AS OF 03/08/96                      
*PHASE RE1C01A,                                                                 
         TITLE 'REREP1C01 - BUDGET ALLOCATION SPREADER SPECS'                   
**********************************************************************          
*                                                                    *          
*        REREP1C01 --- REPPACK BUDGET ALLOCATION SPREADER SPECS       *         
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* SEP11/90 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
* DEC11/91 (BU ) --- MOVE 'ALLOCATION' FIELD 2 POSITIONS TO LEFT     *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1C01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC UPDATE,REPFIL                                                    
         SPROG 0,3,6                                                            
         ASPEC H01,002,REP                                                      
         ASPEC H01,055,C'BUDGET ALLOCATION SPREADER'                            
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H03,054,PERIOD                                                   
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H04,004,C'GROUP'                                                 
         ASPEC H05,006,C'STATION'                                               
         ASPEC H08,008,C'OFFICE'                                                
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H05,006,C'OFFICE'                                                
         ASPEC H08,008,C'STATION'                                               
         SPACE 1                                                                
         SPROG 0,3                                                              
         ASPEC H08,038,C' ALLOCATION'                                           
         SPACE 1                                                                
         SPROG 0,3                                                              
         ASPEC H07,030,C'CONTRACT'                                              
         ASPEC H08,030,C'  TYPE  '                                              
         SPACE 1                                                                
         SPROG 6                                                                
         ASPEC H05,050,C'****NON-JOINED/LEFT STATIONS****'                      
         ASPEC H06,050,C'***OUT-OF-BALANCE ALLOCATIONS***'                      
         ASPEC H08,018,C'             CON '                                     
         ASPEC H09,018,C'STATION  OFF TYPE'                                     
*                                                                               
*        SPROG 0,3,6                                                            
*        ASPEC F01,002,REQDETS                                                  
*                                                                               
* > > > > > > > > >  > > END OF REREP1C01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061REREP1C01 03/08/96'                                      
         END                                                                    
