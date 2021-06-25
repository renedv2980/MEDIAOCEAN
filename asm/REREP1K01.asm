*          DATA SET REREP1K01  AT LEVEL 032 AS OF 03/08/96                      
*PHASE RE1K01A,                                                                 
         TITLE 'REREP1K01 - STATEGY SEEDER'                                     
**********************************************************************          
*                                                                    *          
*        REREP1K01 --- STATEGY SEEDER                                *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* MAR07/95 (BU ) --- INITIAL ENTRY: CLONED FROM REREP1B01            *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1K01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC UPDATE,REPFIL                                                    
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,053,C'S*T*R*A*T*E*G*Y   SEEDER'                              
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,054,PERIOD                                                   
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,004,C'GROUP'                                                 
         ASPEC H05,006,C'STATION'                                               
         SPACE 1                                                                
         ASPEC H06,002,C'CODE'                                                  
         ASPEC H06,007,C'ACCOUNT'                                               
         ASPEC H06,028,C'OFF'                                                   
         ASPEC H06,038,C'PRIOR'                                                 
         ASPEC H06,053,C'PRIOR'                                                 
         ASPEC H06,064,C'SHARE'                                                 
         ASPEC H06,077,C'DOLLARS'                                               
         SPACE 1                                                                
         ASPEC H07,028,C'ICE'                                                   
         ASPEC H07,038,C'SHARE'                                                 
         ASPEC H07,051,C'DOLLARS'                                               
         ASPEC H07,065,C'GOAL'                                                  
         ASPEC H07,080,C'GOAL'                                                  
         SPACE 1                                                                
*                                                                               
* > > > > > > > > >  > > END OF REREP1K01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032REREP1K01 03/08/96'                                      
         END                                                                    
