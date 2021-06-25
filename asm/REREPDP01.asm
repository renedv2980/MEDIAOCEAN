*          DATA SET REREPDP01  AT LEVEL 063 AS OF 07/09/07                      
*PHASE REDP01A,                                                                 
         TITLE 'REREPDP01 - DARE PURGE REPORT SPECS'                            
**********************************************************************          
*                                                                    *          
*        REREPDP01 --- DARE PURGE REPORT SPECS                       *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* JUN02/03 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
* JUL09/07 (BU ) --- OPEN FILES FOR UPDATE.                          *          
*                                                                    *          
**********************************************************************          
REDP01   CSECT                                                                  
         PRINT NOGEN                                                            
***      FSPEC READ,CONTRACTS                                                   
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'DARE RECORD PURGE'                                     
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         SPACE 1                                                                
*                                                                               
* > > > > > > > > >  > > END OF REREPDP01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063REREPDP01 07/09/07'                                      
         END                                                                    
