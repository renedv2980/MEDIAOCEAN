*          DATA SET REREPKZ01  AT LEVEL 053 AS OF 11/30/95                      
*PHASE REKZ01A,                                                                 
         TITLE 'REREPKZ01 - GENERAL CONTRACT FIXER '                            
**********************************************************************          
*                                                                    *          
*        REREPKZ01 --- REPPACK GENERAL KATZ FIXER                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* NOV30/95 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
REKZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,3,4,5                                                          
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'GENERAL   CONTRACT  FIXER'                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPACE 1                                                                
* > > > > > > > > >  > > END OF REREPKZ01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053REREPKZ01 11/30/95'                                      
         END                                                                    
