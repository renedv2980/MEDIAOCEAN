*          DATA SET REREPAI01  AT LEVEL 061 AS OF 08/31/00                      
*          DATA SET REREPAI01  AT LEVEL 060 AS OF 12/05/97                      
*PHASE REAI01A                                                                  
         TITLE 'REREPAI01 - ALTERNATE CALENDAR INITIALIZER'                     
**********************************************************************          
*                                                                    *          
*        REREPAI01 --- REPPACK ALTERNATE CALENDAR INITIALIZER        *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* DEC02/97 (JRD) --- INITIAL ENTRY:                                  *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
REAI01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC UPDATE,REPFIL                                                    
         SPROG 0,1,2,3,4,5,6,7,8,9,10                                           
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'ALTERNATE CALENDAR INITIALIZER'                        
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,054,PERIOD                                                   
         ASPEC H02,080,STATION                                                  
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPACE 1                                                                
* > > > > > > > > >  > > END OF REREPAI01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061REREPAI01 08/31/00'                                      
         END                                                                    
