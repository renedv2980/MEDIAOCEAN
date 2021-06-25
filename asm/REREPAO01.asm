*          DATA SET REREPAO01  AT LEVEL 060 AS OF 10/20/97                      
*PHASE REAO01A,                                                                 
         TITLE 'REREPAO01 - TAKEOVER REPORT SPECS'                              
**********************************************************************          
*                                                                    *          
*        REREPAO01 --- REPPACK MULTI AS-AT DATE REPORT HDLN SPEC     *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* OCT20/97 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
REAO01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'MULTI AS-AT DATE'                                      
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
* > > > > > > > > >  > > END OF REREP1A01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060REREPAO01 10/20/97'                                      
         END                                                                    
