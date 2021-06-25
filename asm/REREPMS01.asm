*          DATA SET REREPMS01  AT LEVEL 008 AS OF 08/31/00                      
*          DATA SET REREPMS01  AT LEVEL 007 AS OF 05/19/99                      
*PHASE REMS01A                                                                  
         TITLE 'REREPMS01 - STATION MASTER CONTROL BUILDER'                     
**********************************************************************          
*                                                                    *          
*        REREPOW01 - STATION MASTER CONTROL BUILDER                  *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 14OCT98   (JRD) -- INITIAL ENTRY                                   *          
*                      TO BAD ASPEC DOESN'T LIKE EQUATES             *          
*                                                                    *          
**********************************************************************          
REMS01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         SPROG 0                                                                
         ASPEC H01,002,RUN                                                      
         ASPEC H01,049,C'STATION MASTER CONTROL BUILDER'                        
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,C'MASTER'                                                
         ASPEC H02,010,C'STATION'                                               
         ASPEC H02,020,C'SUBSIDARY'                                             
         ASPEC H02,035,C'JOIN DATE'                                             
         ASPEC H02,045,C'LEAVE DATE'                                            
         SPACE 1                                                                
         ASPEC H03,002,C'------'                                                
         ASPEC H03,010,C'-------'                                               
         ASPEC H03,020,C'---------'                                             
         ASPEC H03,035,C'---------'                                             
         ASPEC H03,045,C'----------'                                            
         SPACE 1                                                                
*                                                                               
* > > > > > > > > >  > > END OF REREPOW01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREPMS01 08/31/00'                                      
         END                                                                    
