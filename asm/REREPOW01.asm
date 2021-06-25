*          DATA SET REREPOW01  AT LEVEL 015 AS OF 08/31/00                      
*          DATA SET REREPOW01  AT LEVEL 014 AS OF 11/05/98                      
*PHASE REOW01A                                                                  
         TITLE 'REREPOW01 - INACTIVE OWNER PURGE'                               
**********************************************************************          
*                                                                    *          
*        REREPOW01 - INACTIVE OWNER PURGE HEADINGS                   *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 14OCT98   (JRD) -- INITIAL ENTRY                                   *          
*                     TOO BAD ASPEC DOESN'T LIKE EQUATES             *          
*                                                                    *          
**********************************************************************          
REOW01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H01,049,C'INACTIVE OWNER PURGE'                                  
         ASPEC H01,100,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H03,002,C'STATUS'                                                
         ASPEC H03,009,C'OWNER'                                                 
         ASPEC H03,040,C'REP'                                                   
         ASPEC H03,046,C'STATION LIST'                                          
*                                                                               
* > > > > > > > > >  > > END OF REREPOW01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015REREPOW01 08/31/00'                                      
         END                                                                    
