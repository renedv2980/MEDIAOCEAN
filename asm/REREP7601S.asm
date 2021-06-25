*          DATA SET REREP7601S AT LEVEL 009 AS OF 09/09/94                      
*PHASE RE7601A,*                                                                
         TITLE 'REREP7601 (RE7601) --- SPECS FOR AGENCY LISTING'                
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP7601 - RE7601 - SPECS FOR REP AGENCY LISTING               *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* APR22/92 (SKU) --- ADD ROOM FOR RISK/LIAB MSG                     *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE7601   CSECT                                                                  
         FSPEC READ,AGENCIES                                                    
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H01,002,REP1                                                     
         ASPEC H01,050,C'AGENCY LISTING'                                        
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,RUN                                                      
         ASPEC H02,050,14C'-'                                                   
         ASPEC H02,100,REQUESTOR                                                
*                                                                               
         SPROG 0                                                                
         ASPEC H07,002,C'CODE OFFICE AGENCY NAME'                               
         ASPEC H07,039,C'CODE OFFICE AGENCY NAME'                               
         ASPEC H07,076,C'CODE OFFICE AGENCY NAME'                               
         ASPEC H08,002,C'---- ------ -----------'                               
         ASPEC H08,039,C'---- ------ -----------'                               
         ASPEC H08,076,C'---- ------ -----------'                               
*                                                                               
         SPROG 1                                                                
         ASPEC H07,014,C'NAME AND ADDRESS'                                      
         ASPEC H07,051,C'NAME AND ADDRESS'                                      
         ASPEC H07,088,C'NAME AND ADDRESS'                                      
         ASPEC H08,014,C'----------------'                                      
         ASPEC H08,051,C'----------------'                                      
         ASPEC H08,088,C'----------------'                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREP7601S09/09/94'                                      
         END                                                                    
